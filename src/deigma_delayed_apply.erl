-module(deigma_delayed_apply).
-behaviour(workforce_resource).
-behaviour(gen_server).

% https://gist.github.com/marcelog/97708058cd17f86326c82970a7f81d40#file-simpleproc-erl

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [pool_name/1,
    schedule/4
   ]).

%%-------------------------------------------------------------------
%% workforce_resource Function Exports
%%-------------------------------------------------------------------

-export(
   [start_link/2
   ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export(
   [init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

%%-------------------------------------------------------------------
%% Macro Definitions
%%-------------------------------------------------------------------

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(state, {
          category :: atom(),
          agent_pid :: pid(),
          schedules :: gb_trees:tree(schedule_key(), schedule())
         }).

-type schedule_key() :: {Ts :: integer(), reference()}.

-record(schedule, {
          function :: fun(),
          args :: list()
         }).
-type schedule() :: #schedule{}.

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

pool_name(Category) ->
    list_to_atom("deigma." ++ atom_to_list(Category)).

schedule(Category, Timestamp, Function, Args) ->
    PoolName = pool_name(Category),
    Schedule = #schedule{ function = Function, args = Args},
    workforce:transaction(
      PoolName,
      fun (ApplierPid) ->
              gen_server:call(ApplierPid, {add_schedule, Timestamp, Schedule})
      end).

%%-------------------------------------------------------------------
%% workforce_resource Function Definitions
%%-------------------------------------------------------------------

-spec start_link([atom(), ...], pid()) -> {ok, pid()}.
%% @private
start_link([Category], AgentPid) ->
    gen_server:start_link(?MODULE, [Category, AgentPid], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Category, AgentPid]) ->
    workforce_agent:declare_ready(AgentPid, self()),
    {ok, #state{
            category = Category,
            agent_pid = AgentPid,
            schedules = gb_trees:empty()
           }}.

handle_call({add_schedule, Timestamp, NewSchedule}, From, State) ->
    {_CallerPid, CallRef} = From,
    Schedules = State#state.schedules,
    NewScheduleKey = {Timestamp, CallRef},
    UpdatedSchedules = gb_trees:insert(NewScheduleKey, NewSchedule, Schedules),
    UpdatedState = State#state{ schedules = UpdatedSchedules },
    NextTimeout = next_timeout(UpdatedState),
    {reply, ok, UpdatedState, NextTimeout};
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

handle_info(timeout, State) ->
    Schedules = State#state.schedules,
    {_ScheduleKey, Schedule, UpdatedSchedules} = gb_trees:take_smallest(Schedules),
    run_scheduled_apply(Schedule),
    UpdatedState = State#state{ schedules = UpdatedSchedules },
    NextTimeout = next_timeout(UpdatedState),
    {noreply, UpdatedState, NextTimeout};
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

next_timeout(State) ->
    Schedules = State#state.schedules,
    try gb_trees:smallest(Schedules) of
        {{Timestamp, _CallRef}, _Schedule} ->
            Timeout = Timestamp - erlang:monotonic_time(),
            TimeoutMs = erlang:convert_time_unit(Timeout, native, millisecond),
            max(0, TimeoutMs)
    catch
        error:function_clause ->
            infinity
    end.

run_scheduled_apply(Schedule) ->
    apply(Schedule#schedule.function, Schedule#schedule.args).
