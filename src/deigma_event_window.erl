%% Copyright (c) 2018 Guilherme Andrade
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% @private
-module(deigma_event_window).

% based on https://gist.github.com/marcelog/97708058cd17f86326c82970a7f81d40#file-simpleproc-erl

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [start_link/2,
    ask/4
   ]).

-ignore_xref(
   [start_link/2
   ]).

%%-------------------------------------------------------------------
%% OTP Exports
%%-------------------------------------------------------------------

-export(
   [init/1,
    system_code_change/4,
    system_continue/3,
    system_terminate/4,
    write_debug/3
   ]).

-ignore_xref(
   [init/1,
    system_code_change/4,
    system_continue/3,
    system_terminate/4,
    write_debug/3
   ]).

%%-------------------------------------------------------------------
%% Macro Definitions
%%-------------------------------------------------------------------

-define(time_span(), 1). % in seconds
-define(ms_time_span(), 1000). % in milliseconds
-define(native_time_span(), (erlang:convert_time_unit(?time_span(), seconds, native))). % in native units

-define(DEFAULT_MAX_RATE, 100).

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(state, {
          category :: atom(),
          event_type :: term(),
          window = queue:new() :: queue:queue(event()),
          window_size = 0 :: non_neg_integer(),
          sampled_counter = 0 :: non_neg_integer()
         }).
-type state() :: state().

-type event() :: {timestamp(), decision()}.
-type timestamp() :: integer().
-type decision() :: accept | drop.

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec start_link(atom(), term()) -> {ok, pid()} | {error, {already_started, pid()}}.
start_link(Category, EventType) ->
    proc_lib:start_link(?MODULE, init, [{self(), [Category, EventType]}]).

-spec ask(atom(), term(), fun ((integer(), decision(), float())
        -> term()), [deigma:ask_opt()]) -> term() | no_return().
ask(Category, EventType, EventFun, Opts) ->
    MaxRate = proplists:get_value(max_rate, Opts, ?DEFAULT_MAX_RATE),
    Pid = lookup_or_start(Category, EventType),
    Mon = monitor(process, Pid),
    Tag = Mon,
    Pid ! {ask, self(), Tag, EventFun, MaxRate},
    receive
        {Tag, Reply} ->
            demonitor(Mon, [flush]),
            case Reply of
                {result, Result} ->
                    Result;
                {exception, Class, Reason, Stacktrace} ->
                    erlang:raise(Class, Reason, Stacktrace)
            end;
        {'DOWN', Mon, process, _Pid, Reason} when Reason =:= noproc; Reason =:= normal ->
            % inactive process stopped; ask again
            ask(Category, EventType, EventFun, Opts);
        {'DOWN', Mon, process, _Pid, Reason} ->
            error({category_stopped, Reason})
    end.

%%-------------------------------------------------------------------
%% OTP Function Definitions
%%-------------------------------------------------------------------

-spec init({pid(), [atom() | term(), ...]}) -> no_return().
init({Parent, [Category, EventType]}) ->
    Debug = sys:debug_options([]),
    Server = registered_name(EventType),
    case deigma_proc_reg:register(Category, Server, self()) of
        ok ->
            proc_lib:init_ack(Parent, {ok, self()}),
            State = #state{ category = Category, event_type = EventType },
            loop(Parent, Debug, State);
        {error, {already_registered, Pid}} ->
            proc_lib:init_ack(Parent, {error, {already_started, Pid}}),
            exit(normal)
    end.

-spec system_code_change(state(), module(), term(), term()) -> {ok, state()}.
system_code_change(State, _Module, _OldVsn, _Extra) when is_record(State, state) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_code_change-4
    {ok, State}.

-spec system_continue(pid(), [sys:dbg_opt()], state()) -> no_return().
system_continue(Parent, Debug, State) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_continue-3
    loop(Parent, Debug, State).

-spec system_terminate(term(), pid(), list(), state()) -> no_return().
system_terminate(Reason, _Parent, _Debug, _State) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_terminate-4
    exit(Reason).

-spec write_debug(io:device(), term(), term()) -> ok.
write_debug(Dev, Event, Name) ->
    % called by sys:handle_debug().
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

%%-------------------------------------------------------------------
%% Internal Function Definitions
%%-------------------------------------------------------------------

registered_name(EventType) ->
    {?MODULE, EventType}.

lookup_or_start(Category, EventType) ->
    Server = registered_name(EventType),
    case deigma_proc_reg:whereis(Category, Server) of
        undefined ->
            case start(Category, EventType) of
                {ok, Pid} ->
                    Pid;
                {error, {already_started, ExistingPid}} ->
                    ExistingPid
            end;
        Pid ->
            Pid
    end.

start(Category, EventType) ->
    deigma_event_window_sup:start_child(Category, [EventType]).

loop(Parent, Debug, State) ->
    receive
        Msg ->
            Now = erlang:monotonic_time(),
            UpdatedState = purge_expired(Now, State),
            handle_message(Now, Msg, Parent, Debug, UpdatedState)
    after
        ?ms_time_span() ->
            exit(normal)
    end.

handle_message(_Now, {system, From, Request}, Parent, Debug, State) ->
    sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
handle_message(Now, Msg, Parent, Debug, State) ->
    UpdatedDebug = sys:handle_debug(Debug, fun ?MODULE:write_debug/3, ?MODULE, {in, Msg}),
    UpdatedState = handle_nonsystem_msg(Now, Msg, State),
    loop(Parent, UpdatedDebug, UpdatedState).

handle_nonsystem_msg(Now, {ask, From, Tag, EventFun, MaxRate}, State) ->
    Window = State#state.window,
    WindowSize = State#state.window_size,
    SampledCounter = State#state.sampled_counter,

    {UpdatedWindowSize, UpdatedSampledCounter, Decision} =
        handle_sampling(WindowSize, SampledCounter, MaxRate),
    UpdatedWindow = queue:in({Now, Decision}, Window),

    SampleRate = UpdatedSampledCounter / UpdatedWindowSize,
    _ = try EventFun(Now, Decision, SampleRate) of
            Result ->
                From ! {Tag, {result, Result}}
        catch
            Class:Reason ->
                Stacktrace = erlang:get_stacktrace(),
                From ! {Tag, {exception, Class, Reason, Stacktrace}}
        end,

    State#state{ window = UpdatedWindow,
                 window_size = UpdatedWindowSize,
                 sampled_counter = UpdatedSampledCounter
               }.

purge_expired(Now, State) ->
    Window = State#state.window,
    WindowSize = State#state.window_size,
    SampledCounter = State#state.sampled_counter,
    TimeFloor = Now - ?native_time_span(),
    {UpdatedWindow, UpdatedWindowSize, UpdatedSampledCounter} =
        purge_expired(TimeFloor, Window, WindowSize, SampledCounter),
    State#state{
      window = UpdatedWindow,
      window_size = UpdatedWindowSize,
      sampled_counter = UpdatedSampledCounter
     }.

purge_expired(TimeFloor, Window, WindowSize, SampledCounter) ->
    case queue:peek(Window) of
        {value, {EventTimestamp, EventDecision}} when EventTimestamp < TimeFloor ->
            UpdatedWindow = queue:drop(Window),
            UpdatedWindowSize = WindowSize - 1,
            case EventDecision of
                accept ->
                    UpdatedSampledCounter = SampledCounter - 1,
                    purge_expired(
                      TimeFloor, UpdatedWindow, UpdatedWindowSize, UpdatedSampledCounter);
                drop ->
                    purge_expired(
                      TimeFloor, UpdatedWindow, UpdatedWindowSize, SampledCounter)
            end;
        _ ->
            {Window, WindowSize, SampledCounter}
    end.

handle_sampling(WindowSize, SampledCounter, MaxRate) when SampledCounter >= MaxRate ->
    {WindowSize + 1, SampledCounter, drop};
handle_sampling(WindowSize, SampledCounter, _MaxRate) ->
    {WindowSize + 1, SampledCounter + 1, accept}.
