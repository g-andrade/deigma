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

-module(deigma_category).

% based on https://gist.github.com/marcelog/97708058cd17f86326c82970a7f81d40#file-simpleproc-erl

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [start_link/1,
    ask/2
   ]).

-ignore_xref(
   [start_link/1
   ]).

%%-------------------------------------------------------------------
%% OTP Exports
%%-------------------------------------------------------------------

-export(
   [init/1,
    system_code_change/4,
    system_continue/3,
    system_terminate/4
   ]).

-ignore_xref(
   [init/1,
    system_code_change/4,
    system_continue/3,
    system_terminate/4
   ]).

%%-------------------------------------------------------------------
%% Macro Definitions
%%-------------------------------------------------------------------

-define(time_span(), 1). % in seconds
-define(native_time_span(), (erlang:convert_time_unit(?time_span(), seconds, native))). % in native units

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(state, {
          category :: term(),
          window = queue:new() :: queue:queue(event()),
          window_size = 0 :: non_neg_integer(),
          sampled_counter = 0 :: non_neg_integer()
         }).
-type state() :: state().

-type event() :: {timestamp(), decision()}.
-type timestamp() :: integer().
-type decision() :: accept | drop.

-type opt() ::
    {max_rate, non_neg_integer() | infinity}.
-export_type([opt/0]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec start_link(term()) -> {ok, pid()} | {error, {already_started, pid()}}.
%% @private
start_link(Category) ->
    proc_lib:start_link(?MODULE, init, [[self(), Category]]).

-spec ask(term(), opt()) ->
        {accept | drop, float()} |
        overloaded |
        stopped.
%% @private
ask(Category, Opts) ->
    MaxRate = proplists:get_value(max_rate, Opts),
    Tag = make_ref(),
    Pid = ensure_server(Category),
    Pid ! {ask, self(), Tag, MaxRate},
    Mon = monitor(process, Pid),
    receive
        {Tag, Reply} ->
            {UpdatedWindowSize, UpdatedSampledCounter, Decision} = Reply,
            SampleRate = UpdatedSampledCounter / UpdatedWindowSize,
            {Decision, SampleRate};
        {'DOWN', Mon, process, _Pid, Reason} when Reason =:= noproc; Reason =:= normal ->
            % inactive process stopped; ask again
            ask(Category, Opts);
        {'DOWN', Mon, process, _Pid, Reason} ->
            error({category_stopped, Reason})
    end.

%%-------------------------------------------------------------------
%% OTP Function Definitions
%%-------------------------------------------------------------------

-spec init([pid() | term(), ...]) -> no_return().
%% @private
init([Parent, Category]) ->
    Debug = sys:debug_options([]),
    Server = server_name(Category),
    case deigma_proc_reg:register(Server, self()) of
        ok ->
            proc_lib:init_ack(Parent, {ok, self()}),
            State = #state{ category = Category },
            loop(Parent, Debug, State);
        {error, {already_registered, Pid}} ->
            proc_lib:init_ack(Parent, {error, {already_started, Pid}}),
            exit(normal)
    end.

-spec system_code_change(state(), module(), term(), term()) -> {ok, state()}.
%% @private
system_code_change(State, _Module, _OldVsn, _Extra) when is_record(State, state) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_code_change-4
    {ok, State}.

-spec system_continue(pid(), [sys:dbg_opt()], state()) -> no_return().
%% @private
system_continue(Parent, Debug, State) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_continue-3
    loop(Parent, Debug, State).

-spec system_terminate(term(), pid(), list(), state()) -> no_return().
%% @private
system_terminate(Reason, _Parent, _Debug, _State) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_terminate-4
    exit(Reason).

%%-------------------------------------------------------------------
%% Internal Function Definitions
%%-------------------------------------------------------------------

server_name(Category) ->
    {?MODULE, Category}.

ensure_server(Category) ->
    Server = server_name(Category),
    case deigma_proc_reg:whereis(Server) of
        undefined ->
            case start_server(Category) of
                {ok, Pid} ->
                    Pid;
                {error, {already_started, ExistingPid}} ->
                    ExistingPid
            end;
        Pid ->
            Pid
    end.

start_server(Category) ->
    deigma_category_sup:start_child([Category]).

loop(Parent, Debug, State) ->
    UpdatedState = purge_expired(State),
    receive
        Msg -> handle_message(Msg, Parent, Debug, UpdatedState)
    end.

handle_message({system, From, Request}, Parent, Debug, State) ->
    sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
handle_message(Msg, Parent, Debug, State) ->
    UpdatedDebug = sys:handle_debug(Debug, fun ?MODULE:write_debug/3, ?MODULE, {in, Msg}),
    UpdatedState = handle_nonsystem_msg(Msg, State),
    loop(Parent, UpdatedDebug, UpdatedState).

handle_nonsystem_msg({ask, From, Tag, MaxRate}, State) ->
    Window = State#state.window,
    WindowSize = State#state.window_size,
    SampledCounter = State#state.sampled_counter,

    Now = erlang:monotonic_time(),
    {UpdatedWindowSize, UpdatedSampledCounter, Decision} =
        handle_sampling(WindowSize, SampledCounter, MaxRate),
    UpdatedWindow = queue:in({Now, Decision}, Window),
    From ! {Tag, {UpdatedWindowSize, UpdatedSampledCounter, Decision}},
    State#state{ window = UpdatedWindow,
                 window_size = UpdatedWindowSize,
                 sampled_counter = UpdatedSampledCounter
               }.

purge_expired(State) ->
    TimeFloor = erlang:monotonic_time() - ?native_time_span(),
    case queue:peek(State#state.window) of
        {value, {EventTimestamp, EventDecision}} when EventTimestamp =< TimeFloor ->
            UpdatedWindow = queue:drop(State#state.window),
            UpdatedWindowSize = State#state.window_size - 1,
            case EventDecision of
                accept ->
                    UpdatedSampledCounter = State#state.sampled_counter - 1,
                    UpdatedState =
                        State#state{ window = UpdatedWindow,
                                     window_size = UpdatedWindowSize,
                                     sampled_counter = UpdatedSampledCounter
                                   },
                    purge_expired(UpdatedState);
                drop ->
                    UpdatedWindow = queue:drop(State#state.window),
                    UpdatedState =
                        State#state{ window = UpdatedWindow,
                                     window_size = UpdatedWindowSize
                                   },
                    purge_expired(UpdatedState)
            end;
        _ ->
            State
    end.

handle_sampling(WindowSize, SampledCounter, MaxRate) when SampledCounter >= MaxRate ->
    {WindowSize + 1, SampledCounter, drop};
handle_sampling(WindowSize, SampledCounter, _MaxRate) when SampledCounter =:= WindowSize ->
    {WindowSize + 1, SampledCounter + 1, accept};
handle_sampling(WindowSize, SampledCounter, _MaxRate) ->
    NewWindowSize = WindowSize + 1,
    TentativeNewSampledCounter = SampledCounter + 1,
    case rand:uniform(NewWindowSize) =< TentativeNewSampledCounter of
        true ->
            {NewWindowSize, TentativeNewSampledCounter, accept};
        false ->
            {NewWindowSize, SampledCounter, drop}
    end.

%inactivity_timeout() ->
%    InSeconds = max(0, ?inactivity_timeout_mean() + (math:sqrt(?inactivity_timeout_stddev()) * rand:normal())),
%    trunc(InSeconds * 1000).
