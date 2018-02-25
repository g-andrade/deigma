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

-module(deigma_window).

% based on https://gist.github.com/marcelog/97708058cd17f86326c82970a7f81d40#file-simpleproc-erl

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [start/1,
    report/2,
    start_link/1
   ]).

-ignore_xref(
   [start_link/0
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

-define(inactivity_timeout(), (timer:seconds(5))). % in milliseconds

-define(TIME_SPAN, 1). % in seconds
-define(native_time_span(), (erlang:convert_time_unit(?TIME_SPAN, seconds, native))). % in native units
-define(report_timeout(), (timer:seconds(?TIME_SPAN))). % in milliseconds

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(state, {
          event_type :: term(),
          window = queue:new() :: queue:queue(event()),
          window_size = 0 :: non_neg_integer(),
          sampled_counter = 0 :: non_neg_integer(),
          timedout_counter = 0 :: non_neg_integer(),
          next_purge_ts :: undefined | timestamp(),
          last_active = erlang:monotonic_time() :: timestamp()
         }).
-type state() :: state().

-type event() :: {timestamp(), decision()}.
-type timestamp() :: integer().
-type decision() :: accept | drop.

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec start(term()) -> {ok, pid()}.
%% @private
start(EventType) ->
    deigma_window_sup:start_child([EventType]).

-spec report(pid(), non_neg_integer() | infinity) ->
        {accept | drop, float()} |
        overloaded |
        stopped.
%% @private
report(WindowPid, MaxPerSecond) ->
    Now = erlang:monotonic_time(),
    WindowMonitor = monitor(process, WindowPid),
    WindowPid ! {report, self(), Now, MaxPerSecond},
    Timeout = ?report_timeout(),
    receive
        {WindowPid, WindowSize, SampledCounter, Decision} ->
            demonitor(WindowMonitor, [flush]),
            SampleRate = SampledCounter / WindowSize,
            {Decision, SampleRate};
        {'DOWN', WindowMonitor, process, _Pid, Reason} ->
            case Reason of
                noproc -> stopped;
                normal -> stoppec;
                Other -> error({deigma_window_crashed, Other})
            end
    after
        Timeout ->
            overloaded
    end.

-spec start_link(term()) -> {ok, pid()}.
%% @private
start_link(EventType) ->
    proc_lib:start_link(?MODULE, init, [[self(), EventType]]).

%%-------------------------------------------------------------------
%% OTP Function Definitions
%%-------------------------------------------------------------------

-spec init([pid() | term(), ...]) -> no_return().
%% @private
init([Parent, EventType]) ->
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    State = #state{ event_type = EventType },
    %fprof:apply(fun loop/3, [Parent, Debug, State]).
    loop(Parent, Debug, State).

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

loop(Parent, Debug, State) when State#state.next_purge_ts =/= undefined ->
    PurgeTimestamp = State#state.next_purge_ts,
    TimeLeft = max(0, PurgeTimestamp - erlang:monotonic_time()),
    Timeout = erlang:convert_time_unit(TimeLeft, native, milli_seconds),
    receive
        Msg ->
            handle_message(Msg, Parent, Debug, State)
    after
        Timeout ->
            purge(Parent, Debug, State)
    end;
loop(Parent, Debug, State) ->
    Timeout = ?inactivity_timeout(),
    receive
        Msg -> handle_message(Msg, Parent, Debug, State)
    after
        Timeout ->
            exit(normal)
    end.

handle_message({report, From, Timestamp, MaxPerSecond}, Parent, Debug, State) ->
    Now = erlang:monotonic_time(),
    case Now >= (Timestamp + ?native_time_span()) of
        true ->
            deigma_overload_monitor:report_window_timeout(State#state.event_type),
            loop(Parent, Debug, State);
        false ->
            handle_report(From, MaxPerSecond, Now, Parent, Debug, State)
    end;
handle_message({system, From, Request}, Parent, Debug, State) ->
    sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State).

handle_report(From, MaxPerSecond, Now, Parent, Debug, State) ->
    Window = State#state.window,
    WindowSize = State#state.window_size,
    SampledCounter = State#state.sampled_counter,

    {UpdatedWindowSize, UpdatedSampledCounter, Decision} =
        handle_sampling(WindowSize, SampledCounter, MaxPerSecond),
    UpdatedWindow = queue:in({Now, Decision}, Window),
    From ! {self(), UpdatedWindowSize, UpdatedSampledCounter, Decision},
    UpdatedState =
        State#state{ window = UpdatedWindow,
                     window_size = UpdatedWindowSize,
                     sampled_counter = UpdatedSampledCounter,
                     last_active = Now
                   },
    ensure_next_purge(Parent, Debug, UpdatedState).

ensure_next_purge(Parent, Debug, State) when State#state.next_purge_ts =:= undefined,
                                             State#state.window_size > 0 ->
    Window = State#state.window,
    {EventTimestamp, _EventDecision} = queue:get(Window),
    NextPurgeTs = EventTimestamp + ?native_time_span(),
    UpdatedState = State#state{ next_purge_ts = NextPurgeTs },
    loop(Parent, Debug, UpdatedState);
ensure_next_purge(Parent, Debug, State) ->
    loop(Parent, Debug, State).

purge(Parent, Debug, State) ->
    Window = State#state.window,
    {{value, {_EventTimestamp, EventDecision}}, UpdatedWindow} = queue:out(Window),
    UpdatedWindowSize = State#state.window_size - 1,
    case EventDecision of
        accept ->
            UpdatedSampledCounter = State#state.sampled_counter - 1,
            UpdatedState =
                State#state{ window = UpdatedWindow,
                             window_size = UpdatedWindowSize,
                             sampled_counter = UpdatedSampledCounter,
                             next_purge_ts = undefined
                           },
            ensure_next_purge(Parent, Debug, UpdatedState);
        drop ->
            UpdatedState =
                State#state{ window = UpdatedWindow,
                             window_size = UpdatedWindowSize,
                             next_purge_ts = undefined
                           },
            ensure_next_purge(Parent, Debug, UpdatedState)
    end.

handle_sampling(WindowSize, SampledCounter, MaxPerSecond) when SampledCounter >= MaxPerSecond ->
    {WindowSize + 1, SampledCounter, drop};
handle_sampling(WindowSize, SampledCounter, _MaxPerSecond) when SampledCounter =:= WindowSize ->
    {WindowSize + 1, SampledCounter + 1, accept};
handle_sampling(WindowSize, SampledCounter, _MaxPerSecond) ->
    NewWindowSize = WindowSize + 1,
    TentativeNewSampledCounter = SampledCounter + 1,
    case rand:uniform(NewWindowSize) =< TentativeNewSampledCounter of
        true ->
            {NewWindowSize, TentativeNewSampledCounter, accept};
        false ->
            {NewWindowSize, SampledCounter, drop}
    end.
