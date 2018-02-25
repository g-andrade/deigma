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
-compile([inline]).

% based on https://gist.github.com/marcelog/97708058cd17f86326c82970a7f81d40#file-simpleproc-erl

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [start/0,
    report/2,
    start_link/0
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

-define(INACTIVITY_TIMEOUT, (erlang:convert_time_unit(5, seconds, native))).
-define(INACTIVITY_CHECK_PERIOD, (timer:seconds(2))).

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(state, {
          window = queue:new() :: queue:queue(event()),
          window_size = 0 :: non_neg_integer(),
          sample_size = 0 :: non_neg_integer(),
          time_span = erlang:convert_time_unit(1, seconds, native) :: timestamp(),
          last_active = erlang:monotonic_time() :: timestamp()
         }).
-type state() :: state().

-type event() :: {timestamp(), decision()}.
-type timestamp() :: integer().
-type decision() :: accept | drop.

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec start() -> {ok, pid()}.
%% @private
start() ->
    deigma_window_sup:start_child([]).

-spec report(pid(), non_neg_integer() | infinity) ->
        {accept | drop, float()} |
        window_stopped.
%% @private
report(WindowPid, Limit) ->
    WindowMonitor = monitor(process, WindowPid),
    WindowPid ! {report, self(), Limit},
    receive
        {WindowPid, WindowSize, SampleSize, Decision} ->
            demonitor(WindowMonitor, [flush]),
            SampleRate = SampleSize / WindowSize,
            {Decision, SampleRate};
        {'DOWN', WindowMonitor, process, _Pid, Reason} ->
            case Reason of
                noproc -> window_stopped;
                normal -> window_stopped;
                Other -> error({deigma_window_crashed, Other})
            end
    end.

-spec start_link() -> {ok, pid()}.
%% @private
start_link() ->
    proc_lib:start_link(?MODULE, init, [[self()]]).

%%-------------------------------------------------------------------
%% OTP Function Definitions
%%-------------------------------------------------------------------

-spec init([pid() | term(), ...]) -> no_return().
%% @private
init([Parent]) ->
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    erlang:send_after(?INACTIVITY_CHECK_PERIOD, self(), check_inactivity),
    %fprof:apply(fun loop/3, [Parent, Debug, State]).
    loop(Parent, Debug, #state{}).

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

loop(Parent, Debug, State) ->
    receive
        %{system, _From, _Request} = Msg ->
        %    handle_message(Msg, Parent, Debug, State);
        Msg ->
            handle_message(Msg, Parent, Debug, State)
    end.

handle_message({report, From, Limit}, Parent, Debug, State) ->
    handle_report(From, Limit, Parent, Debug, State);
handle_message(check_inactivity, Parent, Debug, State) ->
    check_inactivity(Parent, Debug, State);
handle_message({system, From, Request}, Parent, Debug, State) ->
    sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State).

handle_report(From, Limit, Parent, Debug, State) ->
    Window = State#state.window,
    Timespan = State#state.time_span,
    WindowSize = State#state.window_size,
    SampleSize = State#state.sample_size,
    {PurgedWindow, PurgedWindowSize, PurgedSampleSize} =
        purge_events(Timespan, Window, WindowSize, SampleSize),

    {UpdatedWindowSize, UpdatedSampleSize, Decision} =
        handle_sampling(PurgedWindowSize, PurgedSampleSize, Limit),
    Now = erlang:monotonic_time(),
    UpdatedWindow = queue:in({Now, Decision}, PurgedWindow),
    From ! {self(), UpdatedWindowSize, UpdatedSampleSize, Decision},
    UpdatedState =
        State#state{ window = UpdatedWindow,
                     window_size = UpdatedWindowSize,
                     sample_size = UpdatedSampleSize,
                     last_active = Now
                   },
    loop(Parent, Debug, UpdatedState).

purge_events(_Timespan, Window, WindowSize, SampleSize) when WindowSize =:= 0 ->
    {Window, WindowSize, SampleSize};
purge_events(Timespan, Window, WindowSize, SampleSize) ->
    MinTimestamp = erlang:monotonic_time() - Timespan,
    {OldestTimestamp, OldestDecision} = queue:get(Window),
    case OldestTimestamp =< MinTimestamp of
        true ->
            UpdatedWindow = queue:drop(Window),
            case OldestDecision =:= accept of
                true ->
                    purge_events(Timespan, UpdatedWindow, WindowSize - 1, SampleSize - 1);
                _ ->
                    purge_events(Timespan, UpdatedWindow, WindowSize - 1, SampleSize)
            end;
        _ ->
            {Window, WindowSize, SampleSize}
    end.

check_inactivity(Parent, Debug, State) ->
    Now = erlang:monotonic_time(),
    case Now - State#state.last_active >= ?INACTIVITY_TIMEOUT of
        true ->
            exit(normal);
        false ->
            erlang:send_after(?INACTIVITY_CHECK_PERIOD, self(), check_inactivity),
            loop(Parent, Debug, State)
    end.

handle_sampling(WindowSize, SampleSize, Limit) when SampleSize >= Limit ->
    {WindowSize + 1, SampleSize, drop};
handle_sampling(WindowSize, SampleSize, _Limit) when SampleSize =:= WindowSize ->
    {WindowSize + 1, SampleSize + 1, accept};
handle_sampling(WindowSize, SampleSize, _Limit) ->
    NewWindowSize = WindowSize + 1,
    TentativeNewSampleSize = SampleSize + 1,
    case rand:uniform(NewWindowSize) =< TentativeNewSampleSize of
        true ->
            {NewWindowSize, TentativeNewSampleSize, accept};
        false ->
            {NewWindowSize, SampleSize, drop}
    end.
