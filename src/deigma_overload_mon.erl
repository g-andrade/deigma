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

-module(deigma_overload_mon).
-compile([inline]).

% based on https://gist.github.com/marcelog/97708058cd17f86326c82970a7f81d40#file-simpleproc-erl

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [start/2,
    measure_window_delay/2,
    start_link/2
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

-define(OVERLOAD_CHECK_INTERVAL, (100)).
-define(MAX_SAMPLES, 100).
-define(AVG_DELAY_OVERLOAD_THRESHOLD,
        (erlang:convert_time_unit(1, milli_seconds, native))).

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(state, {
          window_monitor :: reference(),
          event_type :: term(),
          last_delays :: queue:queue(Delay :: integer()),
          last_delays_sz :: non_neg_integer(),
          last_delays_avg :: float(),
          is_overloaded :: boolean()
         }).
-type state() :: state().

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec start(term(), pid()) -> {ok, pid()}.
%% @private
start(EventType, WindowPid) ->
    deigma_overload_mon_sup:start_child([EventType, WindowPid]).

-spec measure_window_delay(pid(), non_neg_integer()) -> ok.
measure_window_delay(MonPid, StartTs) ->
    MonPid ! {measure_window_delay, StartTs},
    ok.

-spec start_link(term(), pid()) -> {ok, pid()}.
%% @private
start_link(EventType, WindowPid) ->
    proc_lib:start_link(?MODULE, init, [[self(), EventType, WindowPid]]).

%%-------------------------------------------------------------------
%% OTP Function Definitions
%%-------------------------------------------------------------------

-spec init([pid() | term() | pid(), ...]) -> no_return().
%% @private
init([Parent, EventType, WindowPid]) ->
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    State = #state{ window_monitor = monitor(process, WindowPid),
                    event_type = EventType,
                    last_delays = queue:new(),
                    last_delays_sz = 0,
                    last_delays_avg = 0.0,
                    is_overloaded = false
                  },
    schedule_next_overload_check(),
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

loop(Parent, Debug, State) ->
    receive
        %{system, _From, _Request} = Msg ->
        %    handle_message(Msg, Parent, Debug, State);
        Msg ->
            handle_message(Msg, Parent, Debug, State)
    end.

handle_message({measure_window_delay, StartTs}, Parent, Debug, State) ->
    handle_measure_window_delay(StartTs, Parent, Debug, State);
handle_message(check_overload, Parent, Debug, State) ->
    check_overload(Parent, Debug, State);
handle_message({'DOWN', Ref, process, _Pid, _Reason}, _Parent, _Debug, State)
  when Ref =:= State#state.window_monitor ->
    exit(normal);
handle_message({system, From, Request}, Parent, Debug, State) ->
    sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State).

handle_measure_window_delay(StartTs, Parent, Debug, State) ->
    LastDelays = State#state.last_delays,
    LastDelaysSz = State#state.last_delays_sz,
    LastDelaysAvg = State#state.last_delays_avg,
    DelaySample = erlang:monotonic_time() - StartTs,
    {Delays, DelaysSz, DelaysAvg} =
        enqueue_delay_sample(LastDelays, LastDelaysSz, LastDelaysAvg, DelaySample),
    UpdatedState =
        State#state{
          last_delays = Delays,
          last_delays_sz = DelaysSz,
          last_delays_avg = DelaysAvg
         },
    loop(Parent, Debug, UpdatedState).

enqueue_delay_sample(LastDelays, LastDelaysSz, LastDelaysAvg, DelaySample)
  when LastDelaysSz >= ?MAX_SAMPLES ->
    {{value,OldestSample}, Delays} = queue:out(LastDelays),
    OldestSampleWeight = OldestSample / LastDelaysSz,
    DelaysSz = LastDelaysSz - 1,
    RatioAdjustment = (LastDelaysSz / DelaysSz),
    DelaysAvg = (LastDelaysAvg - OldestSampleWeight) * RatioAdjustment,
    enqueue_delay_sample(Delays, DelaysSz, DelaysAvg, DelaySample);
enqueue_delay_sample(LastDelays, LastDelaysSz, LastDelaysAvg, DelaySample) ->
    Delays = queue:in(DelaySample, LastDelays),
    DelaysSz = LastDelaysSz + 1,
    DelaysAvg = ((LastDelaysAvg * (LastDelaysSz / DelaysSz))
                 + (DelaySample * (1 / DelaysSz))),
    {Delays, DelaysSz, DelaysAvg}.

check_overload(Parent, Debug, State) ->
    LastDelaysAvg = State#state.last_delays_avg,
    WasOverloaded = State#state.is_overloaded,
    IsOverloaded = (LastDelaysAvg >= ?AVG_DELAY_OVERLOAD_THRESHOLD),
    case IsOverloaded =/= WasOverloaded of
        true when IsOverloaded ->
            react_to_overload(State#state.event_type, LastDelaysAvg);
        true ->
            react_to_unburdening(State#state.event_type);
        false ->
            ok
    end,
    UpdatedState = State#state{ is_overloaded = IsOverloaded },
    schedule_next_overload_check(),
    loop(Parent, Debug, UpdatedState).

react_to_overload(EventType, AvgDelay) ->
    AvgDelayMs = trunc(AvgDelay / erlang:convert_time_unit(1, milli_seconds, native)),
    error_logger:warning_report(
      [{type, deigma_window_overloaded},
       {event, EventType},
       {delay, AvgDelayMs}
      ]),
    true = deigma_window_manager:mark_as_overloaded(EventType).

react_to_unburdening(EventType) ->
    error_logger:info_report(
      [{type, deigma_window_ok},
       {event, EventType}
      ]),
    true = deigma_window_manager:unmark_as_overloaded(EventType).

schedule_next_overload_check() ->
    erlang:send_after(?OVERLOAD_CHECK_INTERVAL, self(), check_overload).
