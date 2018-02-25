-module(deigma_overload_monitor).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]). -ignore_xref({start_link, 0}).
-export([report_window_timeout/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).
-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

% in seconds
-define(SCAN_INTERVAL_MEAN, 5).
-define(SCAN_INTERVAL_STDDEV, 1).

% per second
-define(OVERLOAD_HIGH_WATERMARK, 10).
-define(OVERLOAD_LOW_WATERMARK,   5).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-record(state, {
          overloaded :: gb_sets:set(term())
         }).

-record(window, {
          event_type :: term(),
          timeout_counter = 0 :: non_neg_integer()
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?CB_MODULE, [], []).

report_window_timeout(EventType) ->
    UpdateOp = {#window.timeout_counter, +1},
    Default = #window{},
    ets:update_counter(?TABLE, EventType, UpdateOp, Default).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    EtsOpts = [named_table, public, {keypos,#window.event_type},
               {write_concurrency,true}],
    _ = ets:new(?TABLE, EtsOpts),
    schedule_scan_timer(),
    {ok, #state{ overloaded = gb_sets:empty() }}.

handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

handle_info({scan, ScheduledTs}, State) ->
    TimeElapsed = erlang:monotonic_time() - ScheduledTs,
    SecondsElapsed = TimeElapsed / erlang:convert_time_unit(1, seconds, native),
    PreviouslyOverloaded = State#state.overloaded,
    CurrentlyOverloaded = currently_overloaded(PreviouslyOverloaded, SecondsElapsed),
    ets:delete_all_objects(?TABLE),
    NewlyOverloaded = gb_sets:subtract(CurrentlyOverloaded, PreviouslyOverloaded),
    NewlyUnburdened = gb_sets:subtract(PreviouslyOverloaded, CurrentlyOverloaded),
    gb_sets_foreach(fun handle_newly_overloaded/1, NewlyOverloaded),
    gb_sets_foreach(fun handle_newly_unburdened/1, NewlyUnburdened),
    UpdatedState = State#state{ overloaded = CurrentlyOverloaded },
    schedule_scan_timer(),
    {noreply, UpdatedState};
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

schedule_scan_timer() ->
    Now = erlang:monotonic_time(),
    erlang:send_after(scan_interval_ms(), self(), {scan, Now}).

scan_interval_ms() ->
    InSeconds = max(0, ?SCAN_INTERVAL_MEAN + (math:sqrt(?SCAN_INTERVAL_STDDEV) * rand:normal())),
    trunc(InSeconds * 1000).

currently_overloaded(PreviouslyOverloaded, SecondsElapsed) ->
    HighWatermark = ?OVERLOAD_HIGH_WATERMARK * SecondsElapsed,
    LowWatermark = ?OVERLOAD_LOW_WATERMARK * SecondsElapsed,
    ets:foldl(
      fun (Window, Acc) ->
              EventType = Window#window.event_type,
              TimeoutCounter = Window#window.timeout_counter,
              WasOverloaded = gb_sets:is_member(EventType, PreviouslyOverloaded),
              IsOverloaded = TimeoutCounter >= HighWatermark,

              if WasOverloaded andalso (TimeoutCounter >= LowWatermark) ->
                     gb_sets:add(EventType, Acc);
                 IsOverloaded ->
                     gb_sets:add(EventType, Acc);
                 true ->
                     Acc
              end
      end,
      gb_sets:new(), ?TABLE).

gb_sets_foreach(Fun, Set) ->
    List = gb_sets:to_list(Set),
    lists:foreach(Fun, List).

handle_newly_overloaded(EventType) ->
    _ = deigma_window_manager:mark_as_overloaded(EventType),
    log_overload_detected(EventType).

handle_newly_unburdened(EventType) ->
    _ = deigma_window_manager:unmark_as_overloaded(EventType),
    log_unburden_detected(EventType).

log_overload_detected(EventType) ->
    Report =
        [{type, deigma_window_overloaded},
         {window, EventType}
        ],
    error_logger:warning_report(Report).

log_unburden_detected(EventType) ->
    Report =
        [{type, deigma_window_ok},
         {window, EventType}
        ],
    error_logger:info_report(Report).
