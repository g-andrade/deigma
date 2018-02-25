-module(deigma_overload_monitor).
-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

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

-define(SCAN_INTERVAL, 2). % in seconds
-define(OVERLOAD_HIGH_WATERMARK, (10 * ?SCAN_INTERVAL)).
-define(OVERLOAD_LOW_WATERMARK, (?OVERLOAD_HIGH_WATERMARK div 2)).

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

handle_info(scan, State) ->
    PreviouslyOverloaded = State#state.overloaded,
    CurrentlyOverloaded = currently_overloaded(PreviouslyOverloaded),
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
    erlang:send_after(timer:seconds(?SCAN_INTERVAL), self(), scan).

currently_overloaded(PreviouslyOverloaded) ->
    PotentiallyOverloadList = potentially_overloaded_list(),
    lists:foldl(
      fun ({EventType, TimeoutCounter}, Acc) ->
              case (gb_sets:is_member(EventType, PreviouslyOverloaded) orelse
                    TimeoutCounter >= ?OVERLOAD_HIGH_WATERMARK)
              of
                  true ->
                      gb_sets:add(EventType, Acc);
                  false ->
                      Acc
              end
      end,
      gb_sets:new(), PotentiallyOverloadList).

potentially_overloaded_list() ->
    MatchSpec =
        ets:fun2ms(
          fun (Window) when Window#window.timeout_counter >= ?OVERLOAD_LOW_WATERMARK ->
                  {Window#window.event_type, Window#window.timeout_counter}
          end),
    List = ets:select(?TABLE, MatchSpec),
    ets:delete_all_objects(?TABLE),
    List.

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
        [{type, deigma_window_overload_alarm},
         {window, EventType},
         {status, on}
        ],
    error_logger:warning_report(Report).

log_unburden_detected(EventType) ->
    Report =
        [{type, deigma_window_overload_alarm},
         {window, EventType},
         {status, off}
        ],
    error_logger:info_report(Report).
