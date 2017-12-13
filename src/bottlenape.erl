%% Copyright (c) 2017 Guilherme Andrade <bottlenape@gandrade.net>
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

-module(bottlenape).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([hit/2]).                       -ignore_xref({hit,2}).
-export([child_spec/1]).                -ignore_xref({child_spec,1}).
-export([start/1]).                     -ignore_xref({start,1}).
-export([start_link/1]).                -ignore_xref({start_link,1}).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).
-define(SERVER, ?MODULE).
-define(REFRESH_PERIOD_AVG, (timer:seconds(1))). % in milliseconds
-define(REFRESH_PERIOD_STDDEV, 300). % in milliseconds
-define(MAX_PER_SECOND, 1000). % TODO make configurable
-define(SRATE_DEPTH, (1 bsl 16)).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-record(state, {
          table :: ets:tab()
         }).
-type state() :: #state{}.

-record(limiter, {
          id :: term(),
          hits :: non_neg_integer(),
          sample_rate :: int_sample_rate(), % integer instead of float so we can use update_counter/4
          last_hit_ts :: integer() % native time unit
         }).
-type limiter() :: #limiter{}.

-type int_sample_rate() :: 1..16#FFFF. % 1..?SRATE_DEPTH.

-type child_spec(Context) ::
        #{ id := {?MODULE, Context},
           start := {?MODULE, start_link, [Context, ...]} }.
-export_type([child_spec/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Returns a child specification for instantiating
%% a particular `Context' under a supervisor.
%% @see start/1
%% @see start/2
-spec child_spec(Context) -> child_spec(Context)
        when Context :: atom().
child_spec(Context) ->
    #{ id => {?MODULE, Context},
       start => {?MODULE, start_link, [Context]}
     }.

%% @doc Reports one more hit for event `Id' within context `Context'.
%% Returns `{ok, SampleRate}' if the event was accepted with `0 < SampleRate =< 1' sampling,
%% or `drop' if the event was rejected.
%% `Context' must refer to a previously created context using `child_spec/1',
%% `start/1' or `start_link/1'.
-spec hit(Context, Id) -> {ok, SampleRate} | drop
        when Context :: atom(),
             Id :: term(),
             SampleRate :: float().
hit(Context, Id) ->
    Table = table(Context),
    [_NewHits, SampleRate] =
        ets:update_counter(Table, Id, [{#limiter.hits,+1}, {#limiter.sample_rate,0}],
                           new_limiter(Id)),

    case SampleRate =:= ?SRATE_DEPTH of
        true ->
            {ok, 1.0};
        false ->
            case rand:uniform(?SRATE_DEPTH) < SampleRate of
                true ->
                    {ok, SampleRate / ?SRATE_DEPTH};
                false ->
                    drop
            end
    end.

%% @doc Creates a standalone `Context' process, that is,
%% a bottlenape process that is not part of a supervision tree
%% and thus has no supervisor.
%% @see child_spec/1
%% @see start_link/1
-spec start(Context) -> {ok, pid()}
        when Context :: atom().
start(Context) ->
    gen_server:start(?CB_MODULE, [Context], []).

%% @doc Creates a `Context' process as part of a supervision tree.
%% This function is to be called, directly or indirectly, by the supervisor.
%% For example, it ensures that the gen_server process is linked to the supervisor.
%% @see child_spec/1
%% @see start/1
-spec start_link(Context) -> {ok, pid()}
        when Context :: atom().
start_link(Context) ->
    gen_server:start_link(?CB_MODULE, [Context], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%% @private
-spec init([atom(), ...]) -> {ok, state()}.
init([Context]) ->
    Table = table(Context),
    _ = ets:new(Table, [named_table, public,
                        %{read_concurrency, true},
                        {write_concurrency, true},
                        {keypos, #limiter.id}]),
    _ = erlang:send_after(refresh_period(), self(), {refresh, monotonic_now()}),
    {ok, #state{ table = Table }}.

%% @private
-spec handle_call(term(), {pid(), reference()}, state()) -> {noreply, state()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({refresh, LastRefreshTs}, State) ->
    NowBefore = monotonic_now(),
    refresh(LastRefreshTs, State#state.table),
    NowAfter = monotonic_now(),
    NewLastRefreshTs = (NowAfter + NowBefore) div 2,
    erlang:send_after(refresh_period(), self(), {refresh, NewLastRefreshTs}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec table(atom()) -> atom().
table(Context) ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Context)).

-spec new_limiter(term()) -> limiter().
new_limiter(Id) ->
    #limiter{ id = Id,
              hits = 1,
              sample_rate = ?SRATE_DEPTH,
              last_hit_ts = erlang:monotonic_time() }.

-spec refresh(integer(), ets:tab()) -> ok.
refresh(LastRefreshTs, Table) ->
    Now = monotonic_now(),
    TimeElapsed = Now - LastRefreshTs,
    SecondsElapsed = TimeElapsed / monotonic_seconds(1),
    HitCeiling = SecondsElapsed * ?MAX_PER_SECOND,
    MinimumLastHitTs = Now - monotonic_seconds(10),
    ets:foldl(
      fun (Limiter, Acc)
            when Limiter#limiter.hits =:= 0, Limiter#limiter.last_hit_ts < MinimumLastHitTs ->
              ets:delete(Table, Limiter#limiter.id),
              Acc;
          (Limiter, Acc)
            when Limiter#limiter.hits =:= 0, Limiter#limiter.sample_rate =/= ?SRATE_DEPTH ->
              ets:update_element(Table, Limiter#limiter.id, {#limiter.sample_rate, ?SRATE_DEPTH}),
              Acc;
          (Limiter, Acc)
            when Limiter#limiter.hits =/= 0 ->
              NewSampleRate = trunc(min(1.0, (HitCeiling / Limiter#limiter.hits)) * ?SRATE_DEPTH),
              NewWeightedSampleRate = weighted_sample_rate(Limiter#limiter.sample_rate, NewSampleRate),
              ets:update_element(Table, Limiter#limiter.id,
                                 [{#limiter.hits, 0},
                                  {#limiter.sample_rate, NewWeightedSampleRate},
                                  {#limiter.last_hit_ts, Now}]),
              Acc;
          (_Limiter, Acc) ->
              Acc
      end,
      ok, Table).

-spec weighted_sample_rate(int_sample_rate(), int_sample_rate()) -> int_sample_rate().
weighted_sample_rate(PrevSampleRate, NewSampleRate) ->
    ((PrevSampleRate * 8) + (NewSampleRate * 2)) div 10.

-spec monotonic_now() -> integer().
monotonic_now() ->
    erlang:monotonic_time().

monotonic_seconds(Seconds) ->
    erlang:convert_time_unit(Seconds, seconds, native).

-spec refresh_period() -> pos_integer().
refresh_period() ->
    max(1, trunc( rand_normal(?REFRESH_PERIOD_AVG, ?REFRESH_PERIOD_STDDEV) )).

rand_normal(Mean, StdDev) ->
    Mean + (StdDev * rand:normal()).
