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
-behaviour(gen_server).

% based on https://gist.github.com/marcelog/97708058cd17f86326c82970a7f81d40#file-simpleproc-erl

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [start_link/1,
    table_name/1,
    ask/4,
    get_counters/1,
    decrement/3
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

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(state, {
          category :: atom()
         }).

-record(event_counters, {
          event_type :: term(),
          accepted = 0 :: non_neg_integer(),
          rejected = 0 :: non_neg_integer()
         }).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

start_link(Category) ->
    Server = deigma_util:proc_name(?MODULE, Category),
    gen_server:start_link({local,Server}, ?MODULE, [Category], []).

table_name(Category) ->
    deigma_util:proc_name(?MODULE, Category).

ask(Category, EventType, EventFun, _Opts) ->
    Table = table_name(Category),
    CurrentEventCounters = current_event_counters(Table, EventType),
    AcceptedCount = CurrentEventCounters#event_counters.accepted,
    RejectedCount = CurrentEventCounters#event_counters.rejected,
    StartTs = erlang:monotonic_time(),
    EventFunResult = EventFun(AcceptedCount, RejectedCount),
    {CounterPos, ReturnValue} =
        case EventFunResult of
            accept ->
                {#event_counters.accepted, accepted};
            reject ->
                {#event_counters.rejected, rejected};
            {accept, ExplicitReturnValue} ->
                {#event_counters.accepted, ExplicitReturnValue};
            {reject, ExplicitReturnValue} ->
                {#event_counters.rejected, ExplicitReturnValue}
        end,

    _ = ets:update_counter(Table, EventType, {CounterPos, +1}, #event_counters{}),
    DecrementInterval = erlang:convert_time_unit(1, second, native),
    DecrementTs = StartTs + DecrementInterval,
    ok = deigma_delayed_apply:schedule(Category, DecrementTs,
                                       fun ?MODULE:decrement/3,
                                       [Table, EventType, CounterPos]),
    ReturnValue.

get_counters(Category) ->
    Table = table_name(Category),
    ets:foldl(
      fun (EventCounters, Acc) ->
              EventType = EventCounters#event_counters.event_type,
              Counters =
                #{ accepted => EventCounters#event_counters.accepted,
                   rejected => EventCounters#event_counters.rejected
                 },
              Acc#{ EventType => Counters }
      end,
      #{}, Table).

decrement(Table, EventType, CounterPos) ->
    _ = ets:update_counter(Table, EventType, {CounterPos, -1}),
    true.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Category]) ->
    Table = table_name(Category),
    TableOpts = [named_table, public, {keypos,#event_counters.event_type},
                 {read_concurrency,true}, {write_concurrency,true}],
    _ = ets:new(Table, TableOpts),
    {ok, #state{ category = Category }}.

handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------
%% Internal Function Definitions
%%-------------------------------------------------------------------

current_event_counters(Table, EventType) ->
    case ets:lookup(Table, EventType) of
        [EventCounters] ->
            EventCounters;
        [] ->
            #event_counters{}
    end.
