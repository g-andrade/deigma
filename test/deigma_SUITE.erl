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
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO WORK SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(deigma_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-ifdef(RUNNING_ON_CI).
-define(ASK_TEST_DURATION, (timer:seconds(3))).
-else.
-define(ASK_TEST_DURATION, (timer:seconds(5))).
-endif.

%% ------------------------------------------------------------------
%% Enumeration
%% ------------------------------------------------------------------

all() ->
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    GroupNames = [individual_tests],
    [{GroupName, [parallel], individual_test_cases()} || GroupName <- GroupNames].

individual_test_cases() ->
    ModuleInfo = ?MODULE:module_info(),
    {exports, Exports} = lists:keyfind(exports, 1, ModuleInfo),
    [Name || {Name, 1} <- Exports, lists:suffix("_test", atom_to_list(Name))].

%% ------------------------------------------------------------------
%% Initialization
%% ------------------------------------------------------------------

init_per_testcase(_TestCase, Config) ->
    {ok, _} = application:ensure_all_started(sasl),
    {ok, _} = application:ensure_all_started(deigma),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%% ------------------------------------------------------------------
%% Definition
%% ------------------------------------------------------------------

ask_10_1_test(Config) ->
    run_ask_test(10, 1),
    Config.

ask_100_1_test(Config) ->
    run_ask_test(100, 1),
    Config.

ask_1000_1_test(Config) ->
    run_ask_test(1000, 1),
    Config.

ask_10_10_test(Config) ->
    run_ask_test(10, 10),
    Config.

ask_100_10_test(Config) ->
    run_ask_test(100, 10),
    Config.

ask_1000_10_test(Config) ->
    run_ask_test(1000, 10),
    Config.

ask_10_100_test(Config) ->
    run_ask_test(10, 100),
    Config.

ask_100_100_test(Config) ->
    run_ask_test(100, 100),
    Config.

ask_1000_100_test(Config) ->
    run_ask_test(1000, 100),
    Config.

ask_10_1000_test(Config) ->
    run_ask_test(10, 1000),
    Config.

ask_100_1000_test(Config) ->
    run_ask_test(100, 1000),
    Config.

ask_1000_1000_test(Config) ->
    run_ask_test(1000, 1000),
    Config.

default_event_fun_test(_Config) ->
    {ok, _Pid} = deigma:start(default_event_fun_test),
    ?assertEqual({sample,1.0}, deigma:ask(default_event_fun_test, foobar)),
    ?assertEqual({sample,1.0}, deigma:ask(default_event_fun_test, foobar, [])),
    ?assertEqual({sample,1.0}, deigma:ask(default_event_fun_test, foobar, [{max_rate,5000}])),
    ok = deigma:stop(default_event_fun_test),
    {error, not_started} = deigma:stop(default_event_fun_test).

custom_event_fun_test(_Config) ->
    {ok, _Pid} = deigma:start(custom_event_fun_test),
    Ref = make_ref(),
    ?assertEqual(yes,    deigma:ask(custom_event_fun_test, foobar, event_fun({value, yes}))),
    ?assertEqual(no,     deigma:ask(custom_event_fun_test, foobar, event_fun({value, no}))),
    ?assertEqual(self(), deigma:ask(custom_event_fun_test, foobar, event_fun({value, self()}))),
    ?assertEqual(Ref,    deigma:ask(custom_event_fun_test, foobar, event_fun({value, Ref}))),
    ok = deigma:stop(custom_event_fun_test).

crashing_event_fun_test(_Config) ->
    {ok, _Pid} = deigma:start(crashing_event_fun_test),
    ?assertMatch(yes,
                 catch deigma:ask(crashing_event_fun_test, foobar, event_fun({exception, throw, yes}))),
    ?assertMatch(no,
                 catch deigma:ask(crashing_event_fun_test, foobar, event_fun({exception, throw, no}))),
    ?assertMatch({'EXIT', {its_working, _}},
                 catch deigma:ask(crashing_event_fun_test, foobar, event_fun({exception, error, its_working}))),
    ?assertMatch({'EXIT', oh_my},
                 catch deigma:ask(crashing_event_fun_test, foobar, event_fun({exception, exit, oh_my}))),
    ok = deigma:stop(crashing_event_fun_test).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

run_ask_test(NrOfEventTypes, MaxRate) ->
    Category =
        list_to_atom(
          "ask_" ++
          integer_to_list(NrOfEventTypes) ++
          "_" ++
          integer_to_list(MaxRate) ++
          "_test"),
    {ok, _Pid} = deigma:start(Category),
    _ = erlang:send_after(5000, self(), test_over),
    run_ask_test_recur(Category, NrOfEventTypes, MaxRate, []),
    ok = deigma:stop(Category).

run_ask_test_recur(Category, NrOfEventTypes, MaxRate, Acc) ->
    Timeout = rand:uniform(2) - 1,
    receive
        test_over ->
            check_ask_test_results(MaxRate, Acc)
    after
        Timeout ->
            EventType = rand:uniform(NrOfEventTypes),
            {Ts, Decision, SamplingPercentage} =
                deigma:ask(
                  Category, EventType,
                  fun (Ts, Decision, SamplingPercentage) ->
                          {Ts, Decision, SamplingPercentage}
                  end,
                  [{max_rate, MaxRate}]),
            UpdatedAcc = [{Ts, EventType, Decision, SamplingPercentage} | Acc],
            run_ask_test_recur(Category, NrOfEventTypes, MaxRate, UpdatedAcc)
    end.

check_ask_test_results(MaxRate, Results) ->
    ResultsPerEventType =
        lists:foldl(
          fun ({Ts, EventType, Decision, SamplingPercentage}, Acc) ->
                  maps_update_with(
                    EventType,
                    fun (Events) -> [{Ts, Decision, SamplingPercentage} | Events] end,
                    [{Ts, Decision, SamplingPercentage}],
                    Acc)
          end,
          #{}, Results),

    lists:foreach(
      fun ({_EventType, Events}) ->
              check_ask_test_decisions(MaxRate, Events),
              check_ask_test_rates(Events)
      end,
      lists:keysort(1, maps:to_list(ResultsPerEventType))).

check_ask_test_decisions(MaxRate, Events) ->
    check_ask_test_decisions(MaxRate, Events, [], 0, 0).

check_ask_test_decisions(_MaxRate, [], _Acc, RightDecisions, WrongDecisions) ->
    ct:pal("RightDecisions ~p, WrongDecisions ~p", [RightDecisions, WrongDecisions]),
    ?assert(WrongDecisions / (RightDecisions + WrongDecisions) < 0.01);
check_ask_test_decisions(MaxRate, [Event | Next], Prev, RightDecisions, WrongDecisions) ->
    {Ts, Decision, _SamplingPercentage} = Event,
    RelevantPrev = relevant_history(Ts, Prev),
    CountPerDecision = count_history_decisions(RelevantPrev),
    PrevSamples = maps:get(sample, CountPerDecision),
    RightDecision =
        case PrevSamples >= MaxRate of
            true -> drop;
            false -> sample
        end,

    case RightDecision =:= Decision of
        false ->
            check_ask_test_decisions(MaxRate, Next, [Event | RelevantPrev],
                                     RightDecisions, WrongDecisions + 1);
        true ->
            check_ask_test_decisions(MaxRate, Next, [Event | RelevantPrev],
                                     RightDecisions + 1, WrongDecisions)
    end.

relevant_history(Ts, Prev) ->
    TsFloor = Ts - erlang:convert_time_unit(1, seconds, native),
    lists:takewhile(
      fun ({EntryTs, _Decision, _SamplingPercentage}) ->
              EntryTs >= TsFloor
      end,
      Prev).

count_history_decisions(Prev) ->
    lists:foldl(
      fun ({_Ts, Decision, _SamplingPercentage}, Acc) ->
              maps_update_with(
                Decision,
                fun (Val) -> Val + 1 end,
                Acc)
      end,
      #{ sample => 0,
         drop => 0
       },
      Prev).

check_ask_test_rates(Events) ->
    check_ask_test_rates(Events, []).

check_ask_test_rates([], _Prev) ->
    ok;
check_ask_test_rates([Event | Next], Prev) ->
    {Ts, Decision, SamplingPercentage} = Event,
    ?assert(SamplingPercentage >= 0 andalso SamplingPercentage =< 1),
    RelevantPrev = relevant_history(Ts, Prev),
    CountPerDecision = count_history_decisions(RelevantPrev),
    PrevSamples = maps:get(sample, CountPerDecision),
    PrevDrops = maps:get(drop, CountPerDecision),
    ct:pal("PrevSamples ~p, PrevDrops ~p", [PrevSamples, PrevDrops]),
    Total = PrevSamples + PrevDrops + 1,
    RealSamplingPercentage =
        if Decision =:= sample ->
               (PrevSamples + 1) / Total;
           Decision =:= drop ->
               (PrevSamples / Total)
        end,
    ?assertEqual(RealSamplingPercentage, SamplingPercentage),
    check_ask_test_rates(Next, [Event | Prev]).

event_fun({value, Value}) ->
    fun (_Timestamp, Decision, SamplingPercentage) ->
            ?assert(lists:member(Decision, [sample, drop])),
            ?assert(SamplingPercentage >= 0 andalso SamplingPercentage =< 1),
            Value
    end;
event_fun({exception, Class, Reason}) ->
    fun (_Timestamp, Decision, SamplingPercentage) ->
            ?assert(lists:member(Decision, [sample, drop])),
            ?assert(SamplingPercentage >= 0 andalso SamplingPercentage =< 1),
            erlang:raise(Class, Reason, [])
    end.

maps_update_with(Key, Fun, Map) ->
    maps:update_with(Key, Fun, Map).

maps_update_with(Key, Fun, Init, Map) ->
    maps:update_with(Key, Fun, Init, Map).
