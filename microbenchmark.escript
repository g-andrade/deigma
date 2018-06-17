-module(microbenchmark).
-mode(compile).

-export([main/1]).

-define(NR_OF_WORKERS, 100).

main([]) ->
    Category = microbenchmarking,
    NrOfWorkers = ?NR_OF_WORKERS,
    NrOfCalls = 4000000,
    {ok, _} = application:ensure_all_started(deigma),
    {ok, _} = application:ensure_all_started(sasl),
    {ok, _} = deigma:start(wowow),
    do_it(Category, NrOfWorkers, NrOfCalls).

do_it(Category, NrOfWorkers, NrOfCalls) ->
    NrOfCallsPerWorker = NrOfCalls div NrOfWorkers,
    Parent = self(),
    Pids = [spawn(fun () -> run_worker(Category, Nr, Parent, NrOfCallsPerWorker) end)
            || Nr <- lists:seq(1, NrOfWorkers)],
    WithMonitors = [{Pid, monitor(process, Pid)} || Pid <- Pids],
    io:format("running benchmarks... (~p calls using ~p workers)~n",
              [NrOfCalls, NrOfWorkers]),
    wait_for_workers(WithMonitors, []).

wait_for_workers([], ResultAcc) ->
    UniqueDeigmaResults = lists:usort( lists:flatten([maps:keys(M) || M <- ResultAcc]) ),
    lists:foreach(
      fun (DeigmaResult) ->
              Counts = [maps:get(DeigmaResult, M, 0) || M <- ResultAcc],
              TotalCount = trunc(lists:sum(Counts)),
              io:format("achieved an average of ~p '~p' results per second~n",
                        [TotalCount, DeigmaResult])
      end,
      UniqueDeigmaResults),
    erlang:halt();
wait_for_workers(WithMonitors, ResultAcc) ->
    receive
        {worker_result, Pid, Result} ->
            {value, {Pid, Monitor}, UpdatedWithMonitors} = lists:keytake(Pid, 1, WithMonitors),
            demonitor(Monitor, [flush]),
            UpdatedResultsAcc = [Result | ResultAcc],
            wait_for_workers(UpdatedWithMonitors, UpdatedResultsAcc);
        {'DOWN', _Ref, process, _Pid, Reason} ->
            error(Reason)
    end.

run_worker(Category, Nr, Parent, NrOfCalls) ->
    run_worker_loop(Category, Nr, Parent, NrOfCalls,
                    erlang:monotonic_time(), 0, #{}).

run_worker_loop(_Category, _Nr, Parent, NrOfCalls, StartTs,
                Count, CountPerResult) when Count =:= NrOfCalls ->
    EndTs = erlang:monotonic_time(),
    TimeElapsed = EndTs - StartTs,
    NativeTimeRatio = erlang:convert_time_unit(1, seconds, native),
    SecondsElapsed = TimeElapsed / NativeTimeRatio,
    AdjustedCountPerResult =
        maps:map(
          fun (_Result, Count) ->
                  Count / SecondsElapsed
          end,
          CountPerResult),
    Parent ! {worker_result, self(), AdjustedCountPerResult};
run_worker_loop(Category, Nr, Parent, NrOfCalls, StartTs, Count, CountPerResult) ->
    ActorId = abs(erlang:monotonic_time()) rem ?NR_OF_WORKERS,
    {Result, SampleRate} = deigma:ask(wowow, 1),
    %_ = (Count rem 10000 =:= 10) andalso io:format("Stats (~p): ~p~n", [Nr, Stats]),
    UpdatedCountPerResult = maps_increment(Result, +1, CountPerResult),
    run_worker_loop(Category, Nr, Parent, NrOfCalls, StartTs, Count + 1,
                    UpdatedCountPerResult).

maps_increment(Key, Incr, Map) ->
    maps_update_with(
      Key,
      fun (Value) -> Value + Incr end,
      Incr, Map).

ask_handler(Decision, SampleRate) ->
    {Decision, SampleRate}.

-ifdef(POST_OTP18).
maps_update_with(Key, Fun, Init, Map) ->
    maps:update_with(Key, Fun, Init, Map).
-else.
maps_update_with(Key, Fun, Init, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            UpdatedValue = Fun(Value),
            Map#{ Key := UpdatedValue };
        error ->
            Map#{ Key => Init }
    end.
-endif.
