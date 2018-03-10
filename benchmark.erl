-module(benchmark).

-export([do_it/2]).

do_it(NrOfWorkers, NrOfReports) ->
    NrOfReportsPerWorker = NrOfReports div NrOfWorkers,
    Parent = self(),
    Pids = [spawn(fun () -> run_worker(Parent, NrOfReportsPerWorker) end) || _ <- lists:seq(1, NrOfWorkers)],
    WithMonitors = [{Pid, monitor(process, Pid)} || Pid <- Pids],
    wait_for_workers(WithMonitors, []).

wait_for_workers([], ResultAcc) ->
    TotalCount = trunc( lists:sum(ResultAcc) ),
    {ok, TotalCount};
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

run_worker(Parent, NrOfReports) ->
    EventType = event,
    %EventType = {event, rand:uniform(4)},
    run_worker_loop(Parent, NrOfReports, EventType, erlang:monotonic_time(), 0).

run_worker_loop(Parent, NrOfReports, _EventType, StartTs, Count) when Count =:= NrOfReports ->
    EndTs = erlang:monotonic_time(),
    TimeElapsed = EndTs - StartTs,
    PerSecond = (Count * erlang:convert_time_unit(1, second, native)) / TimeElapsed,
    Parent ! {worker_result, self(), PerSecond};
run_worker_loop(Parent, NrOfReports, EventType, StartTs, Count) ->
    {_Decision, _SampleRate} = deigma:report(EventType),
    run_worker_loop(Parent, NrOfReports, EventType, StartTs, Count + 1).
