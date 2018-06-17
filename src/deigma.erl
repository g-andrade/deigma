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

-module(deigma).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/1,
    child_spec/1,
    start/1,
    stop/1,
    ask/2,
    ask/3,
    ask/4
   ]).

-ignore_xref(
   [start_link/1,
    child_spec/1,
    start/1,
    stop/1,
    ask/2,
    ask/3,
    ask/4
   ]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------

-export(
   [init/1
   ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(Category) -> {ok, pid()} | {error, term()}
        when Category :: atom().
start_link(Category) ->
    Server = deigma_util:proc_name(?MODULE, Category),
    supervisor:start_link({local,Server}, ?MODULE, [Category]).

-spec child_spec(Category) -> supervisor:child_spec()
        when Category :: atom().
-ifdef(POST_OTP17).
child_spec(Category) ->
    #{ id => {deigma, Category},
       start => {?MODULE, start_link, [Category]},
       type => supervisor
     }.
-else.
child_spec(Category) ->
    {{deigma, Category},
     {?MODULE, start_link, [Category]},
     permanent,
     infinity,
     supervisor,
     [?MODULE]
    }.
-endif.

-spec start(Category) -> {ok, pid()} | {error, term()}
        when Category :: atom().
start(Category) ->
    deigma_sup:start_child([Category]).

-spec stop(Category) -> ok | {error, not_started}
        when Category :: atom().
-ifdef(POST_OTP17).
stop(Category) ->
    Server = deigma_util:proc_name(?MODULE, Category),
    try gen_server:stop(Server, shutdown, infinity) of
        ok -> ok
    catch
        exit:Reason when Reason =:= noproc;
                         Reason =:= normal;
                         Reason =:= shutdown ->
            {error, not_started}
    end.
-else.
stop(Category) ->
    Server = deigma_util:proc_name(?MODULE, Category),
    case whereis(Server) of
        undefined ->
            {error, not_started};
        Pid ->
            Mon = monitor(process, Pid),
            exit(Pid, shutdown),
            receive
                {'DOWN', Mon, process, Pid, Reason} when Reason =:= shutdown ->
                    ok;
                {'DOWN', Mon, process, Pid, Reason} when Reason =:= noproc;
                                                         Reason =:= normal ->
                    {error, not_started};
                {'DOWN', Mon, process, Pid, Reason} ->
                    error(Reason)
            end
    end.
-endif.

-spec ask(Category, EventType) -> {Decision, SampleRate}
        when Category :: atom(),
             EventType :: term(),
             Decision :: accept | drop,
             SampleRate :: float().
ask(Category, EventType) ->
    ask(Category, EventType, fun default_ask_fun/3).

-spec ask(Category, EventType, EventFun | Opts) -> {Decision, SampleRate} | EventFunResult
        when Category :: atom(),
             EventType :: term(),
             EventFun :: fun ((Timestamp, Decision, SampleRate) -> EventFunResult),
             Timestamp :: integer(),
             SampleRate :: float(),
             Decision :: accept | drop,
             EventFunResult :: term(),
             Opts :: [deigma_event_window:opt()].
ask(Category, EventType, EventFun) when is_function(EventFun) ->
    ask(Category, EventType, EventFun, []);
ask(Category, EventType, Opts) ->
    ask(Category, EventType, fun default_ask_fun/3, Opts).

-spec ask(Category, EventType, EventFun, Opts) -> EventFunResult
        when Category :: atom(),
             EventType :: term(),
             EventFun :: fun ((Timestamp, Decision, SampleRate) -> EventFunResult),
             Timestamp :: integer(),
             SampleRate :: float(),
             Decision :: accept | drop,
             EventFunResult :: term(),
             Opts :: [deigma_event_window:opt()].
ask(Category, EventType, EventFun, Opts) ->
    deigma_event_window:ask(Category, EventType, EventFun, Opts).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

-spec init([atom(), ...])
        -> {ok, {{rest_for_one, 10, 1}, [supervisor:child_spec(), ...]}}.
%% @private
init([Category]) ->
    SupFlags = {rest_for_one, 10, 1},
    ChildSpecs =
        [{proc_reg,
          {deigma_proc_reg, start_link, [Category]},
          permanent,
          5000,
          worker,
          [deigma_proc_reg]
         },
         {event_windows,
          {deigma_event_window_sup, start_link, [Category]},
          permanent,
          infinity,
          supervisor,
          [deigma_event_window_sup]
         }],
    {ok, {SupFlags, ChildSpecs}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

default_ask_fun(_Timestamp, Decision, SampleRate) ->
    {Decision, SampleRate}.
