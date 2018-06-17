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
child_spec(Category) ->
    #{ id => {deigma, Category},
       start => {?MODULE, start_link, [Category]},
       type => supervisor
     }.

-spec start(Category) -> {ok, pid()} | {error, term()}
        when Category :: atom().
start(Category) ->
    deigma_sup:start_child([Category]).

-spec stop(Category) -> ok | {error, not_started}
        when Category :: atom().
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
        -> {ok, {supervisor:sup_flags(), [supervisor:child_spec(), ...]}}.
%% @private
init([Category]) ->
    SupFlags = #{ strategy => rest_for_one },
    ChildSpecs =
        [#{ id => proc_reg,
            start => {deigma_proc_reg, start_link, [Category]}
          },
         #{ id => event_windows,
            start => {deigma_event_window_sup, start_link, [Category]},
            type => supervisor
          }],
    {ok, {SupFlags, ChildSpecs}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

default_ask_fun(_Timestamp, Decision, SampleRate) ->
    {Decision, SampleRate}.
