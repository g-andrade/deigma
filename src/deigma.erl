%% Copyright (c) 2018-2021 Guilherme Andrade
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
%% Record and Type Definitions
%% ------------------------------------------------------------------

-type ask_opt() ::
    {max_rate, non_neg_integer() | infinity}.
-export_type([ask_opt/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Start a deigma instance named `Category' under your own supervisor
%%
%% <ul>
%% <li>`Category' must be an atom</li>
%% </ul>
%%
%% @see child_spec/1
%% @see start/1
-spec start_link(Category) -> {ok, pid()} | {error, term()}
        when Category :: atom().
start_link(Category) ->
    Server = deigma_util:proc_name(?MODULE, Category),
    supervisor:start_link({local,Server}, ?MODULE, [Category]).

%% @doc Declare a deigma instance named `Category' under your own supervisor
%%
%% <ul>
%% <li>`Category' must be an atom</li>
%% </ul>
%%
%% @see start_link/2
%% @see start/1
-spec child_spec(Category) -> supervisor:child_spec()
        when Category :: atom().
child_spec(Category) ->
    #{ id => {deigma, Category},
       start => {?MODULE, start_link, [Category]},
       type => supervisor
     }.

%% @doc Start a deigma instance named `Category'
%%
%% <ul>
%% <li>`Category' must be an atom</li>
%% </ul>
%%
%% @see stop/1
%% @see start_link/1
%% @see child_spec/1
-spec start(Category) -> {ok, pid()} | {error, term()}
        when Category :: atom().
start(Category) ->
    deigma_sup:start_child([Category]).

%% @doc Stop a deigma instance named `Category'
%%
%% <ul>
%% <li>`Category' must be an atom</li>
%% </ul>
%%
%% @see stop/1
%% @see start_link/1
%% @see child_spec/1
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

%% @doc Ask `Category' to sample an `EventType' event
%%
%% <ul>
%% <li>`Category' must be an atom and correspond to an existing deigma instance</li>
%% <li>`EventType' can be any term</li>
%% </ul>
%%
%% Returns:
%% <ul>
%% <li>`{sample, SamplingPercentage}' if the event was sampled</li>
%% <li>`{drop, SamplingPercentage}' if the event was dropped</li>
%% </ul>
%%
%% `SamplingPercentage' is a floating point number between 0.0 and 1.0 representing
%% the percentage of events that were sampled during the last 1000 milliseconds,
%% <b>including</b> the event reported just now.
%%
%% @see ask/3
%% @see ask/4
-spec ask(Category, EventType) -> {Decision, SamplingPercentage}
        when Category :: atom(),
             EventType :: term(),
             Decision :: sample | drop,
             SamplingPercentage :: float().
ask(Category, EventType) ->
    ask(Category, EventType, fun default_ask_fun/3).

%% @doc Ask `Category' to sample an `EventType' event using custom function or overridden options
%%
%% <ul>
%% <li>`Category' must be an atom and correspond to an existing deigma instance</li>
%% <li>`EventType' can be any term</li>
%% <li>`EventFun' must be a function which will receive the following arguments:
%%      <ul>
%%          <li>`Timestamp': Monotonic timestamp in native units at which the event was registered</li>
%%          <li>`Decision': Either `sample' or `drop' depending on whether the event was sampled or not</li>
%%          <li>`SamplingPercentage': a floating point number between 0.0 and 1.0 representing the percentage
%%              of events that were sampled during the last 1000 milliseconds, <b>including</b> the event
%%              reported just now.
%%          </li>
%%      </ul>
%%      It will be called from within the event window for `EventType', which means
%%      it can be used for fullfilling serialisation constraints; at the same time,
%%      performance has to be taken into account (lest the event window become a bottleneck.)
%% </li>
%% <li>`Opts' must be a list of `ask_opt()' items:
%%      <ul>
%%          <li>{`max_rate, MaxRate}': don't sample more than `MaxRate' `EventType' events per
%%              second (defaults to `100')
%%          </li>
%%      </ul>
%% </li>
%% </ul>
%%
%% If called with `EventFun', it will return or throw whathever `EventFun' returns or throws.
%% If called with `Opts', it will return the same as `:ask/2'.
%%
%% @see ask/2
%% @see ask/4
-spec ask(Category, EventType, EventFun | Opts) -> {Decision, SamplingPercentage} | EventFunResult
        when Category :: atom(),
             EventType :: term(),
             EventFun :: fun ((Timestamp, Decision, SamplingPercentage) -> EventFunResult),
             Timestamp :: integer(),
             SamplingPercentage :: float(),
             Decision :: sample | drop,
             EventFunResult :: term(),
             Opts :: [ask_opt()].
ask(Category, EventType, EventFun) when is_function(EventFun) ->
    ask(Category, EventType, EventFun, []);
ask(Category, EventType, Opts) ->
    ask(Category, EventType, fun default_ask_fun/3, Opts).

%% @doc Ask `Category' to sample an `EventType' event using custom function and overridden options
%%
%% <ul>
%% <li>`Category' must be an atom and correspond to an existing deigma instance</li>
%% <li>`EventType' can be any term</li>
%% <li>`EventFun' must be a function which will receive the following arguments:
%%      <ul>
%%          <li>`Timestamp': Monotonic timestamp in native units at which the event was registered</li>
%%          <li>`Decision': Either `sample' or `drop' depending on whether the event was sampled or not</li>
%%          <li>`SamplingPercentage': a floating point number between 0.0 and 1.0 representing the percentage
%%              of events that were sampled during the last 1000 milliseconds, <b>including</b> the event
%%              reported just now.
%%          </li>
%%      </ul>
%%      It will be called from within the event window for `EventType', which means
%%      it can be used for fullfilling serialisation constraints; at the same time,
%%      performance has to be taken into account (lest the event window become a bottleneck.)
%% </li>
%% <li>`Opts' must be a list of `ask_opt()' items:
%%      <ul>
%%          <li>{`max_rate, MaxRate}': don't sample more than `MaxRate' `EventType' events per
%%              second (defaults to `100')
%%          </li>
%%      </ul>
%% </li>
%% </ul>
%%
%% It will return or throw whathever `EventFun' returns or throws.
%%
%% @see ask/2
%% @see ask/3
-spec ask(Category, EventType, EventFun, Opts) -> EventFunResult
        when Category :: atom(),
             EventType :: term(),
             EventFun :: fun ((Timestamp, Decision, SamplingPercentage) -> EventFunResult),
             Timestamp :: integer(),
             SamplingPercentage :: float(),
             Decision :: sample | drop,
             EventFunResult :: term(),
             Opts :: [ask_opt()].
ask(Category, EventType, EventFun, Opts) ->
    deigma_event_window:ask(Category, EventType, EventFun, Opts).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

-spec init([atom(), ...])
        -> {ok, {supervisor:sup_flags(), [supervisor:child_spec(), ...]}}.
%% @private
init([Category]) ->
    SupFlags =
        #{ strategy => rest_for_one,
           intensity => 5,
           period => 1
         },
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

default_ask_fun(_Timestamp, Decision, SamplingPercentage) ->
    {Decision, SamplingPercentage}.
