%% Copyright (c) 2018-2022 Guilherme Andrade
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

-ifdef(E48).
-moduledoc """
Continuous event sampler for Erlang/OTP and Elixir.

`deigma` samples reported events within continuous one-second windows, steadily
adjusting the sampling percentage so the events that seep through stay
representative of what's happening in the system while honouring rate limits.
The sampling percentage is exposed alongside each event so downstream consumers
can reason about the original population. See the [README](readme.html) for an
overview, configuration and examples.
""".
-endif.

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

-ifdef(E48).
-doc "An option accepted by `ask/3` and `ask/4`.".
-endif.
-type ask_opt() ::
    {max_rate, non_neg_integer() | infinity}.
-export_type([ask_opt/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-ifdef(E48).
-doc """
Starts a deigma instance named `Category` under your own supervisor.

- `Category` must be an atom.

See also `child_spec/1` and `start/1`.
""".
-endif.
-spec start_link(Category) -> {ok, pid()} | {error, term()}
        when Category :: atom().
start_link(Category) ->
    Server = deigma_util:proc_name(?MODULE, Category),
    supervisor:start_link({local,Server}, ?MODULE, [Category]).

-ifdef(E48).
-doc """
Returns a child spec for a deigma instance named `Category`, for launching it
under your own supervisor.

- `Category` must be an atom.

See also `start_link/1` and `start/1`.
""".
-endif.
-spec child_spec(Category) -> supervisor:child_spec()
        when Category :: atom().
child_spec(Category) ->
    #{ id => {deigma, Category},
       start => {?MODULE, start_link, [Category]},
       type => supervisor
     }.

-ifdef(E48).
-doc """
Starts a deigma instance named `Category` under the `deigma` application.

- `Category` must be an atom.

See also `stop/1`, `start_link/1` and `child_spec/1`.
""".
-endif.
-spec start(Category) -> {ok, pid()} | {error, term()}
        when Category :: atom().
start(Category) ->
    deigma_sup:start_child([Category]).

-ifdef(E48).
-doc """
Stops the deigma instance named `Category` running under the `deigma`
application.

- `Category` must be an atom.

See also `start/1`.
""".
-endif.
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

-ifdef(E48).
-doc """
Asks `Category` to sample an `EventType` event.

- `Category` must be an atom and correspond to an existing deigma instance.
- `EventType` can be any term.

Returns:

- `{sample, SamplingPercentage}` if the event was sampled;
- `{drop, SamplingPercentage}` if the event was dropped.

`SamplingPercentage` is a floating point number between 0.0 and 1.0 representing
the percentage of events that were sampled during the last 1000 milliseconds,
**including** the event reported just now.

See also `ask/3` and `ask/4`.
""".
-endif.
-spec ask(Category, EventType) -> {Decision, SamplingPercentage}
        when Category :: atom(),
             EventType :: term(),
             Decision :: sample | drop,
             SamplingPercentage :: float().
ask(Category, EventType) ->
    ask(Category, EventType, fun default_ask_fun/3).

-ifdef(E48).
-doc """
Asks `Category` to sample an `EventType` event using a custom function or
overridden options.

- `Category` must be an atom and correspond to an existing deigma instance.
- `EventType` can be any term.
- `EventFun` is a function called with `(Timestamp, Decision, SamplingPercentage)`:
    - `Timestamp` is the monotonic timestamp, in native units, at which the event
      was registered;
    - `Decision` is either `sample` or `drop`;
    - `SamplingPercentage` is a float between 0.0 and 1.0 (see `ask/2`).

  It runs from within the event window for `EventType`, so it can be used to
  fulfil serialisation constraints — at the expense of possibly turning the
  event window into a bottleneck.
- `Opts` is a list of `t:ask_opt/0` values:
    - `{max_rate, MaxRate}`: don't sample more than `MaxRate` `EventType` events
      per second (defaults to `100`).

When called with `EventFun`, returns (or throws) whatever `EventFun` returns (or
throws). When called with `Opts`, returns the same as `ask/2`.

See also `ask/2` and `ask/4`.
""".
-endif.
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

-ifdef(E48).
-doc """
Asks `Category` to sample an `EventType` event using a custom function and
overridden options.

The arguments are as described in `ask/3`. Returns (or throws) whatever
`EventFun` returns (or throws).

See also `ask/2` and `ask/3`.
""".
-endif.
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

-ifdef(E48).
-doc false.
-endif.
-spec init([atom(), ...])
        -> {ok, {supervisor:sup_flags(), [supervisor:child_spec(), ...]}}.
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
