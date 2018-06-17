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

%% @private
-module(deigma_event_window_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).
-export([start_child/2]).

-ignore_xref([start_link/1]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(Category) ->
    Server = deigma_util:proc_name(?MODULE, Category),
    supervisor:start_link({local,Server}, ?MODULE, [Category]).

-spec start_child(atom(), list()) -> {ok, pid()} | {error, term()}.
start_child(Category, Args) ->
    Server = deigma_util:proc_name(?MODULE, Category),
    supervisor:start_child(Server, Args).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

-spec init([atom(), ...])
        -> {ok, {supervisor:sup_flags(), [supervisor:child_spec(), ...]}}.
init([Category]) ->
    SupFlags = #{ strategy => simple_one_for_one },
    ChildSpecs =
        [#{ id => event_window,
            start => {deigma_event_window, start_link, [Category]},
            restart => temporary
          }
        ],
    {ok, {SupFlags, ChildSpecs}}.
