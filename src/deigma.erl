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
    ask/4,
    get_counters/1
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

start_link(Category) ->
    Server = deigma_util:proc_name(?MODULE, Category),
    supervisor:start_link({local,Server}, ?MODULE, [Category]).

child_spec(Category) ->
    #{ id => {deigma, Category},
       start => {?MODULE, start_link, [Category]},
       type => supervisor
     }.

start(Category) ->
    deigma_sup:start_child([Category]).

stop(Category) ->
    Server = workforce_util:proc_name(?MODULE, Category),
    try gen_server:stop(Server, shutdown, infinity) of
        ok -> ok
    catch
        exit:Reason when Reason =:= noproc;
                         Reason =:= normal;
                         Reason =:= shutdown ->
            {error, not_started}
    end.

ask(Category, EventType) ->
    ask(Category, EventType, fun default_event_fun/2).

ask(Category, EventType, EventFun) ->
    ask(Category, EventType, EventFun, []).

ask(Category, EventType, EventFun, Opts) ->
    deigma_category:ask(Category, EventType, EventFun, Opts).

get_counters(Category) ->
    deigma_category:get_counters(Category).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([Category]) ->
    SupFlags =
        #{ sup_flags => one_for_one,
           intensity => 10,
           period => 1
         },
    DelayedApplyPoolArgs =
        [deigma_delayed_apply:pool_name(Category),
         deigma_delayed_apply, % resource module
         [Category], % resource args
         #{ min_resources => erlang:system_info(schedulers_online)
          }
        ],
    ChildSpecs =
        [#{ id => category,
            start => {deigma_category, start_link, [Category]}
          },
         #{ id => delayed_apply_pool,
            start => {workforce, start_link, DelayedApplyPoolArgs}
          }
        ],
    {ok, {SupFlags, ChildSpecs}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

default_event_fun(_AcceptedCount, _RejectedCount) ->
    accept.
