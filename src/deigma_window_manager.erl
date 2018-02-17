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
-module(deigma_window_manager).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [report/2,
    start_link/0
   ]).

-ignore_xref(
   [start_link/0
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

-define(CB_MODULE, ?MODULE).
-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-record(state, {
          monitors = #{} :: #{ reference() => term() }
         }).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec report(term(), non_neg_integer() | infinity)
        -> {ok, {deigma:decision(), deigma:stats()}} |
           {error, term()}.
report(Event, MaxPerSecond) ->
    WindowPid = find_or_create_window(Event),
    case deigma_window:report(WindowPid, MaxPerSecond) of
        {ok, Result} ->
            {ok, Result};
        {error, {window_stopped, Reason}} when Reason =:= noproc;
                                               Reason =:= normal ->
            % window went away; try again
            report(Event, MaxPerSecond);
        {error, Error} ->
            {error, Error}
    end.

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?CB_MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([]) -> {ok, state()}.
init([]) ->
    _ = ets:new(?TABLE, [named_table, protected, {read_concurrency,true}]),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), reference()}, state())
        -> {reply, pid(), state()} |
           {stop, unexpected_call, state()}.
handle_call({find_or_create_window, Event}, _From, State) ->
    handle_find_or_create_window(Event, State);
handle_call(_Request, _From, State) ->
    {stop, unexpected_call, State}.

-spec handle_cast(term(), state())
        -> {stop, unexpected_cast, state()}.
handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

-spec handle_info(term(), state())
        -> {noreply, state()} |
           {stop, unexpected_info, state()}.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    Monitors = State#state.monitors,
    {Event, UpdatedMonitors} = maps:take(Ref, Monitors),
    ets:delete(?TABLE, Event),
    UpdatedState = State#state{ monitors = UpdatedMonitors },
    {noreply, UpdatedState};
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) when is_record(State, state) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

find_or_create_window(Event) ->
    case find_window(Event) of
        undefined ->
            gen_server:call(?SERVER, {find_or_create_window, Event}, infinity);
        WindowPid ->
            WindowPid
    end.

handle_find_or_create_window(Event, State) ->
    case find_window(Event) of
        undefined ->
            {ok, WindowPid} = deigma_window:start(),
            WindowMonitor = monitor(process, WindowPid),
            ets:insert(?TABLE, {Event, WindowPid}),
            Monitors = State#state.monitors,
            UpdatedMonitors = Monitors#{ WindowMonitor => Event },
            UpdatedState = State#state{ monitors = UpdatedMonitors },
            {reply, WindowPid, UpdatedState};
        WindowPid ->
            {reply, WindowPid, State}
    end.

find_window(Event) ->
    case ets:lookup(?TABLE, Event) of
        [{_, WindowPid}] ->
            WindowPid;
        [] ->
            undefined
    end.
