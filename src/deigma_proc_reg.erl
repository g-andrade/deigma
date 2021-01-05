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
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO WORK SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% @private
-module(deigma_proc_reg).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/1,
    register/3,
    whereis/2
   ]).

-ignore_xref(
   [start_link/1
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
%% Record and Type Definitions
%% ------------------------------------------------------------------

-record(state, {
          category :: atom(),
          table :: ets:tab(),
          monitors :: #{ reference() => term() }
         }).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link(atom()) -> {ok, pid()}.
start_link(Category) ->
    Server = deigma_util:proc_name(?MODULE, Category),
    gen_server:start_link({local,Server}, ?MODULE, [Category], []).

-spec register(atom(), term(), pid()) -> ok | {error, {already_registered, pid()}}.
register(Category, Name, Pid) ->
    Server = deigma_util:proc_name(?MODULE, Category),
    gen_server:call(Server, {register, Name, Pid}, infinity).

-spec whereis(atom(), term()) -> pid() | undefined.
whereis(Category, Name) ->
    Table = table_name(Category),
    case ets:lookup(Table, Name) of
        [{_, Pid}] -> Pid;
        _ -> undefined
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([atom(), ...]) -> {ok, state()}.
init([Category]) ->
    Table = table_name(Category),
    TableOpts = [named_table, protected, {read_concurrency,true}],
    _ = ets:new(Table, TableOpts),
    {ok, #state{ category = Category, table = Table, monitors = #{} }}.

-spec handle_call(term(), {pid(),reference()}, state())
        -> {reply, Reply, state()} |
           {stop, unexpected_call, state()}
    when Reply :: ok | {error, {already_registered,pid()}}.
handle_call({register, Name, Pid}, _From, State) ->
    Table = State#state.table,
    case ets:lookup(Table, Name) of
        [{_, ExistingPid}] ->
            {reply, {error, {already_registered, ExistingPid}}, State};
        [] ->
            ets:insert(Table, {Name,Pid}),
            NewMonitor = monitor(process, Pid),
            Monitors = State#state.monitors,
            UpdatedMonitors = Monitors#{ NewMonitor => Name },
            UpdatedState = State#state{ monitors = UpdatedMonitors },
            {reply, ok, UpdatedState}
    end;
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

-spec handle_cast(term(), state()) -> {stop, unexpected_cast, state()}.
handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

-spec handle_info(term(), state())
        -> {noreply, state()} |
           {stop, unexpected_info, state()}.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    Monitors = State#state.monitors,
    {Name, UpdatedMonitors} = maps_take(Ref, Monitors),
    [_] = ets:take(State#state.table, Name),
    UpdatedState = State#state{ monitors = UpdatedMonitors },
    {noreply, UpdatedState};
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

table_name(Category) ->
    deigma_util:proc_name(?MODULE, Category).

maps_take(Key, Map) ->
    maps:take(Key, Map).
