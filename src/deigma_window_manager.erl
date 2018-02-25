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
    mark_as_overloaded/1,
    unmark_as_overloaded/1,
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

-record(window, {
          event_type :: term(),
          pid :: pid(),
          is_overloaded :: boolean()
         }).
-type window() :: #window{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec report(term(), non_neg_integer() | infinity)
        -> {accept | drop, float()} | overloaded.
report(EventType, MaxPerSecond) ->
    Window = find_or_create_window(EventType),
    case (Window#window.is_overloaded orelse
         deigma_window:report(Window#window.pid, MaxPerSecond))
    of
        true ->
            overloaded;
        stopped ->
            % window went away; try again
            report(EventType, MaxPerSecond);
        Result ->
            Result
    end.

-spec mark_as_overloaded(term()) -> boolean().
mark_as_overloaded(EventType) ->
    ets:update_element(?TABLE, EventType, {#window.is_overloaded,true}).

-spec unmark_as_overloaded(term()) -> boolean().
unmark_as_overloaded(EventType) ->
    ets:update_element(?TABLE, EventType, {#window.is_overloaded,false}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?CB_MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([]) -> {ok, state()}.
init([]) ->
    EtsOpts = [named_table, public, {keypos,#window.event_type},
               {read_concurrency,true}],
    _ = ets:new(?TABLE, EtsOpts),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), reference()}, state())
        -> {reply, window(), state()} |
           {stop, unexpected_call, state()}.
handle_call({find_or_create_window, EventType}, _From, State) ->
    handle_find_or_create_window(EventType, State);
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
    {EventType, UpdatedMonitors} = maps_take(Ref, Monitors),
    [#window{}] = ets:take(?TABLE, EventType),
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

find_or_create_window(EventType) ->
    case find_window(EventType) of
        undefined ->
            gen_server:call(?SERVER, {find_or_create_window, EventType}, infinity);
        Window ->
            Window
    end.

handle_find_or_create_window(EventType, State) ->
    case find_window(EventType) of
        undefined ->
            {ok, WindowPid} = deigma_window:start(EventType),
            WindowMonitor = monitor(process, WindowPid),
            Window = #window{ event_type = EventType,
                              pid = WindowPid,
                              is_overloaded = false
                            },
            ets:insert(?TABLE, Window),
            Monitors = State#state.monitors,
            UpdatedMonitors = maps:put(WindowMonitor, EventType, Monitors),
            UpdatedState = State#state{ monitors = UpdatedMonitors },
            {reply, Window, UpdatedState};
        Window ->
            {reply, Window, State}
    end.

find_window(EventType) ->
    case ets:lookup(?TABLE, EventType) of
        [Window] ->
            Window;
        [] ->
            undefined
    end.

-ifdef(POST_OTP_18).
maps_take(Key, Map) ->
    maps:take(Key, Map).
-else.
maps_take(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            {Value, maps:remove(Key, Map)};
        error ->
            error
    end.
-endif.
