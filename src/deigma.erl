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

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [report/1,
    report/2
   ]).

-ignore_xref(
   [report/1,
    report/2
   ]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(is_non_neg_integer(V), (is_integer((V)) andalso (V) >= 0)).
-define(is_limit(V), (?is_non_neg_integer((V)) orelse ((V) =:= infinity))).

-define(DEFAULT_MAX_PER_SECOND, infinity).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec report(EventType) -> {Decision, SampleRate} | overloaded
        when EventType :: term(),
             Decision :: accept | drop,
             SampleRate :: float().
report(EventType) ->
    deigma_window_manager:report(EventType, ?DEFAULT_MAX_PER_SECOND).

-spec report(EventType, MaxPerSecond) -> {Decision, SampleRate} | overloaded
        when EventType :: term(),
             MaxPerSecond :: non_neg_integer(),
             Decision :: accept | drop,
             SampleRate :: float().
report(EventType, MaxPerSecond) when ?is_limit(MaxPerSecond) ->
    deigma_window_manager:report(EventType, MaxPerSecond).
