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

-module(deigma_util).

-ifdef(E48).
-moduledoc false.
-endif.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    proc_name/2,
    dialyzer_opaque_term/1
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec proc_name(module(), atom()) -> atom().
proc_name(Module, PoolId) ->
    list_to_atom(
        atom_to_list(Module) ++
            "." ++
            atom_to_list(PoolId)
    ).

%% Identity function that launders its argument's type to `term()'. Routing a
%% value through it stops Dialyzer from inferring an over-specific success type,
%% which would otherwise trip `underspecs' against a deliberately broad public
%% spec (e.g. `child_spec/1' returning the whole `supervisor:child_spec()').
-spec dialyzer_opaque_term(term()) -> term().
dialyzer_opaque_term(Term) ->
    Term.
