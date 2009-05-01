% Copyright (c) 2008-2009 Paul J. Davis <paul.joseph.davis@gmail.com>
% Copyright (c) 2008-2009 Enrico Thierbach <eno@open-lab.org>
%
% This file is part of EEP0018, which is released under the MIT
% license.

-module(compare).
-export([equiv/2]).

%% Test for equivalence of Erlang terms.
%% Due to arbitrary order of construction, equivalent objects might
%% compare unequal as erlang terms, so we need to carefully recurse
%% through aggregates (tuples and objects).

equiv({Props1}, {Props2}) ->
    equiv_object(Props1, Props2);
equiv(L1, L2) when is_list(L1), is_list(L2) ->
    equiv_list(L1, L2);
equiv(N1, N2) when is_number(N1), is_number(N2) -> N1 == N2;
equiv(B1, B2) when is_binary(B1), is_binary(B2) -> B1 == B2;
equiv(true, true) -> true;
equiv(false, false) -> true;
equiv(null, null) -> true.

%% Convert any keys with lists or atoms to binaries before comparison
equiv_key(K1, K2) when is_atom(K1) ->
    equiv_key(list_to_binary(atom_to_list(K1)), K2);
equiv_key(K1, K2) when is_list(K1) ->
    equiv_key(list_to_binary(K1), K2);
equiv_key(K1, K2) when is_atom(K2) ->
    equiv_key(K1, list_to_binary(atom_to_list(K2)));
equiv_key(K1, K2) when is_list(K2) ->
    equiv_key(K1, list_to_binary(K2));
equiv_key(K1, K2) ->
    equiv(K1, K2).


%% Object representation and traversal order is unknown.
%% Use the sledgehammer and sort property lists.

equiv_object(Props1, Props2) ->
    L1 = lists:keysort(1, Props1),
    L2 = lists:keysort(1, Props2),
    Pairs = lists:zip(L1, L2),
    true = lists:all(fun({{K1, V1}, {K2, V2}}) ->
                             equiv_key(K1, K2) and equiv(V1, V2)
                     end, Pairs).

%% Recursively compare tuple elements for equivalence.

equiv_list([], []) ->
    true;
equiv_list([V1 | L1], [V2 | L2]) ->
    case equiv(V1, V2) of
        true ->
            equiv_list(L1, L2);
        false ->
            false
    end.
