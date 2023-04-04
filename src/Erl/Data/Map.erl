-module(erl_data_map@foreign).
-export([ delete/2
        , difference/2
        , intersectionWithImpl/3
        , empty/0
        , filterWithKeyImpl/2
        , foldlImpl/3
        , foldrImpl/3
        , insert/3
        , isEmpty/1
        , keys/1
        , lookupImpl/4
        , mapImpl/2
        , mapWithKeyImpl/2
        , member/2
        , size/1
        , toUnfoldableImpl/2
        , toUnfoldableUnorderedImpl/2
        , values/1
        , union/2
        , unionWithImpl/3
]).

empty() -> #{}.
isEmpty(M) -> M =:= #{}.

size(M) -> maps:size(M).

insert(K, V, M) -> M#{ K => V}.

filterWithKeyImpl(F, M) -> maps:filter(F, M).

lookupImpl(Nothing, Just, K, M) ->
    case maps:find(K, M) of
      {ok, Value} -> Just(Value);
      error -> Nothing
    end.

values(M) -> maps:values(M).

keys(M) -> maps:keys(M).

% [drathier]: intentional argument order swap; erl merge is right-biased, but we want it to be left-biased.
union(M1, M2) -> maps:merge(M2,M1).

unionWithImpl(F, M1, M2) -> maps:merge_with(fun (_K,A,B) -> F(A,B) end,M1,M2).

mapImpl(F, M) -> maps:map(fun (_K, V) -> F(V) end, M).

mapWithKeyImpl(F, M) -> maps:map(F, M).

foldlImpl(F, Init, M) ->
    lists:foldl(fun (K, Z) ->
        V = maps:get(K, M),
        F(K, V, Z)
    end, Init, lists:sort(maps:keys(M))).

foldrImpl(F, Init, M) ->
    lists:foldr(fun (K, Z) ->
        V = maps:get(K, M),
        F(K, V, Z)
    end, Init, lists:sort(maps:keys(M))).


member(K, M) -> maps:is_key(K, M).

delete(K, M) -> maps:remove(K, M).

difference(M1, M2) ->
  maps:fold(fun(M2Key, _, Acc) ->
                maps:remove(M2Key, Acc)
            end,
            M1,
            M2).

intersectionWithImpl(F, M1, M2) ->
  maps:intersect_with(fun(_K,A,B) -> F(A,B) end, M1, M2).

toUnfoldableImpl(Tuple, M) ->
  [Tuple(K, V) || {K, V} <- lists:sort(maps:to_list(M))].

toUnfoldableUnorderedImpl(Tuple, M) ->
  [Tuple(K, V) || {K, V} <- maps:to_list(M)].
