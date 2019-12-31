-module(erl_data_map@foreign).
-export([ delete/2
        , difference/2
        , empty/0
        , filterWithKeyImpl/2
        , foldImpl/3
        , foldMImpl/4
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

mapImpl(F, M) -> maps:map(fun (_K, V) -> F(V) end, M).

mapWithKeyImpl(F, M) -> maps:map(F, M).

foldMImpl(Bind, F, MZ, M) ->
    maps:fold(fun (K, V, Acc) ->
        (Bind(Acc))(fun (Z) -> ((F(Z))(K))(V) end)
    end, MZ, M).

foldImpl(F, MZ, M) ->
    maps:fold(F, MZ, M).

member(K, M) -> maps:is_key(K, M).

delete(K, M) -> maps:remove(K, M).

difference(M1, M2) ->
  maps:fold(fun(M2Key, _, Acc) ->
                maps:remove(M2Key, Acc)
            end,
            M1,
            M2).


toUnfoldableImpl(Tuple, M) ->
  [Tuple(K, V) || {K, V} <- lists:sort(maps:to_list(M))].

toUnfoldableUnorderedImpl(Tuple, M) ->
  [Tuple(K, V) || {K, V} <- maps:to_list(M)].
