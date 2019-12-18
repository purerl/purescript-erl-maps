-module(erl_data_map@foreign).
-export([empty/0,
         isEmpty/1,
         size/1,
         insert/3,
         lookupImpl/4,
         values/1,
         keys/1,
         member/2,
         mapImpl/2,
         mapWithKeyImpl/2,
         delete/2,
         difference/2,
         foldMImpl/4,
         toUnfoldMImp/2,
         toUnfoldableUnorderedImp/2]).

empty() -> #{}.

isEmpty(M) -> M =:= #{}.

size(M) -> maps:size(M).

insert(K, V, M) -> M#{ K => V}.

lookupImpl(Nothing, Just, K, M) ->
    case maps:is_key(K, M) of
        true -> Just(maps:get(K, M));
        false -> Nothing
    end.

values(M) -> maps:values(M).

keys(M) -> maps:keys(M).

mapImpl(F, M) -> maps:map(fun (_K, V) -> F(V) end, M).

mapWithKeyImpl(F, M) -> maps:map(F, M).

foldMImpl(Bind, F, MZ, M) ->
    maps:fold(fun (K, V, Acc) ->
        (Bind(Acc))(fun (Z) -> ((F(Z))(K))(V) end)
    end, MZ, M).

toUnfoldMImp(Tuple, M) -> lists:map(fun (a, b) ->
    Tuple(a, b) end, lists:sort(maps:to_list(M))).

toUnfoldableUnorderedImp(Tuple, M) -> lists:map(fun (a, b) ->
    Tuple(a, b) end, maps:to_list(M)).

member(K, M) -> maps:is_key(K, M).

delete(K, M) -> maps:remove(K, M).

difference(M1, M2) ->
  maps:fold(fun(M2Key, _, Acc) ->
                maps:remove(M2Key, Acc)
            end,
            M1,
            M2).
