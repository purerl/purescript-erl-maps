-module(erl_data_map@foreign).
-export([empty/0,
         isEmpty/1,
         insert/3,
         lookupImpl/4,
         values/1,
         keys/1,
         memberImpl/2,
         mapImpl/2,
         mapWithKeyImpl/2,
         deleteImpl/2,
         differenceImpl/2,
         foldMImpl/4]).

empty() -> #{}.
isEmpty(M) -> M =:= #{}.

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

memberImpl(K, M) -> maps:is_key(K, M).

deleteImpl(K, M) -> maps:remove(K, M).

differenceImpl(M1, M2) ->
  maps:fold(fun(M2Key, _, Acc) ->
                maps:remove(M2Key, Acc)
            end,
            M1,
            M2).
