-module(erl_data_map@foreign).
-export([empty/0, isEmpty/1, insert/3, lookupImpl/4, values/1, mapImpl/2, mapWithKeyImpl/2, foldMImpl/4]).

empty() -> #{}.
isEmpty(M) -> M =:= #{}.

insert(K, V, M) -> M#{ K => V}.

lookupImpl(Nothing, Just, K, M) ->
    case maps:is_key(K, M) of
        true -> Just(maps:get(K, M));
        false -> Nothing
    end.

values(M) -> maps:values(M).

mapImpl(F, M) -> maps:map(fun (_K, V) -> F(V) end, M).

mapWithKeyImpl(F, M) -> maps:map(F, M).

foldMImpl(Bind, F, MZ, M) ->
    maps:fold(fun (K, V, Acc) -> 
        (Bind(Acc))(fun (Z) -> ((F(Z))(K))(V) end)
    end, MZ, M).