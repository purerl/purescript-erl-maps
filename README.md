# purescript-erl-maps

This library provides FFI definitions for native Erlang maps and associated helper functions and type class instances.

The type of `Map String Int` or `Map Atom String` represent the types of homogenous erlang maps from (binary) string to integer or atom to string respectively. For non-homogenous maps keyed by atoms, notice that this is actually the representation of PureScript records.