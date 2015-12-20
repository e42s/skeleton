-module(bindings).

-export([empty/0, lookup/2, bind/3, test/0]).


empty() ->
    [].


lookup(_, []) ->
    none;
lookup(Var, [{Var, Value}|_]) ->
    {ok, Value};
lookup(Var, [_|Bindings]) ->
    lookup(Var, Bindings).


bind(Var, Value, Bindings) ->
    [{Var, Value}|Bindings].


test() ->
    Bindings = empty(),
    none = lookup(a, Bindings),
    Bindings1 = bind(a, b, Bindings),
    {ok, b}  = lookup(a, Bindings1),
    ok.

