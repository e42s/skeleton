-module(seq).

-export([a/1,b/1,c/1]).

-export([test/0]).

a(1) -> 1;
a(2) -> 2;
a(3) -> 3;
a(4) -> 4.

b(N) when N >= 1 -> 1.
c(N) when N >= 1 -> N.


test(a) ->
    1 = a(1),
    2 = a(2),
    3 = a(3),
    4 = a(4),
    ok;
test(b) ->
    1 = b(1),
    1 = b(2),
    1 = b(3),
    1 = b(4),
    ok;
test(c) ->
    1 = c(1),
    2 = c(2),
    3 = c(3),
    4 = c(4),
    ok.

test() ->
    test(a),
    test(b),
    test(c),
    ok.
