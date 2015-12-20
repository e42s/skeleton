-module(board).

-export([test/0, move/2, moves/2]).

move(left, {X,Y})
  when X > 1, X =< 8 ->
    {X-1, Y};
move(right, {X,Y})
  when X >= 1, X < 8 ->
    {X+1, Y};
move(up, {X,Y})
  when Y > 1, Y =< 8 ->
    {X, Y-1};
move(down, {X,Y})
  when Y >= 1, Y < 8 ->
    {X, Y+1}.

moves([], From) ->
    From;
moves([H|T], From) ->
    moves(T, move(H, From)).

test(move) ->
    A = {3,3},
    B = {4,3},
    C = {3,4},
    B = move(right, A),
    A = move(left, B),
    C = move(down, A),
    A = move(up, C);
test(moves) ->
    {4,1} = moves([right,right,right], {1,1}),
    {1,4} = moves([down,down,down], {1,1}).

test() ->
    test(move),
    test(moves),
    ok.

