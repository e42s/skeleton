-module(peg_parse_int).

-compile({parse_transform, peg_transform}).

-export([test/0]).

-rules([int/1, int/2, digit/1]).


digit([H|T])
  when H >= $0, H =< $9 ->
    {ok, H - $0, T}.


int(Acc, S) ->
    {ok, N, S1} = digit(S),
    int(Acc*10+N, S1);
int(Acc, S) ->
    {ok, N, S1} = digit(S),
    {ok, Acc*10+N, S1}.


int(S) ->
    int(0,S).


test() ->
    {ok, 1, []} = int("1"),
    {ok, 12, []} = int("12"),
    {ok, 123, []} = int("123"),
    error = int("a"),
    ok.
