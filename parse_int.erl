-module(parse_int).

-export([test/0]).


digit([H|T])
  when H >= $0, H =< $9 ->
    {ok, H - $0, T};
digit(_S) ->
    error.


int(Acc, S) ->
    case digit(S) of
        {ok, N, S1} ->
            Acc1 = Acc*10 + N,
            case int(Acc1, S1) of
                {ok, N1, S2} ->
                    {ok, N1, S2};
                error ->
                    {ok, Acc1, S1}
            end;
        error ->
            error
    end.


int(S) ->
    int(0,S).


test() ->
    {ok, 1, []} = int("1"),
    {ok, 12, []} = int("12"),
    {ok, 123, []} = int("123"),
    error = int("a"),
    ok.
