-module(parse_util).

-export([expr/1, file/1, test/0]).


expr(S) ->
    {ok, Tokens, _} = erl_scan:string(S),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    Expr.


file(FileName) ->
    {ok, Forms} = epp:parse_file(FileName, []),
    Forms.

test(expr) ->
    {integer, 1, 1} = expr("1."),
    {atom, 1, a} = expr("a."),
    {var, 1, 'X'} = expr("X."),
    {match,1,{var,1,'X'},{integer,1,1}} = expr("X = 1."),
    {cons,1,{var,1,'X'},{var,1,'Y'}} = expr("[X|Y]."),
    {cons,1,{var,1,'X'},{cons,1,{var,1,'Y'},{nil,1}}} = expr("[X, Y]."),
    {match,1,{cons,1,{var,1,'X'},{var,1,'Y'}},{var,1,'Z'}} = expr("[X|Y] = Z."),
    {tuple,1,[{var,1,'X'},{var,1,'Y'}]} = expr("{X, Y}."),
    {string,1,"123"} = expr("\"123\"."),
    ok.


test() ->
    test(expr),
    ok.
