-module(parse_util).

-export([parse_expr/1, parse_file/1, test/0]).


parse_expr(S) ->
    {ok, Tokens, _} = erl_scan:string(S),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    Expr = expr_parser:parse_expr(S),
    Expr.


parse_file(FileName) ->
    {ok, Forms} = epp:parse_file(FileName, []),
    Forms.


test(parse_expr) ->
    {integer, 1, 1} = parse_expr("1."),
    {integer, 1, 12} = parse_expr("12."),
    {atom, 1, a} = parse_expr("a."),
    {atom, 1, ab} = parse_expr("ab."),
    {atom, 1, a} = parse_expr("'a'."),
    {atom, 1, '\''} = parse_expr("'\\''."),
    {string,1,""} = parse_expr("\"\"."),
    {string,1,"123"} = parse_expr("\"123\"."),
    {char,1,$a} = parse_expr("$a."),
    {char,1,$\\} = parse_expr("$\\\\."),

    {var, 1, 'X'} = parse_expr("X."),

    {nil,1} = parse_expr("[]."),
    {cons,1,{var,1,'X'},{nil,1}} = parse_expr("[X]."),
    {cons,1,{var,1,'X'},{var,1,'Y'}} = parse_expr("[X|Y]."),
    {cons,1,{var,1,'X'},{cons,1,{var,1,'Y'},{nil,1}}} = parse_expr("[X, Y]."),

    {tuple,1,[]} = parse_expr("{}."),
    {tuple,1,[{var,1,'X'}]} = parse_expr("{ X }."),
    {tuple,1,[{var,1,'X'},{var,1,'Y'}]} = parse_expr("{X, Y}."),
    {tuple,1,[{var,1,'X'},{var,1,'Y'},{var,1,'Z'}]} = parse_expr("{X, Y, Z}."),

    {op, 1, '+', {integer, 1, 1}} = parse_expr("+1."),
    {op, 1, '-', {integer, 1, 1}} = parse_expr("-1."),
    {op,1,'*',
     {op,1,'*',{integer,1,1},{integer,1,2}},
     {integer,1,3}}
        = parse_expr("1*2*3."),

    {op,1,'div',
     {op,1,'*',{integer,1,1},{integer,1,2}},
     {integer,1,3}}
        = parse_expr("1*2 div 3."),

    {op,1,'+',
     {integer,1,1},
     {op,1,'*',{integer,1,2},{integer,1,3}}}
        = parse_expr("1+2*3."),

    {match,1,{var,1,'X'},{integer,1,1}}
        = parse_expr("X = 1."),

    {'case',1,{var,1,'X'},
     [{clause,1,[{integer,1,1}],[],[{atom,1,a}]}]}
      = parse_expr("case X of 1 -> a end."),

    {'case',1,{var,1,'X'},
     [{clause,1,[{integer,1,1}],[],[{atom,1,a}]},
      {clause,1,[{integer,1,2}],[],[{atom,1,b}]}]}
        = parse_expr("case X of 1 -> a; 2 -> b end."),

    {'case',1,
     {var,1,'X'},
     [{clause,1,
       [{var,1,'X'}],
       [[{op,1,'<',{var,1,'X'},{integer,1,2}}],
        [{op,1,'>',{var,1,'X'},{integer,1,4}}]],
       [{atom,1,a}]}]}
        = parse_expr("case X of X when X < 2; X > 4 -> a end."),

    {'case',1,
     {var,1,'X'},
     [{clause,1,
       [{var,1,'X'}],
       [[{op,1,'>',{var,1,'X'},{integer,1,2}},
         {op,1,'<',{var,1,'X'},{integer,1,4}}]],
       [{atom,1,a}]}]}
        = parse_expr("case X of X when X > 2, X < 4 -> a end."),

    {'case',1,
     {var,1,'X'},
     [{clause,1,
       [{var,1,'X'}],
       [[{call,1,
          {remote,1,
           {atom,1,erlang},
           {atom,1,is_integer}},
          [{var,1,'X'}]}]],
       [{atom,1,a}]}]}
        = parse_expr("case X of X when erlang:is_integer(X) -> a end."),

    {call,1,{atom,1,test},[]}
        = parse_expr("test()."),

    ok.


test() ->
    test(parse_expr),
    ok.
