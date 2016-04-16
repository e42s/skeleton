-module(expr_parser).

-compile({parse_transform, peg_transform}).

-export([test/0,parse_expr/1]).

-rules(
   [whitespaces/1,
    escape/1,
    quoted/2,
    digits/1,
    name/1,
    exprs_tail/1,
    exprs/1,
    exprs_empty/1,
    list_tail/1,
    list/1,
    guards_tail/1,
    guards/1,
    clause/1,
    clauses_tail/1,
    clauses/1,
    expr0/1,
    expr1_tail/2,
    expr1/1,
    call_tail/2,
    call/1,
    expr3/1,
    expr4_tail/2,
    expr4/1,
    expr5_tail/2,
    expr5/1,
    expr7_tail/2,
    expr7/1,
    expr10_tail/2,
    expr/1,
    form/1
   ]).


whitespaces([$ |S]) ->
    whitespaces(S);
whitespaces(S) ->
    {ok, ok, S}.


escape([C|S]) ->
    {ok, C, S}.


quoted(Q, [Q|S]) ->
    {ok, [], S};
quoted(Q, [$\\, Q|S]) ->
    {ok, Str, S1} = quoted(Q, S),
    {ok, [Q|Str], S1};
quoted(Q, [$\\|S]) ->
    {ok, H, S1} = escape(S),
    {ok, T, S2} = quoted(Q, S1),
    {ok, [H|T], S2};
quoted(Q, [H|S]) ->
    {ok, Str, S1} = quoted(Q, S),
    {ok, [H|Str], S1}.


digits([H|S])
  when H >= $0, H =< $9 ->
    {ok, D, S1} = digits(S),
    {ok, [H|D], S1};
digits(S) ->
    {ok, [], S}.


name([H|S])
  when H >= $0, H =< $9;
       H >= $a, H =< $z;
       H >= $A, H =< $Z;
       H =:= $_ ->
    {ok, N, S1} = name(S),
    {ok, [H|N], S1};
name(S) ->
    {ok, [], S}.


exprs_tail([$,|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, H, S2} = expr(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok, T, S4} = exprs_tail(S3),
    {ok, [H|T], S4};
exprs_tail(S) ->
    {ok, [], S}.


exprs(S) ->
    {ok, H, S1} = expr(S),
    {ok, _, S2} = whitespaces(S1),
    {ok, T, S3} = exprs_tail(S2),
    {ok, [H|T], S3}.

exprs_empty(S) ->
    exprs(S);
exprs_empty(S) ->
    {ok, [], S}.


list_tail([$,|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, H, S2} = expr(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok, T, S4} = list_tail(S3),
    {ok, {cons,1,H,T}, S4};
list_tail([$||S]) ->
    {ok, _, S1} = whitespaces(S),
    expr(S1);
list_tail(S) ->
    {ok, {nil, 1}, S}.


list(S) ->
    {ok, H, S1} = expr(S),
    {ok, _, S2} = whitespaces(S1),
    {ok, T, S3} = list_tail(S2),
    {ok, {cons,1,H,T}, S3};
list(S) ->
    {ok, {nil, 1}, S}.


guards_tail([$;|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, H, S2} = exprs(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok, T, S4} = guards_tail(S3),
    {ok, [H|T], S4};
guards_tail(S) ->
    {ok, [], S}.


guards([$w,$h,$e,$n|S]) ->
    {ok, [], S} = name(S),
    {ok, _, S1} = whitespaces(S),
    {ok, H, S2} = exprs(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok, T, S4} = guards_tail(S3),
    {ok, [H|T], S4};
guards(S) ->
    {ok, [], S}.


clause(S) ->
    {ok, P, S1} = expr(S),
    {ok, _, S2} = whitespaces(S1),
    {ok, G, S3} = guards(S2),
    {ok, _, [$-,$>|S4]} = whitespaces(S3),
    {ok, [], S4} = name(S4),
    {ok, _, S5} = whitespaces(S4),
    {ok, E, S6} = exprs(S5),
    {ok, {clause, 1, [P], G, E}, S6}.


clauses_tail([$;|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, H, S2} = clause(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok, T, S4} = clauses_tail(S3),
    {ok, [H|T], S4};
clauses_tail(S) ->
    {ok, [], S}.


clauses(S) ->
    {ok, H, S1} = clause(S),
    {ok, _, S2} = whitespaces(S1),
    {ok, T, S3} = clauses_tail(S2),
    {ok, [H|T], S3}.


expr0([$c,$a,$s,$e|S]) ->
    {ok, [], S} = name(S),
    {ok, _, S1} = whitespaces(S),
    {ok, Expr, S2} = expr(S1),
    {ok, _, [$o,$f|S3]} = whitespaces(S2),
    {ok, [], S3} = name(S3),
    {ok, _, S4} = whitespaces(S3),
    {ok, Clauses, S5} = clauses(S4),
    {ok, _, [$e,$n,$d|S6]} = whitespaces(S5),
    {ok, {'case',1,Expr,Clauses},S6};
expr0([D|S])
  when D >= $0, D =< $9 ->
    {ok, Ds, S1} = digits(S),
    {ok, {integer,1,list_to_integer([D|Ds])}, S1};
expr0([A|S])
  when A >= $a, A =< $z ->
    {ok, N, S1} = name(S),
    {ok, {atom, 1, list_to_atom([A|N])}, S1};
expr0([$'|S]) ->
    {ok, A, S1} = quoted($', S),
    {ok, {atom, 1, list_to_atom(A)}, S1};
expr0([V|S])
  when V >= $A, V =< $Z;
       V =:= $_ ->
    {ok, N, S1} = name(S),
    {ok, {var, 1, list_to_atom([V|N])}, S1};
expr0([$"|S]) ->
    {ok, Str, S1} = quoted($", S),
    {ok, {string, 1, Str}, S1};
expr0([${|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, Elems, S2} = exprs_empty(S1),
    {ok, _, [$}|S3]} = whitespaces(S2),
    {ok, {tuple, 1, Elems}, S3};
expr0([$[|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, List, S2} = list(S1),
    {ok, _, [$]|S3]} = whitespaces(S2),
    {ok, List, S3};
expr0([$$,$\\|S]) ->
    {ok, C, S1} = escape(S),
    {ok, {char,1,C}, S1};
expr0([$$,C|S]) ->
    {ok, {char,1,C}, S};
expr0([$(|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, Expr, S2} = expr(S1),
    {ok, _, [$)|S3]} = whitespaces(S2),
    {ok, Expr, S3}.


expr1_tail(E1,[$:|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr0(S1),
    {ok, {remote,1,E1,E2}, S2};
expr1_tail(Expr,S) ->
    {ok,Expr,S}.


expr1(S) ->
    {ok, Expr, S1} = expr0(S),
    {ok, _, S2} = whitespaces(S1),
    expr1_tail(Expr, S2).


call_tail(Expr,[$(|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, Args, S2} = exprs_empty(S1),
    {ok, _, [$)|S3]} = whitespaces(S2),
    {ok, {call,1,Expr,Args}, S3};
call_tail(Expr,S) ->
    {ok,Expr,S}.


call(S) ->
    {ok, Expr, S1} = expr1(S),
    {ok, _, S2} = whitespaces(S1),
    call_tail(Expr, S2).


expr3([$+|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E, S2} = call(S1),
    {ok, {op,1,'+',E}, S2};
expr3([$-|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E, S2} = call(S1),
    {ok, {op,1,'-',E}, S2};
expr3(S) ->
    call(S).


expr4_tail(E1, [$*|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr3(S1),
    {ok, _, S3} = whitespaces(S2),
    expr4_tail({op,1,'*',E1,E2},S3);
expr4_tail(E1, [$d,$i,$v|S]) ->
    {ok, [], S} = name(S),
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr3(S1),
    {ok, _, S3} = whitespaces(S2),
    expr4_tail({op,1,'div',E1,E2},S3);
expr4_tail(E1, [$r,$e,$m|S]) ->
    {ok, [], S} = name(S),
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr3(S1),
    {ok, _, S3} = whitespaces(S2),
    expr4_tail({op,1,'rem',E1,E2},S3);
expr4_tail(Expr, S) ->
    {ok, Expr, S}.


expr4(S) ->
    {ok, Expr, S1} = expr3(S),
    {ok, _, S2} = whitespaces(S1),
    expr4_tail(Expr, S2).


expr5_tail(E1, [$+|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr4(S1),
    {ok, _, S3} = whitespaces(S2),
    expr5_tail({op,1,'+',E1,E2},S3);
expr5_tail(E1, [$-,C|S])
  when C =/= $> ->
    {ok, _, S1} = whitespaces([C|S]),
    {ok, E2, S2} = expr4(S1),
    {ok, _, S3} = whitespaces(S2),
    expr5_tail({op,1,'-',E1,E2},S3);
expr5_tail(Expr, S) ->
    {ok, Expr, S}.


expr5(S) ->
    {ok, Expr, S1} = expr4(S),
    {ok, _, S2} = whitespaces(S1),
    expr5_tail(Expr, S2).


expr7_tail(E1, [$=,$<|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr5(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok,{op,1,'=<',E1,E2},S3};
expr7_tail(E1, [$>,$=|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr5(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok,{op,1,'>=',E1,E2},S3};
expr7_tail(E1, [$=,$:,$=|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr5(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok,{op,1,'=:=',E1,E2},S3};
expr7_tail(E1, [$=,$/,$=|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr5(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok,{op,1,'=/=',E1,E2},S3};
expr7_tail(E1, [$<|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr5(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok,{op,1,'<',E1,E2},S3};
expr7_tail(E1, [$>|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr5(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok,{op,1,'>',E1,E2},S3};
expr7_tail(Expr, S) ->
    {ok, Expr, S}.


expr7(S) ->
    {ok, Expr, S1} = expr5(S),
    {ok, _, S2} = whitespaces(S1),
    expr7_tail(Expr, S2).


expr10_tail(E1, [$=|S]) ->
    {ok, _, S1} = whitespaces(S),
    {ok, E2, S2} = expr7(S1),
    {ok, _, S3} = whitespaces(S2),
    {ok, E3, S4} = expr10_tail(E2, S3),
    {ok, {match,1,E1,E3}, S4};
expr10_tail(Expr, S) ->
    {ok, Expr, S}.


expr(S) ->
    {ok, Expr, S1} = expr7(S),
    {ok, _, S2} = whitespaces(S1),
    expr10_tail(Expr, S2).


form(S0) ->
    {ok, _, S1} = whitespaces(S0),
    {ok, Expr, S2} = expr(S1),
    {ok, _, [$.|S3]} = whitespaces(S2),
    {ok, Expr, S3}.


parse_expr(S) ->
    {ok, Expr, []} = form(S),
    Expr.


test() ->
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
