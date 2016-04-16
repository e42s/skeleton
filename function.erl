-module(function).

-export([test/0, selftest/0]).


eval({integer, _, Value}, Bindings, _Funs) ->
    {ok, Value, Bindings};
eval({atom, _, Value}, Bindings, _Funs) ->
    {ok, Value, Bindings};
eval({string, _, Value}, Bindings, _Funs) ->
    {ok, Value, Bindings};
eval({char, _, Value}, Bindings, _Funs) ->
    {ok, Value, Bindings};
eval({var, _, Var}, Bindings, _Funs) ->
    case bindings:lookup(Var, Bindings) of
        {ok, Value} ->
            {ok, Value, Bindings};
        none ->
            {error, {unbound, Var}}
    end;
eval({nil, _}, Bindings, _Funs) ->
    {ok, [], Bindings};
eval({cons, _, H, T}, Bindings, Funs) ->
    case eval(H, Bindings, Funs) of
        {ok, H1, Bindings1} ->
            case eval(T, Bindings1, Funs) of
                {ok, T1, Bindings2} ->
                    {ok, [H1|T1], Bindings2};
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
eval({tuple, _, Elements}, Bindings, Funs) ->
    case eval_elements(Elements, Bindings, Funs) of
        {ok, Value, Bindings1} ->
            {ok, erlang:list_to_tuple(Value), Bindings1};
        Error ->
            Error
    end;
eval({match, _, A, B}, Bindings, Funs) ->
    case eval(B, Bindings, Funs) of
        {ok, Value, Bindings1} ->
            match(A, Value, Bindings1);
        Error ->
            Error
    end;
eval({call,_,{remote,_,M,F},A}, Bindings, Funs) ->
    case eval(M, Bindings, Funs) of
        {ok, M1, Bindings1} ->
            case eval(F, Bindings1, Funs) of
                {ok, F1, Bindings2} ->
                    case eval_elements(A, Bindings2, Funs) of
                        {ok, A1, Bindings3} ->
                            {ok, erlang:apply(M1,F1,A1), Bindings3};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
eval({call,_,F,A}, Bindings, Funs) ->
    case eval(F, Bindings, Funs) of
        {ok, F1, Bindings1} ->
            case eval_elements(A, Bindings1, Funs) of
                {ok, A1, Bindings2} ->
                    case bindings:lookup({F1,erlang:length(A1)}, Funs) of
                        none ->
                            {error, {undefined, {F1,erlang:length(A1)}}};
                        {ok,Clauses} ->
                            case eval_clauses(Clauses,A1,[],Funs) of
                                {ok, Value, _} ->
                                    {ok, Value, Bindings2};
                                Error ->
                                    Error
                            end
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
eval({op,_,Op,A}, Bindings, Funs) ->
    case eval(A, Bindings, Funs) of
        {ok, A1, Bindings1} ->
            {ok, erlang:Op(A1), Bindings1};
        Error ->
            Error
    end;
eval({op,_,Op,A,B}, Bindings, Funs) ->
    case eval(A, Bindings, Funs) of
        {ok, A1, Bindings1} ->
            case eval(B, Bindings1, Funs) of
                {ok, B1, Bindings2} ->
                    {ok, erlang:Op(A1,B1), Bindings2};
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
eval({'case',_,Expr,Clauses}, Bindings, Funs) ->
    case eval(Expr, Bindings,Funs) of
        {ok, Value, Bindings1} ->
            eval_clauses(Clauses,[Value],Bindings1,Funs);
        Error ->
            Error
    end.


eval_elements([], Bindings, _Funs) ->
    {ok, [], Bindings};
eval_elements([H|T], Bindings, Funs) ->
    case eval(H, Bindings, Funs) of
        {ok, H1, Bindings1} ->
            case eval_elements(T, Bindings1, Funs) of
                {ok, T1, Bindings2} ->
                    {ok, [H1|T1], Bindings2};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.


match({integer, _, Value}, Value, Bindings) ->
    {ok, Value, Bindings};
match({integer, _, _}, Value, _) ->
    {error, {mismatch, Value}};
match({op, _, '-', {integer, _, Int}}, Value, Bindings)
  when -Int =:= Value ->
    {ok, Value, Bindings};
match({op, _, '-', {integer, _, _}}, Value, _) ->
    {error, {mismatch, Value}};
match({atom, _, Value}, Value, Bindings) ->
    {ok, Value, Bindings};
match({atom, _, _}, Value, _) ->
    {error, {mismatch, Value}};
match({string, _, Value}, Value, Bindings) ->
    {ok, Value, Bindings};
match({string, _, _}, Value, _) ->
    {error, {mismatch, Value}};
match({char, _, Value}, Value, Bindings) ->
    {ok, Value, Bindings};
match({char, _, _}, Value, _) ->
    {error, {mismatch, Value}};
match({var, _, Var}, Value, Bindings) ->
    case erlang:atom_to_list(Var) of
        [$_|_]->
            {ok, Value, Bindings};
        _ ->
            case bindings:lookup(Var, Bindings) of
                {ok, Value} ->
                    {ok, Value, Bindings};
                {ok, Value2} ->
                    {error, {mismatch, Value2}};
                none ->
                    {ok, Value, [{Var,Value}|Bindings]}
            end
    end;
match({nil, _}, [], Bindings) ->
    {ok, [], Bindings};
match({nil, _}, Value, _) ->
    {error, {mismatch, Value}};
match({cons, _, H, T}, [VH|VT], Bindings) ->
    case match(H, VH, Bindings) of
        {ok, H1, Bindings1} ->
            case match(T, VT, Bindings1) of
                {ok, T1, Bindings2} ->
                    {ok, [H1|T1], Bindings2};
                {error, {mismatch, _}} ->
                    {error, {mismatch, [VH|VT]}};
                Error ->
                    Error
            end;
        {error, {mismatch, _}} ->
            {error, {mismatch, [VH|VT]}};
        Error ->
            Error
    end;
match({cons, _, _, _}, Value, _) ->
    {error, {mismatch, Value}};
match({tuple, _, Elements}, Value, Bindings)
  when erlang:is_tuple(Value) ->
    case match_elements(Elements, erlang:tuple_to_list(Value), Bindings) of
        {ok, _Value1, Bindings1} ->
            {ok, Value, Bindings1};
        {error, {mismatch, _}} ->
            {error, {mismatch, Value}};
        Error ->
            Error
    end;
match({tuple, _, _}, Value, _) ->
    {error, {mismatch, Value}}.


match_elements([], [], Bindings) ->
    {ok, [], Bindings};
match_elements([H|T], [VH|VT], Bindings) ->
    case match(H, VH, Bindings) of
        {ok, H1, Bindings1} ->
            case match_elements(T, VT, Bindings1) of
                {ok, T1, Bindings2} ->
                    {ok, [H1|T1], Bindings2};
                {error, {mismatch, _}} ->
                    {error, {mismatch, [VH|VT]}};
                Error ->
                    Error
            end;
        {error, {mismatch, _}} ->
            {error, {mismatch, [VH|VT]}};
        Error ->
            Error
    end;
match_elements(_, Value, _) ->
    {error, {mismatch, Value}}.


eval_clauses([], Values, _, _Funs) ->
    {error, {mismatch, Values}};
eval_clauses([{clause,_,Patterns,GuardsList,Exprs}|Clauses],Values,Bindings, Funs) ->
    case match_elements(Patterns,Values,Bindings) of
        {ok, _Values1, Bindings1} ->
            case GuardsList of
                [] ->
                    eval_exprs(Exprs,Bindings1,Funs);
                _ ->
                    case eval_guards_list(GuardsList,Bindings1,Funs) of
                        {ok, true, Bindings2} ->
                            eval_exprs(Exprs,Bindings2,Funs);
                        _ ->
                            eval_clauses(Clauses,Values,Bindings,Funs)
                    end
            end;
        _Error ->
            eval_clauses(Clauses,Values,Bindings,Funs)
    end.


eval_guards_list([], _Bindings, _Funs) ->
    false;
eval_guards_list([H|T], Bindings, Funs) ->
    case eval_guards(H,Bindings,Funs) of
        {ok, true, Bindings1} ->
            {ok, true, Bindings1};
        _ ->
            eval_guards_list(T, Bindings, Funs)
    end.


eval_guards([], Bindings, _Funs) ->
    {ok, true, Bindings};
eval_guards([H|T], Bindings, Funs) ->
    case eval(H, Bindings, Funs) of
        {ok, true, Bindings1} ->
            eval_guards(T, Bindings1, Funs);
        _ ->
            false
    end.


eval_exprs([E], Bindings, Funs) ->
    eval(E, Bindings, Funs);
eval_exprs([H|T], Bindings, Funs) ->
    case eval(H, Bindings, Funs) of
        {ok, _, Bindings1} ->
            eval_exprs(T, Bindings1, Funs);
        Error ->
            Error
    end.


eval_string(S, Bindings, Funs) ->
    eval(parse_util:parse_expr(S), Bindings, Funs).


convert_forms([]) ->
    [];
convert_forms([{function,_,F,A,Clauses}|T]) ->
    [{{F,A}, Clauses}|convert_forms(T)];
convert_forms([_|T]) ->
    convert_forms(T).


test(eval_integer) ->
    {ok, 1, []} = eval_string("1.", [], []),
    {ok, 2, []} = eval_string("2.", [], []),
    {ok, 3, []} = eval_string("3.", [], []),
    {ok,-1, []} = eval_string("-1.", [], []),
    ok;
test(eval_atom) ->
    {ok, a, []} = eval_string("a.", [], []),
    {ok, b, []} = eval_string("b.", [], []),
    {ok, c, []} = eval_string("c.", [], []),
    ok;
test(eval_string) ->
    {ok, "", []} = eval_string("\"\".", [], []),
    {ok, "abc", []} = eval_string("\"abc\".", [], []),
    ok;
test(eval_char) ->
    {ok, $a, []} = eval_string("$a.", [], []),
    ok;
test(eval_var) ->
    {ok, 1, [{'X', 1}]} = eval_string("X.", [{'X', 1}], []),
    {error, {unbound, 'X'}} = eval_string("X.", [], []),
    ok;
test(eval_list) ->
    {ok, [], []} = eval_string("[].", [], []),
    {ok, [1,2], []} = eval_string("[1,2].", [], []),
    {ok, [1,2], [{'X', 1}, {'Y', 2}]} = eval_string("[X,Y].", [{'X', 1}, {'Y', 2}], []),
    ok;
test(eval_tuple) ->
    {ok, {1,2}, []} = eval_string("{1,2}.", [], []),
    {ok, {1,2}, [{'X', 1}, {'Y', 2}]} = eval_string("{X,Y}.", [{'X', 1}, {'Y', 2}], []),
    ok;
test(match_integer) ->
    {ok, 1, []} = eval_string("1 = 1.", [], []),
    {error, {mismatch, 2}} = eval_string("1 = 2.", [], []),
    ok;
test(match_atom) ->
    {ok, a, []} = eval_string("a = a.", [], []),
    {error, {mismatch, b}} = eval_string("a = b.", [], []),
    ok;
test(match_string) ->
    {error, {mismatch, a}} = eval_string("\"a\" = a.", [], []),
    {ok, "a", []} = eval_string("\"a\" = \"a\".", [], []),
    ok;
test(match_char) ->
    {error, {mismatch, a}} = eval_string("$a = a.", [], []),
    {ok, $a, []} = eval_string("$a = $a.", [], []),
    ok;
test(match_var) ->
    {ok, a, [{'X', a}]} = eval_string("X = a.", [], []),
    {ok, a, [{'X', a}]} = eval_string("X = a.", [{'X', a}], []),
    {error, {mismatch, b}} = eval_string("X = a.", [{'X', b}], []),
    {ok, a, []} = eval_string("_ = a.", [], []),
    ok;
test(match_list) ->
    {ok, [], []} = eval_string("[] = [].", [], []),
    {error, {mismatch, 1}} = eval_string("[] = 1.", [], []),
    {ok, [1,2,3], []} = eval_string("[1,2,3] = [1,2,3].", [], []),
    {error, {mismatch, [1,2]}} = eval_string("[1,2,3] = [1,2].", [], []),
    ok;
test(match_tuple) ->
    {ok, {}, []} = eval_string("{} = {}.", [], []),
    {error, {mismatch, 1}} = eval_string("{} = 1.", [], []),
    {ok, {1,2,3}, []} = eval_string("{1,2,3} = {1,2,3}.", [], []),
    {error, {mismatch, {1,2}}} = eval_string("{1,2,3} = {1,2}.", [], []),
    ok;
test(eval_call) ->
    {ok, 1, []} = eval_string("seq:a(1).", [], []),
    {ok, 2, []} = eval_string("seq:a(2).", [], []),
    {ok, 3, []} = eval_string("seq:a(3).", [], []),
    {ok, 4, []} = eval_string("seq:a(4).", [], []),
    {ok, 1, []} = eval_string("seq:b(1).", [], []),
    {ok, 1, []} = eval_string("seq:b(2).", [], []),
    {ok, 1, []} = eval_string("seq:b(3).", [], []),
    {ok, 1, []} = eval_string("seq:b(4).", [], []),
    {ok, 1, []} = eval_string("seq:c(1).", [], []),
    {ok, 2, []} = eval_string("seq:c(2).", [], []),
    {ok, 3, []} = eval_string("seq:c(3).", [], []),
    {ok, 4, []} = eval_string("seq:c(4).", [], []),
    ok;
test(eval_op) ->
    {ok, -1, []} = eval_string("1-2.", [], []),
    ok;
test(eval_case) ->
    {ok, a, [{'X', 1}]} = eval_string("case X of 1 -> a end.", [{'X', 1}], []),
    {error, {mismatch, [2]}} = eval_string("case X of 1 -> a end.", [{'X', 2}], []),
    {ok, b, [{'X', 2}]} = eval_string("case X of 1 -> a; 2 -> b end.", [{'X', 2}], []),
    {ok, a, [{'X', 1}]} = eval_string("case X of X when X < 2; X > 4 -> a end.", [{'X', 1}], []),
    {ok, a, [{'X', 5}]} = eval_string("case X of X when X < 2; X > 4 -> a end.", [{'X', 5}], []),
    {ok, a, [{'X', 3}]} = eval_string("case X of X when X > 2, X < 4 -> a end.", [{'X', 3}], []),
    {ok, a, [{'X', 1}]} = eval_string("case X of X when erlang:is_integer(X) -> a end.", [{'X', 1}], []),
    ok;
test({module,FileName}) ->
    Funs = convert_forms(parse_util:parse_file(FileName)),
    {ok, ok, []} = eval_string("test().", [], Funs),
    ok.


test() ->
    test(eval_integer),
    test(eval_atom),
    test(eval_string),
    test(eval_char),
    test(eval_var),
    test(eval_list),
    test(eval_tuple),
    test(match_integer),
    test(match_atom),
    test(match_string),
    test(match_char),
    test(match_var),
    test(match_list),
    test(match_tuple),
    test(eval_call),
    test(eval_op),
    test(eval_case),
    test({module,"seq.erl"}),
    test({module,"board.erl"}),
    test({module,"bindings.erl"}),
    test({module,"parse_util.erl"}),
    test({module,"expression.erl"}),
    ok.


selftest() ->
    test({module,"function.erl"}),
    ok.
