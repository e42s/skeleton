-module(export_all).

-export([parse_transform/2]).


funs([]) ->
    [];
funs([{function,_,F,A,_}|T]) ->
    [{F,A}|funs(T)];
funs([_|T]) ->
    funs(T).


parse_transform([File,Module|Forms], _Options) ->
    [File,Module,{attribute,1,export,funs(Forms)}|Forms].
