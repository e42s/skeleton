==========
初识Erlang
==========

Erlang Shell中进行简单的运算

.. code::

    1> 1+1.
    2
    2> 5-2.
    3
    3> 2*2.
    4
    4> 1+2*2.
    5
    5> (1+2)*2.
    6
    6>

函数
====

数列

+------+---+---+---+---+
|  N   | 1 | 2 | 3 | 4 |
+------+---+---+---+---+
| a(N) | 1 | 2 | 3 | 4 |
+------+---+---+---+---+


用Erlang代码表示

.. code::

    a(1) -> 1;
    a(2) -> 2;
    a(3) -> 3;
    a(4) -> 4.


完整的一个文件

.. code::

    -module(seq).

    -export([a/1]).

    a(1) -> 1;
    a(2) -> 2;
    a(3) -> 3;
    a(4) -> 4.

在Erlang Shell中查看结果

.. code::

    1> c(seq).
    {ok,seq}
    2> seq:a(1).
    1
    3> seq:a(2).
    2
    4> seq:a(3).
    3
    5> seq:a(4).
    4
    6>


只定义到4，5就没有了。

.. code::

    1> seq:a(5).
    ** exception error: no function clause matching seq:a(5) (seq.erl, line 5)
    2>

只会采用按顺序第一个匹配。

.. code::

    a(1) -> 1;
    a(2) -> 2;
    a(3) -> 3;
    a(4) -> 4;
    a(4) -> 5.

编译时就有警告

.. code::

    1> c(seq).
    seq.erl:9: Warning: this clause cannot match because a previous clause at line 8 always matches
    {ok,seq2}
    2> seq:a(4).
    4
    3>

无穷项数列，每一项均为1

+------+---+---+---+---+-----+
|  N   | 1 | 2 | 3 | 4 | ... |
+------+---+---+---+---+-----+
| b(N) | 1 | 1 | 1 | 1 | ... |
+------+---+---+---+---+-----+

.. code::

    b(_) -> 1.


:code:`_` 表示无论传入的这个参数是什么，到这里都会匹配。

.. code::

    1> seq:b(1).
    1
    2> seq:b(2).
    1
    3> seq:b(3).
    1
    4> seq:b(4).
    1
    5>


无穷项数列，第1项为1，第2项为2，...，第N项为N

+------+---+---+---+---+-----+
|  N   | 1 | 2 | 3 | 4 | ... |
+------+---+---+---+---+-----+
| c(N) | 1 | 2 | 3 | 4 | ... |
+------+---+---+---+---+-----+


.. code::

    c(N) -> N.

.. code::

    1> seq:c(1).
    1
    2> seq:c(2).
    2
    3> seq:c(3).
    3
    4> seq:c(4).
    4
    5>


guard
=====

不想定义，但是没有出错

.. code::

    1> seq:b(0).
    1
    2> seq:b(-1).
    1
    3>

加上guard限制N的范围。

.. code::

    b(N) when N >= 1 -> 1.


出错信息

.. code::

    1> seq:b(0).
    ** exception error: no function clause matching seq:b(0) (seq.erl, line 5)
    2>


case
====

.. code::

    a(N) ->
        case N of
            1 -> 1;
            2 -> 2;
            3 -> 3;
            4 -> 4
        end.


match
=====

.. code::

    1> 1 = 1.
    1
    2> 1 = 1+1.
    ** exception error: no match of right hand side value 2
    3>


测试

.. code::

    test() ->
        1 = a(1),
        2 = a(2),
        3 = a(3),
        4 = a(4),
        ok.


atom()
======


.. code::

    1> a = a.
    a
    2> a = b.
    ** exception error: no match of right hand side value b
    3> a = aa.
    ** exception error: no match of right hand side value aa
    4>

保留字和特殊字符

.. code::

    1> case.
    * 1: syntax error before: '.'
    1> 'case'.
    'case'
    2> a = 'a'.
    a
    3>

tuple()
=======

.. code::

        1  2  3  4  5  6  7  8
      +--+--+--+--+--+--+--+--+
     1|  |  |  |  |  |  |  |  |
      +--+--+--+--+--+--+--+--+
     2|  |  |  |  |  |  |  |  |
      +--+--+--+--+--+--+--+--+
     3|  |  | A| B|  |  |  |  |
      +--+--+--+--+--+--+--+--+
     4|  |  | C|  |  |  |  |  |
      +--+--+--+--+--+--+--+--+
     5|  |  |  |  |  |  |  |  |
      +--+--+--+--+--+--+--+--+
     6|  |  |  |  |  |  |  |  |
      +--+--+--+--+--+--+--+--+
     7|  |  |  |  |  |  |  |  |
      +--+--+--+--+--+--+--+--+
     8|  |  |  |  |  |  |  |  |
      +--+--+--+--+--+--+--+--+

A :code:`{3,3}`, B :code:`{4,3}`, C :code:`{3,4}`


.. code::

    1> {} = {}.
    {}
    2> {1} = {}.
    ** exception error: no match of right hand side value {}
    3> {1} = {1}.
    {1}
    4> {1} = {1,1}.
    ** exception error: no match of right hand side value {1,1}
    5> {1,1} = {1,{1}}.
    ** exception error: no match of right hand side value {1,{1}}
    6>

up 上 down 下 left 左 right 右

.. code::

    test(move) ->
        A = {3,3},
        B = {4,3},
        C = {3,4},
        B = move(right, A),
        A = move(left, B),
        C = move(down, A),
        A = move(up, C).


.. code::

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


list()
======


.. code::

    test(moves) ->
        {4,1} = moves([right,right,right], {1,1}),
        {1,4} = moves([down,down,down], {1,1}).


.. code::

    1> [a, b] = [a|[b]].
    [a,b]
    2> [a, b, c] = [a,b|[c]].
    [a,b,c]
    3> [a] = [a|[]].
    [a]
    4> [a,b|_] = [a,b,c].
    [a,b,c]
    5> [a,b|_] = [a,b,c,d].
    [a,b,c,d]
    6>


.. code::

    moves([], From) ->
        From;
    moves([H|T], From) ->
        moves(T, move(H, From)).
