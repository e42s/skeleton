==================
21天学通Erlang梗概
==================

文件内顺序大致以test为准。文件顺序如下


`seq <seq.erl>`_
函数，变量，匹配

`board <board.erl>`_
数据类型

`bindings <bindings.erl>`_
记录变量名和变量值的binding

`expression <expression.erl>`_
表达式求值(类似Erlang Shell)。

`function <function.erl>`_
函数求值(仅限一个模块)

`export_all <export_all.erl>`_  `all_exported <all_exported.erl>`_
简单的parse_transform

`parse_int <parse_int.erl>`_ `peg_transform <peg_transform.erl>`_ `peg_parse_int <peg_parse_int.erl>`_
PEG transform

`expr_parser <expr_parser.erl>`_
表达式解析
