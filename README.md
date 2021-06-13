# toy-pascal-like

An interpreter for toy language, somewhat similar to PASCAL.

# Usage

The interpreter is written in [Rust](https://www.rust-lang.org/ja), so you need to prepare build environment first. You can easily install it using the installer distributed at <https://rustup.rs/>.

After installing, you can run the interpreter by running `cargo run {source-file-path}` in a console. `cargo` automatically builds the interpreter before running the executable, so you don't have to build it manually.

For example, you can run `fib.pas` located at the root directly in this way:

```
$ cargo run fib.pas
```

# Example

The example output for `fib.pas`:

```text
--- ast ---
1:1..12:4 @ Stmt
    1:1..12:4 @ Begin
        2:5..11:8 @ 2:5..2:13 @ Stmt
            2:5..2:13 @ Assg
              var: 2:5..2:8 @ Var(Ident(a_0))
              expr:
                2:12..2:13 @ ArithExpr
                    2:12..2:13 @ MulExpr
                        2:12..2:13 @ UnaryExpr
                            2:12..2:13 @ PrimaryExpr
                                2:12..2:13 @ Const(0)
        3:5..11:8 @ 3:5..3:13 @ Stmt
            3:5..3:13 @ Assg
              var: 3:5..3:8 @ Var(Ident(a_1))
              expr:
                3:12..3:13 @ ArithExpr
                    3:12..3:13 @ MulExpr
                        3:12..3:13 @ UnaryExpr
                            3:12..3:13 @ PrimaryExpr
                                3:12..3:13 @ Const(1)
        4:5..11:8 @ 4:5..4:11 @ Stmt
            4:5..4:11 @ Assg
              var: 4:5..4:6 @ Var(Ident(i))
              expr:
                4:10..4:11 @ ArithExpr
                    4:10..4:11 @ MulExpr
                        4:10..4:11 @ UnaryExpr
                            4:10..4:11 @ PrimaryExpr
                                4:10..4:11 @ Const(0)
        5:5..11:8 @ 5:5..11:8 @ Stmt
            5:5..11:8 @ While
              cond:
                5:11..5:17 @ BoolExpr
                  lhs:
                    5:11..5:12 @ ArithExpr
                        5:11..5:12 @ MulExpr
                            5:11..5:12 @ UnaryExpr
                                5:11..5:12 @ PrimaryExpr
                                    5:11..5:12 @ Var(Ident(i))
                  op: 5:13..5:14 @ CompareOp(<)
                  rhs:
                    5:15..5:17 @ ArithExpr
                        5:15..5:17 @ MulExpr
                            5:15..5:17 @ UnaryExpr
                                5:15..5:17 @ PrimaryExpr
                                    5:15..5:17 @ Const(20)
              body:
                5:21..11:8 @ Stmt
                    5:21..11:8 @ Begin
                        6:9..10:19 @ 6:9..6:17 @ Stmt
                            6:9..6:17 @ Dump
                              6:14..6:17 @ Var(Ident(a_0))
                        7:9..10:19 @ 7:9..7:25 @ Stmt
                            7:9..7:25 @ Assg
                              var: 7:9..7:12 @ Var(Ident(tmp))
                              expr:
                                7:16..7:25 @ ArithExpr
                                  lhs:
                                    7:16..7:19 @ ArithExpr
                                        7:16..7:19 @ MulExpr
                                            7:16..7:19 @ UnaryExpr
                                                7:16..7:19 @ PrimaryExpr
                                                    7:16..7:19 @ Var(Ident(a_0))
                                  rhs:
                                    7:22..7:25 @ MulExpr
                                        7:22..7:25 @ UnaryExpr
                                            7:22..7:25 @ PrimaryExpr
                                                7:22..7:25 @ Var(Ident(a_1))
                        8:9..10:19 @ 8:9..8:19 @ Stmt
                            8:9..8:19 @ Assg
                              var: 8:9..8:12 @ Var(Ident(a_0))
                              expr:
                                8:16..8:19 @ ArithExpr
                                    8:16..8:19 @ MulExpr
                                        8:16..8:19 @ UnaryExpr
                                            8:16..8:19 @ PrimaryExpr
                                                8:16..8:19 @ Var(Ident(a_1))
                        9:9..10:19 @ 9:9..9:19 @ Stmt
                            9:9..9:19 @ Assg
                              var: 9:9..9:12 @ Var(Ident(a_1))
                              expr:
                                9:16..9:19 @ ArithExpr
                                    9:16..9:19 @ MulExpr
                                        9:16..9:19 @ UnaryExpr
                                            9:16..9:19 @ PrimaryExpr
                                                9:16..9:19 @ Var(Ident(tmp))
                        10:9..10:19 @ 10:9..10:19 @ Stmt
                            10:9..10:19 @ Assg
                              var: 10:9..10:10 @ Var(Ident(i))
                              expr:
                                10:14..10:19 @ ArithExpr
                                  lhs:
                                    10:14..10:15 @ ArithExpr
                                        10:14..10:15 @ MulExpr
                                            10:14..10:15 @ UnaryExpr
                                                10:14..10:15 @ PrimaryExpr
                                                    10:14..10:15 @ Var(Ident(i))
                                  rhs:
                                    10:18..10:19 @ MulExpr
                                        10:18..10:19 @ UnaryExpr
                                            10:18..10:19 @ PrimaryExpr
                                                10:18..10:19 @ Const(1)


--- run ---
dump: a_0 = 0
dump: a_0 = 1
dump: a_0 = 1
dump: a_0 = 2
dump: a_0 = 3
dump: a_0 = 5
dump: a_0 = 8
dump: a_0 = 13
dump: a_0 = 21
dump: a_0 = 34
dump: a_0 = 55
dump: a_0 = 89
dump: a_0 = 144
dump: a_0 = 233
dump: a_0 = 377
dump: a_0 = 610
dump: a_0 = 987
dump: a_0 = 1597
dump: a_0 = 2584
dump: a_0 = 4181

--- final state ---
a_0 = 6765
i = 20
a_1 = 10946
tmp = 10946
```
