# toy-pascal-like

An interpreter for toy language, somewhat similar to PASCAL.

# Usage

The interpreter is written in [Rust](https://www.rust-lang.org/ja), so you need to prepare build environment first. You can easily install it using the installer distributed at <https://rustup.rs/>.

After installing, you can run the interpreter by running `cargo run {source-file-path}` in a console. `cargo` automatically builds the interpreter before running the executable, so you don't have to build it manually.

For example, you can run `fib.pas` located under `tests/compile-pass` directory:

```
$ cargo run tests/compile-pass/fib.pas
```

All of working examples are under `tests/compile-pass`.

# Quick Overview

This interpreter is running a program by the following sequence:

1. Split source code into tokens
1. Convert sequence of tokens into syntax tree structure, called AST.
1. Convert AST into somewhat easier tree structure, called HIR (stands for *High-Level Intermediate Representation* --- although I borrowed this naming from Rust language, but it's used in actually a bit different meaning in this project).

    For example, an arithmetic expression has the following structure in AST:
    ```
    AstArithExpr -> AstAddExpr -> AstMulExpr -> AstPrimaryExpr
    ```
    But distinction of ArithExpr, AddExpr and MulExpr is no longer needed, as operator precedence is explicit in the tree structure. Therefore, in HIR:
    ```
    HirArithExpr -> HirPrimaryExpr
    ```

    Another example is the list structures. AST has explicit linked list structure, for example:
    ```
    Nonempty { curr: _, next: Nonempty { curr: _, next: Empty } }
    ```
    This is however bothersome to deal with the list in practice. So, in HIR, we have simple array structure (`Vec<_>`) for those kind of information.

1. Convert HIR into name-resolved HIR, called RHIR.

    We resolve all names of types, functions and variables based on HIR, then converts it to the RHIR. So if you succeeded to create RHIR then you won't encounter unknown type names, undeclared functions and undeclared variables. Note that here type name resolution means resolving the type of directly written in the source code, such as function parameter's type or return type. Each expression's actual type are not inferred here. That's the next work we should do.

1. Convert RHIR into typed HIR, called THIR.

    We infer all types of each variables and expressions. At the same time, we check the types of each expressions are correct. For example, passing `int` value to the `float` variables, adding `float` value to the `int` value, comparing `int` value and `float` value, etc. is type mismatch error. We detect those mismatch in this stage.

    We also detect possibly uninitialized variables. For example, variables only assigned at inside `while` loop body is perhaps not assigned after the loop, because if the condition is initially `false`, loop body is never executed. We report reading those variables as an error in order to protect runtime error.

1. Run THIR.

    Thanks to the all works above, running THIR is safe in terms of it never cause runtime error.

# Example

We show the example output for `tests/compile-pass/fib.pas`. Currently the interpreter dumps all intermediate structures (AST, HIR, RHIR and THIR) for debugging.

```text
--- ast ---
Ast {
    span: 1:1..12:4,
    ast: AstBeginStmt {
        list: Ast {
            span: 2:5..11:8,
            ast: Nonempty {
                stmt: Ast {
                    span: 2:5..2:13,
                    ast: AssgStmt(
                        Ast {
                            span: 2:5..2:13,
                            ast: AstAssgStmt {
                                var: Ast {
                                    span: 2:5..2:8,
                                    ast: AstVar(
                                        Ast {
                                            span: 2:5..2:8,
                                            ast: AstIdent(
                                                "a_0",
                                            ),
                                        },
                                    ),
                                },
                                expr: Ast {
                                    span: 2:12..2:13,
                                    ast: AddExpr(
                                        Ast {
                                            span: 2:12..2:13,
                                            ast: MulExpr(
                                                Ast {
                                                    span: 2:12..2:13,
                                                    ast: UnaryExpr(
                                                        Ast {
                                                            span: 2:12..2:13,
                                                            ast: PrimaryExpr(
                                                                Ast {
                                                                    span: 2:12..2:13,
                                                                    ast: Const(
                                                                        Ast {
                                                                            span: 2:12..2:13,
                                                                            ast: Int(
                                                                                0,
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            ),
                                                        },
                                                    ),
                                                },
                                            ),
                                        },
                                    ),
                                },
                            },
                        },
                    ),
                },
                next: Ast {
                    span: 3:5..11:8,
                    ast: Nonempty {
                        stmt: Ast {
                            span: 3:5..3:13,
                            ast: AssgStmt(
                                Ast {
                                    span: 3:5..3:13,
                                    ast: AstAssgStmt {
                                        var: Ast {
                                            span: 3:5..3:8,
                                            ast: AstVar(
                                                Ast {
                                                    span: 3:5..3:8,
                                                    ast: AstIdent(
                                                        "a_1",
                                                    ),
                                                },
                                            ),
                                        },
                                        expr: Ast {
                                            span: 3:12..3:13,
                                            ast: AddExpr(
                                                Ast {
                                                    span: 3:12..3:13,
                                                    ast: MulExpr(
                                                        Ast {
                                                            span: 3:12..3:13,
                                                            ast: UnaryExpr(
                                                                Ast {
                                                                    span: 3:12..3:13,
                                                                    ast: PrimaryExpr(
                                                                        Ast {
                                                                            span: 3:12..3:13,
                                                                            ast: Const(
                                                                                Ast {
                                                                                    span: 3:12..3:13,
                                                                                    ast: Int(
                                                                                        1,
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            ),
                                                        },
                                                    ),
                                                },
                                            ),
                                        },
                                    },
                                },
                            ),
                        },
                        next: Ast {
                            span: 4:5..11:8,
                            ast: Nonempty {
                                stmt: Ast {
                                    span: 4:5..4:11,
                                    ast: AssgStmt(
                                        Ast {
                                            span: 4:5..4:11,
                                            ast: AstAssgStmt {
                                                var: Ast {
                                                    span: 4:5..4:6,
                                                    ast: AstVar(
                                                        Ast {
                                                            span: 4:5..4:6,
                                                            ast: AstIdent(
                                                                "i",
                                                            ),
                                                        },
                                                    ),
                                                },
                                                expr: Ast {
                                                    span: 4:10..4:11,
                                                    ast: AddExpr(
                                                        Ast {
                                                            span: 4:10..4:11,
                                                            ast: MulExpr(
                                                                Ast {
                                                                    span: 4:10..4:11,
                                                                    ast: UnaryExpr(
                                                                        Ast {
                                                                            span: 4:10..4:11,
                                                                            ast: PrimaryExpr(
                                                                                Ast {
                                                                                    span: 4:10..4:11,
                                                                                    ast: Const(
                                                                                        Ast {
                                                                                            span: 4:10..4:11,
                                                                                            ast: Int(
                                                                                                0,
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            ),
                                                        },
                                                    ),
                                                },
                                            },
                                        },
                                    ),
                                },
                                next: Ast {
                                    span: 5:5..11:8,
                                    ast: Nonempty {
                                        stmt: Ast {
                                            span: 5:5..11:8,
                                            ast: WhileStmt(
                                                Ast {
                                                    span: 5:5..11:8,
                                                    ast: AstWhileStmt {
                                                        cond: Ast {
                                                            span: 5:11..5:17,
                                                            ast: CompareExpr(
                                                                Ast {
                                                                    span: 5:13..5:14,
                                                                    ast: Lt,
                                                                },
                                                                Ast {
                                                                    span: 5:11..5:12,
                                                                    ast: AddExpr(
                                                                        Ast {
                                                                            span: 5:11..5:12,
                                                                            ast: MulExpr(
                                                                                Ast {
                                                                                    span: 5:11..5:12,
                                                                                    ast: UnaryExpr(
                                                                                        Ast {
                                                                                            span: 5:11..5:12,
                                                                                            ast: PrimaryExpr(
                                                                                                Ast {
                                                                                                    span: 5:11..5:12,
                                                                                                    ast: Var(
                                                                                                        Ast {
                                                                                                            span: 5:11..5:12,
                                                                                                            ast: AstVar(
                                                                                                                Ast {
                                                                                                                    span: 5:11..5:12,
                                                                                                                    ast: AstIdent(
                                                                                                                        "i",
                                                                                                                    ),
                                                                                                                },
                                                                                                            ),
                                                                                                        },
                                                                                                    ),
                                                                                                },
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                                Ast {
                                                                    span: 5:15..5:17,
                                                                    ast: MulExpr(
                                                                        Ast {
                                                                            span: 5:15..5:17,
                                                                            ast: UnaryExpr(
                                                                                Ast {
                                                                                    span: 5:15..5:17,
                                                                                    ast: PrimaryExpr(
                                                                                        Ast {
                                                                                            span: 5:15..5:17,
                                                                                            ast: Const(
                                                                                                Ast {
                                                                                                    span: 5:15..5:17,
                                                                                                    ast: Int(
                                                                                                        20,
                                                                                                    ),
                                                                                                },
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            ),
                                                        },
                                                        body: Ast {
                                                            span: 5:21..11:8,
                                                            ast: BeginStmt(
                                                                Ast {
                                                                    span: 5:21..11:8,
                                                                    ast: AstBeginStmt {
                                                                        list: Ast {
                                                                            span: 6:9..10:19,
                                                                            ast: Nonempty {
                                                                                stmt: Ast {
                                                                                    span: 6:9..6:17,
                                                                                    ast: DumpStmt(
                                                                                        Ast {
                                                                                            span: 6:9..6:17,
                                                                                            ast: AstDumpStmt {
                                                                                                var: Ast {
                                                                                                    span: 6:14..6:17,
                                                                                                    ast: AstVar(
                                                                                                        Ast {
                                                                                                            span: 6:14..6:17,
                                                                                                            ast: AstIdent(
                                                                                                                "a_0",
                                                                                                            ),
                                                                                                        },
                                                                                                    ),
                                                                                                },
                                                                                            },
                                                                                        },
                                                                                    ),
                                                                                },
                                                                                next: Ast {
                                                                                    span: 7:9..10:19,
                                                                                    ast: Nonempty {
                                                                                        stmt: Ast {
                                                                                            span: 7:9..7:25,
                                                                                            ast: AssgStmt(
                                                                                                Ast {
                                                                                                    span: 7:9..7:25,
                                                                                                    ast: AstAssgStmt {
                                                                                                        var: Ast {
                                                                                                            span: 7:9..7:12,
                                                                                                            ast: AstVar(
                                                                                                                Ast {
                                                                                                                    span: 7:9..7:12,
                                                                                                                    ast: AstIdent(
                                                                                                                        "tmp",
                                                                                                                    ),
                                                                                                                },
                                                                                                            ),
                                                                                                        },
                                                                                                        expr: Ast {
                                                                                                            span: 7:16..7:25,
                                                                                                            ast: AddExpr(
                                                                                                                Ast {
                                                                                                                    span: 7:16..7:25,
                                                                                                                    ast: Add(
                                                                                                                        Ast {
                                                                                                                            span: 7:16..7:19,
                                                                                                                            ast: MulExpr(
                                                                                                                                Ast {
                                                                                                                                    span: 7:16..7:19,
                                                                                                                                    ast: UnaryExpr(
                                                                                                                                        Ast {
                                                                                                                                            span: 7:16..7:19,
                                                                                                                                            ast: PrimaryExpr(
                                                                                                                                                Ast {
                                                                                                                                                    span: 7:16..7:19,
                                                                                                                                                    ast: Var(
                                                                                                                                                        Ast {
                                                                                                                                                            span: 7:16..7:19,
                                                                                                                                                            ast: AstVar(
                                                                                                                                                                Ast {
                                                                                                                                                                    span: 7:16..7:19,
                                                                                                                                                                    ast: AstIdent(
                                                                                                                                                                        "a_0",
                                                                                                                                                                    ),
                                                                                                                                                                },
                                                                                                                                                            ),
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                },
                                                                                                                            ),
                                                                                                                        },
                                                                                                                        Ast {
                                                                                                                            span: 7:22..7:25,
                                                                                                                            ast: UnaryExpr(
                                                                                                                                Ast {
                                                                                                                                    span: 7:22..7:25,
                                                                                                                                    ast: PrimaryExpr(
                                                                                                                                        Ast {
                                                                                                                                            span: 7:22..7:25,
                                                                                                                                            ast: Var(
                                                                                                                                                Ast {
                                                                                                                                                    span: 7:22..7:25,
                                                                                                                                                    ast: AstVar(
                                                                                                                                                        Ast {
                                                                                                                                                            span: 7:22..7:25,
                                                                                                                                                            ast: AstIdent(
                                                                                                                                                                "a_1",
                                                                                                                                                            ),
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                },
                                                                                                                            ),
                                                                                                                        },
                                                                                                                    ),
                                                                                                                },
                                                                                                            ),
                                                                                                        },
                                                                                                    },
                                                                                                },
                                                                                            ),
                                                                                        },
                                                                                        next: Ast {
                                                                                            span: 8:9..10:19,
                                                                                            ast: Nonempty {
                                                                                                stmt: Ast {
                                                                                                    span: 8:9..8:19,
                                                                                                    ast: AssgStmt(
                                                                                                        Ast {
                                                                                                            span: 8:9..8:19,
                                                                                                            ast: AstAssgStmt {
                                                                                                                var: Ast {
                                                                                                                    span: 8:9..8:12,
                                                                                                                    ast: AstVar(
                                                                                                                        Ast {
                                                                                                                            span: 8:9..8:12,
                                                                                                                            ast: AstIdent(
                                                                                                                                "a_0",
                                                                                                                            ),
                                                                                                                        },
                                                                                                                    ),
                                                                                                                },
                                                                                                                expr: Ast {
                                                                                                                    span: 8:16..8:19,
                                                                                                                    ast: AddExpr(
                                                                                                                        Ast {
                                                                                                                            span: 8:16..8:19,
                                                                                                                            ast: MulExpr(
                                                                                                                                Ast {
                                                                                                                                    span: 8:16..8:19,
                                                                                                                                    ast: UnaryExpr(
                                                                                                                                        Ast {
                                                                                                                                            span: 8:16..8:19,
                                                                                                                                            ast: PrimaryExpr(
                                                                                                                                                Ast {
                                                                                                                                                    span: 8:16..8:19,
                                                                                                                                                    ast: Var(
                                                                                                                                                        Ast {
                                                                                                                                                            span: 8:16..8:19,
                                                                                                                                                            ast: AstVar(
                                                                                                                                                                Ast {
                                                                                                                                                                    span: 8:16..8:19,
                                                                                                                                                                    ast: AstIdent(
                                                                                                                                                                        "a_1",
                                                                                                                                                                    ),
                                                                                                                                                                },
                                                                                                                                                            ),
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                },
                                                                                                                            ),
                                                                                                                        },
                                                                                                                    ),
                                                                                                                },
                                                                                                            },
                                                                                                        },
                                                                                                    ),
                                                                                                },
                                                                                                next: Ast {
                                                                                                    span: 9:9..10:19,
                                                                                                    ast: Nonempty {
                                                                                                        stmt: Ast {
                                                                                                            span: 9:9..9:19,
                                                                                                            ast: AssgStmt(
                                                                                                                Ast {
                                                                                                                    span: 9:9..9:19,
                                                                                                                    ast: AstAssgStmt {
                                                                                                                        var: Ast {
                                                                                                                            span: 9:9..9:12,
                                                                                                                            ast: AstVar(
                                                                                                                                Ast {
                                                                                                                                    span: 9:9..9:12,
                                                                                                                                    ast: AstIdent(
                                                                                                                                        "a_1",
                                                                                                                                    ),
                                                                                                                                },
                                                                                                                            ),
                                                                                                                        },
                                                                                                                        expr: Ast {
                                                                                                                            span: 9:16..9:19,
                                                                                                                            ast: AddExpr(
                                                                                                                                Ast {
                                                                                                                                    span: 9:16..9:19,
                                                                                                                                    ast: MulExpr(
                                                                                                                                        Ast {
                                                                                                                                            span: 9:16..9:19,
                                                                                                                                            ast: UnaryExpr(
                                                                                                                                                Ast {
                                                                                                                                                    span: 9:16..9:19,
                                                                                                                                                    ast: PrimaryExpr(
                                                                                                                                                        Ast {
                                                                                                                                                            span: 9:16..9:19,
                                                                                                                                                            ast: Var(
                                                                                                                                                                Ast {
                                                                                                                                                                    span: 9:16..9:19,
                                                                                                                                                                    ast: AstVar(
                                                                                                                                                                        Ast {
                                                                                                                                                                            span: 9:16..9:19,
                                                                                                                                                                            ast: AstIdent(
                                                                                                                                                                                "tmp",
                                                                                                                                                                            ),
                                                                                                                                                                        },
                                                                                                                                                                    ),
                                                                                                                                                                },
                                                                                                                                                            ),
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                },
                                                                                                                            ),
                                                                                                                        },
                                                                                                                    },
                                                                                                                },
                                                                                                            ),
                                                                                                        },
                                                                                                        next: Ast {
                                                                                                            span: 10:9..10:19,
                                                                                                            ast: Nonempty {
                                                                                                                stmt: Ast {
                                                                                                                    span: 10:9..10:19,
                                                                                                                    ast: AssgStmt(
                                                                                                                        Ast {
                                                                                                                            span: 10:9..10:19,
                                                                                                                            ast: AstAssgStmt {
                                                                                                                                var: Ast {
                                                                                                                                    span: 10:9..10:10,
                                                                                                                                    ast: AstVar(
                                                                                                                                        Ast {
                                                                                                                                            span: 10:9..10:10,
                                                                                                                                            ast: AstIdent(
                                                                                                                                                "i",
                                                                                                                                            ),
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                },
                                                                                                                                expr: Ast {
                                                                                                                                    span: 10:14..10:19,
                                                                                                                                    ast: AddExpr(
                                                                                                                                        Ast {
                                                                                                                                            span: 10:14..10:19,
                                                                                                                                            ast: Add(
                                                                                                                                                Ast {
                                                                                                                                                    span: 10:14..10:15,
                                                                                                                                                    ast: MulExpr(
                                                                                                                                                        Ast {
                                                                                                                                                            span: 10:14..10:15,
                                                                                                                                                            ast: UnaryExpr(
                                                                                                                                                                Ast {
                                                                                                                                                                    span: 10:14..10:15,
                                                                                                                                                                    ast: PrimaryExpr(
                                                                                                                                                                        Ast {
                                                                                                                                                                            span: 10:14..10:15,
                                                                                                                                                                            ast: Var(
                                                                                                                                                                                Ast {
                                                                                                                                                                                    span: 10:14..10:15,
                                                                                                                                                                                    ast: AstVar(
                                                                                                                                                                                        Ast {
                                                                                                                                                                                            span: 10:14..10:15,
                                                                                                                                                                                            ast: AstIdent(
                                                                                                                                                                                                "i",
                                                                                                                                                                                            ),
                                                                                                                                                                                        },
                                                                                                                                                                                    ),
                                                                                                                                                                                },
                                                                                                                                                                            ),
                                                                                                                                                                        },
                                                                                                                                                                    ),
                                                                                                                                                                },
                                                                                                                                                            ),
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                },
                                                                                                                                                Ast {
                                                                                                                                                    span: 10:18..10:19,
                                                                                                                                                    ast: UnaryExpr(
                                                                                                                                                        Ast {
                                                                                                                                                            span: 10:18..10:19,
                                                                                                                                                            ast: PrimaryExpr(
                                                                                                                                                                Ast {
                                                                                                                                                                    span: 10:18..10:19,
                                                                                                                                                                    ast: Const(
                                                                                                                                                                        Ast {
                                                                                                                                                                            span: 10:18..10:19,
                                                                                                                                                                            ast: Int(
                                                                                                                                                                                1,
                                                                                                                                                                            ),
                                                                                                                                                                        },
                                                                                                                                                                    ),
                                                                                                                                                                },
                                                                                                                                                            ),
                                                                                                                                                        },
                                                                                                                                                    ),
                                                                                                                                                },
                                                                                                                                            ),
                                                                                                                                        },
                                                                                                                                    ),
                                                                                                                                },
                                                                                                                            },
                                                                                                                        },
                                                                                                                    ),
                                                                                                                },
                                                                                                                next: Ast {
                                                                                                                    span: 11:5..11:5,
                                                                                                                    ast: Empty,
                                                                                                                },
                                                                                                            },
                                                                                                        },
                                                                                                    },
                                                                                                },
                                                                                            },
                                                                                        },
                                                                                    },
                                                                                },
                                                                            },
                                                                        },
                                                                    },
                                                                },
                                                            ),
                                                        },
                                                    },
                                                },
                                            ),
                                        },
                                        next: Ast {
                                            span: 12:1..12:1,
                                            ast: Empty,
                                        },
                                    },
                                },
                            },
                        },
                    },
                },
            },
        },
    },
}

--- hir ---
HirProgram {
    scopes: {
        ScopeId(
            0,
        ): HirScope {
            id: ScopeId(
                0,
            ),
            parent_id: None,
            fn_ids: [
                FnId(
                    1,
                ),
                FnId(
                    4,
                ),
                FnId(
                    7,
                ),
            ],
            vars: {},
        },
        ScopeId(
            2,
        ): HirScope {
            id: ScopeId(
                2,
            ),
            parent_id: Some(
                ScopeId(
                    0,
                ),
            ),
            fn_ids: [],
            vars: {
                VarId(
                    3,
                ): HirVar {
                    id: VarId(
                        3,
                    ),
                    name: Ident {
                        span: 1:1..1:1,
                        ident: "ftoi",
                    },
                    ty: HirTy {
                        span: 1:1..1:1,
                        res: RefCell {
                            value: Resolved(
                                Revealed(
                                    Int,
                                ),
                            ),
                        },
                    },
                },
            },
        },
        ScopeId(
            5,
        ): HirScope {
            id: ScopeId(
                5,
            ),
            parent_id: Some(
                ScopeId(
                    0,
                ),
            ),
            fn_ids: [],
            vars: {
                VarId(
                    6,
                ): HirVar {
                    id: VarId(
                        6,
                    ),
                    name: Ident {
                        span: 1:1..1:1,
                        ident: "itof",
                    },
                    ty: HirTy {
                        span: 1:1..1:1,
                        res: RefCell {
                            value: Resolved(
                                Revealed(
                                    Float,
                                ),
                            ),
                        },
                    },
                },
            },
        },
        ScopeId(
            8,
        ): HirScope {
            id: ScopeId(
                8,
            ),
            parent_id: Some(
                ScopeId(
                    0,
                ),
            ),
            fn_ids: [],
            vars: {
                VarId(
                    9,
                ): HirVar {
                    id: VarId(
                        9,
                    ),
                    name: Ident {
                        span: 1:1..1:1,
                        ident: "__start",
                    },
                    ty: HirTy {
                        span: 1:1..1:1,
                        res: RefCell {
                            value: Resolved(
                                Revealed(
                                    Void,
                                ),
                            ),
                        },
                    },
                },
                VarId(
                    10,
                ): HirVar {
                    id: VarId(
                        10,
                    ),
                    name: Ident {
                        span: 2:5..2:8,
                        ident: "a_0",
                    },
                    ty: HirTy {
                        span: 2:5..2:8,
                        res: RefCell {
                            value: Resolved(
                                Infer,
                            ),
                        },
                    },
                },
                VarId(
                    11,
                ): HirVar {
                    id: VarId(
                        11,
                    ),
                    name: Ident {
                        span: 3:5..3:8,
                        ident: "a_1",
                    },
                    ty: HirTy {
                        span: 3:5..3:8,
                        res: RefCell {
                            value: Resolved(
                                Infer,
                            ),
                        },
                    },
                },
                VarId(
                    12,
                ): HirVar {
                    id: VarId(
                        12,
                    ),
                    name: Ident {
                        span: 4:5..4:6,
                        ident: "i",
                    },
                    ty: HirTy {
                        span: 4:5..4:6,
                        res: RefCell {
                            value: Resolved(
                                Infer,
                            ),
                        },
                    },
                },
                VarId(
                    13,
                ): HirVar {
                    id: VarId(
                        13,
                    ),
                    name: Ident {
                        span: 7:9..7:12,
                        ident: "tmp",
                    },
                    ty: HirTy {
                        span: 7:9..7:12,
                        res: RefCell {
                            value: Resolved(
                                Infer,
                            ),
                        },
                    },
                },
            },
        },
    },
    start_fn_id: FnId(
        7,
    ),
    fndecls: {
        FnId(
            1,
        ): HirFnDecl {
            id: FnId(
                1,
            ),
            scope_id: ScopeId(
                0,
            ),
            span: 1:1..1:1,
            name: Ident {
                span: 1:1..1:1,
                ident: "ftoi",
            },
            params: [
                HirParam {
                    span: 1:1..1:1,
                    res: None,
                    ty: HirTy {
                        span: 1:1..1:1,
                        res: RefCell {
                            value: Resolved(
                                Revealed(
                                    Float,
                                ),
                            ),
                        },
                    },
                },
            ],
            ret_var: RefCell {
                value: Unresolved(
                    Ident {
                        span: 1:1..1:1,
                        ident: "ftoi",
                    },
                ),
            },
            ret_ty: HirTy {
                span: 1:1..1:1,
                res: RefCell {
                    value: Resolved(
                        Revealed(
                            Int,
                        ),
                    ),
                },
            },
        },
        FnId(
            4,
        ): HirFnDecl {
            id: FnId(
                4,
            ),
            scope_id: ScopeId(
                0,
            ),
            span: 1:1..1:1,
            name: Ident {
                span: 1:1..1:1,
                ident: "itof",
            },
            params: [
                HirParam {
                    span: 1:1..1:1,
                    res: None,
                    ty: HirTy {
                        span: 1:1..1:1,
                        res: RefCell {
                            value: Resolved(
                                Revealed(
                                    Int,
                                ),
                            ),
                        },
                    },
                },
            ],
            ret_var: RefCell {
                value: Unresolved(
                    Ident {
                        span: 1:1..1:1,
                        ident: "itof",
                    },
                ),
            },
            ret_ty: HirTy {
                span: 1:1..1:1,
                res: RefCell {
                    value: Resolved(
                        Revealed(
                            Float,
                        ),
                    ),
                },
            },
        },
        FnId(
            7,
        ): HirFnDecl {
            id: FnId(
                7,
            ),
            scope_id: ScopeId(
                0,
            ),
            span: 1:1..12:4,
            name: Ident {
                span: 1:1..1:1,
                ident: "__start",
            },
            params: [],
            ret_var: RefCell {
                value: Unresolved(
                    Ident {
                        span: 1:1..1:1,
                        ident: "__start",
                    },
                ),
            },
            ret_ty: HirTy {
                span: 1:1..1:1,
                res: RefCell {
                    value: Resolved(
                        Revealed(
                            Void,
                        ),
                    ),
                },
            },
        },
    },
    fnbodies: {
        FnId(
            1,
        ): HirFnBody {
            id: FnId(
                1,
            ),
            inner_scope_id: ScopeId(
                2,
            ),
            kind: Builtin(
                _,
            ),
        },
        FnId(
            4,
        ): HirFnBody {
            id: FnId(
                4,
            ),
            inner_scope_id: ScopeId(
                5,
            ),
            kind: Builtin(
                _,
            ),
        },
        FnId(
            7,
        ): HirFnBody {
            id: FnId(
                7,
            ),
            inner_scope_id: ScopeId(
                8,
            ),
            kind: Stmt(
                HirBeginStmt {
                    span: 1:1..12:4,
                    stmts: [
                        HirStmt {
                            span: 2:5..2:13,
                            kind: Assg(
                                HirAssgStmt {
                                    span: 2:5..2:13,
                                    var: HirVarRef {
                                        span: 2:5..2:8,
                                        res: RefCell {
                                            value: Unresolved(
                                                Ident {
                                                    span: 2:5..2:8,
                                                    ident: "a_0",
                                                },
                                            ),
                                        },
                                    },
                                    expr: HirArithExpr {
                                        span: 2:12..2:13,
                                        ty: HirTy {
                                            span: 2:12..2:13,
                                            res: RefCell {
                                                value: Resolved(
                                                    Infer,
                                                ),
                                            },
                                        },
                                        kind: Primary(
                                            HirPrimaryExpr {
                                                span: 2:12..2:13,
                                                ty: HirTy {
                                                    span: 2:12..2:13,
                                                    res: RefCell {
                                                        value: Resolved(
                                                            Infer,
                                                        ),
                                                    },
                                                },
                                                kind: Const(
                                                    HirConst {
                                                        span: 2:12..2:13,
                                                        ty: HirTy {
                                                            span: 2:12..2:13,
                                                            res: RefCell {
                                                                value: Resolved(
                                                                    Revealed(
                                                                        Int,
                                                                    ),
                                                                ),
                                                            },
                                                        },
                                                        value: Int(
                                                            0,
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                        HirStmt {
                            span: 3:5..3:13,
                            kind: Assg(
                                HirAssgStmt {
                                    span: 3:5..3:13,
                                    var: HirVarRef {
                                        span: 3:5..3:8,
                                        res: RefCell {
                                            value: Unresolved(
                                                Ident {
                                                    span: 3:5..3:8,
                                                    ident: "a_1",
                                                },
                                            ),
                                        },
                                    },
                                    expr: HirArithExpr {
                                        span: 3:12..3:13,
                                        ty: HirTy {
                                            span: 3:12..3:13,
                                            res: RefCell {
                                                value: Resolved(
                                                    Infer,
                                                ),
                                            },
                                        },
                                        kind: Primary(
                                            HirPrimaryExpr {
                                                span: 3:12..3:13,
                                                ty: HirTy {
                                                    span: 3:12..3:13,
                                                    res: RefCell {
                                                        value: Resolved(
                                                            Infer,
                                                        ),
                                                    },
                                                },
                                                kind: Const(
                                                    HirConst {
                                                        span: 3:12..3:13,
                                                        ty: HirTy {
                                                            span: 3:12..3:13,
                                                            res: RefCell {
                                                                value: Resolved(
                                                                    Revealed(
                                                                        Int,
                                                                    ),
                                                                ),
                                                            },
                                                        },
                                                        value: Int(
                                                            1,
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                        HirStmt {
                            span: 4:5..4:11,
                            kind: Assg(
                                HirAssgStmt {
                                    span: 4:5..4:11,
                                    var: HirVarRef {
                                        span: 4:5..4:6,
                                        res: RefCell {
                                            value: Unresolved(
                                                Ident {
                                                    span: 4:5..4:6,
                                                    ident: "i",
                                                },
                                            ),
                                        },
                                    },
                                    expr: HirArithExpr {
                                        span: 4:10..4:11,
                                        ty: HirTy {
                                            span: 4:10..4:11,
                                            res: RefCell {
                                                value: Resolved(
                                                    Infer,
                                                ),
                                            },
                                        },
                                        kind: Primary(
                                            HirPrimaryExpr {
                                                span: 4:10..4:11,
                                                ty: HirTy {
                                                    span: 4:10..4:11,
                                                    res: RefCell {
                                                        value: Resolved(
                                                            Infer,
                                                        ),
                                                    },
                                                },
                                                kind: Const(
                                                    HirConst {
                                                        span: 4:10..4:11,
                                                        ty: HirTy {
                                                            span: 4:10..4:11,
                                                            res: RefCell {
                                                                value: Resolved(
                                                                    Revealed(
                                                                        Int,
                                                                    ),
                                                                ),
                                                            },
                                                        },
                                                        value: Int(
                                                            0,
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                        HirStmt {
                            span: 5:5..11:8,
                            kind: While(
                                HirWhileStmt {
                                    span: 5:5..11:8,
                                    cond: HirArithExpr {
                                        span: 5:11..5:17,
                                        ty: HirTy {
                                            span: 5:11..5:17,
                                            res: RefCell {
                                                value: Resolved(
                                                    Infer,
                                                ),
                                            },
                                        },
                                        kind: BinOp(
                                            Lt,
                                            HirArithExpr {
                                                span: 5:11..5:12,
                                                ty: HirTy {
                                                    span: 5:11..5:12,
                                                    res: RefCell {
                                                        value: Resolved(
                                                            Infer,
                                                        ),
                                                    },
                                                },
                                                kind: Primary(
                                                    HirPrimaryExpr {
                                                        span: 5:11..5:12,
                                                        ty: HirTy {
                                                            span: 5:11..5:12,
                                                            res: RefCell {
                                                                value: Resolved(
                                                                    Infer,
                                                                ),
                                                            },
                                                        },
                                                        kind: Var(
                                                            HirVarRef {
                                                                span: 5:11..5:12,
                                                                res: RefCell {
                                                                    value: Unresolved(
                                                                        Ident {
                                                                            span: 5:11..5:12,
                                                                            ident: "i",
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                ),
                                            },
                                            HirArithExpr {
                                                span: 5:15..5:17,
                                                ty: HirTy {
                                                    span: 5:15..5:17,
                                                    res: RefCell {
                                                        value: Resolved(
                                                            Infer,
                                                        ),
                                                    },
                                                },
                                                kind: Primary(
                                                    HirPrimaryExpr {
                                                        span: 5:15..5:17,
                                                        ty: HirTy {
                                                            span: 5:15..5:17,
                                                            res: RefCell {
                                                                value: Resolved(
                                                                    Infer,
                                                                ),
                                                            },
                                                        },
                                                        kind: Const(
                                                            HirConst {
                                                                span: 5:15..5:17,
                                                                ty: HirTy {
                                                                    span: 5:15..5:17,
                                                                    res: RefCell {
                                                                        value: Resolved(
                                                                            Revealed(
                                                                                Int,
                                                                            ),
                                                                        ),
                                                                    },
                                                                },
                                                                value: Int(
                                                                    20,
                                                                ),
                                                            },
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                    body: HirStmt {
                                        span: 5:21..11:8,
                                        kind: Begin(
                                            HirBeginStmt {
                                                span: 5:21..11:8,
                                                stmts: [
                                                    HirStmt {
                                                        span: 6:9..6:17,
                                                        kind: Dump(
                                                            HirDumpStmt {
                                                                span: 6:9..6:17,
                                                                var: HirVarRef {
                                                                    span: 6:14..6:17,
                                                                    res: RefCell {
                                                                        value: Unresolved(
                                                                            Ident {
                                                                                span: 6:14..6:17,
                                                                                ident: "a_0",
                                                                            },
                                                                        ),
                                                                    },
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    HirStmt {
                                                        span: 7:9..7:25,
                                                        kind: Assg(
                                                            HirAssgStmt {
                                                                span: 7:9..7:25,
                                                                var: HirVarRef {
                                                                    span: 7:9..7:12,
                                                                    res: RefCell {
                                                                        value: Unresolved(
                                                                            Ident {
                                                                                span: 7:9..7:12,
                                                                                ident: "tmp",
                                                                            },
                                                                        ),
                                                                    },
                                                                },
                                                                expr: HirArithExpr {
                                                                    span: 7:16..7:25,
                                                                    ty: HirTy {
                                                                        span: 7:16..7:25,
                                                                        res: RefCell {
                                                                            value: Resolved(
                                                                                Infer,
                                                                            ),
                                                                        },
                                                                    },
                                                                    kind: BinOp(
                                                                        Add,
                                                                        HirArithExpr {
                                                                            span: 7:16..7:19,
                                                                            ty: HirTy {
                                                                                span: 7:16..7:19,
                                                                                res: RefCell {
                                                                                    value: Resolved(
                                                                                        Infer,
                                                                                    ),
                                                                                },
                                                                            },
                                                                            kind: Primary(
                                                                                HirPrimaryExpr {
                                                                                    span: 7:16..7:19,
                                                                                    ty: HirTy {
                                                                                        span: 7:16..7:19,
                                                                                        res: RefCell {
                                                                                            value: Resolved(
                                                                                                Infer,
                                                                                            ),
                                                                                        },
                                                                                    },
                                                                                    kind: Var(
                                                                                        HirVarRef {
                                                                                            span: 7:16..7:19,
                                                                                            res: RefCell {
                                                                                                value: Unresolved(
                                                                                                    Ident {
                                                                                                        span: 7:16..7:19,
                                                                                                        ident: "a_0",
                                                                                                    },
                                                                                                ),
                                                                                            },
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                        HirArithExpr {
                                                                            span: 7:22..7:25,
                                                                            ty: HirTy {
                                                                                span: 7:22..7:25,
                                                                                res: RefCell {
                                                                                    value: Resolved(
                                                                                        Infer,
                                                                                    ),
                                                                                },
                                                                            },
                                                                            kind: Primary(
                                                                                HirPrimaryExpr {
                                                                                    span: 7:22..7:25,
                                                                                    ty: HirTy {
                                                                                        span: 7:22..7:25,
                                                                                        res: RefCell {
                                                                                            value: Resolved(
                                                                                                Infer,
                                                                                            ),
                                                                                        },
                                                                                    },
                                                                                    kind: Var(
                                                                                        HirVarRef {
                                                                                            span: 7:22..7:25,
                                                                                            res: RefCell {
                                                                                                value: Unresolved(
                                                                                                    Ident {
                                                                                                        span: 7:22..7:25,
                                                                                                        ident: "a_1",
                                                                                                    },
                                                                                                ),
                                                                                            },
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    HirStmt {
                                                        span: 8:9..8:19,
                                                        kind: Assg(
                                                            HirAssgStmt {
                                                                span: 8:9..8:19,
                                                                var: HirVarRef {
                                                                    span: 8:9..8:12,
                                                                    res: RefCell {
                                                                        value: Unresolved(
                                                                            Ident {
                                                                                span: 8:9..8:12,
                                                                                ident: "a_0",
                                                                            },
                                                                        ),
                                                                    },
                                                                },
                                                                expr: HirArithExpr {
                                                                    span: 8:16..8:19,
                                                                    ty: HirTy {
                                                                        span: 8:16..8:19,
                                                                        res: RefCell {
                                                                            value: Resolved(
                                                                                Infer,
                                                                            ),
                                                                        },
                                                                    },
                                                                    kind: Primary(
                                                                        HirPrimaryExpr {
                                                                            span: 8:16..8:19,
                                                                            ty: HirTy {
                                                                                span: 8:16..8:19,
                                                                                res: RefCell {
                                                                                    value: Resolved(
                                                                                        Infer,
                                                                                    ),
                                                                                },
                                                                            },
                                                                            kind: Var(
                                                                                HirVarRef {
                                                                                    span: 8:16..8:19,
                                                                                    res: RefCell {
                                                                                        value: Unresolved(
                                                                                            Ident {
                                                                                                span: 8:16..8:19,
                                                                                                ident: "a_1",
                                                                                            },
                                                                                        ),
                                                                                    },
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    HirStmt {
                                                        span: 9:9..9:19,
                                                        kind: Assg(
                                                            HirAssgStmt {
                                                                span: 9:9..9:19,
                                                                var: HirVarRef {
                                                                    span: 9:9..9:12,
                                                                    res: RefCell {
                                                                        value: Unresolved(
                                                                            Ident {
                                                                                span: 9:9..9:12,
                                                                                ident: "a_1",
                                                                            },
                                                                        ),
                                                                    },
                                                                },
                                                                expr: HirArithExpr {
                                                                    span: 9:16..9:19,
                                                                    ty: HirTy {
                                                                        span: 9:16..9:19,
                                                                        res: RefCell {
                                                                            value: Resolved(
                                                                                Infer,
                                                                            ),
                                                                        },
                                                                    },
                                                                    kind: Primary(
                                                                        HirPrimaryExpr {
                                                                            span: 9:16..9:19,
                                                                            ty: HirTy {
                                                                                span: 9:16..9:19,
                                                                                res: RefCell {
                                                                                    value: Resolved(
                                                                                        Infer,
                                                                                    ),
                                                                                },
                                                                            },
                                                                            kind: Var(
                                                                                HirVarRef {
                                                                                    span: 9:16..9:19,
                                                                                    res: RefCell {
                                                                                        value: Unresolved(
                                                                                            Ident {
                                                                                                span: 9:16..9:19,
                                                                                                ident: "tmp",
                                                                                            },
                                                                                        ),
                                                                                    },
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    HirStmt {
                                                        span: 10:9..10:19,
                                                        kind: Assg(
                                                            HirAssgStmt {
                                                                span: 10:9..10:19,
                                                                var: HirVarRef {
                                                                    span: 10:9..10:10,
                                                                    res: RefCell {
                                                                        value: Unresolved(
                                                                            Ident {
                                                                                span: 10:9..10:10,
                                                                                ident: "i",
                                                                            },
                                                                        ),
                                                                    },
                                                                },
                                                                expr: HirArithExpr {
                                                                    span: 10:14..10:19,
                                                                    ty: HirTy {
                                                                        span: 10:14..10:19,
                                                                        res: RefCell {
                                                                            value: Resolved(
                                                                                Infer,
                                                                            ),
                                                                        },
                                                                    },
                                                                    kind: BinOp(
                                                                        Add,
                                                                        HirArithExpr {
                                                                            span: 10:14..10:15,
                                                                            ty: HirTy {
                                                                                span: 10:14..10:15,
                                                                                res: RefCell {
                                                                                    value: Resolved(
                                                                                        Infer,
                                                                                    ),
                                                                                },
                                                                            },
                                                                            kind: Primary(
                                                                                HirPrimaryExpr {
                                                                                    span: 10:14..10:15,
                                                                                    ty: HirTy {
                                                                                        span: 10:14..10:15,
                                                                                        res: RefCell {
                                                                                            value: Resolved(
                                                                                                Infer,
                                                                                            ),
                                                                                        },
                                                                                    },
                                                                                    kind: Var(
                                                                                        HirVarRef {
                                                                                            span: 10:14..10:15,
                                                                                            res: RefCell {
                                                                                                value: Unresolved(
                                                                                                    Ident {
                                                                                                        span: 10:14..10:15,
                                                                                                        ident: "i",
                                                                                                    },
                                                                                                ),
                                                                                            },
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                        HirArithExpr {
                                                                            span: 10:18..10:19,
                                                                            ty: HirTy {
                                                                                span: 10:18..10:19,
                                                                                res: RefCell {
                                                                                    value: Resolved(
                                                                                        Infer,
                                                                                    ),
                                                                                },
                                                                            },
                                                                            kind: Primary(
                                                                                HirPrimaryExpr {
                                                                                    span: 10:18..10:19,
                                                                                    ty: HirTy {
                                                                                        span: 10:18..10:19,
                                                                                        res: RefCell {
                                                                                            value: Resolved(
                                                                                                Infer,
                                                                                            ),
                                                                                        },
                                                                                    },
                                                                                    kind: Const(
                                                                                        HirConst {
                                                                                            span: 10:18..10:19,
                                                                                            ty: HirTy {
                                                                                                span: 10:18..10:19,
                                                                                                res: RefCell {
                                                                                                    value: Resolved(
                                                                                                        Revealed(
                                                                                                            Int,
                                                                                                        ),
                                                                                                    ),
                                                                                                },
                                                                                            },
                                                                                            value: Int(
                                                                                                1,
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                ],
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                    ],
                },
            ),
        },
    },
}

--- resolved hir ---
RhirProgram {
    scopes: {
        ScopeId(
            0,
        ): RhirScope {
            id: ScopeId(
                0,
            ),
            parent_id: None,
            fn_ids: [
                FnId(
                    1,
                ),
                FnId(
                    4,
                ),
                FnId(
                    7,
                ),
            ],
            vars: {},
        },
        ScopeId(
            2,
        ): RhirScope {
            id: ScopeId(
                2,
            ),
            parent_id: Some(
                ScopeId(
                    0,
                ),
            ),
            fn_ids: [],
            vars: {
                VarId(
                    3,
                ): RhirVar {
                    id: VarId(
                        3,
                    ),
                    name: Ident {
                        span: 1:1..1:1,
                        ident: "ftoi",
                    },
                    ty: RhirTy {
                        span: 1:1..1:1,
                        res: RefCell {
                            value: Revealed(
                                Int,
                            ),
                        },
                    },
                },
            },
        },
        ScopeId(
            5,
        ): RhirScope {
            id: ScopeId(
                5,
            ),
            parent_id: Some(
                ScopeId(
                    0,
                ),
            ),
            fn_ids: [],
            vars: {
                VarId(
                    6,
                ): RhirVar {
                    id: VarId(
                        6,
                    ),
                    name: Ident {
                        span: 1:1..1:1,
                        ident: "itof",
                    },
                    ty: RhirTy {
                        span: 1:1..1:1,
                        res: RefCell {
                            value: Revealed(
                                Float,
                            ),
                        },
                    },
                },
            },
        },
        ScopeId(
            8,
        ): RhirScope {
            id: ScopeId(
                8,
            ),
            parent_id: Some(
                ScopeId(
                    0,
                ),
            ),
            fn_ids: [],
            vars: {
                VarId(
                    9,
                ): RhirVar {
                    id: VarId(
                        9,
                    ),
                    name: Ident {
                        span: 1:1..1:1,
                        ident: "__start",
                    },
                    ty: RhirTy {
                        span: 1:1..1:1,
                        res: RefCell {
                            value: Revealed(
                                Void,
                            ),
                        },
                    },
                },
                VarId(
                    10,
                ): RhirVar {
                    id: VarId(
                        10,
                    ),
                    name: Ident {
                        span: 2:5..2:8,
                        ident: "a_0",
                    },
                    ty: RhirTy {
                        span: 2:5..2:8,
                        res: RefCell {
                            value: Infer,
                        },
                    },
                },
                VarId(
                    11,
                ): RhirVar {
                    id: VarId(
                        11,
                    ),
                    name: Ident {
                        span: 3:5..3:8,
                        ident: "a_1",
                    },
                    ty: RhirTy {
                        span: 3:5..3:8,
                        res: RefCell {
                            value: Infer,
                        },
                    },
                },
                VarId(
                    12,
                ): RhirVar {
                    id: VarId(
                        12,
                    ),
                    name: Ident {
                        span: 4:5..4:6,
                        ident: "i",
                    },
                    ty: RhirTy {
                        span: 4:5..4:6,
                        res: RefCell {
                            value: Infer,
                        },
                    },
                },
                VarId(
                    13,
                ): RhirVar {
                    id: VarId(
                        13,
                    ),
                    name: Ident {
                        span: 7:9..7:12,
                        ident: "tmp",
                    },
                    ty: RhirTy {
                        span: 7:9..7:12,
                        res: RefCell {
                            value: Infer,
                        },
                    },
                },
            },
        },
    },
    start_fn_id: FnId(
        7,
    ),
    fndecls: {
        FnId(
            1,
        ): RhirFnDecl {
            id: FnId(
                1,
            ),
            scope_id: ScopeId(
                0,
            ),
            span: 1:1..1:1,
            name: Ident {
                span: 1:1..1:1,
                ident: "ftoi",
            },
            params: [
                RhirParam {
                    span: 1:1..1:1,
                    res: None,
                    ty: RhirTy {
                        span: 1:1..1:1,
                        res: RefCell {
                            value: Revealed(
                                Float,
                            ),
                        },
                    },
                },
            ],
            ret_var: VarId(
                3,
            ),
            ret_ty: RhirTy {
                span: 1:1..1:1,
                res: RefCell {
                    value: Revealed(
                        Int,
                    ),
                },
            },
        },
        FnId(
            4,
        ): RhirFnDecl {
            id: FnId(
                4,
            ),
            scope_id: ScopeId(
                0,
            ),
            span: 1:1..1:1,
            name: Ident {
                span: 1:1..1:1,
                ident: "itof",
            },
            params: [
                RhirParam {
                    span: 1:1..1:1,
                    res: None,
                    ty: RhirTy {
                        span: 1:1..1:1,
                        res: RefCell {
                            value: Revealed(
                                Int,
                            ),
                        },
                    },
                },
            ],
            ret_var: VarId(
                6,
            ),
            ret_ty: RhirTy {
                span: 1:1..1:1,
                res: RefCell {
                    value: Revealed(
                        Float,
                    ),
                },
            },
        },
        FnId(
            7,
        ): RhirFnDecl {
            id: FnId(
                7,
            ),
            scope_id: ScopeId(
                0,
            ),
            span: 1:1..12:4,
            name: Ident {
                span: 1:1..1:1,
                ident: "__start",
            },
            params: [],
            ret_var: VarId(
                9,
            ),
            ret_ty: RhirTy {
                span: 1:1..1:1,
                res: RefCell {
                    value: Revealed(
                        Void,
                    ),
                },
            },
        },
    },
    fnbodies: {
        FnId(
            1,
        ): RhirFnBody {
            id: FnId(
                1,
            ),
            inner_scope_id: ScopeId(
                2,
            ),
            kind: Builtin(
                _,
            ),
        },
        FnId(
            4,
        ): RhirFnBody {
            id: FnId(
                4,
            ),
            inner_scope_id: ScopeId(
                5,
            ),
            kind: Builtin(
                _,
            ),
        },
        FnId(
            7,
        ): RhirFnBody {
            id: FnId(
                7,
            ),
            inner_scope_id: ScopeId(
                8,
            ),
            kind: Stmt(
                RhirBeginStmt {
                    span: 1:1..12:4,
                    stmts: [
                        RhirStmt {
                            span: 2:5..2:13,
                            kind: Assg(
                                RhirAssgStmt {
                                    span: 2:5..2:13,
                                    var: RhirVarRef {
                                        span: 2:5..2:8,
                                        res: VarId(
                                            10,
                                        ),
                                    },
                                    expr: RhirArithExpr {
                                        span: 2:12..2:13,
                                        ty: RhirTy {
                                            span: 2:12..2:13,
                                            res: RefCell {
                                                value: Infer,
                                            },
                                        },
                                        kind: Primary(
                                            RhirPrimaryExpr {
                                                span: 2:12..2:13,
                                                ty: RhirTy {
                                                    span: 2:12..2:13,
                                                    res: RefCell {
                                                        value: Infer,
                                                    },
                                                },
                                                kind: Const(
                                                    RhirConst {
                                                        span: 2:12..2:13,
                                                        ty: RhirTy {
                                                            span: 2:12..2:13,
                                                            res: RefCell {
                                                                value: Revealed(
                                                                    Int,
                                                                ),
                                                            },
                                                        },
                                                        value: Int(
                                                            0,
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                        RhirStmt {
                            span: 3:5..3:13,
                            kind: Assg(
                                RhirAssgStmt {
                                    span: 3:5..3:13,
                                    var: RhirVarRef {
                                        span: 3:5..3:8,
                                        res: VarId(
                                            11,
                                        ),
                                    },
                                    expr: RhirArithExpr {
                                        span: 3:12..3:13,
                                        ty: RhirTy {
                                            span: 3:12..3:13,
                                            res: RefCell {
                                                value: Infer,
                                            },
                                        },
                                        kind: Primary(
                                            RhirPrimaryExpr {
                                                span: 3:12..3:13,
                                                ty: RhirTy {
                                                    span: 3:12..3:13,
                                                    res: RefCell {
                                                        value: Infer,
                                                    },
                                                },
                                                kind: Const(
                                                    RhirConst {
                                                        span: 3:12..3:13,
                                                        ty: RhirTy {
                                                            span: 3:12..3:13,
                                                            res: RefCell {
                                                                value: Revealed(
                                                                    Int,
                                                                ),
                                                            },
                                                        },
                                                        value: Int(
                                                            1,
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                        RhirStmt {
                            span: 4:5..4:11,
                            kind: Assg(
                                RhirAssgStmt {
                                    span: 4:5..4:11,
                                    var: RhirVarRef {
                                        span: 4:5..4:6,
                                        res: VarId(
                                            12,
                                        ),
                                    },
                                    expr: RhirArithExpr {
                                        span: 4:10..4:11,
                                        ty: RhirTy {
                                            span: 4:10..4:11,
                                            res: RefCell {
                                                value: Infer,
                                            },
                                        },
                                        kind: Primary(
                                            RhirPrimaryExpr {
                                                span: 4:10..4:11,
                                                ty: RhirTy {
                                                    span: 4:10..4:11,
                                                    res: RefCell {
                                                        value: Infer,
                                                    },
                                                },
                                                kind: Const(
                                                    RhirConst {
                                                        span: 4:10..4:11,
                                                        ty: RhirTy {
                                                            span: 4:10..4:11,
                                                            res: RefCell {
                                                                value: Revealed(
                                                                    Int,
                                                                ),
                                                            },
                                                        },
                                                        value: Int(
                                                            0,
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                        RhirStmt {
                            span: 5:5..11:8,
                            kind: While(
                                RhirWhileStmt {
                                    span: 5:5..11:8,
                                    cond: RhirArithExpr {
                                        span: 5:11..5:17,
                                        ty: RhirTy {
                                            span: 5:11..5:17,
                                            res: RefCell {
                                                value: Infer,
                                            },
                                        },
                                        kind: BinOp(
                                            Lt,
                                            RhirArithExpr {
                                                span: 5:11..5:12,
                                                ty: RhirTy {
                                                    span: 5:11..5:12,
                                                    res: RefCell {
                                                        value: Infer,
                                                    },
                                                },
                                                kind: Primary(
                                                    RhirPrimaryExpr {
                                                        span: 5:11..5:12,
                                                        ty: RhirTy {
                                                            span: 5:11..5:12,
                                                            res: RefCell {
                                                                value: Infer,
                                                            },
                                                        },
                                                        kind: Var(
                                                            RhirVarRef {
                                                                span: 5:11..5:12,
                                                                res: VarId(
                                                                    12,
                                                                ),
                                                            },
                                                        ),
                                                    },
                                                ),
                                            },
                                            RhirArithExpr {
                                                span: 5:15..5:17,
                                                ty: RhirTy {
                                                    span: 5:15..5:17,
                                                    res: RefCell {
                                                        value: Infer,
                                                    },
                                                },
                                                kind: Primary(
                                                    RhirPrimaryExpr {
                                                        span: 5:15..5:17,
                                                        ty: RhirTy {
                                                            span: 5:15..5:17,
                                                            res: RefCell {
                                                                value: Infer,
                                                            },
                                                        },
                                                        kind: Const(
                                                            RhirConst {
                                                                span: 5:15..5:17,
                                                                ty: RhirTy {
                                                                    span: 5:15..5:17,
                                                                    res: RefCell {
                                                                        value: Revealed(
                                                                            Int,
                                                                        ),
                                                                    },
                                                                },
                                                                value: Int(
                                                                    20,
                                                                ),
                                                            },
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                    body: RhirStmt {
                                        span: 5:21..11:8,
                                        kind: Begin(
                                            RhirBeginStmt {
                                                span: 5:21..11:8,
                                                stmts: [
                                                    RhirStmt {
                                                        span: 6:9..6:17,
                                                        kind: Dump(
                                                            RhirDumpStmt {
                                                                span: 6:9..6:17,
                                                                var: RhirVarRef {
                                                                    span: 6:14..6:17,
                                                                    res: VarId(
                                                                        10,
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    RhirStmt {
                                                        span: 7:9..7:25,
                                                        kind: Assg(
                                                            RhirAssgStmt {
                                                                span: 7:9..7:25,
                                                                var: RhirVarRef {
                                                                    span: 7:9..7:12,
                                                                    res: VarId(
                                                                        13,
                                                                    ),
                                                                },
                                                                expr: RhirArithExpr {
                                                                    span: 7:16..7:25,
                                                                    ty: RhirTy {
                                                                        span: 7:16..7:25,
                                                                        res: RefCell {
                                                                            value: Infer,
                                                                        },
                                                                    },
                                                                    kind: BinOp(
                                                                        Add,
                                                                        RhirArithExpr {
                                                                            span: 7:16..7:19,
                                                                            ty: RhirTy {
                                                                                span: 7:16..7:19,
                                                                                res: RefCell {
                                                                                    value: Infer,
                                                                                },
                                                                            },
                                                                            kind: Primary(
                                                                                RhirPrimaryExpr {
                                                                                    span: 7:16..7:19,
                                                                                    ty: RhirTy {
                                                                                        span: 7:16..7:19,
                                                                                        res: RefCell {
                                                                                            value: Infer,
                                                                                        },
                                                                                    },
                                                                                    kind: Var(
                                                                                        RhirVarRef {
                                                                                            span: 7:16..7:19,
                                                                                            res: VarId(
                                                                                                10,
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                        RhirArithExpr {
                                                                            span: 7:22..7:25,
                                                                            ty: RhirTy {
                                                                                span: 7:22..7:25,
                                                                                res: RefCell {
                                                                                    value: Infer,
                                                                                },
                                                                            },
                                                                            kind: Primary(
                                                                                RhirPrimaryExpr {
                                                                                    span: 7:22..7:25,
                                                                                    ty: RhirTy {
                                                                                        span: 7:22..7:25,
                                                                                        res: RefCell {
                                                                                            value: Infer,
                                                                                        },
                                                                                    },
                                                                                    kind: Var(
                                                                                        RhirVarRef {
                                                                                            span: 7:22..7:25,
                                                                                            res: VarId(
                                                                                                11,
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    RhirStmt {
                                                        span: 8:9..8:19,
                                                        kind: Assg(
                                                            RhirAssgStmt {
                                                                span: 8:9..8:19,
                                                                var: RhirVarRef {
                                                                    span: 8:9..8:12,
                                                                    res: VarId(
                                                                        10,
                                                                    ),
                                                                },
                                                                expr: RhirArithExpr {
                                                                    span: 8:16..8:19,
                                                                    ty: RhirTy {
                                                                        span: 8:16..8:19,
                                                                        res: RefCell {
                                                                            value: Infer,
                                                                        },
                                                                    },
                                                                    kind: Primary(
                                                                        RhirPrimaryExpr {
                                                                            span: 8:16..8:19,
                                                                            ty: RhirTy {
                                                                                span: 8:16..8:19,
                                                                                res: RefCell {
                                                                                    value: Infer,
                                                                                },
                                                                            },
                                                                            kind: Var(
                                                                                RhirVarRef {
                                                                                    span: 8:16..8:19,
                                                                                    res: VarId(
                                                                                        11,
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    RhirStmt {
                                                        span: 9:9..9:19,
                                                        kind: Assg(
                                                            RhirAssgStmt {
                                                                span: 9:9..9:19,
                                                                var: RhirVarRef {
                                                                    span: 9:9..9:12,
                                                                    res: VarId(
                                                                        11,
                                                                    ),
                                                                },
                                                                expr: RhirArithExpr {
                                                                    span: 9:16..9:19,
                                                                    ty: RhirTy {
                                                                        span: 9:16..9:19,
                                                                        res: RefCell {
                                                                            value: Infer,
                                                                        },
                                                                    },
                                                                    kind: Primary(
                                                                        RhirPrimaryExpr {
                                                                            span: 9:16..9:19,
                                                                            ty: RhirTy {
                                                                                span: 9:16..9:19,
                                                                                res: RefCell {
                                                                                    value: Infer,
                                                                                },
                                                                            },
                                                                            kind: Var(
                                                                                RhirVarRef {
                                                                                    span: 9:16..9:19,
                                                                                    res: VarId(
                                                                                        13,
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    RhirStmt {
                                                        span: 10:9..10:19,
                                                        kind: Assg(
                                                            RhirAssgStmt {
                                                                span: 10:9..10:19,
                                                                var: RhirVarRef {
                                                                    span: 10:9..10:10,
                                                                    res: VarId(
                                                                        12,
                                                                    ),
                                                                },
                                                                expr: RhirArithExpr {
                                                                    span: 10:14..10:19,
                                                                    ty: RhirTy {
                                                                        span: 10:14..10:19,
                                                                        res: RefCell {
                                                                            value: Infer,
                                                                        },
                                                                    },
                                                                    kind: BinOp(
                                                                        Add,
                                                                        RhirArithExpr {
                                                                            span: 10:14..10:15,
                                                                            ty: RhirTy {
                                                                                span: 10:14..10:15,
                                                                                res: RefCell {
                                                                                    value: Infer,
                                                                                },
                                                                            },
                                                                            kind: Primary(
                                                                                RhirPrimaryExpr {
                                                                                    span: 10:14..10:15,
                                                                                    ty: RhirTy {
                                                                                        span: 10:14..10:15,
                                                                                        res: RefCell {
                                                                                            value: Infer,
                                                                                        },
                                                                                    },
                                                                                    kind: Var(
                                                                                        RhirVarRef {
                                                                                            span: 10:14..10:15,
                                                                                            res: VarId(
                                                                                                12,
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                        RhirArithExpr {
                                                                            span: 10:18..10:19,
                                                                            ty: RhirTy {
                                                                                span: 10:18..10:19,
                                                                                res: RefCell {
                                                                                    value: Infer,
                                                                                },
                                                                            },
                                                                            kind: Primary(
                                                                                RhirPrimaryExpr {
                                                                                    span: 10:18..10:19,
                                                                                    ty: RhirTy {
                                                                                        span: 10:18..10:19,
                                                                                        res: RefCell {
                                                                                            value: Infer,
                                                                                        },
                                                                                    },
                                                                                    kind: Const(
                                                                                        RhirConst {
                                                                                            span: 10:18..10:19,
                                                                                            ty: RhirTy {
                                                                                                span: 10:18..10:19,
                                                                                                res: RefCell {
                                                                                                    value: Revealed(
                                                                                                        Int,
                                                                                                    ),
                                                                                                },
                                                                                            },
                                                                                            value: Int(
                                                                                                1,
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                ],
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                    ],
                },
            ),
        },
    },
}

--- typed hir ---
ThirProgram {
    scopes: {
        ScopeId(
            0,
        ): ThirScope {
            id: ScopeId(
                0,
            ),
            parent_id: None,
            fn_ids: [
                FnId(
                    1,
                ),
                FnId(
                    4,
                ),
                FnId(
                    7,
                ),
            ],
            vars: {},
        },
        ScopeId(
            2,
        ): ThirScope {
            id: ScopeId(
                2,
            ),
            parent_id: Some(
                ScopeId(
                    0,
                ),
            ),
            fn_ids: [],
            vars: {
                VarId(
                    3,
                ): ThirVar {
                    id: VarId(
                        3,
                    ),
                    name: Ident {
                        span: 1:1..1:1,
                        ident: "ftoi",
                    },
                    ty: ThirTy {
                        span: 1:1..1:1,
                        res: Int,
                    },
                },
            },
        },
        ScopeId(
            5,
        ): ThirScope {
            id: ScopeId(
                5,
            ),
            parent_id: Some(
                ScopeId(
                    0,
                ),
            ),
            fn_ids: [],
            vars: {
                VarId(
                    6,
                ): ThirVar {
                    id: VarId(
                        6,
                    ),
                    name: Ident {
                        span: 1:1..1:1,
                        ident: "itof",
                    },
                    ty: ThirTy {
                        span: 1:1..1:1,
                        res: Float,
                    },
                },
            },
        },
        ScopeId(
            8,
        ): ThirScope {
            id: ScopeId(
                8,
            ),
            parent_id: Some(
                ScopeId(
                    0,
                ),
            ),
            fn_ids: [],
            vars: {
                VarId(
                    9,
                ): ThirVar {
                    id: VarId(
                        9,
                    ),
                    name: Ident {
                        span: 1:1..1:1,
                        ident: "__start",
                    },
                    ty: ThirTy {
                        span: 1:1..1:1,
                        res: Void,
                    },
                },
                VarId(
                    10,
                ): ThirVar {
                    id: VarId(
                        10,
                    ),
                    name: Ident {
                        span: 2:5..2:8,
                        ident: "a_0",
                    },
                    ty: ThirTy {
                        span: 2:5..2:8,
                        res: Int,
                    },
                },
                VarId(
                    11,
                ): ThirVar {
                    id: VarId(
                        11,
                    ),
                    name: Ident {
                        span: 3:5..3:8,
                        ident: "a_1",
                    },
                    ty: ThirTy {
                        span: 3:5..3:8,
                        res: Int,
                    },
                },
                VarId(
                    12,
                ): ThirVar {
                    id: VarId(
                        12,
                    ),
                    name: Ident {
                        span: 4:5..4:6,
                        ident: "i",
                    },
                    ty: ThirTy {
                        span: 4:5..4:6,
                        res: Int,
                    },
                },
                VarId(
                    13,
                ): ThirVar {
                    id: VarId(
                        13,
                    ),
                    name: Ident {
                        span: 7:9..7:12,
                        ident: "tmp",
                    },
                    ty: ThirTy {
                        span: 7:9..7:12,
                        res: Int,
                    },
                },
            },
        },
    },
    start_fn_id: FnId(
        7,
    ),
    fndecls: {
        FnId(
            1,
        ): ThirFnDecl {
            id: FnId(
                1,
            ),
            scope_id: ScopeId(
                0,
            ),
            span: 1:1..1:1,
            name: Ident {
                span: 1:1..1:1,
                ident: "ftoi",
            },
            params: [
                ThirParam {
                    span: 1:1..1:1,
                    res: None,
                    ty: ThirTy {
                        span: 1:1..1:1,
                        res: Float,
                    },
                },
            ],
            ret_var: VarId(
                3,
            ),
            ret_ty: ThirTy {
                span: 1:1..1:1,
                res: Int,
            },
        },
        FnId(
            4,
        ): ThirFnDecl {
            id: FnId(
                4,
            ),
            scope_id: ScopeId(
                0,
            ),
            span: 1:1..1:1,
            name: Ident {
                span: 1:1..1:1,
                ident: "itof",
            },
            params: [
                ThirParam {
                    span: 1:1..1:1,
                    res: None,
                    ty: ThirTy {
                        span: 1:1..1:1,
                        res: Int,
                    },
                },
            ],
            ret_var: VarId(
                6,
            ),
            ret_ty: ThirTy {
                span: 1:1..1:1,
                res: Float,
            },
        },
        FnId(
            7,
        ): ThirFnDecl {
            id: FnId(
                7,
            ),
            scope_id: ScopeId(
                0,
            ),
            span: 1:1..12:4,
            name: Ident {
                span: 1:1..1:1,
                ident: "__start",
            },
            params: [],
            ret_var: VarId(
                9,
            ),
            ret_ty: ThirTy {
                span: 1:1..1:1,
                res: Void,
            },
        },
    },
    fnbodies: {
        FnId(
            1,
        ): ThirFnBody {
            id: FnId(
                1,
            ),
            inner_scope_id: ScopeId(
                2,
            ),
            kind: Builtin(
                _,
            ),
        },
        FnId(
            4,
        ): ThirFnBody {
            id: FnId(
                4,
            ),
            inner_scope_id: ScopeId(
                5,
            ),
            kind: Builtin(
                _,
            ),
        },
        FnId(
            7,
        ): ThirFnBody {
            id: FnId(
                7,
            ),
            inner_scope_id: ScopeId(
                8,
            ),
            kind: Stmt(
                ThirBeginStmt {
                    span: 1:1..12:4,
                    stmts: [
                        ThirStmt {
                            span: 2:5..2:13,
                            kind: Assg(
                                ThirAssgStmt {
                                    span: 2:5..2:13,
                                    var: ThirVarRef {
                                        span: 2:5..2:8,
                                        res: VarId(
                                            10,
                                        ),
                                    },
                                    expr: ThirArithExpr {
                                        span: 2:12..2:13,
                                        ty: ThirTy {
                                            span: 2:12..2:13,
                                            res: Int,
                                        },
                                        kind: Primary(
                                            ThirPrimaryExpr {
                                                span: 2:12..2:13,
                                                ty: ThirTy {
                                                    span: 2:12..2:13,
                                                    res: Int,
                                                },
                                                kind: Const(
                                                    ThirConst {
                                                        span: 2:12..2:13,
                                                        ty: ThirTy {
                                                            span: 2:12..2:13,
                                                            res: Int,
                                                        },
                                                        value: Int(
                                                            0,
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                        ThirStmt {
                            span: 3:5..3:13,
                            kind: Assg(
                                ThirAssgStmt {
                                    span: 3:5..3:13,
                                    var: ThirVarRef {
                                        span: 3:5..3:8,
                                        res: VarId(
                                            11,
                                        ),
                                    },
                                    expr: ThirArithExpr {
                                        span: 3:12..3:13,
                                        ty: ThirTy {
                                            span: 3:12..3:13,
                                            res: Int,
                                        },
                                        kind: Primary(
                                            ThirPrimaryExpr {
                                                span: 3:12..3:13,
                                                ty: ThirTy {
                                                    span: 3:12..3:13,
                                                    res: Int,
                                                },
                                                kind: Const(
                                                    ThirConst {
                                                        span: 3:12..3:13,
                                                        ty: ThirTy {
                                                            span: 3:12..3:13,
                                                            res: Int,
                                                        },
                                                        value: Int(
                                                            1,
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                        ThirStmt {
                            span: 4:5..4:11,
                            kind: Assg(
                                ThirAssgStmt {
                                    span: 4:5..4:11,
                                    var: ThirVarRef {
                                        span: 4:5..4:6,
                                        res: VarId(
                                            12,
                                        ),
                                    },
                                    expr: ThirArithExpr {
                                        span: 4:10..4:11,
                                        ty: ThirTy {
                                            span: 4:10..4:11,
                                            res: Int,
                                        },
                                        kind: Primary(
                                            ThirPrimaryExpr {
                                                span: 4:10..4:11,
                                                ty: ThirTy {
                                                    span: 4:10..4:11,
                                                    res: Int,
                                                },
                                                kind: Const(
                                                    ThirConst {
                                                        span: 4:10..4:11,
                                                        ty: ThirTy {
                                                            span: 4:10..4:11,
                                                            res: Int,
                                                        },
                                                        value: Int(
                                                            0,
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                        ThirStmt {
                            span: 5:5..11:8,
                            kind: While(
                                ThirWhileStmt {
                                    span: 5:5..11:8,
                                    cond: ThirArithExpr {
                                        span: 5:11..5:17,
                                        ty: ThirTy {
                                            span: 5:11..5:17,
                                            res: Bool,
                                        },
                                        kind: BinOp(
                                            Lt,
                                            ThirArithExpr {
                                                span: 5:11..5:12,
                                                ty: ThirTy {
                                                    span: 5:11..5:12,
                                                    res: Int,
                                                },
                                                kind: Primary(
                                                    ThirPrimaryExpr {
                                                        span: 5:11..5:12,
                                                        ty: ThirTy {
                                                            span: 5:11..5:12,
                                                            res: Int,
                                                        },
                                                        kind: Var(
                                                            ThirVarRef {
                                                                span: 5:11..5:12,
                                                                res: VarId(
                                                                    12,
                                                                ),
                                                            },
                                                        ),
                                                    },
                                                ),
                                            },
                                            ThirArithExpr {
                                                span: 5:15..5:17,
                                                ty: ThirTy {
                                                    span: 5:15..5:17,
                                                    res: Int,
                                                },
                                                kind: Primary(
                                                    ThirPrimaryExpr {
                                                        span: 5:15..5:17,
                                                        ty: ThirTy {
                                                            span: 5:15..5:17,
                                                            res: Int,
                                                        },
                                                        kind: Const(
                                                            ThirConst {
                                                                span: 5:15..5:17,
                                                                ty: ThirTy {
                                                                    span: 5:15..5:17,
                                                                    res: Int,
                                                                },
                                                                value: Int(
                                                                    20,
                                                                ),
                                                            },
                                                        ),
                                                    },
                                                ),
                                            },
                                        ),
                                    },
                                    body: ThirStmt {
                                        span: 5:21..11:8,
                                        kind: Begin(
                                            ThirBeginStmt {
                                                span: 5:21..11:8,
                                                stmts: [
                                                    ThirStmt {
                                                        span: 6:9..6:17,
                                                        kind: Dump(
                                                            ThirDumpStmt {
                                                                span: 6:9..6:17,
                                                                var: ThirVarRef {
                                                                    span: 6:14..6:17,
                                                                    res: VarId(
                                                                        10,
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    ThirStmt {
                                                        span: 7:9..7:25,
                                                        kind: Assg(
                                                            ThirAssgStmt {
                                                                span: 7:9..7:25,
                                                                var: ThirVarRef {
                                                                    span: 7:9..7:12,
                                                                    res: VarId(
                                                                        13,
                                                                    ),
                                                                },
                                                                expr: ThirArithExpr {
                                                                    span: 7:16..7:25,
                                                                    ty: ThirTy {
                                                                        span: 7:16..7:25,
                                                                        res: Int,
                                                                    },
                                                                    kind: BinOp(
                                                                        Add,
                                                                        ThirArithExpr {
                                                                            span: 7:16..7:19,
                                                                            ty: ThirTy {
                                                                                span: 7:16..7:19,
                                                                                res: Int,
                                                                            },
                                                                            kind: Primary(
                                                                                ThirPrimaryExpr {
                                                                                    span: 7:16..7:19,
                                                                                    ty: ThirTy {
                                                                                        span: 7:16..7:19,
                                                                                        res: Int,
                                                                                    },
                                                                                    kind: Var(
                                                                                        ThirVarRef {
                                                                                            span: 7:16..7:19,
                                                                                            res: VarId(
                                                                                                10,
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                        ThirArithExpr {
                                                                            span: 7:22..7:25,
                                                                            ty: ThirTy {
                                                                                span: 7:22..7:25,
                                                                                res: Int,
                                                                            },
                                                                            kind: Primary(
                                                                                ThirPrimaryExpr {
                                                                                    span: 7:22..7:25,
                                                                                    ty: ThirTy {
                                                                                        span: 7:22..7:25,
                                                                                        res: Int,
                                                                                    },
                                                                                    kind: Var(
                                                                                        ThirVarRef {
                                                                                            span: 7:22..7:25,
                                                                                            res: VarId(
                                                                                                11,
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    ThirStmt {
                                                        span: 8:9..8:19,
                                                        kind: Assg(
                                                            ThirAssgStmt {
                                                                span: 8:9..8:19,
                                                                var: ThirVarRef {
                                                                    span: 8:9..8:12,
                                                                    res: VarId(
                                                                        10,
                                                                    ),
                                                                },
                                                                expr: ThirArithExpr {
                                                                    span: 8:16..8:19,
                                                                    ty: ThirTy {
                                                                        span: 8:16..8:19,
                                                                        res: Int,
                                                                    },
                                                                    kind: Primary(
                                                                        ThirPrimaryExpr {
                                                                            span: 8:16..8:19,
                                                                            ty: ThirTy {
                                                                                span: 8:16..8:19,
                                                                                res: Int,
                                                                            },
                                                                            kind: Var(
                                                                                ThirVarRef {
                                                                                    span: 8:16..8:19,
                                                                                    res: VarId(
                                                                                        11,
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    ThirStmt {
                                                        span: 9:9..9:19,
                                                        kind: Assg(
                                                            ThirAssgStmt {
                                                                span: 9:9..9:19,
                                                                var: ThirVarRef {
                                                                    span: 9:9..9:12,
                                                                    res: VarId(
                                                                        11,
                                                                    ),
                                                                },
                                                                expr: ThirArithExpr {
                                                                    span: 9:16..9:19,
                                                                    ty: ThirTy {
                                                                        span: 9:16..9:19,
                                                                        res: Int,
                                                                    },
                                                                    kind: Primary(
                                                                        ThirPrimaryExpr {
                                                                            span: 9:16..9:19,
                                                                            ty: ThirTy {
                                                                                span: 9:16..9:19,
                                                                                res: Int,
                                                                            },
                                                                            kind: Var(
                                                                                ThirVarRef {
                                                                                    span: 9:16..9:19,
                                                                                    res: VarId(
                                                                                        13,
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                    ThirStmt {
                                                        span: 10:9..10:19,
                                                        kind: Assg(
                                                            ThirAssgStmt {
                                                                span: 10:9..10:19,
                                                                var: ThirVarRef {
                                                                    span: 10:9..10:10,
                                                                    res: VarId(
                                                                        12,
                                                                    ),
                                                                },
                                                                expr: ThirArithExpr {
                                                                    span: 10:14..10:19,
                                                                    ty: ThirTy {
                                                                        span: 10:14..10:19,
                                                                        res: Int,
                                                                    },
                                                                    kind: BinOp(
                                                                        Add,
                                                                        ThirArithExpr {
                                                                            span: 10:14..10:15,
                                                                            ty: ThirTy {
                                                                                span: 10:14..10:15,
                                                                                res: Int,
                                                                            },
                                                                            kind: Primary(
                                                                                ThirPrimaryExpr {
                                                                                    span: 10:14..10:15,
                                                                                    ty: ThirTy {
                                                                                        span: 10:14..10:15,
                                                                                        res: Int,
                                                                                    },
                                                                                    kind: Var(
                                                                                        ThirVarRef {
                                                                                            span: 10:14..10:15,
                                                                                            res: VarId(
                                                                                                12,
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                        ThirArithExpr {
                                                                            span: 10:18..10:19,
                                                                            ty: ThirTy {
                                                                                span: 10:18..10:19,
                                                                                res: Int,
                                                                            },
                                                                            kind: Primary(
                                                                                ThirPrimaryExpr {
                                                                                    span: 10:18..10:19,
                                                                                    ty: ThirTy {
                                                                                        span: 10:18..10:19,
                                                                                        res: Int,
                                                                                    },
                                                                                    kind: Const(
                                                                                        ThirConst {
                                                                                            span: 10:18..10:19,
                                                                                            ty: ThirTy {
                                                                                                span: 10:18..10:19,
                                                                                                res: Int,
                                                                                            },
                                                                                            value: Int(
                                                                                                1,
                                                                                            ),
                                                                                        },
                                                                                    ),
                                                                                },
                                                                            ),
                                                                        },
                                                                    ),
                                                                },
                                                            },
                                                        ),
                                                    },
                                                ],
                                            },
                                        ),
                                    },
                                },
                            ),
                        },
                    ],
                },
            ),
        },
    },
}

--- run (THIR) ---
a_0 = 0 (int)
a_0 = 1 (int)
a_0 = 1 (int)
a_0 = 2 (int)
a_0 = 3 (int)
a_0 = 5 (int)
a_0 = 8 (int)
a_0 = 13 (int)
a_0 = 21 (int)
a_0 = 34 (int)
a_0 = 55 (int)
a_0 = 89 (int)
a_0 = 144 (int)
a_0 = 233 (int)
a_0 = 377 (int)
a_0 = 610 (int)
a_0 = 987 (int)
a_0 = 1597 (int)
a_0 = 2584 (int)
a_0 = 4181 (int)
```
