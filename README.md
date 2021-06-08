# toy-pascal-like

An interpreter for toy language, somewhat similar to PASCAL.

Currently the syntax follows the following BNF:

```bnf
<stmt> ::= <if-stmt> | <while-stmt> | <begin-stmt> | <assg-stmt> | <dump-stmt>
<if-stmt> ::= if <bool-expr> then <stmt> else <stmt>
<while-stmt> ::= while <bool-expr> do <stmt>
<begin-stmt> ::= begin <stmt-list> end
<stmt-list> ::= <stmt> | <stmt>; <stmt-list>
<assg-stmt> ::= <var> := <arith-expr>
<dump-stmt> ::= dump <var>
<bool-expr> ::= <arith-expr><compare-op><arith-expr>
<compare-op> ::= < | > | <= | >= | == | !=
<arith-expr> ::= <var> | <const> | (<arith-expr><arith-op><arith-expr>)
<arith-op> ::= + | - | * | /
<const> ::= <digit> | <digit><const>
<digit> ::= 0 | 1 | 2 | ... | 9
<var> ::= <letter> | <var><letter> | <var><digit>
<letter> ::= a | b | ... | z
```

# Usage

The interpreter is written in [Rust](https://www.rust-lang.org/ja), so you need to prepare build environment first. You can easily install it using the installer distributed at <https://rustup.rs/>.

After installing, you can run the interpreter by running `cargo run` in a console. `cargo` automatically builds the interpreter before running the executable, so you don't have to build it manually.

After compilation, the program waits for your keyboard input. Type entire source code and send EOF (press `Ctrl-Z` on Windows, or usually `Ctrl-D` on UNIX-like environments). The interpreter will execute your code. Note that REPL is **not** supported.

For now, source code can only be loaded from stdin. If you have the source code as a text file, you can redirect it to the stdin:

```
$ cargo run < source.pas
```

For example, you can run `fib.pas` located at the root directly in this way:

```
$ cargo run < fib.pas
```

# Example

The example output for `fib.pas`:

```text
--- tokens ---
Begin
Ident("a_0")
AssgEqual
Number(0)
Semicolon
Ident("a_1")
AssgEqual
Number(1)
Semicolon
Ident("i")
AssgEqual
Number(0)
Semicolon
While
Ident("i")
Lt
Number(20)
Do
Begin
Dump
Ident("a_0")
Semicolon
Ident("tmp")
AssgEqual
OpenPar
Ident("a_0")
Add
Ident("a_1")
ClosePar
Semicolon
Ident("a_0")
AssgEqual
Ident("a_1")
Semicolon
Ident("a_1")
AssgEqual
Ident("tmp")
Semicolon
Ident("i")
AssgEqual
OpenPar
Ident("i")
Add
Number(1)
ClosePar
End
End

--- ast ---
Stmt
    Begin
        Stmt
            Assg
              var: Var(a_0)
              expr:
                ArithExpr
                  Const(0)
        Stmt
            Assg
              var: Var(a_1)
              expr:
                ArithExpr
                  Const(1)
        Stmt
            Assg
              var: Var(i)
              expr:
                ArithExpr
                  Const(0)
        Stmt
            While
              cond:
                BoolExpr
                  lhs:
                    ArithExpr
                      Var(i)
                  op: CompareOp(<)
                  rhs:
                    ArithExpr
                      Const(20)
              body:
                Stmt
                    Begin
                        Stmt
                            Dump
                              Var(a_0)
                        Stmt
                            Assg
                              var: Var(tmp)
                              expr:
                                ArithExpr
                                  lhs:
                                    ArithExpr
                                      Var(a_0)
                                  op: ArithOp(+)
                                  rhs:
                                    ArithExpr
                                      Var(a_1)
                        Stmt
                            Assg
                              var: Var(a_0)
                              expr:
                                ArithExpr
                                  Var(a_1)
                        Stmt
                            Assg
                              var: Var(a_1)
                              expr:
                                ArithExpr
                                  Var(tmp)
                        Stmt
                            Assg
                              var: Var(i)
                              expr:
                                ArithExpr
                                  lhs:
                                    ArithExpr
                                      Var(i)
                                  op: ArithOp(+)
                                  rhs:
                                    ArithExpr
                                      Const(1)


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

--- variables ---
a_0 = 6765
a_1 = 10946
tmp = 10946
i = 20
```
