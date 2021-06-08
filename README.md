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
<const> ::= 0 | 1 | ... | 9
<var> ::= a | b | ... | z
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
Other("f")
AssgEqual
Other("0")
Semicolon
Other("s")
AssgEqual
Other("1")
Semicolon
Other("n")
AssgEqual
Other("0")
Semicolon
While
Other("n")
Lt
Other("9")
Do
Begin
Dump
Other("f")
Semicolon
Other("t")
AssgEqual
OpenPar
Other("f")
Add
Other("s")
ClosePar
Semicolon
Other("f")
AssgEqual
Other("s")
Semicolon
Other("s")
AssgEqual
Other("t")
Semicolon
Other("n")
AssgEqual
OpenPar
Other("n")
Add
Other("1")
ClosePar
End
End

--- ast ---
Stmt
    Begin
        Stmt
            Assg
              var: Var(f)
              expr:
                ArithExpr
                  Const(0)
        Stmt
            Assg
              var: Var(s)
              expr:
                ArithExpr
                  Const(1)
        Stmt
            Assg
              var: Var(n)
              expr:
                ArithExpr
                  Const(0)
        Stmt
            While
              cond:
                BoolExpr
                  lhs:
                    ArithExpr
                      Var(n)
                  op: CompareOp(<)
                  rhs:
                    ArithExpr
                      Const(9)
              body:
                Stmt
                    Begin
                        Stmt
                            Dump
                              Var(f)
                        Stmt
                            Assg
                              var: Var(t)
                              expr:
                                ArithExpr
                                  lhs:
                                    ArithExpr
                                      Var(f)
                                  op: ArithOp(+)
                                  rhs:
                                    ArithExpr
                                      Var(s)
                        Stmt
                            Assg
                              var: Var(f)
                              expr:
                                ArithExpr
                                  Var(s)
                        Stmt
                            Assg
                              var: Var(s)
                              expr:
                                ArithExpr
                                  Var(t)
                        Stmt
                            Assg
                              var: Var(n)
                              expr:
                                ArithExpr
                                  lhs:
                                    ArithExpr
                                      Var(n)
                                  op: ArithOp(+)
                                  rhs:
                                    ArithExpr
                                      Const(1)


--- run ---
dump: f = 0
dump: f = 1
dump: f = 1
dump: f = 2
dump: f = 3
dump: f = 5
dump: f = 8
dump: f = 13
dump: f = 21

--- variables ---
n = 9
t = 55
s = 55
f = 34
```
