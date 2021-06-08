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
