pub mod span;

pub mod lexer;

pub mod parser;

pub mod ast;

pub mod lowerer;

pub mod hir;
pub mod hir_visit;

pub mod resolver;

pub mod rhir;
pub mod rhir_visit;

pub mod typeck;

pub mod thir;

pub mod builtins;

pub mod thir_interp;

pub mod error;

#[cfg(test)]
mod tests {
    use crate::builtins::populate_builtins;
    use crate::error::{print_error, Error};
    use crate::hir::Value;
    use crate::lexer::tokenize;
    use crate::lowerer::lower_ast;
    use crate::parser::parse;
    use crate::resolver::resolve_hir;
    use crate::thir_interp::run;
    use crate::typeck::check_rhir;
    use itertools::Itertools as _;
    use std::collections::BTreeMap;
    use std::fs;
    use std::path::Path;
    use termcolor::NoColor;

    fn run_source(source: &str) -> Result<BTreeMap<String, Value>, Vec<Error>> {
        let tokens = tokenize(source);
        let ast = parse(&tokens).map_err(|e| vec![e.into()])?;
        let hir = lower_ast(&ast, populate_builtins());
        let rhir = resolve_hir(hir).map_err(|es| es.into_iter().map(Into::into).collect_vec())?;
        let thir = check_rhir(rhir).map_err(|es| es.into_iter().map(Into::into).collect_vec())?;

        Ok(run(&thir))
    }

    fn read_after_state(after_state_file_path: &Path) -> BTreeMap<String, Value> {
        let after_state_file = fs::read_to_string(after_state_file_path).unwrap();
        let mut after_state = BTreeMap::new();
        for line in after_state_file.lines() {
            if line.trim().is_empty() {
                continue;
            }

            let mut line = line.splitn(2, '=');
            let name = line
                .next()
                .expect("failed to read variable name")
                .trim()
                .to_string();
            let value = {
                let raw = line.next().expect("failed to read the value").trim();

                if raw.starts_with("Void") {
                    Value::Void
                } else if raw.starts_with("Int") {
                    Value::Int(
                        raw["Int(".len()..raw.len() - ")".len()]
                            .parse()
                            .expect("failed to parse value as int"),
                    )
                } else if raw.starts_with("Float") {
                    Value::Float(
                        raw["Float(".len()..raw.len() - ")".len()]
                            .parse()
                            .expect("failed to parse value as float"),
                    )
                } else if raw.starts_with("Bool") {
                    Value::Bool(
                        raw["Bool(".len()..raw.len() - ")".len()]
                            .parse()
                            .expect("failed to parse value as float"),
                    )
                } else {
                    panic!("Unknown value: {}", raw)
                }
            };

            after_state.insert(name, value);
        }

        after_state
    }

    fn eq_value(lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            (Value::Void, Value::Void) => true,
            (Value::Int(l), Value::Int(r)) => l == r,
            (Value::Float(l), Value::Float(r)) => (l - r).abs() < 1e-8,
            (Value::Bool(l), Value::Bool(r)) => l == r,
            _ => panic!("inequatable: {:?} vs {:?}", lhs, rhs),
        }
    }

    #[test]
    fn test_compile_pass() {
        for entry in glob::glob("tests/compile-pass/**/*.pas").unwrap() {
            let entry = entry.unwrap();
            let filename = entry.display().to_string();

            println!();
            println!("---- run test for `{}` ----", filename);
            println!();

            let source = fs::read_to_string(&entry).unwrap();
            let state = match run_source(&source) {
                Ok(state) => state,
                Err(errs) => {
                    let mut buf = NoColor::new(Vec::new());
                    for err in &errs {
                        print_error(&mut buf, &filename, &source, err);
                    }
                    panic!("\n\n{}\n", String::from_utf8_lossy(&buf.into_inner()));
                }
            };

            let after_state_file_path = entry.with_extension("after-state");
            if after_state_file_path.exists() {
                let after_state = read_after_state(&after_state_file_path);
                for (name, value) in after_state {
                    assert!(
                        eq_value(&value, &state[&name]),
                        "unexpected value for variable `{}`:\n  expected: {:?}\n  found: {:?}",
                        name,
                        value,
                        state[&name],
                    );
                }
            }
        }
    }
}
