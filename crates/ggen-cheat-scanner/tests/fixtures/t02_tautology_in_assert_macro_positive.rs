// Positive fixture for CHEAT-T02 tautological-result-check written directly
// inside an `assert!(...)` invocation -- the dominant real-world shape
// (`assert!(result.is_ok() || result.is_err())`), as opposed to the
// standalone-`let`-then-assert shape in `t02_tautology_positive.rs`.
// `assert!(...)` parses as an opaque `Expr::Macro`/`Stmt::Macro`, so this
// fixture exists to prove the T02 detector re-parses and walks macro
// tokens rather than only ever seeing the tautology when it is spelled out
// as a real `Expr::Binary` in a `let` statement.
fn parse_it(s: &str) -> Result<i32, std::num::ParseIntError> {
    s.parse::<i32>()
}

#[test]
fn test_tautological_result_check_inside_assert_macro() {
    let result = parse_it("not a number");
    assert!(result.is_ok() || result.is_err());
}
