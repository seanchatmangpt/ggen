// Positive fixture for CHEAT-T02 tautological-result-check: this
// disjunction is always true regardless of what `parse_it` returns.
fn parse_it(s: &str) -> Result<i32, std::num::ParseIntError> {
    s.parse::<i32>()
}

#[test]
fn test_tautological_result_check() {
    let result = parse_it("not a number");
    let always_true = result.is_ok() || result.is_err();
    assert!(always_true);
}
