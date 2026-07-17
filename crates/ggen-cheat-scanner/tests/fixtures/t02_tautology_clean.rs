// Clean fixture for CHEAT-T02: asserts the actual, meaningful branch, not a
// tautological disjunction over both outcomes.
fn parse_it(s: &str) -> Result<i32, std::num::ParseIntError> {
    s.parse::<i32>()
}

#[test]
fn test_parse_failure_is_reported() {
    let result = parse_it("not a number");
    assert!(result.is_err());
}
