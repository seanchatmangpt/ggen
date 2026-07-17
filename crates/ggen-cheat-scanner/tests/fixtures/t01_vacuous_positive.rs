// Positive fixture for CHEAT-T01 vacuous-assert: this test's only
// meaningful statement is assert!(true) -- it can never fail.
#[test]
fn test_vacuous() {
    assert!(true);
}
