// NEGATIVE: `assert!(result.is_ok())` alone is a weak check but NOT the
// T02 tautology (no `|| result.is_err()` twin on the same scrutinee).
// Expected verdict: [] (clean).
#[test]
fn single_branch_check_is_not_tautological() {
    let result: Result<u8, String> = Ok(1);
    assert!(result.is_ok());
    assert!(result.unwrap() == 1);
}
