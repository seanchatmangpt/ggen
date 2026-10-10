// POSITIVE: tautology in let-binding shape. Expected verdict: [CHEAT-T02].
#[test]
fn standalone_tautology_binding() {
    let result: Result<u8, String> = Ok(7);
    let always_true = result.is_ok() || result.is_err();
    assert!(always_true);
}
