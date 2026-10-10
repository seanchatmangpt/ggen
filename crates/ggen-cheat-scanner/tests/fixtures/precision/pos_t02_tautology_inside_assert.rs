// POSITIVE: dominant real-world tautology shape, written inside assert!.
// Interior macro tokens must be re-parsed. Expected verdict: [CHEAT-T02].
#[test]
fn tautology_inside_assert_macro() {
    let opt: Option<u32> = Some(3);
    assert!(opt.is_some() || opt.is_none(), "opt was consulted");
}
