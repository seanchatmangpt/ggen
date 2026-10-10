// NEGATIVE: assert!(true) present but alongside a real assertion, so the
// test does NOT reduce to assert!(true). Expected verdict: [] (clean).
#[test]
fn vacuous_assert_is_not_the_only_check() {
    let got = 2 + 2;
    assert!(true); // sanity probe
    assert_eq!(got, 4);
}
