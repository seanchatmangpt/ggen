// NEGATIVE: assertion helper macros (assert_matches!-style, prop_assert!
// prefix family) are failure-capable and never reduce the test to
// assert!(true). Expected verdict: [] (clean).
macro_rules! assert_matches {
    ($cond:expr) => {
        assert!($cond)
    };
}

#[test]
fn helper_macro_counts_as_failure_capable() {
    let got = 40 + 2;
    assert_matches!(got == 42);
    assert_matches!(got > 0);
}
