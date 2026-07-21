//! Clean fixture: a test delegating its assertions to an `assert_*` helper
//! fn is failure-capable — the helper's assert! fires in the test's frame.
fn assert_all_even(xs: &[u32]) {
    for x in xs {
        assert!(x % 2 == 0, "odd value {x}");
    }
}

#[test]
fn evens_stay_even() {
    let xs = vec![2u32, 4, 6];
    assert_all_even(&xs);
}
