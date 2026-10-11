// NEGATIVE (historical FP class): assert!(true) and an assertion-free
// helper exist OUTSIDE any #[test] fn. Only #[test]/#[tokio::test] bodies
// are in scope. Expected verdict: [] (clean).
fn production_helper(x: u8) -> u8 {
    assert!(x <= 100);
    x + 1
}

fn debug_probe(flag: bool) {
    if flag {
        assert!(true); // intentional no-op in non-test code
    }
}

#[test]
fn test_fn_itself_is_real() {
    assert_eq!(production_helper(1), 2);
}
