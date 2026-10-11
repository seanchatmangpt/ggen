// NEGATIVE (historical FP class): assert_eq! on genuinely computed values.
// Real check, expected verdict: [] (clean).
fn double(x: i64) -> i64 {
    x * 2
}

#[test]
fn asserts_on_computed_values() {
    let input = 21;
    let got = double(input);
    assert_eq!(got, 42);
    assert_ne!(got, 0);
}
