// NEGATIVE (historical FP class): assert! with a real side-effect-free
// predicate. Not vacuous -- it depends on computed data. Expected verdict: [] (clean).
#[test]
fn real_predicate_assert() {
    let items: Vec<u32> = (0..10).collect();
    let sum: u32 = items.iter().sum();
    assert!(sum == 45, "sum was {sum}");
    assert!(sum > 0 && items.len() == 10);
}
