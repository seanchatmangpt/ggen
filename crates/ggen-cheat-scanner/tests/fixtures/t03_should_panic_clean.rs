//! Clean fixture: a #[should_panic] test with no assert macro is still
//! failure-capable (the harness fails it when no panic occurs).
pub fn divide(a: u32, b: u32) -> u32 {
    a / b
}

#[test]
#[should_panic(expected = "divide by zero")]
fn divide_by_zero_panics() {
    let _ = divide(1, 0);
}
