// Sample Rust code for metrics testing
fn main() {
    println!("Hello, world!");
}

/// A simple function for testing
fn add(a: i32, b: i32) -> i32 {
    a + b
}

#[test]
fn test_add() {
    assert_eq!(add(2, 2), 4);
}
