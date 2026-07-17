// Clean fixture for CHEAT-T01: a real assertion on computed state, not a
// vacuous assert!(true).
#[test]
fn test_real_check() {
    let sum = 2 + 2;
    assert_eq!(sum, 4);
}
