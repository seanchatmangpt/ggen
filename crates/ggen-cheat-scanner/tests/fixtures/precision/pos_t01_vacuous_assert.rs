// POSITIVE: whole test reduces to assert!(true). Expected verdict: [CHEAT-T01].
#[test]
fn vacuous_reduces_to_true() {
    let _x = 2 + 2;
    assert!(true);
}
