use prolog8::types::ProofMode;
use prolog8::{atom, query, rule};

#[test]
fn test_atom_macro() {
    let a = atom!(1, 10, 20);
    assert_eq!(a.pred_id, 1);
    assert_eq!(a.arity, 2);
    assert_eq!(a.args[0], 10);
    assert_eq!(a.args[1], 20);
}

#[test]
fn test_rule_macro() {
    let head = atom!(1, 10, 20);
    let b1 = atom!(2, 10);
    let b2 = atom!(3, 20);

    let r = rule!(100, head, b1, b2);

    assert_eq!(r.rule_id, 100);
    assert_eq!(r.head.pred_id, 1);
    assert_eq!(r.body_len, 2);
    assert_eq!(r.body[0].pred_id, 2);
    assert_eq!(r.body[1].pred_id, 3);
}

#[test]
fn test_query_macro() {
    let a = atom!(5, 42);
    let q1 = query!(a);
    assert_eq!(q1.proof_mode, ProofMode::Positive);
    assert_eq!(q1.atom.pred_id, 5);

    let q2 = query!(a, ProofMode::Full);
    assert_eq!(q2.proof_mode, ProofMode::Full);
}
