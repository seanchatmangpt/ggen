use crate::term::{Term, VarOrTerm};
use crate::Encoder;

#[test]
fn test_literal_term_roundtrip() {
    let lit_term_str = "\"hello\"@en".to_string();
    let var_or_term = VarOrTerm::new_term(lit_term_str.clone());

    assert!(var_or_term.is_term());
    let term = var_or_term.as_term();
    assert!(matches!(term, Term::Literal(_)));

    let decoded = Encoder::decode(&term.id()).unwrap();
    assert_eq!(decoded, lit_term_str);
}

#[test]
fn test_blank_node_term_encoding() {
    let blank_node_str = "_:b0".to_string();
    let var_or_term = VarOrTerm::new_term(blank_node_str.clone());

    assert!(var_or_term.is_term());
    let term = var_or_term.as_term();
    assert!(matches!(term, Term::BlankNode(_)));

    let decoded = Encoder::decode(&term.id()).unwrap();
    assert_eq!(decoded, blank_node_str);
}
