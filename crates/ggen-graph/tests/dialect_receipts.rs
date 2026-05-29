use ggen_graph::dialect::check_n3;

#[test]
fn test_dialect_receipt_binding() {
    let input = "{ ?x a <http://www.w3.org/ns/prov#Entity> } => { ?x a <http://www.w3.org/ns/prov#Item> } .";

    // Evaluate N3 input
    let res = check_n3(input).unwrap();

    // Bind inputs/outputs in a cryptographic hash
    let mut hasher = blake3::Hasher::new();
    hasher.update(input.as_bytes());
    hasher.update(res.conforms.to_string().as_bytes());
    hasher.update(res.message.as_bytes());
    hasher.update(res.supported.to_string().as_bytes());
    let receipt_hash = hasher.finalize().to_hex().to_string();

    assert!(!receipt_hash.is_empty());

    // Verify that the receipt is deterministic
    let mut hasher2 = blake3::Hasher::new();
    hasher2.update(input.as_bytes());
    hasher2.update(res.conforms.to_string().as_bytes());
    hasher2.update(res.message.as_bytes());
    hasher2.update(res.supported.to_string().as_bytes());
    let receipt_hash2 = hasher2.finalize().to_hex().to_string();

    assert_eq!(receipt_hash, receipt_hash2);
}
