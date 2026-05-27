//! Verification tests for Genesis core primitives.
//! Following AGENTS.md Chicago TDD and anti-cheating mandates.

use ggen_core::genesis::{
    Admission8, Construct8, Graph8, Mask8, Node8, Object8, Pair2, Predicate8, Receipt,
    ReceiptHint8, RefusalCode, RelationPage, Replay, Provenance8, HASH_SIZE,
};

#[test]
fn test_pair2_and_relation_page_lifecycle() {
    let s = Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]);
    let o = Node8::from_bytes([8, 7, 6, 5, 4, 3, 2, 1]);
    let pair = Pair2::new(s, o);

    assert_eq!(pair.subject.as_bytes(), &[1, 2, 3, 4, 5, 6, 7, 8]);
    assert_eq!(pair.object.as_bytes(), &[8, 7, 6, 5, 4, 3, 2, 1]);

    let mut page = RelationPage::new();
    assert_eq!(page.length, 0);

    // Initial insert
    assert!(page.insert(pair));
    assert_eq!(page.length, 1);
    assert!(page.contains(&pair));

    // Duplicate insert should be blocked
    assert!(!page.insert(pair));
    assert_eq!(page.length, 1);

    // Insert until full
    for idx in 2..=32 {
        let val = idx as u8;
        let item = Pair2::new(
            Node8::from_bytes([val, 0, 0, 0, 0, 0, 0, 0]),
            Node8::from_bytes([0, val, 0, 0, 0, 0, 0, 0]),
        );
        assert!(page.insert(item));
    }
    assert_eq!(page.length, 32);

    // Any more inserts should fail (page full)
    let overflow = Pair2::new(
        Node8::from_bytes([99, 99, 99, 99, 99, 99, 99, 99]),
        Node8::from_bytes([99, 99, 99, 99, 99, 99, 99, 99]),
    );
    assert!(!page.insert(overflow));

    // Removal
    assert!(page.remove(&pair));
    assert_eq!(page.length, 31);
    assert!(!page.contains(&pair));
}

#[test]
fn test_construct8_and_receipt_cryptography() {
    use rand::RngCore;

    // Set up some realistic data
    let s = Node8::from_bytes([10, 20, 30, 40, 50, 60, 70, 80]);
    let p = Predicate8::from_bytes([1, 1, 1, 1, 1, 1, 1, 1]);
    let o = Object8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]);
    let g = Graph8::from_bytes([2, 2, 2, 2, 2, 2, 2, 2]);
    let m = Mask8::from_bytes([0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]);
    let prov = Provenance8::from_bytes([3, 3, 3, 3, 3, 3, 3, 3]);
    let adm = Admission8::from_bytes([4, 4, 4, 4, 4, 4, 4, 4]);
    let hint = ReceiptHint8::from_bytes([5, 5, 5, 5, 5, 5, 5, 5]);

    let construct = Construct8::new(s, p, o, g, m, prov, adm, hint);
    let serialized_bytes = construct.to_bytes();
    assert_eq!(serialized_bytes.len(), 64);
    assert_eq!(&serialized_bytes[0..8], s.as_bytes());

    // Generate cryptographic keys
    let mut csprng = rand::rngs::OsRng;
    let signing_key = ed25519_dalek::SigningKey::generate(&mut csprng);

    let mut inputs = [0u8; HASH_SIZE];
    csprng.fill_bytes(&mut inputs);
    let mut outputs = [0u8; HASH_SIZE];
    csprng.fill_bytes(&mut outputs);

    let operation_id = [7u8; 16];

    // Create a chained receipt
    let parent_hash = [1u8; HASH_SIZE];
    let receipt = Receipt::new(
        operation_id,
        inputs,
        outputs,
        Some(parent_hash),
        1716912345,
    );

    // Verify unsigned fails
    assert_eq!(receipt.verify(), Err(RefusalCode::BoundaryEvidenceMissing));

    // Sign it
    let signed_receipt = receipt.sign(&signing_key);

    // Verify signed receipt succeeds
    assert!(signed_receipt.verify().is_ok());

    // Modify a field to ensure validation fails (integrity/tamper check)
    let mut tampered_receipt = signed_receipt;
    tampered_receipt.outputs_hash[0] ^= 1; // Flip a single bit
    assert_eq!(tampered_receipt.verify(), Err(RefusalCode::InvalidSignature));
}

#[test]
fn test_replay_engine_determinism_and_refusal() {
    let mut state = RelationPage::new();

    let step1 = Construct8::new(
        Node8::from_bytes([1, 1, 1, 1, 1, 1, 1, 1]),
        Predicate8::from_bytes([2, 2, 2, 2, 2, 2, 2, 2]),
        Node8::from_bytes([3, 3, 3, 3, 3, 3, 3, 3]), // Object matches subject ID space
        Graph8::from_bytes([4, 4, 4, 4, 4, 4, 4, 4]),
        Mask8::from_bytes([5, 5, 5, 5, 5, 5, 5, 5]),
        Provenance8::from_bytes([6, 6, 6, 6, 6, 6, 6, 6]),
        Admission8::from_bytes([7, 7, 7, 7, 7, 7, 7, 7]),
        ReceiptHint8::from_bytes([8, 8, 8, 8, 8, 8, 8, 8]),
    );

    let step2 = Construct8::new(
        Node8::from_bytes([10, 10, 10, 10, 10, 10, 10, 10]),
        Predicate8::from_bytes([2, 2, 2, 2, 2, 2, 2, 2]),
        Node8::from_bytes([20, 20, 20, 20, 20, 20, 20, 20]),
        Graph8::from_bytes([4, 4, 4, 4, 4, 4, 4, 4]),
        Mask8::from_bytes([5, 5, 5, 5, 5, 5, 5, 5]),
        Provenance8::from_bytes([6, 6, 6, 6, 6, 6, 6, 6]),
        Admission8::from_bytes([7, 7, 7, 7, 7, 7, 7, 7]),
        ReceiptHint8::from_bytes([8, 8, 8, 8, 8, 8, 8, 8]),
    );

    let mut replay = Replay::new();
    assert!(replay.push(step1));
    assert!(replay.push(step2));

    // Run successful replay
    let new_state = replay.run(state.clone(), 1716912000).unwrap();
    assert_eq!(new_state.length, 2);

    let pair1 = Pair2::new(step1.subject, step1.object);
    let pair2 = Pair2::new(step2.subject, step2.object);
    assert!(new_state.contains(&pair1));
    assert!(new_state.contains(&pair2));

    // Now test a refusal state: Construct8 with empty capability mask (all zeros)
    let invalid_step = Construct8::new(
        Node8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
        Predicate8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
        Node8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
        Graph8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
        Mask8::from_bytes([0, 0, 0, 0, 0, 0, 0, 0]), // Constraint violation mask
        Provenance8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
        Admission8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
        ReceiptHint8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
    );

    let mut failing_replay = Replay::new();
    assert!(failing_replay.push(invalid_step));

    // Replay run must refuse this, yielding Err(Refusal)
    let result = failing_replay.run(state, 1716912999);
    assert!(result.is_err());
    let refusal = result.err().unwrap();
    assert_eq!(refusal.code, RefusalCode::ConstraintViolation);
    assert_eq!(refusal.timestamp, 1716912999);
    // Boundary evidence inside refusal must exactly contain the failing step's bytes
    assert_eq!(refusal.evidence, invalid_step.to_bytes());
}
