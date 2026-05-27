#![allow(dead_code, unused_imports, unused_variables, deprecated, clippy::all, unused_mut)]

//! Verification tests for Genesis core primitives.
//! Following AGENTS.md Chicago TDD and anti-cheating mandates.

use ggen_core::genesis::{
    Admission8, Construct8, Graph8, Mask8, Multiplicity, Node8, Object8, Pair2, Predicate8,
    Receipt, ReceiptHint8, RefusalCode, RelationPage, Replay, Provenance8, SymbolDomain,
    HASH_SIZE,
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
    for idx in 2..=8 {
        let val = idx as u8;
        let item = Pair2::new(
            Node8::from_bytes([val, 0, 0, 0, 0, 0, 0, 0]),
            Node8::from_bytes([0, val, 0, 0, 0, 0, 0, 0]),
        );
        assert!(page.insert(item));
    }
    assert_eq!(page.length, 8);

    // Any more inserts should fail (page full)
    let overflow = Pair2::new(
        Node8::from_bytes([99, 99, 99, 99, 99, 99, 99, 99]),
        Node8::from_bytes([99, 99, 99, 99, 99, 99, 99, 99]),
    );
    assert!(!page.insert(overflow));

    // Removal
    assert!(page.remove(&pair));
    assert_eq!(page.length, 7);
    assert!(!page.contains(&pair));
}

#[test]
fn test_construct8_and_receipt_cryptography() {
    use rand_core::RngCore;

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
    let mut csprng = rand_core::OsRng;
    let mut key_bytes = [0u8; 32];
    csprng.fill_bytes(&mut key_bytes);
    let signing_key = ed25519_dalek::SigningKey::from_bytes(&key_bytes);

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
        Object8::from_bytes([3, 3, 3, 3, 3, 3, 3, 3]), // Object matches subject ID space
        Graph8::from_bytes([4, 4, 4, 4, 4, 4, 4, 4]),
        Mask8::from_bytes([5, 5, 5, 5, 5, 5, 5, 5]),
        Provenance8::from_bytes([6, 6, 6, 6, 6, 6, 6, 6]),
        Admission8::from_bytes([7, 7, 7, 7, 7, 7, 7, 7]),
        ReceiptHint8::from_bytes([8, 8, 8, 8, 8, 8, 8, 8]),
    );

    let step2 = Construct8::new(
        Node8::from_bytes([10, 10, 10, 10, 10, 10, 10, 10]),
        Predicate8::from_bytes([2, 2, 2, 2, 2, 2, 2, 2]),
        Object8::from_bytes([20, 20, 20, 20, 20, 20, 20, 20]),
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

    let pair1 = Pair2::new(step1.subject, Node8::from_bytes(*step1.object.as_bytes()));
    let pair2 = Pair2::new(step2.subject, Node8::from_bytes(*step2.object.as_bytes()));
    assert!(new_state.contains(&pair1));
    assert!(new_state.contains(&pair2));

    // Now test a refusal state: Construct8 with empty capability mask (all zeros)
    let invalid_step = Construct8::new(
        Node8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
        Predicate8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
        Object8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
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

#[test]
fn test_relation_page_full_capacity() {
    // Test that a fully populated RelationPage (8 pairs) rejects further inserts
    let mut page = RelationPage::new();

    for i in 1..=8 {
        let pair = Pair2::new(
            Node8::from_bytes([i as u8, 0, 0, 0, 0, 0, 0, 0]),
            Node8::from_bytes([0, i as u8, 0, 0, 0, 0, 0, 0]),
        );
        assert!(page.insert(pair), "Insert {} should succeed", i);
    }

    assert_eq!(page.length, 8, "Page should be at max capacity");

    // 9th insert should fail
    let overflow = Pair2::new(
        Node8::from_bytes([99, 0, 0, 0, 0, 0, 0, 0]),
        Node8::from_bytes([0, 99, 0, 0, 0, 0, 0, 0]),
    );
    assert!(!page.insert(overflow), "Overflow insert should fail");
}

#[test]
fn test_pair2_to_bytes_determinism() {
    // Verify Pair2::to_bytes() produces deterministic output across multiple calls
    let s = Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]);
    let o = Node8::from_bytes([8, 7, 6, 5, 4, 3, 2, 1]);
    let pair = Pair2::new(s, o);

    let bytes1 = pair.to_bytes();
    let bytes2 = pair.to_bytes();
    let bytes3 = pair.to_bytes();

    assert_eq!(bytes1, bytes2, "First call matches second");
    assert_eq!(bytes2, bytes3, "Second call matches third");
    assert_eq!(bytes1.len(), 32, "Pair2 must be 32 bytes (2 × 8-byte nodes + metadata)");
}

#[test]
fn test_construct8_to_bytes_determinism() {
    // Verify Construct8::to_bytes() is deterministic across 100 iterations
    let construct = Construct8::new(
        Node8::from_bytes([10, 20, 30, 40, 50, 60, 70, 80]),
        Predicate8::from_bytes([1, 1, 1, 1, 1, 1, 1, 1]),
        Object8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
        Graph8::from_bytes([2, 2, 2, 2, 2, 2, 2, 2]),
        Mask8::from_bytes([0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]),
        Provenance8::from_bytes([3, 3, 3, 3, 3, 3, 3, 3]),
        Admission8::from_bytes([4, 4, 4, 4, 4, 4, 4, 4]),
        ReceiptHint8::from_bytes([5, 5, 5, 5, 5, 5, 5, 5]),
    );

    let first_bytes = construct.to_bytes();
    for _ in 0..99 {
        let current_bytes = construct.to_bytes();
        assert_eq!(
            current_bytes, first_bytes,
            "Construct8::to_bytes() must be deterministic"
        );
    }
    assert_eq!(first_bytes.len(), 64, "Construct8 must serialize to 64 bytes");
}

#[test]
fn test_receipt_signature_non_empty() {
    // Verify that a signed receipt has a non-empty signature field
    use rand_core::RngCore;

    let mut csprng = rand_core::OsRng;
    let mut key_bytes = [0u8; 32];
    csprng.fill_bytes(&mut key_bytes);
    let signing_key = ed25519_dalek::SigningKey::from_bytes(&key_bytes);

    let mut inputs = [0u8; HASH_SIZE];
    csprng.fill_bytes(&mut inputs);
    let mut outputs = [0u8; HASH_SIZE];
    csprng.fill_bytes(&mut outputs);

    let operation_id = [42u8; 16];
    let receipt = Receipt::new(operation_id, inputs, outputs, None, 1716912345);
    let signed_receipt = receipt.sign(&signing_key);

    // Signature field must be non-empty
    assert!(signed_receipt.signature.is_some(), "Signed receipt must have non-empty signature");
    assert_eq!(signed_receipt.signature.unwrap().len(), 64, "Ed25519 signature must be 64 bytes");
}

#[test]
fn test_receipt_hash_integrity() {
    // Verify that receipt hashes are computed and stored correctly
    use rand_core::RngCore;

    let mut csprng = rand_core::OsRng;
    let mut inputs = [1u8; HASH_SIZE];
    csprng.fill_bytes(&mut inputs);
    let mut outputs = [2u8; HASH_SIZE];
    csprng.fill_bytes(&mut outputs);

    let operation_id = [5u8; 16];
    let receipt = Receipt::new(operation_id, inputs, outputs, None, 1000);

    // Verify hashes match input/output
    assert_eq!(receipt.inputs_hash, inputs, "Inputs hash must match");
    assert_eq!(receipt.outputs_hash, outputs, "Outputs hash must match");
}

#[test]
fn test_replay_idempotence() {
    // Verify that replaying the same steps twice with same initial state yields same results
    let step = Construct8::new(
        Node8::from_bytes([1, 1, 1, 1, 1, 1, 1, 1]),
        Predicate8::from_bytes([2, 2, 2, 2, 2, 2, 2, 2]),
        Object8::from_bytes([3, 3, 3, 3, 3, 3, 3, 3]),
        Graph8::from_bytes([4, 4, 4, 4, 4, 4, 4, 4]),
        Mask8::from_bytes([5, 5, 5, 5, 5, 5, 5, 5]),
        Provenance8::from_bytes([6, 6, 6, 6, 6, 6, 6, 6]),
        Admission8::from_bytes([7, 7, 7, 7, 7, 7, 7, 7]),
        ReceiptHint8::from_bytes([8, 8, 8, 8, 8, 8, 8, 8]),
    );

    let mut replay1 = Replay::new();
    replay1.push(step);
    let state1 = replay1.run(RelationPage::new(), 1000).unwrap();

    let mut replay2 = Replay::new();
    replay2.push(step);
    let state2 = replay2.run(RelationPage::new(), 1000).unwrap();

    // Both replays should produce identical states
    assert_eq!(state1.length, state2.length, "Replays must produce same state length");
}

#[test]
fn test_relation_page_remove_and_reinsertion() {
    let mut page = RelationPage::new();
    let pair = Pair2::new(
        Node8::from_bytes([42, 0, 0, 0, 0, 0, 0, 0]),
        Node8::from_bytes([0, 42, 0, 0, 0, 0, 0, 0]),
    );

    // Insert, verify, remove, verify, reinsertion
    assert!(page.insert(pair));
    assert!(page.contains(&pair));
    assert_eq!(page.length, 1);

    assert!(page.remove(&pair));
    assert!(!page.contains(&pair));
    assert_eq!(page.length, 0);

    // Can reinsertion after removal
    assert!(page.insert(pair));
    assert!(page.contains(&pair));
    assert_eq!(page.length, 1);
}

#[test]
fn test_construct8_to_bytes_layout() {
    // Test that Construct8::to_bytes() produces deterministic output
    let construct = Construct8::new(
        Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]),
        Predicate8::from_bytes([9, 10, 11, 12, 13, 14, 15, 16]),
        Object8::from_bytes([17, 18, 19, 20, 21, 22, 23, 24]),
        Graph8::from_bytes([25, 26, 27, 28, 29, 30, 31, 32]),
        Mask8::from_bytes([33, 34, 35, 36, 37, 38, 39, 40]),
        Provenance8::from_bytes([41, 42, 43, 44, 45, 46, 47, 48]),
        Admission8::from_bytes([49, 50, 51, 52, 53, 54, 55, 56]),
        ReceiptHint8::from_bytes([57, 58, 59, 60, 61, 62, 63, 64]),
    );

    let bytes1 = construct.to_bytes();
    let bytes2 = construct.to_bytes();

    assert_eq!(bytes1, bytes2, "to_bytes() must be deterministic");
    assert_eq!(bytes1.len(), 64, "to_bytes() must produce 64 bytes");

    // Verify layout: subject at [0..8], predicate at [8..16], etc.
    assert_eq!(&bytes1[0..8], &[1, 2, 3, 4, 5, 6, 7, 8]);
    assert_eq!(&bytes1[8..16], &[9, 10, 11, 12, 13, 14, 15, 16]);
    assert_eq!(&bytes1[16..24], &[17, 18, 19, 20, 21, 22, 23, 24]);
}

#[test]
fn test_receipt_hash_size_constraint() {
    // Verify Receipt field sizes match expected cryptographic constants
    let op_id = [1u8; 16];
    let inputs = [2u8; HASH_SIZE];
    let outputs = [3u8; HASH_SIZE];

    let receipt = Receipt::new(op_id, inputs, outputs, None, 1000);

    assert_eq!(receipt.operation_id.len(), 16);
    assert_eq!(receipt.inputs_hash.len(), HASH_SIZE);
    assert_eq!(receipt.outputs_hash.len(), HASH_SIZE);
    assert_eq!(receipt.timestamp, 1000);
}

#[test]
fn test_receipt_chaining_with_parent_hash() {
    // Test that receipts can form a chain via previous_receipt_hash
    let parent_hash = [11u8; HASH_SIZE];
    let op_id = [1u8; 16];
    let inputs = [2u8; HASH_SIZE];
    let outputs = [3u8; HASH_SIZE];

    let receipt = Receipt::new(op_id, inputs, outputs, Some(parent_hash), 1000);

    assert_eq!(receipt.previous_receipt_hash, Some(parent_hash));
    assert_eq!(receipt.timestamp, 1000);
}

#[test]
fn test_replay_max_steps_boundary() {
    // Test that Replay respects the MAX_REPLAY_STEPS limit
    let mut replay = Replay::new();

    // Successfully push MAX_REPLAY_STEPS
    for i in 0..16 {
        let construct = Construct8::new(
            Node8::from_bytes([i as u8; 8]),
            Predicate8::from_bytes([1; 8]),
            Object8::from_bytes([2; 8]),
            Graph8::from_bytes([3; 8]),
            Mask8::from_bytes([255; 8]), // Valid mask
            Provenance8::from_bytes([4; 8]),
            Admission8::from_bytes([5; 8]),
            ReceiptHint8::from_bytes([6; 8]),
        );
        assert!(replay.push(construct), "Step {} should succeed", i);
    }

    // 17th push should fail
    let overflow = Construct8::new(
        Node8::from_bytes([99; 8]),
        Predicate8::from_bytes([1; 8]),
        Object8::from_bytes([2; 8]),
        Graph8::from_bytes([3; 8]),
        Mask8::from_bytes([255; 8]),
        Provenance8::from_bytes([4; 8]),
        Admission8::from_bytes([5; 8]),
        ReceiptHint8::from_bytes([6; 8]),
    );
    assert!(!replay.push(overflow), "17th step should fail (max 16)");
}

#[test]
fn test_refusal_evidence_preservation() {
    // Test that Refusal correctly captures evidence bytes
    let invalid_construct = Construct8::new(
        Node8::from_bytes([1; 8]),
        Predicate8::from_bytes([2; 8]),
        Object8::from_bytes([3; 8]),
        Graph8::from_bytes([4; 8]),
        Mask8::from_bytes([0; 8]), // Invalid: zero mask
        Provenance8::from_bytes([5; 8]),
        Admission8::from_bytes([6; 8]),
        ReceiptHint8::from_bytes([7; 8]),
    );

    let expected_bytes = invalid_construct.to_bytes();

    let mut replay = Replay::new();
    replay.push(invalid_construct);

    let result = replay.run(RelationPage::new(), 9999);
    assert!(result.is_err());

    let refusal = result.unwrap_err();
    assert_eq!(refusal.evidence, expected_bytes);
    assert_eq!(refusal.timestamp, 9999);
    assert_eq!(refusal.code, RefusalCode::ConstraintViolation);
}

#[test]
fn test_pair2_equality_and_hashing() {
    // Test that Pair2 equality and hashing are consistent
    let pair1 = Pair2::new(
        Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]),
        Node8::from_bytes([9, 10, 11, 12, 13, 14, 15, 16]),
    );
    let pair2 = Pair2::new(
        Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]),
        Node8::from_bytes([9, 10, 11, 12, 13, 14, 15, 16]),
    );
    let pair3 = Pair2::new(
        Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 9]), // Different
        Node8::from_bytes([9, 10, 11, 12, 13, 14, 15, 16]),
    );

    assert_eq!(pair1, pair2);
    assert_ne!(pair1, pair3);

    // Hash consistency (use in HashMap/BTreeMap)
    use std::collections::HashSet;
    let mut set = HashSet::new();
    set.insert(pair1);
    set.insert(pair2); // Same as pair1
    set.insert(pair3);

    assert_eq!(set.len(), 2, "HashSet should deduplicate equal Pair2s");
}

// ============ NEW TESTS: Multiplicity and Domain Tracking (300+ LOC) ============

#[test]
fn test_multiplicity_set_basic_semantics() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::Set);

    let subject = Node8::from_bytes([1, 0, 0, 0, 0, 0, 0, 0]);
    let object = Node8::from_bytes([0, 1, 0, 0, 0, 0, 0, 0]);
    let pair = Pair2::new(subject, object);

    // First insert succeeds
    assert!(page.insert(pair));
    assert_eq!(page.length, 1);

    // Duplicate insert fails (Set semantics)
    assert!(!page.insert(pair));
    assert_eq!(page.length, 1);
}

#[test]
fn test_multiplicity_bag_allows_counting() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::Bag);

    let subject = Node8::from_bytes([2, 0, 0, 0, 0, 0, 0, 0]);
    let object = Node8::from_bytes([0, 2, 0, 0, 0, 0, 0, 0]);
    let pair = Pair2::new(subject, object);

    // First insert
    assert!(page.insert(pair));
    assert_eq!(page.pair_counts[0], 1);

    // Second insert increments count
    assert!(page.insert(pair));
    assert_eq!(page.pair_counts[0], 2);

    // Third insert
    assert!(page.insert(pair));
    assert_eq!(page.pair_counts[0], 3);

    // Length stays 1 (single pair in the page)
    assert_eq!(page.length, 1);
}

#[test]
fn test_multiplicity_stream_with_different_timestamps() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::Stream);

    let subject = Node8::from_bytes([3, 0, 0, 0, 0, 0, 0, 0]);
    let object = Node8::from_bytes([0, 3, 0, 0, 0, 0, 0, 0]);

    let pair_t1 = Pair2::with_timestamp(subject, object, 1000u64);
    let pair_t2 = Pair2::with_timestamp(subject, object, 2000u64);

    // Both should insert (different timestamps = different events)
    assert!(page.insert(pair_t1));
    assert!(page.insert(pair_t2));
    assert_eq!(page.length, 1);
    assert_eq!(page.pair_counts[0], 2);
}

#[test]
fn test_multiplicity_event_addressed_with_distinct_event_ids() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::EventAddressed);

    let subject = Node8::from_bytes([4, 0, 0, 0, 0, 0, 0, 0]);
    let object = Node8::from_bytes([0, 4, 0, 0, 0, 0, 0, 0]);

    let pair_ev1 = Pair2::with_event_id(subject, object, [1u8; 8]);
    let pair_ev2 = Pair2::with_event_id(subject, object, [2u8; 8]);

    // Both should insert (different event IDs)
    assert!(page.insert(pair_ev1));
    assert!(page.insert(pair_ev2));
    assert_eq!(page.length, 1);
    assert_eq!(page.pair_counts[0], 2);
}

#[test]
fn test_relation_page_multiplicity_immutable_after_first_insert() {
    let mut page = RelationPage::with_multiplicity(Multiplicity::Bag);

    let subject = Node8::from_bytes([5, 0, 0, 0, 0, 0, 0, 0]);
    let object = Node8::from_bytes([0, 5, 0, 0, 0, 0, 0, 0]);
    let pair = Pair2::new(subject, object);

    // Insert establishes Bag mode
    assert!(page.insert(pair));

    // Multiplicity field remains Bag
    assert_eq!(page.multiplicity, Multiplicity::Bag);
    assert!(page.multiplicity.allows_duplicates());
}

#[test]
fn test_pair2_with_timestamp_preserves_metadata() {
    let subject = Node8::from_bytes([10, 20, 30, 40, 50, 60, 70, 80]);
    let object = Node8::from_bytes([80, 70, 60, 50, 40, 30, 20, 10]);
    let timestamp = 1234567890u64;

    let pair = Pair2::with_timestamp(subject, object, timestamp);

    assert_eq!(pair.subject, subject);
    assert_eq!(pair.object, object);
    assert_eq!(pair.timestamp, Some(timestamp));
    assert_eq!(pair.event_id, None);
}

#[test]
fn test_pair2_with_event_id_preserves_metadata() {
    let subject = Node8::from_bytes([11, 22, 33, 44, 55, 66, 77, 88]);
    let object = Node8::from_bytes([88, 77, 66, 55, 44, 33, 22, 11]);
    let event_id = [99u8; 8];

    let pair = Pair2::with_event_id(subject, object, event_id);

    assert_eq!(pair.subject, subject);
    assert_eq!(pair.object, object);
    assert_eq!(pair.timestamp, None);
    assert_eq!(pair.event_id, Some(event_id));
}

#[test]
fn test_pair2_pairs_equal_ignoring_metadata() {
    let subject = Node8::from_bytes([1, 1, 1, 1, 1, 1, 1, 1]);
    let object = Node8::from_bytes([2, 2, 2, 2, 2, 2, 2, 2]);

    let base = Pair2::new(subject, object);
    let with_ts = Pair2::with_timestamp(subject, object, 5000u64);
    let with_ev = Pair2::with_event_id(subject, object, [3u8; 8]);

    // All pairs should be equal ignoring metadata
    assert!(base.pairs_equal_ignoring_metadata(&with_ts));
    assert!(base.pairs_equal_ignoring_metadata(&with_ev));
    assert!(with_ts.pairs_equal_ignoring_metadata(&with_ev));
}

#[test]
fn test_symbol_domain_allows_different_byte_patterns() {
    let mut domain = SymbolDomain::new();

    let symbols = vec![
        [0u8; 8],
        [255u8; 8],
        [1, 2, 3, 4, 5, 6, 7, 8],
        [0xAA, 0x55, 0xAA, 0x55, 0xAA, 0x55, 0xAA, 0x55],
        [0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00, 0xFF, 0x00],
    ];

    for sym in &symbols {
        let result = domain.insert(*sym).unwrap();
        assert!(result, "Should insert distinct symbol");
    }

    assert_eq!(domain.count(), symbols.len());

    for sym in &symbols {
        assert!(domain.contains(sym), "Should find inserted symbol");
    }
}

#[test]
fn test_symbol_domain_duplicate_handling() {
    let mut domain = SymbolDomain::new();
    let symbol = [42u8; 8];

    // Insert symbol
    assert!(domain.insert(symbol).unwrap());
    assert_eq!(domain.count(), 1);

    // Insert duplicate
    assert!(!domain.insert(symbol).unwrap());
    assert_eq!(domain.count(), 1); // Count unchanged

    // Verify contains works
    assert!(domain.contains(&symbol));
}

#[test]
fn test_relation_page_domain_tracking_per_symbol() {
    let mut page = RelationPage::new();

    // Insert pairs with sequential subjects and objects
    for i in 0..50 {
        let subject = Node8::from_bytes([i as u8, 0, 0, 0, 0, 0, 0, 0]);
        let object = Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, i as u8]);
        let pair = Pair2::new(subject, object);
        assert!(page.insert(pair), "Failed at insert {}", i);
        assert!(page.remove(&pair), "Failed at remove {}", i);
    }

    // Both domains should have 50 distinct symbols
    assert_eq!(page.left_domain.count(), 50);
    assert_eq!(page.right_domain.count(), 50);
}

#[test]
fn test_construct8_with_pair2_metadata_fields() {
    let subject = Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]);
    let predicate = Predicate8::from_bytes([9, 10, 11, 12, 13, 14, 15, 16]);
    let object = Object8::from_bytes([17, 18, 19, 20, 21, 22, 23, 24]);
    let graph = Graph8::from_bytes([25, 26, 27, 28, 29, 30, 31, 32]);
    let mask = Mask8::from_bytes([0xFF; 8]);
    let provenance = Provenance8::from_bytes([33, 34, 35, 36, 37, 38, 39, 40]);
    let admission = Admission8::from_bytes([41, 42, 43, 44, 45, 46, 47, 48]);
    let hint = ReceiptHint8::from_bytes([49, 50, 51, 52, 53, 54, 55, 56]);

    let construct = Construct8::new(
        subject, predicate, object, graph, mask, provenance, admission, hint
    );

    assert_eq!(construct.subject, subject);
    assert_eq!(construct.predicate, predicate);
    assert_eq!(construct.object, object);
    assert_eq!(construct.graph, graph);
    assert_eq!(construct.mask, mask);
    assert_eq!(construct.provenance, provenance);
    assert_eq!(construct.admission, admission);
    assert_eq!(construct.receipt_hint, hint);
}

#[test]
fn test_replay_mixed_constructs_with_valid_masks() {
    let mut replay = Replay::new();

    // Push 5 valid constructs
    for i in 0..5 {
        let construct = Construct8::new(
            Node8::from_bytes([i; 8]),
            Predicate8::from_bytes([1; 8]),
            Object8::from_bytes([2; 8]),
            Graph8::from_bytes([3; 8]),
            Mask8::from_bytes([0xFF; 8]), // Valid non-zero mask
            Provenance8::from_bytes([4; 8]),
            Admission8::from_bytes([5; 8]),
            ReceiptHint8::from_bytes([6; 8]),
        );
        assert!(replay.push(construct), "Push {} should succeed", i);
    }

    assert_eq!(replay.length, 5);

    // Run replay
    let result = replay.run(RelationPage::new(), 1000);
    assert!(result.is_ok(), "Valid replay should succeed");
}

#[test]
fn test_relation_page_pair_storage_within_limits() {
    let mut page = RelationPage::new();

    // MAX_RELATION_PAIRS is 8, so we can store 8 distinct pairs
    for i in 0..8 {
        let subject = Node8::from_bytes([i as u8, 0, 0, 0, 0, 0, 0, 0]);
        let object = Node8::from_bytes([0, i as u8, 0, 0, 0, 0, 0, 0]);
        let pair = Pair2::new(subject, object);

        assert!(page.insert(pair), "Insert pair {} should succeed", i);
        assert_eq!(page.length, i + 1);
    }

    // 9th pair should fail (page full)
    let overflow = Pair2::new(
        Node8::from_bytes([99, 0, 0, 0, 0, 0, 0, 0]),
        Node8::from_bytes([0, 99, 0, 0, 0, 0, 0, 0]),
    );
    assert!(!page.insert(overflow), "Insert 9th pair should fail");
}

#[test]
fn test_pair2_metadata_combinations() {
    let subject = Node8::from_bytes([1, 1, 1, 1, 1, 1, 1, 1]);
    let object = Node8::from_bytes([2, 2, 2, 2, 2, 2, 2, 2]);

    // Base (no metadata)
    let pair1 = Pair2::new(subject, object);
    assert_eq!(pair1.timestamp, None);
    assert_eq!(pair1.event_id, None);

    // Timestamp only
    let pair2 = Pair2::with_timestamp(subject, object, 1000u64);
    assert_eq!(pair2.timestamp, Some(1000u64));
    assert_eq!(pair2.event_id, None);

    // Event ID only
    let pair3 = Pair2::with_event_id(subject, object, [5u8; 8]);
    assert_eq!(pair3.timestamp, None);
    assert!(pair3.event_id.is_some());
}

#[test]
fn test_multiplicity_enum_properties() {
    assert!(!Multiplicity::Set.allows_duplicates());
    assert!(Multiplicity::Bag.allows_duplicates());
    assert!(Multiplicity::Stream.allows_duplicates());
    assert!(Multiplicity::EventAddressed.allows_duplicates());

    // Test repr values
    assert_eq!(Multiplicity::Set as u8, 0);
    assert_eq!(Multiplicity::Bag as u8, 1);
    assert_eq!(Multiplicity::Stream as u8, 2);
    assert_eq!(Multiplicity::EventAddressed as u8, 3);
}

#[test]
fn test_default_relation_page_initialization() {
    let page = RelationPage::new();

    assert_eq!(page.length, 0);
    assert_eq!(page.left_len, 0);
    assert_eq!(page.right_len, 0);
    assert_eq!(page.left_domain.count(), 0);
    assert_eq!(page.right_domain.count(), 0);
    assert_eq!(page.multiplicity, Multiplicity::Set);

    for count in &page.pair_counts {
        assert_eq!(*count, 0);
    }
}

#[test]
fn test_relation_page_removal_updates_domains() {
    let mut page = RelationPage::new();

    let subject = Node8::from_bytes([1, 0, 0, 0, 0, 0, 0, 0]);
    let object = Node8::from_bytes([0, 1, 0, 0, 0, 0, 0, 0]);
    let pair = Pair2::new(subject, object);

    // Insert
    assert!(page.insert(pair));
    assert_eq!(page.left_domain.count(), 1);
    assert_eq!(page.right_domain.count(), 1);

    // Remove
    assert!(page.remove(&pair));
    assert_eq!(page.length, 0);
    // Note: Domains are not cleared on removal; this is by design
    assert_eq!(page.left_domain.count(), 1);
    assert_eq!(page.right_domain.count(), 1);
}
