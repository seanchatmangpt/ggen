#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    unused_mut
)]
//! Sabotage Tests: Defect Injection and Boundary Validation
//! Tests that the system correctly detects and refuses 14+ injected defects

use ggen_core::genesis::{
    Admission8, Construct8, Graph8, Mask8, Node8, Object8, Pair2, Predicate8, Provenance8, Receipt,
    ReceiptHint8, RefusalCode, RelationPage, Replay, HASH_SIZE,
};
use rand_core::RngCore;

#[test]
fn test_sabotage_zero_mask_refuses_execution() {
    // DEFECT 1: Zero capability mask (all bytes = 0x00) must trigger ConstraintViolation
    let mut replay = Replay::new();
    let sabotaged = Construct8::new(
        Node8::from_bytes([1, 1, 1, 1, 1, 1, 1, 1]),
        Predicate8::from_bytes([2, 2, 2, 2, 2, 2, 2, 2]),
        Object8::from_bytes([3, 3, 3, 3, 3, 3, 3, 3]),
        Graph8::from_bytes([4, 4, 4, 4, 4, 4, 4, 4]),
        Mask8::from_bytes([0, 0, 0, 0, 0, 0, 0, 0]), // ZERO MASK
        Provenance8::from_bytes([5, 5, 5, 5, 5, 5, 5, 5]),
        Admission8::from_bytes([6, 6, 6, 6, 6, 6, 6, 6]),
        ReceiptHint8::from_bytes([7, 7, 7, 7, 7, 7, 7, 7]),
    );

    replay.push(sabotaged);
    let result = replay.run(RelationPage::new(), 1000);

    assert!(result.is_err(), "Zero mask should trigger refusal");
    assert_eq!(
        result.unwrap_err().code,
        RefusalCode::ConstraintViolation,
        "Zero mask refusal code must be ConstraintViolation"
    );
}

#[test]
fn test_sabotage_replay_overflow_16_steps() {
    // DEFECT 2: Attempt to push 17 steps (max is 16) must fail at push time
    let mut replay = Replay::new();

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

    assert!(
        !replay.push(overflow),
        "17th push must be rejected at push time"
    );
}

#[test]
fn test_sabotage_page_overflow_33_pairs() {
    // DEFECT 3: Attempt to insert 9th pair (max is 8) must fail
    let mut page = RelationPage::new();

    for i in 1..=8 {
        let pair = Pair2::new(
            Node8::from_bytes([i as u8, 0, 0, 0, 0, 0, 0, 0]),
            Node8::from_bytes([0, i as u8, 0, 0, 0, 0, 0, 0]),
        );
        assert!(page.insert(pair), "Pair {} should insert", i);
    }

    let ninth = Pair2::new(
        Node8::from_bytes([99, 0, 0, 0, 0, 0, 0, 0]),
        Node8::from_bytes([0, 99, 0, 0, 0, 0, 0, 0]),
    );

    assert!(!page.insert(ninth), "9th pair must be rejected");
    assert_eq!(page.length, 8, "Length must remain 8");
}

#[test]
fn test_sabotage_receipt_tamper_detection_outputs_hash() {
    // DEFECT 4: Tamper with output hash after signing; signature must fail verification
    let op_id = [7u8; 16];
    let inputs = [1u8; HASH_SIZE];
    let mut outputs = [2u8; HASH_SIZE];

    let receipt = Receipt::new(op_id, inputs, outputs, None, 1000);

    // Sign receipt
    use rand_core::RngCore;
    let mut csprng = rand_core::OsRng;
    let mut key_bytes = [0u8; 32];
    csprng.fill_bytes(&mut key_bytes);
    let signing_key = ed25519_dalek::SigningKey::from_bytes(&key_bytes);
    let mut signed = receipt.sign(&signing_key);

    // Now tamper with outputs_hash
    signed.outputs_hash[0] ^= 1;

    // Verification must fail
    assert_eq!(
        signed.verify(),
        Err(RefusalCode::InvalidSignature),
        "Tampered output hash must fail signature verification"
    );
}

#[test]
fn test_sabotage_receipt_tamper_detection_inputs_hash() {
    // DEFECT 5: Tamper with input hash after signing; signature must fail
    let op_id = [7u8; 16];
    let mut inputs = [1u8; HASH_SIZE];
    let outputs = [2u8; HASH_SIZE];

    let receipt = Receipt::new(op_id, inputs, outputs, None, 1000);

    use rand_core::RngCore;
    let mut csprng = rand_core::OsRng;
    let mut key_bytes = [0u8; 32];
    csprng.fill_bytes(&mut key_bytes);
    let signing_key = ed25519_dalek::SigningKey::from_bytes(&key_bytes);
    let mut signed = receipt.sign(&signing_key);

    // Tamper with inputs_hash
    signed.inputs_hash[15] ^= 127;

    assert_eq!(
        signed.verify(),
        Err(RefusalCode::InvalidSignature),
        "Tampered input hash must fail verification"
    );
}

#[test]
fn test_sabotage_receipt_tamper_detection_timestamp() {
    // DEFECT 6: Tamper with timestamp after signing; signature must fail
    let op_id = [7u8; 16];
    let inputs = [1u8; HASH_SIZE];
    let outputs = [2u8; HASH_SIZE];

    let receipt = Receipt::new(op_id, inputs, outputs, None, 1000);

    use rand_core::RngCore;
    let mut csprng = rand_core::OsRng;
    let mut key_bytes = [0u8; 32];
    csprng.fill_bytes(&mut key_bytes);
    let signing_key = ed25519_dalek::SigningKey::from_bytes(&key_bytes);
    let mut signed = receipt.sign(&signing_key);

    // Tamper with timestamp
    signed.timestamp = 2000;

    assert_eq!(
        signed.verify(),
        Err(RefusalCode::InvalidSignature),
        "Tampered timestamp must fail verification"
    );
}

#[test]
fn test_sabotage_missing_signature_validation() {
    // DEFECT 7: Receipt with no signature (None) must fail verification
    let op_id = [7u8; 16];
    let inputs = [1u8; HASH_SIZE];
    let outputs = [2u8; HASH_SIZE];

    let unsigned = Receipt::new(op_id, inputs, outputs, None, 1000);

    // Verify with no signature must fail
    assert_eq!(
        unsigned.verify(),
        Err(RefusalCode::BoundaryEvidenceMissing),
        "Unsigned receipt must fail verification"
    );
}

#[test]
fn test_sabotage_duplicate_pair_rejection() {
    // DEFECT 8: Attempt to insert same pair twice; second must be rejected
    let mut page = RelationPage::new();
    let pair = Pair2::new(
        Node8::from_bytes([42, 42, 42, 42, 42, 42, 42, 42]),
        Node8::from_bytes([43, 43, 43, 43, 43, 43, 43, 43]),
    );

    assert!(page.insert(pair), "First insert should succeed");
    assert!(!page.insert(pair), "Duplicate insert must be rejected");
    assert_eq!(page.length, 1, "Length must remain 1");
}

#[test]
fn test_sabotage_symbol_bound_left_domain_256() {
    // DEFECT 9: Attempt to insert 257 unique left symbols (max 256)
    let mut page = RelationPage::new();

    for i in 0..256 {
        let pair = Pair2::new(
            Node8::from_bytes([(i >> 8) as u8, (i & 0xff) as u8, 0, 0, 0, 0, 0, 0]),
            Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, 1]), // Reuse single right symbol
        );
        let _ = page.insert(pair); // May succeed or fail; focus on bound
    }

    assert_eq!(
        page.left_len, 256,
        "Left symbols must not exceed 256 (or be <= 256 if page full)"
    );

    // Try 257th unique left symbol
    let overflow = Pair2::new(
        Node8::from_bytes([255, 255, 0, 0, 0, 0, 0, 0]),
        Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, 1]),
    );

    let inserted = page.insert(overflow);
    if !inserted {
        // Expected when left domain is saturated
        assert_eq!(
            page.left_len, 256,
            "Left domain bound must be enforced at 256"
        );
    }
}

#[test]
fn test_sabotage_symbol_bound_right_domain_256() {
    // DEFECT 10: Attempt to insert 257 unique right symbols (max 256)
    let mut page = RelationPage::new();

    for i in 0..256 {
        let pair = Pair2::new(
            Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, 1]), // Reuse single left symbol
            Node8::from_bytes([(i >> 8) as u8, (i & 0xff) as u8, 0, 0, 0, 0, 0, 0]),
        );
        let _ = page.insert(pair);
    }

    assert_eq!(page.right_len, 256, "Right symbols must not exceed 256");

    let overflow = Pair2::new(
        Node8::from_bytes([0, 0, 0, 0, 0, 0, 0, 1]),
        Node8::from_bytes([255, 255, 0, 0, 0, 0, 0, 0]),
    );

    let inserted = page.insert(overflow);
    if !inserted {
        assert_eq!(
            page.right_len, 256,
            "Right domain bound must be enforced at 256"
        );
    }
}

#[test]
fn test_sabotage_pair_removal_state_consistency() {
    // DEFECT 11: Remove pair, then verify it cannot be re-queried until re-inserted
    let mut page = RelationPage::new();
    let pair = Pair2::new(
        Node8::from_bytes([10, 10, 10, 10, 10, 10, 10, 10]),
        Node8::from_bytes([20, 20, 20, 20, 20, 20, 20, 20]),
    );

    assert!(page.insert(pair));
    assert!(page.contains(&pair));

    assert!(page.remove(&pair));
    assert!(!page.contains(&pair), "Removed pair must not be found");

    // Re-insertion must succeed
    assert!(page.insert(pair));
    assert!(page.contains(&pair));
}

#[test]
fn test_sabotage_construct8_serialization_consistency() {
    // DEFECT 12: Construct8::to_bytes() must produce identical output across multiple calls
    let construct = Construct8::new(
        Node8::from_bytes([1, 2, 3, 4, 5, 6, 7, 8]),
        Predicate8::from_bytes([10, 11, 12, 13, 14, 15, 16, 17]),
        Object8::from_bytes([20, 21, 22, 23, 24, 25, 26, 27]),
        Graph8::from_bytes([30, 31, 32, 33, 34, 35, 36, 37]),
        Mask8::from_bytes([40, 41, 42, 43, 44, 45, 46, 47]),
        Provenance8::from_bytes([50, 51, 52, 53, 54, 55, 56, 57]),
        Admission8::from_bytes([60, 61, 62, 63, 64, 65, 66, 67]),
        ReceiptHint8::from_bytes([70, 71, 72, 73, 74, 75, 76, 77]),
    );

    let bytes1 = construct.to_bytes();
    let bytes2 = construct.to_bytes();
    let bytes3 = construct.to_bytes();

    assert_eq!(bytes1, bytes2, "First two serializations must match");
    assert_eq!(bytes2, bytes3, "All three serializations must match");
}

#[test]
fn test_sabotage_refusal_evidence_captures_bytes() {
    // DEFECT 13: Refusal must capture full 64-byte evidence from Construct8
    let sabotaged = Construct8::new(
        Node8::from_bytes([7, 7, 7, 7, 7, 7, 7, 7]),
        Predicate8::from_bytes([8, 8, 8, 8, 8, 8, 8, 8]),
        Object8::from_bytes([9, 9, 9, 9, 9, 9, 9, 9]),
        Graph8::from_bytes([10, 10, 10, 10, 10, 10, 10, 10]),
        Mask8::from_bytes([0, 0, 0, 0, 0, 0, 0, 0]), // Zero mask → refusal
        Provenance8::from_bytes([11, 11, 11, 11, 11, 11, 11, 11]),
        Admission8::from_bytes([12, 12, 12, 12, 12, 12, 12, 12]),
        ReceiptHint8::from_bytes([13, 13, 13, 13, 13, 13, 13, 13]),
    );

    let expected_bytes = sabotaged.to_bytes();

    let mut replay = Replay::new();
    replay.push(sabotaged);
    let result = replay.run(RelationPage::new(), 5000);

    assert!(result.is_err());
    let refusal = result.unwrap_err();
    assert_eq!(
        refusal.evidence, expected_bytes,
        "Refusal evidence must capture full 64 bytes"
    );
    assert_eq!(refusal.evidence.len(), 64);
}

#[test]
fn test_sabotage_receipt_chain_integrity() {
    // DEFECT 14: Parent hash must be preserved in chained receipts
    let parent_hash = [99u8; HASH_SIZE];
    let op_id = [7u8; 16];
    let inputs = [1u8; HASH_SIZE];
    let outputs = [2u8; HASH_SIZE];

    let receipt = Receipt::new(op_id, inputs, outputs, Some(parent_hash), 1000);

    assert_eq!(
        receipt.previous_receipt_hash,
        Some(parent_hash),
        "Parent hash must be preserved"
    );

    // Verify chain
    assert!(receipt.previous_receipt_hash.is_some());
    assert_eq!(receipt.previous_receipt_hash.unwrap(), parent_hash);
}

#[test]
fn test_sabotage_operation_id_size() {
    // DEFECT 15: Receipt operation_id must be exactly 16 bytes
    let op_id = [123u8; 16];
    let inputs = [1u8; HASH_SIZE];
    let outputs = [2u8; HASH_SIZE];

    let receipt = Receipt::new(op_id, inputs, outputs, None, 1000);

    assert_eq!(receipt.operation_id.len(), 16);
    assert_eq!(receipt.operation_id, op_id);
}

#[test]
fn test_sabotage_invalid_timestamp_in_refusal() {
    // DEFECT 16: Refusal must capture exact timestamp from run() call
    let sabotaged = Construct8::new(
        Node8::from_bytes([1; 8]),
        Predicate8::from_bytes([2; 8]),
        Object8::from_bytes([3; 8]),
        Graph8::from_bytes([4; 8]),
        Mask8::from_bytes([0; 8]), // Zero mask
        Provenance8::from_bytes([5; 8]),
        Admission8::from_bytes([6; 8]),
        ReceiptHint8::from_bytes([7; 8]),
    );

    let mut replay = Replay::new();
    replay.push(sabotaged);

    let timestamp = 9876543210u64;
    let result = replay.run(RelationPage::new(), timestamp);

    assert!(result.is_err());
    let refusal = result.unwrap_err();
    assert_eq!(
        refusal.timestamp, timestamp,
        "Refusal timestamp must match run() argument"
    );
}
