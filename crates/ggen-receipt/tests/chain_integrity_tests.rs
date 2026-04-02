//! Chain integrity verification tests for ggen-receipt.
//!
//! These tests validate the full cryptographic integrity of receipt chains:
//! - Hash-link consistency between consecutive receipts
//! - Signature verification across the entire chain
//! - Tamper detection at every position (genesis, intermediate, leaf)
//! - Chain reconstruction after serialization round-trip
//! - Multi-key chain verification (mixed signing keys)
//! - Chain ordering integrity
//! - Hash-data utility integrity
//! - Input/output provenance tracking
//!
//! Chicago TDD: Real Ed25519 signing, real SHA-256 hashing, no mocks.

use ggen_receipt::{create_chained_receipt, generate_keypair, hash_data, Receipt, ReceiptChain};
use std::fs;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// 1. Hash-link integrity
// ---------------------------------------------------------------------------

#[test]
fn test_hash_links_are_consistent_across_chain() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new("op-0".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis failed");

    let mut prev = genesis;
    for i in 1..=5 {
        let receipt = Receipt::new(format!("op-{}", i), vec![], vec![], None)
            .chain(&prev)
            .expect("chaining failed")
            .sign(&signing_key)
            .expect("signing failed");

        // Verify the link before appending
        let expected_hash = prev.hash().expect("hash failed");
        assert_eq!(
            receipt.previous_receipt_hash.as_ref(),
            Some(&expected_hash),
            "Receipt {} must link to previous hash",
            i
        );

        chain.append(receipt.clone()).expect("append failed");
        prev = receipt;
    }

    // Full chain verification must pass
    assert!(chain.verify(&verifying_key).is_ok());
}

#[test]
fn test_each_receipt_hashes_to_stable_value() {
    let (signing_key, _) = generate_keypair();

    let genesis = Receipt::new("stable-hash-test".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let h1 = genesis.hash().expect("hash 1 failed");
    let h2 = genesis.hash().expect("hash 2 failed");
    let h3 = genesis.hash().expect("hash 3 failed");

    assert_eq!(h1, h2, "Hash must be deterministic across calls");
    assert_eq!(h2, h3, "Hash must be deterministic across calls");
    assert_eq!(h1.len(), 64, "SHA-256 hash must be 64 hex chars");
}

#[test]
fn test_hash_links_change_when_previous_receipt_data_changes() {
    let (signing_key, _) = generate_keypair();

    let receipt_a = Receipt::new("op-a".to_string(), vec!["in".to_string()], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let receipt_b = Receipt::new(
        "op-b".to_string(),
        vec!["different-in".to_string()],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("signing failed");

    let hash_a = receipt_a.hash().expect("hash a failed");
    let hash_b = receipt_b.hash().expect("hash b failed");

    assert_ne!(
        hash_a, hash_b,
        "Different receipts must have different hashes"
    );

    // A receipt chained to A vs chained to B must have different previous hashes
    let linked_to_a = Receipt::new("next".to_string(), vec![], vec![], None)
        .chain(&receipt_a)
        .expect("chain failed");

    let linked_to_b = Receipt::new("next".to_string(), vec![], vec![], None)
        .chain(&receipt_b)
        .expect("chain failed");

    assert_ne!(
        linked_to_a.previous_receipt_hash, linked_to_b.previous_receipt_hash,
        "Chain links must differ when parents differ"
    );
}

// ---------------------------------------------------------------------------
// 2. Signature integrity across chain
// ---------------------------------------------------------------------------

#[test]
fn test_all_signatures_in_chain_verify() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new("sig-chain-genesis".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis failed");

    let mut prev = genesis;
    for i in 1..=20 {
        let receipt = Receipt::new(format!("sig-op-{}", i), vec![], vec![], None)
            .chain(&prev)
            .expect("chaining failed")
            .sign(&signing_key)
            .expect("signing failed");

        // Individual signature check before append
        assert!(
            receipt.verify(&verifying_key).is_ok(),
            "Receipt {} signature must verify individually",
            i
        );

        chain.append(receipt.clone()).expect("append failed");
        prev = receipt;
    }

    // Full chain verification
    assert!(chain.verify(&verifying_key).is_ok());
}

#[test]
fn test_chain_verify_detects_single_bad_signature() {
    let (signing_key, verifying_key) = generate_keypair();
    let (attacker_signing_key, _attacker_verifying) = generate_keypair();

    let genesis = Receipt::new("integrity-genesis".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut prev = genesis;
    let mut chain = ReceiptChain::from_genesis(prev.clone()).expect("genesis failed");

    for i in 1..=5 {
        let use_attacker = i == 3; // Corrupt receipt at position 3
        let key = if use_attacker {
            &attacker_signing_key
        } else {
            &signing_key
        };

        let receipt = Receipt::new(format!("integrity-op-{}", i), vec![], vec![], None)
            .chain(&prev)
            .expect("chaining failed")
            .sign(key)
            .expect("signing failed");

        chain.append(receipt.clone()).expect("append failed");
        prev = receipt;
    }

    // Chain verify must fail because receipt at index 3 has wrong signature
    let result = chain.verify(&verifying_key);
    assert!(
        result.is_err(),
        "Chain with corrupted signature must fail verification"
    );
}

// ---------------------------------------------------------------------------
// 3. Tamper detection at every position
// ---------------------------------------------------------------------------

#[test]
fn test_tampering_genesis_operation_id_detected() {
    let (signing_key, verifying_key) = generate_keypair();

    let mut genesis = Receipt::new("genesis".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    genesis.operation_id = "tampered-genesis".to_string();

    let chain = ReceiptChain::from_genesis(genesis).expect("genesis creation failed");

    let result = chain.verify(&verifying_key);
    assert!(
        result.is_err(),
        "Tampered genesis operation_id must be detected"
    );
}

#[test]
fn test_tampering_genesis_timestamp_detected() {
    let (signing_key, verifying_key) = generate_keypair();

    let mut genesis = Receipt::new("genesis".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    genesis.timestamp = genesis.timestamp - chrono::Duration::hours(24);

    let chain = ReceiptChain::from_genesis(genesis).expect("genesis creation failed");

    let result = chain.verify(&verifying_key);
    assert!(
        result.is_err(),
        "Tampered genesis timestamp must be detected"
    );
}

#[test]
fn test_tampering_intermediate_receipt_input_hashes_detected() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new("gen".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut receipt2 = Receipt::new(
        "mid".to_string(),
        vec!["real-input".to_string()],
        vec![],
        None,
    )
    .chain(&genesis)
    .expect("chaining failed")
    .sign(&signing_key)
    .expect("signing failed");

    receipt2.input_hashes = vec!["fake-input".to_string()];

    let receipt3 = Receipt::new("leaf".to_string(), vec![], vec![], None)
        .chain(&receipt2)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis).expect("genesis failed");
    chain.append(receipt2).expect("append failed");
    chain.append(receipt3).expect("append failed");

    let result = chain.verify(&verifying_key);
    assert!(
        result.is_err(),
        "Tampered intermediate input_hashes must be detected"
    );
}

#[test]
fn test_tampering_leaf_receipt_output_hashes_detected() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new("gen".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let receipt2 = Receipt::new("mid".to_string(), vec![], vec![], None)
        .chain(&genesis)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

    let mut receipt3 = Receipt::new(
        "leaf".to_string(),
        vec![],
        vec!["real-output".to_string()],
        None,
    )
    .chain(&receipt2)
    .expect("chaining failed")
    .sign(&signing_key)
    .expect("signing failed");

    receipt3.output_hashes = vec!["tampered-output".to_string()];

    let mut chain = ReceiptChain::from_genesis(genesis).expect("genesis failed");
    chain.append(receipt2).expect("append failed");
    chain.append(receipt3).expect("append failed");

    let result = chain.verify(&verifying_key);
    assert!(
        result.is_err(),
        "Tampered leaf output_hashes must be detected"
    );
}

#[test]
fn test_tampering_signature_bytes_detected() {
    let (signing_key, verifying_key) = generate_keypair();

    let mut receipt = Receipt::new("sig-tamper".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    // Flip bits in the signature
    let mut sig_bytes = hex::decode(&receipt.signature).expect("hex decode failed");
    sig_bytes[0] ^= 0xFF;
    sig_bytes[31] ^= 0xAA;
    receipt.signature = hex::encode(&sig_bytes);

    assert!(
        receipt.verify(&verifying_key).is_err(),
        "Bit-flipped signature must fail verification"
    );
}

// ---------------------------------------------------------------------------
// 4. Serialization round-trip integrity
// ---------------------------------------------------------------------------

#[test]
fn test_chain_integrity_preserved_after_serialization_roundtrip() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new(
        "ser-genesis".to_string(),
        vec!["in1".to_string()],
        vec!["out1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis failed");

    let mut prev = genesis;
    for i in 1..=5 {
        let receipt = Receipt::new(
            format!("ser-op-{}", i),
            vec![format!("in-{}", i)],
            vec![format!("out-{}", i)],
            None,
        )
        .chain(&prev)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

        chain.append(receipt.clone()).expect("append failed");
        prev = receipt;
    }

    // Verify before serialization
    assert!(chain.verify(&verifying_key).is_ok());

    // Serialize to JSON
    let json = serde_json::to_string(&chain).expect("serialization failed");

    // Deserialize
    let restored: ReceiptChain = serde_json::from_str(&json).expect("deserialization failed");

    // Verify after deserialization
    assert!(
        restored.verify(&verifying_key).is_ok(),
        "Restored chain must verify"
    );
    assert_eq!(
        restored.len(),
        chain.len(),
        "Chain length must be preserved"
    );
}

#[test]
fn test_chain_integrity_preserved_after_file_roundtrip() {
    let temp_dir = TempDir::new().expect("temp dir failed");
    let chain_path = temp_dir.path().join("receipt-chain.json");

    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new("file-genesis".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis failed");

    let receipt2 = Receipt::new("file-op-2".to_string(), vec![], vec![], None)
        .chain(&genesis)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

    let receipt3 = Receipt::new("file-op-3".to_string(), vec![], vec![], None)
        .chain(&receipt2)
        .expect("chaining failed")
        .sign(&signing_key)
        .expect("signing failed");

    chain.append(receipt2).expect("append failed");
    chain.append(receipt3).expect("append failed");

    // Write to file
    let json = serde_json::to_string_pretty(&chain).expect("serialization failed");
    fs::write(&chain_path, &json).expect("write failed");

    // Read from file
    let read_json = fs::read_to_string(&chain_path).expect("read failed");
    let loaded: ReceiptChain = serde_json::from_str(&read_json).expect("deserialization failed");

    // Verify loaded chain
    assert!(
        loaded.verify(&verifying_key).is_ok(),
        "Loaded chain must verify"
    );
    assert_eq!(loaded.len(), 3);
}

#[test]
fn test_individual_receipt_integrity_after_json_roundtrip() {
    let (signing_key, verifying_key) = generate_keypair();

    let original = Receipt::new(
        "roundtrip-receipt".to_string(),
        vec!["input-a".to_string(), "input-b".to_string()],
        vec!["output-x".to_string()],
        Some("prev-hash".to_string()),
    )
    .sign(&signing_key)
    .expect("signing failed");

    let json = serde_json::to_string(&original).expect("serialization failed");
    let restored: Receipt = serde_json::from_str(&json).expect("deserialization failed");

    // All fields must be preserved
    assert_eq!(original.operation_id, restored.operation_id);
    assert_eq!(original.input_hashes, restored.input_hashes);
    assert_eq!(original.output_hashes, restored.output_hashes);
    assert_eq!(original.signature, restored.signature);
    assert_eq!(
        original.previous_receipt_hash,
        restored.previous_receipt_hash
    );

    // Signature must still verify
    assert!(
        restored.verify(&verifying_key).is_ok(),
        "Round-tripped receipt must verify"
    );
}

// ---------------------------------------------------------------------------
// 5. create_chained_receipt helper integrity
// ---------------------------------------------------------------------------

#[test]
fn test_create_chained_receipt_produces_valid_link() {
    let (signing_key, verifying_key) = generate_keypair();

    let parent = Receipt::new("parent".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let parent_hash = parent.hash().expect("hash failed");

    let child = create_chained_receipt(
        &parent,
        "child-op".to_string(),
        vec!["child-in".to_string()],
        vec!["child-out".to_string()],
        &signing_key,
    )
    .expect("chained creation failed");

    // Verify link
    assert_eq!(
        child.previous_receipt_hash.as_ref(),
        Some(&parent_hash),
        "Child must link to parent hash"
    );

    // Verify signature
    assert!(
        child.verify(&verifying_key).is_ok(),
        "Chained receipt must verify"
    );

    // Verify in a chain
    let mut chain = ReceiptChain::from_genesis(parent).expect("genesis failed");
    chain.append(child).expect("append failed");
    assert!(chain.verify(&verifying_key).is_ok());
}

#[test]
fn test_create_chained_receipt_chain_of_ten() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new("auto-genesis".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis failed");
    let mut prev = genesis;

    for i in 1..=10 {
        let next = create_chained_receipt(
            &prev,
            format!("auto-op-{}", i),
            vec![format!("in-{}", i)],
            vec![format!("out-{}", i)],
            &signing_key,
        )
        .expect("chained creation failed");

        chain.append(next.clone()).expect("append failed");
        prev = next;
    }

    assert_eq!(chain.len(), 11);
    assert!(chain.verify(&verifying_key).is_ok());
}

// ---------------------------------------------------------------------------
// 6. Chain ordering integrity
// ---------------------------------------------------------------------------

#[test]
fn test_receipts_maintain_insertion_order() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new("ordered-genesis".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis failed");

    let ids: Vec<String> = (1..=7).map(|i| format!("ordered-op-{}", i)).collect();
    let mut prev = genesis;

    for id in &ids {
        let receipt = Receipt::new(id.clone(), vec![], vec![], None)
            .chain(&prev)
            .expect("chaining failed")
            .sign(&signing_key)
            .expect("signing failed");

        chain.append(receipt.clone()).expect("append failed");
        prev = receipt;
    }

    // Verify ordering
    let receipts = chain.receipts();
    assert_eq!(receipts[0].operation_id, "ordered-genesis");
    for (i, expected_id) in ids.iter().enumerate() {
        assert_eq!(
            receipts[i + 1].operation_id,
            *expected_id,
            "Receipt at position {} must have correct operation_id",
            i + 1
        );
    }

    assert!(chain.verify(&verifying_key).is_ok());
}

#[test]
fn test_chain_length_matches_receipt_count() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new("count-genesis".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis failed");

    let expected_length = 50;
    let mut prev = genesis;
    for i in 1..expected_length {
        let receipt = Receipt::new(format!("count-op-{}", i), vec![], vec![], None)
            .chain(&prev)
            .expect("chaining failed")
            .sign(&signing_key)
            .expect("signing failed");

        chain.append(receipt.clone()).expect("append failed");
        prev = receipt;
    }

    assert_eq!(chain.len(), expected_length);
    assert_eq!(chain.receipts().len(), expected_length);
    assert!(!chain.is_empty());
    assert!(chain.verify(&verifying_key).is_ok());
}

// ---------------------------------------------------------------------------
// 7. Boundary and edge case integrity
// ---------------------------------------------------------------------------

#[test]
fn test_single_receipt_chain_integrity() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new("single".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let chain = ReceiptChain::from_genesis(genesis).expect("genesis failed");

    assert_eq!(chain.len(), 1);
    assert!(chain.verify(&verifying_key).is_ok());
    assert_eq!(chain.genesis().unwrap().operation_id, "single");
    assert_eq!(chain.last().unwrap().operation_id, "single");
    // genesis and last should be the same receipt for a single-element chain
    assert_eq!(
        chain.genesis().unwrap().hash().expect("hash failed"),
        chain.last().unwrap().hash().expect("hash failed"),
    );
}

#[test]
fn test_empty_chain_verifies_trivially() {
    let (_, verifying_key) = generate_keypair();
    let chain = ReceiptChain::new();

    assert!(chain.is_empty());
    assert_eq!(chain.len(), 0);
    assert!(chain.genesis().is_none());
    assert!(chain.last().is_none());
    assert!(chain.verify(&verifying_key).is_ok());
}

#[test]
fn test_chain_with_large_data_payloads_integrity() {
    let (signing_key, verifying_key) = generate_keypair();

    let large_inputs: Vec<String> = (0..50)
        .map(|i| format!("input-hash-{}-padding-{}", i, "x".repeat(64)))
        .collect();
    let large_outputs: Vec<String> = (0..50)
        .map(|i| format!("output-hash-{}-padding-{}", i, "y".repeat(64)))
        .collect();

    let genesis = Receipt::new(
        "large-genesis".to_string(),
        large_inputs.clone(),
        large_outputs.clone(),
        None,
    )
    .sign(&signing_key)
    .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("genesis failed");

    let receipt2 = Receipt::new(
        "large-op-2".to_string(),
        (50..100)
            .map(|i| format!("input-{}-{}", i, "a".repeat(64)))
            .collect(),
        (50..100)
            .map(|i| format!("output-{}-{}", i, "b".repeat(64)))
            .collect(),
        None,
    )
    .chain(&genesis)
    .expect("chaining failed")
    .sign(&signing_key)
    .expect("signing failed");

    chain.append(receipt2).expect("append failed");

    assert!(chain.verify(&verifying_key).is_ok());
    assert_eq!(chain.genesis().unwrap().input_hashes.len(), 50);
    assert_eq!(chain.genesis().unwrap().output_hashes.len(), 50);
}

#[test]
fn test_chain_default_is_empty() {
    let chain = ReceiptChain::default();
    assert!(chain.is_empty());
    assert_eq!(chain.len(), 0);
    assert!(chain.receipts().is_empty());
}

// ---------------------------------------------------------------------------
// 8. Tampered chain link detection (hash mismatch, not just signature)
// ---------------------------------------------------------------------------

#[test]
fn test_append_rejects_receipt_with_wrong_previous_hash() {
    let (signing_key, _) = generate_keypair();

    let genesis = Receipt::new("hash-check-genesis".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis).expect("genesis failed");

    // Create a receipt that points to a completely wrong hash
    let bad_link = Receipt::new(
        "bad-link".to_string(),
        vec![],
        vec![],
        Some("0000000000000000000000000000000000000000000000000000000000000000".to_string()),
    )
    .sign(&signing_key)
    .expect("signing failed");

    let result = chain.append(bad_link);
    assert!(
        result.is_err(),
        "Receipt with wrong previous hash must be rejected"
    );
}

#[test]
fn test_append_rejects_genesis_after_chain_has_receipts() {
    let (signing_key, _) = generate_keypair();

    let genesis = Receipt::new("first-genesis".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis).expect("genesis failed");

    // Try to append another genesis (no previous hash) to a non-empty chain
    let second_genesis = Receipt::new("second-genesis".to_string(), vec![], vec![], None)
        .sign(&signing_key)
        .expect("signing failed");

    let result = chain.append(second_genesis);
    assert!(
        result.is_err(),
        "Non-genesis receipt must have a previous hash when chain is non-empty"
    );
}

// ---------------------------------------------------------------------------
// 9. hash_data utility integrity
// ---------------------------------------------------------------------------

#[test]
fn test_hash_data_known_empty_value() {
    let hash = hash_data(b"");
    assert_eq!(
        hash, "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
        "SHA-256 of empty input must match known value"
    );
}

#[test]
fn test_hash_data_avalanche_property() {
    let data1 = b"receipt-chain-integrity";
    let data2 = b"receipt-chain-integritf"; // Single character difference

    let hash1 = hash_data(data1);
    let hash2 = hash_data(data2);

    assert_ne!(
        hash1, hash2,
        "One-bit input change must produce different hash"
    );

    // Count differing hex characters (not bits, but still illustrative)
    let diff_count = hash1
        .chars()
        .zip(hash2.chars())
        .filter(|(a, b)| a != b)
        .count();

    // A significant portion should differ
    assert!(
        diff_count > 40,
        "Expected at least 40 differing hex chars, got {}",
        diff_count
    );
}

// ---------------------------------------------------------------------------
// 10. Multi-receipt chain with input/output tracking integrity
// ---------------------------------------------------------------------------

#[test]
fn test_chain_tracks_input_output_provenance() {
    let (signing_key, verifying_key) = generate_keypair();

    // Simulate a pipeline: extract -> transform -> load
    let extract = Receipt::new(
        "extract".to_string(),
        vec!["source-data-hash".to_string()],
        vec!["extracted-model-hash".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("signing failed");

    let transform = Receipt::new(
        "transform".to_string(),
        vec!["extracted-model-hash".to_string()],
        vec!["transformed-model-hash".to_string()],
        None,
    )
    .chain(&extract)
    .expect("chaining failed")
    .sign(&signing_key)
    .expect("signing failed");

    let load = Receipt::new(
        "load".to_string(),
        vec!["transformed-model-hash".to_string()],
        vec!["deployed-artifact-hash".to_string()],
        None,
    )
    .chain(&transform)
    .expect("chaining failed")
    .sign(&signing_key)
    .expect("signing failed");

    let mut chain = ReceiptChain::from_genesis(extract).expect("genesis failed");
    chain.append(transform).expect("append failed");
    chain.append(load).expect("append failed");

    assert!(chain.verify(&verifying_key).is_ok());

    // Verify data provenance
    let receipts = chain.receipts();
    assert_eq!(receipts[0].operation_id, "extract");
    assert_eq!(receipts[1].operation_id, "transform");
    assert_eq!(receipts[2].operation_id, "load");

    // Extract's output feeds transform's input
    assert_eq!(
        receipts[0].output_hashes[0], receipts[1].input_hashes[0],
        "Extract output must match transform input"
    );

    // Transform's output feeds load's input
    assert_eq!(
        receipts[1].output_hashes[0], receipts[2].input_hashes[0],
        "Transform output must match load input"
    );
}
