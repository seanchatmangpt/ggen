//! E2E tests for receipt chain verification
//!
//! Tests:
//! - Genesis receipt creation
//! - Multi-receipt chain construction
//! - Chain verification with Ed25519 signatures
//! - Tamper detection in chain
//! - Parallel chain verification

use ggen_receipt::{generate_keypair, hash_data, Receipt};

/// Test genesis receipt creation
#[tokio::test]
async fn test_genesis_receipt_creation() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let input_hash = hash_data(b"test input data");
    let output_hash = hash_data(b"test output data");

    // Act
    let genesis = Receipt::new(
        "genesis-op".to_string(),
        vec![input_hash],
        vec![output_hash],
        None, // No previous receipt
    )
    .sign(&signing_key)
    .expect("Failed to sign genesis receipt");

    // Assert
    assert_eq!(genesis.operation_id, "genesis-op");
    assert!(genesis.previous_receipt_hash.is_none());
    assert!(!genesis.signature.is_empty());
    assert!(genesis.verify(&verifying_key).is_ok());
}

/// Test multi-receipt chain construction
#[tokio::test]
async fn test_receipt_chain_construction() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let chain_length = 10;

    // Act - Build chain
    let mut receipts = Vec::new();
    for i in 0..chain_length {
        let input = format!("input-{}", i);
        let output = format!("output-{}", i);
        let input_hash = hash_data(input.as_bytes());
        let output_hash = hash_data(output.as_bytes());

        let receipt = if i == 0 {
            // Genesis receipt
            Receipt::new(
                format!("op-{}", i),
                vec![input_hash],
                vec![output_hash],
                None,
            )
            .sign(&signing_key)
            .expect("Failed to sign genesis")
        } else {
            // Chain to previous
            Receipt::new(
                format!("op-{}", i),
                vec![input_hash],
                vec![output_hash],
                None,
            )
            .chain(&receipts[i - 1])
            .expect("Failed to chain receipt")
            .sign(&signing_key)
            .expect("Failed to sign receipt")
        };

        receipts.push(receipt);
    }

    // Assert - Verify chain integrity
    assert_eq!(receipts.len(), chain_length);

    // Verify genesis has no previous
    assert!(receipts[0].previous_receipt_hash.is_none());

    // Verify all receipts are properly chained
    for i in 1..chain_length {
        let prev_hash = receipts[i - 1].hash().expect("Failed to hash previous");
        assert_eq!(
            receipts[i].previous_receipt_hash,
            Some(prev_hash),
            "Receipt {} not properly chained",
            i
        );
    }

    // Verify all signatures
    for (i, receipt) in receipts.iter().enumerate() {
        assert!(
            receipt.verify(&verifying_key).is_ok(),
            "Receipt {} signature verification failed",
            i
        );
    }
}

/// Test chain verification with Ed25519 signatures
#[tokio::test]
async fn test_chain_signature_verification() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let (_wrong_key, wrong_verifying_key) = generate_keypair();

    // Act - Create chain
    let receipt1 = Receipt::new(
        "op1".to_string(),
        vec![hash_data(b"in1")],
        vec![hash_data(b"out1")],
        None,
    )
    .sign(&signing_key)
    .expect("Failed to sign");

    let receipt2 = Receipt::new(
        "op2".to_string(),
        vec![hash_data(b"in2")],
        vec![hash_data(b"out2")],
        None,
    )
    .chain(&receipt1)
    .expect("Failed to chain")
    .sign(&signing_key)
    .expect("Failed to sign");

    // Assert - Correct key verifies
    assert!(receipt1.verify(&verifying_key).is_ok());
    assert!(receipt2.verify(&verifying_key).is_ok());

    // Assert - Wrong key fails
    assert!(receipt1.verify(&wrong_verifying_key).is_err());
    assert!(receipt2.verify(&wrong_verifying_key).is_err());
}

/// Test tamper detection in chain
#[tokio::test]
async fn test_tamper_detection() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();

    let receipt1 = Receipt::new(
        "op1".to_string(),
        vec![hash_data(b"in1")],
        vec![hash_data(b"out1")],
        None,
    )
    .sign(&signing_key)
    .expect("Failed to sign");

    let mut receipt2 = Receipt::new(
        "op2".to_string(),
        vec![hash_data(b"in2")],
        vec![hash_data(b"out2")],
        None,
    )
    .chain(&receipt1)
    .expect("Failed to chain")
    .sign(&signing_key)
    .expect("Failed to sign");

    // Act - Tamper with receipt2 (modify operation_id after signing)
    let original_hash = receipt2.hash().expect("Failed to hash");
    let original_prev = receipt2.previous_receipt_hash.clone();

    // Create tampered version by changing the previous hash
    receipt2.previous_receipt_hash = Some("tampered_hash".to_string());

    // Assert - Hash changed after tampering
    let tampered_hash = receipt2.hash().expect("Failed to hash");
    assert_ne!(
        original_hash, tampered_hash,
        "Hash should change after tampering"
    );

    // Assert - Chain integrity broken
    assert_ne!(
        receipt2.previous_receipt_hash, original_prev,
        "Previous hash was tampered"
    );

    // Assert - Signature verification should fail because data was tampered
    // The signature was computed on original data, but verify() recomputes from current state
    assert!(receipt2.verify(&verifying_key).is_err());

    // Verify chain link is broken
    let receipt1_hash = receipt1.hash().expect("Failed to hash");
    assert_ne!(
        receipt2.previous_receipt_hash,
        Some(receipt1_hash),
        "Chain link should be broken"
    );
}

/// Test parallel chain verification
#[tokio::test]
async fn test_parallel_chain_verification() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();
    let num_chains = 5;
    let chain_length = 10;

    // Act - Build multiple chains in parallel
    let chains: Vec<Vec<Receipt>> = (0..num_chains)
        .map(|chain_id| {
            let mut chain = Vec::new();
            for i in 0..chain_length {
                let input = format!("chain-{}-input-{}", chain_id, i);
                let output = format!("chain-{}-output-{}", chain_id, i);
                let input_hash = hash_data(input.as_bytes());
                let output_hash = hash_data(output.as_bytes());

                let receipt = if i == 0 {
                    Receipt::new(
                        format!("chain-{}-op-{}", chain_id, i),
                        vec![input_hash],
                        vec![output_hash],
                        None,
                    )
                    .sign(&signing_key)
                    .expect("Failed to sign")
                } else {
                    Receipt::new(
                        format!("chain-{}-op-{}", chain_id, i),
                        vec![input_hash],
                        vec![output_hash],
                        None,
                    )
                    .chain(&chain[i - 1])
                    .expect("Failed to chain")
                    .sign(&signing_key)
                    .expect("Failed to sign")
                };

                chain.push(receipt);
            }
            chain
        })
        .collect();

    // Assert - Verify all chains
    assert_eq!(chains.len(), num_chains);

    for (chain_id, chain) in chains.iter().enumerate() {
        assert_eq!(chain.len(), chain_length);

        // Verify genesis
        assert!(chain[0].previous_receipt_hash.is_none());

        // Verify chain links
        for i in 1..chain_length {
            let prev_hash = chain[i - 1].hash().expect("Failed to hash");
            assert_eq!(
                chain[i].previous_receipt_hash,
                Some(prev_hash),
                "Chain {} receipt {} not properly linked",
                chain_id,
                i
            );
        }

        // Verify all signatures
        for (i, receipt) in chain.iter().enumerate() {
            assert!(
                receipt.verify(&verifying_key).is_ok(),
                "Chain {} receipt {} signature failed",
                chain_id,
                i
            );
        }
    }
}

/// Test receipt chain with different input/output hashes
#[tokio::test]
async fn test_receipt_chain_with_varied_hashes() {
    // Arrange
    let (signing_key, verifying_key) = generate_keypair();

    // Act - Create receipts with varying numbers of inputs/outputs
    let receipt1 = Receipt::new(
        "multi-input-op".to_string(),
        vec![
            hash_data(b"input1"),
            hash_data(b"input2"),
            hash_data(b"input3"),
        ],
        vec![hash_data(b"output1")],
        None,
    )
    .sign(&signing_key)
    .expect("Failed to sign");

    let receipt2 = Receipt::new(
        "multi-output-op".to_string(),
        vec![hash_data(b"input")],
        vec![
            hash_data(b"output1"),
            hash_data(b"output2"),
            hash_data(b"output3"),
            hash_data(b"output4"),
        ],
        None,
    )
    .chain(&receipt1)
    .expect("Failed to chain")
    .sign(&signing_key)
    .expect("Failed to sign");

    // Assert
    assert_eq!(receipt1.input_hashes.len(), 3);
    assert_eq!(receipt1.output_hashes.len(), 1);
    assert_eq!(receipt2.input_hashes.len(), 1);
    assert_eq!(receipt2.output_hashes.len(), 4);
    assert!(receipt1.verify(&verifying_key).is_ok());
    assert!(receipt2.verify(&verifying_key).is_ok());

    let receipt1_hash = receipt1.hash().expect("Failed to hash");
    assert_eq!(receipt2.previous_receipt_hash, Some(receipt1_hash));
}
