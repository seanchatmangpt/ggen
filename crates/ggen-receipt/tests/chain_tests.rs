//! Comprehensive tests for receipt chain validation.

use ggen_receipt::{generate_keypair, Receipt, ReceiptChain};

#[test]
fn test_empty_chain() {
    let chain = ReceiptChain::new();
    assert!(chain.is_empty());
    assert_eq!(chain.len(), 0);
    assert!(chain.genesis().is_none());
    assert!(chain.last().is_none());
}

#[test]
fn test_genesis_chain_creation() {
    let (signing_key, _) = generate_keypair();

    let genesis = Receipt::new(
        "genesis".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let chain = ReceiptChain::from_genesis(genesis.clone()).expect("Genesis creation failed");

    assert!(!chain.is_empty());
    assert_eq!(chain.len(), 1);
    assert_eq!(chain.genesis().unwrap().operation_id, "genesis");
    assert_eq!(chain.last().unwrap().operation_id, "genesis");
}

#[test]
fn test_genesis_with_previous_hash_rejected() {
    let (signing_key, _) = generate_keypair();

    let invalid_genesis = Receipt::new(
        "genesis".to_string(),
        vec![],
        vec![],
        Some("previous".to_string()),
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let result = ReceiptChain::from_genesis(invalid_genesis);
    assert!(result.is_err());
}

#[test]
fn test_chain_append_single() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new(
        "op1".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("Genesis creation failed");

    let receipt2 = Receipt::new(
        "op2".to_string(),
        vec![],
        vec![],
        None,
    )
    .chain(&genesis)
    .expect("Chaining failed")
    .sign(&signing_key)
    .expect("Signing failed");

    chain.append(receipt2).expect("Append failed");

    assert_eq!(chain.len(), 2);
    assert!(chain.verify(&verifying_key).is_ok());
}

#[test]
fn test_chain_append_multiple() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new(
        "op1".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("Genesis creation failed");

    let mut prev = genesis;
    for i in 2..=10 {
        let receipt = Receipt::new(
            format!("op{}", i),
            vec![],
            vec![],
            None,
        )
        .chain(&prev)
        .expect("Chaining failed")
        .sign(&signing_key)
        .expect("Signing failed");

        chain.append(receipt.clone()).expect("Append failed");
        prev = receipt;
    }

    assert_eq!(chain.len(), 10);
    assert!(chain.verify(&verifying_key).is_ok());
}

#[test]
fn test_chain_append_wrong_hash_rejected() {
    let (signing_key, _) = generate_keypair();

    let genesis = Receipt::new(
        "op1".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis).expect("Genesis creation failed");

    let wrong_receipt = Receipt::new(
        "op2".to_string(),
        vec![],
        vec![],
        Some("wrong_hash".to_string()),
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let result = chain.append(wrong_receipt);
    assert!(result.is_err());
}

#[test]
fn test_chain_verify_complete() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new(
        "genesis".to_string(),
        vec!["input1".to_string()],
        vec!["output1".to_string()],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("Genesis creation failed");

    let receipt2 = Receipt::new(
        "op2".to_string(),
        vec!["input2".to_string()],
        vec!["output2".to_string()],
        None,
    )
    .chain(&genesis)
    .expect("Chaining failed")
    .sign(&signing_key)
    .expect("Signing failed");

    let receipt3 = Receipt::new(
        "op3".to_string(),
        vec!["input3".to_string()],
        vec!["output3".to_string()],
        None,
    )
    .chain(&receipt2)
    .expect("Chaining failed")
    .sign(&signing_key)
    .expect("Signing failed");

    chain.append(receipt2).expect("Append failed");
    chain.append(receipt3).expect("Append failed");

    assert_eq!(chain.len(), 3);
    assert!(chain.verify(&verifying_key).is_ok());
}

#[test]
fn test_chain_verify_fails_wrong_key() {
    let (signing_key, _) = generate_keypair();
    let (_, wrong_key) = generate_keypair();

    let genesis = Receipt::new(
        "genesis".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let chain = ReceiptChain::from_genesis(genesis).expect("Genesis creation failed");

    assert!(chain.verify(&wrong_key).is_err());
}

#[test]
fn test_chain_receipts_accessor() {
    let (signing_key, _) = generate_keypair();

    let genesis = Receipt::new(
        "op1".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("Genesis creation failed");

    let receipt2 = Receipt::new(
        "op2".to_string(),
        vec![],
        vec![],
        None,
    )
    .chain(&genesis)
    .expect("Chaining failed")
    .sign(&signing_key)
    .expect("Signing failed");

    chain.append(receipt2).expect("Append failed");

    let receipts = chain.receipts();
    assert_eq!(receipts.len(), 2);
    assert_eq!(receipts[0].operation_id, "op1");
    assert_eq!(receipts[1].operation_id, "op2");
}

#[test]
fn test_chain_serialization() {
    let (signing_key, _) = generate_keypair();

    let genesis = Receipt::new(
        "genesis".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let chain = ReceiptChain::from_genesis(genesis).expect("Genesis creation failed");

    let json = serde_json::to_string(&chain).expect("Serialization failed");
    let deserialized: ReceiptChain = serde_json::from_str(&json).expect("Deserialization failed");

    assert_eq!(chain.len(), deserialized.len());
    assert_eq!(
        chain.genesis().unwrap().operation_id,
        deserialized.genesis().unwrap().operation_id
    );
}

#[test]
fn test_empty_chain_verification() {
    let (_, verifying_key) = generate_keypair();

    let chain = ReceiptChain::new();

    // Empty chain should verify successfully
    assert!(chain.verify(&verifying_key).is_ok());
}

#[test]
fn test_chain_append_to_empty_must_be_genesis() {
    let (signing_key, _) = generate_keypair();

    let mut chain = ReceiptChain::new();

    let non_genesis = Receipt::new(
        "op1".to_string(),
        vec![],
        vec![],
        Some("previous".to_string()),
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let result = chain.append(non_genesis);
    assert!(result.is_err());
}

#[test]
fn test_long_chain() {
    let (signing_key, verifying_key) = generate_keypair();

    let genesis = Receipt::new(
        "op0".to_string(),
        vec![],
        vec![],
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let mut chain = ReceiptChain::from_genesis(genesis.clone()).expect("Genesis creation failed");

    let mut prev = genesis;
    for i in 1..100 {
        let receipt = Receipt::new(
            format!("op{}", i),
            vec![format!("input{}", i)],
            vec![format!("output{}", i)],
            None,
        )
        .chain(&prev)
        .expect("Chaining failed")
        .sign(&signing_key)
        .expect("Signing failed");

        chain.append(receipt.clone()).expect("Append failed");
        prev = receipt;
    }

    assert_eq!(chain.len(), 100);
    assert!(chain.verify(&verifying_key).is_ok());
}

#[test]
fn test_chain_with_complex_data() {
    let (signing_key, verifying_key) = generate_keypair();

    let inputs: Vec<String> = (0..10).map(|i| format!("input-{}", i)).collect();
    let outputs: Vec<String> = (0..5).map(|i| format!("output-{}", i)).collect();

    let genesis = Receipt::new(
        "complex-genesis".to_string(),
        inputs.clone(),
        outputs.clone(),
        None,
    )
    .sign(&signing_key)
    .expect("Signing failed");

    let chain = ReceiptChain::from_genesis(genesis).expect("Genesis creation failed");

    assert_eq!(chain.len(), 1);
    assert!(chain.verify(&verifying_key).is_ok());
    assert_eq!(chain.genesis().unwrap().input_hashes.len(), 10);
    assert_eq!(chain.genesis().unwrap().output_hashes.len(), 5);
}

#[test]
fn test_chain_default() {
    let chain = ReceiptChain::default();
    assert!(chain.is_empty());
    assert_eq!(chain.len(), 0);
}
