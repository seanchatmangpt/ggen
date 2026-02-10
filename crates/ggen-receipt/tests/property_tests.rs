//! Property-based tests for receipt system.

use ggen_receipt::{generate_keypair, hash_data, Receipt, ReceiptChain};
use proptest::prelude::*;

proptest! {
    #[test]
    fn test_receipt_hash_stability(
        operation_id in "\\PC{1,100}",
        input_count in 0usize..20,
        output_count in 0usize..20,
    ) {
        let inputs: Vec<String> = (0..input_count)
            .map(|i| format!("input-{}", i))
            .collect();
        let outputs: Vec<String> = (0..output_count)
            .map(|i| format!("output-{}", i))
            .collect();

        let receipt = Receipt::new(
            operation_id,
            inputs,
            outputs,
            None,
        );

        let hash1 = receipt.hash().expect("Hashing failed");
        let hash2 = receipt.hash().expect("Hashing failed");

        prop_assert_eq!(hash1.len(), 64);
        prop_assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_signature_always_verifies_with_correct_key(
        operation_id in "\\PC{1,100}",
        input_count in 0usize..10,
        output_count in 0usize..10,
    ) {
        let (signing_key, verifying_key) = generate_keypair();

        let inputs: Vec<String> = (0..input_count)
            .map(|i| format!("input-{}", i))
            .collect();
        let outputs: Vec<String> = (0..output_count)
            .map(|i| format!("output-{}", i))
            .collect();

        let receipt = Receipt::new(
            operation_id,
            inputs,
            outputs,
            None,
        )
        .sign(&signing_key)
        .expect("Signing failed");

        prop_assert!(receipt.verify(&verifying_key).is_ok());
    }

    #[test]
    fn test_signature_never_verifies_with_wrong_key(
        operation_id in "\\PC{1,100}",
    ) {
        let (signing_key, _) = generate_keypair();
        let (_, wrong_key) = generate_keypair();

        let receipt = Receipt::new(
            operation_id,
            vec!["input".to_string()],
            vec!["output".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("Signing failed");

        prop_assert!(receipt.verify(&wrong_key).is_err());
    }

    #[test]
    fn test_hash_data_deterministic(data in prop::collection::vec(any::<u8>(), 0..1000)) {
        let hash1 = hash_data(&data);
        let hash2 = hash_data(&data);

        prop_assert_eq!(hash1.len(), 64);
        prop_assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_chain_length_increases_correctly(chain_length in 1usize..20) {
        let (signing_key, verifying_key) = generate_keypair();

        let genesis = Receipt::new(
            "genesis".to_string(),
            vec![],
            vec![],
            None,
        )
        .sign(&signing_key)
        .expect("Signing failed");

        let mut chain = ReceiptChain::from_genesis(genesis.clone())
            .expect("Genesis creation failed");

        let mut prev = genesis;
        for i in 1..chain_length {
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

        prop_assert_eq!(chain.len(), chain_length);
        prop_assert!(chain.verify(&verifying_key).is_ok());
    }

    #[test]
    fn test_serialization_roundtrip(
        operation_id in "\\PC{1,100}",
        input_count in 0usize..10,
        output_count in 0usize..10,
    ) {
        let (signing_key, _) = generate_keypair();

        let inputs: Vec<String> = (0..input_count)
            .map(|i| format!("input-{}", i))
            .collect();
        let outputs: Vec<String> = (0..output_count)
            .map(|i| format!("output-{}", i))
            .collect();

        let receipt = Receipt::new(
            operation_id.clone(),
            inputs.clone(),
            outputs.clone(),
            None,
        )
        .sign(&signing_key)
        .expect("Signing failed");

        let json = serde_json::to_string(&receipt).expect("Serialization failed");
        let deserialized: Receipt = serde_json::from_str(&json)
            .expect("Deserialization failed");

        prop_assert_eq!(receipt.operation_id, deserialized.operation_id);
        prop_assert_eq!(receipt.signature, deserialized.signature);
        prop_assert_eq!(receipt.input_hashes, deserialized.input_hashes);
        prop_assert_eq!(receipt.output_hashes, deserialized.output_hashes);
    }
}
