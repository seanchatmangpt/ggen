// Test to verify security.rs Display implementation doesn't panic with short strings

use ggen_marketplace::security::SignatureReceipt;

#[test]
fn test_signature_receipt_display_with_short_strings() {
    // Create a receipt with very short strings (less than 16 chars)
    let receipt = SignatureReceipt {
        package_identifier: "test-pkg".to_string(),
        signature: "abc".to_string(),
        public_key: "short".to_string(), // Only 5 chars - would panic with [..16]
        signed_at: chrono::Utc::now(),
        data_checksum: "xyz".to_string(), // Only 3 chars - would panic with [..16]
    };

    // This should NOT panic - it should handle short strings gracefully
    let display = format!("{}", receipt);

    // Verify the display output contains the short strings
    assert!(display.contains("test-pkg"));
    assert!(display.contains("short"));
    assert!(display.contains("xyz"));
}

#[test]
fn test_signature_receipt_display_with_long_strings() {
    // Create a receipt with long strings (more than 16 chars)
    let long_key = "a".repeat(64); // 64 chars
    let long_checksum = "b".repeat(64); // 64 chars

    let receipt = SignatureReceipt {
        package_identifier: "test-pkg".to_string(),
        signature: "sig".to_string(),
        public_key: long_key.clone(),
        signed_at: chrono::Utc::now(),
        data_checksum: long_checksum.clone(),
    };

    // This should truncate to 16 chars
    let display = format!("{}", receipt);

    // Verify the display output contains truncated strings
    assert!(display.contains("test-pkg"));
    // Should show first 16 chars of the long key
    assert!(display.contains(&long_key[..16]));
}

#[test]
fn test_signature_receipt_display_with_empty_strings() {
    // Edge case: empty strings
    let receipt = SignatureReceipt {
        package_identifier: "test-pkg".to_string(),
        signature: "".to_string(),
        public_key: "".to_string(),
        signed_at: chrono::Utc::now(),
        data_checksum: "".to_string(),
    };

    // This should NOT panic even with empty strings
    let display = format!("{}", receipt);
    assert!(display.contains("test-pkg"));
}
