//! Evidence collection and receipt generation tests

#[cfg(test)]
mod tests {
    use tai_validation::evidence::{EvidenceCollector, FileHash, ValidationReceipt};

    #[test]
    fn test_receipt_generation() {
        let receipt = ValidationReceipt::new();
        assert!(!receipt.receipt_id.is_empty());
        assert!(!receipt.execution_id.is_empty());
    }

    #[test]
    fn test_receipt_with_manifest_hash() {
        let receipt = ValidationReceipt::new().with_manifest_hash("abc123def456".to_string());
        assert_eq!(receipt.manifest_hash, "abc123def456");
    }

    #[test]
    fn test_receipt_with_files() {
        let receipt = ValidationReceipt::new()
            .add_file("src/lib.rs".to_string(), "hash1".to_string())
            .add_file("src/main.rs".to_string(), "hash2".to_string());
        assert_eq!(receipt.files.len(), 2);
    }

    #[test]
    fn test_receipt_serialization() {
        let receipt = ValidationReceipt::new()
            .with_manifest_hash("manifest".to_string())
            .with_duration(5.5)
            .with_rules_executed(42);

        let json = receipt.to_json();
        assert!(json.is_ok());
        let json_str = json.unwrap();
        assert!(json_str.contains("receipt_id"));
        assert!(json_str.contains("manifest"));
    }

    #[test]
    fn test_evidence_collector_basic() {
        let mut collector = EvidenceCollector::new();
        assert_eq!(collector.count(), 0);

        collector.collect(
            "log".to_string(),
            "Test log".to_string(),
            "data".to_string(),
        );
        assert_eq!(collector.count(), 1);
    }

    #[test]
    fn test_evidence_hashing() {
        let hash1 = EvidenceCollector::hash_value("test");
        let hash2 = EvidenceCollector::hash_value("test");
        let hash3 = EvidenceCollector::hash_value("different");

        assert_eq!(hash1, hash2);
        assert_ne!(hash1, hash3);
    }

    #[test]
    fn test_evidence_audit_trail() {
        let mut collector = EvidenceCollector::new();
        collector.collect(
            "log".to_string(),
            "Audit event".to_string(),
            "event data".to_string(),
        );
        collector.collect(
            "config".to_string(),
            "Config check".to_string(),
            "config data".to_string(),
        );

        let trail = collector.generate_audit_trail();
        assert!(trail.is_ok());
        let trail_str = trail.unwrap();
        assert!(trail_str.contains("Audit event"));
        assert!(trail_str.contains("Config check"));
    }

    #[test]
    fn test_file_hash_creation() {
        let fh = FileHash {
            path: "src/main.rs".to_string(),
            hash: "abcdef123456".to_string(),
        };
        assert_eq!(fh.path, "src/main.rs");
        assert_eq!(fh.hash, "abcdef123456");
    }
}
