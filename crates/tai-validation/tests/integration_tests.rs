//! Integration tests for tai-validation

#[cfg(test)]
mod tests {
    use tai_validation::slo::SloValidator;
    use tai_validation::slo::metrics::MetricType;
    use tai_validation::compliance::ComplianceFramework;
    use tai_validation::security::SecurityScanner;
    use tai_validation::evidence::{ValidationReceipt, EvidenceCollector};
    use tai_validation::execution::{TestBatch, TestResult};

    #[test]
    fn test_slo_validator_integration() {
        let validator = SloValidator::new();
        let result = validator.validate_metric(MetricType::BuildTime, 12.0);
        assert!(result.is_ok());
        let metrics = result.unwrap();
        assert!(!metrics.is_violating());
    }

    #[test]
    fn test_multiple_metrics() {
        let validator = SloValidator::new();
        let m1 = validator.validate_metric(MetricType::BuildTime, 12.0).unwrap();
        let m2 = validator.validate_metric(MetricType::MemoryUsage, 80.0).unwrap();

        let result = validator.validate_metrics(vec![m1, m2]);
        assert!(result.all_compliant);
    }

    #[test]
    fn test_violation_detection() {
        let validator = SloValidator::new();
        let m1 = validator.validate_metric(MetricType::BuildTime, 20.0).unwrap();
        let m2 = validator.validate_metric(MetricType::MemoryUsage, 80.0).unwrap();

        let result = validator.validate_metrics(vec![m1, m2]);
        assert!(!result.all_compliant);
        assert_eq!(result.violation_count, 1);
    }

    #[test]
    fn test_evidence_collection() {
        let mut collector = EvidenceCollector::new();
        collector.collect("log".to_string(), "Test log".to_string(), "data".to_string());
        assert_eq!(collector.count(), 1);
    }

    #[test]
    fn test_validation_receipt() {
        let receipt = ValidationReceipt::new()
            .with_manifest_hash("hash1".to_string())
            .add_file("file1.rs".to_string(), "hash2".to_string());

        let json = receipt.to_json();
        assert!(json.is_ok());
    }

    #[test]
    fn test_test_batch_creation() {
        let tests = vec!["test1".to_string(), "test2".to_string()];
        let batch = TestBatch::new("batch1".to_string(), tests);
        assert_eq!(batch.test_count(), 2);
    }

    #[test]
    fn test_test_result_aggregation() {
        let results = vec![
            TestResult::passed("test1".to_string(), 1.0),
            TestResult::passed("test2".to_string(), 2.0),
        ];
        let agg = tai_validation::execution::AggregatedResults::from_results(results);
        assert_eq!(agg.passed_tests, 2);
        assert!(agg.all_passed());
    }
}
