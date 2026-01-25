//! Security scanning tests

#[cfg(test)]
mod tests {
    use tai_validation::security::{SecurityScanner, ScanType};

    #[test]
    fn test_sast_scanner_creation() {
        let scanner = SecurityScanner::sast();
        assert_eq!(scanner.scan_type(), ScanType::SAST);
    }

    #[test]
    fn test_dast_scanner_creation() {
        let scanner = SecurityScanner::dast();
        assert_eq!(scanner.scan_type(), ScanType::DAST);
    }

    #[test]
    fn test_dependency_scanner_creation() {
        let scanner = SecurityScanner::dependency_scan();
        assert_eq!(scanner.scan_type(), ScanType::DependencyScan);
    }

    #[test]
    fn test_sbom_generator_creation() {
        let scanner = SecurityScanner::sbom();
        assert_eq!(scanner.scan_type(), ScanType::SBOM);
    }

    #[tokio::test]
    async fn test_scanner_execution() {
        let scanner = SecurityScanner::sast();
        let result = scanner.scan().await;
        assert!(result.is_ok());
        let scan_result = result.unwrap();
        assert!(scan_result.is_compliant());
    }
}
