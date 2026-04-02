//! Compliance framework tests

#[cfg(test)]
mod tests {
    use tai_validation::compliance::{ComplianceFramework, Control, ControlStatus, FrameworkType};

    #[test]
    fn test_fisma_framework_creation() {
        let framework = ComplianceFramework::fisma();
        assert_eq!(framework.framework_type(), FrameworkType::FISMA);
    }

    #[test]
    fn test_fedramp_framework_creation() {
        let framework = ComplianceFramework::fedramp();
        assert_eq!(framework.framework_type(), FrameworkType::FedRAMP);
    }

    #[test]
    fn test_soc2_framework_creation() {
        let framework = ComplianceFramework::soc2_type_ii();
        assert_eq!(framework.framework_type(), FrameworkType::SOC2TypeII);
    }

    #[test]
    fn test_hipaa_framework_creation() {
        let framework = ComplianceFramework::hipaa();
        assert_eq!(framework.framework_type(), FrameworkType::HIPAA);
    }

    #[test]
    fn test_cfr_part_11_framework_creation() {
        let framework = ComplianceFramework::cfr_part_11();
        assert_eq!(framework.framework_type(), FrameworkType::CFRPart11);
    }

    #[test]
    fn test_nist_800_53_framework_creation() {
        let framework = ComplianceFramework::nist_800_53();
        assert_eq!(framework.framework_type(), FrameworkType::NIST80053);
    }

    #[test]
    fn test_dfars_framework_creation() {
        let framework = ComplianceFramework::dfars();
        assert_eq!(framework.framework_type(), FrameworkType::DFARS);
    }

    #[test]
    fn test_control_creation() {
        let control = Control::new(
            "AC-2".to_string(),
            "Account Management".to_string(),
            "Manage information system accounts".to_string(),
        );
        assert_eq!(control.id, "AC-2");
        assert_eq!(control.status, ControlStatus::NonCompliant);
    }

    #[test]
    fn test_control_status_update() {
        let control = Control::new(
            "AC-2".to_string(),
            "Account Management".to_string(),
            "Description".to_string(),
        )
        .with_status(ControlStatus::Compliant);
        assert!(control.is_compliant());
    }

    #[test]
    fn test_control_evidence_collection() {
        let control = Control::new(
            "SI-4".to_string(),
            "System Monitoring".to_string(),
            "Monitor system".to_string(),
        )
        .add_evidence("Log file review completed".to_string());
        assert!(!control.evidence.is_empty());
    }
}
