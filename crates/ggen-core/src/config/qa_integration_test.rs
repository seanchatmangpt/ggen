//! Integration tests for Quality Assurance systems
//! FMEA, POKA-YOKE, MURA, MUDA, ANDON, GEMBA WALK, and Health Monitoring

#[cfg(test)]
mod qa_system_tests {
    use crate::config::{FMEA, PokaYoke, MURA, MUDA, HealthMonitor, Andon, GembaWalk};

    #[test]
    fn test_qa_systems_can_be_instantiated() {
        // Verify all QA systems can be created
        let _fmea = FMEA::new("System".to_string(), 100);
        let _poka_yoke = PokaYoke::new();
        let _mura = MURA::new();
        let _muda = MUDA::new();
        let _health_monitor = HealthMonitor::new();
        let _andon = Andon::new(100);
        let _gemba = GembaWalk::new("Area".to_string());
    }

    #[test]
    fn test_fmea_high_risk_detection() {
        let mut fmea = FMEA::new("System".to_string(), 80);

        fmea.add_failure_mode(crate::config::FailureMode {
            name: "Critical failure".to_string(),
            potential_cause: "Invalid input".to_string(),
            potential_effect: "System crash".to_string(),
            severity: 10,
            occurrence: 10,
            detection: 9,
            rpn: 900,
            preventive_actions: vec!["Validate input".to_string()],
            current_controls: vec!["Schema check".to_string()],
        });

        let high_risks = fmea.high_risk_failures();
        assert_eq!(high_risks.len(), 1);
        assert_eq!(high_risks[0].rpn, 900);
    }

    #[test]
    fn test_poka_yoke_prevention_rules() {
        let mut poka_yoke = PokaYoke::new();

        poka_yoke.add_prevention_rule(crate::config::PreventionRule {
            id: "rule-1".to_string(),
            prevents: "Invalid input".to_string(),
            implementation: "Schema validation".to_string(),
            rule_type: crate::config::PreventionType::Forced,
            enforced: true,
            implementation_cost: "Low".to_string(),
        });

        assert_eq!(poka_yoke.prevention_rules.len(), 1);
    }

    #[test]
    fn test_mura_standards() {
        let mut mura = MURA::new();

        mura.add_standard(crate::config::Standard {
            id: "std-1".to_string(),
            name: "Versioning".to_string(),
            definition: "Use semver".to_string(),
            acceptable_variance: 0.0,
            is_critical: true,
            verification_method: "Parser check".to_string(),
        });

        assert_eq!(mura.standards.len(), 1);
    }

    #[test]
    fn test_mura_consistency_scoring() {
        let mut mura = MURA::new();

        mura.consistency_metrics
            .insert("std-1".to_string(), 95.0);
        mura.consistency_metrics
            .insert("std-2".to_string(), 98.0);

        let score = mura.consistency_score();
        assert!(score > 90.0);
    }

    #[test]
    fn test_mura_violations() {
        let mut mura = MURA::new();

        mura.record_violation(crate::config::MuraViolation {
            standard_id: "std-1".to_string(),
            actual_variance: 50.0,
            severity: crate::config::ViolationSeverity::Medium,
            description: "Missing field".to_string(),
            corrective_action: "Add field".to_string(),
        });

        assert_eq!(mura.violations.len(), 1);
    }

    #[test]
    fn test_health_monitor_check() {
        let health_monitor = HealthMonitor::new();
        let report = health_monitor.comprehensive_health_check();

        assert!(report.checks.len() > 0);
    }
}
