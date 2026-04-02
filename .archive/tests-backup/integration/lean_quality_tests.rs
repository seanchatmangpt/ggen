// Integration tests for Lean Quality Systems (Andon + Gemba Walk)

mod andon_system {
    use std::time::SystemTime;
    use std::collections::HashMap;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum Severity {
        Red,
        Yellow,
        Green,
    }

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum FailureCategory {
        CompilationError,
        TestTimeout,
        FlakyTest,
        MemoryLeak,
        Assertion,
        Panic,
        Performance,
    }

    #[derive(Debug, Clone)]
    pub struct AndonSignal {
        pub severity: Severity,
        pub test_name: String,
        pub error_message: String,
        pub timestamp: SystemTime,
        pub remediation: String,
        pub category: FailureCategory,
    }

    impl AndonSignal {
        pub fn red_alert(test: &str, error: &str, category: FailureCategory) -> Self {
            Self {
                severity: Severity::Red,
                test_name: test.to_string(),
                error_message: error.to_string(),
                timestamp: SystemTime::now(),
                remediation: Self::get_remediation(category),
                category,
            }
        }

        pub fn yellow_alert(test: &str, warning: &str, category: FailureCategory) -> Self {
            Self {
                severity: Severity::Yellow,
                test_name: test.to_string(),
                error_message: warning.to_string(),
                timestamp: SystemTime::now(),
                remediation: Self::get_remediation(category),
                category,
            }
        }

        pub fn green_ok(test: &str) -> Self {
            Self {
                severity: Severity::Green,
                test_name: test.to_string(),
                error_message: String::new(),
                timestamp: SystemTime::now(),
                remediation: "Continue normal operations".to_string(),
                category: FailureCategory::Assertion,
            }
        }

        fn get_remediation(category: FailureCategory) -> String {
            match category {
                FailureCategory::CompilationError => {
                    "1. Check syntax errors\n2. Verify dependencies\n3. Review recent changes".to_string()
                },
                FailureCategory::TestTimeout => {
                    "1. Check for infinite loops\n2. Review async operations\n3. Increase timeout if justified".to_string()
                },
                FailureCategory::FlakyTest => {
                    "1. Run test 10 times locally\n2. Check for race conditions\n3. Review shared state".to_string()
                },
                _ => "General remediation needed".to_string(),
            }
        }
    }

    #[derive(Debug, Default)]
    pub struct TestHealthDashboard {
        pub total_tests: usize,
        pub passing: usize,
        pub failing: usize,
        pub flaky: usize,
        pub fail_threshold: f32,
        pub flaky_threshold: f32,
        pub test_history: HashMap<String, Vec<bool>>,
    }

    impl TestHealthDashboard {
        pub fn new() -> Self {
            Self {
                fail_threshold: 0.05,
                flaky_threshold: 0.02,
                ..Default::default()
            }
        }

        pub fn record_test(&mut self, test_name: &str, passed: bool) {
            self.total_tests += 1;
            if passed {
                self.passing += 1;
            } else {
                self.failing += 1;
            }

            self.test_history
                .entry(test_name.to_string())
                .or_insert_with(Vec::new)
                .push(passed);
        }

        pub fn status(&self) -> AndonSignal {
            if self.total_tests == 0 {
                return AndonSignal::green_ok("No tests run");
            }

            let fail_rate = self.failing as f32 / self.total_tests as f32;
            let flaky_rate = self.flaky as f32 / self.total_tests as f32;

            match (fail_rate, flaky_rate) {
                (fail, _) if fail > self.fail_threshold => {
                    AndonSignal::red_alert(
                        "Test suite",
                        &format!("{:.1}% tests failing", fail * 100.0),
                        FailureCategory::Assertion,
                    )
                },
                (_, flaky) if flaky > self.flaky_threshold => {
                    AndonSignal::yellow_alert(
                        "Test suite",
                        &format!("{:.1}% tests flaky", flaky * 100.0),
                        FailureCategory::FlakyTest,
                    )
                },
                _ => AndonSignal::green_ok("All tests passing"),
            }
        }
    }

    #[test]
    fn test_andon_red_alert_creation() {
        let alert = AndonSignal::red_alert(
            "test_compilation",
            "syntax error in main.rs",
            FailureCategory::CompilationError,
        );

        assert_eq!(alert.severity, Severity::Red);
        assert_eq!(alert.test_name, "test_compilation");
        assert_eq!(alert.error_message, "syntax error in main.rs");
        assert!(alert.remediation.contains("syntax"));
    }

    #[test]
    fn test_andon_yellow_alert_creation() {
        let alert = AndonSignal::yellow_alert(
            "test_flaky",
            "intermittent failure detected",
            FailureCategory::FlakyTest,
        );

        assert_eq!(alert.severity, Severity::Yellow);
        assert_eq!(alert.test_name, "test_flaky");
        assert!(alert.remediation.contains("race conditions"));
    }

    #[test]
    fn test_andon_green_signal() {
        let signal = AndonSignal::green_ok("test_success");

        assert_eq!(signal.severity, Severity::Green);
        assert_eq!(signal.test_name, "test_success");
        assert_eq!(signal.error_message, "");
    }

    #[test]
    fn test_dashboard_all_passing() {
        let mut dashboard = TestHealthDashboard::new();
        dashboard.record_test("test1", true);
        dashboard.record_test("test2", true);
        dashboard.record_test("test3", true);

        let status = dashboard.status();
        assert_eq!(status.severity, Severity::Green);
        assert_eq!(dashboard.passing, 3);
        assert_eq!(dashboard.failing, 0);
    }

    #[test]
    fn test_dashboard_high_failure_rate() {
        let mut dashboard = TestHealthDashboard::new();
        dashboard.record_test("test1", false);
        dashboard.record_test("test2", false);
        dashboard.record_test("test3", true);

        let status = dashboard.status();
        assert_eq!(status.severity, Severity::Red);
        assert_eq!(dashboard.failing, 2);
        assert_eq!(dashboard.passing, 1);
    }

    #[test]
    fn test_dashboard_moderate_failure_rate() {
        let mut dashboard = TestHealthDashboard::new();
        dashboard.record_test("test1", true);
        dashboard.record_test("test2", true);
        dashboard.record_test("test3", true);
        dashboard.record_test("test4", true);
        dashboard.record_test("test5", false);

        let status = dashboard.status();
        // 20% failure rate is below 5% threshold, should be green
        assert_eq!(status.severity, Severity::Red); // Actually > 5% (1/5 = 20%)
    }

    #[test]
    fn test_test_history_tracking() {
        let mut dashboard = TestHealthDashboard::new();

        dashboard.record_test("flaky_test", true);
        dashboard.record_test("flaky_test", false);
        dashboard.record_test("flaky_test", true);

        let history = dashboard.test_history.get("flaky_test").unwrap();
        assert_eq!(history.len(), 3);
        assert_eq!(history, &vec![true, false, true]);
    }

    #[test]
    fn test_failure_category_remediation() {
        let timeout_alert = AndonSignal::red_alert(
            "slow_test",
            "timeout",
            FailureCategory::TestTimeout,
        );
        assert!(timeout_alert.remediation.contains("infinite loops"));

        let flaky_alert = AndonSignal::yellow_alert(
            "unstable_test",
            "flaky",
            FailureCategory::FlakyTest,
        );
        assert!(flaky_alert.remediation.contains("race conditions"));
    }
}

mod gemba_walk {
    use std::path::PathBuf;

    #[derive(Debug, Clone)]
    pub struct GembaCheck {
        pub name: String,
        pub passed: bool,
        pub observation: String,
        pub weight: f32,
    }

    #[derive(Debug, Clone)]
    pub struct GembaWalkChecklist {
        pub test_file: PathBuf,
        pub checks: Vec<GembaCheck>,
        pub score: f32,
        pub findings: Vec<String>,
    }

    impl GembaWalkChecklist {
        pub fn new(test_file: PathBuf) -> Self {
            Self {
                test_file,
                checks: Vec::new(),
                score: 0.0,
                findings: Vec::new(),
            }
        }

        pub fn add_check(&mut self, name: &str, passed: bool, observation: &str, weight: f32) {
            self.checks.push(GembaCheck {
                name: name.to_string(),
                passed,
                observation: observation.to_string(),
                weight,
            });
        }

        pub fn calculate_score(&mut self) {
            let total_weight: f32 = self.checks.iter().map(|c| c.weight).sum();
            let passed_weight: f32 = self.checks
                .iter()
                .filter(|c| c.passed)
                .map(|c| c.weight)
                .sum();

            self.score = if total_weight > 0.0 {
                (passed_weight / total_weight) * 100.0
            } else {
                0.0
            };
        }
    }

    #[test]
    fn test_gemba_checklist_creation() {
        let checklist = GembaWalkChecklist::new(PathBuf::from("test.rs"));

        assert_eq!(checklist.test_file, PathBuf::from("test.rs"));
        assert_eq!(checklist.checks.len(), 0);
        assert_eq!(checklist.score, 0.0);
    }

    #[test]
    fn test_gemba_add_checks() {
        let mut checklist = GembaWalkChecklist::new(PathBuf::from("test.rs"));

        checklist.add_check("Mock usage", true, "Minimal mocking", 10.0);
        checklist.add_check("Assertions", false, "Needs improvement", 10.0);

        assert_eq!(checklist.checks.len(), 2);
        assert_eq!(checklist.checks[0].name, "Mock usage");
        assert!(checklist.checks[0].passed);
        assert!(!checklist.checks[1].passed);
    }

    #[test]
    fn test_gemba_score_calculation() {
        let mut checklist = GembaWalkChecklist::new(PathBuf::from("test.rs"));

        checklist.add_check("Check 1", true, "Good", 10.0);
        checklist.add_check("Check 2", false, "Bad", 10.0);
        checklist.calculate_score();

        assert_eq!(checklist.score, 50.0);
    }

    #[test]
    fn test_gemba_weighted_scoring() {
        let mut checklist = GembaWalkChecklist::new(PathBuf::from("test.rs"));

        checklist.add_check("High priority", true, "Good", 20.0);
        checklist.add_check("Low priority", false, "Bad", 5.0);
        checklist.calculate_score();

        // 20 / (20 + 5) = 80%
        assert!((checklist.score - 80.0).abs() < 0.1);
    }

    #[test]
    fn test_gemba_perfect_score() {
        let mut checklist = GembaWalkChecklist::new(PathBuf::from("test.rs"));

        checklist.add_check("Check 1", true, "Perfect", 10.0);
        checklist.add_check("Check 2", true, "Perfect", 10.0);
        checklist.add_check("Check 3", true, "Perfect", 10.0);
        checklist.calculate_score();

        assert_eq!(checklist.score, 100.0);
    }

    #[test]
    fn test_gemba_failing_score() {
        let mut checklist = GembaWalkChecklist::new(PathBuf::from("test.rs"));

        checklist.add_check("Check 1", false, "Failed", 10.0);
        checklist.add_check("Check 2", false, "Failed", 10.0);
        checklist.calculate_score();

        assert_eq!(checklist.score, 0.0);
    }
}

#[test]
fn test_andon_gemba_integration() {
    // Simulate a complete workflow
    let mut dashboard = andon_system::TestHealthDashboard::new();
    let mut gemba = gemba_walk::GembaWalkChecklist::new(
        PathBuf::from("tests/integration/lifecycle_simple_tests.rs")
    );

    // Record some test results
    dashboard.record_test("test_1", true);
    dashboard.record_test("test_2", true);
    dashboard.record_test("test_3", false);

    // Add gemba checks
    gemba.add_check("Real implementations", true, "Good", 15.0);
    gemba.add_check("Clear assertions", true, "Good", 15.0);
    gemba.add_check("Bug detection", false, "Needs edge cases", 20.0);
    gemba.calculate_score();

    // Verify results
    let andon_status = dashboard.status();
    assert_eq!(andon_status.severity, andon_system::Severity::Red); // 33% failure

    // Gemba score: (15 + 15) / (15 + 15 + 20) = 60%
    assert!((gemba.score - 60.0).abs() < 0.1);
}

use std::path::PathBuf;
