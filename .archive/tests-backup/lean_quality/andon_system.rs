// Andon System - Real-time Test Failure Detection
// Implements Lean Manufacturing's Andon (failure alerts) for test monitoring

use std::time::{SystemTime, Duration};
use std::collections::HashMap;
use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Severity {
    Red,      // Critical - Stop pipeline immediately
    Yellow,   // Warning - Monitor next run
    Green,    // Pass - All good
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AndonSignal {
    pub severity: Severity,
    pub test_name: String,
    pub error_message: String,
    pub timestamp: SystemTime,
    pub remediation: String,
    pub category: FailureCategory,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FailureCategory {
    CompilationError,
    TestTimeout,
    FlakyTest,
    MemoryLeak,
    Assertion,
    Panic,
    Performance,
}

impl AndonSignal {
    pub fn red_alert(test: &str, error: &str, category: FailureCategory) -> Self {
        eprintln!("\n🚨 RED ALERT: {} - {}", test, error);
        eprintln!("ACTION: Stop pipeline, fix immediately");
        eprintln!("CATEGORY: {:?}\n", category);

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
        eprintln!("\n⚠️  YELLOW ALERT: {} - {}", test, warning);
        eprintln!("ACTION: Monitor next run, investigate");
        eprintln!("CATEGORY: {:?}\n", category);

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
                "1. Run test 10 times locally\n2. Check for race conditions\n3. Review shared state\n4. Add proper synchronization".to_string()
            },
            FailureCategory::MemoryLeak => {
                "1. Run with valgrind\n2. Check for unclosed resources\n3. Review Drop implementations".to_string()
            },
            FailureCategory::Assertion => {
                "1. Review test expectations\n2. Check actual vs expected values\n3. Verify test setup".to_string()
            },
            FailureCategory::Panic => {
                "1. Review panic message\n2. Check for unwrap() calls\n3. Add proper error handling".to_string()
            },
            FailureCategory::Performance => {
                "1. Profile with criterion\n2. Check algorithmic complexity\n3. Review recent changes".to_string()
            },
        }
    }

    pub fn print_alert(&self) {
        let icon = match self.severity {
            Severity::Red => "🚨",
            Severity::Yellow => "⚠️ ",
            Severity::Green => "✅",
        };

        println!("{} {} - {}", icon, self.test_name, self.error_message);
        if self.severity != Severity::Green {
            println!("REMEDIATION:\n{}", self.remediation);
        }
    }
}

#[derive(Debug, Default)]
pub struct TestHealthDashboard {
    pub total_tests: usize,
    pub passing: usize,
    pub failing: usize,
    pub flaky: usize,
    pub timeout: usize,

    // Alert thresholds
    pub fail_threshold: f32,  // > 5% = RED
    pub flaky_threshold: f32, // > 2% = YELLOW

    // Failure tracking
    pub failures: Vec<AndonSignal>,
    pub test_history: HashMap<String, Vec<bool>>, // test_name -> [pass/fail history]
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

        // Track history for flakiness detection
        self.test_history
            .entry(test_name.to_string())
            .or_insert_with(Vec::new)
            .push(passed);
    }

    pub fn detect_flaky_tests(&mut self) {
        for (test_name, history) in &self.test_history {
            if history.len() >= 3 {
                let passes = history.iter().filter(|&&p| p).count();
                let fails = history.len() - passes;

                // Flaky if both passes and failures exist
                if passes > 0 && fails > 0 {
                    self.flaky += 1;
                    let signal = AndonSignal::yellow_alert(
                        test_name,
                        &format!("Flaky: {} passes, {} fails", passes, fails),
                        FailureCategory::FlakyTest,
                    );
                    self.failures.push(signal);
                }
            }
        }
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

    pub fn print_report(&self) {
        println!("\n📊 TEST HEALTH DASHBOARD");
        println!("═══════════════════════════════════════");
        println!("Total Tests:    {}", self.total_tests);
        println!("✅ Passing:      {} ({:.1}%)",
            self.passing,
            self.passing as f32 / self.total_tests as f32 * 100.0
        );
        println!("❌ Failing:      {} ({:.1}%)",
            self.failing,
            self.failing as f32 / self.total_tests as f32 * 100.0
        );
        println!("⚠️  Flaky:        {} ({:.1}%)",
            self.flaky,
            self.flaky as f32 / self.total_tests as f32 * 100.0
        );
        println!("⏱️  Timeout:      {}", self.timeout);
        println!("═══════════════════════════════════════");

        let status = self.status();
        status.print_alert();

        if !self.failures.is_empty() {
            println!("\n🚨 ACTIVE ALERTS ({}):", self.failures.len());
            for (i, failure) in self.failures.iter().enumerate() {
                println!("\n{}. {}", i + 1, failure.test_name);
                println!("   Category: {:?}", failure.category);
                println!("   Message:  {}", failure.error_message);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_andon_red_alert() {
        let alert = AndonSignal::red_alert(
            "test_compilation",
            "syntax error",
            FailureCategory::CompilationError,
        );

        assert_eq!(alert.severity, Severity::Red);
        assert_eq!(alert.test_name, "test_compilation");
        assert!(alert.remediation.contains("syntax"));
    }

    #[test]
    fn test_andon_yellow_alert() {
        let alert = AndonSignal::yellow_alert(
            "test_flaky",
            "intermittent failure",
            FailureCategory::FlakyTest,
        );

        assert_eq!(alert.severity, Severity::Yellow);
        assert!(alert.remediation.contains("race conditions"));
    }

    #[test]
    fn test_dashboard_all_passing() {
        let mut dashboard = TestHealthDashboard::new();
        dashboard.record_test("test1", true);
        dashboard.record_test("test2", true);
        dashboard.record_test("test3", true);

        let status = dashboard.status();
        assert_eq!(status.severity, Severity::Green);
    }

    #[test]
    fn test_dashboard_high_failure_rate() {
        let mut dashboard = TestHealthDashboard::new();
        dashboard.record_test("test1", false);
        dashboard.record_test("test2", false);
        dashboard.record_test("test3", true);

        let status = dashboard.status();
        assert_eq!(status.severity, Severity::Red);
    }

    #[test]
    fn test_flaky_detection() {
        let mut dashboard = TestHealthDashboard::new();

        // Simulate flaky test
        dashboard.record_test("flaky_test", true);
        dashboard.record_test("flaky_test", false);
        dashboard.record_test("flaky_test", true);

        dashboard.detect_flaky_tests();

        assert!(dashboard.flaky > 0);
        assert!(!dashboard.failures.is_empty());
    }
}
