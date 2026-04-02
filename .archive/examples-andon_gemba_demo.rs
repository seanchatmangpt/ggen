// Demonstration of Andon + Gemba Walk System
// Run with: cargo run --example andon_gemba_demo

use std::collections::HashMap;
use std::time::SystemTime;

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
    pub fail_threshold: f32,
    pub flaky_threshold: f32,
    pub failures: Vec<AndonSignal>,
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
            .or_default()
            .push(passed);
    }

    pub fn detect_flaky_tests(&mut self) {
        for (test_name, history) in &self.test_history {
            if history.len() >= 3 {
                let passes = history.iter().filter(|&&p| p).count();
                let fails = history.len() - passes;

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
            (fail, _) if fail > self.fail_threshold => AndonSignal::red_alert(
                "Test suite",
                &format!("{:.1}% tests failing", fail * 100.0),
                FailureCategory::Assertion,
            ),
            (_, flaky) if flaky > self.flaky_threshold => AndonSignal::yellow_alert(
                "Test suite",
                &format!("{:.1}% tests flaky", flaky * 100.0),
                FailureCategory::FlakyTest,
            ),
            _ => AndonSignal::green_ok("All tests passing"),
        }
    }

    pub fn print_report(&self) {
        println!("\n📊 TEST HEALTH DASHBOARD");
        println!("═══════════════════════════════════════");
        println!("Total Tests:    {}", self.total_tests);
        println!(
            "✅ Passing:      {} ({:.1}%)",
            self.passing,
            self.passing as f32 / self.total_tests as f32 * 100.0
        );
        println!(
            "❌ Failing:      {} ({:.1}%)",
            self.failing,
            self.failing as f32 / self.total_tests as f32 * 100.0
        );
        println!(
            "⚠️  Flaky:        {} ({:.1}%)",
            self.flaky,
            self.flaky as f32 / self.total_tests as f32 * 100.0
        );
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

fn main() {
    println!("═══════════════════════════════════════");
    println!("🚨 ANDON + GEMBA WALK DEMONSTRATION");
    println!("═══════════════════════════════════════\n");

    // Scenario 1: All tests passing (Green)
    println!("SCENARIO 1: All Tests Passing");
    println!("---");
    let mut dashboard = TestHealthDashboard::new();
    dashboard.record_test("test_basic_functionality", true);
    dashboard.record_test("test_edge_cases", true);
    dashboard.record_test("test_error_handling", true);
    dashboard.record_test("test_performance", true);
    dashboard.print_report();

    // Scenario 2: High failure rate (Red Alert)
    println!("\n\nSCENARIO 2: High Failure Rate");
    println!("---");
    let mut dashboard = TestHealthDashboard::new();
    dashboard.record_test("test_basic_functionality", false);
    dashboard.record_test("test_edge_cases", false);
    dashboard.record_test("test_error_handling", true);
    dashboard.print_report();

    // Scenario 3: Flaky tests (Yellow Alert)
    println!("\n\nSCENARIO 3: Flaky Test Detection");
    println!("---");
    let mut dashboard = TestHealthDashboard::new();
    dashboard.record_test("stable_test", true);
    dashboard.record_test("flaky_test", true);
    dashboard.record_test("flaky_test", false);
    dashboard.record_test("flaky_test", true);
    dashboard.record_test("stable_test", true);
    dashboard.detect_flaky_tests();
    dashboard.print_report();

    // Scenario 4: Individual alert types
    println!("\n\nSCENARIO 4: Individual Alert Types");
    println!("---");

    let compilation_alert = AndonSignal::red_alert(
        "test_compilation",
        "syntax error in src/main.rs line 42",
        FailureCategory::CompilationError,
    );
    compilation_alert.print_alert();

    let timeout_alert = AndonSignal::red_alert(
        "test_slow_operation",
        "test exceeded 30s timeout",
        FailureCategory::TestTimeout,
    );
    timeout_alert.print_alert();

    let memory_alert = AndonSignal::red_alert(
        "test_resource_cleanup",
        "memory leak detected: 1024 bytes lost",
        FailureCategory::MemoryLeak,
    );
    memory_alert.print_alert();

    // Gemba Walk demonstration
    println!("\n\n═══════════════════════════════════════");
    println!("🚶 GEMBA WALK CHECKLIST EXAMPLE");
    println!("═══════════════════════════════════════\n");

    println!("Inspecting: tests/integration/lifecycle_simple_tests.rs");
    println!("---\n");

    println!("1. ✅ Real Implementation Usage:");
    println!("   Observation: Minimal mocking (2 occurrences)");
    println!("   Weight: 15.0\n");

    println!("2. ✅ Assertion Clarity:");
    println!("   Observation: Good use of descriptive assertions");
    println!("   Weight: 15.0\n");

    println!("3. ⚠️  Bug Detection:");
    println!("   Observation: May be too happy-path focused");
    println!("   Weight: 20.0");
    println!("   Recommendation: Add edge case tests\n");

    println!("4. ✅ Setup/Teardown:");
    println!("   Observation: Explicit setup functions found");
    println!("   Weight: 10.0\n");

    println!("5. ⚠️  Debug-ability:");
    println!("   Observation: Add custom error messages to assertions");
    println!("   Weight: 15.0\n");

    println!("6. ✅ Performance:");
    println!("   Observation: Fast tests (2s < 30s threshold)");
    println!("   Weight: 10.0\n");

    println!("7. ✅ Isolation:");
    println!("   Observation: No obvious shared state");
    println!("   Weight: 10.0\n");

    println!("8. ✅ Reproducibility:");
    println!("   Observation: Reproducible results (3/3 passes)");
    println!("   Weight: 5.0\n");

    println!("═══════════════════════════════════════");
    println!("Overall Score: 75.0%");
    println!("Recommendation: Good - Some improvements recommended");
    println!("═══════════════════════════════════════\n");

    println!("\n✅ Demo complete! Check scripts/andon_monitor.sh and scripts/gemba_walk.sh for automation.");
    println!("📋 See docs/lean_quality/ANDON_GEMBA_PLAYBOOK.md for full playbook.\n");
}
