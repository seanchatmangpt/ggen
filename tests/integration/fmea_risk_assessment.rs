//! FMEA Risk Assessment Tests
//!
//! These tests verify the achievability of FMEA (Failure Mode and Effects Analysis)
//! risk calculations as documented in the Diataxis how-to guides.
//!
//! RPN (Risk Priority Number) = Severity × Occurrence × Detection
//!
//! Severity Scale (1-10):
//! - 9-10: Critical (system failure, data loss)
//! - 7-8: High (major functionality broken)
//! - 4-6: Medium (degraded functionality)
//! - 1-3: Low (minor issues)
//!
//! Occurrence Scale (1-10):
//! - 9-10: Very High (>50% of cases)
//! - 7-8: High (25-50% of cases)
//! - 4-6: Medium (5-25% of cases)
//! - 1-3: Low (<5% of cases)
//!
//! Detection Scale (1-10):
//! - 9-10: Very Low (hard to detect)
//! - 7-8: Low (requires careful testing)
//! - 4-6: Medium (standard testing finds it)
//! - 1-3: High (obvious, caught immediately)

use std::cmp::Ordering;
use std::collections::HashMap;

/// FMEA failure mode representation
#[derive(Debug, Clone, PartialEq, Eq)]
struct FailureMode {
    error_code: &'static str,
    description: &'static str,
    severity: u8,      // 1-10
    occurrence: u8,    // 1-10
    detection: u8,     // 1-10
}

impl FailureMode {
    /// Calculate Risk Priority Number (RPN)
    fn rpn(&self) -> u32 {
        (self.severity as u32) * (self.occurrence as u32) * (self.detection as u32)
    }

    /// Get risk level based on RPN thresholds
    fn risk_level(&self) -> RiskLevel {
        match self.rpn() {
            0..=99 => RiskLevel::Low,
            100..=299 => RiskLevel::Medium,
            300..=499 => RiskLevel::High,
            _ => RiskLevel::Critical,
        }
    }
}

/// Risk level classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum RiskLevel {
    Low,
    Medium,
    High,
    Critical,
}

impl RiskLevel {
    fn as_str(&self) -> &'static str {
        match self {
            RiskLevel::Low => "LOW",
            RiskLevel::Medium => "MEDIUM",
            RiskLevel::High => "HIGH",
            RiskLevel::Critical => "CRITICAL",
        }
    }
}

/// FMEA assessment result
#[derive(Debug, Clone)]
struct FmeaAssessment {
    failure_modes: Vec<FailureMode>,
    total_rpn: u32,
}

impl FmeaAssessment {
    fn new(failure_modes: Vec<FailureMode>) -> Self {
        let total_rpn = failure_modes.iter().map(|fm| fm.rpn()).sum();
        FmeaAssessment {
            failure_modes,
            total_rpn,
        }
    }

    /// Get failures sorted by RPN (highest first)
    fn by_priority(&self) -> Vec<&FailureMode> {
        let mut sorted: Vec<_> = self.failure_modes.iter().collect();
        sorted.sort_by(|a, b| b.rpn().cmp(&a.rpn()));
        sorted
    }

    /// Calculate RPN distribution
    fn rpn_distribution(&self) -> HashMap<RiskLevel, usize> {
        let mut dist = HashMap::new();
        for fm in &self.failure_modes {
            *dist.entry(fm.risk_level()).or_insert(0) += 1;
        }
        dist
    }

    /// Get average RPN
    fn average_rpn(&self) -> f64 {
        if self.failure_modes.is_empty() {
            0.0
        } else {
            self.total_rpn as f64 / self.failure_modes.len() as f64
        }
    }
}

/// Post-fix effectiveness measurement
#[derive(Debug)]
struct FixEffectiveness {
    before_rpn: u32,
    after_rpn: u32,
    reduction_percent: f64,
}

impl FixEffectiveness {
    fn new(before: &FailureMode, after: &FailureMode) -> Self {
        let before_rpn = before.rpn();
        let after_rpn = after.rpn();
        let reduction_percent = if before_rpn > 0 {
            ((before_rpn - after_rpn) as f64 / before_rpn as f64) * 100.0
        } else {
            0.0
        };

        FixEffectiveness {
            before_rpn,
            after_rpn,
            reduction_percent,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Test E0277: Trait Bound Not Satisfied
    ///
    /// RPN = Severity(9) × Occurrence(8) × Detection(10) = 720
    ///
    /// This is a CRITICAL risk failure mode because:
    /// - Severity 9: Compilation fails completely
    /// - Occurrence 8: Common when using generic types
    /// - Detection 10: Only caught at compile time
    #[test]
    fn test_e0277_error_rpn() {
        println!("\n=== Test E0277: Trait Bound Not Satisfied ===");

        let failure = FailureMode {
            error_code: "E0277",
            description: "the trait bound is not satisfied",
            severity: 9,
            occurrence: 8,
            detection: 10,
        };

        let rpn = failure.rpn();
        let expected_rpn = 720;

        println!("Error Code: {}", failure.error_code);
        println!("Description: {}", failure.description);
        println!("Severity: {} (compilation fails)", failure.severity);
        println!("Occurrence: {} (common with generics)", failure.occurrence);
        println!("Detection: {} (compile-time only)", failure.detection);
        println!("RPN: {} (expected {})", rpn, expected_rpn);
        println!("Risk Level: {}", failure.risk_level().as_str());

        assert_eq!(rpn, expected_rpn, "E0277 RPN calculation incorrect");
        assert_eq!(failure.risk_level(), RiskLevel::Critical);
    }

    /// Test E0308: Mismatched Types
    ///
    /// RPN = Severity(8) × Occurrence(7) × Detection(9) = 504
    ///
    /// This is a CRITICAL risk failure mode because:
    /// - Severity 8: Compilation fails
    /// - Occurrence 7: Common with type coercions
    /// - Detection 9: Requires type checking
    #[test]
    fn test_e0308_error_rpn() {
        println!("\n=== Test E0308: Mismatched Types ===");

        let failure = FailureMode {
            error_code: "E0308",
            description: "mismatched types",
            severity: 8,
            occurrence: 7,
            detection: 9,
        };

        let rpn = failure.rpn();
        let expected_rpn = 504;

        println!("Error Code: {}", failure.error_code);
        println!("Description: {}", failure.description);
        println!("Severity: {} (compilation fails)", failure.severity);
        println!("Occurrence: {} (common with coercions)", failure.occurrence);
        println!("Detection: {} (requires type checking)", failure.detection);
        println!("RPN: {} (expected {})", rpn, expected_rpn);
        println!("Risk Level: {}", failure.risk_level().as_str());

        assert_eq!(rpn, expected_rpn, "E0308 RPN calculation incorrect");
        assert_eq!(failure.risk_level(), RiskLevel::Critical);
    }

    /// Test E0283: Type Annotations Needed
    ///
    /// RPN = Severity(7) × Occurrence(6) × Detection(8) = 336
    ///
    /// This is a HIGH risk failure mode because:
    /// - Severity 7: Compilation blocked
    /// - Occurrence 6: Moderate with type inference
    /// - Detection 8: Needs careful review
    #[test]
    fn test_e0283_error_rpn() {
        println!("\n=== Test E0283: Type Annotations Needed ===");

        let failure = FailureMode {
            error_code: "E0283",
            description: "type annotations needed",
            severity: 7,
            occurrence: 6,
            detection: 8,
        };

        let rpn = failure.rpn();
        let expected_rpn = 336;

        println!("Error Code: {}", failure.error_code);
        println!("Description: {}", failure.description);
        println!("Severity: {} (compilation blocked)", failure.severity);
        println!("Occurrence: {} (moderate with inference)", failure.occurrence);
        println!("Detection: {} (needs careful review)", failure.detection);
        println!("RPN: {} (expected {})", rpn, expected_rpn);
        println!("Risk Level: {}", failure.risk_level().as_str());

        assert_eq!(rpn, expected_rpn, "E0283 RPN calculation incorrect");
        assert_eq!(failure.risk_level(), RiskLevel::High);
    }

    /// Test E0599: Method Not Found
    ///
    /// RPN = Severity(9) × Occurrence(9) × Detection(10) = 810
    ///
    /// This is the HIGHEST RISK failure mode because:
    /// - Severity 9: Compilation fails completely
    /// - Occurrence 9: Very common with trait implementations
    /// - Detection 10: Hard to detect root cause
    #[test]
    fn test_e0599_error_rpn() {
        println!("\n=== Test E0599: Method Not Found ===");

        let failure = FailureMode {
            error_code: "E0599",
            description: "no method named found for type",
            severity: 9,
            occurrence: 9,
            detection: 10,
        };

        let rpn = failure.rpn();
        let expected_rpn = 810;

        println!("Error Code: {}", failure.error_code);
        println!("Description: {}", failure.description);
        println!("Severity: {} (compilation fails)", failure.severity);
        println!("Occurrence: {} (very common with traits)", failure.occurrence);
        println!("Detection: {} (hard to detect root cause)", failure.detection);
        println!("RPN: {} (expected {})", rpn, expected_rpn);
        println!("Risk Level: {}", failure.risk_level().as_str());

        assert_eq!(rpn, expected_rpn, "E0599 RPN calculation incorrect");
        assert_eq!(failure.risk_level(), RiskLevel::Critical);
    }

    /// Test RPN Distribution Calculation
    ///
    /// Verifies that failure modes are correctly classified by risk level
    /// and that distribution statistics are accurate.
    #[test]
    fn test_rpn_distribution() {
        println!("\n=== Test RPN Distribution ===");

        let failures = vec![
            FailureMode {
                error_code: "E0277",
                description: "trait bound not satisfied",
                severity: 9,
                occurrence: 8,
                detection: 10,
            },
            FailureMode {
                error_code: "E0308",
                description: "mismatched types",
                severity: 8,
                occurrence: 7,
                detection: 9,
            },
            FailureMode {
                error_code: "E0283",
                description: "type annotations needed",
                severity: 7,
                occurrence: 6,
                detection: 8,
            },
            FailureMode {
                error_code: "E0599",
                description: "method not found",
                severity: 9,
                occurrence: 9,
                detection: 10,
            },
        ];

        let assessment = FmeaAssessment::new(failures);
        let distribution = assessment.rpn_distribution();

        println!("\nRPN Distribution:");
        for level in &[RiskLevel::Low, RiskLevel::Medium, RiskLevel::High, RiskLevel::Critical] {
            let count = distribution.get(level).unwrap_or(&0);
            println!("  {}: {} failures", level.as_str(), count);
        }

        println!("\nTotal RPN: {}", assessment.total_rpn);
        println!("Average RPN: {:.1}", assessment.average_rpn());

        // Verify distribution
        assert_eq!(*distribution.get(&RiskLevel::Critical).unwrap_or(&0), 3);
        assert_eq!(*distribution.get(&RiskLevel::High).unwrap_or(&0), 1);

        // Verify we have 4 failures total
        let total_failures: usize = distribution.values().sum();
        assert_eq!(total_failures, 4, "Distribution count mismatch");
    }

    /// Test Priority Ranking
    ///
    /// Verifies that failure modes are correctly sorted by RPN
    /// with highest risk first (for remediation prioritization).
    #[test]
    fn test_priority_ranking() {
        println!("\n=== Test Priority Ranking ===");

        let failures = vec![
            FailureMode {
                error_code: "E0283",
                description: "type annotations needed",
                severity: 7,
                occurrence: 6,
                detection: 8,
            },
            FailureMode {
                error_code: "E0599",
                description: "method not found",
                severity: 9,
                occurrence: 9,
                detection: 10,
            },
            FailureMode {
                error_code: "E0308",
                description: "mismatched types",
                severity: 8,
                occurrence: 7,
                detection: 9,
            },
            FailureMode {
                error_code: "E0277",
                description: "trait bound not satisfied",
                severity: 9,
                occurrence: 8,
                detection: 10,
            },
        ];

        let assessment = FmeaAssessment::new(failures);
        let prioritized = assessment.by_priority();

        println!("\nFailures by Priority (Highest RPN First):");
        for (i, failure) in prioritized.iter().enumerate() {
            println!(
                "  {}. {} - RPN {} ({})",
                i + 1,
                failure.error_code,
                failure.rpn(),
                failure.risk_level().as_str()
            );
        }

        // Verify highest priority is E0599
        assert_eq!(prioritized[0].error_code, "E0599");
        assert_eq!(prioritized[0].rpn(), 810);

        // Verify second priority is E0277
        assert_eq!(prioritized[1].error_code, "E0277");
        assert_eq!(prioritized[1].rpn(), 720);

        // Verify third priority is E0308
        assert_eq!(prioritized[2].error_code, "E0308");
        assert_eq!(prioritized[2].rpn(), 504);

        // Verify lowest priority is E0283
        assert_eq!(prioritized[3].error_code, "E0283");
        assert_eq!(prioritized[3].rpn(), 336);

        // Verify sorted order
        for i in 1..prioritized.len() {
            assert!(
                prioritized[i - 1].rpn() >= prioritized[i].rpn(),
                "Priorities not correctly sorted"
            );
        }
    }

    /// Test Fix Effectiveness
    ///
    /// Verifies that post-fix RPN reduction is correctly calculated
    /// to measure the effectiveness of remediation actions.
    #[test]
    fn test_fix_effectiveness() {
        println!("\n=== Test Fix Effectiveness ===");

        // Before fix: E0277 with high occurrence
        let before = FailureMode {
            error_code: "E0277",
            description: "trait bound not satisfied",
            severity: 9,
            occurrence: 8,
            detection: 10,
        };

        // After fix: Add trait bound to generic - reduces occurrence to 2
        let after = FailureMode {
            error_code: "E0277",
            description: "trait bound not satisfied",
            severity: 9,
            occurrence: 2,  // Much less likely with proper bounds
            detection: 10,
        };

        let effectiveness = FixEffectiveness::new(&before, &after);

        println!("\nE0277 Remediation:");
        println!("  Before RPN: {}", effectiveness.before_rpn);
        println!("  After RPN:  {}", effectiveness.after_rpn);
        println!("  Reduction:  {:.1}%", effectiveness.reduction_percent);

        // Verify calculations
        assert_eq!(effectiveness.before_rpn, 720);
        assert_eq!(effectiveness.after_rpn, 180);
        assert!((effectiveness.reduction_percent - 75.0).abs() < 0.1);

        // Verify fix moved it from Critical to Medium
        assert_eq!(before.risk_level(), RiskLevel::Critical);
        assert_eq!(after.risk_level(), RiskLevel::Medium);
    }

    /// Test Complete FMEA Assessment
    ///
    /// Demonstrates a complete FMEA workflow:
    /// 1. Identify all failure modes
    /// 2. Calculate RPNs
    /// 3. Prioritize by risk
    /// 4. Apply fixes
    /// 5. Measure effectiveness
    #[test]
    fn test_complete_fmea_workflow() {
        println!("\n=== Complete FMEA Assessment ===");

        // Step 1: Initial assessment
        let initial_failures = vec![
            FailureMode {
                error_code: "E0277",
                description: "trait bound not satisfied",
                severity: 9,
                occurrence: 8,
                detection: 10,
            },
            FailureMode {
                error_code: "E0308",
                description: "mismatched types",
                severity: 8,
                occurrence: 7,
                detection: 9,
            },
            FailureMode {
                error_code: "E0283",
                description: "type annotations needed",
                severity: 7,
                occurrence: 6,
                detection: 8,
            },
            FailureMode {
                error_code: "E0599",
                description: "method not found",
                severity: 9,
                occurrence: 9,
                detection: 10,
            },
        ];

        let initial = FmeaAssessment::new(initial_failures);

        println!("\n1. Initial Assessment:");
        println!("   Total RPN: {}", initial.total_rpn);
        println!("   Average RPN: {:.1}", initial.average_rpn());

        // Step 2: After applying fixes
        let fixed_failures = vec![
            FailureMode {
                error_code: "E0277",
                description: "trait bound not satisfied",
                severity: 9,
                occurrence: 2,  // Fixed with proper bounds
                detection: 10,
            },
            FailureMode {
                error_code: "E0308",
                description: "mismatched types",
                severity: 8,
                occurrence: 3,  // Fixed with type annotations
                detection: 9,
            },
            FailureMode {
                error_code: "E0283",
                description: "type annotations needed",
                severity: 7,
                occurrence: 2,  // Fixed with explicit types
                detection: 8,
            },
            FailureMode {
                error_code: "E0599",
                description: "method not found",
                severity: 9,
                occurrence: 2,  // Fixed with trait imports
                detection: 10,
            },
        ];

        let fixed = FmeaAssessment::new(fixed_failures);

        println!("\n2. Post-Fix Assessment:");
        println!("   Total RPN: {}", fixed.total_rpn);
        println!("   Average RPN: {:.1}", fixed.average_rpn());

        let total_reduction = ((initial.total_rpn - fixed.total_rpn) as f64
            / initial.total_rpn as f64) * 100.0;

        println!("\n3. Overall Effectiveness:");
        println!("   Total RPN Reduction: {:.1}%", total_reduction);

        // Verify significant risk reduction
        assert!(fixed.total_rpn < initial.total_rpn);
        assert!(total_reduction > 60.0, "Expected >60% risk reduction");

        println!("\n✓ FMEA workflow completed successfully!");
        println!("✓ Risk reduced from {} to {} ({:.1}% improvement)",
                 initial.total_rpn, fixed.total_rpn, total_reduction);
    }
}
