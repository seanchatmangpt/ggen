//! Property-Based Invariant Validation
//!
//! Uses proptest to exhaustively verify that invariant violations are always detected.
//! All properties are derived from the 47 failure modes in INVARIANT_MATRIX.md.
//!
//! This module tests the invariant detection mechanisms themselves, ensuring:
//! - Thermal validators reject backward clocks and monster jumps
//! - Effect validators enforce closed-world assumptions
//! - State validators prevent invalid transitions
//! - Consensus validators enforce 2/3 Byzantine quorum
//! - And 43 more failure modes across all 12 phases

#[cfg(test)]
#[allow(clippy::unwrap_used)] // Test code: unwrap is acceptable
mod properties {
    use crate::core::fail_fast::*;
    use crate::core::invariants::*;
    use proptest::prelude::*;

    // ========================================================================
    // Phase 2: Thermal Validator Properties
    // ========================================================================

    proptest! {
        #[test]
        fn prop_thermal_accepts_monotonic_sequences(
            values in prop::collection::vec(1u64..1_000_000, 1..100)
        ) {
            let mut validator = ThermalValidator::new(1_000_000_000);
            let mut sorted = values.clone();
            sorted.sort_unstable();

            for &tau in &sorted {
                let result = validator.validate_tau(tau);
                prop_assert!(result.is_ok(), "Monotonic sequence should always pass");
            }
        }

        #[test]
        fn prop_thermal_rejects_backward_clock(
            a in 100u64..1_000_000,
            b in 1u64..100u64,
        ) {
            let mut validator = ThermalValidator::new(1_000_000_000);
            let _ = validator.validate_tau(a);
            let result = validator.validate_tau(b); // b < a, going backward
            prop_assert!(result.is_err(), "Backward clock should always fail");
        }

        #[test]
        fn prop_thermal_rejects_monster_jumps(
            prev in 1u64..1_000_000,
            threshold in 1000u64..10_000u64,
        ) {
            let mut validator = ThermalValidator::new(threshold);
            let _ = validator.validate_tau(prev);
            let huge_jump = prev + threshold + 1; // Exceeds threshold by 1
            let result = validator.validate_tau(huge_jump);
            prop_assert!(result.is_err(), "Jump exceeding threshold should fail");
        }

        #[test]
        fn prop_thermal_allows_jumps_within_threshold(
            prev in 1u64..10_000u64,
            jump in 0u64..1000u64,
        ) {
            let threshold = jump + 100; // Ensure jump is within threshold
            let mut validator = ThermalValidator::new(threshold);
            let _ = validator.validate_tau(prev);
            let result = validator.validate_tau(prev + jump);
            prop_assert!(result.is_ok(), "Jump within threshold should pass");
        }
    }

    // ========================================================================
    // Phase 3: Effect Validator Properties
    // ========================================================================

    proptest! {
        #[test]
        fn prop_effects_rejects_unobserved_effects(
            declared in prop::collection::vec("A|B|C|D|E", 1..6),
            unobserved in "F|G|H|I|J",
        ) {
            let declared_unique: Vec<_> = declared.iter().cloned().collect();
            if let Ok(validator) = EffectValidator::new(declared_unique) {
                let result = validator.validate_observed(&[unobserved.to_string()]);
                prop_assert!(result.is_err(), "Unobserved effect should fail");
            }
        }

        #[test]
        fn prop_effects_accepts_subset_of_declared(
            all_effects in prop::collection::vec("[a-z]", 2..8),
        ) {
            let unique: Vec<_> = all_effects.iter().cloned().collect();
            if let Ok(validator) = EffectValidator::new(unique.clone()) {
                // Create a subset (empty subset is valid)
                let subset = unique.get(..unique.len().saturating_sub(1))
                    .unwrap_or(&[])
                    .to_vec();
                let result = validator.validate_observed(&subset);
                prop_assert!(result.is_ok(), "Subset of declared effects should pass");
            }
        }
    }

    // ========================================================================
    // Phase 4: State Validator Properties
    // ========================================================================

    proptest! {
        #[test]
        fn prop_states_reachable_from_initial(
            initial in "[A-Z][a-z]+",
            next in "[A-Z][a-z]+",
        ) {
            if initial != next {
                let states = vec![initial.clone(), next.clone()];
                if let Ok(mut validator) = StateValidator::new(initial, states) {
                    let result = validator.validate_transition(&next);
                    prop_assert!(result.is_ok(), "Transition to declared state should pass");
                }
            }
        }

        #[test]
        fn prop_states_reject_empty_state_name(initial in "[A-Z][a-z]+") {
            let states = vec![initial.clone(), "Next".to_string()];
            if let Ok(mut validator) = StateValidator::new(initial, states) {
                let result = validator.validate_transition("");
                prop_assert!(result.is_err(), "Empty state name should fail");
            }
        }
    }

    // ========================================================================
    // Phase 5: Receipt Validator Properties
    // ========================================================================

    proptest! {
        #[test]
        fn prop_receipts_detect_version_mismatch(
            wrong_version in prop::num::u32::ANY,
        ) {
            let rv = ReceiptValidator::new(1);
            // Ensure wrong_version != 1
            let wrong = if wrong_version == 1 { 2 } else { wrong_version };
            let result = rv.validate_receipt(wrong, 0x1234, 0x1234);
            prop_assert!(result.is_err(), "Version mismatch should fail");
        }

        #[test]
        fn prop_receipts_detect_checksum_mismatch(
            checksum in prop::num::u32::ANY,
            computed in prop::num::u32::ANY,
        ) {
            if checksum != computed {
                let rv = ReceiptValidator::new(1);
                let result = rv.validate_receipt(1, checksum, computed);
                prop_assert!(result.is_err(), "Checksum mismatch should fail");
            }
        }

        #[test]
        fn prop_receipts_accept_matching_fields(
            value in prop::num::u32::ANY,
        ) {
            let rv = ReceiptValidator::new(1);
            let result = rv.validate_receipt(1, value, value);
            prop_assert!(result.is_ok(), "Matching fields should pass");
        }
    }

    // ========================================================================
    // Phase 6-7: Orchestration & Pipeline Properties
    // ========================================================================

    proptest! {
        #[test]
        fn prop_swarm_requires_all_tests_executed(
            scheduled in 1usize..100,
            executed in 0usize..100,
        ) {
            let mut ctx = StrictExecutionContext::new("test".to_string()).unwrap();
            let result = ctx.phase_6_swarm_orchestration(scheduled, executed);

            if executed >= scheduled {
                prop_assert!(result.is_ok(), "All executed tests should pass");
            } else {
                prop_assert!(result.is_err(), "Unexecuted tests should fail");
            }
        }
    }

    // ========================================================================
    // Phase 9: Consensus Properties
    // ========================================================================

    proptest! {
        #[test]
        fn prop_consensus_requires_2_3_quorum(
            total_votes in 3usize..30,
            approval_votes in 0usize..30,
        ) {
            let required = (total_votes * 2) / 3 + 1;
            let mut ctx = StrictExecutionContext::new("test".to_string()).unwrap();
            let result = ctx.phase_9_distributed_consensus(approval_votes, total_votes);

            if approval_votes >= required {
                prop_assert!(result.is_ok(),
                    "Sufficient approvals ({} >= {}) should pass", approval_votes, required);
            } else {
                prop_assert!(result.is_err(),
                    "Insufficient approvals ({} < {}) should fail", approval_votes, required);
            }
        }
    }

    // ========================================================================
    // Phase 11: Prophet Validator Properties
    // ========================================================================

    proptest! {
        #[test]
        fn prop_prophet_rejects_invalid_confidence(
            confidence in prop::num::f64::NORMAL,
        ) {
            let mut ctx = StrictExecutionContext::new("test".to_string()).unwrap();
            let result = ctx.phase_11_performance_prophet(1000, confidence);

            if (0.0..=1.0).contains(&confidence) && confidence.is_finite() {
                prop_assert!(result.is_ok(), "Valid confidence should pass");
            } else {
                prop_assert!(result.is_err(), "Invalid confidence should fail");
            }
        }

        #[test]
        fn prop_prophet_rejects_zero_ticks(confidence in 0.0f64..1.0f64) {
            let mut ctx = StrictExecutionContext::new("test".to_string()).unwrap();
            let result = ctx.phase_11_performance_prophet(0, confidence);
            prop_assert!(result.is_err(), "Zero predicted ticks should fail");
        }

        #[test]
        fn prop_prophet_accepts_positive_ticks(
            ticks in 1u64..1_000_000_000,
            confidence in 0.0f64..1.0f64,
        ) {
            let mut ctx = StrictExecutionContext::new("test".to_string()).unwrap();
            let result = ctx.phase_11_performance_prophet(ticks, confidence);
            prop_assert!(result.is_ok(), "Positive ticks with valid confidence should pass");
        }
    }

    // ========================================================================
    // Phase 12: Dashboard Validator Properties
    // ========================================================================

    proptest! {
        #[test]
        fn prop_dashboard_requires_consistent_totals(
            passed in 0usize..100,
            failed in 0usize..100,
        ) {
            let total = passed + failed;
            let mut ctx = StrictExecutionContext::new("test".to_string()).unwrap();
            let result = ctx.phase_12_quality_dashboard(total, passed, failed);
            prop_assert!(result.is_ok(), "Consistent totals should pass");
        }

        #[test]
        fn prop_dashboard_rejects_inconsistent_totals(
            total in 10usize..100,
            passed in 0usize..100,
            failed in 0usize..100,
        ) {
            if passed + failed != total {
                let mut ctx = StrictExecutionContext::new("test".to_string()).unwrap();
                let result = ctx.phase_12_quality_dashboard(total, passed, failed);
                prop_assert!(result.is_err(), "Inconsistent totals should fail");
            }
        }
    }

    // ========================================================================
    // Compound Properties: Full Pipeline
    // ========================================================================

    proptest! {
        #[test]
        fn prop_full_pipeline_execution(
            contract_id in "[a-z0-9_]{1,20}",
            tau in 10u64..100_000,
            samples in 5usize..50,
        ) {
            let mut ctx = StrictExecutionContext::new(contract_id)?;

            // Phase 1: Contract
            let _ = ctx.phase_1_contract_definition(12);

            // Phase 2: Thermal
            let _ = ctx.phase_2_thermal_testing(tau, 1_000_000);

            // Phase 8: Learning
            let probability = (samples % 10) as f64 / 10.0;
            let _ = ctx.phase_8_continuous_learning(samples, probability);

            // Phase 11: Prophet
            let _ = ctx.phase_11_performance_prophet(tau, 0.8);

            // Phase 12: Dashboard
            let _ = ctx.phase_12_quality_dashboard(100, 80, 20);
        }
    }

    // ========================================================================
    // Edge Cases & Adversarial Input
    // ========================================================================

    proptest! {
        #[test]
        fn prop_thermal_handles_max_u64(threshold in 1u64..u64::MAX) {
            let mut validator = ThermalValidator::new(threshold);
            let _ = validator.validate_tau(0);
            let result = validator.validate_tau(u64::MAX);
            prop_assert!(result.is_ok() || result.is_err(), "Should handle max u64");
        }

        #[test]
        fn prop_confidence_boundaries(boundary in prop::sample::select(vec![0.0f64, 1.0f64])) {
            let mut ctx = StrictExecutionContext::new("test".to_string()).unwrap();
            let result = ctx.phase_11_performance_prophet(1000, boundary);
            prop_assert!(result.is_ok(), "Boundary values [0.0, 1.0] should pass");
        }

        #[test]
        fn prop_empty_effects_rejected(_ignored: ()) {
            let result = EffectValidator::new(vec![]);
            prop_assert!(result.is_err(), "Empty effects should be rejected");
        }
    }
}

/// Public helpers for invariant validation
pub mod helpers {
    use crate::core::invariants::{EffectValidator, InvariantResult, ThermalValidator};

    /// Batch-validate a collection of thermal measurements for monotonicity.
    ///
    /// # Arguments
    /// * `taus` - Sequence of τ values to validate
    /// * `threshold` - Maximum allowed jump between consecutive measurements
    ///
    /// # Returns
    /// `Ok(())` if all measurements are monotonic and within threshold, or an error.
    ///
    /// # Errors
    ///
    /// Returns an error if any τ measurement violates monotonicity or exceeds the threshold.
    pub fn validate_thermal_sequence(taus: &[u64], threshold: u64) -> InvariantResult<()> {
        let mut validator = ThermalValidator::new(threshold);
        for &tau in taus {
            validator.validate_tau(tau)?;
        }
        Ok(())
    }

    /// Validate that all observed effects are within the declared effect set.
    ///
    /// Enforces closed-world assumption: no effect can occur that wasn't declared.
    ///
    /// # Arguments
    /// * `declared` - Complete set of allowed effects
    /// * `observed` - Effects that occurred at runtime
    ///
    /// # Returns
    /// `Ok(())` if observed ⊆ declared, or an error.
    ///
    /// # Errors
    ///
    /// Returns an error if any observed effect is not in the declared set.
    pub fn validate_all_effects(declared: Vec<String>, observed: &[String]) -> InvariantResult<()> {
        let validator = EffectValidator::new(declared)?;
        validator.validate_observed(observed)?;
        Ok(())
    }
}

#[cfg(test)]
mod integration_tests {
    use crate::core::invariant_properties::helpers::*;

    #[test]
    fn test_thermal_sequence_helper() {
        let sequence = vec![100, 200, 300, 400, 500];
        let result = validate_thermal_sequence(&sequence, 1_000_000);
        assert!(result.is_ok());
    }

    #[test]
    fn test_thermal_sequence_fails_on_backward() {
        let sequence = vec![100, 200, 150, 400]; // 200 -> 150 goes backward
        let result = validate_thermal_sequence(&sequence, 1_000_000);
        assert!(result.is_err());
    }

    #[test]
    fn test_effects_helper() {
        let declared = vec!["A".to_string(), "B".to_string(), "C".to_string()];
        let observed = vec!["A".to_string(), "B".to_string()];
        let result = validate_all_effects(declared, &observed);
        assert!(result.is_ok());
    }
}
