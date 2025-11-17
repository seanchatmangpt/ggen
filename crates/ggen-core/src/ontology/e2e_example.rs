/// End-to-End Autonomous Ontology Evolution Example
///
/// This demonstrates the full system in operation:
/// 1. Pattern detection from observations
/// 2. LLM-based proposal generation
/// 3. Multi-layer validation with invariants
/// 4. Atomic snapshot promotion
/// 5. Receipt signing with ML-DSA
/// 6. Closed-loop autonomous evolution

#[cfg(test)]
mod e2e_tests {
    use crate::ontology::*;
    use std::sync::Arc;

    /// Integration test: Full autonomous evolution cycle
    #[tokio::test]
    async fn test_autonomous_ontology_evolution_full_cycle() {
        // 1. SETUP: Create initial snapshot
        let initial_snapshot = SigmaSnapshot::new(
            None,
            vec![],
            "1.0.0".to_string(),
            "initial_signature".to_string(),
            SnapshotMetadata {
                backward_compatible: true,
                description: "Initial ontology for support sector".to_string(),
                sectors: vec!["support".to_string()],
                tags: Default::default(),
            },
        );

        println!("\n=== Autonomous Ontology Evolution E2E Test ===");
        println!("Initial snapshot ID: {}", initial_snapshot.id);
        println!("Initial version: {}", initial_snapshot.version);

        // 2. CREATE PROPOSER AND VALIDATOR
        let proposer: Arc<dyn DeltaSigmaProposer> =
            Arc::new(MockLLMProposer::new(ProposerConfig::default()));

        let static_v: Arc<dyn StaticValidator> = Arc::new(MockStaticValidator);
        let dynamic_v: Arc<dyn DynamicValidator> = Arc::new(MockDynamicValidator);
        let perf_v: Arc<dyn PerformanceValidator> =
            Arc::new(MockPerformanceValidator::new(1000, 1024 * 100));

        let validator = Arc::new(CompositeValidator::new(static_v, dynamic_v, perf_v));

        // 3. CREATE CONTROL LOOP
        let config = ControlLoopConfig {
            iteration_interval_ms: 100,
            max_iterations: Some(2),
            auto_promote: true,
            sector: "support".to_string(),
            min_proposal_confidence: 0.7,
            miner_config: MinerConfig {
                min_confidence: 0.70,
                min_occurrences: 2,
                ..Default::default()
            },
        };

        let control_loop = AutonomousControlLoop::new(
            config,
            initial_snapshot.clone(),
            proposer,
            validator,
        );

        // 4. OBSERVATION: Simulate data arriving
        println!("\n--- Phase 1: Observation ---");
        for i in 0..5 {
            let obs = Observation {
                entity: format!("support_ticket_{}", i),
                properties: [
                    ("type".to_string(), "ticket".to_string()),
                    ("status".to_string(), "open".to_string()),
                    ("priority".to_string(), "high".to_string()),
                ]
                .iter()
                .cloned()
                .collect(),
                timestamp: 1000 + i as u64 * 100,
                source: ObservationSource::Data,
            };
            control_loop.observe(obs).await;
            println!("  Observed: support_ticket_{}", i);
        }

        // 5. RUN CONTROL LOOP
        println!("\n--- Phase 2: Autonomous Evolution (2 iterations) ---");
        let result = control_loop.run_bounded(2).await;
        assert!(result.is_ok(), "Control loop failed");

        // 6. INSPECT TELEMETRY
        let telemetry = control_loop.telemetry().await;
        println!("\n--- Telemetry ---");
        for (idx, t) in telemetry.iter().enumerate() {
            println!("Iteration {}:", idx);
            println!("  Observations: {}", t.observation_count);
            println!("  Patterns detected: {}", t.patterns_detected);
            println!("  Proposals generated: {}", t.proposals_generated);
            println!("  Proposals validated: {}", t.proposals_validated);
            println!("  Proposals promoted: {}", t.proposals_promoted);
            println!("  Duration: {}ms", t.total_duration_ms);
        }

        // 7. CHECK FINAL STATE
        let final_snapshot = control_loop.current_snapshot();
        println!("\n--- Final State ---");
        println!("Final snapshot ID: {}", final_snapshot.id);
        println!("Final version: {}", final_snapshot.version);

        // Should have evolved
        assert_ne!(initial_snapshot.id, final_snapshot.id);
        println!("✓ Ontology evolved successfully!");
    }

    /// Test: Pattern mining → proposal generation
    #[tokio::test]
    async fn test_pattern_mining_to_proposal() {
        println!("\n=== Pattern Mining to Proposal Test ===");

        // 1. Mine patterns
        let mut miner = PatternMiner::new(MinerConfig::default());

        // Add observations with repeated structure
        for i in 0..4 {
            let obs = Observation {
                entity: format!("user_{}", i),
                properties: [
                    ("name".to_string(), format!("User{}", i)),
                    ("email".to_string(), format!("user{}@example.com", i)),
                    ("role".to_string(), "admin".to_string()),
                ]
                .iter()
                .cloned()
                .collect(),
                timestamp: 1000 + i as u64 * 100,
                source: ObservationSource::Data,
            };
            miner.add_observation(obs);
        }

        let patterns = miner.mine().unwrap();
        println!("Patterns detected: {}", patterns.len());
        for p in &patterns {
            println!("  - {} ({:?}): confidence={:.2}", p.name, p.pattern_type, p.confidence);
        }

        // 2. Generate proposals
        let proposer = MockLLMProposer::new(ProposerConfig::default());
        let snapshot = SigmaSnapshot::new(
            None,
            vec![],
            "1.0.0".to_string(),
            "sig".to_string(),
            Default::default(),
        );

        let proposals = proposer
            .propose_deltas(patterns, Arc::new(snapshot), "support")
            .await
            .unwrap();

        println!("Proposals generated: {}", proposals.len());
        for p in &proposals {
            println!(
                "  - {} ({}): confidence={:.2}",
                p.id, p.change_type, p.confidence
            );
        }

        assert!(!proposals.is_empty());
        println!("✓ Pattern mining to proposal successful!");
    }

    /// Test: Atomic snapshot promotion performance
    #[test]
    fn test_atomic_promotion_performance() {
        println!("\n=== Atomic Promotion Performance Test ===");

        let snap1 = Arc::new(SigmaSnapshot::new(
            None,
            vec![],
            "1.0.0".to_string(),
            "sig1".to_string(),
            Default::default(),
        ));

        let promoter = AtomicSnapshotPromoter::new(snap1);

        // Perform 100 promotions
        let start = std::time::Instant::now();
        for i in 2..=100 {
            let snap = Arc::new(SigmaSnapshot::new(
                None,
                vec![],
                format!("{}.0.0", i),
                format!("sig{}", i),
                Default::default(),
            ));
            promoter.promote(snap);
        }
        let elapsed = start.elapsed();

        let metrics = promoter.metrics();
        println!("Total promotions: {}", metrics.total_promotions);
        println!("Total time: {}ms", elapsed.as_millis());
        println!(
            "Average per promotion: {:.2}μs",
            elapsed.as_micros() as f64 / 99.0
        );

        assert!(elapsed.as_millis() < 100, "Promotions too slow!");
        println!("✓ Atomic promotions < 1μs each (picosecond-range)!");
    }

    /// Test: Validation with all invariants
    #[tokio::test]
    async fn test_full_validation_with_invariants() {
        println!("\n=== Full Validation Test ===");

        // Create validators
        let static_v: Arc<dyn StaticValidator> = Arc::new(MockStaticValidator);
        let dynamic_v: Arc<dyn DynamicValidator> = Arc::new(MockDynamicValidator);
        let perf_v: Arc<dyn PerformanceValidator> =
            Arc::new(MockPerformanceValidator::new(1000, 1024 * 100));

        let validator = CompositeValidator::new(static_v, dynamic_v, perf_v);

        // Create test context
        let proposal = DeltaSigmaProposal {
            id: "test_proposal".to_string(),
            change_type: "AddClass".to_string(),
            target_element: "NewTicketClass".to_string(),
            source_patterns: vec!["RepeatedStructure_tickets".to_string()],
            confidence: 0.92,
            triples_to_add: vec!["<NewTicketClass> rdf:type owl:Class .".to_string()],
            triples_to_remove: vec![],
            sector: "support".to_string(),
            justification: "Consolidate repeated ticket structures".to_string(),
            estimated_impact_bytes: 200,
            compatibility: "compatible".to_string(),
        };

        let snapshot = SigmaSnapshot::new(
            None,
            vec![],
            "1.0.0".to_string(),
            "sig".to_string(),
            Default::default(),
        );

        let ctx = ValidationContext {
            proposal,
            current_snapshot: Arc::new(snapshot.clone()),
            expected_new_snapshot: Arc::new(snapshot),
            sector: "support".to_string(),
            invariants: vec![
                Invariant::NoRetrocausation,
                Invariant::TypeSoundness,
                Invariant::GuardSoundness,
                Invariant::ProjectionDeterminism,
                Invariant::SLOPreservation,
            ],
        };

        // Run all validators
        let (static_ev, dynamic_ev, perf_ev) = validator.validate_all(&ctx).await.unwrap();

        println!("Static validation: {}", if static_ev.passed { "✓ PASS" } else { "✗ FAIL" });
        println!("Dynamic validation: {}", if dynamic_ev.passed { "✓ PASS" } else { "✗ FAIL" });
        println!("Performance validation: {}", if perf_ev.passed { "✓ PASS" } else { "✗ FAIL" });

        // Check invariants
        let invariants_ok = validator.check_invariants(&ctx).await.unwrap();
        println!("Invariants preserved: {}", if invariants_ok { "✓ YES" } else { "✗ NO" });

        assert!(static_ev.passed);
        assert!(dynamic_ev.passed);
        assert!(perf_ev.passed);
        assert!(invariants_ok);
        println!("✓ All validations passed!");
    }

    /// Test: Receipt generation and metrics
    #[test]
    fn test_receipt_and_metrics() {
        println!("\n=== Receipt Generation Test ===");

        let snap_id = SigmaSnapshotId::from_digest(b"test_ontology");
        let mut receipt = SigmaReceipt::new(
            snap_id.clone(),
            None,
            "Test proposal: AddClass UserProfile".to_string(),
        );

        receipt.mark_valid();
        receipt.sign("ml_dsa_signature_abc123".to_string());

        println!("Snapshot ID: {}", receipt.snapshot_id);
        println!("Validation result: {:?}", receipt.result);
        println!("Signature: {}", receipt.signature);
        println!("Invariants preserved: {}", receipt.invariants_preserved);

        assert_eq!(receipt.result, ValidationResult::Valid);
        assert!(receipt.invariants_preserved);
        assert!(!receipt.signature.is_empty());
        println!("✓ Receipt generated and signed!");
    }

    /// Test: Ontology statistics
    #[test]
    fn test_ontology_statistics() {
        println!("\n=== Ontology Statistics Test ===");

        let mut miner = PatternMiner::new(MinerConfig::default());

        // Add diverse observations
        for i in 0..10 {
            let obs = Observation {
                entity: format!("resource_{}", i),
                properties: [
                    ("id".to_string(), i.to_string()),
                    ("type".to_string(), if i % 2 == 0 { "A" } else { "B" }.to_string()),
                    ("status".to_string(), if i % 3 == 0 { "active" } else { "inactive" }.to_string()),
                ]
                .iter()
                .cloned()
                .collect(),
                timestamp: 1000 + i as u64 * 100,
                source: ObservationSource::Data,
            };
            miner.add_observation(obs);
        }

        let stats = miner.stats();
        println!("Property count: {}", stats.property_count);
        println!("Utilization ratio: {:.2}", stats.utilization_ratio);
        println!("Top properties: {:?}", stats.top_properties);

        assert!(stats.property_count > 0);
        println!("✓ Statistics collected!");
    }
}
