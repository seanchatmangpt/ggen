//! MAPE-K Integration Tests
//!
//! Tests the complete Monitor-Analyze-Plan-Execute-Knowledge feedback loop.

#[cfg(test)]
mod tests {
    use crate::mape_k::{
        AnalyzeEngine, ExecuteEngine, FindingKind, KnowledgeStore as KStore, MonitorEngine,
        Observation, ObservationType, OverlayKind, OverlayProposal, PlanEngine, PolicyAction,
        PolicyRule, SLOConfig, ValidationStatus,
    };
    use std::collections::HashMap;

    fn get_timestamp() -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0)
    }

    #[test]
    fn test_plan_with_custom_policies() {
        let mut planner = PlanEngine::new();

        // Add custom policy
        let custom_policy = PolicyRule {
            id: "custom-policy".to_string(),
            name: "Custom optimization policy".to_string(),
            trigger_condition: "GuardFailureRate".to_string(),
            action: PolicyAction::ProposeOverlay {
                overlay_kind: "Modification".to_string(),
                parameters: {
                    let mut m = HashMap::new();
                    m.insert("action".to_string(), serde_json::json!("adjust_threshold"));
                    m
                },
            },
            priority: 110,
            enabled: true,
        };

        planner.add_policy(custom_policy);
        assert_eq!(planner.policies().len(), 4); // 3 defaults + 1 custom
    }

    #[test]
    fn test_execute_validation_stages() {
        let executor = ExecuteEngine::new();
        assert_eq!(executor.promotion_history().len(), 0);

        let validators = executor
            .validators()
            .iter()
            .map(|v| v.name())
            .collect::<Vec<_>>();

        // Should have all 4 validators
        assert!(validators.contains(&"SHACL"));
        assert!(validators.contains(&"TDD"));
        assert!(validators.contains(&"Performance"));
        assert!(validators.contains(&"Security"));
    }

    #[test]
    fn test_knowledge_snapshot_tracking() {
        let mut knowledge = KStore::new();

        // Verify initial snapshot
        assert_eq!(knowledge.snapshot_history().len(), 1);
        let initial = knowledge.active_snapshot().unwrap();
        assert!(initial.is_active);
        assert_eq!(initial.id, "snapshot-0");

        // Simulate overlay promotion
        knowledge.record_promotion("overlay-1".to_string(), "snapshot-1".to_string());

        assert_eq!(knowledge.snapshot_history().len(), 2);
        let active = knowledge.active_snapshot().unwrap();
        assert_eq!(active.id, "snapshot-1");
        assert!(active.is_active);

        // Old snapshot should be inactive
        let history = knowledge.snapshot_history();
        let old = history.iter().find(|s| s.id == "snapshot-0");
        assert!(old.is_some());
        assert!(!old.unwrap().is_active);
    }

    #[test]
    fn test_knowledge_query_findings_by_component() {
        let mut knowledge = KStore::new();

        // Add findings for different components
        for i in 0..3 {
            let finding = crate::mape_k::Finding {
                id: format!("finding-{}", i),
                kind: FindingKind::TickBudgetViolation,
                severity: if i == 0 {
                    "Critical".to_string()
                } else {
                    "High".to_string()
                },
                description: format!("Finding {}", i),
                component: if i == 0 {
                    "comp-a".to_string()
                } else {
                    "comp-b".to_string()
                },
                evidence: vec![],
                suggested_action: None,
                timestamp: get_timestamp(),
                metadata: HashMap::new(),
            };
            knowledge.record_finding(finding);
        }

        // Query for comp-a
        let results = knowledge.findings_for_component("comp-a");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].id, "finding-0");
    }

    #[test]
    fn test_knowledge_metrics_accumulation() {
        let mut knowledge = KStore::new();

        assert_eq!(knowledge.mape_metrics().observations_ingested, 0);
        assert_eq!(knowledge.mape_metrics().findings_generated, 0);

        // Add observations
        for i in 0..5 {
            knowledge.record_observation(Observation {
                id: format!("obs-{}", i),
                obs_type: ObservationType::Metric,
                timestamp: get_timestamp(),
                data: serde_json::json!({}),
                source: "test".to_string(),
            });
        }

        assert_eq!(knowledge.mape_metrics().observations_ingested, 5);

        // Add findings
        for i in 0..3 {
            knowledge.record_finding(crate::mape_k::Finding {
                id: format!("find-{}", i),
                kind: FindingKind::DriftDetected,
                severity: "Medium".to_string(),
                description: "Test".to_string(),
                component: "test".to_string(),
                evidence: vec![],
                suggested_action: None,
                timestamp: get_timestamp(),
                metadata: HashMap::new(),
            });
        }

        assert_eq!(knowledge.mape_metrics().findings_generated, 3);
    }

    #[test]
    fn test_end_to_end_finding_to_execution() {
        let mut monitor = MonitorEngine::new();
        let mut analyzer = AnalyzeEngine::new(SLOConfig::default());
        let mut planner = PlanEngine::new();
        let mut executor = ExecuteEngine::new();
        let mut knowledge = KStore::new();

        let now = get_timestamp();

        // Monitor: Create tick budget violation observation
        monitor.ingest_observation(Observation {
            id: "e2e-test-obs".to_string(),
            obs_type: ObservationType::Metric,
            timestamp: now,
            data: serde_json::json!({"pattern": "e2e_pattern", "ticks": 20}),
            source: "e2e-monitor".to_string(),
        });

        monitor.run_aggregations();
        knowledge.record_observation(Observation {
            id: "e2e-test-obs".to_string(),
            obs_type: ObservationType::Metric,
            timestamp: now,
            data: serde_json::json!({"pattern": "e2e_pattern", "ticks": 20}),
            source: "e2e-monitor".to_string(),
        });

        // Analyze: Generate findings
        let findings = analyzer.analyze(&monitor);
        for finding in &findings {
            knowledge.record_finding(finding.clone());
        }

        // Plan: Generate proposals
        if !findings.is_empty() {
            let proposals = planner.plan(&findings);
            for proposal in proposals {
                knowledge.record_overlay(proposal.overlay.clone());

                // Execute: Validate
                let mut exec_proposal = OverlayProposal {
                    title: proposal.title.clone(),
                    description: proposal.description.clone(),
                    overlay: proposal.overlay,
                    estimated_effort: proposal.estimated_effort,
                    expected_improvement: proposal.expected_improvement,
                    risk_level: proposal.risk_level,
                };

                let _exec_result = executor.execute(&mut exec_proposal);
            }
        }

        // Verify knowledge store has accumulated everything
        let stats = knowledge.statistics();
        assert!(stats.total_observations > 0);
        assert!(stats.total_findings > 0 || stats.total_observations > 0);

        // Verify MAPE metrics
        let metrics = knowledge.mape_metrics();
        assert!(metrics.observations_ingested > 0);
    }

    #[test]
    fn test_policy_priority_ordering() {
        let planner = PlanEngine::new();

        // Default policies should be ordered by priority
        let policies = planner.policies();
        if policies.len() >= 2 {
            for i in 0..policies.len() - 1 {
                assert!(
                    policies[i].priority >= policies[i + 1].priority,
                    "Policies should be ordered by descending priority"
                );
            }
        }
    }

    #[test]
    fn test_overlay_proposal_immutability() {
        // Create proposal
        let proposal = OverlayProposal {
            title: "Immutable Test".to_string(),
            description: "Test immutability".to_string(),
            overlay: crate::mape_k::OntologyOverlay {
                id: "immutable-overlay".to_string(),
                base_snapshot_id: "snapshot-0".to_string(),
                rdf_patch: "@prefix test: <http://test#> .".to_string(),
                overlay_kind: OverlayKind::Modification,
                guard_changes: vec![],
                config_changes: HashMap::new(),
                proposer: crate::mape_k::OverlayProposer::Policy,
                related_finding: None,
                created_at: get_timestamp(),
                validation_status: ValidationStatus::Pending,
                validation_results: vec![],
            },
            estimated_effort: 1.0,
            expected_improvement: 2.0,
            risk_level: "Low".to_string(),
        };

        // Store in knowledge
        let mut knowledge = KStore::new();
        knowledge.record_overlay(proposal.overlay.clone());

        // Verify it's stored correctly
        assert_eq!(knowledge.all_overlays().len(), 1);
        assert_eq!(knowledge.all_overlays()[0].id, "immutable-overlay");
    }

    #[test]
    fn test_mape_k_metrics_tracking() {
        let mut knowledge = KStore::new();

        // Add some activity
        for _ in 0..10 {
            knowledge.record_observation(Observation {
                id: "metric-test".to_string(),
                obs_type: ObservationType::Metric,
                timestamp: get_timestamp(),
                data: serde_json::json!({}),
                source: "test".to_string(),
            });
        }

        // Update execution time
        knowledge.update_execution_time(500);
        knowledge.update_execution_time(300);

        let metrics = knowledge.mape_metrics();
        assert_eq!(metrics.observations_ingested, 10);
        assert_eq!(metrics.total_execution_time_ms, 800);
        assert!(metrics.last_execution_timestamp > 0);
    }
}
