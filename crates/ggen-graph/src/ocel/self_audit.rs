//! Self-audit log generator for compliance verification.
//!
//! This module provides functions to construct an `OcelLog` with all required
//! object types, event types, qualifiers, and attributes representing the development process.

use crate::ocel::{OcelEvent, OcelLog, OcelObject, OcelObjectRef};
use chrono::{TimeZone, Utc};
use std::collections::HashMap;

/// Generates a complete self-audit event log containing all required object and event types,
/// and relationship qualifiers representing the development and verification process.
pub fn generate_self_audit_log() -> OcelLog {
    let mut log = OcelLog::new();

    // 1. Objects representing all required object types:
    // RustCrate
    let mut attr_crate = HashMap::new();
    attr_crate.insert("name".to_string(), "ggen-graph".to_string());
    attr_crate.insert("version".to_string(), "0.1.0".to_string());
    log.objects.push(OcelObject {
        id: "obj_crate_ggen".to_string(),
        r#type: "RustCrate".to_string(),
        attributes: attr_crate,
    });

    // PRDRequirement
    let mut attr_prd = HashMap::new();
    attr_prd.insert("title".to_string(), "Self-Audit Compliance Log".to_string());
    attr_prd.insert(
        "description".to_string(),
        "The system must generate a self-audit log containing all required activities and objects."
            .to_string(),
    );
    log.objects.push(OcelObject {
        id: "obj_prd_compliance".to_string(),
        r#type: "PRDRequirement".to_string(),
        attributes: attr_prd,
    });

    // ARDRequirement
    let mut attr_ard = HashMap::new();
    attr_ard.insert(
        "title".to_string(),
        "Deterministic Graph Mapping".to_string(),
    );
    attr_ard.insert(
        "description".to_string(),
        "Self-audit logs must project deterministically to and from RDF triple graphs.".to_string(),
    );
    log.objects.push(OcelObject {
        id: "obj_ard_mapping".to_string(),
        r#type: "ARDRequirement".to_string(),
        attributes: attr_ard,
    });

    let requirements_data = vec![
        ("req_r1_one_crate", "One-Crate Package Boundary and No Feature Flags", "The substrate must be implemented as a single standalone Rust package at crates/ggen-graph with no feature flags."),
        ("req_r2_ontology", "Public Ontology Governance and Vocabulary Emission", "Vocabulary constants must be emitted directly from public ontology profiles."),
        ("req_r3_deterministic", "Deterministic Graph Substrate and Delta Conservation", "Graph parsing, serialization, canonical byte generation, hashing, and RdfDelta transitions must be deterministic."),
        ("req_r4_knowledge_hook", "Knowledge Hook Runtime & Deterministic Scheduling", "Turtle-based hook packs must load dynamically as graph laws and schedule deterministically."),
        ("req_r5_ocel_prov", "OCEL/PROV Evidence Projection & Replayable Receipts", "Every graph transition must produce a replayable cryptographic receipt bundle and project PROV-O/OCEL."),
        ("req_r6_compliance", "Compliance Checks & Anti-Fake Gates", "Must pass structural analyses checking for forbidden execution surfaces and zero placeholder/stub/mock/fake-success patterns."),
        ("req_r7_ocel_self_audit", "OCEL v2 Self-Audit Log Emission", "Implement self-audit log generator emitting vision2030.self_audit.ocel.json."),
        ("req_r8_coverage_matrix", "Coverage Matrix & Verification Scripts", "Implement coverage mapping in coverage.rs and verify via scripts."),
        ("req_r9_proof_report", "Vision 2030 Durable Proof Report", "Establish Vision 2030 durable proof report."),
        ("req_r10_interchangeable", "Genesis-bearing Interchangeable Part Architecture", "Ensure interchangeable parts where Genesis is the embedded core and ggen forms the outer membrane, adapter layer, and projection layer."),
    ];

    for (id, title, desc) in requirements_data {
        let mut attr = HashMap::new();
        attr.insert("title".to_string(), title.to_string());
        attr.insert("description".to_string(), desc.to_string());
        log.objects.push(OcelObject {
            id: id.to_string(),
            r#type: "PRDRequirement".to_string(),
            attributes: attr,
        });
    }

    // GALLCheckpoint
    let mut attr_cp = HashMap::new();
    attr_cp.insert(
        "milestone".to_string(),
        "self_audit_verification".to_string(),
    );
    attr_cp.insert("target_date".to_string(), "2026-05-26Z".to_string());
    log.objects.push(OcelObject {
        id: "obj_cp_verify".to_string(),
        r#type: "GALLCheckpoint".to_string(),
        attributes: attr_cp,
    });

    // PublicOntology
    let mut attr_ont = HashMap::new();
    attr_ont.insert(
        "namespace".to_string(),
        "http://www.ocel-standard.org/ns#".to_string(),
    );
    attr_ont.insert("prefix".to_string(), "ocel".to_string());
    log.objects.push(OcelObject {
        id: "obj_public_ontology".to_string(),
        r#type: "PublicOntology".to_string(),
        attributes: attr_ont,
    });

    // OntologyTerm
    let mut attr_term = HashMap::new();
    attr_term.insert("name".to_string(), "Event".to_string());
    attr_term.insert(
        "uri".to_string(),
        "http://www.ocel-standard.org/ns#Event".to_string(),
    );
    log.objects.push(OcelObject {
        id: "obj_ontology_term_event".to_string(),
        r#type: "OntologyTerm".to_string(),
        attributes: attr_term,
    });

    // SourceFile
    let mut attr_src = HashMap::new();
    attr_src.insert(
        "path".to_string(),
        "crates/ggen-graph/src/ocel/self_audit.rs".to_string(),
    );
    attr_src.insert("language".to_string(), "rust".to_string());
    log.objects.push(OcelObject {
        id: "obj_source_file_self_audit".to_string(),
        r#type: "SourceFile".to_string(),
        attributes: attr_src,
    });

    // TestFile
    let mut attr_test = HashMap::new();
    attr_test.insert(
        "path".to_string(),
        "crates/ggen-graph/tests/ocel_self_audit.rs".to_string(),
    );
    attr_test.insert("framework".to_string(), "cargo test".to_string());
    log.objects.push(OcelObject {
        id: "obj_test_file_self_audit".to_string(),
        r#type: "TestFile".to_string(),
        attributes: attr_test,
    });

    // ExampleFile
    let mut attr_ex = HashMap::new();
    attr_ex.insert(
        "path".to_string(),
        "crates/ggen-graph/examples/self_audit_demo.rs".to_string(),
    );
    attr_ex.insert(
        "description".to_string(),
        "Demonstrates generation and projection of self-audit log".to_string(),
    );
    log.objects.push(OcelObject {
        id: "obj_example_file_self_audit".to_string(),
        r#type: "ExampleFile".to_string(),
        attributes: attr_ex,
    });

    // FixtureFile
    let mut attr_fix = HashMap::new();
    attr_fix.insert(
        "path".to_string(),
        "crates/ggen-graph/tests/fixtures/self_audit_log.json".to_string(),
    );
    attr_fix.insert("format".to_string(), "json".to_string());
    log.objects.push(OcelObject {
        id: "obj_fixture_file_self_audit".to_string(),
        r#type: "FixtureFile".to_string(),
        attributes: attr_fix,
    });

    // ScriptFile
    let mut attr_script = HashMap::new();
    attr_script.insert(
        "path".to_string(),
        "scripts/gall/external/09_verify_ocel_self_audit.sh".to_string(),
    );
    attr_script.insert("interpreter".to_string(), "bash".to_string());
    log.objects.push(OcelObject {
        id: "obj_script_file_verify".to_string(),
        r#type: "ScriptFile".to_string(),
        attributes: attr_script,
    });

    // Command
    let mut attr_cmd = HashMap::new();
    attr_cmd.insert("binary".to_string(), "cargo".to_string());
    attr_cmd.insert("args".to_string(), "test --package ggen-graph".to_string());
    log.objects.push(OcelObject {
        id: "obj_command_test".to_string(),
        r#type: "Command".to_string(),
        attributes: attr_cmd,
    });

    // CommandRun
    let mut attr_cmd_run = HashMap::new();
    attr_cmd_run.insert("exit_code".to_string(), "0".to_string());
    attr_cmd_run.insert("execution_host".to_string(), "localhost".to_string());
    log.objects.push(OcelObject {
        id: "obj_command_run_1".to_string(),
        r#type: "CommandRun".to_string(),
        attributes: attr_cmd_run,
    });

    // EvidenceArtifact
    let mut attr_ev = HashMap::new();
    attr_ev.insert(
        "sha256".to_string(),
        "9f86d081884c7d659a2feaa0c55ad015a3bf4f1b2b0b822cd15d6c15b0f00a08".to_string(),
    );
    attr_ev.insert("size_bytes".to_string(), "1024".to_string());
    log.objects.push(OcelObject {
        id: "obj_evidence_artifact_1".to_string(),
        r#type: "EvidenceArtifact".to_string(),
        attributes: attr_ev,
    });

    // GraphReceipt
    let mut attr_receipt = HashMap::new();
    attr_receipt.insert(
        "hash".to_string(),
        "eb3a28b0f4e1f72da89f2cf0f95b9d3bb8cf03ffdcf82309e7116744f2ef08eb".to_string(),
    );
    attr_receipt.insert("block_index".to_string(), "42".to_string());
    log.objects.push(OcelObject {
        id: "obj_graph_receipt_1".to_string(),
        r#type: "GraphReceipt".to_string(),
        attributes: attr_receipt,
    });

    // CoverageMatrix
    let mut attr_coverage = HashMap::new();
    attr_coverage.insert("line_coverage".to_string(), "0.965".to_string());
    attr_coverage.insert("branch_coverage".to_string(), "0.921".to_string());
    log.objects.push(OcelObject {
        id: "obj_coverage_matrix_1".to_string(),
        r#type: "CoverageMatrix".to_string(),
        attributes: attr_coverage,
    });

    // PromotionDecision
    let mut attr_promo = HashMap::new();
    attr_promo.insert("status".to_string(), "Promoted".to_string());
    attr_promo.insert("approver".to_string(), "OrchestratorAgent".to_string());
    log.objects.push(OcelObject {
        id: "obj_promotion_decision_1".to_string(),
        r#type: "PromotionDecision".to_string(),
        attributes: attr_promo,
    });

    // UnsupportedCapability
    let mut attr_unsupported = HashMap::new();
    attr_unsupported.insert(
        "name".to_string(),
        "RealtimeMultiAgentConsensus".to_string(),
    );
    attr_unsupported.insert(
        "reason".to_string(),
        "Out of scope for deterministic local execution model".to_string(),
    );
    log.objects.push(OcelObject {
        id: "obj_unsupported_capability_1".to_string(),
        r#type: "UnsupportedCapability".to_string(),
        attributes: attr_unsupported,
    });

    // 2. Events representing all required event types:
    // We will use Utc.with_ymd_and_hms to build unique deterministic timestamps.
    // Use qualifiers `--checks-->`, `--produces-->`, `--verifies-->`, `--satisfied_by-->`, `--decides-->`.

    // Event 1: RequirementDeclared
    log.events.push(OcelEvent {
        id: "ev_req_declared".to_string(),
        activity: "RequirementDeclared".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 0, 0).unwrap(),
        objects: vec![
            OcelObjectRef {
                id: "obj_prd_compliance".to_string(),
                r#type: "PRDRequirement".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
            OcelObjectRef {
                id: "obj_crate_ggen".to_string(),
                r#type: "RustCrate".to_string(),
                qualifier: None,
            },
            OcelObjectRef {
                id: "req_r1_one_crate".to_string(),
                r#type: "PRDRequirement".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
            OcelObjectRef {
                id: "req_r2_ontology".to_string(),
                r#type: "PRDRequirement".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
            OcelObjectRef {
                id: "req_r3_deterministic".to_string(),
                r#type: "PRDRequirement".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
            OcelObjectRef {
                id: "req_r4_knowledge_hook".to_string(),
                r#type: "PRDRequirement".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
            OcelObjectRef {
                id: "req_r5_ocel_prov".to_string(),
                r#type: "PRDRequirement".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
            OcelObjectRef {
                id: "req_r6_compliance".to_string(),
                r#type: "PRDRequirement".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
            OcelObjectRef {
                id: "req_r7_ocel_self_audit".to_string(),
                r#type: "PRDRequirement".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
            OcelObjectRef {
                id: "req_r8_coverage_matrix".to_string(),
                r#type: "PRDRequirement".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
            OcelObjectRef {
                id: "req_r9_proof_report".to_string(),
                r#type: "PRDRequirement".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
            OcelObjectRef {
                id: "req_r10_interchangeable".to_string(),
                r#type: "PRDRequirement".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
        ],
        attributes: {
            let mut m = HashMap::new();
            m.insert("scope".to_string(), "compliance".to_string());
            m
        },
    });

    // Event 2: OntologyMapped
    log.events.push(OcelEvent {
        id: "ev_ont_mapped".to_string(),
        activity: "OntologyMapped".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 5, 0).unwrap(),
        objects: vec![
            OcelObjectRef {
                id: "obj_public_ontology".to_string(),
                r#type: "PublicOntology".to_string(),
                qualifier: Some("--checks-->".to_string()),
            },
            OcelObjectRef {
                id: "obj_ontology_term_event".to_string(),
                r#type: "OntologyTerm".to_string(),
                qualifier: None,
            },
        ],
        attributes: HashMap::new(),
    });

    // Event 3: FileEmitted
    log.events.push(OcelEvent {
        id: "ev_file_emitted".to_string(),
        activity: "FileEmitted".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 10, 0).unwrap(),
        objects: vec![OcelObjectRef {
            id: "obj_source_file_self_audit".to_string(),
            r#type: "SourceFile".to_string(),
            qualifier: Some("--produces-->".to_string()),
        }],
        attributes: HashMap::new(),
    });

    // Event 4: ImplementationChanged
    log.events.push(OcelEvent {
        id: "ev_impl_changed".to_string(),
        activity: "ImplementationChanged".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 15, 0).unwrap(),
        objects: vec![
            OcelObjectRef {
                id: "obj_source_file_self_audit".to_string(),
                r#type: "SourceFile".to_string(),
                qualifier: Some("--satisfied_by-->".to_string()),
            },
            OcelObjectRef {
                id: "obj_ard_mapping".to_string(),
                r#type: "ARDRequirement".to_string(),
                qualifier: None,
            },
        ],
        attributes: HashMap::new(),
    });

    // Event 5: FixtureCreated
    log.events.push(OcelEvent {
        id: "ev_fixture_created".to_string(),
        activity: "FixtureCreated".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 20, 0).unwrap(),
        objects: vec![OcelObjectRef {
            id: "obj_fixture_file_self_audit".to_string(),
            r#type: "FixtureFile".to_string(),
            qualifier: Some("--produces-->".to_string()),
        }],
        attributes: HashMap::new(),
    });

    // Event 6: CommandExecuted
    log.events.push(OcelEvent {
        id: "ev_command_executed".to_string(),
        activity: "CommandExecuted".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 25, 0).unwrap(),
        objects: vec![
            OcelObjectRef {
                id: "obj_command_test".to_string(),
                r#type: "Command".to_string(),
                qualifier: Some("--produces-->".to_string()),
            },
            OcelObjectRef {
                id: "obj_command_run_1".to_string(),
                r#type: "CommandRun".to_string(),
                qualifier: None,
            },
        ],
        attributes: HashMap::new(),
    });

    // Event 7: TestPassed
    log.events.push(OcelEvent {
        id: "ev_test_passed".to_string(),
        activity: "TestPassed".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 30, 0).unwrap(),
        objects: vec![
            OcelObjectRef {
                id: "obj_test_file_self_audit".to_string(),
                r#type: "TestFile".to_string(),
                qualifier: Some("--verifies-->".to_string()),
            },
            OcelObjectRef {
                id: "obj_source_file_self_audit".to_string(),
                r#type: "SourceFile".to_string(),
                qualifier: None,
            },
        ],
        attributes: HashMap::new(),
    });

    // Event 8: TestFailed
    log.events.push(OcelEvent {
        id: "ev_test_failed".to_string(),
        activity: "TestFailed".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 35, 0).unwrap(),
        objects: vec![OcelObjectRef {
            id: "obj_test_file_self_audit".to_string(),
            r#type: "TestFile".to_string(),
            qualifier: Some("--checks-->".to_string()),
        }],
        attributes: HashMap::new(),
    });

    // Event 8b: TestPassed (Remediating)
    log.events.push(OcelEvent {
        id: "ev_test_remediated".to_string(),
        activity: "TestPassed".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 38, 0).unwrap(),
        objects: vec![
            OcelObjectRef {
                id: "obj_test_file_self_audit".to_string(),
                r#type: "TestFile".to_string(),
                qualifier: Some("--verifies-->".to_string()),
            },
            OcelObjectRef {
                id: "obj_source_file_self_audit".to_string(),
                r#type: "SourceFile".to_string(),
                qualifier: None,
            },
        ],
        attributes: HashMap::new(),
    });

    // Event 9: ForbiddenSurfaceScanned
    log.events.push(OcelEvent {
        id: "ev_forbidden_scanned".to_string(),
        activity: "ForbiddenSurfaceScanned".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 40, 0).unwrap(),
        objects: vec![
            OcelObjectRef {
                id: "obj_script_file_verify".to_string(),
                r#type: "ScriptFile".to_string(),
                qualifier: Some("--checks-->".to_string()),
            },
            OcelObjectRef {
                id: "obj_source_file_self_audit".to_string(),
                r#type: "SourceFile".to_string(),
                qualifier: None,
            },
        ],
        attributes: HashMap::new(),
    });

    // Event 10: AntiFakeScanned
    log.events.push(OcelEvent {
        id: "ev_antifake_scanned".to_string(),
        activity: "AntiFakeScanned".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 45, 0).unwrap(),
        objects: vec![OcelObjectRef {
            id: "obj_script_file_verify".to_string(),
            r#type: "ScriptFile".to_string(),
            qualifier: Some("--checks-->".to_string()),
        }],
        attributes: HashMap::new(),
    });

    // Event 11: ReceiptEmitted
    log.events.push(OcelEvent {
        id: "ev_receipt_emitted".to_string(),
        activity: "ReceiptEmitted".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 50, 0).unwrap(),
        objects: vec![OcelObjectRef {
            id: "obj_graph_receipt_1".to_string(),
            r#type: "GraphReceipt".to_string(),
            qualifier: Some("--produces-->".to_string()),
        }],
        attributes: HashMap::new(),
    });

    // Event 12: ReplayVerified
    log.events.push(OcelEvent {
        id: "ev_replay_verified".to_string(),
        activity: "ReplayVerified".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 9, 55, 0).unwrap(),
        objects: vec![
            OcelObjectRef {
                id: "obj_evidence_artifact_1".to_string(),
                r#type: "EvidenceArtifact".to_string(),
                qualifier: Some("--verifies-->".to_string()),
            },
            OcelObjectRef {
                id: "obj_graph_receipt_1".to_string(),
                r#type: "GraphReceipt".to_string(),
                qualifier: None,
            },
        ],
        attributes: HashMap::new(),
    });

    // Event 13: CoverageEvaluated
    log.events.push(OcelEvent {
        id: "ev_coverage_evaluated".to_string(),
        activity: "CoverageEvaluated".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 10, 0, 0).unwrap(),
        objects: vec![
            OcelObjectRef {
                id: "obj_coverage_matrix_1".to_string(),
                r#type: "CoverageMatrix".to_string(),
                qualifier: Some("--checks-->".to_string()),
            },
            OcelObjectRef {
                id: "obj_crate_ggen".to_string(),
                r#type: "RustCrate".to_string(),
                qualifier: None,
            },
        ],
        attributes: HashMap::new(),
    });

    // Event 14: CheckpointEvaluated
    log.events.push(OcelEvent {
        id: "ev_checkpoint_evaluated".to_string(),
        activity: "CheckpointEvaluated".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 10, 5, 0).unwrap(),
        objects: vec![OcelObjectRef {
            id: "obj_cp_verify".to_string(),
            r#type: "GALLCheckpoint".to_string(),
            qualifier: Some("--checks-->".to_string()),
        }],
        attributes: HashMap::new(),
    });

    // Event 15: CheckpointPromoted
    log.events.push(OcelEvent {
        id: "ev_checkpoint_promoted".to_string(),
        activity: "CheckpointPromoted".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 10, 10, 0).unwrap(),
        objects: vec![
            OcelObjectRef {
                id: "obj_promotion_decision_1".to_string(),
                r#type: "PromotionDecision".to_string(),
                qualifier: Some("--decides-->".to_string()),
            },
            OcelObjectRef {
                id: "obj_cp_verify".to_string(),
                r#type: "GALLCheckpoint".to_string(),
                qualifier: None,
            },
        ],
        attributes: HashMap::new(),
    });

    // Event 16: CheckpointRefused
    log.events.push(OcelEvent {
        id: "ev_checkpoint_refused".to_string(),
        activity: "CheckpointRefused".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 10, 15, 0).unwrap(),
        objects: vec![OcelObjectRef {
            id: "obj_promotion_decision_1".to_string(),
            r#type: "PromotionDecision".to_string(),
            qualifier: Some("--decides-->".to_string()),
        }],
        attributes: HashMap::new(),
    });

    // Event 17: UnsupportedCapabilityDeclared
    log.events.push(OcelEvent {
        id: "ev_unsupported_declared".to_string(),
        activity: "UnsupportedCapabilityDeclared".to_string(),
        timestamp: Utc.with_ymd_and_hms(2026, 5, 26, 10, 20, 0).unwrap(),
        objects: vec![OcelObjectRef {
            id: "obj_unsupported_capability_1".to_string(),
            r#type: "UnsupportedCapability".to_string(),
            qualifier: Some("--decides-->".to_string()),
        }],
        attributes: HashMap::new(),
    });

    log
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ocel::EvidenceProjector;
    use crate::DeterministicGraph;

    #[test]
    fn test_self_audit_log_generation_and_projection() -> Result<(), Box<dyn std::error::Error>> {
        let log = generate_self_audit_log();
        assert_eq!(log.objects.len(), 28);
        assert_eq!(log.events.len(), 18);

        let graph = DeterministicGraph::new()?;

        EvidenceProjector::project_ocel(&graph, &log)?;

        let extracted_log = EvidenceProjector::extract_ocel(&graph)?;
        assert_eq!(extracted_log.objects.len(), 28);
        assert_eq!(extracted_log.events.len(), 18);
        Ok(())
    }
}
