//! Requirement-to-evidence coverage mapping for ggen self-audit.

use serde::{Deserialize, Serialize};

/// Evidence linking a requirement to its implementation files, test files, and verification commands.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RequirementEvidence {
    /// Unique identifier of the requirement (e.g. "R1", "R2", etc.).
    pub id: String,
    /// Human-readable title of the requirement.
    pub title: String,
    /// Detailed description of the requirement.
    pub description: String,
    /// Source files that implement this requirement.
    pub source_files: Vec<String>,
    /// Test files that verify this requirement.
    pub test_files: Vec<String>,
    /// Commands that can be executed to verify this requirement.
    pub commands: Vec<String>,
}

/// Matrix mapping all project requirements to their verification evidence.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CoverageMatrix {
    /// List of requirement evidences in the matrix.
    pub requirements: Vec<RequirementEvidence>,
}

/// Generates the standard coverage matrix mapping all 9 requirements.
pub fn generate_coverage_matrix() -> CoverageMatrix {
    CoverageMatrix {
        requirements: vec![
            RequirementEvidence {
                id: "req_r1_one_crate".to_string(),
                title: "One-Crate Package Boundary and No Feature Flags".to_string(),
                description: "The substrate must be implemented as a single standalone Rust package at crates/ggen-graph with no feature flags.".to_string(),
                source_files: vec![
                    "crates/ggen-graph/Cargo.toml".to_string(),
                    "crates/ggen-graph/src/lib.rs".to_string(),
                ],
                test_files: vec![
                    "crates/ggen-graph/tests/anti_fake_implementation.rs".to_string(),
                ],
                commands: vec![
                    "cargo check -p ggen-graph --all-targets".to_string(),
                ],
            },
            RequirementEvidence {
                id: "req_r2_ontology".to_string(),
                title: "Public Ontology Governance and Vocabulary Emission".to_string(),
                description: "Vocabulary constants must be emitted directly from public ontology profiles.".to_string(),
                source_files: vec![
                    "crates/ggen-graph/src/vocab/mod.rs".to_string(),
                ],
                test_files: vec![
                    "crates/ggen-graph/tests/vocab_projection.rs".to_string(),
                ],
                commands: vec![
                    "cargo test -p ggen-graph --test vocab_projection".to_string(),
                ],
            },
            RequirementEvidence {
                id: "req_r3_deterministic".to_string(),
                title: "Deterministic Graph Substrate and Delta Conservation".to_string(),
                description: "Graph parsing, serialization, canonical byte generation, hashing, and RdfDelta transitions must be deterministic.".to_string(),
                source_files: vec![
                    "crates/ggen-graph/src/graph/mod.rs".to_string(),
                    "crates/ggen-graph/src/delta/mod.rs".to_string(),
                ],
                test_files: vec![
                    "crates/ggen-graph/tests/delta_determinism.rs".to_string(),
                    "crates/ggen-graph/tests/hash_stability.rs".to_string(),
                ],
                commands: vec![
                    "cargo test -p ggen-graph --test delta_determinism".to_string(),
                    "cargo test -p ggen-graph --test hash_stability".to_string(),
                ],
            },
            RequirementEvidence {
                id: "req_r4_knowledge_hook".to_string(),
                title: "Knowledge Hook Runtime & Deterministic Scheduling".to_string(),
                description: "Turtle-based hook packs must load dynamically as graph laws and schedule deterministically.".to_string(),
                source_files: vec![
                    "crates/ggen-graph/src/diagnostics/mod.rs".to_string(),
                ],
                test_files: vec![
                    "crates/ggen-graph/tests/hook_loader.rs".to_string(),
                    "crates/ggen-graph/tests/hook_scheduler.rs".to_string(),
                    "crates/ggen-graph/tests/sparql_actuation.rs".to_string(),
                ],
                commands: vec![
                    "cargo test -p ggen-graph --test hook_loader".to_string(),
                    "cargo test -p ggen-graph --test hook_scheduler".to_string(),
                    "cargo test -p ggen-graph --test sparql_actuation".to_string(),
                ],
            },
            RequirementEvidence {
                id: "req_r5_ocel_prov".to_string(),
                title: "OCEL/PROV Evidence Projection & Replayable Receipts".to_string(),
                description: "Every graph transition must produce a replayable cryptographic receipt bundle and project PROV-O/OCEL.".to_string(),
                source_files: vec![
                    "crates/ggen-graph/src/ocel/projection.rs".to_string(),
                    "crates/ggen-graph/src/receipt/mod.rs".to_string(),
                ],
                test_files: vec![
                    "crates/ggen-graph/tests/receipt_replay.rs".to_string(),
                    "crates/ggen-graph/tests/ocel_diagnostics_doctor_test.rs".to_string(),
                ],
                commands: vec![
                    "cargo test -p ggen-graph --test receipt_replay".to_string(),
                    "cargo test -p ggen-graph --test ocel_diagnostics_doctor_test".to_string(),
                ],
            },
            RequirementEvidence {
                id: "req_r6_compliance".to_string(),
                title: "Compliance Checks & Anti-Fake Gates".to_string(),
                description: "Must pass structural analyses checking for forbidden execution surfaces and zero placeholder/stub/mock/fake-success patterns.".to_string(),
                source_files: vec![
                    "crates/ggen-graph/src/doctor/mod.rs".to_string(),
                ],
                test_files: vec![
                    "crates/ggen-graph/tests/anti_fake_implementation.rs".to_string(),
                    "crates/ggen-graph/tests/forbidden_surface.rs".to_string(),
                ],
                commands: vec![
                    "cargo test -p ggen-graph --test anti_fake_implementation".to_string(),
                    "cargo test -p ggen-graph --test forbidden_surface".to_string(),
                ],
            },
            RequirementEvidence {
                id: "req_r7_ocel_self_audit".to_string(),
                title: "OCEL v2 Self-Audit Log Emission".to_string(),
                description: "Implement self-audit log generator emitting vision2030.self_audit.ocel.json.".to_string(),
                source_files: vec![
                    "crates/ggen-graph/src/ocel/self_audit.rs".to_string(),
                    "crates/ggen-graph/src/ocel/gall_projection.rs".to_string(),
                ],
                test_files: vec![
                    "crates/ggen-graph/tests/ocel_self_audit.rs".to_string(),
                ],
                commands: vec![
                    "cargo run -p ggen-graph --bin emit_audit".to_string(),
                ],
            },
            RequirementEvidence {
                id: "req_r8_coverage_matrix".to_string(),
                title: "Coverage Matrix & Verification Scripts".to_string(),
                description: "Implement coverage mapping in coverage.rs and verify via scripts.".to_string(),
                source_files: vec![
                    "crates/ggen-graph/src/ocel/coverage.rs".to_string(),
                    "crates/ggen-graph/src/bin/verify_audit.rs".to_string(),
                ],
                test_files: vec![
                    "crates/ggen-graph/tests/vision2030_coverage.rs".to_string(),
                ],
                commands: vec![
                    "cargo run -p ggen-graph --bin verify_audit".to_string(),
                ],
            },
            RequirementEvidence {
                id: "req_r9_proof_report".to_string(),
                title: "Derived GALL Checkpoint Proof Report".to_string(),
                description: "Vision 2030 GALL Checkpoint Proof Report derived from self-audit log.".to_string(),
                source_files: vec![
                    "docs/VISION_2030_GALL_PROOF.md".to_string(),
                ],
                test_files: vec![
                    "crates/ggen-graph/tests/ocel_self_audit.rs".to_string(),
                ],
                commands: vec![
                    "cargo test -p ggen-graph --test ocel_self_audit".to_string(),
                ],
            },
            RequirementEvidence {
                id: "req_r10_interchangeable".to_string(),
                title: "Genesis-bearing Interchangeable Part Architecture".to_string(),
                description: "Ensure interchangeable parts where Genesis is the embedded core and ggen forms the outer membrane, adapter layer, and projection layer.".to_string(),
                source_files: vec![
                    "crates/ggen-graph/src/interchangeable.rs".to_string(),
                ],
                test_files: vec![
                    "crates/ggen-graph/tests/interchangeable_test.rs".to_string(),
                ],
                commands: vec![
                    "cargo test -p ggen-graph --test interchangeable_test".to_string(),
                ],
            },
        ],
    }
}
