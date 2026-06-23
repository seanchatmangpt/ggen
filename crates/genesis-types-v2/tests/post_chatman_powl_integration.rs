//! Integration tests for post-Chatman POWL process types.
//!
//! Tests the full admission flow end-to-end:
//!   build graph → validate structural integrity → compute gate status → emit report → roundtrip JSON
//!
//! Chicago TDD: real data structures, real computation, state-based assertions.
//! No mocks, no test doubles. Panics inside `#[test]` are intentional failure signals.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use genesis_types::{
    AdmissionStatus, GateResult, PowlEdge, PowlGraph, PowlNode, ProcessAdmissionReport,
};

// ─── full VisionSnapLoop admission flow ───────────────────────────────────────

#[test]
fn test_vision_snap_loop_admission_full_flow() {
    // Arrange: build a VisionSnapLoop-style POWL graph with a repair loop
    let graph = PowlGraph {
        id: "vision-snap-loop-001".to_string(),
        name: "VisionSnapLoop".to_string(),
        nodes: vec![
            PowlNode {
                id: "gen".to_string(),
                activity: "GenerateGeometry".to_string(),
                object_refs: vec!["artifact-001".to_string()],
                guard: None,
            },
            PowlNode {
                id: "render".to_string(),
                activity: "RenderProjection".to_string(),
                object_refs: vec![],
                guard: None,
            },
            PowlNode {
                id: "measure".to_string(),
                activity: "MeasureVisualGap".to_string(),
                object_refs: vec![],
                guard: None,
            },
            PowlNode {
                id: "repair".to_string(),
                activity: "SelectBoundedRepairOperator".to_string(),
                object_refs: vec![],
                guard: Some("residual > threshold".to_string()),
            },
            PowlNode {
                id: "emit".to_string(),
                activity: "EmitReceipt".to_string(),
                object_refs: vec![],
                guard: None,
            },
        ],
        edges: vec![
            PowlEdge {
                from: "gen".to_string(),
                to: "render".to_string(),
                condition: None,
            },
            PowlEdge {
                from: "render".to_string(),
                to: "measure".to_string(),
                condition: None,
            },
            PowlEdge {
                from: "measure".to_string(),
                to: "repair".to_string(),
                condition: Some("gap > 0.1".to_string()),
            },
            PowlEdge {
                from: "repair".to_string(),
                to: "gen".to_string(), // loop back
                condition: None,
            },
            PowlEdge {
                from: "measure".to_string(),
                to: "emit".to_string(),
                condition: Some("gap <= 0.1".to_string()),
            },
        ],
        root: "gen".to_string(),
        sinks: vec!["emit".to_string()],
    };

    // Act: validate structural integrity
    let violations = graph.validate();
    assert!(
        violations.is_empty(),
        "VisionSnapLoop graph should be structurally valid; violations: {violations:?}"
    );

    // Act: build gate results (all Alive)
    let gates = vec![
        GateResult {
            gate_id: "source-law".to_string(),
            gate_name: "Source Law Integrity".to_string(),
            status: AdmissionStatus::Alive,
            detail: "all_merged.ttl regenerates cleanly".to_string(),
            evidence_hash: Some("abc123def456".to_string()),
        },
        GateResult {
            gate_id: "fresh-render".to_string(),
            gate_name: "Fresh Render Guarantee".to_string(),
            status: AdmissionStatus::Alive,
            detail: "render hash verified against receipt".to_string(),
            evidence_hash: Some("def456abc123".to_string()),
        },
    ];

    // Act: compute overall status from gates
    let status = ProcessAdmissionReport::compute_status(&gates);
    assert_eq!(
        status,
        AdmissionStatus::Alive,
        "all-Alive gates must produce Alive status"
    );

    // Act: build full report
    let report = ProcessAdmissionReport {
        operation_id: uuid::Uuid::new_v4().to_string(),
        graph_id: graph.id.clone(),
        status,
        gates,
        receipt_hash: None,
        timestamp: chrono::Utc::now(),
    };

    // Assert: report reflects correct shape and state
    assert!(
        report.status.is_admitted(),
        "all-Alive report must be admitted"
    );
    assert_eq!(report.graph_id, "vision-snap-loop-001");
    assert_eq!(report.gates.len(), 2);
    assert!(
        !report.operation_id.is_empty(),
        "operation_id must be non-empty"
    );

    // Assert: evidence hashes are preserved
    assert_eq!(
        report.gates[0].evidence_hash.as_deref(),
        Some("abc123def456")
    );
    assert_eq!(
        report.gates[1].evidence_hash.as_deref(),
        Some("def456abc123")
    );
}

// ─── PartialAlive propagates when one gate is partial ─────────────────────────

#[test]
fn test_partial_alive_propagates_when_one_gate_partial() {
    let gates = vec![
        GateResult {
            gate_id: "g1".to_string(),
            gate_name: "Source Law Integrity".to_string(),
            status: AdmissionStatus::Alive,
            detail: "ok".to_string(),
            evidence_hash: None,
        },
        GateResult {
            gate_id: "g2".to_string(),
            gate_name: "Render Freshness".to_string(),
            status: AdmissionStatus::PartialAlive,
            detail: "repair underway — 1 of 2 dimensions passing".to_string(),
            evidence_hash: None,
        },
    ];

    let status = ProcessAdmissionReport::compute_status(&gates);
    assert_eq!(
        status,
        AdmissionStatus::PartialAlive,
        "mixed Alive + PartialAlive should compute PartialAlive"
    );
    assert!(!status.is_admitted(), "PartialAlive must not be admitted");
    assert!(
        status.should_continue_repair(),
        "PartialAlive must signal that repair should continue"
    );
}

// ─── Refused dominates all other statuses ─────────────────────────────────────

#[test]
fn test_refused_gate_dominates_alive_gates() {
    let gates = vec![
        GateResult {
            gate_id: "g1".to_string(),
            gate_name: "G1".to_string(),
            status: AdmissionStatus::Alive,
            detail: "ok".to_string(),
            evidence_hash: None,
        },
        GateResult {
            gate_id: "g2".to_string(),
            gate_name: "G2".to_string(),
            status: AdmissionStatus::Refused,
            detail: "ontology hash mismatch detected — admission blocked".to_string(),
            evidence_hash: Some("badhash".to_string()),
        },
        GateResult {
            gate_id: "g3".to_string(),
            gate_name: "G3".to_string(),
            status: AdmissionStatus::PartialAlive,
            detail: "partial".to_string(),
            evidence_hash: None,
        },
    ];

    let status = ProcessAdmissionReport::compute_status(&gates);
    assert_eq!(
        status,
        AdmissionStatus::Refused,
        "a single Refused gate must force overall Refused"
    );
    assert!(!status.is_admitted());
    assert!(!status.should_continue_repair());
}

// ─── graph with loop: validate structural integrity ───────────────────────────

#[test]
fn test_powl_graph_with_loop_is_structurally_valid() {
    // A cycle (repair loop) is not a structural violation in POWL — loops are legal.
    let graph = PowlGraph {
        id: "loop-graph".to_string(),
        name: "RepairLoop".to_string(),
        nodes: vec![
            PowlNode {
                id: "start".to_string(),
                activity: "Start".to_string(),
                object_refs: vec![],
                guard: None,
            },
            PowlNode {
                id: "work".to_string(),
                activity: "DoWork".to_string(),
                object_refs: vec![],
                guard: None,
            },
            PowlNode {
                id: "check".to_string(),
                activity: "CheckResult".to_string(),
                object_refs: vec![],
                guard: None,
            },
            PowlNode {
                id: "done".to_string(),
                activity: "Emit".to_string(),
                object_refs: vec![],
                guard: None,
            },
        ],
        edges: vec![
            PowlEdge {
                from: "start".to_string(),
                to: "work".to_string(),
                condition: None,
            },
            PowlEdge {
                from: "work".to_string(),
                to: "check".to_string(),
                condition: None,
            },
            PowlEdge {
                from: "check".to_string(),
                to: "work".to_string(), // loop
                condition: Some("not_done".to_string()),
            },
            PowlEdge {
                from: "check".to_string(),
                to: "done".to_string(),
                condition: Some("done".to_string()),
            },
        ],
        root: "start".to_string(),
        sinks: vec!["done".to_string()],
    };

    let violations = graph.validate();
    assert!(
        violations.is_empty(),
        "graph with a legal repair loop must be structurally valid; violations: {violations:?}"
    );
}

// ─── report serialises to JSON and back without loss ─────────────────────────

#[test]
fn test_process_admission_report_roundtrip_json() {
    let gates = vec![GateResult {
        gate_id: "roundtrip-gate".to_string(),
        gate_name: "RoundtripCheck".to_string(),
        status: AdmissionStatus::Alive,
        detail: "all clear".to_string(),
        evidence_hash: Some("cafe1234".to_string()),
    }];
    let original = ProcessAdmissionReport {
        operation_id: uuid::Uuid::new_v4().to_string(),
        graph_id: "test-graph-42".to_string(),
        status: AdmissionStatus::Alive,
        gates,
        receipt_hash: Some("deadbeef".to_string()),
        timestamp: chrono::Utc::now(),
    };

    let json = serde_json::to_string(&original).unwrap();
    let restored: ProcessAdmissionReport = serde_json::from_str(&json).unwrap();

    assert_eq!(original.operation_id, restored.operation_id);
    assert_eq!(original.graph_id, restored.graph_id);
    assert_eq!(original.status, restored.status);
    assert_eq!(original.gates.len(), restored.gates.len());
    assert_eq!(restored.gates[0].evidence_hash.as_deref(), Some("cafe1234"));
    assert_eq!(original.receipt_hash, restored.receipt_hash);
}

// ─── full graph validation: duplicate nodes are caught ───────────────────────

#[test]
fn test_powl_graph_full_pipeline_duplicate_node_is_violation() {
    let graph = PowlGraph {
        id: "dup-graph".to_string(),
        name: "DupTest".to_string(),
        nodes: vec![
            PowlNode {
                id: "gen".to_string(),
                activity: "GenerateGeometry".to_string(),
                object_refs: vec![],
                guard: None,
            },
            PowlNode {
                id: "gen".to_string(), // duplicate id
                activity: "GenerateGeometryDuplicate".to_string(),
                object_refs: vec![],
                guard: None,
            },
        ],
        edges: vec![],
        root: "gen".to_string(),
        sinks: vec![],
    };

    let violations = graph.validate();
    assert!(
        violations.iter().any(|v| v.contains("duplicate")),
        "duplicate node id must produce a violation; got: {violations:?}"
    );
}

// ─── Unknown gate produces Unknown overall status ─────────────────────────────

#[test]
fn test_unknown_gate_without_refused_produces_unknown_status() {
    let gates = vec![
        GateResult {
            gate_id: "g1".to_string(),
            gate_name: "G1".to_string(),
            status: AdmissionStatus::PartialAlive,
            detail: "partial".to_string(),
            evidence_hash: None,
        },
        GateResult {
            gate_id: "g2".to_string(),
            gate_name: "G2".to_string(),
            status: AdmissionStatus::Unknown,
            detail: "no evidence available".to_string(),
            evidence_hash: None,
        },
    ];

    let status = ProcessAdmissionReport::compute_status(&gates);
    assert_eq!(
        status,
        AdmissionStatus::Unknown,
        "Unknown gate without Refused should produce Unknown overall"
    );
    assert!(!status.is_admitted());
    assert!(status.should_continue_repair());
}
