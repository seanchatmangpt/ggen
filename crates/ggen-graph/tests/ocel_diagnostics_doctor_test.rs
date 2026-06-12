#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
use chrono::{TimeZone, Utc};
use ggen_graph::diagnostics::{DiagnosticStatus, DiagnosticsRunner};
use ggen_graph::doctor::ProcessDoctor;
use ggen_graph::ocel::{
    EvidenceProjector, OcelEvent, OcelLog, OcelObject, OcelObjectRef, ProvActivity, ProvAgent,
    ProvDocument, ProvEntity, ProvGeneration, ProvUsage,
};
use ggen_graph::DeterministicGraph;
use std::collections::HashMap;

#[test]
fn test_ocel_roundtrip_and_diagnostics_flow() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Create a clean graph
    let graph = DeterministicGraph::new()?;

    // 2. Define an OCEL Log representing a standard process run
    let mut log = OcelLog::new();

    // Objects
    log.objects.push(OcelObject {
        id: "artifact-123".to_string(),
        r#type: "Artifact".to_string(),
        attributes: {
            let mut m = HashMap::new();
            m.insert("name".to_string(), "revenue-ops-spec".to_string());
            m
        },
    });

    // Events
    let ts1 = Utc.with_ymd_and_hms(2026, 5, 26, 12, 0, 0).unwrap();
    let ts2 = Utc.with_ymd_and_hms(2026, 5, 26, 12, 10, 0).unwrap();
    let ts3 = Utc.with_ymd_and_hms(2026, 5, 26, 12, 20, 0).unwrap();

    log.events.push(OcelEvent {
        id: "event-1".to_string(),
        activity: "CreatedOnly".to_string(),
        timestamp: ts1,
        objects: vec![OcelObjectRef {
            id: "artifact-123".to_string(),
            r#type: "Artifact".to_string(),
            qualifier: Some("input".to_string()),
        }],
        attributes: HashMap::new(),
    });

    log.events.push(OcelEvent {
        id: "event-2".to_string(),
        activity: "ArtifactEmitted".to_string(),
        timestamp: ts2,
        objects: vec![OcelObjectRef {
            id: "artifact-123".to_string(),
            r#type: "Artifact".to_string(),
            qualifier: Some("output".to_string()),
        }],
        attributes: HashMap::new(),
    });

    log.events.push(OcelEvent {
        id: "event-3".to_string(),
        activity: "Closed".to_string(),
        timestamp: ts3,
        objects: vec![OcelObjectRef {
            id: "artifact-123".to_string(),
            r#type: "Artifact".to_string(),
            qualifier: None,
        }],
        attributes: HashMap::new(),
    });

    // 3. Project OCEL to RDF (Boundary Crossing)
    EvidenceProjector::project_ocel(&graph, &log)?;

    // 4. Extract and check round-trip equivalence
    let extracted_log = EvidenceProjector::extract_ocel(&graph)?;
    assert_eq!(extracted_log.objects.len(), 1);
    assert_eq!(extracted_log.events.len(), 3);
    assert_eq!(extracted_log.objects[0].id, "artifact-123");
    assert_eq!(extracted_log.events[0].activity, "CreatedOnly");
    assert_eq!(extracted_log.events[1].activity, "ArtifactEmitted");
    assert_eq!(extracted_log.events[2].activity, "Closed");

    // 5. Run Process Doctor (Conformance checks)
    let expected_seq = vec![
        "CreatedOnly".to_string(),
        "ArtifactEmitted".to_string(),
        "Closed".to_string(),
    ];
    let report = ProcessDoctor::diagnose(&graph, &expected_seq)?;
    assert!(report.conforms);
    assert_eq!(report.fitness, 1.0);
    assert!(report.deviations.is_empty());

    // 6. Test negative case: process with skipped stage
    let graph_skipped = DeterministicGraph::new()?;
    let mut log_skipped = log.clone();
    // Remove the intermediate step "ArtifactEmitted"
    log_skipped.events.remove(1);
    EvidenceProjector::project_ocel(&graph_skipped, &log_skipped)?;

    let report_skipped = ProcessDoctor::diagnose(&graph_skipped, &expected_seq)?;
    assert!(!report_skipped.conforms);
    assert_eq!(report_skipped.fitness, 0.0);
    assert!(!report_skipped.deviations.is_empty());
    assert!(report_skipped
        .deviations
        .iter()
        .any(|d| d.description.contains("Skipped stage")));

    // 7. Run general diagnostics suite
    let diag_report = DiagnosticsRunner::run_diagnostics(&graph);
    assert_eq!(diag_report.overall_status, DiagnosticStatus::Ok);

    Ok(())
}

#[test]
fn test_prov_roundtrip() -> Result<(), Box<dyn std::error::Error>> {
    let graph = DeterministicGraph::new()?;
    let mut doc = ProvDocument::new();

    doc.entities.push(ProvEntity {
        id: "spec.ttl".to_string(),
        attributes: HashMap::new(),
    });

    doc.activities.push(ProvActivity {
        id: "compile-spec".to_string(),
        start_time: Some(Utc.with_ymd_and_hms(2026, 5, 26, 12, 0, 0).unwrap()),
        end_time: Some(Utc.with_ymd_and_hms(2026, 5, 26, 12, 1, 0).unwrap()),
        attributes: HashMap::new(),
    });

    doc.agents.push(ProvAgent {
        id: "antigravity-agent".to_string(),
        attributes: HashMap::new(),
    });

    doc.generations.push(ProvGeneration {
        entity: "spec.ttl".to_string(),
        activity: "compile-spec".to_string(),
    });

    doc.usages.push(ProvUsage {
        activity: "compile-spec".to_string(),
        entity: "spec.ttl".to_string(),
    });

    // Project PROV (Boundary Crossing)
    EvidenceProjector::project_prov(&graph, &doc)?;

    // Extract PROV
    let extracted_doc = EvidenceProjector::extract_prov(&graph)?;
    assert_eq!(extracted_doc.entities.len(), 1);
    assert_eq!(extracted_doc.activities.len(), 1);
    assert_eq!(extracted_doc.agents.len(), 1);
    assert_eq!(extracted_doc.generations.len(), 1);
    assert_eq!(extracted_doc.usages.len(), 1);

    assert_eq!(extracted_doc.entities[0].id, "spec.ttl");
    assert_eq!(extracted_doc.activities[0].id, "compile-spec");
    assert_eq!(extracted_doc.agents[0].id, "antigravity-agent");

    Ok(())
}
