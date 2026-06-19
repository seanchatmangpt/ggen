//! Honesty test for reverse-pipeline OCEL persistence.
//!
//! Chicago TDD: real temp filesystem, real NDJSON append, real fold-back read.
//! Proves that `append_reverse_events` actually writes the reverse-pipeline
//! events to the `.ggen/ocel` intel log (not just returns them in memory), and
//! that they round-trip with their activity, subject object, and attributes
//! intact — so the offline miner can see the reverse process.

use ggen_core::reverse::ReverseEvent;
use ggen_lsp::intel::{append_reverse_events, IntelLog};

fn temp_root() -> std::path::PathBuf {
    let uniq = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let root = std::env::temp_dir().join(format!("ggen_reverse_ocel_{}_{}", std::process::id(), uniq));
    std::fs::create_dir_all(&root).expect("create temp root");
    root
}

#[test]
fn reverse_events_persist_and_fold_back() {
    let root = temp_root();

    let events = vec![
        ReverseEvent::new("ReverseScanStarted", "demo", "discovered_graph"),
        ReverseEvent::new("ReverseScanFile", "src/lib.rs", "source_file").with_attr("services", "2"),
        ReverseEvent::new("ReverseScanCompleted", "demo", "discovered_graph")
            .with_attr("triples", "34"),
    ];

    // Act: real append to the real OCEL log.
    append_reverse_events(&root, &events).expect("append reverse events");

    // The canonical log file exists on disk.
    let log = IntelLog::at_root(&root);
    assert!(
        log.path().exists(),
        "OCEL log must exist at {}",
        log.path().display()
    );
    assert!(log.path().ends_with("agent-edit-events.ocel.jsonl"));

    // It folds back to exactly the events we appended, with fields intact.
    let folded = log.read();
    assert_eq!(folded.events.len(), 3, "all three events persisted");

    let file_ev = folded
        .events
        .iter()
        .find(|e| e.activity == "ReverseScanFile")
        .expect("ReverseScanFile event present");
    assert!(
        file_ev
            .objects
            .iter()
            .any(|o| o.id == "src/lib.rs" && o.r#type == "source_file"),
        "event references its subject object with the right type"
    );
    assert_eq!(
        file_ev.attributes.get("services").map(String::as_str),
        Some("2"),
        "event attribute persisted"
    );

    // Event ids are non-empty (deterministic blake3 prefixes), not placeholders.
    assert!(folded.events.iter().all(|e| !e.id.is_empty()));

    let _ = std::fs::remove_dir_all(&root);
}

#[test]
fn empty_events_is_a_noop() {
    // Appending zero events must not error and must not fabricate a log.
    let root = temp_root();
    append_reverse_events(&root, &[]).expect("empty append ok");
    let folded = IntelLog::at_root(&root).read();
    assert_eq!(folded.events.len(), 0, "no events, no entries");
    let _ = std::fs::remove_dir_all(&root);
}
