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
//! TRIAD-STRESS-1 — the route engine holds under simultaneous LSP/MCP/A2A load.
//!
//! Mixed concurrent pressure against one shared root: capture writers (lsp/mcp/a2a
//! transports) contend on the append-only OCEL log while MCP + A2A readers hammer
//! the route engine. Asserts: the log stays uncorrupted (every line parses, no
//! events lost), the same input still resolves to the same route id (no drift),
//! and a sampled case replays cleanly.

use std::fs;
use std::thread;

use ggen_lsp::intel::events::obj_type;
use ggen_lsp::{check_files_in_root, replay_case, Attribution, IntelLog};
use ggen_lsp_a2a::dispatch_tool;
use ggen_lsp_mcp::build_repair_routes_in;
use serde_json::json;
use tempfile::TempDir;

const E0011: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";
const CAPTURES: usize = 15;

#[test]
fn triad_holds_under_concurrent_pressure() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    let rq = root.join("q.rq");
    fs::write(&rq, E0011).expect("write");
    let root_str = root.to_str().expect("utf8");

    // Mixed concurrent load across all three channels against one shared root.
    thread::scope(|s| {
        // Capture writers — log contention, three transports.
        for i in 0..CAPTURES {
            let rq = rq.clone();
            s.spawn(move || {
                let transport = ["lsp", "mcp", "a2a"][i % 3];
                let attr = Attribution::new(format!("agent{i}"), transport, format!("s{i}"));
                check_files_in_root(root, std::slice::from_ref(&rq), true)
                    .capture_attributed(root, &attr);
            });
        }
        // MCP route readers — route-engine pressure.
        for _ in 0..10 {
            s.spawn(move || {
                let _ = build_repair_routes_in(Some(root_str), "q.rq", E0011);
            });
        }
        // A2A dispatch readers.
        for _ in 0..10 {
            s.spawn(|| {
                let _ = dispatch_tool(
                    "ggen.lsp.repair_route",
                    &json!({ "file_path": "q.rq", "file_content": E0011 }),
                );
            });
        }
    });

    // 1) Append-only OCEL log is uncorrupted: every line is valid JSON.
    let path = IntelLog::at_root(root).path().to_path_buf();
    let raw = fs::read_to_string(&path).expect("read log");
    let lines: Vec<&str> = raw.lines().filter(|l| !l.trim().is_empty()).collect();
    for line in &lines {
        assert!(
            serde_json::from_str::<serde_json::Value>(line).is_ok(),
            "corrupted OCEL line under concurrency: {line}"
        );
    }

    // 2) No events lost or merged: read() recovers exactly what was written.
    let log = IntelLog::at_root(root).read();
    assert_eq!(
        log.events.len(),
        lines.len(),
        "no events lost/merged under contention"
    );
    // Each capture writes two 5-event chains for the 2 diagnostics raised by E0011 (total 10 events).
    assert_eq!(
        log.events.len(),
        CAPTURES * 10,
        "every capture's chain landed intact"
    );

    // 3) No route drift: MCP and A2A resolve the same route id.
    let mcp = build_repair_routes_in(Some(root_str), "q.rq", E0011);
    let route_id = mcp["envelopes"][0]["route_id"]
        .as_str()
        .expect("route id")
        .to_string();
    let a2a = dispatch_tool(
        "ggen.lsp.repair_route",
        &json!({ "file_path": "q.rq", "file_content": E0011 }),
    )
    .expect("a2a dispatch");
    assert_eq!(
        a2a["envelopes"][0]["route_id"].as_str(),
        Some(route_id.as_str()),
        "no route drift across channels under pressure"
    );

    // 4) A sampled case replays cleanly.
    let episode = log
        .events
        .iter()
        .find_map(|e| {
            e.objects
                .iter()
                .find(|o| o.r#type == obj_type::EPISODE)
                .map(|o| o.id.clone())
        })
        .expect("an episode exists");
    assert!(replay_case(root, &episode).found, "sampled case replays");
}
