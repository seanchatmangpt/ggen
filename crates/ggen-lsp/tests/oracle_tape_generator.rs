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
//! ORACLE-TAPE GENERATOR — produces the committed `wpm` decisive-gate fixtures.
//!
//! This is a `#[ignore]`-gated generator (NOT a CI assertion test). Running it
//! drives the REAL living-LSP loop
//! ([`ServerState::analyze_and_observe`](ggen_lsp::ServerState::analyze_and_observe))
//! over a real GGEN-TPL-001 project — raise (analyze the template that consumes
//! an unbound projection var) then cross-surface repair (fix the SPARQL `SELECT`
//! so the var IS bound, then analyze the query) — and persists the produced
//! on-disk OCEL tape (`.ggen/ocel/agent-edit-events.ocel.jsonl`) as committed
//! fixtures under `tests/fixtures/oracle-tapes/`:
//!
//!   * `ggen-6link-good.ocel.jsonl` — the GOOD tape: the full 6-link chain for
//!     the template episode (`DiagnosticRaised → RouteSelected → RepairSuggested
//!     → RepairApplied → GatePassed → ReceiptEmitted`).
//!   * `ggen-6link-prefix-DEAD.ocel.jsonl` — the DECISIVE-GATE input for `wpm`:
//!     the SAME tape truncated to a valid prefix that ends after `RepairApplied`,
//!     i.e. it is MISSING `GatePassed` (and the trailing `ReceiptEmitted`). A
//!     receipt without a preceding passing gate is the canonical conformance
//!     violation `wpm` must reject.
//!
//! Chicago TDD: the tape is captured from REAL execution of production code
//! (`analyze_and_observe` → `observe_diagnostics` → `IntelLog::at_root().append`)
//! over a real on-disk project. No fabricated events, no hand-built JSONL, no
//! mocks. The generator filters the real log to the template episode's
//! GGEN-TPL-001 events (the only thing it asserts about) and writes them verbatim.
//!
//! ## How to (re)generate the committed fixtures
//!
//! ```bash
//! cargo test -p ggen-lsp --test oracle_tape_generator -- --ignored --nocapture
//! ```
//!
//! The generated lines are timestamped (real `chrono::Utc::now()` from the live
//! run), so re-running rewrites the committed bytes. That is intentional: the
//! tape is a *captured recording*, not a deterministic golden. `wpm`'s gate keys
//! on the ACTIVITY SEQUENCE, not the timestamps, so a re-recorded GOOD tape stays
//! GOOD and a re-recorded DEAD tape stays DEAD.

use std::path::{Path, PathBuf};

use lsp_max::lsp_types::{Diagnostic, NumberOrString, Url};

fn url_from_path(path: impl AsRef<std::path::Path>) -> Url {
    url::Url::from_file_path(path.as_ref())
        .expect("absolute path")
        .to_string()
        .parse::<Url>()
        .expect("valid uri")
}

use ggen_lsp::ServerState;

/// Absolute path to the committed oracle-tapes fixtures dir (created if absent).
fn oracle_tapes_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("oracle-tapes")
}

use lsp_max_protocol::MaxDiagnostic;

/// True if a `Diagnostic.code` renders as exactly `GGEN-TPL-001`.
fn is_tpl_001(d: &MaxDiagnostic) -> bool {
    matches!(&d.lsp.code, Some(NumberOrString::String(s)) if s == "GGEN-TPL-001")
}

/// Write a minimal valid ggen project whose query binds only `?name` while its
/// template consumes `row["title"]` — the canonical GGEN-TPL-001 (unbound
/// projection) defect. Mirrors the proven living-loop fixture in
/// `ggen_tpl_001_living_loop.rs::write_project`.
fn write_project(dir: &Path, query: &str, template: &str) {
    std::fs::create_dir_all(dir.join("schema")).expect("schema dir");
    std::fs::create_dir_all(dir.join("queries")).expect("queries dir");
    std::fs::create_dir_all(dir.join("templates")).expect("templates dir");
    std::fs::write(
        dir.join("schema/domain.ttl"),
        "@prefix schema: <https://schema.org/> .\n",
    )
    .expect("ttl");
    std::fs::write(dir.join("queries/items.rq"), query).expect("rq");
    std::fs::write(dir.join("templates/item.tera"), template).expect("tera");
    std::fs::write(
        dir.join("ggen.toml"),
        r#"[project]
name = "oracle-tape-fixture"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"

[generation]
output_dir = "."

[[generation.rules]]
name = "items"
query = { file = "queries/items.rq" }
template = { file = "templates/item.tera" }
output_file = "out.txt"
"#,
    )
    .expect("ggen.toml");
}

/// Read every line of the EXTERNAL on-disk OCEL log under `<root>`.
fn read_log_lines(root: &Path) -> Vec<String> {
    let path = root
        .join(".ggen")
        .join("ocel")
        .join("agent-edit-events.ocel.jsonl");
    std::fs::read_to_string(path)
        .unwrap_or_default()
        .lines()
        .map(str::to_string)
        .collect()
}

/// True if a JSONL event line names this `activity`, the GGEN-TPL-001 `code`, and
/// the template file — the three substrings that identify a single template
/// GGEN-TPL-001 event in the captured log.
fn is_template_tpl_event(line: &str, activity: &str) -> bool {
    line.contains(&format!("\"activity\":\"{activity}\""))
        && line.contains("item.tera")
        && line.contains("GGEN-TPL-001")
}

/// The 6 activities of the living-LSP chain, in lawful order.
const SIX_LINK: [&str; 6] = [
    "DiagnosticRaised",
    "RouteSelected",
    "RepairSuggested",
    "RepairApplied",
    "GatePassed",
    "ReceiptEmitted",
];

#[ignore = "generator: writes committed oracle-tape fixtures; run with --ignored"]
#[tokio::test]
async fn generate_ggen_6link_oracle_tapes() {
    // ── Arrange: a real on-disk GGEN-TPL-001 project in a hermetic TempDir.
    let tmp = tempfile::tempdir().expect("tempdir");
    let root = tmp.path();
    write_project(
        root,
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
        r#"{{ row["title"] }}"#,
    );
    let state = ServerState::with_root(root);

    let tera_path = root.join("templates/item.tera");
    let tera_uri = url_from_path(&tera_path);
    let rq_path = root.join("queries/items.rq");
    let rq_uri = url_from_path(&rq_path);

    // ── Act 1 — RAISE: analyze the template through the REAL orchestration.
    let tera_src = std::fs::read_to_string(&tera_path).expect("read tera");
    let raised = state.analyze_and_observe(&tera_uri, &tera_src).await;
    assert!(
        raised
            .iter()
            .any(|(u, diags)| u == &tera_uri && diags.iter().any(is_tpl_001)),
        "generator precondition: analyze_and_observe must raise GGEN-TPL-001 on \
         the template. published: {raised:?}"
    );

    // ── Act 2 — CROSS-SURFACE REPAIR: bind `?title` in the QUERY (template
    // untouched), then analyze the query so the orchestration recomputes the
    // project graph and clears the now-lawful template through the SAME
    // observe_diagnostics the editor uses (RepairApplied → GatePassed →
    // ReceiptEmitted).
    let repaired_rq = "SELECT ?name ?title WHERE { ?s <https://schema.org/name> ?name ; \
                       <https://schema.org/title> ?title }";
    std::fs::write(&rq_path, repaired_rq).expect("rewrite rq");
    let _ = state.analyze_and_observe(&rq_uri, repaired_rq).await;

    // ── Capture: the template episode's GGEN-TPL-001 events, in file (= lawful)
    // order, filtered from the REAL on-disk log. We keep only the template's
    // GGEN-TPL-001 lines so the tape is the single episode `wpm` reasons about.
    let all_lines = read_log_lines(root);
    let mut good_lines: Vec<String> = Vec::new();
    for activity in SIX_LINK {
        let line = all_lines
            .iter()
            .find(|l| is_template_tpl_event(l, activity))
            .unwrap_or_else(|| {
                panic!(
                    "generator precondition: captured log missing template \
                     GGEN-TPL-001 {activity:?} event.\nlog:\n{}",
                    all_lines.join("\n")
                )
            });
        good_lines.push(line.clone());
    }
    assert_eq!(
        good_lines.len(),
        6,
        "the GOOD tape must be exactly the 6-link chain"
    );

    // ── DEAD variant: valid prefix ending after RepairApplied — MISSING
    // `GatePassed` (the decisive gate) and the trailing `ReceiptEmitted`. A
    // receipt-less repair with no passing gate is the conformance violation `wpm`
    // must reject. Take the prefix up to (excluding) the GatePassed line.
    let gate_idx = SIX_LINK
        .iter()
        .position(|a| *a == "GatePassed")
        .expect("GatePassed is in SIX_LINK");
    let dead_lines: Vec<String> = good_lines[..gate_idx].to_vec();
    assert_eq!(
        dead_lines.len(),
        4,
        "the DEAD prefix must keep DiagnosticRaised..RepairApplied and drop \
         GatePassed + ReceiptEmitted"
    );
    assert!(
        !dead_lines
            .iter()
            .any(|l| l.contains("\"activity\":\"GatePassed\"")),
        "the DEAD tape must NOT contain GatePassed (the decisive gate)"
    );
    assert!(
        !dead_lines
            .iter()
            .any(|l| l.contains("\"activity\":\"ReceiptEmitted\"")),
        "the DEAD tape must NOT contain ReceiptEmitted (no receipt without a gate)"
    );

    // ── Persist as committed fixtures (NDJSON: one JSON object per line, trailing
    // newline — matches the on-disk writer's shape so the reader folds it back).
    let dir = oracle_tapes_dir();
    std::fs::create_dir_all(&dir).expect("create oracle-tapes dir");

    let good_path = dir.join("ggen-6link-good.ocel.jsonl");
    let mut good_out = good_lines.join("\n");
    good_out.push('\n');
    std::fs::write(&good_path, good_out.as_bytes()).expect("write good tape");

    let dead_path = dir.join("ggen-6link-prefix-DEAD.ocel.jsonl");
    let mut dead_out = dead_lines.join("\n");
    dead_out.push('\n');
    std::fs::write(&dead_path, dead_out.as_bytes()).expect("write dead tape");

    eprintln!("WROTE GOOD TAPE: {}", good_path.display());
    eprintln!("WROTE DEAD TAPE: {}", dead_path.display());
}
