//! CP38 real end-to-end proof: `WriteApplyParams`'s `caller_origin` (private,
//! only settable via the module's own narrow constructors) actually reaches
//! the resulting receipt's `origin` field (CP37) -- an ordinary external-MCP
//! call and the bounded unattended dispatcher produce distinguishable
//! receipts, not identical ones.

use ggen_mcp::tools::sync_dry_run::{sync_dry_run, SyncDryRunParams};
use ggen_mcp::tools::unattended_dispatch::{
    try_unattended_apply, CircuitBreaker, UnattendedApplyOutcome,
};
use ggen_mcp::tools::write_apply::{write_apply, WriteApplyParams};
use tempfile::TempDir;

fn write_frontmatter_project(root: &std::path::Path) {
    std::fs::write(
        root.join("ggen.toml"),
        "[project]\nname = \"cp38-e2e\"\n[ontology]\nsource = \"model.ttl\"\n\
         [templates]\ndir = \"templates\"\n",
    )
    .expect("write ggen.toml");
    std::fs::write(
        root.join("model.ttl"),
        "@prefix ex: <http://example.org/> .\nex:owner a ex:Person .\n",
    )
    .expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
}

fn read_receipt_origin(root: &std::path::Path) -> Option<String> {
    let receipt_path = root.join(".ggen-v2/receipt.json");
    let content = std::fs::read_to_string(&receipt_path).expect("read receipt.json");
    let value: serde_json::Value = serde_json::from_str(&content).expect("parse receipt.json");
    // The persisted file wraps `ReceiptRecord` under a top-level `record` key
    // (see `.ggen-v2/receipt.json`'s real shape) -- confirmed by dumping a
    // real receipt while debugging this test, not assumed.
    value["record"]["origin"].as_str().map(str::to_string)
}

/// An ordinary, real MCP-shaped call (`WriteApplyParams::new`, exactly what
/// `Deserialize`-ing a real JSON-RPC request produces) must leave the
/// receipt's `origin` field entirely absent -- today's exact behavior,
/// provably unchanged by CP37/38's addition.
#[test]
fn external_mcp_call_produces_a_receipt_with_no_origin_tag() {
    let tmp = TempDir::new().expect("tempdir");
    write_frontmatter_project(tmp.path());
    std::fs::write(
        tmp.path().join("templates/plain.tmpl"),
        "---\nto: out/plain.txt\n---\nplain\n",
    )
    .expect("write template");

    let pre = sync_dry_run(&SyncDryRunParams {
        root: tmp.path().display().to_string(),
    })
    .expect("dry run");
    write_apply(&WriteApplyParams::new(
        tmp.path().display().to_string(),
        true,
        pre.graph_hash,
    ))
    .expect("apply");

    assert_eq!(
        read_receipt_origin(tmp.path()),
        None,
        "an ordinary external-MCP-shaped call must produce a receipt with no origin tag"
    );
}

/// A real write fired through the bounded unattended dispatcher must
/// produce a receipt whose `origin` reads back as `"unattended-dispatch"` --
/// the CP37/38 provenance tag actually reaching the chain, not just sitting
/// unused in `SyncOptions`.
#[tokio::test]
async fn unattended_dispatch_call_produces_a_receipt_tagged_unattended_dispatch() {
    let tmp = TempDir::new().expect("tempdir");
    write_frontmatter_project(tmp.path());
    std::fs::write(
        tmp.path().join("templates/eligible.tmpl"),
        "---\nto: out/eligible.txt\nunless_exists: true\n\
         unattended_write_eligible: true\n---\ngenerated\n",
    )
    .expect("write eligible template");

    let breaker = CircuitBreaker::default();
    let outcome = try_unattended_apply(tmp.path(), &breaker).await;
    assert!(
        matches!(outcome, UnattendedApplyOutcome::Applied(_)),
        "expected a real applied dispatch, got {outcome:?}"
    );

    assert_eq!(
        read_receipt_origin(tmp.path()),
        Some("unattended-dispatch".to_string()),
        "a receipt produced by the bounded unattended dispatcher must be tagged, \
         distinguishing it from a human/LLM-reviewed one in the chain"
    );
}
