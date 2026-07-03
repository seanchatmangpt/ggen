//! Chicago TDD for the agent WIRE surface (MCP/A2A) over `PackAgent`.
//!
//! These tests cross the real boundary an agent crosses: a tool *name* plus
//! JSON *arguments* are dispatched through the same pure result functions the
//! MCP server (`GgenMcpServer`) and the A2A `PackToolsAdapter` use, into the
//! real `PackAgent`, against a real filesystem and real Ed25519 receipts. The
//! evidence asserted on is the structured JSON returned (externalizable) — never
//! an internal flag. Where the facade tests prove `PackAgent` works, these prove
//! the wire layer actually *routes* to it, and that the A2A agent card and the
//! dispatch table do not drift apart.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::fs;
use std::path::{Path, PathBuf};

use ggen_core::agent::{emit_install_receipt, PackInstallClosure};
use ggen_lsp::a2a_mcp::a2a_generated::adapter::Adapter;
use ggen_lsp::a2a_mcp::{dispatch_pack_tool, pack_agent_card, PackToolsAdapter, PACK_TOOLS};
use serde_json::json;
use tempfile::TempDir;

fn block_on<F: std::future::Future>(fut: F) -> F::Output {
    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap()
        .block_on(fut)
}

/// Seed a real `.ggen/packs.lock` under `root`.
fn seed_lockfile(root: &Path, id: &str, version: &str) {
    let body = format!(
        r#"{{
  "packs": {{
    "{id}": {{
      "version": "{version}",
      "source": {{ "type": "Registry", "url": "https://registry.ggen.io" }},
      "integrity": "sha256-seeded",
      "installed_at": "2024-01-01T00:00:00Z",
      "dependencies": []
    }}
  }},
  "updated_at": "2024-01-01T00:00:00Z",
  "ggen_version": "6.0.0"
}}"#
    );
    let lock = root.join(".ggen").join("packs.lock");
    fs::create_dir_all(lock.parent().unwrap()).unwrap();
    fs::write(lock, body).unwrap();
}

/// Emit a real signed receipt under `root` and return its path.
fn emit_receipt(root: &Path) -> PathBuf {
    let packages = vec!["alpha-core".to_string()];
    let artifacts: Vec<PathBuf> = vec![];
    let closure = PackInstallClosure {
        pack_id: "alpha",
        pack_version: "1.0.0",
        pack_digest: "deadbeef",
        packages_installed: &packages,
        artifact_paths: &artifacts,
    };
    emit_install_receipt(root, &closure).expect("emit receipt")
}

// ── dispatch_pack_tool: the single A2A/MCP entry point ──────────────────────

#[test]
fn dispatch_capabilities_returns_the_operation_set() {
    let v = block_on(dispatch_pack_tool("ggen.packs.capabilities", &json!({}))).expect("dispatch");
    let ops: Vec<String> = v["operations"]
        .as_array()
        .expect("operations array")
        .iter()
        .filter_map(|o| o["name"].as_str().map(String::from))
        .collect();
    for expected in [
        "search",
        "list",
        "show",
        "resolve",
        "compatibility",
        "status",
        "verify",
        "install",
        "remove",
    ] {
        assert!(
            ops.contains(&expected.to_string()),
            "missing op {expected} in {ops:?}"
        );
    }
}

#[test]
fn dispatch_status_reads_a_real_lockfile_via_root() {
    let root = TempDir::new().unwrap();
    seed_lockfile(root.path(), "alpha", "1.2.3");

    let v = block_on(dispatch_pack_tool(
        "ggen.packs.status",
        &json!({ "root": root.path().to_str().unwrap() }),
    ))
    .expect("dispatch");

    assert_eq!(v["lockfile_present"], json!(true));
    let installed = v["installed"].as_array().expect("installed array");
    assert!(
        installed
            .iter()
            .any(|p| p["pack_id"] == "alpha" && p["version"] == "1.2.3"),
        "status must report the seeded pack through the wire: {v}"
    );
}

#[test]
fn dispatch_verify_validates_a_real_receipt_via_root() {
    let root = TempDir::new().unwrap();
    let receipt = emit_receipt(root.path());

    let v = block_on(dispatch_pack_tool(
        "ggen.packs.verify",
        &json!({
            "root": root.path().to_str().unwrap(),
            "receipt_path": receipt.to_str().unwrap(),
        }),
    ))
    .expect("dispatch");

    assert_eq!(
        v["is_valid"],
        json!(true),
        "a real signed receipt must verify through the wire: {v}"
    );
}

#[test]
fn dispatch_verify_tampered_receipt_is_invalid() {
    let root = TempDir::new().unwrap();
    let receipt = emit_receipt(root.path());

    // Tamper the signature on disk, then verify through the wire.
    let mut body: serde_json::Value = serde_json::from_slice(&fs::read(&receipt).unwrap()).unwrap();
    body["signature"] = json!("00".repeat(64));
    fs::write(&receipt, serde_json::to_vec_pretty(&body).unwrap()).unwrap();

    let v = block_on(dispatch_pack_tool(
        "ggen.packs.verify",
        &json!({
            "root": root.path().to_str().unwrap(),
            "receipt_path": receipt.to_str().unwrap(),
        }),
    ))
    .expect("dispatch");

    assert_eq!(
        v["is_valid"],
        json!(false),
        "a tampered receipt must not verify through the wire"
    );
}

#[test]
fn dispatch_unknown_tool_is_rejected() {
    let result = block_on(dispatch_pack_tool("ggen.packs.nonexistent", &json!({})));
    assert!(
        result.is_err(),
        "an unknown tool must be rejected, not silently handled"
    );
}

#[test]
fn dispatch_install_with_invalid_pack_id_is_fail_closed() {
    // A malformed id is refused at the wire before any durable state is touched.
    let result = block_on(dispatch_pack_tool(
        "ggen.packs.install",
        &json!({ "pack_id": "bad name!" }),
    ));
    assert!(result.is_err(), "an invalid pack id must be rejected");
}

// ── agent card / adapter: no drift, correct routing ─────────────────────────

#[test]
fn agent_card_advertises_exactly_the_dispatchable_tools() {
    let card = pack_agent_card();
    let caps = card["capabilities"]
        .as_object()
        .expect("capabilities object");

    // Every advertised capability is a real dispatchable tool, and the card
    // advertises every tool — the card and the dispatch table cannot drift.
    for tool in PACK_TOOLS {
        assert!(
            caps.contains_key(*tool),
            "card is missing advertised tool {tool}"
        );
    }
    assert_eq!(
        caps.len(),
        PACK_TOOLS.len(),
        "agent card and PACK_TOOLS must enumerate the same tool set"
    );
}

#[test]
fn adapter_handles_pack_tools_and_disclaims_foreign_ones() {
    let adapter = PackToolsAdapter::new();
    for tool in PACK_TOOLS {
        assert!(adapter.can_handle(tool), "adapter must handle {tool}");
    }
    assert!(
        !adapter.can_handle("ggen.construct"),
        "adapter must not claim tools it does not own"
    );
}

#[test]
fn adapter_from_a2a_routes_a_task_to_the_facade() {
    let adapter = PackToolsAdapter::new();
    let msg = json!({ "tool": "ggen.packs.capabilities", "arguments": {} });

    let v = block_on(adapter.from_a2a(&msg)).expect("from_a2a");
    assert!(
        v["operations"].is_array(),
        "from_a2a must return the facade's structured result: {v}"
    );
}

#[test]
fn adapter_from_a2a_without_tool_is_rejected() {
    let adapter = PackToolsAdapter::new();
    let result = block_on(adapter.from_a2a(&json!({})));
    assert!(
        result.is_err(),
        "an A2A message without 'tool' must be rejected"
    );
}
