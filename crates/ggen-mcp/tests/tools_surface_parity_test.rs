//! Tool-surface parity court for `ggen-mcp`.
//!
//! `.claude/rules/architecture.md` documents the ggen-mcp surface as
//! "nine tools, read-only except ggen_write_apply (destructive, requires
//! explicit confirm: true)". That claim was doc-anchored with no test --
//! tool-surface drift (a tool added, removed, or renamed in
//! `src/lib.rs`'s `tool_defs!` table) would silently stale it.
//!
//! This court enumerates the tools over the REAL rmcp protocol (an
//! in-process duplex transport: real server, real client, real
//! `tools/list` round trip -- no mocks) and asserts:
//!
//! 1. the registered set equals the pinned expected set exactly
//!    (added/removed/renamed tools fail with a drift-naming message);
//! 2. every tool in the documented architecture.md set is still present;
//! 3. every tool carries a non-empty description;
//! 4. `ggen_write_apply`'s input schema declares `confirm`, the tool is
//!    annotated destructive (not read-only), and the call path refuses
//!    without `confirm: true`.
//!
//! Chicago TDD: real server over real transport, asserts on real returned
//! state.
#![allow(clippy::unwrap_used, clippy::expect_used)] // Chicago TDD: real-IO tests

use rmcp::ServiceExt;

/// The pinned expected tool surface. Source of truth:
/// `crates/ggen-mcp/src/lib.rs`'s `tool_defs!` table, as of the commit
/// this court was written at. If this test fails with a drift message,
/// either update the table here (and the architecture.md count) or fix
/// the accidental surface change.
const EXPECTED_TOOLS: &[&str] = &[
    "ggen_query_preview",
    "ggen_config_classify",
    "ggen_frontmatter_schema",
    "ggen_frontmatter_lint",
    "ggen_sync_dry_run",
    "ggen_check_project",
    "ggen_rule_graph",
    "ggen_capability_status",
    "ggen_pack_capabilities",
    "ggen_pack_query",
    "ggen_receipt_verify",
    "ggen_write_apply",
];

/// The tool names architecture.md's "nine tools" claim anchors on. Every
/// one of these must remain registered -- removal or rename is a real
/// surface break for documented clients.
const DOCUMENTED_TOOLS: &[&str] = &[
    "ggen_query_preview",
    "ggen_config_classify",
    "ggen_frontmatter_schema",
    "ggen_frontmatter_lint",
    "ggen_sync_dry_run",
    "ggen_check_project",
    "ggen_rule_graph",
    "ggen_capability_status",
    "ggen_write_apply",
];

async fn listed_tools() -> Vec<rmcp::model::Tool> {
    let server = ggen_mcp::GgenMcpServer::new();
    let (server_transport, client_transport) = tokio::io::duplex(8192);
    let server_task = tokio::spawn(async move {
        let running = server.serve(server_transport).await?;
        running.waiting().await?;
        anyhow::Ok(())
    });

    let client = ().serve(client_transport).await.expect("client serve");
    let tools = client.list_all_tools().await.expect("tools/list");
    let _ = client.cancel().await;
    server_task.await.expect("server task").expect("server run");
    tools
}

#[tokio::test]
async fn registered_tool_set_matches_pinned_surface_exactly() {
    let tools = listed_tools().await;
    let mut registered: Vec<String> = tools.iter().map(|t| t.name.to_string()).collect();
    registered.sort();

    let mut expected: Vec<String> = EXPECTED_TOOLS.iter().map(|s| s.to_string()).collect();
    expected.sort();

    assert_eq!(
        registered,
        expected,
        "ggen-mcp tool-surface drift. added = {:?}, removed = {:?}. \
         If intentional, update tools_surface_parity_test.rs EXPECTED_TOOLS \
         AND the tool count in .claude/rules/architecture.md (which claims \
         nine tools) -- and src/lib.rs's own module doc.",
        expected
            .iter()
            .filter(|n| !registered.contains(n))
            .collect::<Vec<_>>(),
        registered
            .iter()
            .filter(|n| !expected.contains(n))
            .collect::<Vec<_>>(),
    );
}

#[tokio::test]
async fn every_architecture_md_documented_tool_is_still_registered() {
    let tools = listed_tools().await;
    let registered: Vec<String> = tools.iter().map(|t| t.name.to_string()).collect();
    for doc_name in DOCUMENTED_TOOLS {
        assert!(
            registered.iter().any(|n| n == doc_name),
            "architecture.md documents tool {doc_name:?} but it is no longer \
             registered -- a documented client would break"
        );
    }
}

#[tokio::test]
async fn every_tool_has_a_non_empty_description() {
    let tools = listed_tools().await;
    assert!(!tools.is_empty(), "no tools registered at all");
    for tool in &tools {
        let desc = tool.description.as_deref().unwrap_or("");
        assert!(
            !desc.trim().is_empty(),
            "tool {:?} has an empty description",
            tool.name
        );
    }
}

#[tokio::test]
async fn write_apply_schema_declares_confirm_and_annotation_is_destructive() {
    let tools = listed_tools().await;
    let tool = tools
        .iter()
        .find(|t| t.name == "ggen_write_apply")
        .expect("ggen_write_apply must be registered");

    // Annotation-level: destructive, not read-only (client gate surface).
    let ann = tool.annotations.as_ref().expect("annotations");
    assert!(ann.destructive_hint.unwrap_or(false));
    assert!(!ann.read_only_hint.unwrap_or(true));

    // Schema-level: the input schema declares `confirm` as a boolean
    // property of the object.
    let props = tool
        .input_schema
        .get("properties")
        .and_then(|p| p.get("confirm"))
        .expect("ggen_write_apply input schema must declare a `confirm` property");
    assert_eq!(
        props.get("type").and_then(|t| t.as_str()),
        Some("boolean"),
        "confirm must be schema-typed as boolean"
    );
}

#[tokio::test]
async fn write_apply_call_path_refuses_without_confirm() {
    // Real call path, real params type -- confirm:false must be refused
    // with a typed Unsupported error before any pipeline work.
    let params = ggen_mcp::tools::write_apply::WriteApplyParams::new(
        "/nonexistent-root-for-refusal-path".to_string(),
        false,
        "unused-because-refused".to_string(),
    );
    let err = ggen_mcp::tools::write_apply::write_apply(&params)
        .expect_err("confirm:false must be refused, never applied");
    assert!(
        err.message.contains("confirm"),
        "refusal must name the missing confirm flag, got: {err:?}"
    );
}
