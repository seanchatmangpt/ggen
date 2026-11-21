//! Court: Stage 2c `SyncPlan.order` / composition-plan `order` are admission-only.
//!
//! Pins the falsifier anchor in `crates/ggen-engine/src/sync.rs` and refuses if
//! Stage 2c starts binding or consuming topological order for execution. Real
//! `std::fs` read of the live source in this checkout — no mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::PathBuf;

const FALSIFIER_ANCHOR: &str = "FALSIFIER:STAGE2C_SYNCPLAN_ORDER_ADMISSION_ONLY";
const STAGE2C_BEGIN: &str = "// ── Stage 2c: EA Graph (ABB/SBB) & Depgraph Admission";
const STAGE2C_END: &str = "// ── Stage 2c-caps:";

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("crates/ggen-engine -> workspace root")
        .to_path_buf()
}

fn read_sync_rs() -> String {
    let path = workspace_root().join("crates/ggen-engine/src/sync.rs");
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()))
}

fn stage2c_region<'a>(source: &'a str) -> &'a str {
    let start = source
        .find(STAGE2C_BEGIN)
        .unwrap_or_else(|| panic!("missing Stage 2c begin marker in sync.rs"));
    let rest = &source[start..];
    let end = rest
        .find(STAGE2C_END)
        .unwrap_or_else(|| panic!("missing Stage 2c-caps end marker in sync.rs"));
    &rest[..end]
}

#[test]
fn stage2c_syncplan_order_is_admission_only_falsifier() {
    let source = read_sync_rs();
    assert!(
        source.contains(FALSIFIER_ANCHOR),
        "sync.rs must carry anchor `{FALSIFIER_ANCHOR}` for this court"
    );

    let region = stage2c_region(&source);

    assert!(
        region.contains("Depgraph admission is refusal-only"),
        "Stage 2c header must document admission-only order scoping"
    );
    assert!(
        region.contains("discover_templates"),
        "Stage 2c header must cite [`discover_templates`] as the execution-order surface"
    );

    assert!(
        region.contains("resolve_sync_order"),
        "Stage 2c must retain the legacy `resolve_sync_order` admission gate"
    );
    let forbidden_bindings = [
        "let _plan",
        "let sync_plan",
        "let sync_order",
        ": SyncPlan",
        "SyncPlan {",
    ];
    for needle in forbidden_bindings {
        assert!(
            !region.contains(needle),
            "Stage 2c must not bind `{needle}` — order is admission-only, not consumed"
        );
    }

    assert!(
        !region.contains("plan.order"),
        "Stage 2c inline region must not read `plan.order` (validation lives in \
         `validate_composition_plan` only)"
    );

    assert!(
        region.contains("Cross-pack dependency graph admission"),
        "resolve_sync_order refusals must stay on FM-PACK-015 admission path"
    );
    assert!(
        region.contains("validate_composition_plan(plan, &packs)"),
        "composition-plan path must stop at validate_composition_plan"
    );
}