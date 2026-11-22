//! CP17 graph-hash corroboration for `ggen.construct` (parity with `ggen_write_apply`).

use ggen_engine::sync::{sync, SyncOptions};
use std::path::Path;

pub fn corroborate_expected_graph_hash(
    base_path: &Path,
    expected_graph_hash: &str,
) -> Result<(), String> {
    if expected_graph_hash.trim().is_empty() {
        return Err(
            "REFUSED:MISSING_EXPECTED_GRAPH_HASH — expected_graph_hash from a prior dry_run is required."
                .to_string(),
        );
    }
    let pre = sync(
        base_path,
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .map_err(|e| format!("REFUSED:PREFLIGHT_FAILED — {e}"))?;
    if pre.graph_hash_hex != expected_graph_hash {
        return Err(format!(
            "REFUSED:GRAPH_HASH_MISMATCH — expected {expected_graph_hash}, current {}.",
            pre.graph_hash_hex
        ));
    }
    Ok(())
}

pub fn dry_run_graph_hash_hex(base_path: &Path) -> Result<String, String> {
    let pre = sync(
        base_path,
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .map_err(|e| format!("Generation pipeline failed: {e}"))?;
    Ok(pre.graph_hash_hex)
}

pub fn sync_write_after_corroboration(
    base_path: &Path,
    expected_graph_hash: &str,
) -> Result<ggen_engine::sync::SyncReport, String> {
    corroborate_expected_graph_hash(base_path, expected_graph_hash)?;
    sync(base_path, SyncOptions::default()).map_err(|e| format!("Generation pipeline failed: {e}"))
}
