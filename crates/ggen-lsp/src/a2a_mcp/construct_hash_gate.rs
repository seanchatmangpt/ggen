//! CP17 graph-hash corroboration for CCS construct plane (`ggen.construct`).
//!
//! Fail-closed parity with MCP `ggen_write_apply`: `confirm`-style ambient
//! authority is never enough — construct must independently recompute the
//! current graph hash via dry-run and refuse empty / fabricated /
//! stale `expected_graph_hash` before any write.
//!
//! Pure admit (`corroborate_hashes`) is IO-free and Chicago-tested in-lib.
//! IO wrappers call `ggen_engine::sync` for dry-run / write after the pure gate.
//!
//! Standing: presence of this fence is `PARTIAL_ALIVE` until tip cargo courts
//! exit 0. Does not grant Plane-5 DO (BRCE+AS-01 exclusive).

use ggen_engine::sync::{sync, SyncOptions, SyncReport};
use std::path::Path;

/// Pure CP17 fence: non-empty expected must equal the independently observed
/// current graph hash. No filesystem, no sync — unit-testable without cargo
/// engine load when callers inject hashes.
pub fn corroborate_hashes(
    expected_graph_hash: &str, current_graph_hash: &str,
) -> Result<(), String> {
    if expected_graph_hash.trim().is_empty() {
        return Err(
            "REFUSED:MISSING_EXPECTED_GRAPH_HASH — expected_graph_hash from a prior dry_run is required."
                .to_string(),
        );
    }
    if expected_graph_hash != current_graph_hash {
        return Err(format!(
            "REFUSED:GRAPH_HASH_MISMATCH — expected {expected_graph_hash}, current {current_graph_hash}."
        ));
    }
    Ok(())
}

/// Dry-run the project and corroborate `expected_graph_hash` against the live
/// graph hash. Refuses before any write.
pub fn corroborate_expected_graph_hash(
    base_path: &Path, expected_graph_hash: &str,
) -> Result<(), String> {
    // Empty expected fails closed before IO — same typed refusal as pure gate.
    if expected_graph_hash.trim().is_empty() {
        return corroborate_hashes(expected_graph_hash, "");
    }
    let pre = sync(
        base_path,
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    )
    .map_err(|e| format!("REFUSED:PREFLIGHT_FAILED — {e}"))?;
    corroborate_hashes(expected_graph_hash, &pre.graph_hash_hex)
}

/// Return the dry-run graph hash for a project root (prior dry_run source).
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

/// CP17 then real write sync. Fail-closed: mismatch / missing hash never writes.
pub fn sync_write_after_corroboration(
    base_path: &Path, expected_graph_hash: &str,
) -> Result<SyncReport, String> {
    corroborate_expected_graph_hash(base_path, expected_graph_hash)?;
    sync(base_path, SyncOptions::default()).map_err(|e| format!("Generation pipeline failed: {e}"))
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;

    /// Chicago (lib-only): empty expected refuses with typed CCS construct code.
    #[test]
    fn empty_expected_hash_is_refused() {
        let err = corroborate_hashes("", "abc123").expect_err("empty must refuse");
        assert!(
            err.contains("REFUSED:MISSING_EXPECTED_GRAPH_HASH"),
            "typed refusal required: {err}"
        );
    }

    /// Chicago (lib-only): whitespace-only expected is also empty → refuse.
    #[test]
    fn whitespace_expected_hash_is_refused() {
        let err = corroborate_hashes("   \t", "abc123").expect_err("whitespace must refuse");
        assert!(
            err.contains("REFUSED:MISSING_EXPECTED_GRAPH_HASH"),
            "typed refusal required: {err}"
        );
    }

    /// Chicago (lib-only): fabricated / stale hash refuses without ambient admit.
    #[test]
    fn fabricated_hash_is_refused() {
        let err = corroborate_hashes("deadbeef", "cafebabe").expect_err("mismatch must refuse");
        assert!(
            err.contains("REFUSED:GRAPH_HASH_MISMATCH"),
            "typed refusal required: {err}"
        );
        assert!(err.contains("deadbeef"), "{err}");
        assert!(err.contains("cafebabe"), "{err}");
    }

    /// Chicago (lib-only): matching hashes admit the construct seal (not Plane-5 DO).
    #[test]
    fn matching_hash_admits_construct_seal() {
        assert!(corroborate_hashes("abc123", "abc123").is_ok());
    }
}
