//! Shared typed-skip classification (CP28), extracted from
//! `sync_dry_run.rs` so `crate::bridge::push_sync_refusal_for_root` can
//! reuse the exact same, already-proven classification logic instead of
//! re-implementing (and risking drifting from) it.
//!
//! `classify` is the ONLY thing extracted -- `sync_dry_run.rs` keeps its own
//! `PlannedWrite`/`PlannedSkip`/`SyncDryRunResult` types and tool entry
//! point unchanged; this module has no tool-facing surface of its own.

/// Classify an engine reason string into the closed typed set.
///
/// Deliberately conservative: an unrecognized reason becomes `other` with
/// the raw text preserved, rather than being forced into the nearest
/// category. Silently mislabeling a skip reason would recreate exactly the
/// ambiguity this classification exists to remove.
pub(crate) fn classify(reason: &str) -> &'static str {
    let r = reason.to_ascii_lowercase();
    if r.contains("when") && (r.contains("false") || r.contains("guard")) {
        "when_false"
    } else if r.contains("zero row")
        || r.contains("no rows")
        || r.contains("empty result")
        || r.contains("produced 0 rows")
    {
        // "produced 0 rows" is the real engine wording for the for_each /
        // implicit-row-fan-out zero-row skip (sync.rs's `for_each ...
        // produced 0 rows (...)` messages) -- it contains neither "zero
        // row" nor "no rows" nor "empty result", so without this arm it
        // fell through to `other` despite being exactly the zero-row case
        // this tool exists to classify distinctly. Matched as the literal
        // phrase "produced 0 rows" (not a bare "0 rows") so a future
        // nonzero count like "produced 10 rows" can't collide via
        // substring.
        "zero_rows"
    } else if r.contains("unchanged") || r.contains("identical") {
        "unchanged"
    } else if r.contains("unless_exists") || r.contains("already exists") {
        "exists_no_overwrite"
    } else if r.contains("skip_empty") {
        "skip_empty"
    } else {
        "other"
    }
}

#[cfg(test)]
mod tests {
    use super::classify;

    #[test]
    fn classifies_known_reasons() {
        assert_eq!(classify("skipped: when guard false"), "when_false");
        assert_eq!(
            classify("skipped: for_each produced 0 rows (x)"),
            "zero_rows"
        );
        assert_eq!(classify("skipped: unchanged"), "unchanged");
        assert_eq!(classify("skipped: already exists"), "exists_no_overwrite");
        assert_eq!(classify("skipped: skip_empty"), "skip_empty");
        assert_eq!(classify("skipped: something else entirely"), "other");
    }
}
