// proc-macro code runs at compile time; unwrap_or/unwrap_or_else are correct fallback patterns here
#![allow(clippy::unwrap_used)]
use std::path::{Path, PathBuf};

/// Walk ancestor dirs from manifest_dir to find the workspace root.
/// The workspace root is the first ancestor whose Cargo.toml contains "[workspace]".
pub fn workspace_root(manifest_dir: &Path) -> Option<PathBuf> {
    manifest_dir
        .ancestors()
        .find(|p| {
            let cargo = p.join("Cargo.toml");
            std::fs::read_to_string(&cargo)
                .map(|s| s.contains("[workspace]"))
                .unwrap_or(false)
        })
        .map(|p| p.to_owned())
}

/// Extract ticket ID (e.g. "CC-001") from a filename stem.
/// Matches CC-\d+ pattern.
pub fn extract_ticket_id(filename: &str) -> Option<String> {
    let stem = Path::new(filename).file_stem()?.to_str()?;
    // Find "CC-" then take only the digits immediately after it (not the full suffix)
    let start = stem.find("CC-")?;
    let after_prefix = start + 3;
    if after_prefix >= stem.len() {
        return None;
    }
    let digits_rest = &stem[after_prefix..];
    let end = digits_rest.find(|c: char| !c.is_ascii_digit()).unwrap_or(digits_rest.len());
    let digits = &digits_rest[..end];
    if digits.is_empty() {
        None
    } else {
        Some(format!("CC-{digits}"))
    }
}
