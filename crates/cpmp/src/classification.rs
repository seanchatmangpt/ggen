use crate::models::Symbol;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Classification {
    Live,
    Partial,
    CapabilitySeed,
    LegacyName,
    Dormant,
    BrokenButReal,
    DocOnly,
    TestOnly,
    Ambiguous,
}

impl Classification {
    /// Stable label used in receipts and the capability inventory.
    pub fn as_str(self) -> &'static str {
        match self {
            Classification::Live => "LIVE",
            Classification::Partial => "PARTIAL",
            Classification::CapabilitySeed => "CAPABILITY_SEED",
            Classification::LegacyName => "LEGACY_NAME",
            Classification::Dormant => "DORMANT",
            Classification::BrokenButReal => "BROKEN_BUT_REAL",
            Classification::DocOnly => "DOC_ONLY",
            Classification::TestOnly => "TEST_ONLY",
            Classification::Ambiguous => "AMBIGUOUS",
        }
    }
}

/// Classify a file from its path and content.
///
/// Heuristics use only signals cpmp already has — no compilation or reachability
/// analysis — so they are conservative and explainable:
/// - `DORMANT`: the file is compiled out (`#![cfg(any())]`, the disabled-module
///   idiom).
/// - `DOC_ONLY`: a Markdown/text document.
/// - `TEST_ONLY`: lives under a test path.
/// - `BROKEN_BUT_REAL`: real code with explicit unimplemented holes
///   (`todo!()` / `unimplemented!()`).
/// - `LIVE`: everything else.
pub fn classify_file(path: &str, content: &str, _symbols: &[Symbol]) -> Classification {
    let path_lower = path.to_lowercase();
    if content.contains("#![cfg(any())]") {
        return Classification::Dormant;
    }
    if path_lower.ends_with(".md") || path_lower.ends_with(".txt") {
        return Classification::DocOnly;
    }
    if path_lower.contains("test") {
        return Classification::TestOnly;
    }
    if content.contains("todo!(") || content.contains("unimplemented!(") {
        return Classification::BrokenButReal;
    }
    Classification::Live
}

/// Crate directories present on disk under `<workspace>/crates/` but absent from
/// `[workspace] members` — i.e. crates excluded from the build (dormant). Returns
/// the bare directory names (e.g. `"genesis-construct8"`); empty if no workspace
/// root is found above `scan_root`.
///
/// This is a stronger, repo-level dormancy signal than per-file `#![cfg(any())]`:
/// an entire crate the workspace refuses to compile.
pub fn dormant_crate_dirs(scan_root: &std::path::Path) -> Vec<String> {
    let start = scan_root
        .canonicalize()
        .unwrap_or_else(|_| scan_root.to_path_buf());

    // Nearest ancestor Cargo.toml that declares `[workspace]`.
    let mut ws_root = None;
    let mut dir = Some(start.as_path());
    while let Some(d) = dir {
        let manifest = d.join("Cargo.toml");
        if manifest.is_file()
            && std::fs::read_to_string(&manifest)
                .map(|s| s.contains("[workspace]"))
                .unwrap_or(false)
        {
            ws_root = Some(d.to_path_buf());
            break;
        }
        dir = d.parent();
    }
    let ws_root = match ws_root {
        Some(r) => r,
        None => return Vec::new(),
    };

    // Member crate directory names from `[workspace] members`.
    let members: std::collections::HashSet<String> =
        std::fs::read_to_string(ws_root.join("Cargo.toml"))
            .ok()
            .and_then(|s| toml::from_str::<toml::Value>(&s).ok())
            .and_then(|v| {
                v.get("workspace")
                    .and_then(|w| w.get("members"))
                    .and_then(|m| m.as_array())
                    .map(|arr| {
                        arr.iter()
                            .filter_map(|m| m.as_str())
                            .filter_map(|p| p.strip_prefix("crates/"))
                            .map(|s| s.trim_end_matches('/').to_string())
                            .collect()
                    })
            })
            .unwrap_or_default();

    // Crate directories on disk that are not listed as members.
    let mut dormant = Vec::new();
    if let Ok(rd) = std::fs::read_dir(ws_root.join("crates")) {
        for entry in rd.filter_map(|e| e.ok()) {
            if entry.file_type().map(|t| t.is_dir()).unwrap_or(false) {
                let name = entry.file_name().to_string_lossy().to_string();
                if !members.contains(&name) {
                    dormant.push(name);
                }
            }
        }
    }
    dormant
}
