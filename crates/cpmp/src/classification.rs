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
