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

pub fn classify_file(path: &str, _symbols: &[Symbol]) -> Classification {
    let path_lower = path.to_lowercase();
    if path_lower.contains("test") {
        return Classification::TestOnly;
    }
    if path_lower.ends_with(".md") || path_lower.ends_with(".txt") {
        return Classification::DocOnly;
    }
    Classification::Live
}
