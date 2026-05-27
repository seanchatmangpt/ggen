classification_rs = """use std::path::Path;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Classification {
    CapabilitySeed,
    LegacyName,
    BrokenButReal,
    DocOnly,
    TestOnly,
    Unknown,
}

pub fn classify_file(_path: &Path) -> Classification {
    Classification::Unknown
}
"""

with open('/Users/sac/capability-map/src/classification.rs', 'w') as f:
    f.write(classification_rs)
