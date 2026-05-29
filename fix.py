import os

# Fix models.rs
with open("src/models.rs", "w") as f:
    f.write("""use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileEntry {
    pub path: String,
    pub hash: String,
    pub size: i64,
    pub language: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Symbol {
    pub file_path: String,
    pub name: String,
    pub kind: String,
    pub line: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct DetectedCapability {
    pub file_path: String,
    pub capability: String,
    pub matched_term: String,
    pub confidence: f64,
    pub evidence_type: String,
    pub line_start: usize,
    pub line_end: usize,
    pub line_context: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    pub id: String,
    pub timestamp: String,
    pub root_paths: Vec<String>,
    pub file_count: i64,
    pub directory_count: i64,
    pub total_bytes: i64,
    pub hash_algorithm: String,
    pub aggregate_hash: String,
    pub catalog_version: String,
    pub command_run: String,
    pub output_artifacts: Vec<String>,
    pub warnings: Vec<String>,
    pub refusals: Vec<String>,
    pub files: Vec<FileEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReceiptEntry {
    pub path: String,
    pub hash: String,
    pub size: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemInfo {
    pub pid: u32,
    pub hostname: String,
    pub os: String,
}
""")

# Fix classification.rs
with open("src/classification.rs", "w") as f:
    f.write("""use std::path::Path;
use serde::{Serialize, Deserialize};
use crate::models::Symbol;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Classification {
    Partial,
    #[serde(rename = "CAPABILITY_SEED")]
    CapabilitySeed,
    #[serde(rename = "LEGACY_NAME")]
    LegacyName,
    #[serde(rename = "BROKEN_BUT_REAL")]
    BrokenButReal,
    #[serde(rename = "DOC_ONLY")]
    DocOnly,
    #[serde(rename = "TEST_ONLY")]
    TestOnly,
}

pub fn classify_file(path: &Path, _symbols: &[Symbol]) -> Classification {
    let path_str = path.to_string_lossy();
    if path_str.contains("test") {
        Classification::Partial
    } else {
        Classification::CapabilitySeed
    }
}
""")

