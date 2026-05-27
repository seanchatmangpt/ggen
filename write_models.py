models_rs = """use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileEntry {
    pub path: String,
    pub hash: String,
    pub size: i64,
    pub language: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Symbol {
    pub file_path: String,
    pub name: String,
    pub kind: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetectedCapability {
    pub file_path: String,
    pub capability: String,
    pub matched_term: String,
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
"""

with open('/Users/sac/capability-map/src/models.rs', 'w') as f:
    f.write(models_rs)
