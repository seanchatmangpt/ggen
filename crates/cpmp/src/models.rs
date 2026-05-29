use serde::{Deserialize, Serialize};
use std::time::SystemTime;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Language {
    Rust,
    Go,
    Python,
    JavaScript,
    TypeScript,
    Java,
    C,
    Cpp,
    Erlang,
    Wasm,
    Json,
    Yaml,
    Toml,
    Markdown,
    Shell,
    Unknown,
}

impl Language {
    pub fn from_extension(ext: &str) -> Self {
        match ext.to_lowercase().as_str() {
            "rs" => Language::Rust,
            "go" => Language::Go,
            "py" => Language::Python,
            "js" => Language::JavaScript,
            "ts" => Language::TypeScript,
            "java" => Language::Java,
            "c" => Language::C,
            "cpp" | "cc" | "cxx" => Language::Cpp,
            "erl" => Language::Erlang,
            "wasm" => Language::Wasm,
            "json" => Language::Json,
            "yaml" | "yml" => Language::Yaml,
            "toml" => Language::Toml,
            "md" => Language::Markdown,
            "sh" => Language::Shell,
            _ => Language::Unknown,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileEntry {
    pub path: String,
    pub language: Language,
    pub size_bytes: u64,
    pub hash: String,
    pub modified_time: SystemTime,
    pub git_root: Option<String>,
    pub is_test: bool,
    pub is_binary: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Symbol {
    pub file_path: String,
    pub name: String,
    pub kind: String,
    pub line: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetectedCapability {
    pub file_path: String,
    pub capability: String,
    pub matched_term: String,
    pub name: String,
    pub confidence: f64,
    pub evidence_type: String,
    pub classification: String,
    pub line_number: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    pub id: String,
    pub timestamp: String,
    pub scan_roots: Vec<String>,
    pub file_count: usize,
    pub total_bytes: u64,
    pub aggregate_hash: String,
    pub commands_run: Vec<String>,
    pub output_artifacts: Vec<String>,
    pub files: Vec<FileEntry>,
}

#[derive(Debug)]
pub struct VerificationReport {
    pub unchanged_files: usize,
    pub added_files: Vec<String>,
    pub modified_files: Vec<String>,
    pub missing_files: Vec<String>,
}
