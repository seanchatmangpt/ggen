use crate::models::{DetectedCapability, Symbol};
use std::path::Path;

pub fn detect_capabilities(
    path: &Path, content: &str, _symbols: &[Symbol],
) -> Vec<DetectedCapability> {
    let mut detected = Vec::new();
    let capabilities = vec![
        "Genesis",
        "ggen",
        "Truex",
        "Receipt",
        "Replay",
        "Refusal",
        "Construct8",
        "Pair2",
        "RelationPage",
        "Need9",
        "Need257",
        "Shard",
        "Segment",
        "Corpus",
        "O*",
        "mu",
        "AtomVM",
        "Erlang",
        "WASM",
        "POWL",
        "OCEL",
        "PROV",
        "SHACL",
        "DCAT",
        "Field8",
        "Instinct8",
        "Doctor",
        "Wizard",
        "Telco",
    ];

    let path_str = path.to_string_lossy().to_string();
    let classification =
        crate::classification::classify_file(&path_str, content, _symbols).as_str();
    for (line_idx, line) in content.lines().enumerate() {
        for cap in &capabilities {
            if line.contains(cap) {
                detected.push(DetectedCapability {
                    file_path: path_str.clone(),
                    capability: cap.to_string(),
                    matched_term: cap.to_string(),
                    name: cap.to_string(),
                    confidence: 0.9,
                    evidence_type: if path_str.contains("test") {
                        "TEST".to_string()
                    } else if path_str.ends_with(".md") {
                        "DOC".to_string()
                    } else {
                        "CODE".to_string()
                    },
                    classification: classification.to_string(),
                    line_number: (line_idx + 1) as i64,
                });
            }
        }
    }
    detected
}
