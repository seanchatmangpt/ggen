use std::fs;
use std::path::PathBuf;
use walkdir::WalkDir;

#[test]
fn test_gc008_no_lsp_mutation_lock() {
    let mut violations = Vec::new();
    let lsp_path = PathBuf::from("/Users/sac/wasm4pm/crates/wasm4pm-lsp/src");

    if lsp_path.exists() {
        for entry in WalkDir::new(&lsp_path).into_iter().filter_map(|e| e.ok()) {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("rs") {
                let content = fs::read_to_string(path).unwrap_or_default();
                
                if content.contains("std::fs::write") || content.contains("tokio::fs::write") {
                    violations.push(format!("Forbidden fs::write in wasm4pm-lsp {:?}", path));
                }
            }
        }
    }

    if !violations.is_empty() {
        panic!("NO_LSP_MUTATION_LOCK violated:\n{}", violations.join("\n"));
    }
}
