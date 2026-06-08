use std::fs;
use std::path::PathBuf;
use walkdir::WalkDir;

#[test]
fn test_gc008_clap_command_route_lock() {
    let mut violations = Vec::new();
    let lsp_path = PathBuf::from("/Users/sac/wasm4pm/crates/wasm4pm-lsp/src");

    if lsp_path.exists() {
        for entry in WalkDir::new(&lsp_path).into_iter().filter_map(|e| e.ok()) {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("rs") {
                let content = fs::read_to_string(path).unwrap_or_default();
                
                if content.contains("WorkspaceEdit") {
                    violations.push(format!("Forbidden WorkspaceEdit in wasm4pm-lsp {:?}", path));
                }
                if content.contains("wasm4pm.bind_receipt") {
                    violations.push(format!("Forbidden wasm4pm.bind_receipt in wasm4pm-lsp {:?}", path));
                }
                if content.contains("std::fs::read_to_string") && content.contains("receipt") {
                    violations.push(format!("Forbidden direct receipt binding via fs in {:?}", path));
                }
            }
        }
    }
    
    // Also check for the correct CodeAction intent presence
    let main_rs = lsp_path.join("main.rs");
    if main_rs.exists() {
        let content = fs::read_to_string(&main_rs).unwrap_or_default();
        if !content.contains("conformance-receipt.bind") {
            violations.push("Missing CLAP-governed 'conformance-receipt bind' CodeAction in wasm4pm-lsp".to_string());
        }
    }

    if !violations.is_empty() {
        panic!("CLAP_COMMAND_ROUTE_LOCK violated:\n{}", violations.join("\n"));
    }
}
