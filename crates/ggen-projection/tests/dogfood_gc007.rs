use std::path::PathBuf;
use std::fs;
use walkdir::WalkDir;

#[test]
fn test_gc007_wasm4pm_lsp_ownership_surface() {
    let workspace = "/Users/sac/ggen";
    let tower_workspace = "/Users/sac/tower-lsp-max"; // If existed
    
    let mut workspaces_to_check = vec![PathBuf::from(workspace)];
    if PathBuf::from(tower_workspace).exists() {
        workspaces_to_check.push(PathBuf::from(tower_workspace));
    }

    let mut violations = Vec::new();

    for ws in workspaces_to_check {
        let walker = WalkDir::new(&ws).into_iter().filter_entry(|e| {
            let name = e.file_name().to_string_lossy();
            name != ".git" && name != "target" && name != "node_modules" && name != "vendors" && name != "scratch"
        });
        for entry in walker.filter_map(|e| e.ok()) {
            let path = entry.path();
            if path.is_dir() {
                let name = path.file_name().unwrap_or_default().to_string_lossy();
                if name == "wasm4pm-lsp" {
                    violations.push(format!("Forbidden wasm4pm-lsp implementation found at: {:?}", path));
                }
            }
        }
    }

    if !violations.is_empty() {
        panic!("OWNERSHIP_SURFACE_LOCK violated. Found forbidden wasm4pm-lsp implementations outside ~/wasm4pm:\n{}", violations.join("\n"));
    }
}
