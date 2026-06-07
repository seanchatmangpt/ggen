use std::path::{Path, PathBuf};
use std::fs;

fn walk_dir(dir: &Path, violations: &mut Vec<String>) {
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            let name = path.file_name().unwrap_or_default().to_string_lossy();
            if name == ".git" || name == "target" || name == "vendors" || name == "scratch" {
                continue;
            }
            if path.is_dir() {
                walk_dir(&path, violations);
            } else if path.is_file() {
                if name == "Cargo.toml" || name == "pack.toml" || name == "sync_target.rs" {
                    if let Ok(content) = fs::read_to_string(&path) {
                        for (i, line) in content.lines().enumerate() {
                            if line.contains("1.0.0") || line.contains("v1.0.0") {
                                violations.push(format!("Violation in {:?} at line {}: {}", path, i + 1, line.trim()));
                            }
                        }
                    }
                }
            }
        }
    }
}

#[test]
fn test_gc006_release_law_calver_lock() {
    let current_dir = std::env::current_dir().unwrap();
    let tower_lsp_max_root = current_dir.parent().unwrap().parent().unwrap().to_path_buf();
    let ggen_root = tower_lsp_max_root.parent().unwrap().join("ggen");

    let workspaces_to_check = vec![tower_lsp_max_root, ggen_root];
    let mut violations = Vec::new();

    for ws in workspaces_to_check {
        walk_dir(&ws, &mut violations);
    }

    if !violations.is_empty() {
        panic!("RELEASE_LAW_CALVER_LOCK violated. Found forbidden version 1.0.0:\n{}", violations.join("\n"));
    }
}
