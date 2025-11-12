//! File Organization Tests
//!
//! These tests verify that files are placed in the correct directories
//! to prevent waste accumulation in the root directory.

use std::fs;
use std::path::{Path, PathBuf};

/// Verify that no test files are in the root directory
#[test]
fn test_no_test_files_in_root() {
    let root = PathBuf::from(".");
    let test_files = find_files_with_extension(&root, "rs")
        .into_iter()
        .filter(|p| {
            // Allow Cargo.toml and other config files
            let name = p.file_name().unwrap().to_string_lossy();
            !name.starts_with("Cargo") && !name.starts_with(".")
        })
        .collect::<Vec<_>>();

    if !test_files.is_empty() {
        panic!(
            "Found test files in root directory. Move them to tests/:\n{}",
            test_files
                .iter()
                .map(|p| format!("  - {}", p.display()))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
}

/// Verify that no template files are in the root directory
#[test]
fn test_no_template_files_in_root() {
    let root = PathBuf::from(".");
    let template_files = find_files_with_extension(&root, "tmpl");

    if !template_files.is_empty() {
        panic!(
            "Found template files in root directory. Move them to templates/:\n{}",
            template_files
                .iter()
                .map(|p| format!("  - {}", p.display()))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
}

/// Verify that no data files are in the root directory
#[test]
fn test_no_data_files_in_root() {
    let root = PathBuf::from(".");
    let data_extensions = ["ttl", "rdf", "json"];
    let mut data_files = Vec::new();

    for ext in &data_extensions {
        data_files.extend(find_files_with_extension(&root, ext));
    }

    // Filter out allowed files
    let data_files: Vec<_> = data_files
        .into_iter()
        .filter(|p| {
            let name = p.file_name().unwrap().to_string_lossy();
            // Allow Cargo.lock and other essential files
            name != "Cargo.lock" && name != "ggen.lock"
        })
        .collect();

    if !data_files.is_empty() {
        panic!(
            "Found data files in root directory. Move them to tests/data/ or examples/:\n{}",
            data_files
                .iter()
                .map(|p| format!("  - {}", p.display()))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
}

/// Verify that no scripts are in the root directory
#[test]
fn test_no_scripts_in_root() {
    let root = PathBuf::from(".");
    let script_files = find_files_with_extension(&root, "sh");

    if !script_files.is_empty() {
        panic!(
            "Found script files in root directory. Move them to scripts/:\n{}",
            script_files
                .iter()
                .map(|p| format!("  - {}", p.display()))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
}

/// Verify that only Makefile.toml exists (cargo make is standard)
#[test]
fn test_only_makefile_toml_in_root() {
    let root = PathBuf::from(".");
    let makefile_files: Vec<_> = ["Makefile", "make.toml", "Makefile.ultra-deploy"]
        .iter()
        .filter_map(|name| {
            let path = root.join(name);
            if path.exists() {
                Some(path)
            } else {
                None
            }
        })
        .collect();

    if !makefile_files.is_empty() {
        panic!(
            "Found redundant Makefile files in root. Only Makefile.toml should exist:\n{}",
            makefile_files
                .iter()
                .map(|p| format!("  - {}", p.display()))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
}

/// Verify that no temporary files are in the root directory
#[test]
fn test_no_temporary_files_in_root() {
    let root = PathBuf::from(".");
    let temp_patterns = [
        "*.tmp",
        "*.temp",
        "*.bak",
        "*.swp",
        "*.long-type-*.txt",
        "*~",
    ];

    let mut temp_files = Vec::new();
    if let Ok(entries) = fs::read_dir(&root) {
        for entry in entries.flatten() {
            let path = entry.path();
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                for pattern in &temp_patterns {
                    if matches_pattern(name, pattern) {
                        temp_files.push(path.clone());
                        break;
                    }
                }
            }
        }
    }

    if !temp_files.is_empty() {
        panic!(
            "Found temporary files in root directory. Delete them:\n{}",
            temp_files
                .iter()
                .map(|p| format!("  - {}", p.display()))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }
}

/// Helper function to find files with a specific extension in a directory
fn find_files_with_extension(dir: &Path, extension: &str) -> Vec<PathBuf> {
    let mut files = Vec::new();
    if let Ok(entries) = fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_file() {
                if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
                    if ext == extension {
                        files.push(path);
                    }
                }
            }
        }
    }
    files
}

/// Helper function to check if a filename matches a pattern
fn matches_pattern(name: &str, pattern: &str) -> bool {
    // Simple pattern matching for common temp file patterns
    if pattern.contains("*") {
        let parts: Vec<&str> = pattern.split('*').collect();
        if parts.len() == 2 {
            name.starts_with(parts[0]) && name.ends_with(parts[1])
        } else if parts.len() == 1 {
            name.contains(parts[0])
        } else {
            false
        }
    } else {
        name == pattern
    }
}
