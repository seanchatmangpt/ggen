use crate::capability;
use crate::models::{FileEntry, Symbol};
use crate::symbol;
use anyhow::Result;
use std::path::PathBuf;
use walkdir::WalkDir;

pub fn scan(paths: &[PathBuf], out: &PathBuf) -> Result<()> {
    std::fs::create_dir_all(out)?;

    let mut files = Vec::new();
    let mut all_symbols = Vec::new();

    // 1. Walk directories and collect files and symbols
    for p in paths {
        for entry in WalkDir::new(p).into_iter().filter_map(|e| e.ok()) {
            if entry.file_type().is_file() {
                let path_str = entry.path().to_string_lossy().to_string();
                let content = std::fs::read_to_string(entry.path()).unwrap_or_default();
                let hash = blake3::hash(content.as_bytes()).to_hex().to_string();
                let size = entry.metadata().map(|m| m.len()).unwrap_or(0);

                let ext = entry
                    .path()
                    .extension()
                    .map(|e| e.to_string_lossy().to_string())
                    .unwrap_or_default();
                let language = crate::models::Language::from_extension(&ext);
                let modified_time = entry
                    .metadata()
                    .ok()
                    .and_then(|m| m.modified().ok())
                    .unwrap_or_else(std::time::SystemTime::now);
                let is_test = path_str.contains("/tests/")
                    || path_str.contains("/test/")
                    || path_str.contains("test");

                // Determine git root
                let mut git_root = None;
                let mut parent = entry.path().parent();
                while let Some(p) = parent {
                    if p.join(".git").exists() {
                        git_root = Some(p.to_string_lossy().to_string());
                        break;
                    }
                    parent = p.parent();
                }

                files.push(FileEntry {
                    path: path_str.clone(),
                    language,
                    size_bytes: size,
                    hash,
                    modified_time,
                    git_root,
                    is_test,
                    is_binary: false,
                });

                all_symbols.extend(symbol::extract_symbols(entry.path(), &content));
            }
        }
    }

    // 2. Detect capabilities passing symbols in the file
    let mut all_capabilities = Vec::new();
    for p in paths {
        for entry in WalkDir::new(p).into_iter().filter_map(|e| e.ok()) {
            if entry.file_type().is_file() {
                let path_str = entry.path().to_string_lossy().to_string();
                let content = std::fs::read_to_string(entry.path()).unwrap_or_default();
                let file_symbols: Vec<Symbol> = all_symbols
                    .iter()
                    .filter(|s| s.file_path == path_str)
                    .cloned()
                    .collect();
                all_capabilities.extend(capability::detect_capabilities(
                    entry.path(),
                    &content,
                    &file_symbols,
                ));
            }
        }
    }

    // Re-classify capabilities in workspace-excluded crates as DORMANT — a
    // stronger, repo-level dormancy signal than per-file `#![cfg(any())]`.
    let scan_root = paths.first().cloned().unwrap_or_default();
    let dormant_crates = crate::classification::dormant_crate_dirs(&scan_root);
    if !dormant_crates.is_empty() {
        for cap in &mut all_capabilities {
            if dormant_crates
                .iter()
                .any(|d| cap.file_path.contains(&format!("crates/{}/", d)))
            {
                cap.classification = "DORMANT".to_string();
            }
        }
    }

    // 3. Project the human-readable capability inventory + per-crate audit
    //    dashboard from the scan data.
    crate::projection::generate_reports(&files, &all_capabilities, out)?;
    crate::projection::generate_audit_dashboard(&files, &all_capabilities, out)?;

    // 4. Emit the scan receipt with a deterministic aggregate hash — the stable
    //    scan identity a downstream ggen admissibility pack binds to. The only
    //    real output artifact is the JSON scan receipt; nothing downstream reads
    //    anything else, so nothing else is claimed.
    let scan_roots: Vec<String> = paths
        .iter()
        .map(|p| p.to_string_lossy().to_string())
        .collect();
    let output_artifacts = vec![out.join("scan-receipt.json").to_string_lossy().to_string()];
    let receipt = crate::receipt::generate_receipt(&files, out, scan_roots, output_artifacts)?;
    println!("Scan receipt: aggregate_hash={}", receipt.aggregate_hash);

    println!(
        "Scan complete. {} files, {} symbols, {} capabilities inventoried.",
        files.len(),
        all_symbols.len(),
        all_capabilities.len()
    );
    println!(
        "Capability inventory: {}",
        out.join("reports/CAPABILITY_INVENTORY.md").display()
    );
    println!(
        "Audit dashboard:      {}",
        out.join("reports/AUDIT_DASHBOARD.md").display()
    );
    Ok(())
}
