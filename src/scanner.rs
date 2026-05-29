use std::path::PathBuf;
use anyhow::{Result, Context};
use walkdir::WalkDir;
use crate::models::{FileEntry, Symbol, DetectedCapability};
use crate::db;
use crate::receipt;
use crate::projection;
use crate::symbol;
use crate::capability;
use crate::rdf;

pub fn scan(paths: &[PathBuf], out: &PathBuf) -> Result<()> {
    std::fs::create_dir_all(out)?;
    std::fs::create_dir_all(out.join("receipts"))?;
    std::fs::create_dir_all(out.join("reports"))?;
    std::fs::create_dir_all(out.join("catalog"))?;
    
    let mut files = Vec::new();
    let mut all_symbols = Vec::new();
    let mut all_capabilities = Vec::new();
    
    for p in paths {
        for entry in WalkDir::new(p).into_iter().filter_map(|e| e.ok()) {
            if entry.file_type().is_file() {
                let path_str = entry.path().to_string_lossy().to_string();
                let content = std::fs::read_to_string(&entry.path()).unwrap_or_default();
                let hash = blake3::hash(content.as_bytes()).to_hex().to_string();
                let size = entry.metadata().map(|m| m.len() as i64).unwrap_or(0);
                let ext = entry.path().extension().map(|e| e.to_string_lossy().to_string()).unwrap_or_default();
                
                files.push(FileEntry {
                    path: path_str.clone(),
                    hash,
                    size,
                    language: ext,
                });
                
                all_symbols.extend(symbol::extract_symbols(&path, &content));
                all_capabilities.extend(capability::detect_capabilities(&path, &content, &symbols));
            }
        }
    }
    
    let rec = receipt::generate_receipt(out, &files)?;
    
    let receipt_path = out.join("receipts").join(format!("scan-{}.receipt.toml", chrono::Utc::now().timestamp()));
    let receipt_toml = toml::to_string(&rec)?;
    std::fs::write(&receipt_path, receipt_toml)?;
    
    // SQLite as accelerator/local cache
    db::init_db(out)?;
    db::insert_files(out, &files)?;
    
    // Emit cpmp-catalog.ttl and cpmp-shapes.ttl
    rdf::project_to_rdf(&files, &all_capabilities, &rec, out)?;
    
    let catalog_ttl = out.join("catalog/cpmp-catalog.ttl");
    let shapes_ttl = out.join("catalog/cpmp-shapes.ttl");

    let validate_status = std::process::Command::new("open-ontologies")
        .arg("validate")
        .arg(&catalog_ttl)
        .status();
    
    if validate_status.is_err() || !validate_status.unwrap().success() {
        return Err(anyhow::anyhow!("catalog TTL does not parse or open-ontologies validate failed"));
    }

    let load_status = std::process::Command::new("open-ontologies")
        .arg("load")
        .arg(&catalog_ttl)
        .status();

    if load_status.is_err() || !load_status.unwrap().success() {
        return Err(anyhow::anyhow!("Open Ontologies cannot load the catalog"));
    }
    
    let shacl_status = std::process::Command::new("open-ontologies")
        .arg("shacl")
        .arg(&shapes_ttl)
        .status();
    
    if shacl_status.is_err() || !shacl_status.unwrap().success() {
        return Err(anyhow::anyhow!("SHACL fails without an explicit report"));
    }

    projection::generate_reports_open_ontologies(out, &catalog_ttl)?;
    
    let version_status = std::process::Command::new("open-ontologies")
        .arg("version")
        .arg(format!("v-{}", rec.id))
        .status();
    
    if version_status.is_err() || !version_status.unwrap().success() {
        return Err(anyhow::anyhow!("no Open Ontologies version is emitted"));
    }
    
    let tests_detected = files.iter().any(|f| f.path.contains("test"));
    if tests_detected {
        println!("Tests detected.");
    }
    
    println!("Scan complete. {} files, {} symbols, {} capabilities inventoried into Open Ontologies store.", files.len(), all_symbols.len(), all_capabilities.len());
    Ok(())
}
