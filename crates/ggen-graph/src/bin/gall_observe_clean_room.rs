#![allow(clippy::unwrap_used, clippy::unnecessary_debug_formatting)]
use chrono::Utc;
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::model::Term;
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use std::fs::{copy, create_dir_all, read_dir, File};
use std::path::Path;
use std::process::Command;
use std::time::Instant;

#[derive(Serialize, Deserialize, Debug)]
struct CleanRoomPayload {
    timestamp: String,
    clean_room_directory: String,
    rebuild_status: String,
    exit_code: i32,
    duration_seconds: u64,
}

fn copy_dir_all(src: &Path, dst: &Path) -> std::io::Result<()> {
    create_dir_all(dst)?;
    for entry in read_dir(src)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        let name = entry.file_name();
        let name_str = name.to_string_lossy();

        if name_str == "target"
            || name_str == ".git"
            || name_str == ".agents"
            || name_str == ".gemini"
            || name_str == ".antigravitycli"
            || name_str == ".claude"
            || name_str == ".cursor"
            || name_str == ".vscode"
        {
            continue;
        }

        let src_path = entry.path();
        let dst_path = dst.join(&name);

        if ty.is_dir() {
            copy_dir_all(&src_path, &dst_path)?;
        } else if ty.is_file() || ty.is_symlink() {
            if let Err(e) = copy(&src_path, &dst_path) {
                eprintln!("Warning: Failed to copy {:?}: {}", src_path, e);
            }
        }
    }
    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let workspace_root = std::env::current_dir()?;

    // Check if we should execute based on BoundaryExecutionRequest in delta
    let delta_path = workspace_root.join("crates/ggen-graph/audit/gall_decision.delta.ttl");
    let mut should_run = !delta_path.exists();
    let mut req_iri = String::new();
    if delta_path.exists() {
        let store = Store::new()?;
        if let Ok(f) = File::open(&delta_path) {
            if let Ok(()) = store.load_from_reader(RdfParser::from_format(RdfFormat::Turtle), f) {
                let query = "
                    PREFIX prov: <http://www.w3.org/ns/prov#>
                    PREFIX dcterms: <http://purl.org/dc/terms/>
                    SELECT ?req WHERE {
                        ?req a prov:Activity ;
                             dcterms:type \"BoundaryExecutionRequest\" ;
                             dcterms:identifier \"run_clean_room\" .
                    }
                ";
                #[allow(deprecated)]
                if let Ok(QueryResults::Solutions(mut solutions)) = store.query(query) {
                    if let Some(Ok(sol)) = solutions.next() {
                        if let Some(Term::BlankNode(b)) = sol.get("req") {
                            req_iri = format!("_:{}", b.as_str());
                        } else if let Some(Term::NamedNode(n)) = sol.get("req") {
                            req_iri = format!("<{}>", n.as_str());
                        }
                    }
                }
            }
        }
        should_run = !req_iri.is_empty();
    }

    if !should_run {
        println!("W3: No BoundaryExecutionRequest for clean room rebuild. Skipping run.");
        return Ok(());
    }

    let temp_dir = tempfile::tempdir()?;
    let temp_path = temp_dir.path();

    println!("W3: Copying workspace to clean room at {:?}", temp_path);
    copy_dir_all(&workspace_root, temp_path)?;

    let start = Instant::now();

    println!("W3: Running cargo build & test inside clean room...");
    let build_status = Command::new("cargo")
        .args(["build", "-p", "ggen-graph", "--all-targets"])
        .current_dir(temp_path)
        .stdout(std::process::Stdio::inherit())
        .stderr(std::process::Stdio::inherit())
        .status()?;

    let test_status = if build_status.success() {
        Command::new("cargo")
            .args(["test", "-p", "ggen-graph"])
            .current_dir(temp_path)
            .stdout(std::process::Stdio::inherit())
            .stderr(std::process::Stdio::inherit())
            .status()?
    } else {
        build_status
    };

    let duration = start.elapsed().as_secs();
    let success = build_status.success() && test_status.success();
    let exit_code = if success { 0 } else { 1 };
    let rebuild_status = if success {
        "PASS".to_string()
    } else {
        "FAIL".to_string()
    };

    let payload = CleanRoomPayload {
        timestamp: Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
        clean_room_directory: temp_path.to_string_lossy().to_string(),
        rebuild_status,
        exit_code,
        duration_seconds: duration,
    };

    let audit_dir = workspace_root.join("crates/ggen-graph/audit");
    create_dir_all(&audit_dir)?;

    // JSON Output
    let out_file = File::create(audit_dir.join("clean_room_rebuild.json"))?;
    serde_json::to_writer_pretty(out_file, &payload)?;

    // RDF Turtle Output
    let mut ttl_content = String::new();
    ttl_content.push_str("@base <http://example.org/> .\n");
    ttl_content.push_str("@prefix prov: <http://www.w3.org/ns/prov#> .\n");
    ttl_content.push_str("@prefix sh: <http://www.w3.org/ns/shacl#> .\n");
    ttl_content.push_str("@prefix dcterms: <http://purl.org/dc/terms/> .\n");
    ttl_content.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n");

    let safe_req_iri = if req_iri.is_empty() {
        "<#manual_clean_room_request>"
    } else {
        &req_iri
    };

    let description = format!("\"Clean-room build duration: {} seconds\"", duration);
    ttl_content.push_str(&format!(
        "<#clean_room_rebuild> a prov:Entity ;\n    \
         dcterms:identifier \"clean_room_rebuild\" ;\n    \
         dcterms:type \"BoundaryExecutionRequest\" ;\n    \
         sh:conforms {} ;\n    \
         prov:value {} ;\n    \
         dcterms:description {} ;\n    \
         prov:wasGeneratedBy {} .\n",
        if payload.rebuild_status == "PASS" {
            "true"
        } else {
            "false"
        },
        exit_code,
        description,
        safe_req_iri
    ));
    std::fs::write(audit_dir.join("clean_room_rebuild.ttl"), ttl_content)?;

    if success {
        println!("W3: Clean-room build verification passed.");
        Ok(())
    } else {
        eprintln!("W3: Clean-room build verification failed.");
        std::process::exit(1);
    }
}
