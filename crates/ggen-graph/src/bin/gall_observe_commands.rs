#![allow(clippy::unwrap_used, clippy::unnecessary_debug_formatting)]
use chrono::Utc;
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::model::Term;
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs::File;
use std::process::Command;

#[derive(Serialize, Deserialize, Debug)]
struct Transcript {
    command: String,
    argv: Vec<String>,
    cwd: String,
    exit_code: i32,
    duration_ms: f64,
    stdout_path: String,
    stderr_path: String,
    stdout_sha256: String,
    stderr_sha256: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let workspace_root = std::env::current_dir()?;
    let transcripts_dir = workspace_root.join("crates/ggen-graph/audit/transcripts");
    std::fs::create_dir_all(&transcripts_dir)?;

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
                             dcterms:identifier \"run_commands\" .
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
        println!("W1: No BoundaryExecutionRequest for command executions. Skipping run.");
        return Ok(());
    }

    let scripts = vec![
        "00_capture_baseline.sh",
        "01_extract_requirements.sh",
        "02_verify_package_constraints.sh",
        "03_check_feature_flags.sh",
        "04_run_unit_tests.sh",
        "05_run_integration_tests.sh",
        "06_scan_forbidden_surfaces.sh",
        "07_check_anti_fake.sh",
        "08_verify_replay_receipts.sh",
        "09_verify_ocel_self_audit.sh",
        "10_verify_coverage_matrix.sh",
        "11_verify_proof_report.sh",
        "12_detect_contradictions.sh",
        "13_adjudicate_gall_promotion.sh",
    ];

    println!("W1: Observing command executions for scripts 00-13...");

    for script_name in scripts {
        let script_base = script_name.strip_suffix(".sh").unwrap_or(script_name);
        let script_path = workspace_root
            .join("scripts/gall/external")
            .join(script_name);

        if !script_path.exists() {
            eprintln!("Warning: Script not found: {}", script_path.display());
            continue;
        }

        println!("Running {}...", script_name);

        let start_time_utc = Utc::now();
        let start_time_inst = std::time::Instant::now();

        // Run the script. Set TRANSCRIPT_WRAPPED=true so it executes the body directly
        let output = Command::new(&script_path)
            .env("TRANSCRIPT_WRAPPED", "true")
            .output()?;

        let duration_ms = start_time_inst.elapsed().as_secs_f64() * 1000.0;
        let end_time_utc = Utc::now();
        let exit_code = output.status.code().unwrap_or(-1);

        // Compute hashes
        let mut hasher_out = Sha256::new();
        hasher_out.update(&output.stdout);
        let stdout_sha256 = hex::encode(hasher_out.finalize());

        let mut hasher_err = Sha256::new();
        hasher_err.update(&output.stderr);
        let stderr_sha256 = hex::encode(hasher_err.finalize());

        // Write stdout/stderr files
        let stdout_file_name = format!("{}.stdout", script_base);
        let stderr_file_name = format!("{}.stderr", script_base);

        std::fs::write(transcripts_dir.join(&stdout_file_name), &output.stdout)?;
        std::fs::write(transcripts_dir.join(&stderr_file_name), &output.stderr)?;

        // Write transcript JSON
        let transcript = Transcript {
            command: script_base.to_string(),
            argv: vec![script_path.to_string_lossy().to_string()],
            cwd: workspace_root.to_string_lossy().to_string(),
            exit_code,
            duration_ms,
            stdout_path: format!("crates/ggen-graph/audit/transcripts/{}", stdout_file_name),
            stderr_path: format!("crates/ggen-graph/audit/transcripts/{}", stderr_file_name),
            stdout_sha256: stdout_sha256.clone(),
            stderr_sha256: stderr_sha256.clone(),
        };

        let json_file = File::create(transcripts_dir.join(format!("{}.json", script_base)))?;
        serde_json::to_writer_pretty(json_file, &transcript)?;

        // Write individual TTL facts
        let mut ttl_content = String::new();
        ttl_content.push_str("@base <http://example.org/> .\n");
        ttl_content.push_str("@prefix prov: <http://www.w3.org/ns/prov#> .\n");
        ttl_content.push_str("@prefix dcterms: <http://purl.org/dc/terms/> .\n");
        ttl_content.push_str("@prefix spdx: <http://spdx.org/rdf/terms#> .\n");
        ttl_content.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n");

        let script_base_sanitized = script_base
            .replace("gall", "witnessed")
            .replace("GALL", "witnessed");
        let cmd_iri = format!("<#activity_{}>", script_base_sanitized);
        let stdout_iri = format!("<#stdout_{}>", script_base_sanitized);
        let stderr_iri = format!("<#stderr_{}>", script_base_sanitized);

        ttl_content.push_str(&format!(
            "{} a prov:Activity ;\n    \
             dcterms:identifier {:?} ;\n    \
             prov:startedAtTime {:?} ; \n    \
             prov:endedAtTime {:?} ;\n    \
             prov:value {} \n",
            cmd_iri,
            script_base,
            start_time_utc.to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
            end_time_utc.to_rfc3339_opts(chrono::SecondsFormat::Millis, true),
            exit_code
        ));

        if !req_iri.is_empty() {
            ttl_content.push_str(&format!("    ; prov:wasInformedBy {} .\n\n", req_iri));
        } else {
            ttl_content.push_str(".\n\n");
        }

        // Stdout Entity
        ttl_content.push_str(&format!(
            "{} a prov:Entity ;\n    \
             prov:wasGeneratedBy {} ;\n    \
             dcterms:title {:?} ;\n    \
             spdx:checksum [\n        \
                  a spdx:Checksum ;\n        \
                  spdx:algorithm <http://spdx.org/rdf/terms#checksumAlgorithm_sha256> ;\n        \
                  spdx:checksumValue {:?}\n    \
             ] .\n\n",
            stdout_iri,
            cmd_iri,
            format!("{}.stdout", script_base),
            stdout_sha256
        ));

        // Stderr Entity
        ttl_content.push_str(&format!(
            "{} a prov:Entity ;\n    \
             prov:wasGeneratedBy {} ;\n    \
             dcterms:title {:?} ;\n    \
             spdx:checksum [\n        \
                  a spdx:Checksum ;\n        \
                  spdx:algorithm <http://spdx.org/rdf/terms#checksumAlgorithm_sha256> ;\n        \
                  spdx:checksumValue {:?}\n    \
             ] .\n",
            stderr_iri,
            cmd_iri,
            format!("{}.stderr", script_base),
            stderr_sha256
        ));

        std::fs::write(
            transcripts_dir.join(format!("{}.ttl", script_base)),
            ttl_content,
        )?;
    }

    println!("W1: Command transcripts and TTL facts captured successfully.");
    Ok(())
}
