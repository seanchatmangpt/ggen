#![allow(clippy::unwrap_used, clippy::unnecessary_debug_formatting)]
use chrono::Utc;
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::model::Term;
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use std::fs::File;

#[derive(Serialize, Deserialize, Debug)]
struct AdjudicationPayload {
    timestamp: String,
    verdict: String,
    reason: String,
    integrity_status: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct SignedAdjudicationPayload {
    timestamp: String,
    verdict: String,
    reason: String,
    integrity_status: String,
    witness_adjudication_blake3_receipt: String,
}

fn check_validation_report_conforms(
    path: &std::path::Path,
) -> Result<bool, Box<dyn std::error::Error>> {
    if !path.exists() {
        return Ok(false);
    }
    let store = Store::new()?;
    let f = File::open(path)?;
    store.load_from_reader(RdfParser::from_format(RdfFormat::Turtle), f)?;
    let query = "
        PREFIX sh: <http://www.w3.org/ns/shacl#>
        ASK {
            ?report a sh:ValidationReport ;
                    sh:conforms true .
        }
    ";
    #[allow(deprecated)]
    let results = store.query(query)?;
    if let oxigraph::sparql::QueryResults::Boolean(b) = results {
        Ok(b)
    } else {
        Ok(false)
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let workspace_root = std::env::current_dir()?;
    let audit_dir = workspace_root.join("crates/ggen-graph/audit");
    let delta_path = audit_dir.join("gall_decision.delta.ttl");
    let adjudication_file = audit_dir.join("witnessed_truthfulness.external_adjudication.json");

    println!("=== Running Witnessed Agent Truthfulness Adjudication ===");

    let mut violations = 0;
    let mut reason_details = Vec::new();

    // 1. Verify that all required audit logs exist
    let required_audit_files = vec![
        "worktree_inventory.full.json",
        "sabotage_results.json",
        "clean_room_rebuild.json",
        "doctest_results.json",
        "gall_evidence.ttl",
        "gall_decision.delta.ttl",
        "gall_code_evaluation.receipt.ttl",
        "gall_code_evaluation.final.ttl",
    ];

    for filename in &required_audit_files {
        let path = audit_dir.join(filename);
        if !path.exists() {
            violations += 1;
            reason_details.push(format!("Missing required audit file: {}", filename));
        }
    }

    // 2. Load delta and check checkpoints using SPARQL
    let mut passed_checkpoints = std::collections::HashSet::new();
    if delta_path.exists() {
        let store = Store::new()?;
        let f = File::open(&delta_path)?;
        if let Err(e) = store.load_from_reader(RdfParser::from_format(RdfFormat::Turtle), f) {
            violations += 1;
            reason_details.push(format!("Failed to parse delta Turtle: {}", e));
        } else {
            let query = "
                PREFIX sh: <http://www.w3.org/ns/shacl#>
                PREFIX dcterms: <http://purl.org/dc/terms/>
                SELECT ?name ?conforms WHERE {
                    ?cp a sh:ValidationReport ;
                        dcterms:identifier ?name ;
                        sh:conforms ?conforms .
                }
            ";
            #[allow(deprecated)]
            if let Ok(oxigraph::sparql::QueryResults::Solutions(solutions)) = store.query(query) {
                for s in solutions.flatten() {
                    let name = match s.get("name") {
                        Some(Term::Literal(lit)) => lit.value().to_string(),
                        _ => continue,
                    };
                    let conforms = match s.get("conforms") {
                        Some(Term::Literal(lit)) => lit.value() == "true",
                        _ => false,
                    };

                    if conforms {
                        passed_checkpoints.insert(name);
                    } else {
                        violations += 1;
                        reason_details.push(format!("Checkpoint {} failed", name));
                    }
                }
            }
        }
    } else {
        violations += 1;
        reason_details.push("Decision delta file does not exist".to_string());
    }

    // Ensure all 10 checkpoints (W0-W9) plus 5 dialect check hooks passed
    let required_checkpoints = vec![
        "W0",
        "W1",
        "W2",
        "W3",
        "W4",
        "W5",
        "W6",
        "W7",
        "W8",
        "W9",
        "W-DIALECT-SPARQL",
        "W-DIALECT-SHACL",
        "W-DIALECT-N3",
        "W-DIALECT-DATALOG",
        "W-DIALECT-SHEX",
    ];
    for cp in &required_checkpoints {
        if !passed_checkpoints.contains(*cp) {
            violations += 1;
            reason_details.push(format!(
                "Checkpoint {} did not pass or was not evaluated",
                cp
            ));
        }
    }

    // 3. Chronological order check on gall_evidence.ocel.json
    let ocel_file_path = audit_dir.join("gall_evidence.ocel.json");
    if ocel_file_path.exists() {
        let f = File::open(&ocel_file_path)?;
        match serde_json::from_reader::<_, ggen_graph::ocel::OcelLog>(f) {
            Ok(log) => {
                for i in 1..log.events.len() {
                    if log.events[i].timestamp < log.events[i - 1].timestamp {
                        violations += 1;
                        reason_details.push(format!(
                            "Chronological order violation in OCEL: event {} timestamp ({}) is before event {} timestamp ({})",
                            log.events[i].id, log.events[i].timestamp,
                            log.events[i-1].id, log.events[i-1].timestamp
                        ));
                    }
                }
            }
            Err(e) => {
                violations += 1;
                reason_details.push(format!("Failed to parse OCEL evidence log: {}", e));
            }
        }
    } else {
        violations += 1;
        reason_details.push("OCEL evidence log file does not exist".to_string());
    }

    // Verify that the other 4 validation reports exist and conform
    let other_reports = vec![
        ("public_vocab.validation.ttl", "R7 Public Vocabulary Gate"),
        (
            "hook_actuation.validation.ttl",
            "Hook Actuation Conformance",
        ),
        (
            "dialect_completeness.validation.ttl",
            "Dialect Completeness Matrix",
        ),
        ("sabotage.validation.ttl", "Sabotage Suite Verification"),
    ];

    for (filename, _desc) in other_reports {
        let path = audit_dir.join(filename);
        match check_validation_report_conforms(&path) {
            Ok(true) => {}
            Ok(false) => {
                violations += 1;
                reason_details.push(format!(
                    "Validation report {} exists but does not conform, or is missing",
                    filename
                ));
            }
            Err(e) => {
                violations += 1;
                reason_details.push(format!(
                    "Failed to parse validation report {}: {}",
                    filename, e
                ));
            }
        }
    }

    // 4. Adjudicate verdict
    let (verdict, reason, integrity_status) = if violations == 0 {
        (
            "Promoted".to_string(),
            "Witnessed Agent Truthfulness validation checks satisfied. Checkpoints W0-W9 and Dialect matrix verified passed.".to_string(),
            "PASS".to_string(),
        )
    } else {
        let msg = format!(
            "Witnessed Agent Adjudication refused. {} violation(s) detected: {}",
            violations,
            reason_details.join("; ")
        );
        ("Refused".to_string(), msg, "FAIL".to_string())
    };

    // Write final.validation.ttl
    let mut fin_ttl = String::new();
    fin_ttl.push_str("@base <http://example.org/> .\n");
    fin_ttl.push_str("@prefix sh: <http://www.w3.org/ns/shacl#> .\n");
    fin_ttl.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n");
    fin_ttl.push_str("@prefix dcterms: <http://purl.org/dc/terms/> .\n\n");
    fin_ttl.push_str("<#final_adjudication_report> a sh:ValidationReport ;\n");
    fin_ttl.push_str(&format!(
        "    sh:conforms \"{}\"^^xsd:boolean ;\n",
        violations == 0
    ));
    fin_ttl.push_str("    dcterms:identifier \"final.validation\" ;\n");
    fin_ttl.push_str("    dcterms:description \"Final Adjudication Report\" ;\n");
    if violations > 0 {
        fin_ttl.push_str("    sh:result [\n");
        fin_ttl.push_str("        a sh:ValidationResult ;\n");
        fin_ttl.push_str("        sh:resultSeverity sh:Violation ;\n");
        let esc_reason = reason.replace('"', "\\\"");
        fin_ttl.push_str(&format!("        sh:resultMessage \"{}\"\n", esc_reason));
        fin_ttl.push_str("    ] .\n");
    } else {
        fin_ttl.push_str("    dcterms:title \"All checkpoints passed. Promotion approved.\" .\n");
    }
    std::fs::write(audit_dir.join("final.validation.ttl"), &fin_ttl)?;

    let temp_payload = AdjudicationPayload {
        timestamp: Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
        verdict: verdict.clone(),
        reason: reason.clone(),
        integrity_status: integrity_status.clone(),
    };

    // Serialize temp payload to calculate BLAKE3 receipt
    let temp_bytes = serde_json::to_vec_pretty(&temp_payload)?;
    let blake3_receipt = blake3::hash(&temp_bytes).to_hex().to_string();

    let signed_payload = SignedAdjudicationPayload {
        timestamp: temp_payload.timestamp,
        verdict: verdict.clone(),
        reason,
        integrity_status,
        witness_adjudication_blake3_receipt: blake3_receipt,
    };

    let out_file = File::create(&adjudication_file)?;
    serde_json::to_writer_pretty(out_file, &signed_payload)?;

    println!("VERDICT: {}", verdict);
    println!("Results saved to {}", adjudication_file.display());

    if verdict == "Promoted" {
        Ok(())
    } else {
        std::process::exit(1);
    }
}
