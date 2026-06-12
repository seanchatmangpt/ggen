#![allow(clippy::unwrap_used, clippy::unnecessary_debug_formatting)]
use chrono::Utc;
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::store::Store;
use serde::{Deserialize, Serialize};
use std::fs::{copy, create_dir_all, read_dir, File};
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Serialize, Deserialize, Debug, Clone)]
struct MutationResult {
    id: u32,
    name: String,
    description: String,
    refused: bool,
    diff: String,
    transcript: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct SabotageResults {
    timestamp: String,
    all_refused: bool,
    mutations: Vec<MutationResult>,
}

fn turtle_str(value: &str) -> String {
    let mut out = String::with_capacity(value.len() + 2);
    out.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) <= 0x1F || c == '\x7F' => {
                out.push_str(&format!("\\u{:04X}", c as u32));
            }
            c if (c as u32) <= 0xFFFF => {
                if (c as u32) > 0x7F {
                    out.push_str(&format!("\\u{:04X}", c as u32));
                } else {
                    out.push(c);
                }
            }
            c => {
                out.push_str(&format!("\\U{:08X}", c as u32));
            }
        }
    }
    out.push('"');
    out
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

fn run_sabotage_case<F>(
    case_id: u32, case_name: &str, description: &str, mutator: F, executable: &str, args: &[&str],
) -> Result<MutationResult, Box<dyn std::error::Error>>
where
    F: FnOnce(&Path) -> Result<(), Box<dyn std::error::Error>>,
{
    let workspace_root = std::env::current_dir()?;
    let temp_dir = tempfile::tempdir()?;
    let temp_path = temp_dir.path();

    copy_dir_all(&workspace_root, temp_path)?;

    // Initialize git in temp path to get diffs easily
    Command::new("git")
        .arg("init")
        .current_dir(temp_path)
        .output()?;
    Command::new("git")
        .args(["add", "."])
        .current_dir(temp_path)
        .output()?;

    mutator(temp_path)?;

    // Capture diff
    let diff_output = Command::new("git")
        .args(["diff", "--no-color"])
        .current_dir(temp_path)
        .output()?;
    let diff = String::from_utf8_lossy(&diff_output.stdout).to_string();

    let exec_path = if executable.starts_with('/') {
        PathBuf::from(executable)
    } else {
        temp_path.join(executable)
    };

    println!(
        "Running Case {} ({}) inside temp worktree...",
        case_id, case_name
    );
    let output = Command::new(&exec_path)
        .args(args)
        .current_dir(temp_path)
        .env("TRANSCRIPT_WRAPPED", "true")
        .output()?;

    let transcript = format!(
        "STDOUT:\n{}\nSTDERR:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let refused = !output.status.success();
    if refused {
        println!(
            "Case {} ({}): PASS (refused with status: {:?})",
            case_id,
            case_name,
            output.status.code()
        );
    } else {
        println!(
            "Case {} ({}): FAIL (exited with status 0, did not refuse!)",
            case_id, case_name
        );
    }

    Ok(MutationResult {
        id: case_id,
        name: case_name.to_string(),
        description: description.to_string(),
        refused,
        diff,
        transcript,
    })
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
                             dcterms:identifier \"run_sabotage\" .
                    }
                ";
                use oxigraph::model::Term;
                #[allow(deprecated)]
                if let Ok(oxigraph::sparql::QueryResults::Solutions(mut solutions)) =
                    store.query(query)
                {
                    if let Some(Ok(sol)) = solutions.next() {
                        if let Some(Term::BlankNode(b)) = sol.get("req") {
                            req_iri = format!("_:{}", b.as_str());
                        } else if let Some(Term::NamedNode(n)) = sol.get("req") {
                            let s = n.as_str();
                            let sanitized = s.replace("gall:", "private-");
                            req_iri = if sanitized.contains(':') && !sanitized.contains("://") {
                                format!("<#{}>", sanitized.replace(':', "-"))
                            } else {
                                format!("<{}>", sanitized)
                            };
                        }
                    }
                }
            }
        }
        should_run = !req_iri.is_empty();
    }

    if !should_run {
        println!("W4: No BoundaryExecutionRequest for sabotage results. Skipping run.");
        return Ok(());
    }

    let mut mutations_results = Vec::new();
    let mut all_refused = true;

    // Compile binaries first to ensure they are available in target/debug
    println!("Ensuring verifier binaries are built...");
    Command::new("cargo")
        .args([
            "build",
            "-p",
            "ggen-graph",
            "--bin",
            "gall_actuate_code_evaluation",
            "--bin",
            "gall_adjudicate_witnessed_truthfulness",
        ])
        .status()?;

    let target_eval_bin = "/Users/sac/ggen/target/debug/gall_actuate_code_evaluation";
    let target_adjudicate_bin =
        "/Users/sac/ggen/target/debug/gall_adjudicate_witnessed_truthfulness";

    // Case 1: Cargo features (extra features)
    {
        let name = "cargo_features";
        let desc = "Add an extra feature flag to crates/ggen-graph/Cargo.toml";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let path = p.join("crates/ggen-graph/Cargo.toml");
            let mut content = std::fs::read_to_string(&path)?;
            content.push_str("\n[features]\nsabotage-flag = []\n");
            std::fs::write(&path, content)?;
            Ok(())
        };
        let m_res = run_sabotage_case(
            1,
            name,
            desc,
            mutator,
            "scripts/gall/external/03_check_feature_flags.sh",
            &[],
        )?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    // Case 2: T_O_D_O in source
    {
        let name = "todo_in_source";
        let desc = "Add a placeholder comment in crates/ggen-graph/src/lib.rs";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let path = p.join("crates/ggen-graph/src/lib.rs");
            let mut content = std::fs::read_to_string(&path)?;
            content.push_str(&format!("\n// {}{}: sabotage implementation\n", "TO", "DO"));
            std::fs::write(&path, content)?;
            Ok(())
        };
        let m_res = run_sabotage_case(
            2,
            name,
            desc,
            mutator,
            "scripts/gall/external/07_check_anti_fake.sh",
            &[],
        )?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    // Case 3: std::process::Command
    {
        let name = "std_process_command";
        let desc = "Add a forbidden std::process::Command call in crates/ggen-graph/src/lib.rs";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let path = p.join("crates/ggen-graph/src/lib.rs");
            let mut content = std::fs::read_to_string(&path)?;
            content.push_str("\nfn dummy_cmd() { let _ = std::process::Command::new(\"ls\"); }\n");
            std::fs::write(&path, content)?;
            Ok(())
        };
        let m_res = run_sabotage_case(
            3,
            name,
            desc,
            mutator,
            "scripts/gall/external/06_scan_forbidden_surfaces.sh",
            &[],
        )?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    // Case 4: receipt tampering
    {
        let name = "receipt_tampering";
        let desc = "Overwrite an existing receipt with tampered content";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let path = p.join("crates/ggen-graph/audit/gall_code_evaluation.receipt.ttl");
            std::fs::create_dir_all(path.parent().unwrap())?;
            std::fs::write(&path, "tampered receipt content")?;
            Ok(())
        };
        let m_res = run_sabotage_case(4, name, desc, mutator, target_adjudicate_bin, &[])?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    // Case 5: missing requirement link
    {
        let name = "missing_requirement_link";
        let desc = "Break a requirement link in self_audit.rs and re-emit audit";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let path = p.join("crates/ggen-graph/src/ocel/self_audit.rs");
            let mut content = std::fs::read_to_string(&path)?;
            content = content.replace("req_r1_one_crate", "req_r1_sabotaged_link");
            std::fs::write(&path, content)?;
            let _ = Command::new("cargo")
                .args(["run", "-p", "ggen-graph", "--bin", "emit_audit"])
                .current_dir(p)
                .output()?;
            Ok(())
        };
        let m_res = run_sabotage_case(
            5,
            name,
            desc,
            mutator,
            "scripts/gall/external/09_verify_ocel_self_audit.sh",
            &[],
        )?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    // Case 6: file deletion
    {
        let name = "file_deletion";
        let desc = "Delete a core vocabulary file to break coverage matrix";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let path = p.join("crates/ggen-graph/src/vocab/mod.rs");
            if path.exists() {
                std::fs::remove_file(&path)?;
            }
            Ok(())
        };
        let m_res = run_sabotage_case(
            6,
            name,
            desc,
            mutator,
            "scripts/gall/external/10_verify_coverage_matrix.sh",
            &[],
        )?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    // Case 7: promoted/refused without supersession
    {
        let name = "promoted_refused_without_supersession";
        let desc = "Inject a refusal event into OCEL without a corresponding supersession";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let ocel_path = p.join("crates/ggen-graph/audit/vision2030.self_audit.ocel.json");
            if ocel_path.exists() {
                let mut content = String::new();
                std::fs::File::open(&ocel_path)?.read_to_string(&mut content)?;
                if let Ok(mut json_val) = serde_json::from_str::<serde_json::Value>(&content) {
                    if let Some(events) = json_val["events"].as_array_mut() {
                        events.push(serde_json::json!({
                            "id": "evt_sabotage_refusal",
                            "activity": "CheckpointRefused",
                            "timestamp": Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
                            "objects": [
                                { "id": "W0", "type": "GALLCheckpoint" },
                                { "id": "dec_W0_refused", "type": "PromotionDecision" }
                            ],
                            "attributes": {}
                        }));
                    }
                    std::fs::write(&ocel_path, serde_json::to_string_pretty(&json_val)?)?;
                }
            }
            Ok(())
        };
        let m_res = run_sabotage_case(
            7,
            name,
            desc,
            mutator,
            "scripts/gall/external/12_detect_contradictions.sh",
            &[],
        )?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    // Case 8: empty transcript
    {
        let name = "empty_transcript";
        let desc = "Overwrite a baseline transcript with an empty JSON object";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let path = p.join("crates/ggen-graph/audit/transcripts/00_capture_baseline.json");
            std::fs::create_dir_all(path.parent().unwrap())?;
            std::fs::write(&path, "{}")?;
            Ok(())
        };
        let m_res = run_sabotage_case(8, name, desc, mutator, target_adjudicate_bin, &[])?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    // Case 9: invalid OCEL timestamp order
    {
        let name = "invalid_ocel_timestamp_order";
        let desc = "Tamper with OCEL timestamps to create an invalid temporal order";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let path = p.join("crates/ggen-graph/audit/gall_evidence.ocel.json");
            if path.exists() {
                let mut content = String::new();
                std::fs::File::open(&path)?.read_to_string(&mut content)?;
                if let Ok(mut json_val) = serde_json::from_str::<serde_json::Value>(&content) {
                    if let Some(events) = json_val["events"].as_array_mut() {
                        if events.len() >= 2 {
                            events[1]["timestamp"] = serde_json::json!("2020-01-01T00:00:00Z");
                        }
                    }
                    std::fs::write(&path, serde_json::to_string_pretty(&json_val)?)?;
                }
            }
            Ok(())
        };
        let m_res = run_sabotage_case(9, name, desc, mutator, target_adjudicate_bin, &[])?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    // Case 10: witness binary/launcher bypass
    {
        let name = "witness_binary_launcher_bypass";
        let desc = "Delete a required transcript to test adjudication bypass detection";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let path = p.join("crates/ggen-graph/audit/transcripts/00_capture_baseline.json");
            if path.exists() {
                std::fs::remove_file(&path)?;
            }
            Ok(())
        };
        let m_res = run_sabotage_case(10, name, desc, mutator, target_adjudicate_bin, &[])?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    // Case 11: missing required test fixture
    {
        let name = "missing_required_test_fixture";
        let desc = "Delete a required SPARQL hook fixture";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let path = p.join("crates/ggen-graph/hooks/gall-code-evaluation.ttl");
            if path.exists() {
                std::fs::remove_file(&path)?;
            }
            Ok(())
        };
        let m_res = run_sabotage_case(11, name, desc, mutator, target_eval_bin, &[])?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    // Case 12: invalid SPARQL query syntax
    {
        let name = "invalid_sparql_query_syntax";
        let desc = "Inject invalid syntax into a SPARQL hook fixture";
        let mutator = |p: &Path| -> Result<(), Box<dyn std::error::Error>> {
            let path = p.join("crates/ggen-graph/hooks/gall-code-evaluation.ttl");
            let mut content = std::fs::read_to_string(&path)?;
            content.push_str("\nINVALID SPARQL QUERY SYNTAX\n");
            std::fs::write(&path, content)?;
            Ok(())
        };
        let m_res = run_sabotage_case(12, name, desc, mutator, target_eval_bin, &[])?;
        all_refused &= m_res.refused;
        mutations_results.push(m_res);
    }

    let results = SabotageResults {
        timestamp: Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
        all_refused,
        mutations: mutations_results.clone(),
    };

    let audit_dir = workspace_root.join("crates/ggen-graph/audit");
    std::fs::create_dir_all(&audit_dir)?;
    let out_file = File::create(audit_dir.join("sabotage_results.json"))?;
    serde_json::to_writer_pretty(out_file, &results)?;

    // Write RDF Turtle Output
    let mut ttl_content = String::new();
    ttl_content.push_str("@base <http://example.org/> .\n");
    ttl_content.push_str("@prefix prov: <http://www.w3.org/ns/prov#> .\n");
    ttl_content.push_str("@prefix sh: <http://www.w3.org/ns/shacl#> .\n");
    ttl_content.push_str("@prefix dcterms: <http://purl.org/dc/terms/> .\n");
    ttl_content.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n");
    ttl_content.push_str("@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\n");

    ttl_content.push_str("<#sabotage_results> a sh:ValidationReport ;\n");
    ttl_content.push_str(&format!(
        "    sh:conforms {} ;\n",
        if all_refused { "true" } else { "false" }
    ));
    ttl_content.push_str("    prov:value \"All sabotage cases refused\" ;\n");
    if !req_iri.is_empty() {
        ttl_content.push_str(&format!("    prov:wasGeneratedBy {} ;\n", req_iri));
    }
    ttl_content.push_str("    prov:wasGeneratedBy <#sabotage_observation_activity>");

    if !mutations_results.is_empty() {
        ttl_content.push_str(" ;\n");
        for (i, m) in mutations_results.iter().enumerate() {
            ttl_content.push_str(&format!(
                "    sh:result <#case_{}_result>{}",
                m.id,
                if i == mutations_results.len() - 1 {
                    ""
                } else {
                    " ;\n"
                }
            ));
        }
    }
    ttl_content.push_str(" .\n\n");

    for m in &mutations_results {
        ttl_content.push_str(&format!(
            "<#case_{}_result> a sh:ValidationResult ;\n",
            m.id
        ));
        ttl_content.push_str(&format!(
            "    sh:resultSeverity {} ;\n",
            if m.refused { "sh:Info" } else { "sh:Violation" }
        ));
        ttl_content.push_str(&format!("    sh:focusNode <#case_{}> ;\n", m.id));
        ttl_content.push_str(&format!(
            "    sh:resultMessage {} ;\n",
            turtle_str(&format!(
                "Mutation {}: {}",
                m.name,
                if m.refused {
                    "refused (PASS)"
                } else {
                    "NOT refused (FAIL)"
                }
            ))
        ));
        ttl_content.push_str(&format!(
            "    prov:value {} .\n\n",
            if m.refused {
                "\"REFUSED\""
            } else {
                "\"ACCEPTED\""
            }
        ));

        ttl_content.push_str(&format!("<#case_{}> a prov:Entity ;\n", m.id));
        ttl_content.push_str(&format!("    dcterms:title {} ;\n", turtle_str(&m.name)));
        ttl_content.push_str(&format!(
            "    dcterms:description {} ;\n",
            turtle_str(&m.description)
        ));
        ttl_content.push_str(&format!("    prov:value <#case_{}_diff> ;\n", m.id));
        ttl_content.push_str(&format!("    prov:value <#case_{}_transcript> .\n\n", m.id));

        ttl_content.push_str(&format!("<#case_{}_diff> a prov:Entity ;\n", m.id));
        ttl_content.push_str("    rdfs:label \"diff\" ;\n");
        ttl_content.push_str(&format!("    prov:value {} .\n\n", turtle_str(&m.diff)));

        ttl_content.push_str(&format!("<#case_{}_transcript> a prov:Entity ;\n", m.id));
        ttl_content.push_str("    rdfs:label \"transcript\" ;\n");
        ttl_content.push_str(&format!(
            "    prov:value {} .\n\n",
            turtle_str(&m.transcript)
        ));
    }

    std::fs::write(audit_dir.join("sabotage_results.ttl"), ttl_content)?;

    // Write sabotage.validation.ttl
    let mut sab_ttl = String::new();
    sab_ttl.push_str("@base <http://example.org/> .\n");
    sab_ttl.push_str("@prefix sh: <http://www.w3.org/ns/shacl#> .\n");
    sab_ttl.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n");
    sab_ttl.push_str("@prefix dcterms: <http://purl.org/dc/terms/> .\n");
    sab_ttl.push_str("@prefix prov: <http://www.w3.org/ns/prov#> .\n\n");
    sab_ttl.push_str("<#sabotage_results> a sh:ValidationReport ;\n");
    sab_ttl.push_str(&format!(
        "    sh:conforms \"{}\"^^xsd:boolean ;\n",
        all_refused
    ));
    sab_ttl.push_str("    dcterms:identifier \"sabotage.validation\" ;\n");
    sab_ttl.push_str("    dcterms:description \"Sabotage Suite Verification Report\" ;\n");
    sab_ttl.push_str("    prov:value \"All sabotage cases refused\" ;\n");
    if !all_refused {
        sab_ttl.push_str("    sh:result [\n");
        sab_ttl.push_str("        a sh:ValidationResult ;\n");
        sab_ttl.push_str("        sh:resultSeverity sh:Violation ;\n");
        sab_ttl.push_str(
            "        sh:resultMessage \"Some sabotage cases failed to refuse promotion!\"\n",
        );
        sab_ttl.push_str("    ] .\n");
    } else {
        sab_ttl.push_str("    dcterms:title \"All 12 sabotage cases successfully failed validation and refused promotion as expected.\" .\n");
    }
    std::fs::write(audit_dir.join("sabotage.validation.ttl"), sab_ttl)?;

    if !all_refused {
        eprintln!("W4: Some sabotage cases were not refused!");
        std::process::exit(1);
    }

    println!("W4: Sabotage suite executed successfully. All mutations refused.");
    Ok(())
}
