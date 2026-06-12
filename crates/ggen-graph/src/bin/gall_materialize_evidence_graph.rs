#![allow(clippy::unwrap_used, clippy::unnecessary_debug_formatting)]
use chrono::Utc;
use ggen_graph::dialect::{check_datalog, check_n3, check_shacl, check_shex, check_sparql};
use ggen_graph::graph::parse::parse_turtle;
use ggen_graph::graph::serialize::serialize_to_string;
use ggen_graph::ocel::{OcelEvent, OcelLog, OcelObject, OcelObjectRef};
use oxigraph::io::RdfFormat;
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::Path;

fn verify_dialects(workspace_root: &Path) -> Result<String, Box<dyn std::error::Error>> {
    let dialects_dir = workspace_root.join("crates/ggen-graph/tests/fixtures/dialects");

    // 1. Verify SPARQL (check_sparql builds its own internal store)
    let ask_pass = std::fs::read_to_string(dialects_dir.join("sparql/ask_pass.rq"))?;
    let r = check_sparql(&ask_pass)?;
    if !r.conforms {
        return Err("SPARQL ask_pass did not conform".into());
    }

    let ask_fail = std::fs::read_to_string(dialects_dir.join("sparql/ask_fail.rq"))?;
    let r = check_sparql(&ask_fail)?;
    if r.conforms {
        return Err("SPARQL ask_fail conformed".into());
    }

    let select_pass = std::fs::read_to_string(dialects_dir.join("sparql/select_pass.rq"))?;
    let r = check_sparql(&select_pass)?;
    if !r.conforms {
        return Err("SPARQL select_pass did not conform".into());
    }

    let construct_pass = std::fs::read_to_string(dialects_dir.join("sparql/construct_pass.rq"))?;
    let r = check_sparql(&construct_pass)?;
    if !r.conforms {
        return Err("SPARQL construct_pass did not conform".into());
    }

    let malformed_sparql = std::fs::read_to_string(dialects_dir.join("sparql/malformed.rq"))?;
    if check_sparql(&malformed_sparql).is_ok() {
        return Err("SPARQL malformed parsed successfully".into());
    }

    let sabotage_sparql = std::fs::read_to_string(dialects_dir.join("sparql/sabotage.rq"))?;
    if check_sparql(&sabotage_sparql).is_ok() {
        return Err("SPARQL sabotage parsed successfully".into());
    }

    // 2. Verify SHACL (check_shacl builds its own internal store)
    let conforms_shacl = std::fs::read_to_string(dialects_dir.join("shacl/conforms.ttl"))?;
    let r = check_shacl(&conforms_shacl)?;
    if !r.conforms {
        return Err("SHACL conforms.ttl did not conform".into());
    }

    let violates_shacl = std::fs::read_to_string(dialects_dir.join("shacl/violates.ttl"))?;
    let r = check_shacl(&violates_shacl)?;
    if r.conforms {
        return Err("SHACL violates.ttl conformed".into());
    }

    let malformed_shacl = std::fs::read_to_string(dialects_dir.join("shacl/malformed.ttl"))?;
    if check_shacl(&malformed_shacl).is_ok() {
        return Err("SHACL malformed.ttl parsed successfully".into());
    }

    let sabotage_shacl = std::fs::read_to_string(dialects_dir.join("shacl/sabotage.ttl"))?;
    if check_shacl(&sabotage_shacl).is_ok() {
        return Err("SHACL sabotage.ttl parsed successfully".into());
    }

    // 3. Verify N3
    let pass_n3 = std::fs::read_to_string(dialects_dir.join("n3/rule_pass.n3"))?;
    let r = check_n3(&pass_n3)?;
    if r.supported {
        return Err("N3 rule_pass marked as supported".into());
    }

    let malformed_n3 = std::fs::read_to_string(dialects_dir.join("n3/malformed.n3"))?;
    if check_n3(&malformed_n3).is_ok() {
        return Err("N3 malformed parsed successfully".into());
    }

    let sabotage_n3 = std::fs::read_to_string(dialects_dir.join("n3/sabotage.n3"))?;
    if check_n3(&sabotage_n3).is_ok() {
        return Err("N3 sabotage parsed successfully".into());
    }

    // 4. Verify Datalog
    let pass_dl = std::fs::read_to_string(dialects_dir.join("datalog/rule_pass.dl"))?;
    let r = check_datalog(&pass_dl)?;
    if r.supported {
        return Err("Datalog rule_pass marked as supported".into());
    }

    let malformed_dl = std::fs::read_to_string(dialects_dir.join("datalog/malformed.dl"))?;
    if check_datalog(&malformed_dl).is_ok() {
        return Err("Datalog malformed parsed successfully".into());
    }

    let sabotage_dl = std::fs::read_to_string(dialects_dir.join("datalog/sabotage.dl"))?;
    if check_datalog(&sabotage_dl).is_ok() {
        return Err("Datalog sabotage parsed successfully".into());
    }

    // 5. Verify ShEx
    let conforms_shex = std::fs::read_to_string(dialects_dir.join("shex/conforms.shex"))?;
    let r = check_shex(&conforms_shex)?;
    if r.supported {
        return Err("ShEx conforms marked as supported".into());
    }

    let malformed_shex = std::fs::read_to_string(dialects_dir.join("shex/malformed.shex"))?;
    if check_shex(&malformed_shex).is_ok() {
        return Err("ShEx malformed parsed successfully".into());
    }

    let sabotage_shex = std::fs::read_to_string(dialects_dir.join("shex/sabotage.shex"))?;
    if check_shex(&sabotage_shex).is_ok() {
        return Err("ShEx sabotage parsed successfully".into());
    }

    // Generate dialect reports RDF facts
    let ttl_dialect = "
        @base <http://example.org/> .
        @prefix prov: <http://www.w3.org/ns/prov#> .
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix dcterms: <http://purl.org/dc/terms/> .

        <#report_sparql> a sh:ValidationReport ;
            dcterms:identifier \"W-DIALECT-SPARQL\" ;
            sh:conforms true ;
            dcterms:description \"SPARQL dialect verified successfully\" .

        <#report_shacl> a sh:ValidationReport ;
            dcterms:identifier \"W-DIALECT-SHACL\" ;
            sh:conforms true ;
            dcterms:description \"SHACL dialect verified successfully\" .

        <#report_n3> a sh:ValidationReport ;
            dcterms:identifier \"W-DIALECT-N3\" ;
            sh:conforms true ;
            dcterms:description \"N3 dialect verified as bounded unsupported capability\" .

        <#report_datalog> a sh:ValidationReport ;
            dcterms:identifier \"W-DIALECT-DATALOG\" ;
            sh:conforms true ;
            dcterms:description \"Datalog dialect verified as bounded unsupported capability\" .

        <#report_shex> a sh:ValidationReport ;
            dcterms:identifier \"W-DIALECT-SHEX\" ;
            sh:conforms true ;
            dcterms:description \"ShEx dialect verified as bounded unsupported capability\" .

        <#capability_sparql> a prov:Entity ;
            dcterms:identifier \"SPARQL\" ;
            sh:conforms true ;
            dcterms:description \"SPARQL is fully supported\" .

        <#capability_shacl> a prov:Entity ;
            dcterms:identifier \"SHACL\" ;
            sh:conforms true ;
            dcterms:description \"SHACL is fully supported\" .

        <#capability_n3> a prov:Entity ;
            dcterms:identifier \"N3\" ;
            sh:conforms false ;
            dcterms:description \"Notation3 is unsupported in this engine\" .

        <#capability_datalog> a prov:Entity ;
            dcterms:identifier \"Datalog\" ;
            sh:conforms false ;
            dcterms:description \"Datalog is unsupported in this engine\" .

        <#capability_shex> a prov:Entity ;
            dcterms:identifier \"ShEx\" ;
            sh:conforms false ;
            dcterms:description \"Shape Expressions is unsupported in this engine\" .
    ";

    Ok(ttl_dialect.to_string())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let workspace_root = std::env::current_dir()?;
    let audit_dir = workspace_root.join("crates/ggen-graph/audit");
    let transcripts_dir = audit_dir.join("transcripts");

    println!("W6: Aggregating individual observer outputs into cohesive evidence graph...");

    let mut all_quads = Vec::new();

    // 1. Gather individual Turtle files
    let mut ttl_files = vec![
        audit_dir.join("worktree_inventory.ttl"),
        audit_dir.join("clean_room_rebuild.ttl"),
        audit_dir.join("doctest_results.ttl"),
        workspace_root.join("docs/docs.tree.ttl"),
    ];

    // Read scripts/transcripts TTL files
    if transcripts_dir.exists() {
        for entry in std::fs::read_dir(&transcripts_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension().is_some_and(|ext| ext == "ttl") {
                ttl_files.push(path);
            }
        }
    }

    for ttl_path in ttl_files {
        if ttl_path.exists() {
            let mut file = File::open(&ttl_path)?;
            let mut content = String::new();
            file.read_to_string(&mut content)?;

            match parse_turtle(&content) {
                Ok(quads) => {
                    all_quads.extend(quads);
                }
                Err(e) => {
                    eprintln!(
                        "Warning: Failed to parse Turtle file {}: {}",
                        ttl_path.display(),
                        e
                    );
                }
            }
        }
    }

    // 2. Add sabotage results as Turtle facts if available
    let sabotage_json_path = audit_dir.join("sabotage_results.json");
    if sabotage_json_path.exists() {
        let mut file = File::open(&sabotage_json_path)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;
        if let Ok(json_val) = serde_json::from_str::<serde_json::Value>(&content) {
            let all_refused = json_val["all_refused"].as_bool().unwrap_or(false);
            let mut sabotage_ttl = String::new();
            sabotage_ttl.push_str("@base <http://example.org/> .\n");
            sabotage_ttl.push_str("@prefix prov: <http://www.w3.org/ns/prov#> .\n");
            sabotage_ttl.push_str("@prefix sh: <http://www.w3.org/ns/shacl#> .\n");
            sabotage_ttl.push_str("@prefix dcterms: <http://purl.org/dc/terms/> .\n");
            sabotage_ttl.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n\n");
            sabotage_ttl.push_str(&format!(
                "<#sabotage_results> a prov:Entity ;\n    \
                 dcterms:identifier \"sabotage_results\" ;\n    \
                 dcterms:type \"SabotageVerification\" ;\n    \
                 sh:conforms {} ;\n    \
                 dcterms:description {:?} .\n",
                if all_refused { "true" } else { "false" },
                if all_refused {
                    "All sabotage cases refused"
                } else {
                    "Sabotage verification failed"
                }
            ));
            if let Ok(quads) = parse_turtle(&sabotage_ttl) {
                all_quads.extend(quads);
            }
        }
    }

    // 3. Run dialect verification and add dialect reports/capabilities
    let dialect_ttl = verify_dialects(&workspace_root)?;
    if let Ok(quads) = parse_turtle(&dialect_ttl) {
        all_quads.extend(quads);
    }

    // Write dialect_completeness.validation.ttl
    let mut dc_ttl = String::new();
    dc_ttl.push_str("@base <http://example.org/> .\n");
    dc_ttl.push_str("@prefix sh: <http://www.w3.org/ns/shacl#> .\n");
    dc_ttl.push_str("@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .\n");
    dc_ttl.push_str("@prefix dcterms: <http://purl.org/dc/terms/> .\n\n");
    dc_ttl.push_str("<#dialect_completeness_report> a sh:ValidationReport ;\n");
    dc_ttl.push_str("    sh:conforms \"true\"^^xsd:boolean ;\n");
    dc_ttl.push_str("    dcterms:identifier \"dialect_completeness.validation\" ;\n");
    dc_ttl.push_str("    dcterms:description \"Dialect Completeness Validation Report\" ;\n");
    dc_ttl.push_str("    sh:result <#result_sparql_ask> , <#result_sparql_select> , <#result_sparql_construct> , <#result_shacl> , <#result_n3> , <#result_datalog> , <#result_shex> .\n\n");

    dc_ttl.push_str("<#result_sparql_ask> a sh:ValidationResult ;\n");
    dc_ttl.push_str("    sh:focusNode <#dialect_sparql_ask> ;\n");
    dc_ttl.push_str("    sh:resultSeverity sh:Info ;\n");
    dc_ttl.push_str("    sh:conforms \"true\"^^xsd:boolean ;\n");
    dc_ttl.push_str(
        "    dcterms:description \"SPARQL ASK dialect is fully supported and verified.\" .\n\n",
    );

    dc_ttl.push_str("<#result_sparql_select> a sh:ValidationResult ;\n");
    dc_ttl.push_str("    sh:focusNode <#dialect_sparql_select> ;\n");
    dc_ttl.push_str("    sh:resultSeverity sh:Info ;\n");
    dc_ttl.push_str("    sh:conforms \"true\"^^xsd:boolean ;\n");
    dc_ttl.push_str(
        "    dcterms:description \"SPARQL SELECT dialect is fully supported and verified.\" .\n\n",
    );

    dc_ttl.push_str("<#result_sparql_construct> a sh:ValidationResult ;\n");
    dc_ttl.push_str("    sh:focusNode <#dialect_sparql_construct> ;\n");
    dc_ttl.push_str("    sh:resultSeverity sh:Info ;\n");
    dc_ttl.push_str("    sh:conforms \"true\"^^xsd:boolean ;\n");
    dc_ttl.push_str("    dcterms:description \"SPARQL CONSTRUCT dialect is fully supported and verified.\" .\n\n");

    dc_ttl.push_str("<#result_shacl> a sh:ValidationResult ;\n");
    dc_ttl.push_str("    sh:focusNode <#dialect_shacl> ;\n");
    dc_ttl.push_str("    sh:resultSeverity sh:Info ;\n");
    dc_ttl.push_str("    sh:conforms \"true\"^^xsd:boolean ;\n");
    dc_ttl.push_str(
        "    dcterms:description \"SHACL dialect is fully supported and verified.\" .\n\n",
    );

    dc_ttl.push_str("<#result_n3> a sh:ValidationResult ;\n");
    dc_ttl.push_str("    sh:focusNode <#dialect_n3> ;\n");
    dc_ttl.push_str("    sh:resultSeverity sh:Warning ;\n");
    dc_ttl.push_str("    sh:conforms \"false\"^^xsd:boolean ;\n");
    dc_ttl.push_str(
        "    dcterms:description \"N3 dialect is a bounded unsupported capability.\" .\n\n",
    );

    dc_ttl.push_str("<#result_datalog> a sh:ValidationResult ;\n");
    dc_ttl.push_str("    sh:focusNode <#dialect_datalog> ;\n");
    dc_ttl.push_str("    sh:resultSeverity sh:Warning ;\n");
    dc_ttl.push_str("    sh:conforms \"false\"^^xsd:boolean ;\n");
    dc_ttl.push_str(
        "    dcterms:description \"Datalog dialect is a bounded unsupported capability.\" .\n\n",
    );

    dc_ttl.push_str("<#result_shex> a sh:ValidationResult ;\n");
    dc_ttl.push_str("    sh:focusNode <#dialect_shex> ;\n");
    dc_ttl.push_str("    sh:resultSeverity sh:Warning ;\n");
    dc_ttl.push_str("    sh:conforms \"false\"^^xsd:boolean ;\n");
    dc_ttl.push_str(
        "    dcterms:description \"ShEx dialect is a bounded unsupported capability.\" .\n",
    );

    std::fs::write(
        audit_dir.join("dialect_completeness.validation.ttl"),
        dc_ttl,
    )?;

    // 4. Serialize cohesive evidence graph
    let serialized_turtle = serialize_to_string(&all_quads, RdfFormat::Turtle)?;
    std::fs::write(audit_dir.join("gall_evidence.ttl"), serialized_turtle)?;
    println!("W6: gall_evidence.ttl written successfully.");

    // 5. Construct cohesive OCEL log
    let mut ocel_log = OcelLog::new();

    // Add general objects
    let mut worktree_attr = HashMap::new();
    if let Ok(mut f) = File::open(audit_dir.join("worktree_inventory.full.json")) {
        let mut c = String::new();
        f.read_to_string(&mut c)?;
        if let Ok(v) = serde_json::from_str::<serde_json::Value>(&c) {
            worktree_attr.insert(
                "timestamp".to_string(),
                v["timestamp"].as_str().unwrap_or("").to_string(),
            );
            if let Some(arr) = v["files"].as_array() {
                worktree_attr.insert("total_files".to_string(), arr.len().to_string());
            }
        }
    }
    ocel_log.objects.push(OcelObject {
        id: "obj_worktree".to_string(),
        r#type: "WorktreeInventory".to_string(),
        attributes: worktree_attr,
    });

    let mut clean_room_attr = HashMap::new();
    if let Ok(mut f) = File::open(audit_dir.join("clean_room_rebuild.json")) {
        let mut c = String::new();
        f.read_to_string(&mut c)?;
        if let Ok(v) = serde_json::from_str::<serde_json::Value>(&c) {
            clean_room_attr.insert(
                "status".to_string(),
                v["rebuild_status"].as_str().unwrap_or("").to_string(),
            );
            clean_room_attr.insert(
                "duration_seconds".to_string(),
                v["duration_seconds"].as_u64().unwrap_or(0).to_string(),
            );
        }
    }
    ocel_log.objects.push(OcelObject {
        id: "obj_clean_room".to_string(),
        r#type: "CleanRoomRebuild".to_string(),
        attributes: clean_room_attr,
    });

    let mut doctest_attr = HashMap::new();
    if let Ok(mut f) = File::open(audit_dir.join("doctest_results.json")) {
        let mut c = String::new();
        f.read_to_string(&mut c)?;
        if let Ok(v) = serde_json::from_str::<serde_json::Value>(&c) {
            doctest_attr.insert(
                "status".to_string(),
                v["status"].as_str().unwrap_or("").to_string(),
            );
            doctest_attr.insert(
                "passed_count".to_string(),
                v["passed_count"].as_u64().unwrap_or(0).to_string(),
            );
        }
    }
    ocel_log.objects.push(OcelObject {
        id: "obj_doctests".to_string(),
        r#type: "DoctestVerification".to_string(),
        attributes: doctest_attr,
    });

    let mut sabotage_attr = HashMap::new();
    if let Ok(mut f) = File::open(audit_dir.join("sabotage_results.json")) {
        let mut c = String::new();
        f.read_to_string(&mut c)?;
        if let Ok(v) = serde_json::from_str::<serde_json::Value>(&c) {
            sabotage_attr.insert(
                "all_refused".to_string(),
                v["all_refused"].as_bool().unwrap_or(false).to_string(),
            );
        }
    }
    ocel_log.objects.push(OcelObject {
        id: "obj_sabotage".to_string(),
        r#type: "SabotageVerification".to_string(),
        attributes: sabotage_attr,
    });

    // Events
    let now = Utc::now();
    ocel_log.events.push(OcelEvent {
        id: "evt_observe_worktree".to_string(),
        activity: "ObserveWorktree".to_string(),
        timestamp: now,
        objects: vec![OcelObjectRef {
            id: "obj_worktree".to_string(),
            r#type: "WorktreeInventory".to_string(),
            qualifier: None,
        }],
        attributes: HashMap::new(),
    });

    ocel_log.events.push(OcelEvent {
        id: "evt_observe_clean_room".to_string(),
        activity: "ObserveCleanRoom".to_string(),
        timestamp: now,
        objects: vec![OcelObjectRef {
            id: "obj_clean_room".to_string(),
            r#type: "CleanRoomRebuild".to_string(),
            qualifier: None,
        }],
        attributes: HashMap::new(),
    });

    ocel_log.events.push(OcelEvent {
        id: "evt_observe_doctests".to_string(),
        activity: "ObserveDoctests".to_string(),
        timestamp: now,
        objects: vec![OcelObjectRef {
            id: "obj_doctests".to_string(),
            r#type: "DoctestVerification".to_string(),
            qualifier: None,
        }],
        attributes: HashMap::new(),
    });

    ocel_log.events.push(OcelEvent {
        id: "evt_observe_sabotage".to_string(),
        activity: "ObserveSabotage".to_string(),
        timestamp: now,
        objects: vec![OcelObjectRef {
            id: "obj_sabotage".to_string(),
            r#type: "SabotageVerification".to_string(),
            qualifier: None,
        }],
        attributes: HashMap::new(),
    });

    let ocel_file = File::create(audit_dir.join("gall_evidence.ocel.json"))?;
    serde_json::to_writer_pretty(ocel_file, &ocel_log)?;
    println!("W6: gall_evidence.ocel.json written successfully.");

    Ok(())
}
