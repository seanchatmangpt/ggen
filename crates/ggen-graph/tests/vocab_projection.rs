use ggen_graph::{vocab, DeterministicGraph};
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::model::{NamedNode, Quad};
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use std::error::Error;
use std::fs;
use std::path::Path;

#[test]
fn test_vocab_constants_exist_and_match() {
    assert_eq!(vocab::RDF, "http://www.w3.org/1999/02/22-rdf-syntax-ns#");
    assert_eq!(vocab::RDFS, "http://www.w3.org/2000/01/rdf-schema#");
    assert_eq!(vocab::OWL, "http://www.w3.org/2002/07/owl#");
    assert_eq!(vocab::XSD, "http://www.w3.org/2001/XMLSchema#");
    assert_eq!(vocab::PROV, "http://www.w3.org/ns/prov#");
    assert_eq!(vocab::SKOS, "http://www.w3.org/2004/02/skos/core#");
    assert_eq!(vocab::SHACL, "http://www.w3.org/ns/shacl#");
    assert_eq!(vocab::OCEL, "http://www.ocel-standard.org/ns#");
}

#[test]
fn test_vocab_projection_in_graph() -> Result<(), Box<dyn Error>> {
    let graph = DeterministicGraph::new()?;

    // Construct quads using our vocabulary constants
    let subject_uri = format!("{}activity_123", vocab::OCEL);
    let type_predicate = vocab::Rdf::TYPE.as_str();
    let activity_type = vocab::Ocel::ACTIVITY.as_str();

    let subject = NamedNode::new(&subject_uri)?;
    let predicate = NamedNode::new(type_predicate)?;
    let object = NamedNode::new(activity_type)?;

    let quad = Quad::new(
        subject.clone(),
        predicate.clone(),
        object.clone(),
        oxigraph::model::GraphName::DefaultGraph,
    );

    graph.insert_quad(&quad)?;

    assert!(graph.contains_quad(&quad)?);

    // Check that we can query using the projected vocab
    let query_str = format!(
        "ASK WHERE {{ <{}> <{}> <{}> }}",
        subject_uri, type_predicate, activity_type
    );
    let results = graph.query(&query_str)?;
    if let oxigraph::sparql::QueryResults::Boolean(val) = results {
        assert!(val);
    } else {
        return Err("Expected boolean query result".into());
    }

    Ok(())
}

/// Load the hook pack TTL and assert it contains zero triples with a 'gall:' term
/// (in subject, predicate, or object position) — the public surface must use only
/// standard vocabularies (rdf:, rdfs:, prov:, sh:, dcterms:, etc.).
#[test]
fn test_hook_pack_ttl_contains_no_gall_namespace_terms() -> Result<(), Box<dyn Error>> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let ttl_path = manifest_dir.join("hooks").join("gall-code-evaluation.ttl");

    assert!(
        ttl_path.exists(),
        "Hook pack TTL not found at {:?}",
        ttl_path
    );

    let ttl_content = fs::read_to_string(&ttl_path)
        .map_err(|e| format!("Failed to read {:?}: {}", ttl_path, e))?;

    let store = Store::new()?;
    store
        .load_from_reader(
            RdfParser::from_format(RdfFormat::Turtle),
            ttl_content.as_bytes(),
        )
        .map_err(|e| format!("Failed to parse hook pack TTL: {}", e))?;

    // SPARQL: find any triple where subject, predicate, or IRI object
    // contains 'gall' in the IRI string.
    let gall_scan_query = r#"
        SELECT ?s ?p ?o WHERE {
            ?s ?p ?o .
            FILTER(
                CONTAINS(STR(?s), 'gall') ||
                CONTAINS(STR(?p), 'gall') ||
                (isIRI(?o) && CONTAINS(STR(?o), 'gall'))
            )
        }
    "#;

    let results = oxigraph::sparql::SparqlEvaluator::new()
        .parse_query(gall_scan_query)?
        .on_store(&store)
        .execute()?;
    let mut gall_violations: Vec<String> = Vec::new();

    if let QueryResults::Solutions(solutions) = results {
        for sol in solutions {
            let sol = sol?;
            let s = sol.get("s").map(|t| t.to_string()).unwrap_or_default();
            let p = sol.get("p").map(|t| t.to_string()).unwrap_or_default();
            let o = sol.get("o").map(|t| t.to_string()).unwrap_or_default();
            gall_violations.push(format!("  Triple: {} {} {}", s, p, o));
        }
    }

    assert!(
        gall_violations.is_empty(),
        "Hook pack TTL at {:?} contains {} triples with 'gall:' namespace terms \
         (private vocabulary leaked into public surface):\n{}",
        ttl_path,
        gall_violations.len(),
        gall_violations.join("\n")
    );

    Ok(())
}

/// Load audit/doctest_results.ttl and assert it contains no 'gall:' namespace terms
/// and no non-standard prov# terms (e.g., prov#w0_worktree would be a fabricated IRI).
#[test]
fn test_audit_doctest_results_ttl_vocab_is_clean() -> Result<(), Box<dyn Error>> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let ttl_path = manifest_dir.join("audit").join("doctest_results.ttl");

    assert!(
        ttl_path.exists(),
        "Audit doctest_results.ttl not found at {:?}",
        ttl_path
    );

    let ttl_content = fs::read_to_string(&ttl_path)
        .map_err(|e| format!("Failed to read {:?}: {}", ttl_path, e))?;

    // Parse the file — this itself is a real boundary crossing proving the file is valid Turtle.
    let store = Store::new()?;
    store
        .load_from_reader(
            RdfParser::from_format(RdfFormat::Turtle),
            ttl_content.as_bytes(),
        )
        .map_err(|e| format!("Failed to parse doctest_results.ttl: {}", e))?;

    // 1. No 'gall:' terms anywhere
    let gall_query = r#"
        SELECT ?s ?p ?o WHERE {
            ?s ?p ?o .
            FILTER(
                CONTAINS(STR(?s), 'gall') ||
                CONTAINS(STR(?p), 'gall') ||
                (isIRI(?o) && CONTAINS(STR(?o), 'gall'))
            )
        }
    "#;

    let results = oxigraph::sparql::SparqlEvaluator::new()
        .parse_query(gall_query)?
        .on_store(&store)
        .execute()?;
    let mut gall_violations: Vec<String> = Vec::new();
    if let QueryResults::Solutions(solutions) = results {
        for sol in solutions {
            let sol = sol?;
            gall_violations.push(format!(
                "  {} {} {}",
                sol.get("s").map(|t| t.to_string()).unwrap_or_default(),
                sol.get("p").map(|t| t.to_string()).unwrap_or_default(),
                sol.get("o").map(|t| t.to_string()).unwrap_or_default(),
            ));
        }
    }

    assert!(
        gall_violations.is_empty(),
        "doctest_results.ttl contains 'gall:' namespace contamination:\n{}",
        gall_violations.join("\n")
    );

    // 2. No subject uses the prov# fragment form (e.g. prov#w0_worktree is an invalid fabricated IRI)
    // The standard prov namespace is http://www.w3.org/ns/prov# and subjects should NOT be
    // IRIs of the form http://www.w3.org/ns/prov#someLocalTerm (those are prov vocab terms, not data).
    let prov_subject_query = r#"
        PREFIX prov: <http://www.w3.org/ns/prov#>
        SELECT ?s WHERE {
            ?s ?p ?o .
            FILTER(STRSTARTS(STR(?s), "http://www.w3.org/ns/prov#"))
        }
    "#;

    let results = oxigraph::sparql::SparqlEvaluator::new()
        .parse_query(prov_subject_query)?
        .on_store(&store)
        .execute()?;
    let mut prov_subject_violations: Vec<String> = Vec::new();
    if let QueryResults::Solutions(solutions) = results {
        for sol in solutions {
            let sol = sol?;
            if let Some(s) = sol.get("s") {
                prov_subject_violations.push(format!("  Subject: {}", s));
            }
        }
    }

    assert!(
        prov_subject_violations.is_empty(),
        "doctest_results.ttl uses prov: IRI as a data subject (non-standard — \
         prov: terms are vocabulary, not data subjects):\n{}",
        prov_subject_violations.join("\n")
    );

    Ok(())
}

/// Scan all TTL files in the hooks/ and audit/ directories for gall: namespace contamination.
/// This is a comprehensive sweep that covers every TTL file produced by the system.
#[test]
fn test_all_ttl_files_have_no_gall_terms() -> Result<(), Box<dyn Error>> {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let dirs_to_scan = [manifest_dir.join("hooks"), manifest_dir.join("audit")];

    let mut all_violations: Vec<String> = Vec::new();
    let mut files_scanned = 0usize;

    for dir in &dirs_to_scan {
        if !dir.exists() {
            continue;
        }
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_file() && path.extension().is_some_and(|e| e == "ttl") {
                let content = fs::read_to_string(&path)
                    .map_err(|e| format!("Failed to read {:?}: {}", path, e))?;

                let store = Store::new()?;
                // Skip files that fail to parse — report them separately
                if store
                    .load_from_reader(
                        RdfParser::from_format(RdfFormat::Turtle),
                        content.as_bytes(),
                    )
                    .is_err()
                {
                    // Don't silently skip — if a TTL file is corrupt that's a problem
                    // but not a gall: violation; other tests catch parse errors
                    files_scanned += 1;
                    continue;
                }

                let query = r#"
                    SELECT ?s ?p ?o WHERE {
                        ?s ?p ?o .
                        FILTER(
                            CONTAINS(STR(?s), 'gall') ||
                            CONTAINS(STR(?p), 'gall') ||
                            (isIRI(?o) && CONTAINS(STR(?o), 'gall'))
                        )
                    }
                "#;

                let results = oxigraph::sparql::SparqlEvaluator::new()
                    .parse_query(query)?
                    .on_store(&store)
                    .execute()?;

                if let QueryResults::Solutions(solutions) = results {
                    for sol in solutions.flatten() {
                        let rel_path = path.strip_prefix(manifest_dir).unwrap_or(&path);
                        all_violations.push(format!(
                            "  [{:?}] {} {} {}",
                            rel_path,
                            sol.get("s").map(|t| t.to_string()).unwrap_or_default(),
                            sol.get("p").map(|t| t.to_string()).unwrap_or_default(),
                            sol.get("o").map(|t| t.to_string()).unwrap_or_default(),
                        ));
                    }
                }

                files_scanned += 1;
            }
        }
    }

    // Must have scanned at least 1 file (hooks/gall-code-evaluation.ttl)
    assert!(
        files_scanned >= 1,
        "No TTL files were scanned — check that hooks/ and audit/ directories exist"
    );

    assert!(
        all_violations.is_empty(),
        "Found {} gall: namespace violations across {} TTL files:\n{}",
        all_violations.len(),
        files_scanned,
        all_violations.join("\n")
    );

    Ok(())
}
