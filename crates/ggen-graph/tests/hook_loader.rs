#![allow(deprecated)]

use ggen_graph::{DeterministicGraph, KnowledgeHook};
use oxigraph::io::{RdfFormat, RdfParser};
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use std::error::Error;
use std::fs;
use std::path::Path;

fn hook_pack_path() -> std::path::PathBuf {
    let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    manifest_dir.join("hooks").join("gall-code-evaluation.ttl")
}

#[test]
fn test_hook_serialization_and_deserialization() -> Result<(), Box<dyn Error>> {
    let hook = KnowledgeHook::new(
        "non_empty_users".to_string(),
        "ASK WHERE { ?user <http://example.org/type> <http://example.org/User> }".to_string(),
    );

    let serialized = serde_json::to_string(&hook)?;
    let deserialized: KnowledgeHook = serde_json::from_str(&serialized)?;

    assert_eq!(deserialized.name, hook.name);
    assert_eq!(deserialized.sparql_query, hook.sparql_query);

    Ok(())
}

#[test]
fn test_load_hooks_from_json_array() -> Result<(), Box<dyn Error>> {
    let json_data = r#"[
        {
            "name": "has_name",
            "sparql_query": "ASK WHERE { ?s <http://example.org/name> ?name }"
        },
        {
            "name": "no_orphans",
            "sparql_query": "ASK WHERE { FILTER NOT EXISTS { ?s <http://example.org/parent> ?parent } }"
        }
    ]"#;

    let hooks: Vec<KnowledgeHook> = serde_json::from_str(json_data)?;
    assert_eq!(hooks.len(), 2);
    assert_eq!(hooks[0].name, "has_name");
    assert_eq!(hooks[1].name, "no_orphans");

    // Execute loaded hooks
    let graph = DeterministicGraph::new()?;

    // has_name should fail on empty graph
    assert!(!hooks[0].execute(&graph)?);

    // no_orphans ASK query: FILTER NOT EXISTS {...} returns true if there are no subjects having a parent,
    // which is trivially true on an empty graph.
    assert!(hooks[1].execute(&graph)?);

    Ok(())
}

/// Load the real TTL hook pack from disk into an Oxigraph store and verify
/// that all 14 expected hook identifiers are present (W0–W9 + 5 dialect hooks).
#[test]
fn test_load_ttl_hook_pack_has_14_hooks() -> Result<(), Box<dyn Error>> {
    let path = hook_pack_path();
    assert!(
        path.exists(),
        "Hook pack TTL not found at {:?}. Ensure hooks/gall-code-evaluation.ttl exists.",
        path
    );

    let ttl_content = fs::read_to_string(&path)
        .map_err(|e| format!("Failed to read hook pack at {:?}: {}", path, e))?;

    // Load the TTL into a fresh Oxigraph store
    let store = Store::new()?;
    store
        .load_from_reader(
            RdfParser::from_format(RdfFormat::Turtle),
            ttl_content.as_bytes(),
        )
        .map_err(|e| format!("Failed to parse hook pack TTL: {}", e))?;

    // Query for all dcterms:identifier values on prov:Plan subjects
    let query = r"
        PREFIX prov: <http://www.w3.org/ns/prov#>
        PREFIX dcterms: <http://purl.org/dc/terms/>
        SELECT ?id WHERE {
            ?hook a prov:Plan ;
                  dcterms:identifier ?id .
        }
        ORDER BY ?id
    ";

    let results = store.query(query)?;
    let mut found_ids: Vec<String> = Vec::new();

    if let QueryResults::Solutions(solutions) = results {
        for sol in solutions {
            let sol = sol?;
            if let Some(oxigraph::model::Term::Literal(lit)) = sol.get("id") {
                found_ids.push(lit.value().to_string());
            }
        }
    }

    // Expected: W0 through W9 (10 core hooks) + 5 dialect hooks
    let expected_ids: Vec<&str> = vec![
        "W-DIALECT-DATALOG",
        "W-DIALECT-N3",
        "W-DIALECT-SHACL",
        "W-DIALECT-SHEX",
        "W-DIALECT-SPARQL",
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
    ];

    assert_eq!(
        found_ids.len(),
        expected_ids.len(),
        "Expected exactly {} hooks in hook pack, found {}. IDs found: {:?}",
        expected_ids.len(),
        found_ids.len(),
        found_ids
    );

    // Sort found_ids for comparison
    found_ids.sort();

    for (expected, actual) in expected_ids.iter().zip(found_ids.iter()) {
        assert_eq!(
            expected, actual,
            "Hook identifier mismatch: expected '{}', got '{}'",
            expected, actual
        );
    }

    Ok(())
}

/// Verify that every hook in the TTL pack carries a non-empty SPARQL CONSTRUCT query
/// in prov:value, proving that all hooks are executable (not placeholder stubs).
#[test]
fn test_all_ttl_hooks_have_nonempty_sparql_query() -> Result<(), Box<dyn Error>> {
    let path = hook_pack_path();
    assert!(path.exists(), "Hook pack TTL not found at {:?}", path);

    let ttl_content = fs::read_to_string(&path)?;

    let store = Store::new()?;
    store
        .load_from_reader(
            RdfParser::from_format(RdfFormat::Turtle),
            ttl_content.as_bytes(),
        )
        .map_err(|e| format!("Failed to parse hook pack TTL: {}", e))?;

    let query = r"
        PREFIX prov: <http://www.w3.org/ns/prov#>
        PREFIX dcterms: <http://purl.org/dc/terms/>
        SELECT ?id ?query WHERE {
            ?hook a prov:Plan ;
                  dcterms:identifier ?id ;
                  prov:value ?query .
        }
    ";

    let results = store.query(query)?;
    let mut hook_count = 0;

    if let QueryResults::Solutions(solutions) = results {
        for sol in solutions {
            let sol = sol?;
            let id = match sol.get("id") {
                Some(oxigraph::model::Term::Literal(l)) => l.value().to_string(),
                _ => return Err("Hook missing dcterms:identifier".into()),
            };
            let query_body = match sol.get("query") {
                Some(oxigraph::model::Term::Literal(l)) => l.value().to_string(),
                _ => return Err(format!("Hook '{}' missing prov:value (SPARQL query)", id).into()),
            };
            assert!(
                !query_body.trim().is_empty(),
                "Hook '{}' has an empty prov:value SPARQL query — this is a forbidden stub",
                id
            );
            assert!(
                query_body.contains("CONSTRUCT")
                    || query_body.contains("SELECT")
                    || query_body.contains("ASK"),
                "Hook '{}' prov:value does not look like a SPARQL query: {:?}",
                id,
                &query_body[..query_body.len().min(80)]
            );
            hook_count += 1;
        }
    }

    assert_eq!(
        hook_count, 15,
        "Expected 15 hooks with SPARQL queries, found {}",
        hook_count
    );

    Ok(())
}
