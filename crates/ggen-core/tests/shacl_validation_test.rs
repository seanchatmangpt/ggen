//! SHACL shape file loading and structure validation tests.
//!
//! These tests verify that ggen's `.shacl.ttl` files are valid Turtle, contain
//! the expected SHACL constructs (NodeShape, targetClass, property constraints,
//! datatype restrictions), and that SPARQL queries can detect violations.
//!
//! Note: `ShapeLoader::load()` is currently stubbed (always returns empty).
//! These tests bypass the stub by loading `.shacl.ttl` files directly into a
//! `Graph` and querying with SPARQL.

use ggen_core::graph::{CachedResult, Graph};
use ggen_core::validation::shacl::ShapeLoader;
use walkdir::WalkDir;

/// Return the ggen workspace root (two levels up from crates/ggen-core).
fn workspace_root() -> std::path::PathBuf {
    let mut p = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.pop(); // crates/ggen-core -> crates
    p.pop(); // crates -> ggen root
    p
}

/// Helper: run a SELECT SPARQL query and return solution rows.
///
/// Panics if the query fails or does not return solutions.
fn select_solutions(graph: &Graph, sparql: &str) -> Vec<std::collections::BTreeMap<String, String>> {
    match graph.query_cached(sparql) {
        Ok(CachedResult::Solutions(rows)) => rows,
        Ok(other) => panic!("Expected Solutions, got: {:?}", std::mem::discriminant(&other)),
        Err(e) => panic!("SPARQL query failed: {}", e),
    }
}

// ---------------------------------------------------------------------------
// Test 1: api.shacl.ttl loads as valid Turtle
// ---------------------------------------------------------------------------

#[test]
fn test_api_shacl_ttl_loads_as_valid_turtle() {
    let path = workspace_root()
        .join("templates/api/endpoint/graphs/shapes/api.shacl.ttl");

    if !path.exists() {
        return;
    }

    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .load_path(&path)
        .unwrap_or_else(|e| panic!("api.shacl.ttl should parse without error: {}", e));
}

// ---------------------------------------------------------------------------
// Test 2: cli.shacl.ttl loads as valid Turtle
// ---------------------------------------------------------------------------

#[test]
fn test_cli_shacl_ttl_loads_as_valid_turtle() {
    let path = workspace_root()
        .join("templates/cli/subcommand/graphs/shapes/cli.shacl.ttl");

    if !path.exists() {
        return;
    }

    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .load_path(&path)
        .unwrap_or_else(|e| panic!("cli.shacl.ttl should parse without error: {}", e));
}

// ---------------------------------------------------------------------------
// Test 3: api.shacl.ttl contains sh:NodeShape declarations
// ---------------------------------------------------------------------------

#[test]
fn test_api_shacl_ttl_contains_nodeshape_declarations() {
    let path = workspace_root()
        .join("templates/api/endpoint/graphs/shapes/api.shacl.ttl");

    if !path.exists() {
        return;
    }

    let graph = Graph::new().expect("Graph::new should succeed");
    graph.load_path(&path).expect("load api.shacl.ttl");

    let sparql = r#"
        PREFIX sh: <http://www.w3.org/ns/shacl#>
        SELECT ?shape WHERE {
            ?shape a sh:NodeShape .
        }
    "#;

    let rows = select_solutions(&graph, sparql);
    assert!(
        !rows.is_empty(),
        "api.shacl.ttl should declare at least one sh:NodeShape"
    );
}

// ---------------------------------------------------------------------------
// Test 4: api.shacl.ttl has targetClass constraints
// ---------------------------------------------------------------------------

#[test]
fn test_api_shacl_ttl_has_target_class_constraints() {
    let path = workspace_root()
        .join("templates/api/endpoint/graphs/shapes/api.shacl.ttl");

    if !path.exists() {
        return;
    }

    let graph = Graph::new().expect("Graph::new should succeed");
    graph.load_path(&path).expect("load api.shacl.ttl");

    let sparql = r#"
        PREFIX sh: <http://www.w3.org/ns/shacl#>
        SELECT ?tc WHERE {
            ?shape sh:targetClass ?tc .
        }
    "#;

    let rows = select_solutions(&graph, sparql);
    assert!(
        !rows.is_empty(),
        "api.shacl.ttl shapes should have sh:targetClass declarations"
    );
}

// ---------------------------------------------------------------------------
// Test 5: api.shacl.ttl defines property constraints
// ---------------------------------------------------------------------------

#[test]
fn test_api_shacl_ttl_defines_property_constraints() {
    let path = workspace_root()
        .join("templates/api/endpoint/graphs/shapes/api.shacl.ttl");

    if !path.exists() {
        return;
    }

    let graph = Graph::new().expect("Graph::new should succeed");
    graph.load_path(&path).expect("load api.shacl.ttl");

    let sparql = r#"
        PREFIX sh: <http://www.w3.org/ns/shacl#>
        SELECT ?path WHERE {
            ?shape sh:property/sh:path ?path .
        }
    "#;

    let rows = select_solutions(&graph, sparql);
    assert!(
        !rows.is_empty(),
        "api.shacl.ttl shapes should define property constraints with sh:path"
    );
}

// ---------------------------------------------------------------------------
// Test 6: api.shacl.ttl has datatype constraints
// ---------------------------------------------------------------------------

#[test]
fn test_api_shacl_ttl_has_datatype_constraints() {
    let path = workspace_root()
        .join("templates/api/endpoint/graphs/shapes/api.shacl.ttl");

    if !path.exists() {
        return;
    }

    let graph = Graph::new().expect("Graph::new should succeed");
    graph.load_path(&path).expect("load api.shacl.ttl");

    let sparql = r#"
        PREFIX sh: <http://www.w3.org/ns/shacl#>
        SELECT ?datatype WHERE {
            ?shape sh:property/sh:datatype ?datatype .
        }
    "#;

    let rows = select_solutions(&graph, sparql);
    assert!(
        !rows.is_empty(),
        "api.shacl.ttl should have at least one sh:datatype constraint"
    );
}

// ---------------------------------------------------------------------------
// Test 7: cli.shacl.ttl contains sh:NodeShape declarations
// ---------------------------------------------------------------------------

#[test]
fn test_cli_shacl_ttl_contains_nodeshape_declarations() {
    let path = workspace_root()
        .join("templates/cli/subcommand/graphs/shapes/cli.shacl.ttl");

    if !path.exists() {
        return;
    }

    let graph = Graph::new().expect("Graph::new should succeed");
    graph.load_path(&path).expect("load cli.shacl.ttl");

    let sparql = r#"
        PREFIX sh: <http://www.w3.org/ns/shacl#>
        SELECT ?shape WHERE {
            ?shape a sh:NodeShape .
        }
    "#;

    let rows = select_solutions(&graph, sparql);
    assert!(
        !rows.is_empty(),
        "cli.shacl.ttl should declare at least one sh:NodeShape"
    );
}

// ---------------------------------------------------------------------------
// Test 8: cli.shacl.ttl has property constraints
// ---------------------------------------------------------------------------

#[test]
fn test_cli_shacl_ttl_has_property_constraints() {
    let path = workspace_root()
        .join("templates/cli/subcommand/graphs/shapes/cli.shacl.ttl");

    if !path.exists() {
        return;
    }

    let graph = Graph::new().expect("Graph::new should succeed");
    graph.load_path(&path).expect("load cli.shacl.ttl");

    let sparql = r#"
        PREFIX sh: <http://www.w3.org/ns/shacl#>
        SELECT ?path WHERE {
            ?shape sh:property/sh:path ?path .
        }
    "#;

    let rows = select_solutions(&graph, sparql);
    assert!(
        !rows.is_empty(),
        "cli.shacl.ttl shapes should define property constraints with sh:path"
    );
}

// ---------------------------------------------------------------------------
// Test 9: Data violating SHACL shape is detectable via SPARQL
// ---------------------------------------------------------------------------

#[test]
fn test_shacl_violation_detectable_via_sparql() {
    let shape_path = workspace_root()
        .join("templates/api/endpoint/graphs/shapes/api.shacl.ttl");

    if !shape_path.exists() {
        return;
    }

    // Load the SHACL shapes
    let graph = Graph::new().expect("Graph::new should succeed");
    graph.load_path(&shape_path).expect("load api.shacl.ttl");

    // Insert data that violates the shape:
    // api.shacl.ttl requires ex:APIEndpoint nodes to have rdfs:label (minCount 1).
    // We create an ex:APIEndpoint without a rdfs:label.
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:MyEndpoint a ex:APIEndpoint .
    "#,
        )
        .expect("insert violation data");

    // SPARQL query: find nodes of the target class missing the required property.
    // The shape says ex:APIEndpoint must have rdfs:label with sh:minCount 1.
    let sparql = r#"
        PREFIX sh: <http://www.w3.org/ns/shacl#>
        PREFIX ex: <http://example.org/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

        SELECT ?node WHERE {
            ?shape sh:targetClass ex:APIEndpoint .
            ?shape sh:property ?prop .
            ?prop sh:path rdfs:label .
            ?prop sh:minCount ?mc .
            FILTER (?mc > 0)
            ?node a ex:APIEndpoint .
            FILTER NOT EXISTS { ?node rdfs:label ?value }
        }
    "#;

    let rows = select_solutions(&graph, sparql);
    assert!(
        !rows.is_empty(),
        "SPARQL should detect ex:MyEndpoint missing required rdfs:label"
    );

    // Verify the violating node is ex:MyEndpoint
    let node = rows[0].get("node").expect("binding 'node' should exist");
    assert!(
        node.contains("MyEndpoint"),
        "Expected to find MyEndpoint as violator, got: {}",
        node
    );
}

// ---------------------------------------------------------------------------
// Test 10: ShapeLoader stub baseline
// ---------------------------------------------------------------------------

#[test]
fn test_shape_loader_stub_returns_empty_set() {
    // Document the current stub behavior: ShapeLoader::load() always returns
    // an empty ShaclShapeSet regardless of input.
    let loader = ShapeLoader::new();

    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .

        ex:TestShape a sh:NodeShape ;
            sh:targetClass ex:TestClass .
    "#,
        )
        .expect("insert test shapes");

    let result = loader.load(&graph).expect("ShapeLoader::load should not error");
    assert!(
        result.is_empty(),
        "ShapeLoader::load is stubbed and should return empty ShaclShapeSet"
    );
    assert_eq!(
        result.len(),
        0,
        "ShapeLoader stub should report 0 shapes loaded"
    );
}

// ---------------------------------------------------------------------------
// Test 11: All .shacl.ttl files in templates/ parse without error
// ---------------------------------------------------------------------------

#[test]
fn test_all_shacl_ttl_files_parse_without_error() {
    let root = workspace_root();
    let templates_dir = root.join("templates");

    if !templates_dir.exists() {
        return;
    }

    let mut count = 0u32;
    let mut errors = Vec::new();

    for entry in WalkDir::new(&templates_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            e.path()
                .file_name()
                .and_then(|n| n.to_str())
                .map_or(false, |n| n.ends_with(".shacl.ttl"))
        })
    {
        let path = entry.path();

        let graph = match Graph::new() {
            Ok(g) => g,
            Err(e) => {
                errors.push(format!("Graph::new failed for {}: {}", path.display(), e));
                count += 1;
                continue;
            }
        };

        match graph.load_path(path) {
            Ok(()) => {}
            Err(e) => {
                errors.push(format!("PARSE ERROR in {}: {}", path.display(), e));
            }
        }
        count += 1;
    }

    assert!(
        count > 0,
        "Should find at least one .shacl.ttl file in templates/"
    );

    if !errors.is_empty() {
        panic!(
            "SHACL Turtle syntax errors ({} of {} files):\n{}",
            errors.len(),
            count,
            errors.join("\n")
        );
    }
}
