// Test to validate behavior predicates TTL files
use oxigraph::io::RdfFormat;
use oxigraph::store::Store;
use std::fs::File;
use std::io::BufReader;
use std::path::PathBuf;

/// Get the path to the workspace root from CARGO_MANIFEST_DIR
fn workspace_root() -> PathBuf {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    // ggen-core is at crates/ggen-core/, so workspace root is ../../
    PathBuf::from(manifest_dir)
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_path_buf()
}

/// Get the path to the 014-a2a-integration spec directory
fn spec_dir() -> PathBuf {
    workspace_root().join(".specify/specs/014-a2a-integration")
}

#[test]
fn test_behavior_predicates_ttl_syntax() {
    let behavior_predicates_path = spec_dir().join("behavior-predicates.ttl");

    assert!(
        behavior_predicates_path.exists(),
        "behavior-predicates.ttl should exist at {:?}",
        behavior_predicates_path
    );

    let file =
        File::open(&behavior_predicates_path).expect("Failed to open behavior-predicates.ttl");

    let store = Store::new().unwrap();
    let reader = BufReader::new(file);

    let result = store.load_from_reader(RdfFormat::Turtle, reader);

    assert!(
        result.is_ok(),
        "behavior-predicates.ttl should be valid TTL syntax"
    );

    // Verify we can query the ontology
    let query_str = "SELECT ?ex ?pred ?obj WHERE { ?ex ?pred ?obj } LIMIT 10";

    let results = store.query(query_str);
    assert!(
        results.is_ok(),
        "behavior-predicates.ttl should be queryable"
    );

    println!("✓ behavior-predicates.ttl: syntax valid and queryable");
}

#[test]
fn test_behavior_example_ttl_syntax() {
    let behavior_example_path = spec_dir().join("behavior-example.ttl");

    assert!(
        behavior_example_path.exists(),
        "behavior-example.ttl should exist at {:?}",
        behavior_example_path
    );

    let file = File::open(&behavior_example_path).expect("Failed to open behavior-example.ttl");

    let store = Store::new().unwrap();
    let reader = BufReader::new(file);

    let result = store.load_from_reader(RdfFormat::Turtle, reader);

    assert!(
        result.is_ok(),
        "behavior-example.ttl should be valid TTL syntax"
    );

    // Verify we can query for specific predicates
    let query_str = "
        PREFIX a2a: <https://a2a.dev/ontology#>
        PREFIX mcp: <https://ggen.io/ontology/mcp#>
        SELECT ?skill WHERE {
            ?skill a2a:hasSystemPrompt ?prompt .
        }
        LIMIT 10
    ";

    let results = store.query(query_str);
    assert!(
        results.is_ok(),
        "behavior-example.ttl should support SPARQL queries"
    );

    println!("✓ behavior-example.ttl: syntax valid and queryable");
}

#[test]
fn test_has_system_prompt_predicate_exists() {
    let behavior_predicates_path = spec_dir().join("behavior-predicates.ttl");

    let file =
        File::open(&behavior_predicates_path).expect("Failed to open behavior-predicates.ttl");

    let store = Store::new().unwrap();
    let reader = BufReader::new(file);

    store.load_from_reader(RdfFormat::Turtle, reader).unwrap();

    // Check that hasSystemPrompt predicate is defined
    let query_str = "
        PREFIX a2a: <https://a2a.dev/ontology#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK {
            a2a:hasSystemPrompt a rdf:Property .
        }
    ";

    let results = store.query(query_str);
    assert!(
        results.is_ok(),
        "hasSystemPrompt predicate query should succeed"
    );

    println!("✓ hasSystemPrompt predicate queryable");
}

#[test]
fn test_has_implementation_hint_predicate_exists() {
    let behavior_predicates_path = spec_dir().join("behavior-predicates.ttl");

    let file =
        File::open(&behavior_predicates_path).expect("Failed to open behavior-predicates.ttl");

    let store = Store::new().unwrap();
    let reader = BufReader::new(file);

    store.load_from_reader(RdfFormat::Turtle, reader).unwrap();

    // Check that hasImplementationHint predicate is defined
    let query_str = "
        PREFIX a2a: <https://a2a.dev/ontology#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK {
            a2a:hasImplementationHint a rdf:Property .
        }
    ";

    let results = store.query(query_str);
    assert!(
        results.is_ok(),
        "hasImplementationHint predicate query should succeed"
    );

    println!("✓ hasImplementationHint predicate queryable");
}

#[test]
fn test_has_test_example_predicate_exists() {
    let behavior_predicates_path = spec_dir().join("behavior-predicates.ttl");

    let file =
        File::open(&behavior_predicates_path).expect("Failed to open behavior-predicates.ttl");

    let store = Store::new().unwrap();
    let reader = BufReader::new(file);

    store.load_from_reader(RdfFormat::Turtle, reader).unwrap();

    // Check that hasTestExample predicate is defined
    let query_str = "
        PREFIX a2a: <https://a2a.dev/ontology#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK {
            a2a:hasTestExample a rdf:Property .
        }
    ";

    let results = store.query(query_str);
    assert!(
        results.is_ok(),
        "hasTestExample predicate query should succeed"
    );

    println!("✓ hasTestExample predicate queryable");
}

#[test]
fn test_mcp_auto_implementation_predicate_exists() {
    let behavior_predicates_path = spec_dir().join("behavior-predicates.ttl");

    let file =
        File::open(&behavior_predicates_path).expect("Failed to open behavior-predicates.ttl");

    let store = Store::new().unwrap();
    let reader = BufReader::new(file);

    store.load_from_reader(RdfFormat::Turtle, reader).unwrap();

    // Check that hasAutoImplementation predicate is defined
    let query_str = "
        PREFIX mcp: <https://ggen.io/ontology/mcp#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK {
            mcp:hasAutoImplementation a rdf:Property .
        }
    ";

    let results = store.query(query_str);
    assert!(
        results.is_ok(),
        "hasAutoImplementation predicate query should succeed"
    );

    println!("✓ hasAutoImplementation predicate queryable");
}
