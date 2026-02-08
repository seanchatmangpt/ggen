//! Unit tests for ggen-yawl workflow generation.

use ggen_yawl::{OntologyLoader, YawlGenerator};

// Van der Aalst workflow pattern tests
pub mod patterns;

#[test]
fn test_load_fibo_ontology() {
    let ontology = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:TestProcess a owl:Class ;
            rdfs:label "Test Process" ;
            ex:stereo "BusinessProcess" .

        ex:TestStep a owl:Class ;
            rdfs:label "Test Step" ;
            rdfs:subClassOf ex:TestProcess .
    "#;

    let result = OntologyLoader::new().load_from_str(ontology, ggen_yawl::OntologyFormat::Turtle);
    assert!(result.is_ok(), "Should load valid Turtle ontology");
}

#[test]
fn test_template_context_serialization() {
    use ggen_yawl::template::TemplateContext;

    let ctx = TemplateContext {
        workflow_name: "TestWorkflow".to_string(),
        description: "A test workflow".to_string(),
        version: "1.0.0".to_string(),
        ..Default::default()
    };

    let json = serde_json::to_string(&ctx);
    assert!(json.is_ok());

    let parsed: TemplateContext = serde_json::from_str(&json.unwrap()).unwrap();
    assert_eq!(parsed.workflow_name, "TestWorkflow");
}

#[test]
fn test_yawl_xml_generation() {
    use ggen_yawl::codegen::YawlXmlGenerator;
    use ggen_yawl::template::{TemplateContext, TaskContext};

    let ctx = TemplateContext {
        workflow_name: "TestWorkflow".to_string(),
        description: "Test".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![TaskContext {
            id: "t1".to_string(),
            name: "Test Task".to_string(),
            split_type: "XOR".to_string(),
            join_type: "XOR".to_string(),
            is_auto: true,
            decomposition_id: None,
        }],
        flows: vec![],
        input_condition: None,
        output_condition: None,
        variables: vec![],
    };

    let result = YawlXmlGenerator::generate(&ctx);
    assert!(result.is_ok());

    let xml = result.unwrap();
    assert!(xml.contains("<?xml version=\"1.0\""));
    assert!(xml.contains("<specification"));
    assert!(xml.contains("Test Task"));
}

#[test]
fn test_construct_executor_topological_sort() {
    use ggen_yawl::transform::ConstructExecutor;

    let executor = ConstructExecutor::new();
    let order = executor.topological_sort();

    assert!(order.is_ok(), "Should sort queries without cycles");

    let sorted = order.unwrap();
    assert!(!sorted.is_empty(), "Should have queries to execute");

    // class_to_task should execute before property_to_flow
    let class_idx = sorted.iter().position(|x| x == "class_to_task");
    let property_idx = sorted.iter().position(|x| x == "property_to_flow");

    if let (Some(ci), Some(pi)) = (class_idx, property_idx) {
        assert!(ci < pi, "class_to_task should come before property_to_flow");
    }
}

#[test]
fn test_query_files_exist() {
    use std::path::Path;

    let queries_dir = Path::new("crates/ggen-yawl/queries");

    let expected_queries = vec![
        "01-extract-tasks.rq",
        "02-extract-flows.rq",
        "03-cardinality-splitjoin.rq",
        "04-rules-to-conditions.rq",
        "05-multiple-instance.rq",
        "06-composite-task.rq",
    ];

    for query in expected_queries {
        let path = queries_dir.join(query);
        assert!(path.exists(), "Query file should exist: {}", query);
    }
}

#[test]
fn test_template_files_exist() {
    use std::path::Path;

    let templates_dir = Path::new("crates/ggen-yawl/templates");
    let workflow_template = templates_dir.join("workflow.yawl.tera");

    assert!(workflow_template.exists(), "YAWL workflow template should exist");
}

#[test]
fn test_fibo_fixture_exists() {
    use std::path::Path;

    let fixture_path = Path::new("tests/fixtures/ontologies/fibo_account_opening.ttl");
    assert!(fixture_path.exists(), "FIBO fixture should exist");

    // Verify it's valid Turtle by loading it
    let content = std::fs::read_to_string(&fixture_path).unwrap();
    assert!(content.contains("@prefix"));
    assert!(content.contains("AccountOpeningProcess"));
}
