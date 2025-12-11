//! Integration tests for Phase 3.2 introspection flags
//! Tests the --capabilities, --introspect, --graph CLI flags for AI agent discovery

// These tests require the `autonomic` feature flag to access introspection module
#![cfg(feature = "autonomic")]

#[test]
fn test_capabilities_flag_template_generate() {
    // Simulate: ggen --capabilities template generate
    use ggen_cli_lib::introspection;

    let metadata = introspection::get_verb_metadata("template", "generate");
    assert!(metadata.is_some(), "template::generate should exist");

    let m = metadata.unwrap();
    assert_eq!(m.noun, "template");
    assert_eq!(m.verb, "generate");
    assert!(!m.description.is_empty());
    assert!(m.supports_json_output);
}

#[test]
fn test_capabilities_flag_ai_generate_ontology() {
    use ggen_cli_lib::introspection;

    let metadata = introspection::get_verb_metadata("ai", "generate-ontology");
    assert!(metadata.is_some(), "ai::generate-ontology should exist");

    let m = metadata.unwrap();
    assert_eq!(m.noun, "ai");
    assert_eq!(m.verb, "generate-ontology");
    assert!(m.arguments.len() > 0);

    // Verify prompt argument exists and is required
    let prompt_arg = m.arguments.iter().find(|a| a.name == "prompt");
    assert!(prompt_arg.is_some());
    assert!(!prompt_arg.unwrap().optional);
}

#[test]
fn test_capabilities_flag_nonexistent_verb() {
    use ggen_cli_lib::introspection;

    let metadata = introspection::get_verb_metadata("nonexistent", "verb");
    assert!(metadata.is_none(), "nonexistent verb should return None");
}

#[test]
fn test_introspect_flag_shows_type_information() {
    use ggen_cli_lib::introspection;

    let metadata = introspection::get_verb_metadata("graph", "load");
    assert!(metadata.is_some());

    let m = metadata.unwrap();
    assert!(
        !m.return_type.is_empty(),
        "Return type should be documented"
    );
    assert_eq!(m.return_type, "LoadOutput");
}

#[test]
fn test_introspect_flag_argument_metadata() {
    use ggen_cli_lib::introspection;

    let metadata = introspection::get_verb_metadata("template", "generate");
    assert!(metadata.is_some());

    let m = metadata.unwrap();
    for arg in &m.arguments {
        assert!(!arg.name.is_empty());
        assert!(!arg.argument_type.is_empty());
        assert!(!arg.description.is_empty());
    }
}

#[test]
fn test_graph_flag_exports_command_graph() {
    use ggen_cli_lib::introspection;

    let graph = introspection::build_command_graph();
    assert_eq!(graph.version, "5.3.0");
    assert!(graph.total_verbs > 0, "Graph should contain verbs");
    assert!(!graph.nouns.is_empty(), "Graph should contain nouns");
}

#[test]
fn test_graph_contains_expected_nouns() {
    use ggen_cli_lib::introspection;

    let graph = introspection::build_command_graph();
    let expected_nouns = vec!["ai", "template", "graph", "ci", "fmea"];

    for noun in expected_nouns {
        assert!(
            graph.nouns.contains_key(noun),
            "Graph should contain {} noun",
            noun
        );
    }
}

#[test]
fn test_graph_noun_has_verbs() {
    use ggen_cli_lib::introspection;

    let graph = introspection::build_command_graph();
    let template_noun = graph.nouns.get("template");
    assert!(template_noun.is_some());

    let noun_desc = template_noun.unwrap();
    assert!(
        !noun_desc.verbs.is_empty(),
        "template noun should have verbs"
    );
}

#[test]
fn test_all_verbs_serializable_to_json() {
    use ggen_cli_lib::introspection;

    let registry = introspection::get_verb_registry();
    for metadata in registry.values() {
        let json = serde_json::to_string(&metadata);
        assert!(json.is_ok(), "All verbs should be JSON serializable");
    }
}

#[test]
fn test_command_graph_serializable_to_json() {
    use ggen_cli_lib::introspection;

    let graph = introspection::build_command_graph();
    let json = serde_json::to_string_pretty(&graph);
    assert!(json.is_ok(), "Command graph should be JSON serializable");

    let json_str = json.unwrap();
    assert!(json_str.contains("version"));
    assert!(json_str.contains("total_verbs"));
    assert!(json_str.contains("nouns"));
}

#[test]
fn test_capabilities_ci_workflow_metadata() {
    use ggen_cli_lib::introspection;

    let metadata = introspection::get_verb_metadata("ci", "workflow");
    assert!(metadata.is_some());

    let m = metadata.unwrap();
    assert_eq!(m.noun, "ci");
    assert_eq!(m.verb, "workflow");
    assert!(m.supports_json_output);
    assert!(m.arguments.len() > 0);
}

#[test]
fn test_capabilities_fmea_report_metadata() {
    use ggen_cli_lib::introspection;

    let metadata = introspection::get_verb_metadata("fmea", "report");
    assert!(metadata.is_some());

    let m = metadata.unwrap();
    assert_eq!(m.noun, "fmea");
    assert_eq!(m.verb, "report");

    // Verify format, risk, top arguments exist
    assert!(m.arguments.iter().any(|a| a.name == "format"));
    assert!(m.arguments.iter().any(|a| a.name == "risk"));
    assert!(m.arguments.iter().any(|a| a.name == "top"));
}
