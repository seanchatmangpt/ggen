//! Integration tests for Phase 3.2 introspection flags
//! Tests the --capabilities, --introspect, --graph CLI flags for AI agent discovery

// These tests require the `autonomic` feature flag to access introspection module
#![cfg(feature = "autonomic")]

#[test]
fn capabilities_metadata_table_driven() {
    use ggen_cli_lib::introspection;

    struct Case {
        noun: &'static str,
        verb: &'static str,
        require_json: bool,
        required_args: &'static [&'static str],
        expected_return: Option<&'static str>,
    }

    let cases = [
        Case {
            noun: "template",
            verb: "generate",
            require_json: true,
            required_args: &[],
            expected_return: None,
        },
        Case {
            noun: "ai",
            verb: "generate-ontology",
            require_json: true,
            required_args: &["prompt"],
            expected_return: None,
        },
        Case {
            noun: "ci",
            verb: "workflow",
            require_json: true,
            required_args: &[],
            expected_return: None,
        },
        Case {
            noun: "fmea",
            verb: "report",
            require_json: true,
            required_args: &["format", "risk", "top"],
            expected_return: None,
        },
        Case {
            noun: "graph",
            verb: "load",
            require_json: false,
            required_args: &[],
            expected_return: Some("LoadOutput"),
        },
    ];

    for case in cases {
        let metadata = introspection::get_verb_metadata(case.noun, case.verb);
        assert!(
            metadata.is_some(),
            "{}/{} should exist",
            case.noun,
            case.verb
        );

        let m = metadata.unwrap();
        assert_eq!(m.noun, case.noun);
        assert_eq!(m.verb, case.verb);
        assert!(
            !m.description.is_empty(),
            "{}/{} should have a description",
            case.noun,
            case.verb
        );

        if case.require_json {
            assert!(
                m.supports_json_output,
                "{}/{} should emit JSON",
                case.noun, case.verb
            );
        }

        for arg in case.required_args {
            let found = m.arguments.iter().find(|a| a.name == *arg);
            assert!(
                found.is_some(),
                "{}/{} should define required arg {}",
                case.noun,
                case.verb,
                arg
            );
            assert!(
                !found.unwrap().optional,
                "{}/{} arg {} must be required",
                case.noun,
                case.verb,
                arg
            );
        }

        if let Some(ret) = case.expected_return {
            assert_eq!(
                m.return_type, ret,
                "{}/{} return type",
                case.noun, case.verb
            );
        }
    }
}

#[test]
fn capabilities_missing_verb_returns_none() {
    use ggen_cli_lib::introspection;

    let metadata = introspection::get_verb_metadata("nonexistent", "verb");
    assert!(metadata.is_none(), "nonexistent verb should return None");
}

#[test]
fn argument_metadata_is_complete() {
    use ggen_cli_lib::introspection;

    let metadata = introspection::get_verb_metadata("template", "generate");
    let m = metadata.expect("template::generate should exist");
    for arg in &m.arguments {
        assert!(!arg.name.is_empty());
        assert!(!arg.argument_type.is_empty());
        assert!(!arg.description.is_empty());
    }
}

#[test]
fn command_graph_shape_and_content() {
    use ggen_cli_lib::introspection;

    let graph = introspection::build_command_graph();
    assert_eq!(graph.version, "5.3.0");
    assert!(graph.total_verbs > 0, "Graph should contain verbs");
    assert!(!graph.nouns.is_empty(), "Graph should contain nouns");

    let expected_nouns = ["ai", "template", "graph", "ci", "fmea"];
    for noun in expected_nouns {
        let noun_desc = graph
            .nouns
            .get(noun)
            .unwrap_or_else(|| panic!("Graph should contain {} noun", noun));
        assert!(
            !noun_desc.verbs.is_empty(),
            "{} noun should have verbs",
            noun
        );
    }
}

#[test]
fn introspection_payloads_are_serializable() {
    use ggen_cli_lib::introspection;

    let registry = introspection::get_verb_registry();
    for metadata in registry.values() {
        serde_json::to_string(metadata).expect("Verb metadata should serialize to JSON");
    }

    let graph = introspection::build_command_graph();
    let json =
        serde_json::to_string_pretty(&graph).expect("Command graph should serialize to JSON");
    assert!(json.contains("version"));
    assert!(json.contains("total_verbs"));
    assert!(json.contains("nouns"));
}
