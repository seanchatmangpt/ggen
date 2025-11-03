//! Basic test to verify RDF module is properly exported and accessible from ggen-ai

#[test]
fn test_rdf_types_accessible_from_ggen_ai() {
    // Verify all RDF types are accessible from ggen_ai root
    use ggen_ai::{Argument, ArgumentType, CliProject, Dependency, Noun, TemplateRenderer, Validation, Verb};

    // Create a minimal project structure
    let _project = CliProject {
        name: "test-cli".to_string(),
        version: "0.1.0".to_string(),
        description: "Test CLI".to_string(),
        authors: vec!["Test Author <test@example.com>".to_string()],
        edition: "2021".to_string(),
        license: "MIT".to_string(),
        nouns: vec![],
        dependencies: vec![],
    };

    // Verify types can be constructed
    let _noun = Noun {
        name: "user".to_string(),
        description: "User management".to_string(),
        module_path: "cmds::user".to_string(),
        verbs: vec![],
    };

    let _verb = Verb {
        name: "create".to_string(),
        description: "Create a new user".to_string(),
        alias: None,
        arguments: vec![],
        validations: vec![],
        execution_logic: None,
    };

    let _argument = Argument {
        name: "input".to_string(),
        long: Some("input".to_string()),
        short: Some('i'),
        help: "Input file".to_string(),
        required: true,
        default: None,
        value_name: Some("FILE".to_string()),
        position: None,
        arg_type: ArgumentType {
            name: "String".to_string(),
            parser: None,
        },
    };

    let _dependency = Dependency {
        name: "clap".to_string(),
        version: "4.0".to_string(),
        features: vec!["derive".to_string()],
        optional: false,
    };

    let _validation = Validation {
        rule: "file_exists".to_string(),
        pattern: None,
        message: "File must exist".to_string(),
        arg_name: "input".to_string(),
    };

    // Test passes if we can construct all types
}

#[test]
fn test_rdf_module_structure() {
    // Verify RDF module types are accessible via module path
    use ggen_ai::rdf::types::{CliProject, Noun, Verb, Argument, ArgumentType, Validation, Dependency};
    use ggen_ai::rdf::TemplateRenderer;

    // Verify we can reference types via full path
    let _project: CliProject = CliProject {
        name: "test".to_string(),
        version: "1.0.0".to_string(),
        description: "Test".to_string(),
        authors: vec![],
        edition: "2021".to_string(),
        license: "MIT".to_string(),
        nouns: vec![],
        dependencies: vec![],
    };

    // Verify TemplateRenderer type is accessible
    let _renderer_type: Option<TemplateRenderer> = None;

    // Test passes if all types are accessible
}
