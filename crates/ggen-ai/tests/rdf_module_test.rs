//! Test to verify RDF module is properly exported and accessible

use ggen_ai::rdf::{
    Argument, ArgumentType, CliProject, Dependency, Noun, TemplateRenderer, Validation, Verb,
};

#[test]
fn test_rdf_module_exports() {
    // Verify all types are accessible and can be constructed
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

    let _arg_type = ArgumentType {
        name: "String".to_string(),
        parser: None,
    };

    let _validation = Validation {
        rule: "file_exists".to_string(),
        pattern: None,
        message: "File must exist".to_string(),
        arg_name: "input".to_string(),
    };

    let _dependency = Dependency {
        name: "clap".to_string(),
        version: "4.0".to_string(),
        features: vec!["derive".to_string()],
        optional: false,
    };
}

#[test]
fn test_template_renderer_accessible() {
    // Verify TemplateRenderer type is accessible
    // Note: Cannot construct without valid template directory
    use std::path::Path;

    let result = TemplateRenderer::new(Path::new("/nonexistent"));
    assert!(result.is_err(), "Should error with nonexistent path");
}

#[test]
fn test_cli_project_with_complete_structure() {
    let project = CliProject {
        name: "my-cli".to_string(),
        version: "1.0.0".to_string(),
        description: "My CLI app".to_string(),
        authors: vec!["Author <author@example.com>".to_string()],
        edition: "2021".to_string(),
        license: "MIT".to_string(),
        nouns: vec![Noun {
            name: "user".to_string(),
            description: "User operations".to_string(),
            module_path: "cmds::user".to_string(),
            verbs: vec![Verb {
                name: "create".to_string(),
                description: "Create user".to_string(),
                alias: Some("add".to_string()),
                arguments: vec![Argument {
                    name: "name".to_string(),
                    long: Some("name".to_string()),
                    short: Some('n'),
                    help: "User name".to_string(),
                    required: true,
                    default: None,
                    value_name: Some("NAME".to_string()),
                    position: None,
                    arg_type: ArgumentType {
                        name: "String".to_string(),
                        parser: None,
                    },
                }],
                validations: vec![],
                execution_logic: None,
            }],
        }],
        dependencies: vec![Dependency {
            name: "clap".to_string(),
            version: "4.0".to_string(),
            features: vec!["derive".to_string()],
            optional: false,
        }],
    };

    // Verify structure is accessible
    assert_eq!(project.name, "my-cli");
    assert_eq!(project.nouns.len(), 1);
    assert_eq!(project.nouns[0].verbs.len(), 1);
    assert_eq!(project.dependencies.len(), 1);
}
