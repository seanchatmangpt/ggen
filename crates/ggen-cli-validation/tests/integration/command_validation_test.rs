//! Integration tests for command validation

use ggen_cli_validation::NounVerbValidator;

#[test]
fn test_circular_dependency_detection() {
    let mut validator = NounVerbValidator::new();

    // Create circular: A -> B -> C -> A
    validator.register_command("A".to_string(), vec!["B".to_string()]);
    validator.register_command("B".to_string(), vec!["C".to_string()]);
    validator.register_command("C".to_string(), vec!["A".to_string()]);

    let result = validator.validate_sequence(&["A".to_string()]);
    assert!(result.is_err());
}

#[test]
fn test_valid_linear_chain() {
    let mut validator = NounVerbValidator::new();

    // Linear: A -> B -> C
    validator.register_command("A".to_string(), vec!["B".to_string()]);
    validator.register_command("B".to_string(), vec!["C".to_string()]);
    validator.register_command("C".to_string(), vec![]);

    let result = validator.validate_sequence(&["A".to_string()]);
    assert!(result.is_ok());
}

#[test]
fn test_command_structure_validation() {
    let validator = NounVerbValidator::new();

    // Valid commands
    assert!(validator.validate_command_structure("template", "generate").is_ok());
    assert!(validator.validate_command_structure("ontology", "extract").is_ok());

    // Invalid commands
    assert!(validator.validate_command_structure("", "verb").is_err());
    assert!(validator.validate_command_structure("noun", "").is_err());
    assert!(validator.validate_command_structure("noun!", "verb").is_err());
}

#[test]
fn test_audit_trail() {
    let mut validator = NounVerbValidator::new();

    validator.record_execution("template generate".to_string(), vec![], true);
    validator.record_execution("ontology extract".to_string(), vec![], true);

    let trail = validator.get_audit_trail();
    assert_eq!(trail.len(), 2);
    assert_eq!(trail[0].command, "template generate");
    assert_eq!(trail[1].command, "ontology extract");
}

#[test]
fn test_audit_trail_export() {
    let mut validator = NounVerbValidator::new();
    validator.record_execution("test".to_string(), vec![], true);

    let json = validator.export_audit_trail();
    assert!(json.is_ok());
    let json_str = json.expect("Should serialize");
    assert!(json_str.contains("test"));
    assert!(json_str.contains("success"));
}

#[test]
fn test_complex_dependency_graph() {
    let mut validator = NounVerbValidator::new();

    // Complex graph without cycles
    validator.register_command("init".to_string(), vec![]);
    validator.register_command("config".to_string(), vec!["init".to_string()]);
    validator.register_command("build".to_string(), vec!["config".to_string()]);
    validator.register_command("test".to_string(), vec!["build".to_string()]);
    validator.register_command("deploy".to_string(), vec!["test".to_string()]);

    let result = validator.validate_sequence(&["deploy".to_string()]);
    assert!(result.is_ok());
}
