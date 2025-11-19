//! Noun-verb command structure validation
//!
//! Validates clap-noun-verb command sequences for:
//! - Circular dependencies
//! - Valid command chains
//! - Audit trail generation

use crate::error::{Result, ValidationError};
use std::collections::{HashMap, HashSet};

/// Validator for noun-verb command structures
#[derive(Debug, Default)]
pub struct NounVerbValidator {
    /// Command dependency graph
    dependencies: HashMap<String, Vec<String>>,
    /// Execution history for audit trail
    audit_trail: Vec<AuditEntry>,
}

/// Audit trail entry
#[derive(Debug, Clone, serde::Serialize)]
pub struct AuditEntry {
    /// Command executed
    pub command: String,
    /// Timestamp
    pub timestamp: String,
    /// Dependencies resolved
    pub dependencies: Vec<String>,
    /// Success status
    pub success: bool,
}

impl NounVerbValidator {
    /// Create a new validator
    #[must_use]
    pub fn new() -> Self {
        Self {
            dependencies: HashMap::new(),
            audit_trail: Vec::new(),
        }
    }

    /// Register a command with its dependencies
    pub fn register_command(&mut self, command: String, deps: Vec<String>) {
        self.dependencies.insert(command, deps);
    }

    /// Validate a command sequence for circular dependencies
    pub fn validate_sequence(&self, commands: &[String]) -> Result<()> {
        for command in commands {
            self.check_circular_dependencies(command, &mut HashSet::new())?;
        }
        Ok(())
    }

    /// Check for circular dependencies recursively
    fn check_circular_dependencies(
        &self,
        command: &str,
        visited: &mut HashSet<String>,
    ) -> Result<()> {
        if visited.contains(command) {
            return Err(ValidationError::CircularDependency);
        }

        visited.insert(command.to_string());

        if let Some(deps) = self.dependencies.get(command) {
            for dep in deps {
                self.check_circular_dependencies(dep, visited)?;
            }
        }

        visited.remove(command);
        Ok(())
    }

    /// Validate command structure
    pub fn validate_command_structure(&self, noun: &str, verb: &str) -> Result<()> {
        // Check noun is not empty
        if noun.is_empty() {
            return Err(ValidationError::InvalidCommandStructure {
                reason: "Noun cannot be empty".to_string(),
            });
        }

        // Check verb is not empty
        if verb.is_empty() {
            return Err(ValidationError::InvalidCommandStructure {
                reason: "Verb cannot be empty".to_string(),
            });
        }

        // Check for valid characters (alphanumeric, dash, underscore)
        let valid_chars = |s: &str| {
            s.chars()
                .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
        };

        if !valid_chars(noun) {
            return Err(ValidationError::InvalidCommandStructure {
                reason: format!("Noun '{noun}' contains invalid characters"),
            });
        }

        if !valid_chars(verb) {
            return Err(ValidationError::InvalidCommandStructure {
                reason: format!("Verb '{verb}' contains invalid characters"),
            });
        }

        Ok(())
    }

    /// Record command execution in audit trail
    pub fn record_execution(
        &mut self,
        command: String,
        dependencies: Vec<String>,
        success: bool,
    ) {
        let entry = AuditEntry {
            command,
            timestamp: chrono::Utc::now().to_rfc3339(),
            dependencies,
            success,
        };
        self.audit_trail.push(entry);
    }

    /// Get audit trail
    #[must_use]
    pub fn get_audit_trail(&self) -> &[AuditEntry] {
        &self.audit_trail
    }

    /// Export audit trail as JSON
    ///
    /// # Errors
    /// Returns error if serialization fails
    pub fn export_audit_trail(&self) -> Result<String> {
        serde_json::to_string_pretty(&self.audit_trail).map_err(|e| {
            ValidationError::InvalidCommandStructure {
                reason: format!("Failed to serialize audit trail: {e}"),
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_circular_dependency_detection() {
        let mut validator = NounVerbValidator::new();

        // Create circular dependency: A -> B -> C -> A
        validator.register_command("A".to_string(), vec!["B".to_string()]);
        validator.register_command("B".to_string(), vec!["C".to_string()]);
        validator.register_command("C".to_string(), vec!["A".to_string()]);

        let result = validator.validate_sequence(&["A".to_string()]);
        assert!(result.is_err());
        assert!(matches!(result, Err(ValidationError::CircularDependency)));
    }

    #[test]
    fn test_valid_command_structure() {
        let validator = NounVerbValidator::new();

        assert!(validator.validate_command_structure("template", "generate").is_ok());
        assert!(validator.validate_command_structure("ontology", "extract").is_ok());
        assert!(validator.validate_command_structure("hook", "create").is_ok());
    }

    #[test]
    fn test_invalid_command_structure() {
        let validator = NounVerbValidator::new();

        // Empty noun/verb
        assert!(validator.validate_command_structure("", "verb").is_err());
        assert!(validator.validate_command_structure("noun", "").is_err());

        // Invalid characters
        assert!(validator.validate_command_structure("noun!", "verb").is_err());
        assert!(validator.validate_command_structure("noun", "verb*").is_err());
    }

    #[test]
    fn test_audit_trail_recording() {
        let mut validator = NounVerbValidator::new();

        validator.record_execution(
            "template generate".to_string(),
            vec!["template load".to_string()],
            true,
        );

        let trail = validator.get_audit_trail();
        assert_eq!(trail.len(), 1);
        assert_eq!(trail[0].command, "template generate");
        assert!(trail[0].success);
    }

    #[test]
    fn test_audit_trail_export() {
        let mut validator = NounVerbValidator::new();

        validator.record_execution("test command".to_string(), vec![], true);

        let json = validator.export_audit_trail();
        assert!(json.is_ok());
        assert!(json.expect("JSON should be valid").contains("test command"));
    }

    #[test]
    fn test_linear_dependency_chain() {
        let mut validator = NounVerbValidator::new();

        // Linear chain: A -> B -> C (no cycles)
        validator.register_command("A".to_string(), vec!["B".to_string()]);
        validator.register_command("B".to_string(), vec!["C".to_string()]);
        validator.register_command("C".to_string(), vec![]);

        let result = validator.validate_sequence(&["A".to_string()]);
        assert!(result.is_ok());
    }
}
