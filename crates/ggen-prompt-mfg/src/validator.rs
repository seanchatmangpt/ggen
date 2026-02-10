//! Prompt schema validation
//!
//! Validates prompt IR against schema rules before emission.

use crate::ir::{PromptIR, Section, SectionType};
use crate::{PromptError, Result};

/// Prompt validator
///
/// Enforces schema rules and structural constraints on prompt IR.
pub struct PromptValidator {
    /// Minimum schema version supported
    min_schema_version: semver::Version,
}

impl PromptValidator {
    /// Create a new prompt validator
    pub fn new() -> Self {
        Self {
            min_schema_version: semver::Version::new(1, 0, 0),
        }
    }

    /// Validate prompt IR
    ///
    /// # Errors
    ///
    /// Returns error if validation fails
    pub fn validate(&self, ir: &PromptIR) -> Result<()> {
        self.validate_metadata(ir)?;
        self.validate_sections(ir)?;
        self.validate_variables(ir)?;
        Ok(())
    }

    fn validate_metadata(&self, ir: &PromptIR) -> Result<()> {
        // Validate ID is non-empty
        if ir.metadata.id.is_empty() {
            return Err(PromptError::Validation("Prompt ID cannot be empty".to_string()));
        }

        // Validate version format
        semver::Version::parse(&ir.metadata.version).map_err(|e| {
            PromptError::Validation(format!("Invalid version format: {e}"))
        })?;

        // Validate schema version
        let schema_version = semver::Version::parse(&ir.metadata.schema_version).map_err(|e| {
            PromptError::Validation(format!("Invalid schema version: {e}"))
        })?;

        if schema_version < self.min_schema_version {
            return Err(PromptError::Validation(format!(
                "Schema version {} is below minimum {}",
                schema_version, self.min_schema_version
            )));
        }

        // Validate source ontology is URI-like
        if ir.metadata.source_ontology.is_empty() {
            return Err(PromptError::Validation(
                "Source ontology cannot be empty".to_string(),
            ));
        }

        Ok(())
    }

    fn validate_sections(&self, ir: &PromptIR) -> Result<()> {
        for (key, section) in &ir.sections {
            // Validate section key is non-empty
            if key.is_empty() {
                return Err(PromptError::Validation(
                    "Section key cannot be empty".to_string(),
                ));
            }

            // Validate section structure
            self.validate_section(section)?;
        }

        Ok(())
    }

    fn validate_section(&self, section: &Section) -> Result<()> {
        // Validate blocks are non-empty for non-custom sections
        match &section.section_type {
            SectionType::System | SectionType::User | SectionType::Assistant => {
                if section.blocks.is_empty() {
                    return Err(PromptError::Validation(
                        "Standard sections must have at least one block".to_string(),
                    ));
                }
            }
            SectionType::Custom(_) => {
                // Custom sections can be empty
            }
        }

        // Validate each block
        for block in &section.blocks {
            if block.content.is_empty() {
                return Err(PromptError::Validation(
                    "Block content cannot be empty".to_string(),
                ));
            }
        }

        Ok(())
    }

    fn validate_variables(&self, ir: &PromptIR) -> Result<()> {
        for (key, variable) in &ir.variables {
            // Validate variable key matches variable name
            if key != &variable.name {
                return Err(PromptError::Validation(format!(
                    "Variable key '{}' does not match name '{}'",
                    key, variable.name
                )));
            }

            // Validate variable name is valid identifier
            if !is_valid_identifier(&variable.name) {
                return Err(PromptError::Validation(format!(
                    "Invalid variable name: '{}'",
                    variable.name
                )));
            }
        }

        Ok(())
    }
}

impl Default for PromptValidator {
    fn default() -> Self {
        Self::new()
    }
}

/// Check if string is a valid identifier
fn is_valid_identifier(s: &str) -> bool {
    !s.is_empty()
        && s.chars().next().is_some_and(|c| c.is_alphabetic() || c == '_')
        && s.chars().all(|c| c.is_alphanumeric() || c == '_')
}

// Minimal semver implementation for validation
mod semver {
    use std::fmt;

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Version {
        pub major: u64,
        pub minor: u64,
        pub patch: u64,
    }

    impl Version {
        pub fn new(major: u64, minor: u64, patch: u64) -> Self {
            Self {
                major,
                minor,
                patch,
            }
        }

        pub fn parse(s: &str) -> std::result::Result<Self, String> {
            let parts: Vec<&str> = s.split('.').collect();
            if parts.len() != 3 {
                return Err(format!("Invalid version format: {s}"));
            }

            let major = parts[0]
                .parse()
                .map_err(|_| format!("Invalid major version: {}", parts[0]))?;
            let minor = parts[1]
                .parse()
                .map_err(|_| format!("Invalid minor version: {}", parts[1]))?;
            let patch = parts[2]
                .parse()
                .map_err(|_| format!("Invalid patch version: {}", parts[2]))?;

            Ok(Self::new(major, minor, patch))
        }
    }

    impl fmt::Display for Version {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::{BlockType, ContentBlock, PromptMetadata, PromptVariable, VariableType};
    use std::collections::BTreeMap;

    #[test]
    fn test_validator_creation() {
        let validator = PromptValidator::new();
        assert_eq!(validator.min_schema_version, semver::Version::new(1, 0, 0));
    }

    #[test]
    fn test_valid_identifier() {
        assert!(is_valid_identifier("valid_name"));
        assert!(is_valid_identifier("_underscore"));
        assert!(is_valid_identifier("CamelCase"));
        assert!(is_valid_identifier("with123numbers"));

        assert!(!is_valid_identifier(""));
        assert!(!is_valid_identifier("123invalid"));
        assert!(!is_valid_identifier("invalid-dash"));
        assert!(!is_valid_identifier("invalid.dot"));
    }

    #[test]
    fn test_validate_empty_id() {
        let validator = PromptValidator::new();
        let ir = PromptIR {
            sections: BTreeMap::new(),
            metadata: PromptMetadata {
                id: "".to_string(),
                version: "1.0.0".to_string(),
                schema_version: "1.0.0".to_string(),
                source_ontology: "test://ontology".to_string(),
                construct_query: "".to_string(),
            },
            variables: BTreeMap::new(),
        };

        assert!(validator.validate(&ir).is_err());
    }

    #[test]
    fn test_validate_invalid_version() {
        let validator = PromptValidator::new();
        let ir = PromptIR {
            sections: BTreeMap::new(),
            metadata: PromptMetadata {
                id: "test".to_string(),
                version: "invalid".to_string(),
                schema_version: "1.0.0".to_string(),
                source_ontology: "test://ontology".to_string(),
                construct_query: "".to_string(),
            },
            variables: BTreeMap::new(),
        };

        assert!(validator.validate(&ir).is_err());
    }

    #[test]
    fn test_validate_valid_ir() {
        let validator = PromptValidator::new();
        let mut ir = PromptIR {
            sections: BTreeMap::new(),
            metadata: PromptMetadata {
                id: "test_prompt".to_string(),
                version: "1.0.0".to_string(),
                schema_version: "1.0.0".to_string(),
                source_ontology: "test://ontology".to_string(),
                construct_query: "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }".to_string(),
            },
            variables: BTreeMap::new(),
        };

        ir.add_section(
            "system".to_string(),
            Section {
                section_type: SectionType::System,
                blocks: vec![ContentBlock {
                    block_type: BlockType::Text,
                    content: "System prompt".to_string(),
                    metadata: BTreeMap::new(),
                }],
                priority: 0,
            },
        );

        ir.add_variable(
            "test_var".to_string(),
            PromptVariable {
                name: "test_var".to_string(),
                var_type: VariableType::String,
                default: Some("default".to_string()),
                description: "Test variable".to_string(),
            },
        );

        assert!(validator.validate(&ir).is_ok());
    }

    #[test]
    fn test_semver_parsing() {
        let v = semver::Version::parse("1.2.3").unwrap();
        assert_eq!(v.major, 1);
        assert_eq!(v.minor, 2);
        assert_eq!(v.patch, 3);

        assert!(semver::Version::parse("invalid").is_err());
        assert!(semver::Version::parse("1.2").is_err());
    }

    #[test]
    fn test_semver_comparison() {
        let v1 = semver::Version::parse("1.0.0").unwrap();
        let v2 = semver::Version::parse("2.0.0").unwrap();
        let v3 = semver::Version::parse("1.5.0").unwrap();

        assert!(v1 < v2);
        assert!(v1 < v3);
        assert!(v3 < v2);
    }
}
