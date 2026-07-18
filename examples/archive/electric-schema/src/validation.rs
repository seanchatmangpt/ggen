use crate::{SchemaVersion, Table};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Schema validation error details
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationError {
    pub error_type: String,
    pub message: String,
    pub severity: ValidationSeverity,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ValidationSeverity {
    Error,
    Warning,
    Info,
}

/// Schema validator with comprehensive checks
pub struct SchemaValidator;

impl SchemaValidator {
    /// Validate a table schema
    pub fn validate_table(table: &Table) -> Vec<ValidationError> {
        let mut errors = Vec::new();

        // Check for primary key
        if !table.has_primary_key() {
            errors.push(ValidationError {
                error_type: "missing_primary_key".to_string(),
                message: format!("Table '{}' does not have a primary key", table.name),
                severity: ValidationSeverity::Warning,
            });
        }

        // Check for columns without types
        for col in &table.columns {
            if col.col_type.is_empty() {
                errors.push(ValidationError {
                    error_type: "invalid_column_type".to_string(),
                    message: format!("Column '{}' has no type specified", col.name),
                    severity: ValidationSeverity::Error,
                });
            }

            // Check for reasonable type names
            let valid_types = vec![
                "UUID", "VARCHAR", "INTEGER", "BIGINT", "SMALLINT", "DECIMAL", "FLOAT",
                "BOOLEAN", "TEXT", "TIMESTAMP", "DATE", "TIME", "JSON", "JSONB",
            ];
            let col_type_upper = col.col_type.to_uppercase();
            let is_valid = valid_types.iter().any(|&t| col_type_upper.contains(t));

            if !is_valid {
                errors.push(ValidationError {
                    error_type: "unknown_type".to_string(),
                    message: format!("Column '{}' has unknown type: {}", col.name, col.col_type),
                    severity: ValidationSeverity::Warning,
                });
            }
        }

        // Check for naming conventions
        for col in &table.columns {
            if col.name.to_uppercase() != col.name {
                if !col.name.chars().all(|c| c.is_alphanumeric() || c == '_') {
                    errors.push(ValidationError {
                        error_type: "naming_convention".to_string(),
                        message: format!("Column '{}' violates naming convention", col.name),
                        severity: ValidationSeverity::Warning,
                    });
                }
            }
        }

        errors
    }

    /// Validate a schema version
    pub fn validate_version(version: &SchemaVersion) -> Vec<ValidationError> {
        let mut errors = Vec::new();

        // Check that there's at least one table
        if version.tables.is_empty() {
            errors.push(ValidationError {
                error_type: "empty_schema".to_string(),
                message: "Schema version has no tables".to_string(),
                severity: ValidationSeverity::Warning,
            });
        }

        // Validate each table
        for table in version.tables.values() {
            let table_errors = Self::validate_table(table);
            errors.extend(table_errors);
        }

        // Validate constraints reference valid columns
        for constraint in &version.constraints {
            for col_name in &constraint.columns {
                let mut found = false;
                for table in version.tables.values() {
                    if table.get_column(col_name).is_some() {
                        found = true;
                        break;
                    }
                }

                if !found {
                    errors.push(ValidationError {
                        error_type: "invalid_constraint".to_string(),
                        message: format!(
                            "Constraint '{}' references unknown column '{}'",
                            constraint.name, col_name
                        ),
                        severity: ValidationSeverity::Error,
                    });
                }
            }
        }

        errors
    }

    /// Check for schema compatibility between versions
    pub fn check_compatibility(
        from: &SchemaVersion,
        to: &SchemaVersion,
    ) -> Vec<ValidationError> {
        let mut errors = Vec::new();

        // Check for dropped tables without migration
        for (table_name, _) in &from.tables {
            if !to.tables.contains_key(table_name) {
                errors.push(ValidationError {
                    error_type: "breaking_change".to_string(),
                    message: format!("Table '{}' was dropped", table_name),
                    severity: ValidationSeverity::Error,
                });
            }
        }

        // Check for dropped columns without migration
        for (table_name, from_table) in &from.tables {
            if let Some(to_table) = to.tables.get(table_name) {
                for from_col in &from_table.columns {
                    if to_table.get_column(&from_col.name).is_none() {
                        errors.push(ValidationError {
                            error_type: "breaking_change".to_string(),
                            message: format!(
                                "Column '{}.{}' was dropped",
                                table_name, from_col.name
                            ),
                            severity: ValidationSeverity::Error,
                        });
                    }
                }
            }
        }

        errors
    }

    /// Validate column names for uniqueness within a table
    pub fn validate_column_uniqueness(table: &Table) -> Result<(), ValidationError> {
        let mut seen = HashSet::new();
        for col in &table.columns {
            if !seen.insert(&col.name) {
                return Err(ValidationError {
                    error_type: "duplicate_column".to_string(),
                    message: format!("Duplicate column name: '{}'", col.name),
                    severity: ValidationSeverity::Error,
                });
            }
        }
        Ok(())
    }

    /// Validate table naming
    pub fn validate_table_naming(name: &str) -> Result<(), ValidationError> {
        if name.is_empty() {
            return Err(ValidationError {
                error_type: "empty_name".to_string(),
                message: "Table name cannot be empty".to_string(),
                severity: ValidationSeverity::Error,
            });
        }

        if !name.chars().all(|c| c.is_alphanumeric() || c == '_') {
            return Err(ValidationError {
                error_type: "invalid_name".to_string(),
                message: format!("Invalid table name: '{}'", name),
                severity: ValidationSeverity::Error,
            });
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Column, Table};

    #[test]
    fn test_validate_table_with_primary_key() {
        let mut table = Table::new("users".to_string());
        let col = Column::new("id".to_string(), "UUID".to_string()).primary_key();
        table.add_column(col).unwrap();

        let errors = SchemaValidator::validate_table(&table);
        assert!(errors.is_empty());
    }

    #[test]
    fn test_validate_table_without_primary_key() {
        let mut table = Table::new("users".to_string());
        let col = Column::new("email".to_string(), "VARCHAR".to_string());
        table.add_column(col).unwrap();

        let errors = SchemaValidator::validate_table(&table);
        assert!(!errors.is_empty());
        assert!(errors[0].error_type == "missing_primary_key");
    }

    #[test]
    fn test_validate_column_uniqueness() {
        let mut table = Table::new("users".to_string());
        let col1 = Column::new("id".to_string(), "UUID".to_string());
        let col2 = Column::new("id".to_string(), "VARCHAR".to_string());

        table.add_column(col1).unwrap();
        assert!(SchemaValidator::validate_column_uniqueness(&table).is_ok());

        table.add_column(col2).unwrap_err();
    }

    #[test]
    fn test_validate_table_naming() {
        assert!(SchemaValidator::validate_table_naming("users").is_ok());
        assert!(SchemaValidator::validate_table_naming("user_accounts").is_ok());
        assert!(SchemaValidator::validate_table_naming("").is_err());
        assert!(SchemaValidator::validate_table_naming("user-accounts").is_err());
    }

    #[test]
    fn test_validate_version() {
        let version = SchemaVersion::new(1);
        let errors = SchemaValidator::validate_version(&version);
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_compatibility_check_dropped_table() {
        let mut from = SchemaVersion::new(1);
        let table1 = Table::new("users".to_string());
        from.add_table(table1).unwrap();

        let to = SchemaVersion::new(2);

        let errors = SchemaValidator::check_compatibility(&from, &to);
        assert!(!errors.is_empty());
        assert!(errors[0].error_type == "breaking_change");
    }
}
