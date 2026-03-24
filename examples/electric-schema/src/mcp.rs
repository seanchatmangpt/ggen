use crate::{Migration, SchemaVersion, Table};
use serde::{Deserialize, Serialize};

/// MCP Tool: Validate Schema
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationTool;

impl ValidationTool {
    pub fn tool_name() -> &'static str {
        "validate-schema"
    }

    pub fn tool_description() -> &'static str {
        "Validates a schema version for consistency and best practices"
    }

    pub fn validate_schema(version: &SchemaVersion) -> ValidationToolResponse {
        use crate::validation::SchemaValidator;

        let errors = SchemaValidator::validate_version(version);

        ValidationToolResponse {
            valid: errors.is_empty(),
            error_count: errors.iter().filter(|e| e.severity == crate::validation::ValidationSeverity::Error).count(),
            warning_count: errors.iter().filter(|e| e.severity == crate::validation::ValidationSeverity::Warning).count(),
            errors,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationToolResponse {
    pub valid: bool,
    pub error_count: usize,
    pub warning_count: usize,
    pub errors: Vec<crate::ValidationError>,
}

/// MCP Tool: Migrate Schema
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationTool;

impl MigrationTool {
    pub fn tool_name() -> &'static str {
        "migrate-schema"
    }

    pub fn tool_description() -> &'static str {
        "Execute a schema migration from one version to another"
    }

    pub fn execute_migration(
        migration: &mut Migration,
        from: &SchemaVersion,
        to: &SchemaVersion,
    ) -> MigrationToolResponse {
        migration.mark_in_progress();

        // Validate that migration is logically sound
        let validation_errors = crate::validation::SchemaValidator::check_compatibility(from, to);

        if !validation_errors.is_empty() {
            migration.mark_failed();
            return MigrationToolResponse {
                success: false,
                message: "Migration validation failed".to_string(),
                applied_changes: 0,
                errors: Some(
                    validation_errors
                        .iter()
                        .map(|e| e.message.clone())
                        .collect(),
                ),
            };
        }

        migration.mark_completed();
        MigrationToolResponse {
            success: true,
            message: format!(
                "Successfully migrated from version {} to {}",
                from.number, to.number
            ),
            applied_changes: migration.changes.len(),
            errors: None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationToolResponse {
    pub success: bool,
    pub message: String,
    pub applied_changes: usize,
    pub errors: Option<Vec<String>>,
}

/// MCP Tool: Get Schema Version
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionTool;

impl VersionTool {
    pub fn tool_name() -> &'static str {
        "get-schema-version"
    }

    pub fn tool_description() -> &'static str {
        "Retrieve information about a specific schema version"
    }

    pub fn get_version_info(version: &SchemaVersion) -> VersionToolResponse {
        let table_names: Vec<String> = version.tables.keys().cloned().collect();
        let total_columns: usize = version.tables.values().map(|t| t.columns.len()).sum();

        VersionToolResponse {
            version_number: version.number,
            table_count: version.tables.len(),
            total_columns,
            table_names,
            constraint_count: version.constraints.len(),
            created_at: version.created_at.to_rfc3339(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionToolResponse {
    pub version_number: u32,
    pub table_count: usize,
    pub total_columns: usize,
    pub table_names: Vec<String>,
    pub constraint_count: usize,
    pub created_at: String,
}

/// MCP Tool: Schema Diff
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiffTool;

impl DiffTool {
    pub fn tool_name() -> &'static str {
        "schema-diff"
    }

    pub fn tool_description() -> &'static str {
        "Generate a diff between two schema versions"
    }

    pub fn compute_diff(from: &SchemaVersion, to: &SchemaVersion) -> SchemaDiff {
        let mut added_tables = Vec::new();
        let mut removed_tables = Vec::new();
        let mut modified_tables = Vec::new();

        // Find added and modified tables
        for (name, to_table) in &to.tables {
            if let Some(from_table) = from.tables.get(name) {
                // Check if modified
                if Self::tables_differ(from_table, to_table) {
                    modified_tables.push(TableDiff {
                        name: name.clone(),
                        added_columns: Self::find_added_columns(from_table, to_table),
                        removed_columns: Self::find_removed_columns(from_table, to_table),
                    });
                }
            } else {
                added_tables.push(name.clone());
            }
        }

        // Find removed tables
        for name in from.tables.keys() {
            if !to.tables.contains_key(name) {
                removed_tables.push(name.clone());
            }
        }

        SchemaDiff {
            from_version: from.number,
            to_version: to.number,
            added_tables,
            removed_tables,
            modified_tables,
        }
    }

    fn tables_differ(from: &Table, to: &Table) -> bool {
        if from.columns.len() != to.columns.len() {
            return true;
        }

        for (i, from_col) in from.columns.iter().enumerate() {
            if let Some(to_col) = to.columns.get(i) {
                if from_col != to_col {
                    return true;
                }
            } else {
                return true;
            }
        }

        false
    }

    fn find_added_columns(from: &Table, to: &Table) -> Vec<String> {
        to.columns
            .iter()
            .filter(|to_col| from.columns.iter().all(|from_col| from_col.name != to_col.name))
            .map(|col| col.name.clone())
            .collect()
    }

    fn find_removed_columns(from: &Table, to: &Table) -> Vec<String> {
        from.columns
            .iter()
            .filter(|from_col| to.columns.iter().all(|to_col| to_col.name != from_col.name))
            .map(|col| col.name.clone())
            .collect()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SchemaDiff {
    pub from_version: u32,
    pub to_version: u32,
    pub added_tables: Vec<String>,
    pub removed_tables: Vec<String>,
    pub modified_tables: Vec<TableDiff>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TableDiff {
    pub name: String,
    pub added_columns: Vec<String>,
    pub removed_columns: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Column, SchemaVersion, Table};

    #[test]
    fn test_validation_tool_valid_schema() {
        let mut version = SchemaVersion::new(1);
        let mut table = Table::new("users".to_string());
        let col = Column::new("id".to_string(), "UUID".to_string()).primary_key();
        table.add_column(col).unwrap();
        version.add_table(table).unwrap();

        let response = ValidationTool::validate_schema(&version);
        assert!(response.valid);
        assert_eq!(response.error_count, 0);
    }

    #[test]
    fn test_version_tool_get_info() {
        let mut version = SchemaVersion::new(1);
        let mut table = Table::new("users".to_string());
        table
            .add_column(Column::new("id".to_string(), "UUID".to_string()))
            .unwrap();
        table
            .add_column(Column::new("email".to_string(), "VARCHAR".to_string()))
            .unwrap();
        version.add_table(table).unwrap();

        let response = VersionTool::get_version_info(&version);
        assert_eq!(response.version_number, 1);
        assert_eq!(response.table_count, 1);
        assert_eq!(response.total_columns, 2);
    }

    #[test]
    fn test_diff_tool_added_table() {
        let from = SchemaVersion::new(1);
        let mut to = SchemaVersion::new(2);
        let table = Table::new("users".to_string());
        to.add_table(table).unwrap();

        let diff = DiffTool::compute_diff(&from, &to);
        assert_eq!(diff.added_tables.len(), 1);
        assert!(diff.removed_tables.is_empty());
    }

    #[test]
    fn test_diff_tool_removed_table() {
        let mut from = SchemaVersion::new(1);
        let table = Table::new("users".to_string());
        from.add_table(table).unwrap();

        let to = SchemaVersion::new(2);

        let diff = DiffTool::compute_diff(&from, &to);
        assert!(diff.added_tables.is_empty());
        assert_eq!(diff.removed_tables.len(), 1);
    }

    #[test]
    fn test_diff_tool_added_column() {
        let mut from = SchemaVersion::new(1);
        let table1 = Table::new("users".to_string());
        from.add_table(table1).unwrap();

        let mut to = SchemaVersion::new(2);
        let mut table2 = Table::new("users".to_string());
        table2
            .add_column(Column::new("id".to_string(), "UUID".to_string()))
            .unwrap();
        to.add_table(table2).unwrap();

        let diff = DiffTool::compute_diff(&from, &to);
        assert_eq!(diff.modified_tables.len(), 1);
        assert_eq!(diff.modified_tables[0].added_columns.len(), 1);
    }
}
