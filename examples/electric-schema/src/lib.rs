use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

mod agents;
mod errors;
mod mcp;
mod validation;

pub use agents::{AuditAgent, MigrationAgent, SchemaVersionAgent, ValidationAgent};
pub use errors::{SchemaError, SchemaResult};
pub use mcp::{DiffTool, MigrationTool, ValidationTool, VersionTool};
pub use validation::{SchemaValidator, ValidationError};

/// Represents a database column with metadata
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Column {
    pub name: String,
    pub col_type: String,
    pub nullable: bool,
    pub primary_key: bool,
    pub indexed: bool,
    pub default: Option<String>,
}

impl Column {
    pub fn new(name: String, col_type: String) -> Self {
        Self {
            name,
            col_type,
            nullable: true,
            primary_key: false,
            indexed: false,
            default: None,
        }
    }

    pub fn required(mut self) -> Self {
        self.nullable = false;
        self
    }

    pub fn primary_key(mut self) -> Self {
        self.primary_key = true;
        self.nullable = false;
        self
    }

    pub fn indexed(mut self) -> Self {
        self.indexed = true;
        self
    }

    pub fn with_default(mut self, default: String) -> Self {
        self.default = Some(default);
        self
    }
}

/// Represents a database table
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Table {
    pub name: String,
    pub columns: Vec<Column>,
    pub version: u32,
}

impl Table {
    pub fn new(name: String) -> Self {
        Self {
            name,
            columns: Vec::new(),
            version: 1,
        }
    }

    pub fn add_column(&mut self, column: Column) -> SchemaResult<()> {
        // Validate column name
        if column.name.is_empty() {
            return Err(SchemaError::ValidationFailed(
                "Column name cannot be empty".to_string(),
            ));
        }

        // Ensure only one primary key
        if column.primary_key && self.columns.iter().any(|c| c.primary_key) {
            return Err(SchemaError::ValidationFailed(
                "Table can only have one primary key".to_string(),
            ));
        }

        // Check for duplicate column names
        if self.columns.iter().any(|c| c.name == column.name) {
            return Err(SchemaError::ValidationFailed(format!(
                "Column '{}' already exists",
                column.name
            )));
        }

        self.columns.push(column);
        Ok(())
    }

    pub fn remove_column(&mut self, name: &str) -> SchemaResult<()> {
        let initial_len = self.columns.len();
        self.columns.retain(|c| c.name != name);

        if self.columns.len() == initial_len {
            return Err(SchemaError::ValidationFailed(format!(
                "Column '{}' not found",
                name
            )));
        }

        Ok(())
    }

    pub fn get_column(&self, name: &str) -> Option<&Column> {
        self.columns.iter().find(|c| c.name == name)
    }

    pub fn has_primary_key(&self) -> bool {
        self.columns.iter().any(|c| c.primary_key)
    }
}

/// Represents a schema constraint
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Constraint {
    pub name: String,
    pub constraint_type: ConstraintType,
    pub columns: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum ConstraintType {
    PrimaryKey,
    ForeignKey,
    Unique,
    Check,
}

/// Represents a schema version
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SchemaVersion {
    pub number: u32,
    pub tables: HashMap<String, Table>,
    pub constraints: Vec<Constraint>,
    pub created_at: DateTime<Utc>,
    pub migration_script: Option<String>,
}

impl SchemaVersion {
    pub fn new(number: u32) -> Self {
        Self {
            number,
            tables: HashMap::new(),
            constraints: Vec::new(),
            created_at: Utc::now(),
            migration_script: None,
        }
    }

    pub fn add_table(&mut self, table: Table) -> SchemaResult<()> {
        if self.tables.contains_key(&table.name) {
            return Err(SchemaError::ValidationFailed(format!(
                "Table '{}' already exists",
                table.name
            )));
        }
        self.tables.insert(table.name.clone(), table);
        Ok(())
    }

    pub fn get_table(&self, name: &str) -> Option<&Table> {
        self.tables.get(name)
    }

    pub fn get_table_mut(&mut self, name: &str) -> Option<&mut Table> {
        self.tables.get_mut(name)
    }

    pub fn add_constraint(&mut self, constraint: Constraint) -> SchemaResult<()> {
        // Validate constraint references valid columns
        for table in self.tables.values() {
            for col in &constraint.columns {
                if table.get_column(col).is_none() {
                    return Err(SchemaError::ValidationFailed(format!(
                        "Column '{}' referenced in constraint not found",
                        col
                    )));
                }
            }
        }
        self.constraints.push(constraint);
        Ok(())
    }
}

/// Migration from one schema version to another
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Migration {
    pub id: String,
    pub from_version: u32,
    pub to_version: u32,
    pub changes: Vec<MigrationChange>,
    pub status: MigrationStatus,
    pub executed_at: Option<DateTime<Utc>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum MigrationStatus {
    Pending,
    InProgress,
    Completed,
    Failed,
    Rolled,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MigrationChange {
    AddTable(String),
    DropTable(String),
    AddColumn { table: String, column: Column },
    DropColumn { table: String, column: String },
    ModifyColumn { table: String, column: Column },
    AddConstraint(Constraint),
    DropConstraint { name: String },
}

impl Migration {
    pub fn new(from_version: u32, to_version: u32) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            from_version,
            to_version,
            changes: Vec::new(),
            status: MigrationStatus::Pending,
            executed_at: None,
        }
    }

    pub fn add_change(&mut self, change: MigrationChange) {
        self.changes.push(change);
    }

    pub fn mark_in_progress(&mut self) {
        self.status = MigrationStatus::InProgress;
    }

    pub fn mark_completed(&mut self) {
        self.status = MigrationStatus::Completed;
        self.executed_at = Some(Utc::now());
    }

    pub fn mark_failed(&mut self) {
        self.status = MigrationStatus::Failed;
    }
}

/// Audit entry for schema changes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditEntry {
    pub id: String,
    pub action: String,
    pub timestamp: DateTime<Utc>,
    pub details: String,
    pub user: Option<String>,
}

impl AuditEntry {
    pub fn new(action: String, details: String) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            action,
            timestamp: Utc::now(),
            details,
            user: None,
        }
    }

    pub fn with_user(mut self, user: String) -> Self {
        self.user = Some(user);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_column_creation() {
        let col = Column::new("id".to_string(), "UUID".to_string());
        assert_eq!(col.name, "id");
        assert_eq!(col.col_type, "UUID");
        assert!(col.nullable);
    }

    #[test]
    fn test_column_required() {
        let col = Column::new("id".to_string(), "UUID".to_string()).required();
        assert!(!col.nullable);
    }

    #[test]
    fn test_column_primary_key() {
        let col = Column::new("id".to_string(), "UUID".to_string()).primary_key();
        assert!(col.primary_key);
        assert!(!col.nullable);
    }

    #[test]
    fn test_column_with_default() {
        let col = Column::new("created_at".to_string(), "TIMESTAMP".to_string())
            .with_default("CURRENT_TIMESTAMP".to_string());
        assert_eq!(col.default, Some("CURRENT_TIMESTAMP".to_string()));
    }

    #[test]
    fn test_table_creation() {
        let table = Table::new("users".to_string());
        assert_eq!(table.name, "users");
        assert_eq!(table.columns.len(), 0);
        assert_eq!(table.version, 1);
    }

    #[test]
    fn test_add_column_to_table() {
        let mut table = Table::new("users".to_string());
        let col = Column::new("id".to_string(), "UUID".to_string());
        assert!(table.add_column(col).is_ok());
        assert_eq!(table.columns.len(), 1);
    }

    #[test]
    fn test_add_duplicate_column_fails() {
        let mut table = Table::new("users".to_string());
        let col = Column::new("id".to_string(), "UUID".to_string());
        assert!(table.add_column(col.clone()).is_ok());
        assert!(table.add_column(col).is_err());
    }

    #[test]
    fn test_multiple_primary_keys_fails() {
        let mut table = Table::new("users".to_string());
        let pk1 = Column::new("id".to_string(), "UUID".to_string()).primary_key();
        let pk2 = Column::new("email".to_string(), "VARCHAR".to_string()).primary_key();
        assert!(table.add_column(pk1).is_ok());
        assert!(table.add_column(pk2).is_err());
    }

    #[test]
    fn test_remove_column() {
        let mut table = Table::new("users".to_string());
        let col = Column::new("id".to_string(), "UUID".to_string());
        assert!(table.add_column(col).is_ok());
        assert!(table.remove_column("id").is_ok());
        assert_eq!(table.columns.len(), 0);
    }

    #[test]
    fn test_remove_nonexistent_column_fails() {
        let mut table = Table::new("users".to_string());
        assert!(table.remove_column("nonexistent").is_err());
    }

    #[test]
    fn test_get_column() {
        let mut table = Table::new("users".to_string());
        let col = Column::new("id".to_string(), "UUID".to_string());
        assert!(table.add_column(col).is_ok());
        assert!(table.get_column("id").is_some());
        assert!(table.get_column("nonexistent").is_none());
    }

    #[test]
    fn test_schema_version_creation() {
        let version = SchemaVersion::new(1);
        assert_eq!(version.number, 1);
        assert!(version.tables.is_empty());
    }

    #[test]
    fn test_add_table_to_version() {
        let mut version = SchemaVersion::new(1);
        let table = Table::new("users".to_string());
        assert!(version.add_table(table).is_ok());
        assert_eq!(version.tables.len(), 1);
    }

    #[test]
    fn test_add_duplicate_table_fails() {
        let mut version = SchemaVersion::new(1);
        let table = Table::new("users".to_string());
        assert!(version.add_table(table.clone()).is_ok());
        assert!(version.add_table(table).is_err());
    }

    #[test]
    fn test_migration_creation() {
        let migration = Migration::new(1, 2);
        assert_eq!(migration.from_version, 1);
        assert_eq!(migration.to_version, 2);
        assert_eq!(migration.status, MigrationStatus::Pending);
    }

    #[test]
    fn test_migration_status_transitions() {
        let mut migration = Migration::new(1, 2);
        assert_eq!(migration.status, MigrationStatus::Pending);

        migration.mark_in_progress();
        assert_eq!(migration.status, MigrationStatus::InProgress);

        migration.mark_completed();
        assert_eq!(migration.status, MigrationStatus::Completed);
        assert!(migration.executed_at.is_some());
    }

    #[test]
    fn test_audit_entry_creation() {
        let entry = AuditEntry::new("CREATE_TABLE".to_string(), "Created users table".to_string());
        assert_eq!(entry.action, "CREATE_TABLE");
        assert_eq!(entry.details, "Created users table");
        assert!(entry.user.is_none());
    }

    #[test]
    fn test_audit_entry_with_user() {
        let entry = AuditEntry::new("CREATE_TABLE".to_string(), "Created users table".to_string())
            .with_user("admin".to_string());
        assert_eq!(entry.user, Some("admin".to_string()));
    }
}
