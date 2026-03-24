use crate::{AuditEntry, Migration, SchemaVersion};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Schema Version Agent: Manages schema versions and versions history
#[derive(Debug, Clone)]
pub struct SchemaVersionAgent {
    versions: Arc<Mutex<HashMap<u32, SchemaVersion>>>,
    current_version: Arc<Mutex<u32>>,
}

impl SchemaVersionAgent {
    pub fn new() -> Self {
        let mut versions = HashMap::new();
        versions.insert(1, SchemaVersion::new(1));

        Self {
            versions: Arc::new(Mutex::new(versions)),
            current_version: Arc::new(Mutex::new(1)),
        }
    }

    pub fn create_version(&self) -> crate::SchemaResult<u32> {
        let mut versions = self.versions.lock().unwrap();
        let mut current = self.current_version.lock().unwrap();

        *current += 1;
        versions.insert(*current, SchemaVersion::new(*current));
        Ok(*current)
    }

    pub fn get_version(&self, number: u32) -> crate::SchemaResult<SchemaVersion> {
        let versions = self.versions.lock().unwrap();
        versions
            .get(&number)
            .cloned()
            .ok_or_else(|| crate::SchemaError::NotFound(format!("Version {} not found", number)))
    }

    pub fn get_current_version(&self) -> crate::SchemaResult<SchemaVersion> {
        let current = *self.current_version.lock().unwrap();
        self.get_version(current)
    }

    pub fn list_versions(&self) -> Vec<u32> {
        let versions = self.versions.lock().unwrap();
        let mut numbers: Vec<u32> = versions.keys().cloned().collect();
        numbers.sort();
        numbers
    }

    pub fn update_version(&self, version: SchemaVersion) -> crate::SchemaResult<()> {
        let mut versions = self.versions.lock().unwrap();
        if !versions.contains_key(&version.number) {
            return Err(crate::SchemaError::NotFound(format!(
                "Version {} not found",
                version.number
            )));
        }
        versions.insert(version.number, version);
        Ok(())
    }
}

impl Default for SchemaVersionAgent {
    fn default() -> Self {
        Self::new()
    }
}

/// Validation Agent: Validates schemas against rules and constraints
#[derive(Debug, Clone)]
pub struct ValidationAgent {
    validation_rules: Arc<Mutex<Vec<ValidationRule>>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationRule {
    pub name: String,
    pub description: String,
    pub enabled: bool,
}

impl ValidationAgent {
    pub fn new() -> Self {
        let mut rules = Vec::new();
        rules.push(ValidationRule {
            name: "has_primary_key".to_string(),
            description: "All tables must have a primary key".to_string(),
            enabled: true,
        });
        rules.push(ValidationRule {
            name: "column_naming".to_string(),
            description: "Column names must follow naming conventions".to_string(),
            enabled: true,
        });
        rules.push(ValidationRule {
            name: "valid_types".to_string(),
            description: "Column types must be valid SQL types".to_string(),
            enabled: true,
        });

        Self {
            validation_rules: Arc::new(Mutex::new(rules)),
        }
    }

    pub fn validate_version(&self, version: &SchemaVersion) -> ValidateResult {
        use crate::validation::SchemaValidator;

        let errors = SchemaValidator::validate_version(version);
        ValidateResult {
            valid: errors.is_empty(),
            errors: errors.iter().map(|e| e.message.clone()).collect(),
            warnings: errors
                .iter()
                .filter(|e| e.severity == crate::validation::ValidationSeverity::Warning)
                .map(|e| e.message.clone())
                .collect(),
        }
    }

    pub fn validate_table(&self, table: &crate::Table) -> ValidateResult {
        use crate::validation::SchemaValidator;

        let errors = SchemaValidator::validate_table(table);
        ValidateResult {
            valid: errors.is_empty(),
            errors: errors.iter().map(|e| e.message.clone()).collect(),
            warnings: errors
                .iter()
                .filter(|e| e.severity == crate::validation::ValidationSeverity::Warning)
                .map(|e| e.message.clone())
                .collect(),
        }
    }

    pub fn add_rule(&self, rule: ValidationRule) {
        let mut rules = self.validation_rules.lock().unwrap();
        rules.push(rule);
    }

    pub fn list_rules(&self) -> Vec<ValidationRule> {
        let rules = self.validation_rules.lock().unwrap();
        rules.clone()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidateResult {
    pub valid: bool,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

impl Default for ValidationAgent {
    fn default() -> Self {
        Self::new()
    }
}

/// Migration Agent: Handles schema migrations
#[derive(Debug, Clone)]
pub struct MigrationAgent {
    migrations: Arc<Mutex<HashMap<String, Migration>>>,
}

impl MigrationAgent {
    pub fn new() -> Self {
        Self {
            migrations: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn create_migration(&self, from_version: u32, to_version: u32) -> crate::SchemaResult<String> {
        let migration = Migration::new(from_version, to_version);
        let id = migration.id.clone();

        let mut migrations = self.migrations.lock().unwrap();
        migrations.insert(id.clone(), migration);
        Ok(id)
    }

    pub fn get_migration(&self, id: &str) -> crate::SchemaResult<Migration> {
        let migrations = self.migrations.lock().unwrap();
        migrations
            .get(id)
            .cloned()
            .ok_or_else(|| crate::SchemaError::NotFound(format!("Migration {} not found", id)))
    }

    pub fn list_migrations(&self) -> Vec<Migration> {
        let migrations = self.migrations.lock().unwrap();
        migrations.values().cloned().collect()
    }

    pub fn update_migration(&self, migration: Migration) -> crate::SchemaResult<()> {
        let mut migrations = self.migrations.lock().unwrap();
        if !migrations.contains_key(&migration.id) {
            return Err(crate::SchemaError::NotFound(format!(
                "Migration {} not found",
                migration.id
            )));
        }
        migrations.insert(migration.id.clone(), migration);
        Ok(())
    }

    pub fn execute_migration(&self, id: &str) -> crate::SchemaResult<ExecutionResult> {
        let mut migrations = self.migrations.lock().unwrap();
        let migration = migrations
            .get_mut(id)
            .ok_or_else(|| crate::SchemaError::NotFound(format!("Migration {} not found", id)))?
            .clone();

        let mut updated = migration;
        updated.mark_in_progress();
        updated.mark_completed();

        migrations.insert(id.to_string(), updated.clone());

        Ok(ExecutionResult {
            success: true,
            migration_id: id.to_string(),
            message: format!(
                "Migration from v{} to v{} completed successfully",
                updated.from_version, updated.to_version
            ),
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionResult {
    pub success: bool,
    pub migration_id: String,
    pub message: String,
}

impl Default for MigrationAgent {
    fn default() -> Self {
        Self::new()
    }
}

/// Audit Agent: Tracks all schema changes
#[derive(Debug, Clone)]
pub struct AuditAgent {
    entries: Arc<Mutex<Vec<AuditEntry>>>,
}

impl AuditAgent {
    pub fn new() -> Self {
        Self {
            entries: Arc::new(Mutex::new(Vec::new())),
        }
    }

    pub fn log_action(&self, action: String, details: String) -> crate::SchemaResult<String> {
        let entry = AuditEntry::new(action, details);
        let id = entry.id.clone();

        let mut entries = self.entries.lock().unwrap();
        entries.push(entry);
        Ok(id)
    }

    pub fn log_action_with_user(
        &self,
        action: String,
        details: String,
        user: String,
    ) -> crate::SchemaResult<String> {
        let entry = AuditEntry::new(action, details).with_user(user);
        let id = entry.id.clone();

        let mut entries = self.entries.lock().unwrap();
        entries.push(entry);
        Ok(id)
    }

    pub fn get_entry(&self, id: &str) -> crate::SchemaResult<AuditEntry> {
        let entries = self.entries.lock().unwrap();
        entries
            .iter()
            .find(|e| e.id == id)
            .cloned()
            .ok_or_else(|| crate::SchemaError::NotFound(format!("Audit entry {} not found", id)))
    }

    pub fn list_entries(&self) -> Vec<AuditEntry> {
        let entries = self.entries.lock().unwrap();
        entries.clone()
    }

    pub fn list_entries_by_action(&self, action: &str) -> Vec<AuditEntry> {
        let entries = self.entries.lock().unwrap();
        entries
            .iter()
            .filter(|e| e.action == action)
            .cloned()
            .collect()
    }

    pub fn list_entries_by_user(&self, user: &str) -> Vec<AuditEntry> {
        let entries = self.entries.lock().unwrap();
        entries
            .iter()
            .filter(|e| e.user.as_deref() == Some(user))
            .cloned()
            .collect()
    }
}

impl Default for AuditAgent {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_agent_create_version() {
        let agent = SchemaVersionAgent::new();
        assert!(agent.create_version().is_ok());
        assert_eq!(agent.list_versions().len(), 2);
    }

    #[test]
    fn test_version_agent_get_version() {
        let agent = SchemaVersionAgent::new();
        let version = agent.get_version(1);
        assert!(version.is_ok());
        assert_eq!(version.unwrap().number, 1);
    }

    #[test]
    fn test_version_agent_get_nonexistent() {
        let agent = SchemaVersionAgent::new();
        assert!(agent.get_version(999).is_err());
    }

    #[test]
    fn test_validation_agent_create() {
        let agent = ValidationAgent::new();
        let rules = agent.list_rules();
        assert!(!rules.is_empty());
    }

    #[test]
    fn test_validation_agent_add_rule() {
        let agent = ValidationAgent::new();
        let initial_count = agent.list_rules().len();

        let rule = ValidationRule {
            name: "custom_rule".to_string(),
            description: "Custom validation rule".to_string(),
            enabled: true,
        };
        agent.add_rule(rule);

        assert_eq!(agent.list_rules().len(), initial_count + 1);
    }

    #[test]
    fn test_migration_agent_create() {
        let agent = MigrationAgent::new();
        let result = agent.create_migration(1, 2);
        assert!(result.is_ok());
    }

    #[test]
    fn test_migration_agent_list() {
        let agent = MigrationAgent::new();
        agent.create_migration(1, 2).unwrap();
        agent.create_migration(2, 3).unwrap();

        let migrations = agent.list_migrations();
        assert_eq!(migrations.len(), 2);
    }

    #[test]
    fn test_migration_agent_execute() {
        let agent = MigrationAgent::new();
        let id = agent.create_migration(1, 2).unwrap();
        let result = agent.execute_migration(&id);
        assert!(result.is_ok());
        assert!(result.unwrap().success);
    }

    #[test]
    fn test_audit_agent_log() {
        let agent = AuditAgent::new();
        let result = agent.log_action("CREATE_TABLE".to_string(), "Created users table".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_audit_agent_log_with_user() {
        let agent = AuditAgent::new();
        let result = agent.log_action_with_user(
            "CREATE_TABLE".to_string(),
            "Created users table".to_string(),
            "admin".to_string(),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_audit_agent_list() {
        let agent = AuditAgent::new();
        agent.log_action("CREATE_TABLE".to_string(), "Table 1".to_string()).unwrap();
        agent.log_action("CREATE_TABLE".to_string(), "Table 2".to_string()).unwrap();

        let entries = agent.list_entries();
        assert_eq!(entries.len(), 2);
    }

    #[test]
    fn test_audit_agent_list_by_action() {
        let agent = AuditAgent::new();
        agent.log_action("CREATE_TABLE".to_string(), "Table 1".to_string()).unwrap();
        agent.log_action("DROP_TABLE".to_string(), "Table 2".to_string()).unwrap();

        let entries = agent.list_entries_by_action("CREATE_TABLE");
        assert_eq!(entries.len(), 1);
    }

    #[test]
    fn test_audit_agent_list_by_user() {
        let agent = AuditAgent::new();
        agent
            .log_action_with_user("CREATE_TABLE".to_string(), "Table 1".to_string(), "alice".to_string())
            .unwrap();
        agent
            .log_action_with_user("CREATE_TABLE".to_string(), "Table 2".to_string(), "bob".to_string())
            .unwrap();

        let entries = agent.list_entries_by_user("alice");
        assert_eq!(entries.len(), 1);
    }
}
