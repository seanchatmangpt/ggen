use crate::{Api, ApiError, CompatibilityInfo, DeprecationNotice, Migration, MigrationStatus};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, Mutex};
use std::collections::HashMap;

/// Version Manager Agent: Manages API versions and version history
#[derive(Debug, Clone)]
pub struct VersionManagerAgent {
    versions: Arc<Mutex<HashMap<u32, Api>>>,
    current_version: Arc<Mutex<u32>>,
}

impl VersionManagerAgent {
    pub fn new(initial_api: Api) -> Self {
        let mut versions = HashMap::new();
        let version_num = initial_api.version;
        versions.insert(version_num, initial_api);

        Self {
            versions: Arc::new(Mutex::new(versions)),
            current_version: Arc::new(Mutex::new(version_num)),
        }
    }

    pub fn create_version(&self, api: Api) -> crate::ApiResult<u32> {
        let mut versions = self.versions.lock().unwrap();
        let mut current = self.current_version.lock().unwrap();

        if versions.contains_key(&api.version) {
            return Err(ApiError::Conflict(format!(
                "Version {} already exists",
                api.version
            )));
        }

        let version_num = api.version;
        versions.insert(version_num, api);
        *current = version_num;
        Ok(version_num)
    }

    pub fn get_version(&self, number: u32) -> crate::ApiResult<Api> {
        let versions = self.versions.lock().unwrap();
        versions
            .get(&number)
            .cloned()
            .ok_or_else(|| ApiError::NotFound(format!("Version {} not found", number)))
    }

    pub fn get_current_version(&self) -> crate::ApiResult<Api> {
        let current = *self.current_version.lock().unwrap();
        self.get_version(current)
    }

    pub fn list_versions(&self) -> Vec<u32> {
        let versions = self.versions.lock().unwrap();
        let mut numbers: Vec<u32> = versions.keys().cloned().collect();
        numbers.sort();
        numbers
    }
}

impl Default for VersionManagerAgent {
    fn default() -> Self {
        Self::new(Api::default())
    }
}

/// Deprecation Agent: Manages endpoint deprecation and sunset
#[derive(Debug, Clone)]
pub struct DeprecationAgent {
    notices: Arc<Mutex<HashMap<String, DeprecationNotice>>>,
}

impl DeprecationAgent {
    pub fn new() -> Self {
        Self {
            notices: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn deprecate_endpoint(
        &self,
        endpoint: String,
        message: String,
        sunset_days: i64,
    ) -> crate::ApiResult<String> {
        let notice = DeprecationNotice::new(endpoint.clone(), message, sunset_days);
        let id = notice.endpoint.clone();

        let mut notices = self.notices.lock().unwrap();
        if notices.contains_key(&id) {
            return Err(ApiError::DeprecationError(format!(
                "Endpoint {} already has deprecation notice",
                id
            )));
        }
        notices.insert(id.clone(), notice);
        Ok(id)
    }

    pub fn get_notice(&self, endpoint: &str) -> crate::ApiResult<DeprecationNotice> {
        let notices = self.notices.lock().unwrap();
        notices
            .get(endpoint)
            .cloned()
            .ok_or_else(|| ApiError::NotFound(format!("No deprecation notice for {}", endpoint)))
    }

    pub fn list_deprecated(&self) -> Vec<DeprecationNotice> {
        let notices = self.notices.lock().unwrap();
        notices.values().cloned().collect()
    }

    pub fn list_sunset_endpoints(&self) -> Vec<DeprecationNotice> {
        let notices = self.notices.lock().unwrap();
        notices
            .values()
            .filter(|n| n.is_sunset())
            .cloned()
            .collect()
    }
}

impl Default for DeprecationAgent {
    fn default() -> Self {
        Self::new()
    }
}

/// Compatibility Agent: Checks API compatibility between versions
#[derive(Debug, Clone)]
pub struct CompatibilityAgent {
    compatibility_cache: Arc<Mutex<HashMap<(u32, u32), CompatibilityInfo>>>,
}

impl CompatibilityAgent {
    pub fn new() -> Self {
        Self {
            compatibility_cache: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn check_compatibility(&self, from_api: &Api, to_api: &Api) -> CompatibilityInfo {
        let cache_key = (from_api.version, to_api.version);
        let mut cache = self.compatibility_cache.lock().unwrap();

        if let Some(cached) = cache.get(&cache_key) {
            return cached.clone();
        }

        let mut info = CompatibilityInfo::new();

        // Check for removed endpoints (breaking change)
        for from_endpoint in from_api.list_endpoints() {
            if to_api
                .get_endpoint(&from_endpoint.path, &from_endpoint.method)
                .is_none()
            {
                info = info.with_breaking_change(format!(
                    "Removed endpoint: {} {}",
                    from_endpoint.method, from_endpoint.path
                ));
            }
        }

        // Check for new endpoints
        for to_endpoint in to_api.list_endpoints() {
            if from_api
                .get_endpoint(&to_endpoint.path, &to_endpoint.method)
                .is_none()
            {
                info = info.add_new(format!(
                    "Added endpoint: {} {}",
                    to_endpoint.method, to_endpoint.path
                ));
            }
        }

        cache.insert(cache_key, info.clone());
        info
    }

    pub fn is_compatible(&self, from_api: &Api, to_api: &Api) -> bool {
        self.check_compatibility(from_api, to_api).is_backward_compatible
    }
}

impl Default for CompatibilityAgent {
    fn default() -> Self {
        Self::new()
    }
}

/// Migration Agent: Handles API migrations
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

    pub fn create_migration(&self, from_version: u32, to_version: u32) -> crate::ApiResult<String> {
        let migration = Migration::new(from_version, to_version);
        let id = migration.id.clone();

        let mut migrations = self.migrations.lock().unwrap();
        migrations.insert(id.clone(), migration);
        Ok(id)
    }

    pub fn get_migration(&self, id: &str) -> crate::ApiResult<Migration> {
        let migrations = self.migrations.lock().unwrap();
        migrations
            .get(id)
            .cloned()
            .ok_or_else(|| ApiError::NotFound(format!("Migration {} not found", id)))
    }

    pub fn list_migrations(&self) -> Vec<Migration> {
        let migrations = self.migrations.lock().unwrap();
        migrations.values().cloned().collect()
    }

    pub fn list_migrations_by_status(&self, status: MigrationStatus) -> Vec<Migration> {
        let migrations = self.migrations.lock().unwrap();
        migrations
            .values()
            .filter(|m| m.status == status)
            .cloned()
            .collect()
    }

    pub fn execute_migration(&self, id: &str) -> crate::ApiResult<ExecutionResult> {
        let mut migrations = self.migrations.lock().unwrap();
        let migration = migrations
            .get_mut(id)
            .ok_or_else(|| ApiError::NotFound(format!("Migration {} not found", id)))?
            .clone();

        let mut updated = migration;
        updated.mark_in_progress();
        updated.mark_completed();

        migrations.insert(id.to_string(), updated.clone());

        Ok(ExecutionResult {
            success: true,
            migration_id: id.to_string(),
            from_version: updated.from_version,
            to_version: updated.to_version,
            message: format!(
                "Successfully migrated from v{} to v{}",
                updated.from_version, updated.to_version
            ),
        })
    }

    pub fn rollback_migration(&self, id: &str) -> crate::ApiResult<ExecutionResult> {
        let mut migrations = self.migrations.lock().unwrap();
        let migration = migrations
            .get_mut(id)
            .ok_or_else(|| ApiError::NotFound(format!("Migration {} not found", id)))?
            .clone();

        let mut updated = migration;
        updated.status = MigrationStatus::Rolled;

        migrations.insert(id.to_string(), updated.clone());

        Ok(ExecutionResult {
            success: true,
            migration_id: id.to_string(),
            from_version: updated.from_version,
            to_version: updated.to_version,
            message: format!(
                "Rolled back migration from v{} to v{}",
                updated.from_version, updated.to_version
            ),
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionResult {
    pub success: bool,
    pub migration_id: String,
    pub from_version: u32,
    pub to_version: u32,
    pub message: String,
}

impl Default for MigrationAgent {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Endpoint;

    #[test]
    fn test_version_manager_create() {
        let agent = VersionManagerAgent::new(Api::new("test".to_string(), "/api/v1".to_string()));
        assert_eq!(agent.list_versions().len(), 1);
    }

    #[test]
    fn test_version_manager_get_current() {
        let agent = VersionManagerAgent::new(Api::new("test".to_string(), "/api/v1".to_string()));
        let version = agent.get_current_version();
        assert!(version.is_ok());
    }

    #[test]
    fn test_deprecation_agent_deprecate() {
        let agent = DeprecationAgent::new();
        let result = agent.deprecate_endpoint(
            "/old-endpoint".to_string(),
            "Use /new-endpoint".to_string(),
            30,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_deprecation_agent_get_notice() {
        let agent = DeprecationAgent::new();
        agent
            .deprecate_endpoint(
                "/old-endpoint".to_string(),
                "Use /new-endpoint".to_string(),
                30,
            )
            .unwrap();

        let notice = agent.get_notice("/old-endpoint");
        assert!(notice.is_ok());
    }

    #[test]
    fn test_deprecation_agent_list() {
        let agent = DeprecationAgent::new();
        agent
            .deprecate_endpoint(
                "/old-endpoint-1".to_string(),
                "Deprecated".to_string(),
                30,
            )
            .unwrap();
        agent
            .deprecate_endpoint(
                "/old-endpoint-2".to_string(),
                "Deprecated".to_string(),
                30,
            )
            .unwrap();

        let notices = agent.list_deprecated();
        assert_eq!(notices.len(), 2);
    }

    #[test]
    fn test_compatibility_agent_compatible() {
        let agent = CompatibilityAgent::new();
        let from_api = Api::new("API".to_string(), "/api/v1".to_string());
        let mut to_api = Api::new("API".to_string(), "/api/v1".to_string());

        to_api
            .add_endpoint(Endpoint::new(
                "/users".to_string(),
                "GET".to_string(),
                "list".to_string(),
            ))
            .unwrap();

        let info = agent.check_compatibility(&from_api, &to_api);
        assert!(info.is_backward_compatible);
    }

    #[test]
    fn test_compatibility_agent_breaking() {
        let agent = CompatibilityAgent::new();
        let mut from_api = Api::new("API".to_string(), "/api/v1".to_string());
        let to_api = Api::new("API".to_string(), "/api/v1".to_string());

        from_api
            .add_endpoint(Endpoint::new(
                "/users".to_string(),
                "GET".to_string(),
                "list".to_string(),
            ))
            .unwrap();

        let info = agent.check_compatibility(&from_api, &to_api);
        assert!(!info.is_backward_compatible);
    }

    #[test]
    fn test_migration_agent_create() {
        let agent = MigrationAgent::new();
        let result = agent.create_migration(1, 2);
        assert!(result.is_ok());
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
    fn test_migration_agent_list_by_status() {
        let agent = MigrationAgent::new();
        let id = agent.create_migration(1, 2).unwrap();
        agent.execute_migration(&id).unwrap();

        let completed = agent.list_migrations_by_status(MigrationStatus::Completed);
        assert_eq!(completed.len(), 1);
    }

    #[test]
    fn test_migration_agent_rollback() {
        let agent = MigrationAgent::new();
        let id = agent.create_migration(1, 2).unwrap();
        agent.execute_migration(&id).unwrap();
        let result = agent.rollback_migration(&id);
        assert!(result.is_ok());
    }
}
