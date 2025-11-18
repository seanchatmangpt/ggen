/// Audit Logging - comprehensive logging for compliance and security
use crate::models::AuditLogEntry;
use async_trait::async_trait;
use chrono::Utc;
use uuid::Uuid;

/// Audit Log Store trait
#[async_trait]
pub trait AuditLogStore: Send + Sync {
    /// Log an action
    async fn log_action(&self, entry: AuditLogEntry) -> Result<AuditLogEntry, String>;

    /// Get audit logs for a user
    async fn get_user_logs(&self, user_id: Uuid, limit: usize) -> Result<Vec<AuditLogEntry>, String>;

    /// Get audit logs for an organization
    async fn get_org_logs(
        &self,
        org_id: Uuid,
        limit: usize,
    ) -> Result<Vec<AuditLogEntry>, String>;

    /// Get audit logs for a resource
    async fn get_resource_logs(
        &self,
        resource_type: &str,
        resource_id: &str,
        limit: usize,
    ) -> Result<Vec<AuditLogEntry>, String>;

    /// Search audit logs by action
    async fn search_by_action(
        &self,
        org_id: Uuid,
        action: &str,
    ) -> Result<Vec<AuditLogEntry>, String>;

    /// Get failed actions
    async fn get_failed_actions(
        &self,
        org_id: Uuid,
        limit: usize,
    ) -> Result<Vec<AuditLogEntry>, String>;
}

/// In-memory audit log store (for testing)
pub struct InMemoryAuditLogStore {
    logs: std::sync::Arc<tokio::sync::RwLock<Vec<AuditLogEntry>>>,
}

impl InMemoryAuditLogStore {
    pub fn new() -> Self {
        Self {
            logs: std::sync::Arc::new(tokio::sync::RwLock::new(Vec::new())),
        }
    }
}

impl Default for InMemoryAuditLogStore {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait::async_trait]
impl AuditLogStore for InMemoryAuditLogStore {
    async fn log_action(&self, entry: AuditLogEntry) -> Result<AuditLogEntry, String> {
        let mut logs = self.logs.write().await;
        logs.push(entry.clone());
        Ok(entry)
    }

    async fn get_user_logs(&self, user_id: Uuid, limit: usize) -> Result<Vec<AuditLogEntry>, String> {
        let logs = self.logs.read().await;
        Ok(logs
            .iter()
            .filter(|l| l.user_id == Some(user_id))
            .rev()
            .take(limit)
            .cloned()
            .collect())
    }

    async fn get_org_logs(
        &self,
        org_id: Uuid,
        limit: usize,
    ) -> Result<Vec<AuditLogEntry>, String> {
        let logs = self.logs.read().await;
        Ok(logs
            .iter()
            .filter(|l| l.organization_id == org_id)
            .rev()
            .take(limit)
            .cloned()
            .collect())
    }

    async fn get_resource_logs(
        &self,
        resource_type: &str,
        resource_id: &str,
        limit: usize,
    ) -> Result<Vec<AuditLogEntry>, String> {
        let logs = self.logs.read().await;
        Ok(logs
            .iter()
            .filter(|l| {
                l.resource_type == resource_type && l.resource_id.as_deref() == Some(resource_id)
            })
            .rev()
            .take(limit)
            .cloned()
            .collect())
    }

    async fn search_by_action(
        &self,
        org_id: Uuid,
        action: &str,
    ) -> Result<Vec<AuditLogEntry>, String> {
        let logs = self.logs.read().await;
        Ok(logs
            .iter()
            .filter(|l| l.organization_id == org_id && l.action == action)
            .rev()
            .cloned()
            .collect())
    }

    async fn get_failed_actions(
        &self,
        org_id: Uuid,
        limit: usize,
    ) -> Result<Vec<AuditLogEntry>, String> {
        let logs = self.logs.read().await;
        Ok(logs
            .iter()
            .filter(|l| l.organization_id == org_id && l.status == "failure")
            .rev()
            .take(limit)
            .cloned()
            .collect())
    }
}

/// Audit Logger - high-level interface for logging
pub struct AuditLogger {
    store: std::sync::Arc<dyn AuditLogStore>,
}

impl AuditLogger {
    pub fn new(store: std::sync::Arc<dyn AuditLogStore>) -> Self {
        Self { store }
    }

    /// Log login
    pub async fn log_login(
        &self,
        user_id: Uuid,
        org_id: Uuid,
        ip_address: Option<String>,
        user_agent: Option<String>,
        success: bool,
    ) -> Result<(), String> {
        let status = if success { "success" } else { "failure" };
        let error_msg = if !success {
            Some("Invalid credentials".to_string())
        } else {
            None
        };

        self.store
            .log_action(AuditLogEntry {
                id: Uuid::new_v4(),
                user_id: Some(user_id),
                organization_id: org_id,
                action: "login".to_string(),
                resource_type: "user".to_string(),
                resource_id: Some(user_id.to_string()),
                changes: None,
                status: status.to_string(),
                error_message: error_msg,
                ip_address,
                user_agent,
                timestamp: Utc::now(),
            })
            .await?;

        Ok(())
    }

    /// Log pack publication
    pub async fn log_pack_publish(
        &self,
        user_id: Uuid,
        org_id: Uuid,
        pack_id: &str,
        version: &str,
    ) -> Result<(), String> {
        self.store
            .log_action(AuditLogEntry {
                id: Uuid::new_v4(),
                user_id: Some(user_id),
                organization_id: org_id,
                action: "publish_pack".to_string(),
                resource_type: "pack".to_string(),
                resource_id: Some(pack_id.to_string()),
                changes: Some(serde_json::json!({ "version": version })),
                status: "success".to_string(),
                error_message: None,
                ip_address: None,
                user_agent: None,
                timestamp: Utc::now(),
            })
            .await?;

        Ok(())
    }

    /// Log role assignment
    pub async fn log_role_assignment(
        &self,
        user_id: Uuid,
        org_id: Uuid,
        target_user_id: Uuid,
        role: &str,
    ) -> Result<(), String> {
        self.store
            .log_action(AuditLogEntry {
                id: Uuid::new_v4(),
                user_id: Some(user_id),
                organization_id: org_id,
                action: "assign_role".to_string(),
                resource_type: "user".to_string(),
                resource_id: Some(target_user_id.to_string()),
                changes: Some(serde_json::json!({ "role": role })),
                status: "success".to_string(),
                error_message: None,
                ip_address: None,
                user_agent: None,
                timestamp: Utc::now(),
            })
            .await?;

        Ok(())
    }

    /// Log permission change
    pub async fn log_permission_change(
        &self,
        user_id: Uuid,
        org_id: Uuid,
        resource: &str,
        action: &str,
        granted: bool,
    ) -> Result<(), String> {
        let action_str = if granted { "grant_permission" } else { "revoke_permission" };

        self.store
            .log_action(AuditLogEntry {
                id: Uuid::new_v4(),
                user_id: Some(user_id),
                organization_id: org_id,
                action: action_str.to_string(),
                resource_type: "permission".to_string(),
                resource_id: Some(format!("{}:{}", resource, action)),
                changes: Some(serde_json::json!({ "granted": granted })),
                status: "success".to_string(),
                error_message: None,
                ip_address: None,
                user_agent: None,
                timestamp: Utc::now(),
            })
            .await?;

        Ok(())
    }

    /// Log OAuth2 client creation
    pub async fn log_oauth2_client_creation(
        &self,
        user_id: Uuid,
        org_id: Uuid,
        client_id: &str,
        client_name: &str,
    ) -> Result<(), String> {
        self.store
            .log_action(AuditLogEntry {
                id: Uuid::new_v4(),
                user_id: Some(user_id),
                organization_id: org_id,
                action: "create_oauth2_client".to_string(),
                resource_type: "oauth2_client".to_string(),
                resource_id: Some(client_id.to_string()),
                changes: Some(serde_json::json!({ "name": client_name })),
                status: "success".to_string(),
                error_message: None,
                ip_address: None,
                user_agent: None,
                timestamp: Utc::now(),
            })
            .await?;

        Ok(())
    }

    /// Log suspicious activity
    pub async fn log_suspicious_activity(
        &self,
        org_id: Uuid,
        activity: &str,
        details: Option<serde_json::Value>,
    ) -> Result<(), String> {
        self.store
            .log_action(AuditLogEntry {
                id: Uuid::new_v4(),
                user_id: None,
                organization_id: org_id,
                action: format!("suspicious_{}", activity),
                resource_type: "security".to_string(),
                resource_id: None,
                changes: details,
                status: "warning".to_string(),
                error_message: None,
                ip_address: None,
                user_agent: None,
                timestamp: Utc::now(),
            })
            .await?;

        Ok(())
    }

    /// Get recent logs for organization
    pub async fn get_org_audit_trail(
        &self,
        org_id: Uuid,
        limit: usize,
    ) -> Result<Vec<AuditLogEntry>, String> {
        self.store.get_org_logs(org_id, limit).await
    }

    /// Get failed login attempts
    pub async fn get_failed_logins(
        &self,
        org_id: Uuid,
    ) -> Result<Vec<AuditLogEntry>, String> {
        self.store.search_by_action(org_id, "login").await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_log_login() {
        let store = std::sync::Arc::new(InMemoryAuditLogStore::new());
        let logger = AuditLogger::new(store.clone());

        let user_id = Uuid::new_v4();
        let org_id = Uuid::new_v4();

        logger
            .log_login(user_id, org_id, None, None, true)
            .await
            .unwrap();

        let logs = logger.get_org_audit_trail(org_id, 10).await.unwrap();
        assert_eq!(logs.len(), 1);
        assert_eq!(logs[0].action, "login");
        assert_eq!(logs[0].status, "success");
    }

    #[tokio::test]
    async fn test_log_pack_publish() {
        let store = std::sync::Arc::new(InMemoryAuditLogStore::new());
        let logger = AuditLogger::new(store.clone());

        let user_id = Uuid::new_v4();
        let org_id = Uuid::new_v4();

        logger
            .log_pack_publish(user_id, org_id, "pack-123", "1.0.0")
            .await
            .unwrap();

        let logs = logger.get_org_audit_trail(org_id, 10).await.unwrap();
        assert_eq!(logs.len(), 1);
        assert_eq!(logs[0].action, "publish_pack");
        assert_eq!(logs[0].resource_id, Some("pack-123".to_string()));
    }
}
