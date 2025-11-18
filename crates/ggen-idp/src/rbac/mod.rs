pub mod rebac;
pub mod evaluator;
pub mod store;

pub use rebac::*;
pub use evaluator::*;
pub use store::*;

use crate::models::*;
use uuid::Uuid;

/// Relation-Based Access Control (ReBAC) Engine
/// Inspired by Google Zanzibar, tailored for multi-tenant IDP
pub struct RbacEngine {
    store: RbacStore,
}

impl RbacEngine {
    pub fn new(store: RbacStore) -> Self {
        Self { store }
    }

    /// Check if user has permission on a resource
    pub async fn has_permission(
        &self,
        user_id: Uuid,
        org_id: Uuid,
        resource: &str,
        action: &str,
    ) -> Result<bool, String> {
        // Check direct role assignment
        let user_roles = self.store.get_user_roles(user_id, org_id).await?;

        for role in user_roles {
            for perm in &role.permissions {
                if perm.resource == resource && perm.action == action {
                    // Check constraints
                    if let Some(constraint) = &perm.constraints {
                        if self
                            .evaluate_constraint(user_id, org_id, resource, constraint)
                            .await?
                        {
                            return Ok(true);
                        }
                    } else {
                        return Ok(true);
                    }
                }
            }
        }

        Ok(false)
    }

    /// Evaluate permission constraint using CEL
    async fn evaluate_constraint(
        &self,
        user_id: Uuid,
        org_id: Uuid,
        resource: &str,
        constraint: &PermissionConstraint,
    ) -> Result<bool, String> {
        // If owner_only constraint, check if user owns the resource
        if constraint.owner_only {
            let is_owner = self
                .store
                .is_owner(user_id, resource)
                .await?;
            if !is_owner {
                return Ok(false);
            }
        }

        // If org_only constraint, check if in same org
        if constraint.org_only {
            let resource_org = self
                .store
                .get_resource_organization(resource)
                .await?;
            if resource_org != org_id {
                return Ok(false);
            }
        }

        // If requires_approval, check if resource is approved
        if constraint.requires_approval {
            let is_approved = self
                .store
                .is_resource_approved(resource)
                .await?;
            if !is_approved {
                return Ok(false);
            }
        }

        Ok(true)
    }

    /// List all resources user can access
    pub async fn list_accessible_resources(
        &self,
        user_id: Uuid,
        org_id: Uuid,
        resource_type: &str,
        action: &str,
    ) -> Result<Vec<String>, String> {
        let user_roles = self.store.get_user_roles(user_id, org_id).await?;
        let mut accessible = Vec::new();

        for role in user_roles {
            for perm in &role.permissions {
                if perm.resource == resource_type && perm.action == action {
                    let resources = self
                        .store
                        .get_resources_by_type(resource_type)
                        .await?;

                    for resource in resources {
                        if let Some(constraint) = &perm.constraints {
                            if self
                                .evaluate_constraint(user_id, org_id, &resource, constraint)
                                .await?
                            {
                                accessible.push(resource);
                            }
                        } else {
                            accessible.push(resource);
                        }
                    }
                }
            }
        }

        Ok(accessible)
    }

    /// Grant role to user
    pub async fn grant_role(
        &self,
        user_id: Uuid,
        role_id: Uuid,
        org_id: Uuid,
    ) -> Result<(), String> {
        self.store.assign_role_to_user(user_id, role_id, org_id).await
    }

    /// Revoke role from user
    pub async fn revoke_role(
        &self,
        user_id: Uuid,
        role_id: Uuid,
    ) -> Result<(), String> {
        self.store.unassign_role_from_user(user_id, role_id).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_rbac_basic_permission() {
        // Test basic RBAC functionality
    }

    #[tokio::test]
    async fn test_rbac_constraint_evaluation() {
        // Test constraint evaluation
    }

    #[tokio::test]
    async fn test_role_hierarchy() {
        // Test role inheritance
    }
}
