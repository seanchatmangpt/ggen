//! Policy engine for attribute-based access control (ABAC)

use super::context::AuthorizationRequest;
use super::permission::Permission;
use super::resource::ResourceType;
use crate::RbacResult;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Policy effect
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Effect {
    /// Allow the action
    Allow,
    /// Deny the action
    Deny,
}

/// Policy condition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Condition {
    /// User ID equals value
    UserIdEquals(String),
    /// Resource type equals value
    ResourceTypeEquals(ResourceType),
    /// Permission equals value
    PermissionEquals(Permission),
    /// Custom attribute equals value
    AttributeEquals(String, String),
    /// Time-based condition (before timestamp)
    TimeBefore(i64),
    /// Time-based condition (after timestamp)
    TimeAfter(i64),
    /// AND combinator
    And(Vec<Condition>),
    /// OR combinator
    Or(Vec<Condition>),
    /// NOT combinator
    Not(Box<Condition>),
}

impl Condition {
    /// Evaluate condition against request
    pub fn evaluate(&self, request: &AuthorizationRequest) -> bool {
        match self {
            Condition::UserIdEquals(user_id) => &request.user_id == user_id,
            Condition::ResourceTypeEquals(resource_type) => &request.resource_type == resource_type,
            Condition::PermissionEquals(permission) => request.action == *permission,
            Condition::AttributeEquals(key, value) => request
                .context
                .attributes
                .get(key)
                .map(|v| v == value)
                .unwrap_or(false),
            Condition::TimeBefore(timestamp) => {
                let now = chrono::Utc::now().timestamp();
                now < *timestamp
            }
            Condition::TimeAfter(timestamp) => {
                let now = chrono::Utc::now().timestamp();
                now > *timestamp
            }
            Condition::And(conditions) => conditions.iter().all(|c| c.evaluate(request)),
            Condition::Or(conditions) => conditions.iter().any(|c| c.evaluate(request)),
            Condition::Not(condition) => !condition.evaluate(request),
        }
    }
}

/// Policy rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyRule {
    /// Rule identifier
    pub id: String,
    /// Rule name
    pub name: String,
    /// Rule description
    pub description: String,
    /// Effect (Allow or Deny)
    pub effect: Effect,
    /// Conditions that must be met
    pub conditions: Vec<Condition>,
    /// Priority (higher = evaluated first)
    pub priority: u32,
}

impl PolicyRule {
    /// Create a new policy rule
    pub fn new(
        id: String,
        name: String,
        description: String,
        effect: Effect,
        conditions: Vec<Condition>,
        priority: u32,
    ) -> Self {
        Self {
            id,
            name,
            description,
            effect,
            conditions,
            priority,
        }
    }

    /// Evaluate rule against request
    pub fn evaluate(&self, request: &AuthorizationRequest) -> Option<Effect> {
        // All conditions must be true for rule to apply
        let all_conditions_met = self.conditions.iter().all(|c| c.evaluate(request));

        if all_conditions_met {
            Some(self.effect.clone())
        } else {
            None
        }
    }

    /// Create an allow-all rule
    pub fn allow_all() -> Self {
        Self::new(
            "allow_all".to_string(),
            "Allow All".to_string(),
            "Allow all actions".to_string(),
            Effect::Allow,
            vec![],
            0,
        )
    }

    /// Create a deny-all rule
    pub fn deny_all() -> Self {
        Self::new(
            "deny_all".to_string(),
            "Deny All".to_string(),
            "Deny all actions".to_string(),
            Effect::Deny,
            vec![],
            0,
        )
    }
}

/// Policy composed of multiple rules
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Policy {
    /// Policy identifier
    pub id: String,
    /// Policy name
    pub name: String,
    /// Policy description
    pub description: String,
    /// Rules in this policy
    pub rules: Vec<PolicyRule>,
}

impl Policy {
    /// Create a new policy
    pub fn new(id: String, name: String, description: String, rules: Vec<PolicyRule>) -> Self {
        Self {
            id,
            name,
            description,
            rules,
        }
    }

    /// Evaluate policy against request
    pub fn evaluate(&self, request: &AuthorizationRequest) -> Option<Effect> {
        // Sort rules by priority (higher first)
        let mut sorted_rules = self.rules.clone();
        sorted_rules.sort_by(|a, b| b.priority.cmp(&a.priority));

        // Find first matching rule
        for rule in &sorted_rules {
            if let Some(effect) = rule.evaluate(request) {
                return Some(effect);
            }
        }

        None
    }

    /// Create a permissive default policy (allow all)
    pub fn permissive_default() -> Self {
        Self::new(
            "default_permissive".to_string(),
            "Permissive Default".to_string(),
            "Allow all actions by default".to_string(),
            vec![PolicyRule::allow_all()],
        )
    }

    /// Create a restrictive default policy (deny all)
    pub fn restrictive_default() -> Self {
        Self::new(
            "default_restrictive".to_string(),
            "Restrictive Default".to_string(),
            "Deny all actions by default".to_string(),
            vec![PolicyRule::deny_all()],
        )
    }
}

/// Policy engine for evaluating authorization requests
#[derive(Debug, Clone)]
pub struct PolicyEngine {
    policies: HashMap<String, Policy>,
    default_effect: Effect,
}

impl PolicyEngine {
    /// Create a new policy engine with permissive default
    pub fn new() -> Self {
        Self {
            policies: HashMap::new(),
            default_effect: Effect::Allow,
        }
    }

    /// Create a restrictive policy engine (deny by default)
    pub fn restrictive() -> Self {
        Self {
            policies: HashMap::new(),
            default_effect: Effect::Deny,
        }
    }

    /// Add a policy
    pub fn add_policy(&mut self, policy: Policy) {
        self.policies.insert(policy.id.clone(), policy);
    }

    /// Remove a policy
    pub fn remove_policy(&mut self, policy_id: &str) {
        self.policies.remove(policy_id);
    }

    /// Get a policy
    pub fn get_policy(&self, policy_id: &str) -> Option<&Policy> {
        self.policies.get(policy_id)
    }

    /// Evaluate all policies against request
    pub fn evaluate(&self, request: &AuthorizationRequest) -> RbacResult<AuthorizationDecision> {
        // Collect all policy evaluations
        let mut effects = Vec::new();

        for policy in self.policies.values() {
            if let Some(effect) = policy.evaluate(request) {
                effects.push(effect);
            }
        }

        // Deny takes precedence over allow
        let final_effect = if effects.iter().any(|e| *e == Effect::Deny) {
            Effect::Deny
        } else if effects.iter().any(|e| *e == Effect::Allow) {
            Effect::Allow
        } else {
            self.default_effect.clone()
        };

        Ok(AuthorizationDecision {
            allowed: final_effect == Effect::Allow,
            effect: final_effect,
            applied_policies: self
                .policies
                .keys()
                .filter(|id| {
                    self.policies
                        .get(*id)
                        .and_then(|p| p.evaluate(request))
                        .is_some()
                })
                .cloned()
                .collect(),
        })
    }
}

impl Default for PolicyEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Authorization decision
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuthorizationDecision {
    /// Whether the action is allowed
    pub allowed: bool,
    /// The final effect
    pub effect: Effect,
    /// IDs of policies that were applied
    pub applied_policies: Vec<String>,
}

impl AuthorizationDecision {
    /// Check if action is allowed
    pub fn is_allowed(&self) -> bool {
        self.allowed
    }

    /// Check if action is denied
    pub fn is_denied(&self) -> bool {
        !self.allowed
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rbac::context::AuthorizationContext;
    use crate::rbac::role::Role;

    #[test]
    fn test_condition_user_id_equals() {
        // Arrange
        let condition = Condition::UserIdEquals("user123".to_string());
        let context = AuthorizationContext::new("user123", &[Role::user()], &dummy_resource());
        let request = AuthorizationRequest {
            user_id: "user123".to_string(),
            resource_type: ResourceType::Template,
            resource_id: "res1".to_string(),
            action: Permission::Read,
            context,
        };

        // Act
        let result = condition.evaluate(&request);

        // Assert
        assert!(result);
    }

    #[test]
    fn test_condition_and() {
        // Arrange
        let condition = Condition::And(vec![
            Condition::UserIdEquals("user123".to_string()),
            Condition::ResourceTypeEquals(ResourceType::Template),
        ]);
        let context = AuthorizationContext::new("user123", &[Role::user()], &dummy_resource());
        let request = AuthorizationRequest {
            user_id: "user123".to_string(),
            resource_type: ResourceType::Template,
            resource_id: "res1".to_string(),
            action: Permission::Read,
            context,
        };

        // Act
        let result = condition.evaluate(&request);

        // Assert
        assert!(result);
    }

    #[test]
    fn test_condition_or() {
        // Arrange
        let condition = Condition::Or(vec![
            Condition::UserIdEquals("user456".to_string()),
            Condition::ResourceTypeEquals(ResourceType::Template),
        ]);
        let context = AuthorizationContext::new("user123", &[Role::user()], &dummy_resource());
        let request = AuthorizationRequest {
            user_id: "user123".to_string(),
            resource_type: ResourceType::Template,
            resource_id: "res1".to_string(),
            action: Permission::Read,
            context,
        };

        // Act
        let result = condition.evaluate(&request);

        // Assert
        assert!(result);
    }

    #[test]
    fn test_condition_not() {
        // Arrange
        let condition = Condition::Not(Box::new(Condition::UserIdEquals("user456".to_string())));
        let context = AuthorizationContext::new("user123", &[Role::user()], &dummy_resource());
        let request = AuthorizationRequest {
            user_id: "user123".to_string(),
            resource_type: ResourceType::Template,
            resource_id: "res1".to_string(),
            action: Permission::Read,
            context,
        };

        // Act
        let result = condition.evaluate(&request);

        // Assert
        assert!(result);
    }

    #[test]
    fn test_policy_rule_evaluate_match() {
        // Arrange
        let rule = PolicyRule::new(
            "rule1".to_string(),
            "Allow Read".to_string(),
            "Allow read for user123".to_string(),
            Effect::Allow,
            vec![Condition::UserIdEquals("user123".to_string())],
            10,
        );
        let context = AuthorizationContext::new("user123", &[Role::user()], &dummy_resource());
        let request = AuthorizationRequest {
            user_id: "user123".to_string(),
            resource_type: ResourceType::Template,
            resource_id: "res1".to_string(),
            action: Permission::Read,
            context,
        };

        // Act
        let result = rule.evaluate(&request);

        // Assert
        assert_eq!(result, Some(Effect::Allow));
    }

    #[test]
    fn test_policy_rule_evaluate_no_match() {
        // Arrange
        let rule = PolicyRule::new(
            "rule1".to_string(),
            "Allow Read".to_string(),
            "Allow read for user456".to_string(),
            Effect::Allow,
            vec![Condition::UserIdEquals("user456".to_string())],
            10,
        );
        let context = AuthorizationContext::new("user123", &[Role::user()], &dummy_resource());
        let request = AuthorizationRequest {
            user_id: "user123".to_string(),
            resource_type: ResourceType::Template,
            resource_id: "res1".to_string(),
            action: Permission::Read,
            context,
        };

        // Act
        let result = rule.evaluate(&request);

        // Assert
        assert_eq!(result, None);
    }

    #[test]
    fn test_policy_evaluate() {
        // Arrange
        let policy = Policy::new(
            "policy1".to_string(),
            "Test Policy".to_string(),
            "Test policy".to_string(),
            vec![PolicyRule::new(
                "rule1".to_string(),
                "Allow Read".to_string(),
                "Allow read".to_string(),
                Effect::Allow,
                vec![Condition::PermissionEquals(Permission::Read)],
                10,
            )],
        );
        let context = AuthorizationContext::new("user123", &[Role::user()], &dummy_resource());
        let request = AuthorizationRequest {
            user_id: "user123".to_string(),
            resource_type: ResourceType::Template,
            resource_id: "res1".to_string(),
            action: Permission::Read,
            context,
        };

        // Act
        let result = policy.evaluate(&request);

        // Assert
        assert_eq!(result, Some(Effect::Allow));
    }

    #[test]
    fn test_policy_engine_evaluate_allow() {
        // Arrange
        let mut engine = PolicyEngine::new();
        let policy = Policy::new(
            "policy1".to_string(),
            "Test Policy".to_string(),
            "Test policy".to_string(),
            vec![PolicyRule::allow_all()],
        );
        engine.add_policy(policy);

        let context = AuthorizationContext::new("user123", &[Role::user()], &dummy_resource());
        let request = AuthorizationRequest {
            user_id: "user123".to_string(),
            resource_type: ResourceType::Template,
            resource_id: "res1".to_string(),
            action: Permission::Read,
            context,
        };

        // Act
        let result = engine.evaluate(&request);

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap().is_allowed());
    }

    #[test]
    fn test_policy_engine_evaluate_deny_precedence() {
        // Arrange
        let mut engine = PolicyEngine::new();

        let allow_policy = Policy::new(
            "allow_policy".to_string(),
            "Allow Policy".to_string(),
            "Allow policy".to_string(),
            vec![PolicyRule::allow_all()],
        );

        let deny_policy = Policy::new(
            "deny_policy".to_string(),
            "Deny Policy".to_string(),
            "Deny policy".to_string(),
            vec![PolicyRule::deny_all()],
        );

        engine.add_policy(allow_policy);
        engine.add_policy(deny_policy);

        let context = AuthorizationContext::new("user123", &[Role::user()], &dummy_resource());
        let request = AuthorizationRequest {
            user_id: "user123".to_string(),
            resource_type: ResourceType::Template,
            resource_id: "res1".to_string(),
            action: Permission::Read,
            context,
        };

        // Act
        let result = engine.evaluate(&request);

        // Assert - Deny should take precedence
        assert!(result.is_ok());
        assert!(result.unwrap().is_denied());
    }

    #[test]
    fn test_policy_engine_restrictive_default() {
        // Arrange
        let engine = PolicyEngine::restrictive();
        let context = AuthorizationContext::new("user123", &[Role::user()], &dummy_resource());
        let request = AuthorizationRequest {
            user_id: "user123".to_string(),
            resource_type: ResourceType::Template,
            resource_id: "res1".to_string(),
            action: Permission::Read,
            context,
        };

        // Act
        let result = engine.evaluate(&request);

        // Assert
        assert!(result.is_ok());
        assert!(result.unwrap().is_denied());
    }

    // Helper function
    fn dummy_resource() -> crate::rbac::resource::Resource {
        use crate::rbac::permission::Permissions;
        use crate::rbac::resource::{Resource, ResourceOwner, ResourceType};

        Resource {
            id: "res1".to_string(),
            resource_type: ResourceType::Template,
            owner: ResourceOwner::new("owner1"),
            permissions: Permissions::empty(),
        }
    }
}
