/// Relation-Based Access Control (ReBAC) - inspired by Zanzibar
/// Defines relationships between users, roles, and resources
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// A relation tuple: (User, Relation, Resource)
/// Example: (user123, "owner", "pack:abc")
#[derive(Clone, Debug, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub struct RelationTuple {
    pub subject: String,           // e.g., "user:123" or "role:admin"
    pub relation: String,          // e.g., "owner", "editor", "viewer"
    pub object: String,            // e.g., "pack:abc", "org:xyz"
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Relation definition schema
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct RelationDefinition {
    pub name: String,
    pub subject_types: Vec<String>,    // e.g., ["user", "role"]
    pub object_types: Vec<String>,     // e.g., ["pack", "organization"]
    pub transitive: bool,              // Can this relation be composed?
    pub description: Option<String>,
}

/// Standard Relations in the system
pub mod relations {
    pub const OWNER: &str = "owner";
    pub const EDITOR: &str = "editor";
    pub const VIEWER: &str = "viewer";
    pub const MEMBER: &str = "member";
    pub const ADMIN: &str = "admin";
    pub const PUBLISHER: &str = "publisher";
}

/// Object Types
pub mod objects {
    pub const USER: &str = "user";
    pub const ROLE: &str = "role";
    pub const PACK: &str = "pack";
    pub const ORGANIZATION: &str = "organization";
    pub const TEAM: &str = "team";
    pub const PROJECT: &str = "project";
}

/// Check if user has relation to object
/// Handles transitive relations and hierarchies
pub fn check_relation(
    subject: &str,
    relation: &str,
    object: &str,
    tuples: &[RelationTuple],
) -> bool {
    // Direct match
    for tuple in tuples {
        if tuple.subject == subject && tuple.relation == relation && tuple.object == object {
            return true;
        }
    }

    // Check transitive relations (e.g., user is member of team, team is member of org)
    for tuple in tuples {
        if tuple.relation == "member" {
            if tuple.subject == subject && tuple.object.split(':').next() == object.split(':').next()
            {
                return true;
            }
        }
    }

    false
}

/// Permission lookup: does subject have permission on object?
pub fn has_permission(
    subject: &str,
    permission: &str,
    object: &str,
    tuples: &[RelationTuple],
    definitions: &[RelationDefinition],
) -> bool {
    // Direct permission: user:123 has permission on pack:abc
    if check_relation(subject, permission, object, tuples) {
        return true;
    }

    // Transitive via ownership: user owns organization, user can access org's resources
    for def in definitions {
        if def.transitive && def.name == "owner" {
            // Check if subject is owner of any parent resources
            for tuple in tuples {
                if tuple.subject == subject && tuple.relation == "owner" {
                    // e.g., user owns org, can access all packs in org
                    if object.starts_with(&format!("{}:", tuple.object.split(':').next().unwrap()))
                    {
                        return true;
                    }
                }
            }
        }
    }

    // Role-based permission: user has role, role has permission
    for tuple in tuples {
        if tuple.subject == subject && tuple.object.starts_with("role:") {
            let role = &tuple.object;
            if has_permission(role, permission, object, tuples, definitions) {
                return true;
            }
        }
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_tuples() -> Vec<RelationTuple> {
        vec![
            RelationTuple {
                subject: "user:alice".to_string(),
                relation: "owner".to_string(),
                object: "org:acme".to_string(),
                timestamp: chrono::Utc::now(),
            },
            RelationTuple {
                subject: "user:bob".to_string(),
                relation: "member".to_string(),
                object: "org:acme".to_string(),
                timestamp: chrono::Utc::now(),
            },
            RelationTuple {
                subject: "role:editor".to_string(),
                relation: "viewer".to_string(),
                object: "pack:xyz".to_string(),
                timestamp: chrono::Utc::now(),
            },
            RelationTuple {
                subject: "user:charlie".to_string(),
                relation: "member".to_string(),
                object: "role:editor".to_string(),
                timestamp: chrono::Utc::now(),
            },
        ]
    }

    #[test]
    fn test_direct_relation() {
        let tuples = create_test_tuples();
        assert!(check_relation("user:alice", "owner", "org:acme", &tuples));
        assert!(!check_relation("user:bob", "owner", "org:acme", &tuples));
    }

    #[test]
    fn test_has_permission_direct() {
        let tuples = create_test_tuples();
        let defs = vec![];
        assert!(has_permission("user:alice", "owner", "org:acme", &tuples, &defs));
    }

    #[test]
    fn test_has_permission_role_based() {
        let tuples = create_test_tuples();
        let defs = vec![];
        // user:charlie is member of role:editor, role:editor has viewer on pack:xyz
        // So user:charlie should have viewer permission on pack:xyz
        assert!(has_permission("user:charlie", "viewer", "pack:xyz", &tuples, &defs));
    }
}
