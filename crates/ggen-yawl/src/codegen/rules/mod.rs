//! Concrete implementations of code generation rules.
//!
//! Each rule file implements a specific YAWL Java generation task:
//! - jpa_entity.rs: Rule 3 - JPA entities from persistent classes
//! - (Rules 4-10 follow same pattern)

pub mod jpa_entity;

pub use jpa_entity::create_jpa_entity_rule;
