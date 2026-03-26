//! Concrete implementations of code generation rules (Rules 3-10).
//!
//! Each rule file implements a specific YAWL Java generation task using the
//! Rule<Q, T> composition pattern:
//! - jpa_entity.rs: Rule 3 - JPA entities from persistent classes
//! - repositories.rs: Rule 4 - Spring Data JPA repository interfaces
//! - dtos.rs: Rule 5 - Data Transfer Objects for REST APIs
//! - controllers.rs: Rule 6 - Spring REST controllers
//! - enums.rs: Rule 7 - Java enums for enumerated values
//! - services.rs: Rule 8 - Spring business logic services
//! - hbm_mappings.rs: Rule 9 - Hibernate HBM XML mapping files
//! - jackson_serializers.rs: Rule 10 - Jackson JSON serializer implementations

pub mod controllers;
pub mod dtos;
pub mod enums;
pub mod jpa_entity;
pub mod repositories;
pub mod services;
pub mod hbm_mappings;
pub mod jackson_serializers;

pub use controllers::create_controller_rule;
pub use dtos::create_dto_rule;
pub use enums::create_enum_rule;
pub use jpa_entity::create_jpa_entity_rule;
pub use repositories::create_repository_rule;
pub use services::create_service_rule;
pub use hbm_mappings::{
    create_hbm_mapping_rule, HbmMappingQuery, HbmMappingRule, HbmMappingTemplate,
    IdGeneratorStrategy, PropertyMapping,
};
pub use jackson_serializers::{
    create_jackson_serializer_rule, EnumDefinition, FieldInfo, FieldMapping,
    JacksonSerializerQuery, JacksonSerializerRule, JacksonSerializerTemplate, SerializationType,
    SerializerDetail, SerializerQueryResult,
};
