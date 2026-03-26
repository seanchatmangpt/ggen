//! Code generation rules for YAWL workflows.
//!
//! This module contains specialized generation rules that transform RDF ontologies
//! into specific code artifacts (HBM mappings, JPA entities, Jackson serializers, etc.).

pub mod hbm_mappings;
pub mod jackson_serializers;

pub use hbm_mappings::{
    create_hbm_mapping_rule, HbmMappingQuery, HbmMappingRule, HbmMappingTemplate,
    IdGeneratorStrategy, PropertyMapping,
};
pub use jackson_serializers::{
    create_jackson_serializer_rule, EnumDefinition, FieldInfo, FieldMapping,
    JacksonSerializerQuery, JacksonSerializerRule, JacksonSerializerTemplate, SerializationType,
    SerializerDetail, SerializerQueryResult,
};
