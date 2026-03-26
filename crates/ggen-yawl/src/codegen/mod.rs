//! Code generation module for YAWL workflows.

pub mod java_rules;
pub mod rules;
pub mod yawl_xml;

pub use java_rules::{
    ExecutedRuleRecord, GeneratedFile, GenerationMode, Queryable, Renderable, Rule, RuleSet,
};
pub use rules::{
    create_controller_rule, create_dto_rule, create_enum_rule, create_hbm_mapping_rule,
    create_jackson_serializer_rule, create_jpa_entity_rule, create_repository_rule,
    create_service_rule,
    // TODO: Uncomment once spring_boot_app.rs is fixed
    // create_spring_boot_app_rule, create_application_main_rule,
    // create_application_properties_rule, create_application_test_properties_rule, create_gitignore_rule,
    EnumDefinition, FieldInfo, FieldMapping, HbmMappingQuery, HbmMappingRule,
    JacksonSerializerQuery, JacksonSerializerRule, JacksonSerializerTemplate, SerializationType,
    SerializerDetail, SerializerQueryResult,
    // SpringBootAppQuery, PomTemplate, ApplicationTemplate,
    // ApplicationPropertiesTemplate, ApplicationTestPropertiesTemplate, GitignoreTemplate,
};
pub use yawl_xml::{canonicalize, validate, YawlXmlGenerator};
