//! Integration tests for Jackson Serializers (Rule 10)
//!
//! Tests the complete pipeline: Query → Execute → Template → Render

use ggen_yawl::codegen::{
    create_jackson_serializer_rule, EnumDefinition, FieldInfo, JacksonSerializerQuery,
    JacksonSerializerTemplate, SerializationType,
};

#[test]
fn test_enum_serializer_generation() {
    let rule = create_jackson_serializer_rule();
    assert_eq!(rule.metadata.id, "rule-10-jackson-serializers");

    let enum_def = EnumDefinition {
        name: "WorkflowStatus".to_string(),
        values: vec![
            "ACTIVE".to_string(),
            "PAUSED".to_string(),
            "COMPLETED".to_string(),
        ],
    };

    let query = JacksonSerializerQuery::new(
        "com.yawlfoundation.yawl.serializers".to_string(),
        "WorkflowState".to_string(),
    )
    .with_enum(enum_def);

    let templates = rule.apply(&query).unwrap();
    assert!(!templates.is_empty());

    // Verify generated template properties
    let template = &templates[0];
    assert_eq!(template.class_name, "WorkflowStatus");
    assert_eq!(template.package, "com.yawlfoundation.yawl.serializers");
}

#[test]
fn test_datetime_serializer_generation() {
    let rule = create_jackson_serializer_rule();

    let field = FieldInfo {
        name: "createdAt".to_string(),
        type_name: "LocalDateTime".to_string(),
        optional: false,
    };

    let query = JacksonSerializerQuery::new(
        "com.yawlfoundation.yawl.serializers".to_string(),
        "WorkflowEvent".to_string(),
    )
    .with_field(field);

    let result = query.execute().unwrap();
    assert!(result
        .serializers_needed
        .contains(&SerializationType::LocalDateTime));

    let templates = rule.apply(&query).unwrap();
    assert!(!templates.is_empty());

    let template = &templates[0];
    assert_eq!(template.class_name, "LocalDateTimeSerializer");
}

#[test]
fn test_custom_bean_serializer_generation() {
    let rule = create_jackson_serializer_rule();

    let field = FieldInfo {
        name: "metadata".to_string(),
        type_name: "MetadataObject".to_string(),
        optional: true,
    };

    let query = JacksonSerializerQuery::new(
        "com.yawlfoundation.yawl.serializers".to_string(),
        "Workflow".to_string(),
    )
    .with_field(field);

    let result = query.execute().unwrap();
    assert!(!result.serializers_needed.is_empty());

    let templates = rule.apply(&query).unwrap();
    assert!(!templates.is_empty());
}

#[test]
fn test_rendered_enum_serializer_is_valid_java() {
    let template = JacksonSerializerTemplate::enum_serializer(
        "TaskStatus".to_string(),
        "com.yawlfoundation.yawl.serializers".to_string(),
        vec![
            "READY".to_string(),
            "RUNNING".to_string(),
            "CANCELLED".to_string(),
        ],
    );

    let code = template.render().unwrap();

    // Verify Java syntax elements
    assert!(code.contains("package com.yawlfoundation.yawl.serializers;"));
    assert!(code.contains("import com.fasterxml.jackson.databind.JsonSerializer;"));
    assert!(code.contains("public class TaskStatusSerializer extends JsonSerializer<TaskStatus>"));
    assert!(code.contains("public void serialize("));
    assert!(code.contains("gen.writeString(value.toString());"));
    assert!(code.contains("class TaskStatusDeserializer extends JsonDeserializer<TaskStatus>"));
    assert!(code.contains("TaskStatus.valueOf(value.toUpperCase())"));
    assert!(code.contains("IOException"));
}

#[test]
fn test_rendered_datetime_serializer_is_valid_java() {
    let template = JacksonSerializerTemplate::datetime_serializer(
        "com.yawlfoundation.yawl.serializers".to_string(),
    );

    let code = template.render().unwrap();

    // Verify Java syntax elements
    assert!(code.contains("package com.yawlfoundation.yawl.serializers;"));
    assert!(code.contains("import java.time.LocalDateTime;"));
    assert!(code.contains("import java.time.format.DateTimeFormatter;"));
    assert!(
        code.contains("public class LocalDateTimeSerializer extends JsonSerializer<LocalDateTime>")
    );
    assert!(code.contains("DateTimeFormatter.ISO_DATE_TIME"));
    assert!(code.contains("LocalDateTime.parse"));
    assert!(
        code.contains("class LocalDateTimeDeserializer extends JsonDeserializer<LocalDateTime>")
    );
}

#[test]
fn test_rendered_custom_bean_serializer_is_valid_java() {
    let field_mappings = vec![];

    let template = JacksonSerializerTemplate::custom_bean_serializer(
        "ItemSerializer".to_string(),
        "com.yawlfoundation.yawl.serializers".to_string(),
        field_mappings,
    );

    let code = template.render().unwrap();

    // Verify Java syntax elements
    assert!(code.contains("package com.yawlfoundation.yawl.serializers;"));
    assert!(code.contains("public class ItemSerializer extends JsonSerializer<Object>"));
    assert!(code.contains("gen.writeStartObject();"));
    assert!(code.contains("gen.writeEndObject();"));
}

#[test]
fn test_output_path_generation() {
    let rule = create_jackson_serializer_rule();

    let path = rule.output_path("WorkflowStatus", "com.yawlfoundation.yawl.serializers");
    assert_eq!(
        path,
        "src/main/java/com/yawlfoundation/yawl/serializers/WorkflowStatusSerializer.java"
    );

    let path2 = rule.output_path("LocalDateTime", "com.yawlfoundation.yawl.serializers");
    assert_eq!(
        path2,
        "src/main/java/com/yawlfoundation/yawl/serializers/LocalDateTimeSerializer.java"
    );
}

#[test]
fn test_multiple_field_types_in_query() {
    let rule = create_jackson_serializer_rule();

    let field1 = FieldInfo {
        name: "createdAt".to_string(),
        type_name: "LocalDateTime".to_string(),
        optional: false,
    };

    let field2 = FieldInfo {
        name: "metadata".to_string(),
        type_name: "MetadataObject".to_string(),
        optional: true,
    };

    let enum_def = EnumDefinition {
        name: "Status".to_string(),
        values: vec!["ACTIVE".to_string(), "INACTIVE".to_string()],
    };

    let query = JacksonSerializerQuery::new(
        "com.yawlfoundation.yawl.serializers".to_string(),
        "ComplexWorkflow".to_string(),
    )
    .with_field(field1)
    .with_field(field2)
    .with_enum(enum_def);

    let result = query.execute().unwrap();
    assert!(result.serializers_needed.len() >= 2);

    let templates = rule.apply(&query).unwrap();
    assert!(!templates.is_empty());
}

#[test]
fn test_enum_serializer_handles_null_values() {
    let template = JacksonSerializerTemplate::enum_serializer(
        "Priority".to_string(),
        "com.yawlfoundation.yawl.serializers".to_string(),
        vec!["HIGH".to_string(), "LOW".to_string()],
    );

    let code = template.render().unwrap();
    assert!(code.contains("if (value == null)"));
    assert!(code.contains("gen.writeNull();"));
}

#[test]
fn test_datetime_serializer_uses_iso8601_format() {
    let template = JacksonSerializerTemplate::datetime_serializer(
        "com.yawlfoundation.yawl.serializers".to_string(),
    );

    let code = template.render().unwrap();
    assert!(code.contains("ISO_DATE_TIME"));
    assert!(code.contains("DateTimeFormatter"));
}

#[test]
fn test_query_result_contains_serializer_details() {
    let field = FieldInfo {
        name: "scheduledTime".to_string(),
        type_name: "LocalDateTime".to_string(),
        optional: false,
    };

    let query = JacksonSerializerQuery::new("com.example".to_string(), "Task".to_string())
        .with_field(field);

    let result = query.execute().unwrap();
    assert!(!result.serializer_details.is_empty());

    // Verify detail information
    for (_, detail) in result.serializer_details {
        assert!(!detail.class_name.is_empty());
        assert_eq!(detail.package, "com.example");
    }
}

#[test]
fn test_imports_include_jackson_classes() {
    let template = JacksonSerializerTemplate::enum_serializer(
        "Status".to_string(),
        "com.yawlfoundation".to_string(),
        vec!["ACTIVE".to_string()],
    );

    assert!(template
        .imports
        .contains(&"import com.fasterxml.jackson.databind.JsonSerializer;".to_string()));
    assert!(template
        .imports
        .contains(&"import com.fasterxml.jackson.databind.JsonDeserializer;".to_string()));
    assert!(template
        .imports
        .contains(&"import com.fasterxml.jackson.databind.JsonGenerator;".to_string()));
}

#[test]
fn test_serializer_template_datetime_has_proper_imports() {
    let template = JacksonSerializerTemplate::datetime_serializer("com.yawlfoundation".to_string());

    assert!(template
        .imports
        .contains(&"import java.time.LocalDateTime;".to_string()));
    assert!(template
        .imports
        .contains(&"import java.time.format.DateTimeFormatter;".to_string()));
}
