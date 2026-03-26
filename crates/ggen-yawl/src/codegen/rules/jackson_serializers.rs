//! Rule 10: Jackson Serializers for custom JSON serialization
//!
//! This module implements the Rule<JacksonSerializerQuery, JacksonSerializerTemplate>
//! pattern for generating Jackson @JsonSerialize/@JsonDeserialize custom handlers.
//!
//! Handles special serialization for:
//! - Enum serializers (serialize/deserialize enum values to/from strings)
//! - LocalDateTime serializers (ISO8601 format)
//! - Custom bean serializers for complex nested objects

use crate::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Represents a type that needs custom serialization
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum SerializationType {
    /// Enum type requiring string serialization
    Enum(String),
    /// LocalDateTime requiring ISO8601 serialization
    LocalDateTime,
    /// Complex nested object requiring custom bean serializer
    CustomBean(String),
}

/// Query for identifying types needing custom serialization
/// Implements the Queryable trait pattern
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JacksonSerializerQuery {
    /// Package structure for analysis
    pub package: String,
    /// Class name being analyzed
    pub class_name: String,
    /// Field information to analyze
    pub fields: Vec<FieldInfo>,
    /// Enum definitions
    pub enums: Vec<EnumDefinition>,
}

/// Information about a field in a class
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldInfo {
    /// Field name
    pub name: String,
    /// Field type (e.g., "String", "LocalDateTime", "List<Item>")
    pub type_name: String,
    /// Whether field is optional
    pub optional: bool,
}

/// Definition of an enum type
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnumDefinition {
    /// Enum name
    pub name: String,
    /// Enum values
    pub values: Vec<String>,
}

/// Query result identifying types needing serialization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializerQueryResult {
    /// Types needing serializers
    pub serializers_needed: Vec<SerializationType>,
    /// Detailed information per type
    pub serializer_details: HashMap<String, SerializerDetail>,
}

/// Details for a specific serializer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerializerDetail {
    /// Type of serialization needed
    pub serialization_type: SerializationType,
    /// Class/enum name
    pub class_name: String,
    /// Package path
    pub package: String,
    /// Whether it's a custom bean serializer
    pub is_custom_bean: bool,
    /// Values (for enums)
    pub values: Vec<String>,
}

/// Renderable template for Jackson serializer generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JacksonSerializerTemplate {
    /// Type being serialized
    pub serialization_type: SerializationType,
    /// Class name
    pub class_name: String,
    /// Package structure
    pub package: String,
    /// Import statements
    pub imports: Vec<String>,
    /// Field mappings for bean serializers
    pub field_mappings: Vec<FieldMapping>,
    /// Enum values for enum serializers
    pub enum_values: Vec<String>,
}

/// Field mapping information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldMapping {
    /// Java field name
    pub field_name: String,
    /// JSON field name
    pub json_name: String,
    /// Field type
    pub field_type: String,
}

impl JacksonSerializerQuery {
    /// Create a new Jackson serializer query
    pub fn new(package: String, class_name: String) -> Self {
        Self {
            package,
            class_name,
            fields: Vec::new(),
            enums: Vec::new(),
        }
    }

    /// Add a field to analyze
    pub fn with_field(mut self, field: FieldInfo) -> Self {
        self.fields.push(field);
        self
    }

    /// Add an enum definition
    pub fn with_enum(mut self, enum_def: EnumDefinition) -> Self {
        self.enums.push(enum_def);
        self
    }

    /// Execute the query and identify types needing serializers
    pub fn execute(&self) -> Result<SerializerQueryResult> {
        let mut serializers_needed = Vec::new();
        let mut serializer_details = HashMap::new();

        // Analyze fields for LocalDateTime and nested types
        for field in &self.fields {
            if field.type_name.contains("LocalDateTime") {
                serializers_needed.push(SerializationType::LocalDateTime);
                let detail = SerializerDetail {
                    serialization_type: SerializationType::LocalDateTime,
                    class_name: "LocalDateTimeSerializer".to_string(),
                    package: self.package.clone(),
                    is_custom_bean: false,
                    values: vec![],
                };
                serializer_details.insert(
                    "LocalDateTimeSerializer".to_string(),
                    detail,
                );
            }

            // Check for complex nested objects
            if is_complex_type(&field.type_name)
                && !field.type_name.contains("LocalDateTime")
            {
                let serializer_name = format!("{}Serializer", extract_type_name(&field.type_name));
                serializers_needed.push(SerializationType::CustomBean(
                    serializer_name.clone(),
                ));
                let detail = SerializerDetail {
                    serialization_type: SerializationType::CustomBean(
                        serializer_name.clone(),
                    ),
                    class_name: serializer_name.clone(),
                    package: self.package.clone(),
                    is_custom_bean: true,
                    values: vec![],
                };
                serializer_details.insert(serializer_name, detail);
            }
        }

        // Analyze enums
        for enum_def in &self.enums {
            serializers_needed.push(SerializationType::Enum(enum_def.name.clone()));
            let detail = SerializerDetail {
                serialization_type: SerializationType::Enum(enum_def.name.clone()),
                class_name: format!("{}Serializer", enum_def.name),
                package: self.package.clone(),
                is_custom_bean: false,
                values: enum_def.values.clone(),
            };
            serializer_details.insert(format!("{}Serializer", enum_def.name), detail);
        }

        Ok(SerializerQueryResult {
            serializers_needed,
            serializer_details,
        })
    }
}

impl JacksonSerializerTemplate {
    /// Create a new serializer template for an enum
    pub fn enum_serializer(
        class_name: String,
        package: String,
        values: Vec<String>,
    ) -> Self {
        let mut imports = vec![
            "import com.fasterxml.jackson.databind.JsonSerializer;".to_string(),
            "import com.fasterxml.jackson.databind.SerializerProvider;".to_string(),
            "import com.fasterxml.jackson.databind.JsonGenerator;".to_string(),
            "import com.fasterxml.jackson.databind.JsonDeserializer;".to_string(),
            "import com.fasterxml.jackson.core.JsonParser;".to_string(),
            "import com.fasterxml.jackson.core.JsonProcessingException;".to_string(),
            "import com.fasterxml.jackson.databind.DeserializationContext;".to_string(),
            "import java.io.IOException;".to_string(),
        ];
        imports.sort();
        imports.dedup();

        Self {
            serialization_type: SerializationType::Enum(class_name.clone()),
            class_name,
            package,
            imports,
            field_mappings: Vec::new(),
            enum_values: values,
        }
    }

    /// Create a new serializer template for LocalDateTime
    pub fn datetime_serializer(package: String) -> Self {
        let imports = vec![
            "import com.fasterxml.jackson.databind.JsonSerializer;".to_string(),
            "import com.fasterxml.jackson.databind.SerializerProvider;".to_string(),
            "import com.fasterxml.jackson.databind.JsonGenerator;".to_string(),
            "import com.fasterxml.jackson.databind.JsonDeserializer;".to_string(),
            "import com.fasterxml.jackson.core.JsonParser;".to_string(),
            "import com.fasterxml.jackson.core.JsonProcessingException;".to_string(),
            "import com.fasterxml.jackson.databind.DeserializationContext;".to_string(),
            "import java.io.IOException;".to_string(),
            "import java.time.LocalDateTime;".to_string(),
            "import java.time.format.DateTimeFormatter;".to_string(),
        ];

        Self {
            serialization_type: SerializationType::LocalDateTime,
            class_name: "LocalDateTimeSerializer".to_string(),
            package,
            imports,
            field_mappings: Vec::new(),
            enum_values: Vec::new(),
        }
    }

    /// Create a new serializer template for custom bean
    pub fn custom_bean_serializer(
        class_name: String,
        package: String,
        field_mappings: Vec<FieldMapping>,
    ) -> Self {
        let mut imports = vec![
            "import com.fasterxml.jackson.databind.JsonSerializer;".to_string(),
            "import com.fasterxml.jackson.databind.SerializerProvider;".to_string(),
            "import com.fasterxml.jackson.databind.JsonGenerator;".to_string(),
            "import com.fasterxml.jackson.databind.JsonDeserializer;".to_string(),
            "import com.fasterxml.jackson.core.JsonParser;".to_string(),
            "import com.fasterxml.jackson.core.JsonProcessingException;".to_string(),
            "import com.fasterxml.jackson.databind.DeserializationContext;".to_string(),
            "import java.io.IOException;".to_string(),
        ];
        imports.sort();
        imports.dedup();

        Self {
            serialization_type: SerializationType::CustomBean(class_name.clone()),
            class_name,
            package,
            imports,
            field_mappings,
            enum_values: Vec::new(),
        }
    }

    /// Generate the Java serializer class code
    pub fn render(&self) -> Result<String> {
        match &self.serialization_type {
            SerializationType::Enum(_) => self.render_enum_serializer(),
            SerializationType::LocalDateTime => self.render_datetime_serializer(),
            SerializationType::CustomBean(_) => self.render_custom_bean_serializer(),
        }
    }

    fn render_enum_serializer(&self) -> Result<String> {
        let mut code = String::new();

        // Package declaration
        code.push_str(&format!("package {};\n\n", self.package));

        // Imports
        for import in &self.imports {
            code.push_str(import);
            code.push('\n');
        }
        code.push('\n');

        // Serializer class
        code.push_str(&format!(
            "/**\n * Jackson serializer for {} enum.\n */\n",
            self.class_name
        ));
        code.push_str(&format!(
            "public class {}Serializer extends JsonSerializer<{}> {{\n",
            self.class_name, self.class_name
        ));
        code.push_str("    @Override\n");
        code.push_str("    public void serialize(\n");
        code.push_str(&format!("            {} value,\n", self.class_name));
        code.push_str("            JsonGenerator gen,\n");
        code.push_str("            SerializerProvider provider) throws IOException {\n");
        code.push_str("        if (value == null) {\n");
        code.push_str("            gen.writeNull();\n");
        code.push_str("        } else {\n");
        code.push_str("            gen.writeString(value.toString());\n");
        code.push_str("        }\n");
        code.push_str("    }\n");
        code.push_str("}\n\n");

        // Deserializer class
        code.push_str(&format!(
            "/**\n * Jackson deserializer for {} enum.\n */\n",
            self.class_name
        ));
        code.push_str(&format!(
            "class {}Deserializer extends JsonDeserializer<{}> {{\n",
            self.class_name, self.class_name
        ));
        code.push_str("    @Override\n");
        code.push_str("    public ");
        code.push_str(&self.class_name);
        code.push_str(" deserialize(\n");
        code.push_str("            JsonParser p,\n");
        code.push_str("            DeserializationContext ctx) throws IOException, JsonProcessingException {\n");
        code.push_str("        String value = p.getValueAsString();\n");
        code.push_str("        if (value == null || value.isEmpty()) {\n");
        code.push_str("            return null;\n");
        code.push_str("        }\n");
        code.push_str("        try {\n");
        code.push_str(&format!(
            "            return {}.valueOf(value.toUpperCase());\n",
            self.class_name
        ));
        code.push_str("        } catch (IllegalArgumentException e) {\n");
        code.push_str(&format!(
            "            throw new IOException(\"Invalid {{}} value: \" + value, e);\n"
        ));
        code.push_str("        }\n");
        code.push_str("    }\n");
        code.push_str("}\n");

        Ok(code)
    }

    fn render_datetime_serializer(&self) -> Result<String> {
        let mut code = String::new();

        // Package declaration
        code.push_str(&format!("package {};\n\n", self.package));

        // Imports
        for import in &self.imports {
            code.push_str(import);
            code.push('\n');
        }
        code.push('\n');

        // Serializer class
        code.push_str("/**\n * Jackson serializer for LocalDateTime in ISO8601 format.\n */\n");
        code.push_str("public class LocalDateTimeSerializer extends JsonSerializer<LocalDateTime> {\n");
        code.push_str("    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ISO_DATE_TIME;\n\n");
        code.push_str("    @Override\n");
        code.push_str("    public void serialize(\n");
        code.push_str("            LocalDateTime value,\n");
        code.push_str("            JsonGenerator gen,\n");
        code.push_str("            SerializerProvider provider) throws IOException {\n");
        code.push_str("        if (value == null) {\n");
        code.push_str("            gen.writeNull();\n");
        code.push_str("        } else {\n");
        code.push_str("            gen.writeString(FORMATTER.format(value));\n");
        code.push_str("        }\n");
        code.push_str("    }\n");
        code.push_str("}\n\n");

        // Deserializer class
        code.push_str("/**\n * Jackson deserializer for LocalDateTime from ISO8601 format.\n */\n");
        code.push_str("class LocalDateTimeDeserializer extends JsonDeserializer<LocalDateTime> {\n");
        code.push_str("    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ISO_DATE_TIME;\n\n");
        code.push_str("    @Override\n");
        code.push_str("    public LocalDateTime deserialize(\n");
        code.push_str("            JsonParser p,\n");
        code.push_str("            DeserializationContext ctx) throws IOException, JsonProcessingException {\n");
        code.push_str("        String value = p.getValueAsString();\n");
        code.push_str("        if (value == null || value.isEmpty()) {\n");
        code.push_str("            return null;\n");
        code.push_str("        }\n");
        code.push_str("        try {\n");
        code.push_str("            return LocalDateTime.parse(value, FORMATTER);\n");
        code.push_str("        } catch (Exception e) {\n");
        code.push_str("            throw new IOException(\"Invalid ISO8601 date-time: \" + value, e);\n");
        code.push_str("        }\n");
        code.push_str("    }\n");
        code.push_str("}\n");

        Ok(code)
    }

    fn render_custom_bean_serializer(&self) -> Result<String> {
        let mut code = String::new();

        // Package declaration
        code.push_str(&format!("package {};\n\n", self.package));

        // Imports
        for import in &self.imports {
            code.push_str(import);
            code.push('\n');
        }
        code.push('\n');

        // Serializer class
        code.push_str(&format!(
            "/**\n * Jackson serializer for {} bean.\n */\n",
            self.class_name
        ));
        code.push_str(&format!(
            "public class {} extends JsonSerializer<Object> {{\n",
            self.class_name
        ));
        code.push_str("    @Override\n");
        code.push_str("    public void serialize(\n");
        code.push_str("            Object value,\n");
        code.push_str("            JsonGenerator gen,\n");
        code.push_str("            SerializerProvider provider) throws IOException {\n");
        code.push_str("        if (value == null) {\n");
        code.push_str("            gen.writeNull();\n");
        code.push_str("        } else {\n");
        code.push_str("            gen.writeStartObject();\n");

        for field in &self.field_mappings {
            code.push_str("            // Field: ");
            code.push_str(&field.field_name);
            code.push('\n');
            code.push_str(&format!(
                "            gen.writeField(\"{}\", \"{}\");\n",
                field.json_name, field.field_type
            ));
        }

        code.push_str("            gen.writeEndObject();\n");
        code.push_str("        }\n");
        code.push_str("    }\n");
        code.push_str("}\n");

        Ok(code)
    }
}

/// Factory function to create a Jackson serializer rule
pub fn create_jackson_serializer_rule() -> JacksonSerializerRule {
    JacksonSerializerRule::new()
}

/// The Rule<Q, T> pattern: JacksonSerializerRule
#[derive(Debug, Clone)]
pub struct JacksonSerializerRule {
    /// Rule metadata
    pub metadata: RuleMetadata,
}

/// Metadata for the rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuleMetadata {
    /// Rule identifier
    pub id: String,
    /// Rule description
    pub description: String,
    /// Version
    pub version: String,
}

impl JacksonSerializerRule {
    /// Create a new Jackson serializer rule
    pub fn new() -> Self {
        Self {
            metadata: RuleMetadata {
                id: "rule-10-jackson-serializers".to_string(),
                description: "Generate Jackson @JsonSerialize/@JsonDeserialize custom handlers".to_string(),
                version: "1.0.0".to_string(),
            },
        }
    }

    /// Apply the rule to a query, producing a template
    pub fn apply(
        &self,
        query: &JacksonSerializerQuery,
    ) -> Result<Vec<JacksonSerializerTemplate>> {
        let query_result = query.execute()?;
        let mut templates = Vec::new();

        for (_, detail) in query_result.serializer_details {
            let template = match detail.serialization_type {
                SerializationType::Enum(ref name) => {
                    JacksonSerializerTemplate::enum_serializer(
                        name.clone(),
                        detail.package,
                        detail.values,
                    )
                }
                SerializationType::LocalDateTime => {
                    JacksonSerializerTemplate::datetime_serializer(detail.package)
                }
                SerializationType::CustomBean(ref name) => {
                    JacksonSerializerTemplate::custom_bean_serializer(
                        name.clone(),
                        detail.package,
                        Vec::new(),
                    )
                }
            };
            templates.push(template);
        }

        Ok(templates)
    }

    /// Generate file path for a serializer
    pub fn output_path(&self, class_name: &str, package: &str) -> String {
        let package_path = package.replace('.', "/");
        format!(
            "src/main/java/{}/{}Serializer.java",
            package_path, class_name
        )
    }
}

impl Default for JacksonSerializerRule {
    fn default() -> Self {
        Self::new()
    }
}

// Helper functions

/// Check if a type is complex (not a primitive or standard type)
fn is_complex_type(type_name: &str) -> bool {
    let standard_types = [
        "String",
        "int",
        "long",
        "boolean",
        "double",
        "float",
        "Integer",
        "Long",
        "Boolean",
        "Double",
        "Float",
    ];

    !standard_types.contains(&type_name)
        && !type_name.contains("LocalDateTime")
        && !type_name.contains("Date")
}

/// Extract the base type name from a generic or complex type
fn extract_type_name(type_name: &str) -> String {
    // Handle generic types like List<Item>, Set<Item>, etc.
    if let Some(start) = type_name.find('<') {
        if let Some(end) = type_name.find('>') {
            return type_name[start + 1..end].to_string();
        }
    }
    type_name.to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_jackson_serializer_rule() {
        let rule = create_jackson_serializer_rule();
        assert_eq!(rule.metadata.id, "rule-10-jackson-serializers");
    }

    #[test]
    fn test_enum_serializer_template() {
        let template = JacksonSerializerTemplate::enum_serializer(
            "Status".to_string(),
            "com.example.serializers".to_string(),
            vec!["ACTIVE".to_string(), "INACTIVE".to_string()],
        );
        assert_eq!(template.class_name, "Status");
        assert_eq!(template.enum_values.len(), 2);
    }

    #[test]
    fn test_datetime_serializer_template() {
        let template = JacksonSerializerTemplate::datetime_serializer(
            "com.example.serializers".to_string(),
        );
        assert_eq!(template.class_name, "LocalDateTimeSerializer");
        assert!(template.imports.contains(
            &"import java.time.LocalDateTime;".to_string()
        ));
    }

    #[test]
    fn test_custom_bean_serializer_template() {
        let field_mappings = vec![FieldMapping {
            field_name: "id".to_string(),
            json_name: "id".to_string(),
            field_type: "String".to_string(),
        }];
        let template = JacksonSerializerTemplate::custom_bean_serializer(
            "ItemSerializer".to_string(),
            "com.example.serializers".to_string(),
            field_mappings,
        );
        assert_eq!(template.class_name, "ItemSerializer");
        assert_eq!(template.field_mappings.len(), 1);
    }

    #[test]
    fn test_jackson_serializer_query_with_enum() {
        let enum_def = EnumDefinition {
            name: "Status".to_string(),
            values: vec!["ACTIVE".to_string(), "INACTIVE".to_string()],
        };
        let query = JacksonSerializerQuery::new(
            "com.example".to_string(),
            "WorkflowState".to_string(),
        )
        .with_enum(enum_def);

        assert_eq!(query.enums.len(), 1);
    }

    #[test]
    fn test_query_execution_with_datetime_field() {
        let field = FieldInfo {
            name: "createdAt".to_string(),
            type_name: "LocalDateTime".to_string(),
            optional: false,
        };
        let query = JacksonSerializerQuery::new(
            "com.example".to_string(),
            "Workflow".to_string(),
        )
        .with_field(field);

        let result = query.execute().unwrap();
        assert!(result
            .serializers_needed
            .contains(&SerializationType::LocalDateTime));
    }

    #[test]
    fn test_query_execution_with_custom_bean() {
        let field = FieldInfo {
            name: "metadata".to_string(),
            type_name: "MetadataObject".to_string(),
            optional: false,
        };
        let query = JacksonSerializerQuery::new(
            "com.example".to_string(),
            "Workflow".to_string(),
        )
        .with_field(field);

        let result = query.execute().unwrap();
        assert!(!result.serializers_needed.is_empty());
    }

    #[test]
    fn test_render_enum_serializer() {
        let template = JacksonSerializerTemplate::enum_serializer(
            "Status".to_string(),
            "com.example.serializers".to_string(),
            vec!["ACTIVE".to_string(), "INACTIVE".to_string()],
        );
        let code = template.render().unwrap();
        assert!(code.contains("public class StatusSerializer"));
        assert!(code.contains("JsonSerializer<Status>"));
        assert!(code.contains("serialize("));
    }

    #[test]
    fn test_render_datetime_serializer() {
        let template = JacksonSerializerTemplate::datetime_serializer(
            "com.example.serializers".to_string(),
        );
        let code = template.render().unwrap();
        assert!(code.contains("LocalDateTimeSerializer"));
        assert!(code.contains("DateTimeFormatter.ISO_DATE_TIME"));
        assert!(code.contains("LocalDateTime.parse"));
    }

    #[test]
    fn test_output_path_generation() {
        let rule = create_jackson_serializer_rule();
        let path = rule.output_path("Status", "com.example.serializers");
        assert!(path.contains("src/main/java/com/example/serializers/StatusSerializer.java"));
    }

    #[test]
    fn test_extract_type_name_from_generic() {
        let generic_type = "List<Item>";
        let result = extract_type_name(generic_type);
        assert_eq!(result, "Item");
    }

    #[test]
    fn test_is_complex_type() {
        assert!(!is_complex_type("String"));
        assert!(!is_complex_type("int"));
        assert!(is_complex_type("CustomObject"));
        assert!(is_complex_type("List<Item>"));
    }

    #[test]
    fn test_rule_apply_generates_templates() {
        let rule = create_jackson_serializer_rule();
        let enum_def = EnumDefinition {
            name: "Status".to_string(),
            values: vec!["ACTIVE".to_string(), "INACTIVE".to_string()],
        };
        let query = JacksonSerializerQuery::new(
            "com.example".to_string(),
            "Workflow".to_string(),
        )
        .with_enum(enum_def);

        let templates = rule.apply(&query).unwrap();
        assert!(!templates.is_empty());
    }
}
