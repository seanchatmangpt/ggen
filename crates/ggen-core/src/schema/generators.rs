//! Code Generators for A2A Schemas
//!
//! This module provides code generators for multiple target languages from parsed schemas:
//! - Java (POJOs with Jackson annotations)
//! - Elixir (structs with @type specs)
//! - TypeScript (interfaces with optional fields)

use crate::schema::{Schema, SchemaType};
use std::fmt::Write;

/// Java code generator
pub struct JavaGenerator;

impl JavaGenerator {
    /// Generate Java POJO class from schema
    pub fn generate_class(schema: &Schema) -> String {
        let mut output = String::new();

        // Package declaration
        let _ = writeln!(output, "package com.a2a.generated;");
        let _ = writeln!(output);

        // Imports
        let _ = writeln!(output, "import com.fasterxml.jackson.annotation.*;");
        let _ = writeln!(output, "import java.util.List;");
        let _ = writeln!(output, "import java.util.ArrayList;");
        let _ = writeln!(output);

        // Class Javadoc
        if let Some(desc) = &schema.description {
            let _ = writeln!(output, "/**\n * {}\n */", desc);
        }

        // Class declaration
        let _ = writeln!(output, "@JsonInclude(JsonInclude.Include.NON_NULL)");
        let _ = writeln!(output, "public class {} {{", schema.name);
        let _ = writeln!(output);

        // Private fields
        for field in &schema.fields {
            if let Some(desc) = &field.description {
                let _ = writeln!(output, "    /**\n     * {}\n     */", desc);
            }
            let _ = writeln!(
                output,
                "    private {} {};",
                Self::field_type_to_java(&field.field_type),
                Self::to_camel_case(&field.name)
            );
            let _ = writeln!(output);
        }

        // Getters and setters
        for field in &schema.fields {
            let field_name_camel = Self::to_camel_case(&field.name);
            let java_type = Self::field_type_to_java(&field.field_type);

            // Getter
            let _ = writeln!(
                output,
                "    public {} get{}() {{",
                java_type,
                Self::capitalize(&field_name_camel)
            );
            let _ = writeln!(output, "        return this.{};", field_name_camel);
            let _ = writeln!(output, "    }}");
            let _ = writeln!(output);

            // Setter
            let _ = writeln!(
                output,
                "    public void set{}({} {}) {{",
                Self::capitalize(&field_name_camel),
                java_type,
                field_name_camel
            );
            let _ = writeln!(
                output,
                "        this.{} = {};",
                field_name_camel, field_name_camel
            );
            let _ = writeln!(output, "    }}");
            let _ = writeln!(output);
        }

        // toString() method
        let _ = writeln!(output, "    @Override");
        let _ = writeln!(output, "    public String toString() {{");
        let _ = writeln!(output, "        return \"{}{{\" +", schema.name);
        let field_names: Vec<String> = schema
            .fields
            .iter()
            .map(|f| Self::to_camel_case(&f.name))
            .collect();
        for (i, field_name) in field_names.iter().enumerate() {
            let suffix = if i < field_names.len() - 1 { "," } else { "" };
            let _ = writeln!(
                output,
                "                \"{}=\" + {}{}\"",
                field_name, field_name, suffix
            );
        }
        let _ = writeln!(output, "                \"}}\";");
        let _ = writeln!(output, "    }}");

        let _ = writeln!(output, "}}");

        output
    }

    /// Convert SchemaType to Java type
    fn field_type_to_java(field_type: &SchemaType) -> String {
        match field_type {
            SchemaType::String => "String".to_string(),
            SchemaType::Integer => "Long".to_string(),
            SchemaType::Boolean => "Boolean".to_string(),
            SchemaType::Float => "Double".to_string(),
            SchemaType::Any => "Object".to_string(),
            SchemaType::Array(inner) => format!("List<{}>", Self::field_type_to_java(inner)),
            SchemaType::Object(_) => "Map<String, Object>".to_string(),
            SchemaType::Reference(name) => name.clone(),
        }
    }

    /// Convert snake_case to camelCase
    fn to_camel_case(s: &str) -> String {
        let parts: Vec<&str> = s.split('_').collect();
        let mut result = parts[0].to_string();
        for part in &parts[1..] {
            result.push_str(&Self::capitalize(part));
        }
        result
    }

    /// Capitalize first letter
    fn capitalize(s: &str) -> String {
        let mut chars = s.chars();
        match chars.next() {
            None => String::new(),
            Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        }
    }
}

/// Elixir code generator
pub struct ElixirGenerator;

impl ElixirGenerator {
    /// Generate Elixir struct from schema
    pub fn generate_struct(schema: &Schema) -> String {
        let mut output = String::new();

        // Module documentation
        let _ = writeln!(output, "defmodule {} do", schema.name);
        let _ = writeln!(output, "  @moduledoc \"\"\"");
        if let Some(desc) = &schema.description {
            let _ = writeln!(output, "  {}", desc);
        } else {
            let _ = writeln!(output, "  Auto-generated A2A schema: {}", schema.name);
        }
        let _ = writeln!(output, "  \"\"\"");
        let _ = writeln!(output);

        // defstruct
        let _ = writeln!(output, "  defstruct [");
        for (i, field) in schema.fields.iter().enumerate() {
            let comma = if i < schema.fields.len() - 1 { "," } else { "" };
            let _ = writeln!(output, "    :{}{}", field.name, comma);
        }
        let _ = writeln!(output, "  ]");
        let _ = writeln!(output);

        // @type spec
        let _ = writeln!(output, "  @type t :: %__MODULE__{{");
        for field in &schema.fields {
            let elixir_type = Self::field_type_to_elixir(&field.field_type, field.optional);
            let _ = writeln!(output, "    {}: {},", field.name, elixir_type);
        }
        let _ = writeln!(output, "  }}");
        let _ = writeln!(output);

        // Constructor function
        let _ = writeln!(output, "  @doc \"\"\"");
        let _ = writeln!(output, "  Create a new {} struct", schema.name);
        let _ = writeln!(output, "  \"\"\"");
        let _ = writeln!(output, "  def new(fields \\\\ %{{}}) do");
        let _ = writeln!(output, "    struct = struct!(__MODULE__, fields)");
        let _ = writeln!(output, "    {{:ok, struct}}");
        let _ = writeln!(output, "  rescue");
        let _ = writeln!(output, "    error -> {{:error, error}}");
        let _ = writeln!(output, "  end");
        let _ = writeln!(output);

        // Validation function
        let _ = writeln!(output, "  @doc \"\"\"");
        let _ = writeln!(output, "  Validate required fields");
        let _ = writeln!(output, "  \"\"\"");
        let _ = writeln!(
            output,
            "  def validate(%__MODULE__{} = struct) do",
            schema
                .fields
                .iter()
                .map(|f| format!("{}: _", f.name))
                .collect::<Vec<_>>()
                .join(", ")
        );
        let _ = writeln!(output, "    required = [");
        for field in schema.fields.iter().filter(|f| !f.optional) {
            let _ = writeln!(output, "      :{},", field.name);
        }
        let _ = writeln!(output, "    ]");
        let _ = writeln!(output);
        let _ = writeln!(output, "    missing = required -- Map.keys(struct)");
        let _ = writeln!(output);
        let _ = writeln!(output, "    if Enum.empty?(missing) do");
        let _ = writeln!(output, "      {{:ok, struct}}");
        let _ = writeln!(output, "    else");
        let _ = writeln!(output, "      {{:error, {{:missing_fields, missing}}}}");
        let _ = writeln!(output, "    end");
        let _ = writeln!(output, "  end");
        let _ = writeln!(output);

        // JSON encoding helper
        let _ = writeln!(output, "  @doc \"\"\"");
        let _ = writeln!(output, "  Encode to JSON");
        let _ = writeln!(output, "  \"\"\"");
        let _ = writeln!(
            output,
            "  def to_json(%__MODULE__{} = struct) do",
            schema
                .fields
                .iter()
                .map(|f| format!("{}: _", f.name))
                .collect::<Vec<_>>()
                .join(", ")
        );
        let _ = writeln!(output, "    Jason.encode!(struct)");
        let _ = writeln!(output, "  end");

        let _ = writeln!(output, "end");

        output
    }

    /// Convert SchemaType to Elixir type
    fn field_type_to_elixir(field_type: &SchemaType, optional: bool) -> String {
        let base_type = match field_type {
            SchemaType::String => "String.t()".to_string(),
            SchemaType::Integer => "integer()".to_string(),
            SchemaType::Boolean => "boolean()".to_string(),
            SchemaType::Float => "float()".to_string(),
            SchemaType::Any => "term()".to_string(),
            SchemaType::Array(inner) => {
                format!("list({})", Self::field_type_to_elixir(inner, false))
            }
            SchemaType::Object(_) => "map()".to_string(),
            SchemaType::Reference(name) => format!("{}()", name),
        };

        if optional {
            format!("{} | nil", base_type)
        } else {
            base_type
        }
    }
}

/// TypeScript code generator
pub struct TypeScriptGenerator;

impl TypeScriptGenerator {
    /// Generate TypeScript interface from schema
    pub fn generate_interface(schema: &Schema) -> String {
        let mut output = String::new();

        // File header
        let _ = writeln!(output, "/**");
        let _ = writeln!(
            output,
            " * Auto-generated TypeScript interface from A2A schema: {}",
            schema.name
        );
        if let Some(desc) = &schema.description {
            let _ = writeln!(output, " * {}", desc);
        }
        let _ = writeln!(output, " */");
        let _ = writeln!(output);

        // Interface declaration
        let _ = writeln!(output, "/**");
        if let Some(desc) = &schema.description {
            let _ = writeln!(output, " * {}", desc);
        } else {
            let _ = writeln!(output, " * {} interface", schema.name);
        }
        let _ = writeln!(output, " */");
        let _ = writeln!(output, "export interface {} {{", schema.name);

        // Properties
        for field in &schema.fields {
            if let Some(desc) = &field.description {
                let _ = writeln!(output, "  /** {} */", desc);
            }
            let optional_marker = if field.optional { "?" } else { "" };
            let ts_type = Self::field_type_to_typescript(&field.field_type);
            let _ = writeln!(output, "  {}{}: {};", field.name, optional_marker, ts_type);
            let _ = writeln!(output);
        }

        let _ = writeln!(output, "}}");

        output
    }

    /// Convert SchemaType to TypeScript type
    fn field_type_to_typescript(field_type: &SchemaType) -> String {
        match field_type {
            SchemaType::String => "string".to_string(),
            SchemaType::Integer => "number".to_string(),
            SchemaType::Boolean => "boolean".to_string(),
            SchemaType::Float => "number".to_string(),
            SchemaType::Any => "any".to_string(),
            SchemaType::Array(inner) => format!("{}[]", Self::field_type_to_typescript(inner)),
            SchemaType::Object(_) => "Record<string, any>".to_string(),
            SchemaType::Reference(name) => name.clone(),
        }
    }
}

// Implement all code generation methods on Schema
impl Schema {
    /// Generate Rust struct
    pub fn to_rust_struct(&self) -> String {
        RustGenerator::generate(self)
    }

    /// Generate Go struct
    pub fn to_go_struct(&self) -> String {
        GoGenerator::generate(self)
    }

    /// Generate Elixir struct
    pub fn to_elixir_struct(&self) -> String {
        ElixirGenerator::generate_struct(self)
    }

    /// Generate Java class
    pub fn to_java_class(&self) -> String {
        JavaGenerator::generate_class(self)
    }

    /// Generate TypeScript interface
    pub fn to_typescript_interface(&self) -> String {
        TypeScriptGenerator::generate_interface(self)
    }
}

pub struct RustGenerator;

impl RustGenerator {
    /// Generate Rust struct from schema
    pub fn generate(schema: &crate::schema::Schema) -> String {
        let mut output = String::new();

        if let Some(desc) = &schema.description {
            let _ = writeln!(output, "/// {}", desc);
        }
        let _ = writeln!(output, "#[derive(Debug, Clone, Serialize, Deserialize)]");
        let _ = writeln!(output, "pub struct {} {{", schema.name);

        for field in &schema.fields {
            if let Some(desc) = &field.description {
                let _ = writeln!(output, "    /// {}", desc);
            }
            let rust_type = Self::field_type_to_rust(&field.field_type, field.optional);
            let _ = writeln!(output, "    pub {}: {},", field.name, rust_type);
        }

        let _ = writeln!(output, "}}");
        output
    }

    fn field_type_to_rust(field_type: &SchemaType, optional: bool) -> String {
        let base = match field_type {
            SchemaType::String => "String".to_string(),
            SchemaType::Integer => "i64".to_string(),
            SchemaType::Boolean => "bool".to_string(),
            SchemaType::Float => "f64".to_string(),
            SchemaType::Any => "serde_json::Value".to_string(),
            SchemaType::Array(inner) => format!("Vec<{}>", Self::field_type_to_rust(inner, false)),
            SchemaType::Object(_) => {
                "std::collections::HashMap<String, serde_json::Value>".to_string()
            }
            SchemaType::Reference(name) => name.clone(),
        };
        if optional {
            format!("Option<{}>", base)
        } else {
            base
        }
    }
}

/// Go code generator
pub struct GoGenerator;

impl GoGenerator {
    /// Generate Go struct from schema
    pub fn generate(schema: &crate::schema::Schema) -> String {
        let mut output = String::new();

        if let Some(desc) = &schema.description {
            let _ = writeln!(output, "// {} describes the request.", desc);
        }
        let _ = writeln!(output, "type {} struct {{", schema.name);

        for field in &schema.fields {
            let go_type = Self::field_type_to_go(&field.field_type, field.optional);
            let json_tag = if field.optional {
                format!("{},omitempty", field.name)
            } else {
                field.name.clone()
            };
            let _ = writeln!(
                output,
                "    {} {} `json:\"{}\"`",
                Self::capitalize(&field.name),
                go_type,
                json_tag
            );
        }

        let _ = writeln!(output, "}}");
        output
    }

    fn field_type_to_go(field_type: &SchemaType, _optional: bool) -> String {
        // Go uses omitempty in JSON tags for optional fields, not pointer types
        match field_type {
            SchemaType::String => "string".to_string(),
            SchemaType::Integer => "int64".to_string(),
            SchemaType::Boolean => "bool".to_string(),
            SchemaType::Float => "float64".to_string(),
            SchemaType::Any => "interface{}".to_string(),
            SchemaType::Array(inner) => format!("[]{}", Self::field_type_to_go(inner, false)),
            SchemaType::Object(_) => "map[string]interface{}".to_string(),
            SchemaType::Reference(name) => name.clone(),
        }
    }

    fn capitalize(s: &str) -> String {
        let mut chars = s.chars();
        match chars.next() {
            None => String::new(),
            Some(first) => first.to_uppercase().collect::<String>() + chars.as_str(),
        }
    }
}

/// Python code generator (stub for compatibility)
pub struct PythonGenerator;

impl PythonGenerator {
    /// Generate Python dataclass from schema (placeholder)
    pub fn generate(_schema: &crate::schema::Schema) -> String {
        "# Python code generation not yet implemented for A2A schemas\n# Use OntologySchema instead"
            .to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::schema::{Field, SchemaParser};

    fn create_test_schema() -> Schema {
        Schema {
            name: "FileReadRequest".to_string(),
            description: Some("Request to read a file".to_string()),
            fields: vec![
                Field {
                    name: "path".to_string(),
                    field_type: SchemaType::String,
                    optional: false,
                    description: Some("File path to read".to_string()),
                },
                Field {
                    name: "offset".to_string(),
                    field_type: SchemaType::Integer,
                    optional: true,
                    description: Some("Byte offset to start reading".to_string()),
                },
                Field {
                    name: "limit".to_string(),
                    field_type: SchemaType::Integer,
                    optional: true,
                    description: None,
                },
            ],
            nested_schemas: vec![],
        }
    }

    #[test]
    fn test_java_generator() {
        let schema = create_test_schema();
        let java_code = schema.to_java_class();

        assert!(java_code.contains("public class FileReadRequest"));
        assert!(java_code.contains("private String path;"));
        assert!(java_code.contains("private Long offset;"));
        assert!(java_code.contains("public String getPath()"));
        assert!(java_code.contains("public void setPath(String path)"));
        assert!(java_code.contains("@JsonInclude(JsonInclude.Include.NON_NULL)"));
    }

    #[test]
    fn test_elixir_generator() {
        let schema = create_test_schema();
        let elixir_code = schema.to_elixir_struct();

        assert!(elixir_code.contains("defmodule FileReadRequest do"));
        assert!(elixir_code.contains("defstruct ["));
        assert!(elixir_code.contains(":path"));
        assert!(elixir_code.contains("@type t :: %__MODULE__{"));
        assert!(elixir_code.contains("path: String.t()"));
        assert!(elixir_code.contains("offset: integer() | nil"));
        assert!(elixir_code.contains("limit: integer() | nil"));
    }

    #[test]
    fn test_typescript_generator() {
        let schema = create_test_schema();
        let ts_code = schema.to_typescript_interface();

        assert!(ts_code.contains("export interface FileReadRequest {"));
        assert!(ts_code.contains("path: string;"));
        assert!(ts_code.contains("offset?: number;"));
        assert!(ts_code.contains("limit?: number;"));
    }

    #[test]
    fn test_array_types() {
        let schema = Schema {
            name: "ArrayTest".to_string(),
            description: None,
            fields: vec![Field {
                name: "tags".to_string(),
                field_type: SchemaType::Array(Box::new(SchemaType::String)),
                optional: false,
                description: None,
            }],
            nested_schemas: vec![],
        };

        // Java should use List<String>
        let java = schema.to_java_class();
        assert!(java.contains("List<String> tags"));

        // Elixir should use list(String.t())
        let elixir = schema.to_elixir_struct();
        assert!(elixir.contains("list(String.t())"));

        // TypeScript should use string[]
        let ts = schema.to_typescript_interface();
        assert!(ts.contains("tags: string[]"));
    }

    #[test]
    #[ignore = "JSON Schema parsing not yet implemented; enable when SchemaParser::from_json_schema is implemented"]
    fn test_nested_schema() -> Result<(), String> {
        let json_schema = r#"
        {
            "title": "NestedRequest",
            "type": "object",
            "properties": {
                "metadata": {
                    "type": "object",
                    "properties": {
                        "created_at": { "type": "string" }
                    }
                },
                "items": {
                    "type": "array",
                    "items": { "type": "string" }
                }
            },
            "required": ["items"]
        }
        "#;

        let schema = SchemaParser::from_json_schema(json_schema)?;
        assert_eq!(schema.name, "NestedRequest");
        assert_eq!(schema.fields.len(), 2);

        let java = schema.to_java_class();
        assert!(java.contains("private List<String> items;"));
        Ok(())
    }
}
