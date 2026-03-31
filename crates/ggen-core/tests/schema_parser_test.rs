//! Comprehensive tests for schema parser and generators
//!
//! This test suite covers:
//! - Schema parsing with PEST grammar
//! - Code generation for 5 languages (Rust, Go, Elixir, Java, TypeScript)
//! - Tera filter integration
//! - Edge cases and error handling

use ggen_core::register::register_all;
use ggen_core::schema::generators::{
    ElixirGenerator, GoGenerator, JavaGenerator, RustGenerator, TypeScriptGenerator,
};
use ggen_core::schema::{Field, Schema, SchemaParser, SchemaType};
use proptest::prelude::*;
use tera::{Context, Tera};

fn create_test_tera() -> Tera {
    let mut tera = Tera::default();
    register_all(&mut tera);
    tera
}

// =========================================================================
// Basic Parser Tests
// =========================================================================

#[test]
fn test_parse_simple_schema() {
    let schema = SchemaParser::parse("Request { field: string }").expect("Failed to parse");
    assert_eq!(schema.name, "Request");
    assert_eq!(schema.fields.len(), 1);
    assert_eq!(schema.fields[0].name, "field");
    assert_eq!(schema.fields[0].field_type, SchemaType::String);
    assert!(!schema.fields[0].optional);
}

#[test]
fn test_parse_file_read_request() {
    let schema = SchemaParser::parse("FileReadRequest { path: string, offset?: integer }")
        .expect("Failed to parse");
    assert_eq!(schema.name, "FileReadRequest");
    assert_eq!(schema.fields.len(), 2);
    assert_eq!(schema.fields[0].name, "path");
    assert_eq!(schema.fields[1].name, "offset");
    assert!(!schema.fields[0].optional);
    assert!(schema.fields[1].optional);
}

#[test]
fn test_parse_array_types() {
    let schema = SchemaParser::parse("ArrayTest { items: string[], numbers?: integer[] }").unwrap();
    assert_eq!(
        schema.fields[0].field_type,
        SchemaType::Array(Box::new(SchemaType::String))
    );
    assert_eq!(
        schema.fields[1].field_type,
        SchemaType::Array(Box::new(SchemaType::Integer))
    );
}

#[test]
fn test_parse_named_types() {
    let schema =
        SchemaParser::parse("Request { custom: MyCustomType, another: OtherType }").unwrap();
    assert_eq!(
        schema.fields[0].field_type,
        SchemaType::Reference("MyCustomType".to_string())
    );
    assert_eq!(
        schema.fields[1].field_type,
        SchemaType::Reference("OtherType".to_string())
    );
}

#[test]
fn test_parse_all_primitive_types() {
    let schema = SchemaParser::parse(
        "Primitives { \
            str: string, \
            int: integer, \
            float_: float, \
            bool: boolean, \
            any_: any \
        }",
    )
    .unwrap();
    assert_eq!(schema.fields[0].field_type, SchemaType::String);
    assert_eq!(schema.fields[1].field_type, SchemaType::Integer);
    assert_eq!(schema.fields[2].field_type, SchemaType::Float);
    assert_eq!(schema.fields[3].field_type, SchemaType::Boolean);
    assert_eq!(schema.fields[4].field_type, SchemaType::Any);
}

#[test]
fn test_parse_empty_schema() {
    let schema = SchemaParser::parse("Empty { }").unwrap();
    assert_eq!(schema.name, "Empty");
    assert_eq!(schema.fields.len(), 0);
}

#[test]
fn test_parse_error_missing_brace() {
    let result = SchemaParser::parse("Request { field: string");
    assert!(result.is_err());
}

#[test]
fn test_parse_error_empty_name() {
    let result = SchemaParser::parse("{ field: string }");
    assert!(result.is_err());
}

#[test]
fn test_parse_error_invalid_type() {
    // Numbers are not valid types in our grammar
    let result = SchemaParser::parse("Request { field: 123 }");
    assert!(result.is_err());
}

#[test]
fn test_parse_error_malformed_field() {
    let result = SchemaParser::parse("Request { field }");
    assert!(result.is_err());
}

// =========================================================================
// Rust Generator Tests
// =========================================================================

#[test]
fn test_rust_generator_simple() {
    let schema = SchemaParser::parse("FileReadRequest { path: string, offset?: integer }").unwrap();
    let rust = RustGenerator::generate(&schema);

    assert!(rust.contains("pub struct FileReadRequest"));
    assert!(rust.contains("pub path: String"));
    assert!(rust.contains("pub offset: Option<i64>"));
    assert!(rust.contains("#[derive(Debug, Clone, Serialize, Deserialize)]"));
}

#[test]
fn test_rust_generator_arrays() {
    let schema = SchemaParser::parse("ArrayTest { tags: string[] }").unwrap();
    let rust = RustGenerator::generate(&schema);

    assert!(rust.contains("pub tags: Vec<String>"));
}

#[test]
fn test_rust_generator_named_types() {
    let schema = SchemaParser::parse("Request { custom: MyType }").unwrap();
    let rust = RustGenerator::generate(&schema);

    assert!(rust.contains("pub custom: MyType"));
}

#[test]
fn test_rust_generator_all_primitives() {
    let schema = SchemaParser::parse(
        "Primitives { \
            str: string, \
            int: integer, \
            float_: float, \
            bool: boolean, \
            any_: any \
        }",
    )
    .unwrap();
    let rust = RustGenerator::generate(&schema);

    assert!(rust.contains("pub str: String"));
    assert!(rust.contains("pub int: i64"));
    assert!(rust.contains("pub float_: f64"));
    assert!(rust.contains("pub bool: bool"));
    assert!(rust.contains("pub any_: serde_json::Value"));
}

// =========================================================================
// Go Generator Tests
// =========================================================================

#[test]
fn test_go_generator_simple() {
    let schema = SchemaParser::parse("FileReadRequest { path: string, offset?: integer }").unwrap();
    let go = GoGenerator::generate(&schema);

    assert!(go.contains("type FileReadRequest struct"));
    assert!(go.contains("Path string"));
    assert!(go.contains("Offset int64"));
    assert!(go.contains("`json:\"path\"`"));
    assert!(go.contains("`json:\"offset,omitempty\"`"));
}

#[test]
fn test_go_generator_arrays() {
    let schema = SchemaParser::parse("ArrayTest { tags: string[] }").unwrap();
    let go = GoGenerator::generate(&schema);

    assert!(go.contains("Tags []string"));
}

#[test]
fn test_go_generator_named_types() {
    let schema = SchemaParser::parse("Request { custom: MyType }").unwrap();
    let go = GoGenerator::generate(&schema);

    assert!(go.contains("Custom MyType"));
}

// =========================================================================
// Elixir Generator Tests
// =========================================================================

#[test]
fn test_elixir_generator_simple() {
    let schema = SchemaParser::parse("FileReadRequest { path: string, offset?: integer }").unwrap();
    let elixir = ElixirGenerator::generate_struct(&schema);

    assert!(elixir.contains("defmodule FileReadRequest"));
    assert!(elixir.contains("defstruct ["));
    assert!(elixir.contains(":path"));
    assert!(elixir.contains(":offset"));
    assert!(elixir.contains("@type t :: %__MODULE__{"));
    assert!(elixir.contains("path: String.t()"));
    assert!(elixir.contains("offset: integer() | nil"));
}

#[test]
fn test_elixir_generator_arrays() {
    let schema = SchemaParser::parse("ArrayTest { tags: string[] }").unwrap();
    let elixir = ElixirGenerator::generate_struct(&schema);

    assert!(elixir.contains("list(String.t())"));
}

#[test]
fn test_elixir_generator_validation() {
    let schema = SchemaParser::parse("Request { required: string, optional?: integer }").unwrap();
    let elixir = ElixirGenerator::generate_struct(&schema);

    assert!(elixir.contains("def validate"));
    assert!(elixir.contains("required = ["));
    assert!(elixir.contains(":required"));
}

// =========================================================================
// Java Generator Tests
// =========================================================================

#[test]
fn test_java_generator_simple() {
    let schema = SchemaParser::parse("FileReadRequest { path: string, offset?: integer }").unwrap();
    let java = JavaGenerator::generate_class(&schema);

    assert!(java.contains("public class FileReadRequest"));
    assert!(java.contains("private String path;"));
    assert!(java.contains("private Long offset;"));
    assert!(java.contains("public String getPath()"));
    assert!(java.contains("public void setPath(String path)"));
    assert!(java.contains("@JsonInclude(JsonInclude.Include.NON_NULL)"));
}

#[test]
fn test_java_generator_arrays() {
    let schema = SchemaParser::parse("ArrayTest { tags: string[] }").unwrap();
    let java = JavaGenerator::generate_class(&schema);

    assert!(java.contains("List<String> tags"));
}

#[test]
fn test_java_generator_to_string() {
    let schema = SchemaParser::parse("Request { name: string, age: integer }").unwrap();
    let java = JavaGenerator::generate_class(&schema);

    assert!(java.contains("@Override"));
    assert!(java.contains("public String toString()"));
    assert!(java.contains("Request{"));
}

// =========================================================================
// TypeScript Generator Tests
// =========================================================================

#[test]
fn test_typescript_generator_simple() {
    let schema = SchemaParser::parse("FileReadRequest { path: string, offset?: integer }").unwrap();
    let ts = TypeScriptGenerator::generate_interface(&schema);

    assert!(ts.contains("export interface FileReadRequest"));
    assert!(ts.contains("path: string;"));
    assert!(ts.contains("offset?: number;"));
}

#[test]
fn test_typescript_generator_arrays() {
    let schema = SchemaParser::parse("ArrayTest { tags: string[] }").unwrap();
    let ts = TypeScriptGenerator::generate_interface(&schema);

    assert!(ts.contains("tags: string[]"));
}

#[test]
fn test_typescript_generator_all_types() {
    let schema = SchemaParser::parse(
        "AllTypes { \
            str: string, \
            int: integer, \
            float_: float, \
            bool: boolean, \
            any_: any, \
            arr: string[] \
        }",
    )
    .unwrap();
    let ts = TypeScriptGenerator::generate_interface(&schema);

    assert!(ts.contains("str: string"));
    assert!(ts.contains("int: number"));
    assert!(ts.contains("float_: number"));
    assert!(ts.contains("bool: boolean"));
    assert!(ts.contains("any_: any"));
    assert!(ts.contains("arr: string[]"));
}

// =========================================================================
// Schema Trait Method Tests
// =========================================================================

#[test]
fn test_schema_to_rust_struct() {
    let schema = SchemaParser::parse("Request { field: string }").unwrap();
    let rust = schema.to_rust_struct();
    assert!(rust.contains("pub struct Request"));
}

#[test]
fn test_schema_to_go_struct() {
    let schema = SchemaParser::parse("Request { field: string }").unwrap();
    let go = schema.to_go_struct();
    assert!(go.contains("type Request struct"));
}

#[test]
fn test_schema_to_elixir_struct() {
    let schema = SchemaParser::parse("Request { field: string }").unwrap();
    let elixir = schema.to_elixir_struct();
    assert!(elixir.contains("defmodule Request"));
}

#[test]
fn test_schema_to_java_class() {
    let schema = SchemaParser::parse("Request { field: string }").unwrap();
    let java = schema.to_java_class();
    assert!(java.contains("public class Request"));
}

#[test]
fn test_schema_to_typescript_interface() {
    let schema = SchemaParser::parse("Request { field: string }").unwrap();
    let ts = schema.to_typescript_interface();
    assert!(ts.contains("export interface Request"));
}

// =========================================================================
// Tera Filter Integration Tests
// =========================================================================

#[test]
fn test_tera_schema_to_rust_filter() {
    let mut tera = create_test_tera();
    let mut ctx = Context::new();
    ctx.insert(
        "schema",
        "FileReadRequest { path: string, offset?: integer }",
    );

    let result = tera
        .render_str("{{ schema | schema_to_rust }}", &ctx)
        .unwrap();
    assert!(result.contains("pub struct FileReadRequest"));
    assert!(result.contains("pub path: String"));
    assert!(result.contains("pub offset: Option<i64>"));
}

#[test]
fn test_tera_schema_to_go_filter() {
    let mut tera = create_test_tera();
    let mut ctx = Context::new();
    ctx.insert(
        "schema",
        "FileReadRequest { path: string, offset?: integer }",
    );

    let result = tera
        .render_str("{{ schema | schema_to_go }}", &ctx)
        .unwrap();
    assert!(result.contains("type FileReadRequest struct"));
    assert!(result.contains("Path string"));
    assert!(result.contains("Offset int64"));
}

#[test]
fn test_tera_schema_to_elixir_filter() {
    let mut tera = create_test_tera();
    let mut ctx = Context::new();
    ctx.insert(
        "schema",
        "FileReadRequest { path: string, offset?: integer }",
    );

    let result = tera
        .render_str("{{ schema | schema_to_elixir }}", &ctx)
        .unwrap();
    assert!(result.contains("defmodule FileReadRequest"));
    assert!(result.contains("defstruct"));
}

#[test]
fn test_tera_schema_to_java_filter() {
    let mut tera = create_test_tera();
    let mut ctx = Context::new();
    ctx.insert(
        "schema",
        "FileReadRequest { path: string, offset?: integer }",
    );

    let result = tera
        .render_str("{{ schema | schema_to_java }}", &ctx)
        .unwrap();
    assert!(result.contains("public class FileReadRequest"));
    assert!(result.contains("private String path"));
    assert!(result.contains("private Long offset"));
}

#[test]
fn test_tera_schema_to_typescript_filter() {
    let mut tera = create_test_tera();
    let mut ctx = Context::new();
    ctx.insert(
        "schema",
        "FileReadRequest { path: string, offset?: integer }",
    );

    let result = tera
        .render_str("{{ schema | schema_to_typescript }}", &ctx)
        .unwrap();
    assert!(result.contains("export interface FileReadRequest"));
    assert!(result.contains("path: string"));
    assert!(result.contains("offset?: number"));
}

#[test]
fn test_tera_all_schema_filters_registered() {
    let mut tera = create_test_tera();
    let mut ctx = Context::new();
    ctx.insert("schema", "Test { field: string }");

    let filters = vec![
        "schema_to_rust",
        "schema_to_go",
        "schema_to_elixir",
        "schema_to_java",
        "schema_to_typescript",
    ];

    for filter in filters {
        let result = tera.render_str(&format!("{{{{ schema | {} }}}}", filter), &ctx);
        assert!(
            result.is_ok(),
            "Filter '{}' should be registered and working",
            filter
        );
    }
}

// =========================================================================
// Property-Based Tests with Proptest
// =========================================================================

proptest! {
    #[test]
    fn prop_parse_valid_schema(name in "[a-zA-Z][a-zA-Z0-9_]*") {
        let schema_str = format!("{} {{ field: string }}", name);
        let result = SchemaParser::parse(&schema_str);
        prop_assert!(result.is_ok());
        if let Ok(schema) = result {
            prop_assert_eq!(schema.name, name);
            prop_assert_eq!(schema.fields.len(), 1);
        }
    }

    #[test]
    fn prop_parse_multiple_fields(fields in prop::collection::vec("[a-z]+: string", 1..10)) {
        let fields_str = fields.join(", ");
        let schema_str = format!("Request {{ {} }}", fields_str);
        let result = SchemaParser::parse(&schema_str);
        prop_assert!(result.is_ok());
        if let Ok(schema) = result {
            prop_assert_eq!(schema.fields.len(), fields.len());
        }
    }

    #[test]
    fn prop_roundtrip_schema(schema_str in "[a-zA-Z][a-zA-Z0-9_]* \\{ [a-z]+: (string|integer|boolean|float|any)(, [a-z]+: (string|integer|boolean|float|any))* \\}") {
        let result = SchemaParser::parse(&schema_str);
        prop_assert!(result.is_ok());
    }
}

// =========================================================================
// Edge Cases and Complex Scenarios
// =========================================================================

#[test]
fn test_complex_nested_schema() {
    let schema = SchemaParser::parse(
        "ComplexRequest { \
            name: string, \
            tags: string[], \
            count: integer, \
            active?: boolean, \
            metadata?: string[] \
        }",
    )
    .unwrap();

    assert_eq!(schema.fields.len(), 5);
    assert_eq!(schema.fields[0].field_type, SchemaType::String);
    assert_eq!(
        schema.fields[1].field_type,
        SchemaType::Array(Box::new(SchemaType::String))
    );
    assert_eq!(schema.fields[2].field_type, SchemaType::Integer);
    assert_eq!(schema.fields[3].field_type, SchemaType::Boolean);
    assert!(schema.fields[3].optional);
}

#[test]
fn test_schema_with_all_optional_fields() {
    let schema =
        SchemaParser::parse("AllOptional { a?: string, b?: integer, c?: boolean }").unwrap();

    assert!(schema.fields.iter().all(|f| f.optional));
}

#[test]
fn test_whitespace_tolerance() {
    let schemas = vec![
        "Request{field:string}",
        "Request { field : string }",
        "Request{\n  field: string\n}",
        "Request  {  field  :  string  }  ",
    ];

    for schema_str in schemas {
        let result = SchemaParser::parse(schema_str);
        assert!(result.is_ok(), "Failed to parse: '{}'", schema_str);
    }
}

#[test]
fn test_large_schema_performance() {
    let fields: Vec<String> = (0..100).map(|i| format!("field{}: string", i)).collect();
    let fields_str = fields.join(", ");
    let schema_str = format!("Large {{ {} }}", fields_str);

    let start = std::time::Instant::now();
    let result = SchemaParser::parse(&schema_str);
    let duration = start.elapsed();

    assert!(result.is_ok());
    if let Ok(schema) = result {
        assert_eq!(schema.fields.len(), 100);
    }
    // Should parse 100 fields in less than 100ms
    assert!(
        duration.as_millis() < 100,
        "Parsing took too long: {:?}",
        duration
    );
}
