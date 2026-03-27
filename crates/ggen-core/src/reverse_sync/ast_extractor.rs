//! Code to RDF extraction - Parse Rust, Elixir, and Go services into RDF format
//!
//! This module provides AST extraction functionality that converts source code definitions
//! into ServiceDef structures, and then into RDF/Turtle format for integration with the
//! ggen ontology system.
//!
//! # Example
//!
//! ```rust,no_run
//! use ggen_core::reverse_sync::ast_extractor::{extract_rust_service, convert_to_rdf, Language};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let services = extract_rust_service("src/lib.rs")?;
//! let rdf = convert_to_rdf(&services)?;
//! println!("{}", rdf);
//! # Ok(())
//! # }
//! ```

use regex::Regex;
use std::fs;
use ggen_utils::error::{Error, Result};

/// Programming language identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Language {
    Rust,
    Elixir,
    Go,
}

impl Language {
    fn as_str(&self) -> &'static str {
        match self {
            Language::Rust => "rust",
            Language::Elixir => "elixir",
            Language::Go => "go",
        }
    }
}

/// Represents a field in a struct/type
#[derive(Debug, Clone)]
pub struct Field {
    pub name: String,
    pub field_type: String,
}

/// Represents a method or callback
#[derive(Debug, Clone)]
pub struct Method {
    pub name: String,
    pub params: Vec<String>,
    pub return_type: Option<String>,
}

/// Complete service definition extracted from source code
#[derive(Debug, Clone)]
pub struct ServiceDef {
    pub name: String,
    pub language: Language,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
}

/// Extract Rust struct definitions from a file
///
/// Parses `pub struct Name { field: Type, ... }` patterns and returns
/// ServiceDef structs with field information.
///
/// # Example
///
/// ```rust,no_run
/// # use ggen_core::reverse_sync::ast_extractor::extract_rust_service;
/// let services = extract_rust_service("src/service.rs")?;
/// assert!(!services.is_empty());
/// # Ok::<(), ggen_utils::error::Error>(())
/// # }
/// ```
pub fn extract_rust_service(file_path: &str) -> Result<Vec<ServiceDef>> {
    let content = fs::read_to_string(file_path)
        .map_err(|e| Error::new(&format!("Failed to read {}: {}", file_path, e)))?;

    let mut services = Vec::new();

    // Regex to match: pub struct Name { ... }
    let struct_re = Regex::new(r"pub\s+struct\s+(\w+)\s*\{([^}]*)\}")
        .map_err(|e| Error::new(&format!("Failed to compile struct regex: {}", e)))?;

    for cap in struct_re.captures_iter(&content) {
        let name = cap.get(1).unwrap().as_str().to_string();
        let body = cap.get(2).unwrap().as_str();

        let fields = parse_struct_fields(body)?;

        services.push(ServiceDef {
            name,
            language: Language::Rust,
            fields,
            methods: Vec::new(),
        });
    }

    Ok(services)
}

/// Extract Elixir GenServer definitions from a file
///
/// Parses `defmodule Name do ... use GenServer` patterns and extracts
/// callback methods (init, handle_call, handle_cast, etc.)
///
/// # Example
///
/// ```rust,no_run
/// # use ggen_core::reverse_sync::ast_extractor::extract_elixir_genserver;
/// let services = extract_elixir_genserver("lib/processor.ex")?;
/// assert!(!services.is_empty());
/// # Ok::<(), ggen_utils::error::Error>(())
/// # }
/// ```
pub fn extract_elixir_genserver(file_path: &str) -> Result<Vec<ServiceDef>> {
    let content = fs::read_to_string(file_path)
        .map_err(|e| Error::new(&format!("Failed to read {}: {}", file_path, e)))?;

    let mut services = Vec::new();

    // Regex to match: defmodule Name do ... end
    let module_re = Regex::new(r"defmodule\s+(\w+)\s+do")
        .map_err(|e| Error::new(&format!("Failed to compile module regex: {}", e)))?;

    // Check if file contains GenServer pattern
    let has_genserver = content.contains("use GenServer");

    for cap in module_re.captures_iter(&content) {
        let name = cap.get(1).unwrap().as_str().to_string();

        let methods = if has_genserver {
            extract_elixir_callbacks(&content)?
        } else {
            Vec::new()
        };

        services.push(ServiceDef {
            name,
            language: Language::Elixir,
            fields: Vec::new(),
            methods,
        });
    }

    Ok(services)
}

/// Extract callback method names from Elixir code
fn extract_elixir_callbacks(content: &str) -> Result<Vec<Method>> {
    let mut methods = Vec::new();

    // Match: def callback_name(...) do ... end
    let callback_re = Regex::new(r"def\s+(init|handle_call|handle_cast|handle_info|terminate)\s*\(([^)]*)\)")
        .map_err(|e| Error::new(&format!("Failed to compile callback regex: {}", e)))?;

    for cap in callback_re.captures_iter(content) {
        let name = cap.get(1).unwrap().as_str().to_string();
        let params_str = cap.get(2).unwrap().as_str();

        // Simple param parsing (split by comma, trim)
        let params: Vec<String> = params_str
            .split(',')
            .map(|p| p.trim().to_string())
            .filter(|p| !p.is_empty())
            .collect();

        methods.push(Method {
            name,
            params,
            return_type: Some("tuple".to_string()), // Elixir callbacks return tuples
        });
    }

    Ok(methods)
}

/// Extract Go struct definitions and their methods from a file
///
/// Parses `type Name struct { ... }` patterns and associated methods
/// `func (s *Name) MethodName(...) ReturnType`
///
/// # Example
///
/// ```rust,no_run
/// # use ggen_core::reverse_sync::ast_extractor::extract_go_service;
/// let services = extract_go_service("service.go")?;
/// assert!(!services.is_empty());
/// # Ok::<(), ggen_utils::error::Error>(())
/// # }
/// ```
pub fn extract_go_service(file_path: &str) -> Result<Vec<ServiceDef>> {
    let content = fs::read_to_string(file_path)
        .map_err(|e| Error::new(&format!("Failed to read {}: {}", file_path, e)))?;

    let mut services = Vec::new();

    // Regex to match: type Name struct { ... }
    let struct_re = Regex::new(r"type\s+(\w+)\s+struct\s*\{([^}]*)\}")
        .map_err(|e| Error::new(&format!("Failed to compile Go struct regex: {}", e)))?;

    for cap in struct_re.captures_iter(&content) {
        let name = cap.get(1).unwrap().as_str().to_string();
        let body = cap.get(2).unwrap().as_str();

        let fields = parse_go_struct_fields(body)?;
        let methods = extract_go_methods(&content, &name)?;

        services.push(ServiceDef {
            name,
            language: Language::Go,
            fields,
            methods,
        });
    }

    Ok(services)
}

/// Parse struct fields from Go struct body
fn parse_go_struct_fields(body: &str) -> Result<Vec<Field>> {
    let mut fields = Vec::new();

    // Match: FieldName FieldType
    let field_re = Regex::new(r"(\w+)\s+(\w+(?:\s*\[\])?(?:\s*\*)?)")
        .map_err(|e| Error::new(&format!("Failed to compile field regex: {}", e)))?;

    for cap in field_re.captures_iter(body) {
        let field_name = cap.get(1).unwrap().as_str().to_string();
        let field_type = cap.get(2).unwrap().as_str().to_string();

        fields.push(Field {
            name: field_name,
            field_type,
        });
    }

    Ok(fields)
}

/// Extract receiver methods for a Go struct
fn extract_go_methods(content: &str, struct_name: &str) -> Result<Vec<Method>> {
    let mut methods = Vec::new();

    // Match: func (s *StructName) MethodName(...) ReturnType
    let method_pattern = format!(r"func\s*\(\s*\w+\s*\*{}\s*\)\s*(\w+)\s*\(([^)]*)\)\s*(\w+)?", struct_name);
    let method_re = Regex::new(&method_pattern)
        .map_err(|e| Error::new(&format!("Failed to compile Go method regex: {}", e)))?;

    for cap in method_re.captures_iter(content) {
        let method_name = cap.get(1).unwrap().as_str().to_string();
        let params_str = cap.get(2).unwrap().as_str();

        let params: Vec<String> = params_str
            .split(',')
            .map(|p| p.trim().to_string())
            .filter(|p| !p.is_empty())
            .collect();

        let return_type = cap.get(3).map(|m| m.as_str().to_string());

        methods.push(Method {
            name: method_name,
            params,
            return_type,
        });
    }

    Ok(methods)
}

/// Parse struct field definitions from Rust/Elixir struct body
fn parse_struct_fields(body: &str) -> Result<Vec<Field>> {
    let mut fields = Vec::new();

    // Match: field_name: Type or field_name: Type,
    let field_re = Regex::new(r"(\w+)\s*:\s*([^,}]+)")
        .map_err(|e| Error::new(&format!("Failed to compile field regex: {}", e)))?;

    for cap in field_re.captures_iter(body) {
        let field_name = cap.get(1).unwrap().as_str().to_string();
        let field_type = cap.get(2).unwrap().as_str().trim().to_string();

        fields.push(Field {
            name: field_name,
            field_type,
        });
    }

    Ok(fields)
}

/// Convert ServiceDef structures into RDF/Turtle format
///
/// Generates RDF triples describing services and their properties.
/// Uses the `https://ggen.io/code#` namespace for code-related predicates.
///
/// # Example
///
/// ```rust,no_run
/// # use ggen_core::reverse_sync::ast_extractor::{extract_rust_service, convert_to_rdf};
/// let services = extract_rust_service("src/lib.rs")?;
/// let rdf = convert_to_rdf(&services)?;
/// println!("{}", rdf);
/// # Ok::<(), ggen_utils::error::Error>(())
/// # }
/// ```
pub fn convert_to_rdf(services: &[ServiceDef]) -> Result<String> {
    let mut turtle = String::from(
        "@prefix code: <https://ggen.io/code#> .\n\
         @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\
         @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\n"
    );

    for service in services {
        let safe_name = sanitize_iri(&service.name);
        let resource_id = format!("code:{}", safe_name);

        // Service type and language
        turtle.push_str(&format!(
            "{} a code:Service ;\n    code:language \"{}\" ;\n",
            resource_id, service.language.as_str()
        ));

        // Add fields as properties
        if !service.fields.is_empty() {
            for field in &service.fields {
                turtle.push_str(&format!(
                    "    code:hasField [ a code:Field ; code:fieldName \"{}\" ; code:fieldType \"{}\" ] ;\n",
                    field.name, field.field_type
                ));
            }
        }

        // Add methods
        if !service.methods.is_empty() {
            for method in &service.methods {
                turtle.push_str(&format!(
                    "    code:hasMethod [ a code:Method ; code:methodName \"{}\"",
                    method.name
                ));

                if !method.params.is_empty() {
                    let params_str = method.params.join(", ");
                    turtle.push_str(&format!("; code:methodParams \"{}\"", params_str));
                }

                if let Some(return_type) = &method.return_type {
                    turtle.push_str(&format!("; code:returnType \"{}\"", return_type));
                }

                turtle.push_str(" ] ;\n");
            }
        }

        // Close resource (remove trailing semicolon from last property)
        if turtle.ends_with(";\n") {
            turtle.pop();
            turtle.pop();
            turtle.push_str(" .\n");
        }

        turtle.push('\n');
    }

    Ok(turtle)
}

/// Sanitize strings for use in IRIs
///
/// Removes or replaces special characters that are not valid in IRIs.
fn sanitize_iri(input: &str) -> String {
    input
        .chars()
        .map(|c| match c {
            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => c,
            _ => '_',
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sanitize_iri() {
        assert_eq!(sanitize_iri("MyService"), "MyService");
        assert_eq!(sanitize_iri("My-Service"), "My_Service");
        assert_eq!(sanitize_iri("service.type"), "service_type");
    }

    #[test]
    fn test_convert_to_rdf_empty() {
        let services: Vec<ServiceDef> = vec![];
        let rdf = convert_to_rdf(&services).unwrap();
        assert!(rdf.contains("@prefix code:"));
    }

    #[test]
    fn test_convert_to_rdf_with_service() {
        let service = ServiceDef {
            name: "MyService".to_string(),
            language: Language::Rust,
            fields: vec![Field {
                name: "id".to_string(),
                field_type: "u64".to_string(),
            }],
            methods: vec![],
        };

        let rdf = convert_to_rdf(&[service]).unwrap();
        assert!(rdf.contains("code:MyService"));
        assert!(rdf.contains("code:Service"));
        assert!(rdf.contains("\"rust\""));
        assert!(rdf.contains("\"id\""));
        assert!(rdf.contains("\"u64\""));
    }

    #[test]
    fn test_convert_to_rdf_with_method() {
        let service = ServiceDef {
            name: "Handler".to_string(),
            language: Language::Elixir,
            fields: vec![],
            methods: vec![Method {
                name: "handle_call".to_string(),
                params: vec!["msg".to_string(), "from".to_string()],
                return_type: Some("tuple".to_string()),
            }],
        };

        let rdf = convert_to_rdf(&[service]).unwrap();
        assert!(rdf.contains("code:Handler"));
        assert!(rdf.contains("code:Method"));
        assert!(rdf.contains("\"handle_call\""));
        assert!(rdf.contains("code:methodParams"));
        assert!(rdf.contains("code:returnType"));
    }

    #[test]
    fn test_parse_struct_fields() {
        let body = "id: u64, name: String, active: bool";
        let fields = parse_struct_fields(body).unwrap();

        assert_eq!(fields.len(), 3);
        assert_eq!(fields[0].name, "id");
        assert_eq!(fields[0].field_type, "u64");
        assert_eq!(fields[1].name, "name");
        assert_eq!(fields[1].field_type, "String");
    }

    #[test]
    fn test_extract_elixir_callbacks() {
        let content = r#"
            defmodule MyHandler do
              use GenServer

              def init(args) do
                {:ok, args}
              end

              def handle_call(msg, from, state) do
                {:reply, msg, state}
              end
            end
        "#;

        let methods = extract_elixir_callbacks(content).unwrap();
        assert!(methods.iter().any(|m| m.name == "init"));
        assert!(methods.iter().any(|m| m.name == "handle_call"));
    }
}
