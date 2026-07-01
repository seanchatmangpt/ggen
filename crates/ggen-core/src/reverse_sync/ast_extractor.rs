//! Code to RDF extraction - Parse Rust, Elixir, and Go services into RDF format
//!
//! This module provides AST extraction functionality that converts source code definitions
//! into ServiceDef structures, and then into RDF/Turtle format for integration with the
//! ggen ontology system.
//!
//! # Example
//!
//! ```rust,no_run
//! use crate::reverse_sync::ast_extractor::{extract_rust_service, convert_to_rdf, Language};
//!
//! # fn main() -> crate::utils::error::Result<()> {
//! let services = extract_rust_service("src/lib.rs")?;
//! let rdf = convert_to_rdf(&services)?;
//! println!("{}", rdf);
//! # Ok(())
//! # }
//! ```

use crate::utils::error::{Error, Result};
use regex::Regex;
use std::fs;

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

/// Kind of code construct a [`ServiceDef`] represents.
///
/// Regex-based extraction recovers structs, enums, and traits from Rust source.
/// Elixir/Go extraction continues to use [`ServiceKind::Struct`] (the historical
/// default) since those extractors model modules/structs as services.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ServiceKind {
    Struct,
    Enum,
    Trait,
}

impl ServiceKind {
    /// RDF class name (under the `code:` namespace) for this kind.
    fn rdf_class(&self) -> &'static str {
        match self {
            ServiceKind::Struct => "Service",
            ServiceKind::Enum => "Enum",
            ServiceKind::Trait => "Trait",
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

/// Represents an enum variant.
///
/// Captures variant name and optional payload information (tuple or struct fields).
/// Payloads are stored as raw strings without full parsing.
#[derive(Debug, Clone)]
pub struct Variant {
    pub name: String,
    /// Optional payload: `"(u32, String)"` for tuple variants, `"{ field: Type, ... }"` for struct variants
    pub payload: Option<String>,
}

/// Complete service definition extracted from source code
#[derive(Debug, Clone)]
pub struct ServiceDef {
    pub name: String,
    pub language: Language,
    /// Whether this definition is a struct, enum, or trait. Defaults to
    /// [`ServiceKind::Struct`] for backwards compatibility.
    pub kind: ServiceKind,
    pub fields: Vec<Field>,
    pub methods: Vec<Method>,
    /// Enum variants (empty for non-enum kinds).
    pub variants: Vec<Variant>,
    /// Generic type parameters captured as raw strings (e.g. `T`, `K`, `V`).
    /// Bounds and lifetimes are not separated out.
    pub type_params: Vec<String>,
    /// Trait bounds mapped from type parameter to list of bounds.
    /// E.g. { "T" => ["Clone", "Send"], "U" => ["Default"] }
    pub trait_bounds: std::collections::HashMap<String, Vec<String>>,
}

/// Extract Rust struct definitions from a file
///
/// Parses `pub struct Name { field: Type, ... }` patterns and returns
/// ServiceDef structs with field information.
///
/// # Example
///
/// ```rust,no_run
/// # use crate::reverse_sync::ast_extractor::extract_rust_service;
/// let services = extract_rust_service("src/service.rs")?;
/// assert!(!services.is_empty());
/// # Ok::<(), crate::utils::error::Error>(())
/// # }
/// ```
pub fn extract_rust_service(file_path: &str) -> Result<Vec<ServiceDef>> {
    let content = fs::read_to_string(file_path)
        .map_err(|e| Error::new(&format!("Failed to read {}: {}", file_path, e)))?;

    extract_rust_service_from_str(&content)
}

/// Extract Rust definitions from an in-memory source string.
///
/// Captures (regex-based headers + balanced-brace body scan):
/// - `pub struct Name<...> { ... }` — name, generics, fields
/// - `pub enum Name<...> { Variant, ... }` — name, generics, variant names
/// - `pub trait Name<...> { fn ... }` — name, generics, required method signatures
/// - `impl<...> Name<...> { fn ... }` (inherent and trait impls) — method signatures
///   attached back to the matching struct/enum `ServiceDef`.
///
/// Block bodies are delimited with a brace-depth scan ([`balanced_block_body`])
/// rather than a naive `{...}` regex, so nested braces (method bodies inside an
/// `impl`, struct-variant payloads inside an `enum`) do not truncate the capture.
///
/// This is the workhorse behind [`extract_rust_service`]; it is separated so the
/// extraction logic can be exercised without touching the filesystem.
pub fn extract_rust_service_from_str(content: &str) -> Result<Vec<ServiceDef>> {
    let mut services: Vec<ServiceDef> = Vec::new();

    // --- Structs: pub struct Name<generics> { fields } ---
    // The generics group is optional and non-greedy so it does not swallow the
    // body. The body itself is captured with a balanced-brace scan so nested
    // braces (rare in struct fields, common in struct-variant enums below) do
    // not truncate the capture at the first `}`.
    let struct_header_re = Regex::new(r"pub\s+struct\s+(\w+)\s*(?:<([^>]*)>)?\s*\{")
        .map_err(|e| Error::new(&format!("Failed to compile struct header regex: {}", e)))?;

    for cap in struct_header_re.captures_iter(content) {
        let name = cap_str(&cap, 1);
        let type_params = cap
            .get(2)
            .map(|m| parse_type_params(m.as_str()))
            .unwrap_or_default();

        let open_brace_idx = match cap.get(0) {
            Some(m) => m.end() - 1,
            None => continue,
        };
        let body = balanced_block_body(content, open_brace_idx).unwrap_or("");

        let fields = parse_struct_fields(body)?;

        let trait_bounds =
            extract_bounds_from_type_params(cap.get(2).map(|m| m.as_str()).unwrap_or(""));

        services.push(ServiceDef {
            name,
            language: Language::Rust,
            kind: ServiceKind::Struct,
            fields,
            methods: Vec::new(),
            variants: Vec::new(),
            type_params,
            trait_bounds,
        });
    }

    // --- Enums: pub enum Name<generics> { Variant, ... } ---
    let enum_header_re = Regex::new(r"pub\s+enum\s+(\w+)\s*(?:<([^>]*)>)?\s*\{")
        .map_err(|e| Error::new(&format!("Failed to compile enum header regex: {}", e)))?;

    for cap in enum_header_re.captures_iter(content) {
        let name = cap_str(&cap, 1);
        let type_params = cap
            .get(2)
            .map(|m| parse_type_params(m.as_str()))
            .unwrap_or_default();

        let open_brace_idx = match cap.get(0) {
            Some(m) => m.end() - 1,
            None => continue,
        };
        let body = balanced_block_body(content, open_brace_idx).unwrap_or("");

        let variants = parse_enum_variants(body)?;
        let trait_bounds =
            extract_bounds_from_type_params(cap.get(2).map(|m| m.as_str()).unwrap_or(""));

        services.push(ServiceDef {
            name,
            language: Language::Rust,
            kind: ServiceKind::Enum,
            fields: Vec::new(),
            methods: Vec::new(),
            variants,
            type_params,
            trait_bounds,
        });
    }

    // --- Traits: pub trait Name<generics> [: bounds] { fn ...; } ---
    // The trait header is matched with a regex, but the body is extracted with a
    // balanced-brace scan so that default-method bodies (with their own `{ }`)
    // and multiple methods are all captured — not just up to the first `}`.
    let trait_header_re = Regex::new(r"pub\s+trait\s+(\w+)\s*(?:<([^>]*)>)?\s*(?::[^{]*)?\{")
        .map_err(|e| Error::new(&format!("Failed to compile trait header regex: {}", e)))?;

    for cap in trait_header_re.captures_iter(content) {
        let name = cap_str(&cap, 1);
        let type_params = cap
            .get(2)
            .map(|m| parse_type_params(m.as_str()))
            .unwrap_or_default();

        // The full match ends at the opening `{`; scan from there for the body.
        let open_brace_idx = match cap.get(0) {
            Some(m) => m.end() - 1,
            None => continue,
        };
        let body = balanced_block_body(content, open_brace_idx).unwrap_or("");

        let methods = parse_rust_fn_signatures(body)?;
        let trait_bounds =
            extract_bounds_from_type_params(cap.get(2).map(|m| m.as_str()).unwrap_or(""));

        services.push(ServiceDef {
            name,
            language: Language::Rust,
            kind: ServiceKind::Trait,
            fields: Vec::new(),
            methods,
            variants: Vec::new(),
            type_params,
            trait_bounds,
        });
    }

    // --- impl blocks: attach inherent/trait-impl methods to their type ---
    // Matches `impl<...> Type<...> {` and `impl<...> Trait for Type<...> {`.
    // The captured type name (group 1) is the receiver type in both inherent and
    // `impl Trait for Type` forms. The body is extracted with a balanced-brace
    // scan so method bodies do not truncate the capture at the first `}`.
    let impl_header_re =
        Regex::new(r"impl(?:<[^>]*>)?\s+(?:[\w:]+(?:<[^>]*>)?\s+for\s+)?(\w+)(?:<[^>]*>)?\s*\{")
            .map_err(|e| Error::new(&format!("Failed to compile impl header regex: {}", e)))?;

    for cap in impl_header_re.captures_iter(content) {
        let type_name = cap_str(&cap, 1);

        let open_brace_idx = match cap.get(0) {
            Some(m) => m.end() - 1,
            None => continue,
        };
        let body = balanced_block_body(content, open_brace_idx).unwrap_or("");

        let methods = parse_rust_fn_signatures(body)?;
        if methods.is_empty() {
            continue;
        }

        // Attach to an existing struct/enum of the same name if present,
        // otherwise record a stand-alone struct-kind ServiceDef so the methods
        // are not lost (e.g. impl on a type defined in another file).
        if let Some(existing) = services.iter_mut().find(|s| s.name == type_name) {
            existing.methods.extend(methods);
        } else {
            services.push(ServiceDef {
                name: type_name,
                language: Language::Rust,
                kind: ServiceKind::Struct,
                fields: Vec::new(),
                methods,
                variants: Vec::new(),
                type_params: Vec::new(),
                trait_bounds: std::collections::HashMap::new(),
            });
        }
    }

    Ok(services)
}

/// Safely extract a capture group as an owned `String`, returning an empty
/// string when the group is absent (avoids `unwrap` on optional captures).
fn cap_str(cap: &regex::Captures<'_>, idx: usize) -> String {
    cap.get(idx)
        .map(|m| m.as_str().to_string())
        .unwrap_or_default()
}

/// Given a byte index pointing at an opening `{` in `content`, return the inner
/// body slice between that brace and its matching `}` (exclusive of both braces).
///
/// Uses a depth counter so nested braces (e.g. method bodies inside an `impl`
/// block) are spanned correctly. Returns `None` if `open_idx` is not `{` or if
/// no matching close brace is found. String/char-literal braces are NOT handled;
/// this is a deliberate simplification of the regex-based extractor.
fn balanced_block_body(content: &str, open_idx: usize) -> Option<&str> {
    let bytes = content.as_bytes();
    if open_idx >= bytes.len() || bytes[open_idx] != b'{' {
        return None;
    }

    let mut depth = 0usize;
    let body_start = open_idx + 1;
    let mut i = open_idx;
    while i < bytes.len() {
        match bytes[i] {
            b'{' => depth += 1,
            b'}' => {
                depth -= 1;
                if depth == 0 {
                    // body is everything between the opening and this closing brace
                    return content.get(body_start..i);
                }
            }
            _ => {}
        }
        i += 1;
    }

    None
}

/// Parse a comma-separated generic parameter list (the text between `<` and `>`).
///
/// Each parameter is kept as a raw string with any bound stripped to the
/// leading identifier (`T: Clone + Send` -> `T`). Lifetimes (`'a`) and const
/// generics are returned verbatim. This is intentionally shallow.
fn parse_type_params(raw: &str) -> Vec<String> {
    raw.split(',')
        .map(|p| {
            // Take the portion before any bound (`:`) and trim.
            let head = p.split(':').next().unwrap_or(p).trim();
            head.to_string()
        })
        .filter(|p| !p.is_empty())
        .collect()
}

/// Extract trait bounds from a raw type parameter string.
///
/// Given `T: Clone + Send, U, V: Default`, returns a HashMap:
/// - "T" => ["Clone", "Send"]
/// - "V" => ["Default"]
/// (U has no bounds, so it is not included in the map)
pub fn extract_bounds_from_type_params(
    raw: &str,
) -> std::collections::HashMap<String, Vec<String>> {
    use std::collections::HashMap;
    let mut bounds_map: HashMap<String, Vec<String>> = HashMap::new();

    for param_spec in raw.split(',') {
        let param_spec = param_spec.trim();
        if param_spec.is_empty() {
            continue;
        }

        // Split on `:` to separate param name from bounds
        let parts: Vec<&str> = param_spec.split(':').collect();
        if parts.len() < 2 {
            // No bounds for this param
            continue;
        }

        let param_name = parts[0].trim().to_string();
        let bounds_str = parts[1..].join(":"); // Rejoin in case of nested `:` (rare)

        // Split bounds on `+` and collect as strings
        let bounds: Vec<String> = bounds_str
            .split('+')
            .map(|b| b.trim().to_string())
            .filter(|b| !b.is_empty())
            .collect();

        if !bounds.is_empty() {
            bounds_map.insert(param_name, bounds);
        }
    }

    bounds_map
}

/// Parse enum variant names and payloads from an enum body.
///
/// Splits the body on top-level commas (commas at brace/paren/angle depth 0) so
/// payloads do not introduce spurious separators, then extracts the leading
/// identifier and any payload. Handles `Variant`, `Variant(T)`,
/// `Variant { .. }`, and `Variant = 1` forms on one line or many. Tuple/struct
/// payloads are captured as raw strings; explicit discriminants are ignored.
fn parse_enum_variants(body: &str) -> Result<Vec<Variant>> {
    let mut variants = Vec::new();

    // Leading identifier of a segment, allowing leading whitespace, attributes,
    // and doc comments to be skipped by the trim below.
    let ident_re = Regex::new(r"^\s*(?:#\[[^\]]*\]\s*)*(\w+)(.*)")
        .map_err(|e| Error::new(&format!("Failed to compile variant ident regex: {}", e)))?;

    for segment in split_top_level_commas(body) {
        let segment = segment.trim();
        if segment.is_empty() {
            continue;
        }
        if let Some(cap) = ident_re.captures(segment) {
            let name = cap_str(&cap, 1);
            // Skip lines that are clearly not variants (e.g. a stray attribute word).
            if !name.is_empty() {
                let rest = cap.get(2).map(|m| m.as_str()).unwrap_or("").trim();
                // Extract payload if present (before `=` for explicit discriminant)
                let payload = if rest.starts_with('(') || rest.starts_with('{') {
                    let payload_end = rest.find('=').unwrap_or(rest.len());
                    let raw = rest[..payload_end].trim();
                    if raw.is_empty() {
                        None
                    } else {
                        Some(raw.to_string())
                    }
                } else {
                    None
                };
                variants.push(Variant { name, payload });
            }
        }
    }

    Ok(variants)
}

/// Split a string on commas that are at the top nesting level — i.e. not inside
/// `()`, `[]`, `{}`, or `<>`. Used to separate enum variants without breaking on
/// commas inside payloads like `Variant(A, B)` or `Variant { x: u8, y: u8 }`.
fn split_top_level_commas(input: &str) -> Vec<&str> {
    let bytes = input.as_bytes();
    let mut segments = Vec::new();
    let mut depth: i32 = 0;
    let mut start = 0usize;

    for (i, &b) in bytes.iter().enumerate() {
        match b {
            b'(' | b'[' | b'{' | b'<' => depth += 1,
            b')' | b']' | b'}' | b'>' => {
                if depth > 0 {
                    depth -= 1;
                }
            }
            b',' if depth == 0 => {
                if let Some(seg) = input.get(start..i) {
                    segments.push(seg);
                }
                start = i + 1;
            }
            _ => {}
        }
    }

    if let Some(seg) = input.get(start..) {
        segments.push(seg);
    }

    segments
}

/// Parse Rust function signatures from a trait body or impl block body.
///
/// Matches `fn name(params) -> ReturnType` and `fn name(params)` (with optional
/// leading `pub`/`async`/`const`/`unsafe` qualifiers, which are simply not
/// captured). The return type is taken lazily up to the first `{`, `;`, or
/// `where`. Generic params on the fn and `where` clauses are not separately
/// modeled. The body passed in is expected to already be the full balanced block
/// (see [`balanced_block_body`]), so every method in a multi-method impl/trait is
/// visited — not just the first. Method *bodies* themselves are not parsed; only
/// signatures are recovered. A param whose type contains a top-level `,` (e.g.
/// `cb: fn(a, b)`) will be split into multiple raw param strings — a known
/// limitation of the comma-split param parsing.
fn parse_rust_fn_signatures(body: &str) -> Result<Vec<Method>> {
    let mut methods = Vec::new();

    // Qualifiers are optional and order-tolerant enough for common cases.
    // Return type captured up to `{`, `;`, or `where`.
    let fn_re = Regex::new(
        r"fn\s+(\w+)\s*(?:<[^>]*>)?\s*\(([^)]*)\)\s*(?:->\s*([^{;]+?))?\s*(?:where|\{|;)",
    )
    .map_err(|e| Error::new(&format!("Failed to compile fn regex: {}", e)))?;

    for cap in fn_re.captures_iter(body) {
        let name = cap_str(&cap, 1);
        let params_str = cap.get(2).map(|m| m.as_str()).unwrap_or("");

        let params: Vec<String> = params_str
            .split(',')
            .map(|p| p.trim().to_string())
            .filter(|p| !p.is_empty())
            .collect();

        let return_type = cap
            .get(3)
            .map(|m| m.as_str().trim().to_string())
            .filter(|s| !s.is_empty());

        methods.push(Method {
            name,
            params,
            return_type,
        });
    }

    Ok(methods)
}

/// Extract Elixir GenServer definitions from a file
///
/// Parses `defmodule Name do ... use GenServer` patterns and extracts
/// callback methods (init, handle_call, handle_cast, etc.)
///
/// # Example
///
/// ```rust,no_run
/// # use crate::reverse_sync::ast_extractor::extract_elixir_genserver;
/// let services = extract_elixir_genserver("lib/processor.ex")?;
/// assert!(!services.is_empty());
/// # Ok::<(), crate::utils::error::Error>(())
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
            kind: ServiceKind::Struct,
            fields: Vec::new(),
            methods,
            variants: Vec::new(),
            type_params: Vec::new(),
            trait_bounds: std::collections::HashMap::new(),
        });
    }

    Ok(services)
}

/// Extract callback method names from Elixir code
fn extract_elixir_callbacks(content: &str) -> Result<Vec<Method>> {
    let mut methods = Vec::new();

    // Match: def callback_name(...) do ... end
    let callback_re =
        Regex::new(r"def\s+(init|handle_call|handle_cast|handle_info|terminate)\s*\(([^)]*)\)")
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
/// # use crate::reverse_sync::ast_extractor::extract_go_service;
/// let services = extract_go_service("service.go")?;
/// assert!(!services.is_empty());
/// # Ok::<(), crate::utils::error::Error>(())
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
            kind: ServiceKind::Struct,
            fields,
            methods,
            variants: Vec::new(),
            type_params: Vec::new(),
            trait_bounds: std::collections::HashMap::new(),
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
    let method_pattern = format!(
        r"func\s*\(\s*\w+\s*\*{}\s*\)\s*(\w+)\s*\(([^)]*)\)\s*(\w+)?",
        struct_name
    );
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
/// # use crate::reverse_sync::ast_extractor::{extract_rust_service, convert_to_rdf};
/// let services = extract_rust_service("src/lib.rs")?;
/// let rdf = convert_to_rdf(&services)?;
/// println!("{}", rdf);
/// # Ok::<(), crate::utils::error::Error>(())
/// # }
/// ```
pub fn convert_to_rdf(services: &[ServiceDef]) -> Result<String> {
    let mut turtle = String::from(
        "@prefix code: <https://ggen.io/code#> .\n\
         @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\
         @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\n",
    );

    for service in services {
        let safe_name = sanitize_iri(&service.name);
        let resource_id = format!("code:{}", safe_name);

        // Construct type (struct=Service / enum=Enum / trait=Trait) and language.
        turtle.push_str(&format!(
            "{} a code:{} ;\n    code:language \"{}\" ;\n",
            resource_id,
            service.kind.rdf_class(),
            service.language.as_str()
        ));

        // Generic type parameters (captured as raw identifiers).
        if !service.type_params.is_empty() {
            for tp in &service.type_params {
                turtle.push_str(&format!("    code:typeParam \"{}\" ;\n", tp));
                // Emit trait bounds for this parameter if present
                if let Some(bounds) = service.trait_bounds.get(tp) {
                    for bound in bounds {
                        turtle.push_str(&format!("    code:traitBound \"{}\" ;\n", bound));
                    }
                }
            }
        }

        // Add fields as properties
        if !service.fields.is_empty() {
            for field in &service.fields {
                turtle.push_str(&format!(
                    "    code:hasField [ a code:Field ; code:fieldName \"{}\" ; code:fieldType \"{}\" ] ;\n",
                    field.name, field.field_type
                ));
            }
        }

        // Add enum variants
        if !service.variants.is_empty() {
            for variant in &service.variants {
                turtle.push_str(&format!(
                    "    code:hasVariant [ a code:Variant ; code:variantName \"{}\"",
                    variant.name
                ));
                if let Some(payload) = &variant.payload {
                    turtle.push_str(&format!("; code:variantPayload \"{}\"", payload));
                }
                turtle.push_str(" ] ;\n");
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
            kind: ServiceKind::Struct,
            fields: vec![Field {
                name: "id".to_string(),
                field_type: "u64".to_string(),
            }],
            methods: vec![],
            variants: vec![],
            type_params: vec![],
            trait_bounds: std::collections::HashMap::new(),
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
            kind: ServiceKind::Struct,
            fields: vec![],
            methods: vec![Method {
                name: "handle_call".to_string(),
                params: vec!["msg".to_string(), "from".to_string()],
                return_type: Some("tuple".to_string()),
            }],
            variants: vec![],
            type_params: vec![],
            trait_bounds: std::collections::HashMap::new(),
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

    #[test]
    fn test_parse_type_params_strips_bounds() {
        let params = parse_type_params("T: Clone + Send, U, V: Default");
        assert_eq!(
            params,
            vec!["T".to_string(), "U".to_string(), "V".to_string()]
        );
    }

    #[test]
    fn test_parse_enum_variants() {
        let body = "Active,\n    Inactive,\n    Pending(u32),\n    Custom { code: u16 },\n";
        let variants = parse_enum_variants(body).unwrap();
        let names: Vec<&str> = variants.iter().map(|v| v.name.as_str()).collect();
        assert!(names.contains(&"Active"));
        assert!(names.contains(&"Inactive"));
        assert!(names.contains(&"Pending"));
        assert!(names.contains(&"Custom"));

        // Verify payloads are captured
        let pending = variants
            .iter()
            .find(|v| v.name == "Pending")
            .expect("Pending variant");
        assert!(pending.payload.is_some());
        assert!(pending.payload.as_ref().unwrap().contains("u32"));

        let custom = variants
            .iter()
            .find(|v| v.name == "Custom")
            .expect("Custom variant");
        assert!(custom.payload.is_some());
        assert!(custom.payload.as_ref().unwrap().contains("code"));
    }

    #[test]
    fn test_parse_rust_fn_signatures() {
        let body = "fn start(&self) -> bool { true }\n    fn stop(&mut self, force: bool) { }\n";
        let methods = parse_rust_fn_signatures(body).unwrap();
        let start = methods
            .iter()
            .find(|m| m.name == "start")
            .expect("start fn");
        assert_eq!(start.return_type.as_deref(), Some("bool"));
        let stop = methods.iter().find(|m| m.name == "stop").expect("stop fn");
        assert!(stop.params.iter().any(|p| p.contains("force")));
    }

    #[test]
    fn test_extract_rust_struct_with_generics() {
        let src = "pub struct Container<T> { item: T, count: usize }";
        let services = extract_rust_service_from_str(src).unwrap();
        let c = services
            .iter()
            .find(|s| s.name == "Container")
            .expect("Container");
        assert_eq!(c.kind, ServiceKind::Struct);
        assert!(c.type_params.contains(&"T".to_string()));
        assert!(c.fields.iter().any(|f| f.name == "item"));
    }

    #[test]
    fn test_extract_rust_enum() {
        let src = "pub enum Status { Ok, Err, Pending }";
        let services = extract_rust_service_from_str(src).unwrap();
        let e = services
            .iter()
            .find(|s| s.name == "Status")
            .expect("Status enum");
        assert_eq!(e.kind, ServiceKind::Enum);
        let names: Vec<&str> = e.variants.iter().map(|v| v.name.as_str()).collect();
        assert!(names.contains(&"Ok"));
        assert!(names.contains(&"Err"));
        assert!(names.contains(&"Pending"));
    }

    #[test]
    fn test_extract_rust_trait() {
        let src = "pub trait Runnable { fn run(&self) -> i32; fn name(&self) -> String; }";
        let services = extract_rust_service_from_str(src).unwrap();
        let t = services
            .iter()
            .find(|s| s.name == "Runnable")
            .expect("Runnable trait");
        assert_eq!(t.kind, ServiceKind::Trait);
        assert!(t.methods.iter().any(|m| m.name == "run"));
        assert!(t.methods.iter().any(|m| m.name == "name"));
    }

    #[test]
    fn test_impl_methods_attached_to_struct() {
        let src = "pub struct Worker { id: u32 }\n\
                   impl Worker { fn process(&self) -> bool { true } fn reset(&mut self) { } }";
        let services = extract_rust_service_from_str(src).unwrap();
        let w = services
            .iter()
            .find(|s| s.name == "Worker")
            .expect("Worker");
        assert_eq!(w.kind, ServiceKind::Struct);
        assert!(w.methods.iter().any(|m| m.name == "process"));
        assert!(w.methods.iter().any(|m| m.name == "reset"));
    }
}
