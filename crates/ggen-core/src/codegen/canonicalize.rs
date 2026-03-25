//! μ₄: Code Canonicalization
//!
//! This module provides canonicalization (formatting) for generated code.
//! It ensures deterministic output through:
//! - Import sorting (alphabetically)
//! - Whitespace normalization
//! - Generated header injection
//! - Language-specific formatting patterns
//!
//! ## A2A-RS Integration
//!
//! For A2A-RS generated code, this module:
//! 1. Sorts imports alphabetically (std → external → internal)
//! 2. Normalizes line endings to LF
//! 3. Removes trailing whitespace
//! 4. Ensures final newline
//! 5. Injects generated code header with warning

use ggen_utils::error::{Error, Result};
use regex::Regex;
use std::collections::BTreeSet;
use std::path::Path;

/// Canonicalization options for generated code
#[derive(Debug, Clone, Default)]
pub struct CanonicalizeOptions {
    /// Whether to sort imports
    pub sort_imports: bool,

    /// Whether to normalize line endings
    pub normalize_line_endings: bool,

    /// Whether to inject generated header
    pub inject_header: bool,

    /// Custom header content (if None, uses default)
    pub custom_header: Option<String>,

    /// Whether to remove trailing whitespace
    pub trim_trailing_whitespace: bool,
}

impl CanonicalizeOptions {
    /// Create new options with defaults
    pub fn new() -> Self {
        Self::default()
    }

    /// Enable import sorting
    pub fn with_sort_imports(mut self, enabled: bool) -> Self {
        self.sort_imports = enabled;
        self
    }

    /// Enable line ending normalization
    pub fn with_normalize_line_endings(mut self, enabled: bool) -> Self {
        self.normalize_line_endings = enabled;
        self
    }

    /// Enable header injection
    pub fn with_inject_header(mut self, enabled: bool) -> Self {
        self.inject_header = enabled;
        self
    }

    /// Set custom header
    pub fn with_custom_header(mut self, header: String) -> Self {
        self.custom_header = Some(header);
        self
    }

    /// Enable trailing whitespace removal
    pub fn with_trim_trailing_whitespace(mut self, enabled: bool) -> Self {
        self.trim_trailing_whitespace = enabled;
        self
    }
}

/// A parsed Rust import statement
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct RustImport {
    /// The full import line
    full_line: String,
    /// The import path (e.g., "std::collections::HashMap")
    path: String,
    /// Import type: use, mod, extern crate
    import_type: String,
    /// Import group: std, external, or internal
    group: ImportGroup,
}

/// Import group for sorting
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ImportGroup {
    /// std library imports
    Std,
    /// External crate imports
    External,
    /// Internal/crate imports
    Internal,
}

impl RustImport {
    /// Parse a use statement
    fn parse(line: &str) -> Option<Self> {
        let trimmed = line.trim();

        // Match `use` statements
        if !trimmed.starts_with("use ") {
            return None;
        }

        // Remove `use ` prefix and `;` suffix
        let path_part = trimmed[4..].trim_end_matches(';').trim();

        // Skip self/super/crate aliases
        if path_part.starts_with("self::")
            || path_part.starts_with("super::")
            || path_part.starts_with("crate::")
        {
            return None;
        }

        // Determine group
        let group = if path_part.starts_with("std::")
            || path_part.starts_with("core::")
            || path_part.starts_with("alloc::")
        {
            ImportGroup::Std
        } else if path_part.contains("::") {
            // Check if it's an external crate (heuristic: no relative path)
            ImportGroup::External
        } else {
            ImportGroup::Internal
        };

        Some(RustImport {
            full_line: trimmed.to_string(),
            path: path_part.to_string(),
            import_type: "use".to_string(),
            group,
        })
    }

    /// Get the sorting key for this import
    fn sort_key(&self) -> (ImportGroup, String) {
        (self.group, self.path.clone())
    }
}

/// Canonicalize generated Rust code
///
/// # Arguments
/// * `content` - The generated code to canonicalize
/// * `options` - Canonicalization options
///
/// # Returns
/// Canonicalized code string
pub fn canonicalize(content: &str, options: &CanonicalizeOptions) -> Result<String> {
    let mut result = content.to_string();

    // Step 1: Normalize line endings
    if options.normalize_line_endings {
        result = normalize_line_endings(&result);
    }

    // Step 2: Trim trailing whitespace
    if options.trim_trailing_whitespace {
        result = trim_trailing_whitespace(&result);
    }

    // Step 3: Sort imports (before header injection)
    if options.sort_imports {
        result = sort_rust_imports(&result)?;
    }

    // Step 4: Inject header (last, so it's at the very top)
    if options.inject_header {
        let header = options
            .custom_header
            .as_ref()
            .cloned()
            .unwrap_or_else(default_generated_header);
        result = inject_header(&result, &header);
    }

    // Ensure final newline
    if !result.ends_with('\n') {
        result.push('\n');
    }

    Ok(result)
}

/// Normalize line endings to LF
fn normalize_line_endings(content: &str) -> String {
    content.replace("\r\n", "\n").replace('\r', "\n")
}

/// Trim trailing whitespace from each line
fn trim_trailing_whitespace(content: &str) -> String {
    content
        .lines()
        .map(|line| line.trim_end())
        .collect::<Vec<_>>()
        .join("\n")
}

/// Sort Rust imports alphabetically within groups (std → external → internal)
fn sort_rust_imports(content: &str) -> Result<String> {
    let lines: Vec<&str> = content.lines().collect();

    // Find import blocks
    let mut result = String::new();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i];

        // Start of import block?
        if line.trim().starts_with("use ") {
            // Collect all consecutive imports
            let mut imports = Vec::new();

            while i < lines.len() {
                let trimmed = lines[i].trim();

                // Check if this is still an import line or comment
                if trimmed.starts_with("use ") {
                    if let Some(import) = RustImport::parse(lines[i]) {
                        imports.push(import);
                    } else {
                        // Couldn't parse, keep as-is
                        result.push_str(lines[i]);
                        result.push('\n');
                    }
                    i += 1;
                } else if trimmed.is_empty() || trimmed.starts_with("//") {
                    // Blank lines and comments break import blocks
                    i += 1;
                    break;
                } else {
                    break;
                }
            }

            // Sort imports: std → external → internal
            imports.sort_by_key(|imp| imp.sort_key());

            // Group by import type for better organization
            let mut std_imports: BTreeSet<String> = BTreeSet::new();
            let mut external_imports: BTreeSet<String> = BTreeSet::new();
            let mut internal_imports: BTreeSet<String> = BTreeSet::new();

            for imp in &imports {
                match imp.group {
                    ImportGroup::Std => {
                        std_imports.insert(imp.full_line.clone());
                    }
                    ImportGroup::External => {
                        external_imports.insert(imp.full_line.clone());
                    }
                    ImportGroup::Internal => {
                        internal_imports.insert(imp.full_line.clone());
                    }
                }
            }

            // Write sorted imports
            for imp in &std_imports {
                result.push_str(imp);
                result.push('\n');
            }
            if !std_imports.is_empty() && !external_imports.is_empty() {
                result.push('\n');
            }
            for imp in &external_imports {
                result.push_str(imp);
                result.push('\n');
            }
            if !external_imports.is_empty() && !internal_imports.is_empty() {
                result.push('\n');
            }
            for imp in &internal_imports {
                result.push_str(imp);
                result.push('\n');
            }

            // Add blank line after import block if next line is not blank
            if i < lines.len() && !lines[i].trim().is_empty() {
                result.push('\n');
            }

            continue;
        }

        // Non-import line
        result.push_str(line);
        result.push('\n');
        i += 1;
    }

    Ok(result)
}

/// Inject generated header at the top of the file
fn inject_header(content: &str, header: &str) -> String {
    // Check if header already exists
    let has_header = content
        .lines()
        .take(5)
        .any(|line| line.contains("DO NOT EDIT") || line.contains("Generated by"));

    if has_header {
        return content.to_string();
    }

    // Check for shebang
    let (shebang, rest) = if content.starts_with("#!") {
        let lines: Vec<&str> = content.lines().collect();
        if let Some(idx) = lines.iter().position(|l| l.starts_with("#!")) {
            let shebang_line = lines[idx];
            let remaining = lines[idx + 1..].join("\n");
            (Some(shebang_line.to_string()), remaining)
        } else {
            (None, content.to_string())
        }
    } else {
        (None, content.to_string())
    };

    let header = format!("{}\n", header);

    match shebang {
        Some(s) => format!("{}\n{}{}", s, header, rest),
        None => format!("{}{}", header, content),
    }
}

/// Default generated code header
fn default_generated_header() -> String {
    "// DO NOT EDIT\n\
     // This code was generated by ggen (https://github.com/seanchatmangpt/ggen)\n\
     //\n\
     // Changes to this file will be overwritten when the code is regenerated.\n\
     // Please modify the source ontology (.ttl files) and templates instead.\n\
     //\n\
     // Generated at: "
        .to_string()
        + &chrono::Utc::now().to_rfc3339()
}

/// Get the generated header with timestamp
pub fn get_generated_header() -> String {
    default_generated_header()
}

/// Canonicalize a file by path
///
/// # Arguments
/// * `file_path` - Path to the file (used for format detection)
/// * `content` - The file content
/// * `options` - Canonicalization options
///
/// # Returns
/// Canonicalized content
pub fn canonicalize_file(
    file_path: &Path, content: &str, options: &CanonicalizeOptions,
) -> Result<String> {
    // Detect file type from extension
    let ext = file_path.extension().and_then(|e| e.to_str()).unwrap_or("");

    match ext {
        "rs" => canonicalize_rust(content, options),
        "toml" => canonicalize_toml(content, options),
        "json" => canonicalize_json(content, options),
        "ttl" | "turtle" => canonicalize_ttl(content, options),
        _ => canonicalize_generic(content, options),
    }
}

/// Canonicalize Rust code
fn canonicalize_rust(content: &str, options: &CanonicalizeOptions) -> Result<String> {
    canonicalize(content, options)
}

/// Canonicalize TOML (basic: normalize line endings)
fn canonicalize_toml(content: &str, options: &CanonicalizeOptions) -> Result<String> {
    let mut result = content.to_string();

    if options.normalize_line_endings {
        result = normalize_line_endings(&result);
    }

    if options.trim_trailing_whitespace {
        result = trim_trailing_whitespace(&result);
    }

    if !result.ends_with('\n') {
        result.push('\n');
    }

    Ok(result)
}

/// Canonicalize JSON
fn canonicalize_json(content: &str, options: &CanonicalizeOptions) -> Result<String> {
    if !options.normalize_line_endings && !options.trim_trailing_whitespace {
        // Return as-is if no formatting requested
        return Ok(content.to_string());
    }

    // Parse and pretty-print JSON
    let value: serde_json::Value =
        serde_json::from_str(content).map_err(|e| Error::new(&format!("Invalid JSON: {}", e)))?;

    let mut result = serde_json::to_string_pretty(&value)
        .map_err(|e| Error::new(&format!("JSON serialization error: {}", e)))?;

    if !result.ends_with('\n') {
        result.push('\n');
    }

    Ok(result)
}

/// Canonicalize Turtle/RDF (basic: normalize line endings)
fn canonicalize_ttl(content: &str, options: &CanonicalizeOptions) -> Result<String> {
    let mut result = content.to_string();

    if options.normalize_line_endings {
        result = normalize_line_endings(&result);
    }

    if options.trim_trailing_whitespace {
        result = trim_trailing_whitespace(&result);
    }

    // Remove consecutive blank lines
    let re = Regex::new(r"\n{3,}").map_err(|e| Error::new(&format!("Regex error: {}", e)))?;
    result = re.replace_all(&result, "\n\n").to_string();

    if !result.ends_with('\n') {
        result.push('\n');
    }

    Ok(result)
}

/// Generic canonicalization (line endings + whitespace only)
fn canonicalize_generic(content: &str, options: &CanonicalizeOptions) -> Result<String> {
    let mut result = content.to_string();

    if options.normalize_line_endings {
        result = normalize_line_endings(&result);
    }

    if options.trim_trailing_whitespace {
        result = trim_trailing_whitespace(&result);
    }

    if !result.ends_with('\n') {
        result.push('\n');
    }

    Ok(result)
}

/// Quick canonicalize with default options for Rust files
pub fn canonicalize_rust_quick(content: &str) -> Result<String> {
    let options = CanonicalizeOptions::new()
        .with_sort_imports(true)
        .with_normalize_line_endings(true)
        .with_trim_trailing_whitespace(true)
        .with_inject_header(false); // Header injected separately if needed

    canonicalize_rust(content, &options)
}

/// Canonicalize with A2A-RS standard options
///
/// This is the recommended canonicalization for A2A-RS generated code.
pub fn canonicalize_a2a(content: &str) -> Result<String> {
    let options = CanonicalizeOptions::new()
        .with_sort_imports(true)
        .with_normalize_line_endings(true)
        .with_trim_trailing_whitespace(true)
        .with_inject_header(true);

    canonicalize_rust(content, &options)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_normalize_line_endings() {
        let input = "hello\r\nworld\r";
        let result = normalize_line_endings(input);
        assert_eq!(result, "hello\nworld\n");
    }

    #[test]
    fn test_trim_trailing_whitespace() {
        let input = "hello   \nworld  \n  test  \n";
        let result = trim_trailing_whitespace(input);
        assert_eq!(result, "hello\nworld\n  test\n");
    }

    #[test]
    fn test_inject_header() {
        let content = "fn main() {}\n";
        let header = "// Generated header\n";
        let result = inject_header(content, header);
        assert!(result.starts_with("// Generated header\n"));
        assert!(result.contains("fn main() {}\n"));
    }

    #[test]
    fn test_inject_header_preserves_existing() {
        let content = "// DO NOT EDIT this file\nfn main() {}\n";
        let header = "// Generated header\n";
        let result = inject_header(content, header);
        // Should not duplicate
        assert_eq!(result, content);
    }

    #[test]
    fn test_rust_import_parse() {
        let line = "use std::collections::HashMap;";
        let import = RustImport::parse(line);
        assert!(import.is_some());
        let imp = import.unwrap();
        assert_eq!(imp.path, "std::collections::HashMap");
        assert_eq!(imp.group, ImportGroup::Std);
    }

    #[test]
    fn test_rust_import_sorting() {
        let input = r#"
use crate::module::inner::Struct;
use std::collections::HashMap;
use external_crate::Trait;
use std::vec::Vec;
use crate::module::Type;

fn main() {}
"#;

        let result = sort_rust_imports(input).unwrap();
        let lines: Vec<&str> = result.lines().collect();

        // Find import positions
        let std_idx = lines.iter().position(|l| l.contains("std::collections"));
        let vec_idx = lines.iter().position(|l| l.contains("std::vec"));
        let external_idx = lines.iter().position(|l| l.contains("external_crate"));
        let internal_idx = lines.iter().position(|l| l.contains("crate::module"));

        // std imports should come before external
        assert!(std_idx.unwrap() < external_idx.unwrap());
        // external should come before internal
        assert!(external_idx.unwrap() < internal_idx.unwrap());

        // std imports should be sorted
        assert!(std_idx.unwrap() < vec_idx.unwrap());
    }

    #[test]
    fn test_canonicalize_rust() {
        let input = r#"use std::b::B;
use std::a::A;
fn main() { println!("hello"); }"#;

        let options = CanonicalizeOptions::new()
            .with_sort_imports(true)
            .with_normalize_line_endings(true);

        let result = canonicalize(input, &options).unwrap();

        // std::a should come before std::b
        let a_pos = result.find("use std::a").unwrap();
        let b_pos = result.find("use std::b").unwrap();
        assert!(a_pos < b_pos);
    }

    #[test]
    fn test_canonicalize_a2a() {
        let input = r#"use std::b::B;
use std::a::A;

fn main() {}
"#;

        let result = canonicalize_a2a(input).unwrap();

        // Should have header
        assert!(result.contains("DO NOT EDIT"));
        assert!(result.contains("Generated by ggen"));

        // std::a should come before std::b
        let a_pos = result.find("use std::a").unwrap();
        let b_pos = result.find("use std::b").unwrap();
        assert!(a_pos < b_pos);
    }

    #[test]
    fn test_canonicalize_json() {
        let input = r#"{"a":1,"b":2}"#;
        let options = CanonicalizeOptions::new();
        let result = canonicalize_json(input, &options).unwrap();
        assert!(result.contains("\n")); // Pretty-printed
        assert!(result.ends_with('\n'));
    }

    #[test]
    fn test_canonicalize_toml() {
        let input = "[package]\r\nname = \"test\"\r\n";
        let options = CanonicalizeOptions::new().with_normalize_line_endings(true);
        let result = canonicalize_toml(input, &options).unwrap();
        assert!(!result.contains("\r\n"));
        assert_eq!(result, "[package]\nname = \"test\"\n");
    }

    #[test]
    fn test_import_group_detection() {
        assert_eq!(
            RustImport::parse("use std::collections::HashMap;")
                .unwrap()
                .group,
            ImportGroup::Std
        );
        assert_eq!(
            RustImport::parse("use serde::Serialize;").unwrap().group,
            ImportGroup::External
        );
        assert_eq!(
            RustImport::parse("use crate::module::Struct;")
                .unwrap()
                .group,
            ImportGroup::Internal
        );
    }

    #[test]
    fn test_self_import_skipped() {
        // self imports should not be sorted
        let result = RustImport::parse("use self::Struct;");
        assert!(result.is_none());
    }

    #[test]
    fn test_canonicalize_quick() {
        let input = r#"use std::b::B;
use std::a::A;
"#;

        let result = canonicalize_rust_quick(input).unwrap();

        // std::a should come before std::b
        let a_pos = result.find("use std::a").unwrap();
        let b_pos = result.find("use std::b").unwrap();
        assert!(a_pos < b_pos);

        // Should end with newline
        assert!(result.ends_with('\n'));
    }
}
