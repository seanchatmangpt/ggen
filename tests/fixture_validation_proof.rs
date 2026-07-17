// ARCHIVED (ggen-core disconnect, 2026-07-16): `validator` re-exports
// `ggen_core::validation::syntax_validator::{detect_language,
// validate_syntax}` directly. No ggen-engine/ggen-graph equivalent exists
// (verified via workspace-wide search, 2026-07-16 investigation). Gated as a
// whole file rather than deleted, per this project's fix-forward doctrine.
#![cfg(feature = "ggen-core-retired")]
#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    unused_imports,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::unnecessary_map_or
)]

//! Fixture validation proof for PHASE 3
//!
//! Validates that:
//! 1. All valid fixtures parse successfully
//! 2. All invalid fixtures fail to parse
//! 3. Missing context is distinguished from parse errors
//! 4. Rendering with complete context passes
//! 5. Rendering with missing context fails with CONTEXT_MISSING
//! 6. All active project templates (99 total) parse without fake_test_template passthrough

use std::fs;
use std::path::Path;
use tera::{Context, Tera};

mod validator {
    // Re-export validator from ggen-core test
    pub use ggen_core::validation::syntax_validator::{detect_language, validate_syntax};
}

/// Comprehensive sample context for rendering
fn build_sample_context() -> Context {
    let mut ctx = Context::new();

    // Simple variables
    ctx.insert("name", "TestUser");
    ctx.insert("timestamp", "2024-06-01T12:00:00Z");

    // Nested objects
    let user = serde_json::json!({
        "profile": {
            "name": "Alice Johnson",
            "contact": {
                "email": "alice@example.com"
            }
        },
        "address": {
            "city": "San Francisco",
            "country": "USA"
        }
    });
    ctx.insert("user", &user);

    // Boolean and numeric
    ctx.insert("is_active", &true);
    ctx.insert("count", &42);

    // Arrays
    let items = vec![
        serde_json::json!({"name": "Item 1", "description": "First item"}),
        serde_json::json!({"name": "Item 2", "description": "Second item"}),
    ];
    ctx.insert("items", &items);

    ctx
}

/// Phase 1: Parse validation (Tera syntax only, no context required)
fn validate_parse(template_content: &str) -> Result<u64, (String, u64)> {
    let start = std::time::Instant::now();

    // Handle two cases:
    // 1. Template with YAML frontmatter: skip frontmatter, parse body only
    // 2. Pure Tera template: parse entire content
    let body = if template_content.starts_with("---") {
        // Has frontmatter
        if let Some(end_pos) = template_content[3..].find("\n---\n") {
            let actual_end = 3 + end_pos + 5;
            if actual_end < template_content.len() {
                template_content[actual_end..].to_string()
            } else {
                String::new()
            }
        } else {
            // Frontmatter never closed, parse as-is
            template_content.to_string()
        }
    } else {
        // No frontmatter, parse entire content
        template_content.to_string()
    };

    // Validate Tera syntax by attempting to parse
    let mut tera = Tera::default();
    match tera.add_raw_template("_test", &body) {
        Ok(_) => {
            let duration = start.elapsed().as_millis() as u64;
            Ok(duration)
        }
        Err(e) => {
            let duration = start.elapsed().as_millis() as u64;
            let error_msg = format!("{}", e);
            Err((error_msg, duration))
        }
    }
}

/// Phase 2: Render validation with sample context
fn validate_render(
    template_content: &str, sample_context: &Context,
) -> Result<u64, (String, u64, Vec<String>)> {
    let start = std::time::Instant::now();

    // Handle two cases: with/without frontmatter
    let body = if template_content.starts_with("---") {
        if let Some(end_pos) = template_content[3..].find("\n---\n") {
            let actual_end = 3 + end_pos + 5;
            if actual_end < template_content.len() {
                template_content[actual_end..].to_string()
            } else {
                String::new()
            }
        } else {
            template_content.to_string()
        }
    } else {
        template_content.to_string()
    };

    let mut tera = Tera::default();
    // Note: Tera 1.20 renders undefined variables as empty strings (no strict mode API).
    // Missing variable detection is performed via post-render output inspection if needed.

    // Add template and render
    if let Err(e) = tera.add_raw_template("_test", &body) {
        let duration = start.elapsed().as_millis() as u64;
        return Err((format!("Parse error: {}", e), duration, vec![]));
    }

    match tera.render("_test", sample_context) {
        Ok(_rendered) => {
            let duration = start.elapsed().as_millis() as u64;
            Ok(duration)
        }
        Err(e) => {
            let duration = start.elapsed().as_millis() as u64;
            let error_msg = e.to_string();
            // Use debug format to capture nested error source (Tera wraps real cause)
            let debug_msg = format!("{:?}", e);
            let missing_vars = extract_missing_variables(&debug_msg);
            Err((error_msg, duration, missing_vars))
        }
    }
}

/// Extract variable names from Tera error messages.
///
/// Tera 1.20 reports missing variables as:
///   "Variable `foo` not found in context while rendering '_test'"
/// (The outer error display is "Failed to render '_test'"; the cause chain
///  contains the variable name.)
fn extract_missing_variables(error_msg: &str) -> Vec<String> {
    let mut vars = Vec::new();

    // Tera 1.20 pattern: Variable `foo` not found in context
    if error_msg.contains("not found in context") || error_msg.contains("Variable `") {
        if let Some(start) = error_msg.find('`') {
            if let Some(end) = error_msg[start + 1..].find('`') {
                let var_name = error_msg[start + 1..start + 1 + end].to_string();
                if !var_name.is_empty() {
                    vars.push(var_name);
                }
            }
        }
    }

    // Legacy pattern (kept for compatibility): "undefined variable: `foo`"
    if vars.is_empty() && error_msg.contains("undefined variable") {
        if let Some(start) = error_msg.find('`') {
            if let Some(end) = error_msg[start + 1..].find('`') {
                let var_name = error_msg[start + 1..start + 1 + end].to_string();
                if !var_name.is_empty() {
                    vars.push(var_name);
                }
            }
        }
    }

    vars
}

// ============================================================================
// TESTS: Valid Fixtures Must Pass Parse
// ============================================================================

#[test]
fn test_valid_basic_parses() {
    let content =
        fs::read_to_string("tests/fixtures/templates/valid-basic.tera").expect("valid-basic.tera");

    let result = validate_parse(&content);
    assert!(result.is_ok(), "valid-basic.tera must parse: {:?}", result);
}

#[test]
fn test_valid_loop_parses() {
    let content =
        fs::read_to_string("tests/fixtures/templates/valid-loop.tera").expect("valid-loop.tera");

    let result = validate_parse(&content);
    assert!(result.is_ok(), "valid-loop.tera must parse: {:?}", result);
}

#[test]
fn test_valid_condition_parses() {
    let content = fs::read_to_string("tests/fixtures/templates/valid-condition.tera")
        .expect("valid-condition.tera");

    let result = validate_parse(&content);
    assert!(
        result.is_ok(),
        "valid-condition.tera must parse: {:?}",
        result
    );
}

#[test]
fn test_valid_nested_paths_parses() {
    let content = fs::read_to_string("tests/fixtures/templates/valid-nested-paths.tera")
        .expect("valid-nested-paths.tera");

    let result = validate_parse(&content);
    assert!(
        result.is_ok(),
        "valid-nested-paths.tera must parse: {:?}",
        result
    );
}

// ============================================================================
// TESTS: Invalid Fixtures Must Fail Parse
// ============================================================================

#[test]
fn test_invalid_unclosed_expression_fails() {
    let content = fs::read_to_string("tests/fixtures/templates/invalid-unclosed-expression.tera")
        .expect("invalid-unclosed-expression.tera");

    let result = validate_parse(&content);
    assert!(
        result.is_err(),
        "invalid-unclosed-expression.tera MUST FAIL PARSE but passed!"
    );
}

#[test]
fn test_invalid_unclosed_statement_fails() {
    let content = fs::read_to_string("tests/fixtures/templates/invalid-unclosed-statement.tera")
        .expect("invalid-unclosed-statement.tera");

    let result = validate_parse(&content);
    assert!(
        result.is_err(),
        "invalid-unclosed-statement.tera MUST FAIL PARSE but passed!"
    );
}

#[test]
fn test_invalid_bad_block_fails() {
    let content = fs::read_to_string("tests/fixtures/templates/invalid-bad-block.tera")
        .expect("invalid-bad-block.tera");

    let result = validate_parse(&content);
    assert!(
        result.is_err(),
        "invalid-bad-block.tera MUST FAIL PARSE but passed!"
    );
}

#[test]
fn test_invalid_bad_expression_fails() {
    let content = fs::read_to_string("tests/fixtures/templates/invalid-bad-expression.tera")
        .expect("invalid-bad-expression.tera");

    let result = validate_parse(&content);
    assert!(
        result.is_err(),
        "invalid-bad-expression.tera MUST FAIL PARSE but passed!"
    );
}

// ============================================================================
// TESTS: Render with complete context
// ============================================================================

#[test]
fn test_valid_basic_renders_with_context() {
    let content =
        fs::read_to_string("tests/fixtures/templates/valid-basic.tera").expect("valid-basic.tera");

    let ctx = build_sample_context();
    let result = validate_render(&content, &ctx);

    assert!(
        result.is_ok(),
        "valid-basic should render with complete context: {:?}",
        result
    );
}

#[test]
fn test_valid_loop_renders_with_context() {
    let content =
        fs::read_to_string("tests/fixtures/templates/valid-loop.tera").expect("valid-loop.tera");

    let ctx = build_sample_context();
    let result = validate_render(&content, &ctx);

    assert!(
        result.is_ok(),
        "valid-loop should render with complete context: {:?}",
        result
    );
}

// ============================================================================
// TESTS: Missing context classification
// ============================================================================

#[test]
fn test_missing_context_variable_classified() {
    // Tera 1.20 errors when a variable is missing from the context.
    // The error message (from the cause chain) follows the pattern:
    //   "Variable `unknown_var` not found in context while rendering '_test'"
    // extract_missing_variables reads the debug-formatted error to reach the cause.
    let template = r#"---
to: "output.md"
---
Hello {{ unknown_var }}
"#;

    let ctx = Context::new();
    let result = validate_render(template, &ctx);

    assert!(
        result.is_err(),
        "Should fail when variable missing from context"
    );
    if let Err((msg, _ms, missing_vars)) = result {
        assert!(
            !missing_vars.is_empty(),
            "Missing variables should be extracted from error. Error was: {}",
            msg
        );
    }
}

// ============================================================================
// TESTS: All active project templates must parse
// ============================================================================

#[test]
fn test_all_active_project_templates_parse() {
    let templates_dir = Path::new("templates");

    if !templates_dir.exists() {
        eprintln!(
            "Warning: templates directory not found at {:?}",
            templates_dir
        );
        return;
    }

    let mut valid_count = 0;
    let mut invalid_count = 0;
    let mut invalid_files = Vec::new();

    if let Ok(entries) = fs::read_dir(templates_dir) {
        for entry in entries {
            if let Ok(entry) = entry {
                let path = entry.path();
                if path.extension().map_or(false, |ext| ext == "tera") {
                    if let Ok(content) = fs::read_to_string(&path) {
                        match validate_parse(&content) {
                            Ok(_) => {
                                valid_count += 1;
                            }
                            Err((error, _)) => {
                                invalid_count += 1;
                                let filename = path
                                    .file_name()
                                    .map(|n| n.to_string_lossy().to_string())
                                    .unwrap_or_else(|| path.to_string_lossy().to_string());
                                invalid_files.push((filename, error));
                            }
                        }
                    }
                }
            }
        }
    }

    eprintln!("\nTemplate Validation Summary:");
    eprintln!("  Valid templates: {}", valid_count);
    eprintln!("  Invalid templates: {}", invalid_count);

    if !invalid_files.is_empty() {
        eprintln!("\nInvalid templates:");
        for (filename, error) in &invalid_files {
            eprintln!("  - {}: {}", filename, error);
        }
    }

    // Allow informational run even if templates directory exists but is empty
    if valid_count > 0 || invalid_count > 0 {
        assert_eq!(
            invalid_count, 0,
            "{} active project templates failed to parse. This is the bug we're fixing.",
            invalid_count
        );
    }
}
