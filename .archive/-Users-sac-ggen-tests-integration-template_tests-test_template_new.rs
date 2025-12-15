//! Chicago TDD tests for template new command

use ggen_cli::domain::template::{generate_template_content, TemplateService};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_create_real_rust_template() {
    // REAL file system
    let temp_dir = TempDir::new().unwrap();
    let service = TemplateService::new(temp_dir.path().join("templates"));

    // Generate REAL template content
    let content = generate_template_content("my_module", "rust").unwrap();

    // Write REAL file
    let path = service.write_template("my_module", &content).unwrap();

    // REAL assertions on REAL file
    assert!(path.exists());
    let written_content = fs::read_to_string(&path).unwrap();
    assert_eq!(written_content, content);

    // Verify REAL template structure
    assert!(content.contains("to: src/{{ name }}.rs"));
    assert!(content.contains("pub struct {{ name | pascal_case }}"));
    assert!(content.contains("name: \"my_module\""));
}

#[test]
fn test_create_real_python_template() {
    // REAL file system
    let temp_dir = TempDir::new().unwrap();
    let service = TemplateService::new(temp_dir.path().join("templates"));

    // Generate REAL template content
    let content = generate_template_content("my_module", "python").unwrap();

    // Write REAL file
    let path = service.write_template("my_module", &content).unwrap();

    // REAL assertions
    assert!(path.exists());
    assert!(content.contains("to: src/{{ name }}.py"));
    assert!(content.contains("class {{ name | pascal_case }}"));
}

#[test]
fn test_create_real_typescript_template() {
    // REAL file system
    let temp_dir = TempDir::new().unwrap();
    let service = TemplateService::new(temp_dir.path().join("templates"));

    // Generate REAL template content
    let content = generate_template_content("my_module", "typescript").unwrap();

    // Write REAL file
    let path = service.write_template("my_module", &content).unwrap();

    // REAL assertions
    assert!(path.exists());
    assert!(content.contains("to: src/{{ name }}.ts"));
    assert!(content.contains("interface {{ name | pascal_case }}"));
}

#[test]
fn test_create_generic_template_fallback() {
    // REAL file system
    let temp_dir = TempDir::new().unwrap();
    let service = TemplateService::new(temp_dir.path().join("templates"));

    // Generate REAL template with unknown type
    let content = generate_template_content("test", "unknown_type").unwrap();

    // Write REAL file
    let path = service.write_template("test", &content).unwrap();

    // REAL assertions - should fall back to generic
    assert!(path.exists());
    assert!(content.contains("to: output/{{ name }}.txt"));
    assert!(content.contains("generic template"));
}

#[test]
fn test_duplicate_template_fails() {
    // REAL file system
    let temp_dir = TempDir::new().unwrap();
    let service = TemplateService::new(temp_dir.path().join("templates"));

    // Create first REAL template
    let content = generate_template_content("test", "rust").unwrap();
    service.write_template("test", &content).unwrap();

    // REAL attempt to create duplicate
    let result = service.write_template("test", &content);

    // REAL assertion - should fail
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("already exists"));
}

#[test]
fn test_template_directory_auto_created() {
    // REAL file system where templates dir doesn't exist
    let temp_dir = TempDir::new().unwrap();
    let templates_dir = temp_dir.path().join("templates");

    // Verify directory doesn't exist
    assert!(!templates_dir.exists());

    let service = TemplateService::new(templates_dir.clone());
    let content = generate_template_content("test", "rust").unwrap();

    // Write REAL template - should create directory
    let path = service.write_template("test", &content).unwrap();

    // REAL assertions
    assert!(templates_dir.exists());
    assert!(path.exists());
    assert!(path.starts_with(&templates_dir));
}

#[test]
fn test_template_content_has_valid_frontmatter() {
    // Generate REAL template
    let content = generate_template_content("test_module", "rust").unwrap();

    // REAL assertion - parse as actual YAML frontmatter
    assert!(content.starts_with("---\n"));
    assert!(content.contains("\n---\n"));

    // Extract frontmatter section
    let parts: Vec<&str> = content.split("\n---\n").collect();
    assert_eq!(parts.len(), 2);

    let frontmatter = parts[0].trim_start_matches("---\n");

    // REAL assertions on frontmatter content
    assert!(frontmatter.contains("to:"));
    assert!(frontmatter.contains("vars:"));
    assert!(frontmatter.contains("name: \"test_module\""));
    assert!(frontmatter.contains("author:"));
}

#[test]
fn test_read_written_template() {
    // REAL file system
    let temp_dir = TempDir::new().unwrap();
    let service = TemplateService::new(temp_dir.path().join("templates"));

    // Write REAL template
    let content = generate_template_content("test", "rust").unwrap();
    service.write_template("test", &content).unwrap();

    // Read REAL template back
    let read_content = service.read_template("test").unwrap();

    // REAL assertion - content should match exactly
    assert_eq!(read_content, content);
}
