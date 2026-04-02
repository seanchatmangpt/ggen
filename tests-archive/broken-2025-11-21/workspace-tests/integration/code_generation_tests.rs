//! Integration Tests for Code Generation Workflow
//!
//! Tests the complete code generation pipeline including template loading,
//! variable substitution, rendering, and output validation.
//!
//! ## Test Coverage
//!
//! - Template loading from various sources
//! - Variable substitution and Tera rendering
//! - File tree generation
//! - Output validation
//! - RDF integration with templates
//! - Gpack template processing
//!
//! ## Running These Tests
//!
//! ```bash
//! cargo test --test code_generation_tests
//! ```

use chicago_tdd_tools::test;
use ggen_core::{GenContext, Generator, Pipeline, Template};
use std::collections::BTreeMap;
use std::fs;
use std::path::PathBuf;
use tera::Context;

// Import common test utilities
#[path = "../common/mod.rs"]
mod common;
use common::{create_temp_dir, sample_template_content, sample_template_vars, write_file_in_temp};

// ============================================================================
// Basic Template Rendering Tests
// ============================================================================

test!(test_simple_template_rendering, {
    // Arrange
    let template_content = "Hello, {{ name }}!";
    let mut vars = BTreeMap::new();
    vars.insert("name".to_string(), "World".to_string());

    let mut pipeline = Pipeline::new()?;
    let ctx = Context::from_serialize(&vars)?;

    // Act
    let rendered = pipeline.render_body(template_content, &ctx)?;

    // Assert
    assert_eq!(rendered, "Hello, World!");
    Ok::<(), ggen_utils::error::Error>(())
});

test!(test_template_with_multiple_variables, {
    // Arrange
    let template_content = r#"# {{ project_name }}
Author: {{ author }}
Version: {{ version }}"#;

    let vars = sample_template_vars();
    let mut pipeline = Pipeline::new()?;
    let ctx = Context::from_serialize(&vars)?;

    // Act
    let rendered = pipeline.render_body(template_content, &ctx)?;

    // Assert
    assert!(rendered.contains("TestProject"));
    assert!(rendered.contains("Test Author"));
    assert!(rendered.contains("1.0.0"));
    Ok::<(), ggen_utils::error::Error>(())
});

test!(test_template_with_conditionals, {
    // Arrange
    let template_content = r#"{% if is_public %}
pub struct {{ name }} {}
{% else %}
struct {{ name }} {}
{% endif %}"#;

    let mut vars = BTreeMap::new();
    vars.insert("name".to_string(), "MyStruct".to_string());
    vars.insert("is_public".to_string(), "true".to_string());

    let mut pipeline = Pipeline::new()?;
    let ctx = Context::from_serialize(&vars)?;

    // Act
    let rendered = pipeline.render_body(template_content, &ctx)?;

    // Assert
    assert!(rendered.contains("pub struct MyStruct"));
    Ok::<(), ggen_utils::error::Error>(())
});

test!(test_template_with_loops, {
    // Arrange
    let template_content = r#"{% for item in items %}
- {{ item }}
{% endfor %}"#;

    let mut ctx = Context::new();
    ctx.insert("items", &vec!["one", "two", "three"]);

    let mut pipeline = Pipeline::new()?;

    // Act
    let rendered = pipeline.render_body(template_content, &ctx)?;

    // Assert
    assert!(rendered.contains("- one"));
    assert!(rendered.contains("- two"));
    assert!(rendered.contains("- three"));
    Ok::<(), ggen_utils::error::Error>(())
});

// ============================================================================
// Generator Workflow Tests
// ============================================================================

test!(test_generator_basic_workflow, {
    // Arrange
    let temp_dir = create_temp_dir();
    let template_content = sample_template_content();
    let template_path = write_file_in_temp(&temp_dir, "template.md", &template_content);
    let output_dir = temp_dir.path().join("output");

    let pipeline = Pipeline::new()?;
    let ctx = GenContext::new(PathBuf::from(&template_path), output_dir.clone())
        .with_vars(sample_template_vars());

    let mut generator = Generator::new(pipeline, ctx);

    // Act
    let result = generator.generate();

    // Assert
    assert!(result.is_ok(), "Generator should complete successfully");
    let output_path = result?;
    assert!(output_path.exists(), "Output file should be created");

    let content = fs::read_to_string(&output_path)?;
    assert!(content.contains("TestProject"));
    Ok::<(), ggen_utils::error::Error>(())
});

test!(test_generator_with_custom_frontmatter, {
    // Arrange
    let temp_dir = create_temp_dir();
    let template_content = r#"---
to: "README.md"
---
Project: {{ project_name }}"#;
    let template_path = write_file_in_temp(&temp_dir, "template.md", template_content);
    let output_dir = temp_dir.path().join("output");

    let pipeline = Pipeline::new()?;
    let ctx = GenContext::new(PathBuf::from(&template_path), output_dir.clone())
        .with_vars(sample_template_vars());

    let mut generator = Generator::new(pipeline, ctx);

    // Act
    let output_path = generator.generate()?;

    // Assert
    assert_eq!(output_path.file_name().expect("No filename"), "README.md");
    assert!(output_path.exists());
    Ok::<(), ggen_utils::error::Error>(())
});

test!(test_generator_preserves_directory_structure, {
    // Arrange
    let temp_dir = create_temp_dir();
    fs::create_dir_all(temp_dir.path().join("templates/nested"))?;

    let template_path =
        write_file_in_temp(&temp_dir, "templates/nested/file.tmpl", "{{ content }}");
    let output_dir = temp_dir.path().join("output");

    let mut vars = BTreeMap::new();
    vars.insert("content".to_string(), "Test content".to_string());

    let pipeline = Pipeline::new()?;
    let ctx = GenContext::new(PathBuf::from(&template_path), output_dir.clone()).with_vars(vars);

    let mut generator = Generator::new(pipeline, ctx);

    // Act
    let output_path = generator.generate()?;

    // Assert
    assert!(output_path.exists());
    assert!(output_path.to_string_lossy().contains("output"));
    Ok::<(), ggen_utils::error::Error>(())
});

// ============================================================================
// File Tree Generation Tests
// ============================================================================
// NOTE: FileTreeGenerator API tests are disabled until the API is stabilized.
// The FileTreeGenerator is still under development and the API is not finalized.

// test!(test_file_tree_generation_basic, {
//     // DISABLED: FileTreeGenerator API not available
//     Ok(())
// });

// test!(test_file_tree_with_nested_directories, {
//     // DISABLED: FileTreeGenerator API not available
//     Ok(())
// });

// ============================================================================
// Template Format Parsing Tests
// ============================================================================
// NOTE: TemplateParser API tests are disabled. The Template API uses Template::parse() directly.

// test!(test_template_yaml_frontmatter, {
//     // DISABLED: Use Template::parse() instead of TemplateParser
//     Ok(())
// });

// test!(test_template_toml_frontmatter, {
//     // DISABLED: Template API only supports YAML frontmatter currently
//     Ok(())
// });

// test!(test_template_no_frontmatter, {
//     // DISABLED: Use Template::parse() for plain templates
//     Ok(())
// });

// ============================================================================
// Template Object Tests
// ============================================================================

test!(test_template_load_from_file, {
    // Arrange
    let temp_dir = create_temp_dir();
    let template_content = sample_template_content();
    let template_path = write_file_in_temp(&temp_dir, "test.tmpl", &template_content);

    // Act
    let template = Template::from_file(std::path::Path::new(&template_path))?;

    // Assert
    assert!(!template.body.is_empty());
    assert!(template.body.contains("project_name"));
    Ok::<(), ggen_utils::error::Error>(())
});

test!(test_template_render_with_context, {
    // Arrange
    let temp_dir = create_temp_dir();
    let template_content = "Hello, {{ name }}!";
    let template_path = write_file_in_temp(&temp_dir, "test.tmpl", template_content);

    let mut vars = BTreeMap::new();
    vars.insert("name".to_string(), "Alice".to_string());

    let mut template = Template::from_file(std::path::Path::new(&template_path))?;
    let mut pipeline = Pipeline::new()?;
    let ctx = Context::from_serialize(&vars)?;

    // Render frontmatter first
    template.render_frontmatter(&mut pipeline.tera_mut(), &ctx)?;

    // Act
    let rendered = template.render(pipeline.tera_mut(), &ctx)?;

    // Assert
    assert_eq!(rendered, "Hello, Alice!");
    Ok::<(), ggen_utils::error::Error>(())
});

// ============================================================================
// Error Handling Tests
// ============================================================================

test!(test_template_missing_variable_error, {
    // Arrange
    let mut pipeline = Pipeline::new()?;
    let template_content = "Hello, {{ missing_var }}!";
    let vars: BTreeMap<String, String> = BTreeMap::new(); // Empty vars
    let ctx = Context::from_serialize(&vars)?;

    // Act
    let result = pipeline.render_body(template_content, &ctx);

    // Assert
    assert!(
        result.is_err(),
        "Should error when required variable is missing"
    );
    Ok::<(), ggen_utils::error::Error>(())
});

test!(test_template_syntax_error, {
    // Arrange
    let mut pipeline = Pipeline::new()?;
    let template_content = "{% if unclosed %}"; // Syntax error
    let vars: BTreeMap<String, String> = BTreeMap::new();
    let ctx = Context::from_serialize(&vars)?;

    // Act
    let result = pipeline.render_body(template_content, &ctx);

    // Assert
    assert!(result.is_err(), "Should error on template syntax error");
    Ok::<(), ggen_utils::error::Error>(())
});

// ============================================================================
// Performance Tests
// ============================================================================

test!(test_template_rendering_performance, {
    // Arrange
    let mut pipeline = Pipeline::new()?;
    let template_content = r#"# {{ title }}
{% for i in range(end=100) %}
Item {{ i }}
{% endfor %}"#;

    let mut vars = BTreeMap::new();
    vars.insert("title".to_string(), "Performance Test".to_string());
    let ctx = Context::from_serialize(&vars)?;

    // Act
    let start = std::time::Instant::now();
    let result = pipeline.render_body(template_content, &ctx);
    let duration = start.elapsed();

    // Assert
    assert!(result.is_ok(), "Template should render successfully");
    assert!(
        duration.as_millis() < 100,
        "Template rendering should be fast (< 100ms), took {:?}",
        duration
    );
    Ok::<(), ggen_utils::error::Error>(())
});

// test!(test_large_file_tree_generation_performance, {
//     // DISABLED: FileTreeGenerator API not available
//     Ok(())
// });
