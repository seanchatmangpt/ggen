//! Chicago TDD tests for template list command

use ggen_cli::domain::template::{list_templates, ListFilters, TemplateSource};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_list_empty_directory() {
    // REAL file system with TempDir
    let temp_dir = TempDir::new().unwrap();
    let templates_dir = temp_dir.path().join("templates");
    fs::create_dir_all(&templates_dir).unwrap();

    let filters = ListFilters {
        pattern: None,
        local_only: false,
        gpack_only: false,
    };

    let templates = list_templates(&templates_dir, &filters).unwrap();

    // REAL assertion - no mocks
    assert_eq!(templates.len(), 0);
}

#[test]
fn test_list_real_templates() {
    // REAL file system with REAL template files
    let temp_dir = TempDir::new().unwrap();
    let templates_dir = temp_dir.path().join("templates");
    fs::create_dir_all(&templates_dir).unwrap();

    // Create REAL template files
    let rust_template = r#"---
description: "Rust module template"
to: "src/{{ name }}.rs"
---
pub struct {{ name | pascal_case }} {
    pub id: String,
}"#;

    let python_template = r#"---
description: "Python module template"
to: "src/{{ name }}.py"
---
class {{ name | pascal_case }}:
    pass
"#;

    fs::write(templates_dir.join("rust.tmpl"), rust_template).unwrap();
    fs::write(templates_dir.join("python.tmpl"), python_template).unwrap();

    let filters = ListFilters {
        pattern: None,
        local_only: false,
        gpack_only: false,
    };

    let templates = list_templates(&templates_dir, &filters).unwrap();

    // REAL assertions on REAL template data
    assert_eq!(templates.len(), 2);

    let rust_tmpl = templates.iter().find(|t| t.name == "rust.tmpl").unwrap();
    assert_eq!(rust_tmpl.source, TemplateSource::Local);
    assert_eq!(rust_tmpl.description, Some("Rust module template".to_string()));

    let python_tmpl = templates.iter().find(|t| t.name == "python.tmpl").unwrap();
    assert_eq!(python_tmpl.description, Some("Python module template".to_string()));
}

#[test]
fn test_list_with_pattern_filter() {
    // REAL file system
    let temp_dir = TempDir::new().unwrap();
    let templates_dir = temp_dir.path().join("templates");
    fs::create_dir_all(&templates_dir).unwrap();

    // Create multiple REAL template files
    fs::write(templates_dir.join("rust_api.tmpl"), "---\n---\nRust API").unwrap();
    fs::write(templates_dir.join("rust_cli.tmpl"), "---\n---\nRust CLI").unwrap();
    fs::write(templates_dir.join("python_app.tmpl"), "---\n---\nPython App").unwrap();

    let filters = ListFilters {
        pattern: Some("rust*".to_string()),
        local_only: false,
        gpack_only: false,
    };

    let templates = list_templates(&templates_dir, &filters).unwrap();

    // REAL assertion - glob pattern actually filtered files
    assert_eq!(templates.len(), 2);
    assert!(templates.iter().all(|t| t.name.starts_with("rust")));
}

#[test]
fn test_list_nonexistent_directory() {
    // REAL non-existent directory
    let temp_dir = TempDir::new().unwrap();
    let nonexistent = temp_dir.path().join("does_not_exist");

    let filters = ListFilters {
        pattern: None,
        local_only: false,
        gpack_only: false,
    };

    let templates = list_templates(&nonexistent, &filters).unwrap();

    // REAL assertion - should return empty, not error
    assert_eq!(templates.len(), 0);
}

#[test]
fn test_list_extracts_real_descriptions() {
    // REAL file system
    let temp_dir = TempDir::new().unwrap();
    let templates_dir = temp_dir.path().join("templates");
    fs::create_dir_all(&templates_dir).unwrap();

    // Template WITH description
    let with_desc = r#"---
description: "This is a test template"
to: "output.txt"
---
Content"#;

    // Template WITHOUT description
    let without_desc = r#"---
to: "output.txt"
---
Content"#;

    fs::write(templates_dir.join("with_desc.tmpl"), with_desc).unwrap();
    fs::write(templates_dir.join("without_desc.tmpl"), without_desc).unwrap();

    let filters = ListFilters {
        pattern: None,
        local_only: false,
        gpack_only: false,
    };

    let templates = list_templates(&templates_dir, &filters).unwrap();

    // REAL assertion on REAL frontmatter extraction
    let with = templates.iter().find(|t| t.name == "with_desc.tmpl").unwrap();
    assert_eq!(with.description, Some("This is a test template".to_string()));

    let without = templates.iter().find(|t| t.name == "without_desc.tmpl").unwrap();
    assert_eq!(without.description, None);
}
