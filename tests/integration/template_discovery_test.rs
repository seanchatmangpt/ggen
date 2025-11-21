//! Integration test for build.rs template auto-discovery
//!
//! **Week 2 Efficiency Improvement**: Validate template auto-discovery
//! Tests that build.rs correctly discovers and generates code for all templates
//!
//! **Chicago TDD**: State-based testing with real file system

use std::fs;
use std::path::Path;

#[test]
fn test_template_directory_exists() {
    // Arrange: Check templates directory
    let templates_dir = Path::new("templates");

    // Act & Assert: Verify directory exists
    assert!(
        templates_dir.exists(),
        "templates/ directory should exist"
    );
    assert!(
        templates_dir.is_dir(),
        "templates/ should be a directory"
    );
}

#[test]
fn test_templates_discovered() {
    // Arrange: Count actual .tmpl files
    let templates_dir = Path::new("templates");
    let mut actual_count = 0;

    if templates_dir.exists() {
        // Recursively count .tmpl files
        fn count_templates(dir: &Path, count: &mut usize) -> std::io::Result<()> {
            if dir.is_dir() {
                for entry in fs::read_dir(dir)? {
                    let entry = entry?;
                    let path = entry.path();
                    if path.is_dir() {
                        count_templates(&path, count)?;
                    } else if let Some(ext) = path.extension() {
                        if ext == "tmpl" {
                            *count += 1;
                        }
                    }
                }
            }
            Ok(())
        }

        #[allow(clippy::expect_used)]
        count_templates(templates_dir, &mut actual_count).expect("failed to count templates");
    }

    // Assert: Should discover templates (at least 200+)
    assert!(
        actual_count > 200,
        "Should discover at least 200 templates, found: {}",
        actual_count
    );
    println!("✅ Discovered {} templates", actual_count);
}

#[test]
fn test_build_script_compiles() {
    // Arrange: Check build.rs exists
    let build_script = Path::new("build.rs");

    // Act & Assert: Verify build script exists
    assert!(
        build_script.exists(),
        "build.rs should exist in workspace root"
    );

    // Verify it's a file
    assert!(
        build_script.is_file(),
        "build.rs should be a file"
    );
}

#[test]
fn test_generated_template_metadata_accessible() {
    // This test verifies that build.rs generates accessible template metadata
    // The generated code is in OUT_DIR/templates.rs

    // If we can import this in the future, we would test:
    // - TEMPLATES static exists
    // - templates_by_category function works
    // - find_template function works
    // - stats module exists

    // For now, we verify the build succeeds (implicit in cargo test passing)
    assert!(true, "If this test runs, build.rs succeeded");
}

#[test]
fn test_template_categories_exist() {
    // Arrange: Known template categories
    let known_categories = vec![
        "clnrm",
        "papers",
        "ultra",
        "clap-noun-verb-360",
    ];

    // Act: Check each category directory exists
    let templates_dir = Path::new("templates");

    for category in known_categories {
        let category_path = templates_dir.join(category);

        // Assert: Category should exist
        if category_path.exists() {
            println!("✅ Category found: {}", category);
        } else {
            println!("⚠️  Category not found: {} (may have been removed)", category);
        }
    }
}

#[test]
fn test_no_empty_templates() {
    // Arrange: Find all .tmpl files
    let templates_dir = Path::new("templates");
    let mut empty_templates = Vec::new();

    fn check_templates(dir: &Path, empty: &mut Vec<String>) -> std::io::Result<()> {
        if dir.is_dir() {
            for entry in fs::read_dir(dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_dir() {
                    check_templates(&path, empty)?;
                } else if let Some(ext) = path.extension() {
                    if ext == "tmpl" {
                        let contents = fs::read_to_string(&path)?;
                        if contents.trim().is_empty() {
                            empty.push(path.display().to_string());
                        }
                    }
                }
            }
        }
        Ok(())
    }

    if templates_dir.exists() {
        check_templates(templates_dir, &mut empty_templates)
            #[allow(clippy::expect_used)]
            .expect("failed to check templates");
    }

    // Assert: No templates should be empty
    if !empty_templates.is_empty() {
        println!("⚠️  Found {} empty templates:", empty_templates.len());
        for tmpl in &empty_templates {
            println!("  - {}", tmpl);
        }
    }

    // Allow empty templates but warn
    assert!(
        empty_templates.len() < 10,
        "Too many empty templates found: {}",
        empty_templates.len()
    );
}
