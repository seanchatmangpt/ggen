//! Tera template environment setup and configuration
//!
//! This module provides utilities for building and configuring Tera template
//! engines with glob support for includes and macros. It sets up the template
//! environment with all necessary filters and functions.
//!
//! ## Features
//!
//! - **Glob Support**: Enable `{% include %}` and `{% import %}` across template directories
//! - **Auto-escape Control**: Disable auto-escaping for code generation (literal output)
//! - **Filter Registration**: Automatically register all text transformation filters
//! - **Minimal Mode**: Build lightweight Tera instances for ad-hoc string rendering
//!
//! ## Examples
//!
//! ### Building Tera with Glob Support
//!
//! ```rust,no_run
//! use ggen_core::tera_env::build_tera_with_glob;
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let templates_dir = Path::new("./templates");
//! let mut tera = build_tera_with_glob(templates_dir)?;
//!
//! // Now templates can use {% include %} and {% import %}
//! let mut ctx = tera::Context::new();
//! ctx.insert("name", "World");
//! let result = tera.render("main.tmpl", &ctx)?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Building Minimal Tera Instance
//!
//! ```rust
//! use ggen_core::tera_env::build_tera_minimal;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let mut tera = build_tera_minimal()?;
//!
//! // Use for ad-hoc string rendering
//! let mut ctx = tera::Context::new();
//! ctx.insert("name", "hello_world");
//! let result = tera.render_str("{{ name | pascal }}", &ctx)?;
//! assert_eq!(result, "HelloWorld");
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::Result;
use std::path::Path;
use tera::Tera;

use crate::register::register_all;

/// Build a Tera instance with template glob support for includes and macros.
///
/// This enables `{% include %}` and `{% import %}` directives across the
/// entire templates directory tree.
pub fn build_tera_with_glob(templates_dir: &Path) -> Result<Tera> {
    let glob = format!("{}/**/*.tmpl", templates_dir.display());

    // Tera::new() will return an error if no templates match the glob,
    // but we want to allow empty template directories during development
    let mut tera = Tera::new(&glob).unwrap_or_else(|_| {
        // Create empty Tera instance if no templates found
        Tera::default()
    });

    // Disable auto-escape for code generation (we want literal output)
    tera.autoescape_on(vec![]);

    // Register all text transformation filters
    register_all(&mut tera);

    Ok(tera)
}

/// Build a minimal Tera instance for ad-hoc string rendering.
///
/// Use this when you need to render individual strings without
/// template file dependencies.
pub fn build_tera_minimal() -> Result<Tera> {
    let mut tera = Tera::default();
    tera.autoescape_on(vec![]);
    register_all(&mut tera);
    Ok(tera)
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;
    use std::fs;
    use tempfile::TempDir;

    test!(test_build_tera_with_glob, {
        let temp_dir = TempDir::new()?;
        let templates_dir = temp_dir.path().join("templates");
        fs::create_dir_all(&templates_dir)?;

        // Create a test template
        let template_file = templates_dir.join("test.tmpl");
        fs::write(&template_file, "Hello {{ name }}!")?;

        let tera = build_tera_with_glob(&templates_dir)?;

        // Should be able to render the template
        let mut ctx = tera::Context::new();
        ctx.insert("name", "World");
        let result = tera.render("test.tmpl", &ctx)?;
        assert_eq!(result, "Hello World!");

        Ok(())
    });

    test!(test_build_tera_empty_directory, {
        let temp_dir = TempDir::new()?;
        let templates_dir = temp_dir.path().join("templates");
        fs::create_dir_all(&templates_dir)?;

        // Empty directory should not cause an error
        let mut tera = build_tera_with_glob(&templates_dir)?;

        // Should still have filters registered
        let mut ctx = tera::Context::new();
        ctx.insert("name", "hello_world");
        let result = tera.render_str("{{ name | pascal }}", &ctx)?;
        assert_eq!(result, "HelloWorld");

        Ok(())
    });

    test!(test_build_tera_minimal, {
        let mut tera = build_tera_minimal().unwrap();

        // Should have filters registered
        let mut ctx = tera::Context::new();
        ctx.insert("name", "hello_world");
        let result = tera.render_str("{{ name | snake }}", &ctx).unwrap();
        assert_eq!(result, "hello_world");
    });

    test!(test_autoescape_disabled, {
        let mut tera = build_tera_minimal().unwrap();

        // HTML should not be escaped
        let mut ctx = tera::Context::new();
        ctx.insert("html", "<script>alert('xss')</script>");
        let result = tera.render_str("{{ html }}", &ctx).unwrap();
        assert_eq!(result, "<script>alert('xss')</script>");
    });
}
