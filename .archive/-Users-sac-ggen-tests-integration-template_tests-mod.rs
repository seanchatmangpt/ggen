//! Integration tests for template commands using Chicago TDD
//!
//! Chicago TDD Principles:
//! - Use REAL TemplateEngine objects (no mocks)
//! - Test ACTUAL template rendering with real files
//! - Verify REAL output file creation
//! - Use TempDir for file system tests
//! - Minimal mocking - only RDF/SPARQL endpoints

mod test_template_list;
mod test_template_new;
mod test_template_generate_tree;
mod test_template_regenerate;
