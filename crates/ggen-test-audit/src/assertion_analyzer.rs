//! Assertion strength analysis using AST parsing
//!
//! This module analyzes test assertions to detect weak tests that don't properly verify behavior.
//! Uses syn crate to parse Rust test files and classify assertion strength.

use crate::types::{AssertionStrength, AuditResult, TestId};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use syn::{Expr, ExprCall, ExprMacro, Item, ItemFn};
use walkdir::WalkDir;

/// Assertion strength analyzer using AST parsing
#[derive(Debug)]
pub struct AssertionAnalyzer {
    /// Test directory paths to analyze
    test_dirs: Vec<PathBuf>,
}

impl AssertionAnalyzer {
    /// Create a new assertion analyzer
    ///
    /// # Arguments
    /// * `test_dirs` - Directories containing Rust test files
    pub fn new(test_dirs: Vec<PathBuf>) -> Self {
        Self { test_dirs }
    }

    /// Analyze a single test file
    ///
    /// Parses Rust test file and extracts assertion classifications.
    ///
    /// # Errors
    /// Returns `AuditError::IoError` if file read fails
    /// Returns `AuditError::AssertionParseError` if AST parsing fails
    pub fn analyze_test_file(&self, file_path: &Path) -> AuditResult<Vec<TestAssertion>> {
        let content = std::fs::read_to_string(file_path)?;

        let syntax_tree = syn::parse_file(&content).map_err(|e| {
            crate::types::AuditError::AssertionParseError(format!(
                "Failed to parse {}: {}",
                file_path.display(),
                e
            ))
        })?;

        let mut assertions = Vec::new();

        for item in &syntax_tree.items {
            if let Item::Fn(func) = item {
                if self.is_test_function(func) {
                    let test_id = TestId::new(func.sig.ident.to_string())?;
                    let strength = self.analyze_function_assertions(func);

                    assertions.push(TestAssertion {
                        test_id,
                        file_path: file_path.to_path_buf(),
                        assertion_strength: strength,
                        assertion_count: self.count_assertions(func),
                    });
                }
            }
        }

        Ok(assertions)
    }

    /// Check if function is a test (has #[test] or #[cfg(test)])
    fn is_test_function(&self, func: &ItemFn) -> bool {
        func.attrs.iter().any(|attr| attr.path().is_ident("test"))
    }

    /// Analyze assertions in a test function
    fn analyze_function_assertions(&self, func: &ItemFn) -> AssertionStrength {
        let mut strongest = AssertionStrength::Weak;

        // Visit all expressions in function body
        for stmt in &func.block.stmts {
            let expr = match stmt {
                syn::Stmt::Expr(e, _) => Some(e),
                _ => None,
            };

            if let Some(expr) = expr {
                let strength = self.classify_expression(expr);
                if strength > strongest {
                    strongest = strength;
                }
            }
        }

        strongest
    }

    /// Classify assertion strength from expression
    fn classify_expression(&self, expr: &Expr) -> AssertionStrength {
        match expr {
            Expr::Macro(ExprMacro { mac, .. }) => self.classify_macro_assertion(mac),
            Expr::Call(ExprCall { func, .. }) => self.classify_method_call_assertion(func),
            _ => AssertionStrength::Weak,
        }
    }

    /// Classify macro-based assertions (assert!, assert_eq!, etc.)
    fn classify_macro_assertion(&self, mac: &syn::Macro) -> AssertionStrength {
        let path_str = mac
            .path
            .segments
            .last()
            .map(|seg| seg.ident.to_string())
            .unwrap_or_default();

        self.classify_assertion(&path_str)
    }

    /// Classify method call assertions (is_ok(), is_some(), etc.)
    fn classify_method_call_assertion(&self, _func: &Expr) -> AssertionStrength {
        // For now, classify method calls as weak
        // TODO: Extract method name and classify properly
        AssertionStrength::Weak
    }

    /// Classify assertion by name into strength category
    ///
    /// # Classification Rules
    /// - **Strong**: assert_eq!, assert_ne! with explicit expected values
    /// - **Medium**: assert!, is_err!
    /// - **Weak**: is_ok!, is_some!, is_none! (execution-only checks)
    pub fn classify_assertion(&self, assertion_name: &str) -> AssertionStrength {
        match assertion_name {
            // Strong: Value-based assertions
            "assert_eq" | "assert_ne" => AssertionStrength::Strong,

            // Medium: Truthiness checks
            "assert" | "is_err" => AssertionStrength::Medium,

            // Weak: Execution-only checks
            "is_ok" | "is_some" | "is_none" => AssertionStrength::Weak,

            _ => AssertionStrength::Weak,
        }
    }

    /// Score assertion strength on 0.0-10.0 scale
    ///
    /// Maps AssertionStrength enum to numeric score:
    /// - Weak: 0-3 (score: 2.0)
    /// - Medium: 4-7 (score: 5.5)
    /// - Strong: 8-10 (score: 9.0)
    #[must_use]
    pub fn score_assertion(&self, strength: AssertionStrength) -> f64 {
        strength.to_score()
    }

    /// Count assertions in test function
    fn count_assertions(&self, func: &ItemFn) -> usize {
        let mut count = 0;

        for stmt in &func.block.stmts {
            let expr = match stmt {
                syn::Stmt::Expr(e, _) => Some(e),
                _ => None,
            };

            if let Some(expr) = expr {
                if self.is_assertion_expr(expr) {
                    count += 1;
                }
            }
        }

        count
    }

    /// Check if expression is an assertion
    fn is_assertion_expr(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Macro(ExprMacro { mac, .. }) => {
                let path_str = mac
                    .path
                    .segments
                    .last()
                    .map(|seg| seg.ident.to_string())
                    .unwrap_or_default();

                path_str.starts_with("assert") || path_str.starts_with("is_")
            }
            _ => false,
        }
    }

    /// Analyze all test files in configured directories
    ///
    /// # Errors
    /// Returns `AuditError::IoError` if directory traversal fails
    pub fn analyze_all_tests(&self) -> AuditResult<Vec<TestAssertion>> {
        let mut all_assertions = Vec::new();

        for test_dir in &self.test_dirs {
            for entry in WalkDir::new(test_dir)
                .into_iter()
                .filter_map(Result::ok)
                .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("rs"))
            {
                let assertions = self.analyze_test_file(entry.path())?;
                all_assertions.extend(assertions);
            }
        }

        Ok(all_assertions)
    }
}

/// Test assertion metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestAssertion {
    /// Test identifier
    pub test_id: TestId,
    /// File path where test is defined
    pub file_path: PathBuf,
    /// Classified assertion strength
    pub assertion_strength: AssertionStrength,
    /// Number of assertions in test
    pub assertion_count: usize,
}
