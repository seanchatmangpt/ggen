//! Assertion strength analysis using AST parsing
//!
//! This module analyzes test assertions to detect weak tests that don't properly verify behavior.
//! Uses syn crate to parse Rust test files and classify assertion strength.

use crate::types::{AssertionStrength, AuditResult, TestId};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use syn::{Expr, ExprCall, ExprMacro, ExprMethodCall, Item, ItemFn};
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
            // Free function call: is_ok(result), assert_eq!(a, b) when not a macro
            Expr::Call(ExprCall { func, .. }) => self.classify_method_call_assertion(func),
            // Method call: result.is_ok(), result.is_some(), val.is_err()
            Expr::MethodCall(ExprMethodCall { method, .. }) => {
                self.classify_assertion(&method.to_string())
            }
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

    /// Classify method call assertions for free-function-style calls.
    ///
    /// Extracts the function name from the path expression (e.g. `is_ok(x)` → `"is_ok"`)
    /// and delegates to `classify_assertion`. Falls back to `Weak` for non-path callees
    /// (closures, indirect calls, etc.).
    fn classify_method_call_assertion(&self, func: &Expr) -> AssertionStrength {
        if let Expr::Path(expr_path) = func {
            let name = expr_path
                .path
                .segments
                .last()
                .map(|seg| seg.ident.to_string())
                .unwrap_or_default();
            self.classify_assertion(&name)
        } else {
            AssertionStrength::Weak
        }
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

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;

    fn make_analyzer() -> AssertionAnalyzer {
        AssertionAnalyzer::new(vec![])
    }

    // --- classify_assertion (baseline) ---

    #[test]
    fn classify_assert_eq_is_strong() {
        let a = make_analyzer();
        assert_eq!(a.classify_assertion("assert_eq"), AssertionStrength::Strong);
    }

    #[test]
    fn classify_assert_ne_is_strong() {
        let a = make_analyzer();
        assert_eq!(a.classify_assertion("assert_ne"), AssertionStrength::Strong);
    }

    #[test]
    fn classify_assert_is_medium() {
        let a = make_analyzer();
        assert_eq!(a.classify_assertion("assert"), AssertionStrength::Medium);
    }

    #[test]
    fn classify_is_err_is_medium() {
        let a = make_analyzer();
        assert_eq!(a.classify_assertion("is_err"), AssertionStrength::Medium);
    }

    #[test]
    fn classify_is_ok_is_weak() {
        let a = make_analyzer();
        assert_eq!(a.classify_assertion("is_ok"), AssertionStrength::Weak);
    }

    #[test]
    fn classify_is_some_is_weak() {
        let a = make_analyzer();
        assert_eq!(a.classify_assertion("is_some"), AssertionStrength::Weak);
    }

    #[test]
    fn classify_is_none_is_weak() {
        let a = make_analyzer();
        assert_eq!(a.classify_assertion("is_none"), AssertionStrength::Weak);
    }

    // --- classify_method_call_assertion: path-based free function calls ---

    #[test]
    fn method_call_path_assert_eq_is_strong() {
        let a = make_analyzer();
        // Simulate: assert_eq(a, b) — Expr::Call with Expr::Path func
        let path: syn::ExprPath = syn::parse_str("assert_eq").expect("parse path");
        let func_expr = Expr::Path(path);
        assert_eq!(
            a.classify_method_call_assertion(&func_expr),
            AssertionStrength::Strong,
            "free function assert_eq must be Strong"
        );
    }

    #[test]
    fn method_call_path_is_ok_is_weak() {
        let a = make_analyzer();
        let path: syn::ExprPath = syn::parse_str("is_ok").expect("parse path");
        let func_expr = Expr::Path(path);
        assert_eq!(
            a.classify_method_call_assertion(&func_expr),
            AssertionStrength::Weak,
            "free function is_ok must be Weak"
        );
    }

    #[test]
    fn method_call_non_path_fallback_is_weak() {
        let a = make_analyzer();
        // Non-path callee (closure): should fall back to Weak
        let closure: syn::ExprClosure = syn::parse_str("|| true").expect("parse closure");
        let func_expr = Expr::Closure(closure);
        assert_eq!(
            a.classify_method_call_assertion(&func_expr),
            AssertionStrength::Weak,
            "non-path callee must fall back to Weak"
        );
    }

    // --- classify_expression: MethodCall branch (result.is_ok(), val.is_err()) ---

    #[test]
    fn expression_method_call_is_ok_is_weak() {
        let a = make_analyzer();
        let expr: Expr = syn::parse_str("result.is_ok()").expect("parse method call");
        assert_eq!(
            a.classify_expression(&expr),
            AssertionStrength::Weak,
            "result.is_ok() must be Weak"
        );
    }

    #[test]
    fn expression_method_call_is_err_is_medium() {
        let a = make_analyzer();
        let expr: Expr = syn::parse_str("result.is_err()").expect("parse method call");
        assert_eq!(
            a.classify_expression(&expr),
            AssertionStrength::Medium,
            "result.is_err() must be Medium"
        );
    }

    #[test]
    fn expression_method_call_is_some_is_weak() {
        let a = make_analyzer();
        let expr: Expr = syn::parse_str("opt.is_some()").expect("parse method call");
        assert_eq!(
            a.classify_expression(&expr),
            AssertionStrength::Weak,
            "opt.is_some() must be Weak"
        );
    }

    #[test]
    fn expression_macro_assert_eq_is_strong() {
        let a = make_analyzer();
        let expr: Expr = syn::parse_str("assert_eq!(1, 1)").expect("parse macro");
        assert_eq!(
            a.classify_expression(&expr),
            AssertionStrength::Strong,
            "assert_eq! macro must be Strong"
        );
    }
}
