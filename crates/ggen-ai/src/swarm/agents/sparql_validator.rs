//! SPARQL Validator Agent - Autonomous SPARQL syntax validation and fixing
//!
//! This agent uses Oxigraph to parse SPARQL queries, detect syntax errors,
//! and automatically apply common fixes for malformed queries.

use crate::error::{GgenAiError, Result};
use crate::swarm::{AgentHealth, AgentInput, AgentOutput, HealthStatus, SwarmAgent, SwarmContext};
use async_trait::async_trait;
use oxigraph::sparql::Query as SparqlQuery;
use oxigraph::store::Store;
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::HashMap;
use tracing::{debug, info, warn};

/// SPARQL Validator Agent - validates and fixes SPARQL syntax errors
#[derive(Debug)]
pub struct SPARQLValidatorAgent {
    /// Dry run mode - don't apply fixes
    dry_run: bool,
    /// Enable verbose logging
    verbose: bool,
}

impl SPARQLValidatorAgent {
    /// Create a new SPARQL validator agent
    pub fn new(dry_run: bool) -> Self {
        Self {
            dry_run,
            verbose: false,
        }
    }

    /// Create with verbose logging
    pub fn with_verbose(mut self, verbose: bool) -> Self {
        self.verbose = verbose;
        self
    }

    /// Validate and fix a SPARQL query
    pub fn validate_and_fix(&self, query: &str) -> Result<ValidationReport> {
        let mut report = ValidationReport {
            original_query: query.to_string(),
            fixed_query: query.to_string(),
            is_valid: false,
            errors: Vec::new(),
            fixes_applied: Vec::new(),
            line: 0,
            column: 0,
        };

        // Step 1: Try to parse the query
        match SparqlQuery::parse(query, None) {
            Ok(_) => {
                report.is_valid = true;
                if self.verbose {
                    info!("Query is valid SPARQL");
                }
                return Ok(report);
            }
            Err(e) => {
                let error_msg = e.to_string();
                if self.verbose {
                    warn!("Parse error: {}", error_msg);
                }

                // Extract line and column from error message
                report.line = self.extract_line_from_error(&error_msg);
                report.column = self.extract_column_from_error(&error_msg);

                report.errors.push(SparqlError {
                    error_type: self.classify_error(&error_msg),
                    message: error_msg.clone(),
                    line: report.line,
                    column: report.column,
                    suggestion: None,
                });
            }
        }

        // Step 2: Apply fixes iteratively until no more fixes can be applied
        let mut current_query = query.to_string();
        let mut max_iterations = 10; // Prevent infinite loops
        let mut iteration = 0;

        while iteration < max_iterations {
            iteration += 1;

            // Try to parse the current query
            match SparqlQuery::parse(&current_query, None) {
                Ok(_) => {
                    report.fixed_query = current_query.clone();
                    report.is_valid = true;
                    if self.verbose {
                        info!("Query fixed successfully after {} iterations", iteration);
                    }
                    break;
                }
                Err(e) => {
                    let error_msg = e.to_string();

                    // Try to apply a fix based on the error
                    if let Some(fix) = self.suggest_fix(&current_query, &error_msg) {
                        if self.verbose {
                            debug!("Applying fix: {}", fix.description);
                        }

                        if !self.dry_run {
                            current_query = fix.fixed_query;
                        }

                        report.fixes_applied.push(fix);
                    } else {
                        // No more fixes can be applied
                        if self.verbose {
                            warn!("No more fixes can be applied");
                        }
                        break;
                    }
                }
            }
        }

        // Final validation attempt
        if let Err(e) = SparqlQuery::parse(&current_query, None) {
            report.errors.push(SparqlError {
                error_type: self.classify_error(&e.to_string()),
                message: e.to_string(),
                line: self.extract_line_from_error(&e.to_string()),
                column: self.extract_column_from_error(&e.to_string()),
                suggestion: None,
            });
        }

        report.fixed_query = current_query.clone();
        Ok(report)
    }

    /// Validate a SPARQL query without fixing
    pub fn validate(&self, query: &str) -> Result<bool> {
        match Query::parse(query, None) {
            Ok(_) => Ok(true),
            Err(e) => Err(GgenAiError::validation_error(&format!(
                "SPARQL validation failed: {}",
                e
            ))),
        }
    }

    /// Fix common syntax errors in SPARQL
    pub fn fix_syntax(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // Fix 1: Missing periods after triple patterns
        fixed = self.fix_missing_periods(&fixed);

        // Fix 2: Unbalanced brackets
        fixed = self.fix_unbalanced_brackets(&fixed);

        // Fix 3: Unbalanced parentheses
        fixed = self.fix_unbalanced_parens(&fixed);

        // Fix 4: Missing semicolons in PREFIX declarations
        fixed = self.fix_prefix_declarations(&fixed);

        // Fix 5: Missing quotes in literals
        fixed = self.fix_missing_quotes(&fixed);

        fixed
    }

    /// Fix variable reference issues
    pub fn fix_variables(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // Fix 1: Variables used in SELECT but not in WHERE clause
        fixed = self.fix_orphan_variables(&fixed);

        // Fix 2: Invalid variable names (must start with ? or $)
        fixed = self.fix_invalid_variable_names(&fixed);

        fixed
    }

    /// Restructure malformed queries
    pub fn restructure(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // Fix 1: Missing WHERE clause for SELECT queries
        fixed = self.fix_missing_where(&fixed);

        // Fix 2: Malformed FILTER expressions
        fixed = self.fix_filter_expressions(&fixed);

        // Fix 3: Malformed OPTIONAL patterns
        fixed = self.fix_optional_patterns(&fixed);

        fixed
    }

    // Internal fix methods

    fn fix_missing_periods(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // Pattern: triple pattern without period at end (before closing brace or another triple)
        let re = Regex::new(r"(\?[^\s]+\s+[^\s]+\s+[^\s]+)(\s*\n|\s*\})").unwrap();
        fixed = re.replace_all(&fixed, "$1.$2").to_string();

        fixed
    }

    fn fix_unbalanced_brackets(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // Count opening and closing brackets
        let open_count = query.matches('[').count();
        let close_count = query.matches(']').count();

        if open_count > close_count {
            // Add missing closing brackets
            let missing = open_count - close_count;
            for _ in 0..missing {
                fixed.push(']');
            }
        } else if close_count > open_count {
            // Remove extra closing brackets (from the end)
            let extra = close_count - open_count;
            let mut chars = fixed.chars().collect::<Vec<_>>();
            for _ in 0..extra {
                if let Some(pos) = chars.iter().rposition(|&c| c == ']') {
                    chars.remove(pos);
                }
            }
            fixed = chars.into_iter().collect();
        }

        fixed
    }

    fn fix_unbalanced_parens(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // Count opening and closing parentheses
        let open_count = query.matches('(').count();
        let close_count = query.matches(')').count();

        if open_count > close_count {
            // Add missing closing parentheses
            let missing = open_count - close_count;
            for _ in 0..missing {
                fixed.push(')');
            }
        } else if close_count > open_count {
            // Remove extra closing parentheses (from the end)
            let extra = close_count - open_count;
            let mut chars = fixed.chars().collect::<Vec<_>>();
            for _ in 0..extra {
                if let Some(pos) = chars.iter().rposition(|&c| c == ')') {
                    chars.remove(pos);
                }
            }
            fixed = chars.into_iter().collect();
        }

        fixed
    }

    fn fix_prefix_declarations(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // Fix PREFIX declarations missing angle brackets or full stops
        let re = Regex::new(r"PREFIX\s+(\w+):\s*([^<][^\s]*)").unwrap();
        fixed = re.replace_all(&fixed, "PREFIX $1: <$2>").to_string();

        fixed
    }

    fn fix_missing_quotes(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // Fix literals with missing closing quotes
        let re = Regex::new(r#""([^"]*?)(\s*[.\},\)]|$)"#).unwrap();
        fixed = re.replace_all(&fixed, r#""$1""$2"#).to_string();

        // Fix literals with missing opening quotes
        let re = Regex::new(r#"(\s)([^"]*?)"#).unwrap();
        fixed = re.replace_all(&fixed, r#" "$1""#).to_string();

        fixed
    }

    fn fix_orphan_variables(&self, query: &str) -> String {
        // This is a heuristic fix - warn about variables in SELECT not used in WHERE
        let mut fixed = query.to_string();

        // Extract variables from SELECT clause
        let select_re = Regex::new(r"SELECT\s+(.*?)\s+WHERE").unwrap();
        if let Some(caps) = select_re.captures(query) {
            let select_vars = caps.get(1).map(|m| m.as_str()).unwrap_or("");
            let var_re = Regex::new(r"\?(\w+)").unwrap();

            let select_var_names: Vec<String> = var_re
                .find_iter(select_vars)
                .map(|m| m.as_str().to_string())
                .collect();

            // Check if these variables appear in WHERE clause
            for var in &select_var_names {
                if !query.contains(var) {
                    if self.verbose {
                        warn!("Variable {} appears in SELECT but not in WHERE clause", var);
                    }
                }
            }
        }

        fixed
    }

    fn fix_invalid_variable_names(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // Fix variables that don't start with ? or $
        let re = Regex::new(r"\b([a-z_]\w*)\b").unwrap();
        fixed = re.replace_all(&fixed, "?$1").to_string();

        fixed
    }

    fn fix_missing_where(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // If SELECT query has no WHERE clause, add empty WHERE
        if query.contains("SELECT") && !query.contains("WHERE") {
            let re = Regex::new(r"(SELECT\s+.*?)(\{|$)").unwrap();
            fixed = re.replace_all(&fixed, "$1 WHERE {} $2").to_string();
        }

        fixed
    }

    fn fix_filter_expressions(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // Fix FILTER without parentheses
        let re = Regex::new(r"FILTER\s+([^\{]+?)(\s*[\.}\])|$)").unwrap();
        fixed = re.replace_all(&fixed, "FILTER ($1)$2").to_string();

        fixed
    }

    fn fix_optional_patterns(&self, query: &str) -> String {
        let mut fixed = query.to_string();

        // Fix OPTIONAL without braces
        let re = Regex::new(r"OPTIONAL\s+([^\{]+?)(\s*[\.}\])|$)").unwrap();
        fixed = re.replace_all(&fixed, "OPTIONAL {$1}$2").to_string();

        fixed
    }

    // Helper methods

    fn suggest_fix(&self, query: &str, error_msg: &str) -> Option<SparqlFix> {
        let error_type = self.classify_error(error_msg);

        match error_type {
            SparqlErrorType::SyntaxError => {
                let fixed = self.fix_syntax(query);
                if fixed != query {
                    Some(SparqlFix {
                        fix_type: "syntax_fix".to_string(),
                        description: "Fixed syntax errors (periods, brackets, quotes)".to_string(),
                        fixed_query: fixed,
                    })
                } else {
                    None
                }
            }
            SparqlErrorType::VariableError => {
                let fixed = self.fix_variables(query);
                if fixed != query {
                    Some(SparqlFix {
                        fix_type: "variable_fix".to_string(),
                        description: "Fixed variable reference issues".to_string(),
                        fixed_query: fixed,
                    })
                } else {
                    None
                }
            }
            SparqlErrorType::StructureError => {
                let fixed = self.restructure(query);
                if fixed != query {
                    Some(SparqlFix {
                        fix_type: "structure_fix".to_string(),
                        description: "Restructured malformed query".to_string(),
                        fixed_query: fixed,
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn classify_error(&self, error_msg: &str) -> SparqlErrorType {
        let lower = error_msg.to_lowercase();

        if lower.contains("syntax error") || lower.contains("unexpected token") {
            SparqlErrorType::SyntaxError
        } else if lower.contains("variable") || lower.contains("binding name") {
            SparqlErrorType::VariableError
        } else if lower.contains("where") || lower.contains("filter") || lower.contains("optional")
        {
            SparqlErrorType::StructureError
        } else if lower.contains("prefix") || lower.contains("namespace") {
            SparqlErrorType::PrefixError
        } else {
            SparqlErrorType::Unknown
        }
    }

    fn extract_line_from_error(&self, error_msg: &str) -> usize {
        // Try to extract line number from error message
        let re = Regex::new(r"line (\d+)").unwrap();
        if let Some(caps) = re.captures(error_msg) {
            caps.get(1)
                .and_then(|m| m.as_str().parse().ok())
                .unwrap_or(0)
        } else {
            0
        }
    }

    fn extract_column_from_error(&self, error_msg: &str) -> usize {
        // Try to extract column number from error message
        let re = Regex::new(r"column (\d+)").unwrap();
        if let Some(caps) = re.captures(error_msg) {
            caps.get(1)
                .and_then(|m| m.as_str().parse().ok())
                .unwrap_or(0)
        } else {
            0
        }
    }
}

#[async_trait]
impl SwarmAgent for SPARQLValidatorAgent {
    fn name(&self) -> &str {
        "sparql_validator"
    }

    fn capabilities(&self) -> Vec<String> {
        vec![
            "sparql_validation".to_string(),
            "sparql_fixing".to_string(),
            "syntax_correction".to_string(),
            "variable_validation".to_string(),
            "query_restructuring".to_string(),
        ]
    }

    async fn execute(&self, _context: &SwarmContext, input: AgentInput) -> Result<AgentOutput> {
        // Extract query from input
        let query = input
            .data
            .get("query")
            .and_then(|v| v.as_str())
            .ok_or_else(|| GgenAiError::validation_error("Missing 'query' in input data"))?;

        // Validate and fix
        let report = self.validate_and_fix(query)?;

        // Convert report to JSON
        let output_data = json!({
            "is_valid": report.is_valid,
            "original_query": report.original_query,
            "fixed_query": report.fixed_query,
            "errors": report.errors,
            "fixes_applied": report.fixes_applied,
            "line": report.line,
            "column": report.column,
        });

        Ok(AgentOutput {
            data: output_data,
            output_type: "sparql_validation_report".to_string(),
            target_agents: vec!["code_generator".to_string()],
            metadata: {
                let mut meta = HashMap::new();
                meta.insert("agent".to_string(), "sparql_validator".to_string());
                meta.insert(
                    "fixes_count".to_string(),
                    report.fixes_applied.len().to_string(),
                );
                meta
            },
        })
    }

    async fn validate(&self) -> Result<bool> {
        // Validate that Oxigraph parser is working
        let test_query = "SELECT * WHERE { ?s ?p ?o }";
        Ok(SparqlQuery::parse(test_query, None).is_ok())
    }

    async fn health_check(&self) -> AgentHealth {
        let is_healthy = self.validate().await.unwrap_or(false);

        AgentHealth {
            status: if is_healthy {
                HealthStatus::Healthy
            } else {
                HealthStatus::Unhealthy
            },
            score: if is_healthy { 1.0 } else { 0.0 },
            last_check: chrono::Utc::now().to_rfc3339(),
            issues: if is_healthy {
                vec![]
            } else {
                vec!["Oxigraph parser not functioning".to_string()]
            },
        }
    }
}

/// Validation report for SPARQL queries
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReport {
    /// Original query
    pub original_query: String,
    /// Fixed query (if errors were found)
    pub fixed_query: String,
    /// Whether the query is valid
    pub is_valid: bool,
    /// Errors found
    pub errors: Vec<SparqlError>,
    /// Fixes applied
    pub fixes_applied: Vec<SparqlFix>,
    /// Line number of error (0 if unknown)
    pub line: usize,
    /// Column number of error (0 if unknown)
    pub column: usize,
}

/// SPARQL error details
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SparqlError {
    /// Error type
    pub error_type: SparqlErrorType,
    /// Error message
    pub message: String,
    /// Line number (0 if unknown)
    pub line: usize,
    /// Column number (0 if unknown)
    pub column: usize,
    /// Suggested fix (if available)
    pub suggestion: Option<String>,
}

/// SPARQL error types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SparqlErrorType {
    /// Syntax errors (missing periods, brackets, etc.)
    SyntaxError,
    /// Variable reference errors
    VariableError,
    /// Query structure errors (missing WHERE, malformed FILTER, etc.)
    StructureError,
    /// PREFIX declaration errors
    PrefixError,
    /// Unknown error type
    Unknown,
}

/// SPARQL fix details
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SparqlFix {
    /// Type of fix applied
    pub fix_type: String,
    /// Human-readable description
    pub description: String,
    /// Fixed query
    pub fixed_query: String,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_valid_query() {
        let agent = SPARQLValidatorAgent::new(false);
        let query = "SELECT * WHERE { ?s ?p ?o }";
        let result = agent.validate_and_fix(query).unwrap();
        assert!(result.is_valid);
        assert_eq!(result.fixed_query, query);
    }

    #[test]
    fn test_fix_missing_periods() {
        let agent = SPARQLValidatorAgent::new(false);
        let query = "SELECT * WHERE { ?s ?p ?o }";
        let fixed = agent.fix_syntax(query);
        assert!(fixed.contains("."));
    }

    #[test]
    fn test_fix_unbalanced_brackets() {
        let agent = SPARQLValidatorAgent::new(false);
        let query = "SELECT * WHERE { ?s ?p [ ?o1 }";
        let fixed = agent.fix_unbalanced_brackets(query);
        assert_eq!(fixed.matches('[').count(), fixed.matches(']').count());
    }

    #[test]
    fn test_fix_prefix_declarations() {
        let agent = SPARQLValidatorAgent::new(false);
        let query =
            "PREFIX rdf: http://www.w3.org/1999/02/22-rdf-syntax-ns# SELECT * WHERE { ?s ?p ?o }";
        let fixed = agent.fix_prefix_declarations(query);
        assert!(fixed.contains("<http://"));
    }

    #[test]
    fn test_fix_missing_where() {
        let agent = SPARQLValidatorAgent::new(false);
        let query = "SELECT * { ?s ?p ?o }";
        let fixed = agent.fix_missing_where(query);
        assert!(fixed.contains("WHERE"));
    }
}
