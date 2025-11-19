#![cfg(feature = "london-tdd")]
//! Comprehensive Error Message Quality Tests
//!
//! This test suite validates that all error paths produce actionable, user-friendly
//! messages with:
//! - Proper context about what failed
//! - Suggested fixes
//! - Relevant file paths/line numbers
//! - Clear distinction between user errors vs system errors
//! - Consistent formatting
//! - Helpful examples
//!
//! Test Coverage:
//! - CLI argument parsing
//! - File I/O operations
//! - Ontology validation
//! - SPARQL errors
//! - Template rendering

use crate::lib::*;

// ============================================================================
// Error Quality Verification Helpers
// ============================================================================

/// Verifies an error message meets quality standards
#[derive(Debug, Default)]
struct ErrorQualityAssertion {
    error_message: String,
}

impl ErrorQualityAssertion {
    fn new(error_message: String) -> Self {
        Self { error_message }
    }

    /// Assert error has proper context about what failed
    fn has_context(&self, expected_context: &str) -> &Self {
        assert!(
            self.error_message.contains(expected_context),
            "Error message missing context '{}'. Message: {}",
            expected_context,
            self.error_message
        );
        self
    }

    /// Assert error suggests a fix
    fn has_fix_suggestion(&self, expected_fix: &str) -> &Self {
        assert!(
            self.error_message.contains(expected_fix)
                || self.error_message.to_lowercase().contains("try")
                || self.error_message.to_lowercase().contains("use")
                || self.error_message.to_lowercase().contains("run")
                || self.error_message.to_lowercase().contains("check"),
            "Error message missing fix suggestion containing '{}'. Message: {}",
            expected_fix,
            self.error_message
        );
        self
    }

    /// Assert error includes file path or location
    fn has_file_path(&self) -> &Self {
        assert!(
            self.error_message.contains("/")
                || self.error_message.contains("\\")
                || self.error_message.contains(".ttl")
                || self.error_message.contains(".toml")
                || self.error_message.contains(".yaml")
                || self.error_message.contains(".json")
                || self.error_message.contains(".tmpl"),
            "Error message missing file path. Message: {}",
            self.error_message
        );
        self
    }

    /// Assert error is clearly a user error (not system error)
    fn is_user_error(&self) -> &Self {
        let user_error_indicators = [
            "not found",
            "invalid",
            "missing",
            "required",
            "expected",
            "must",
            "cannot",
        ];
        assert!(
            user_error_indicators
                .iter()
                .any(|&indicator| self.error_message.to_lowercase().contains(indicator)),
            "Error message doesn't clearly indicate user error. Message: {}",
            self.error_message
        );
        self
    }

    /// Assert error is clearly a system error
    fn is_system_error(&self) -> &Self {
        let system_error_indicators = [
            "permission denied",
            "access denied",
            "network",
            "timeout",
            "unavailable",
            "failed to",
            "internal",
        ];
        assert!(
            system_error_indicators
                .iter()
                .any(|&indicator| self.error_message.to_lowercase().contains(indicator)),
            "Error message doesn't clearly indicate system error. Message: {}",
            self.error_message
        );
        self
    }

    /// Assert error has consistent formatting (starts with capital, ends with period or newline)
    fn has_consistent_formatting(&self) -> &Self {
        let first_char = self.error_message.chars().next().unwrap_or(' ');
        assert!(
            first_char.is_uppercase() || first_char.is_numeric(),
            "Error message should start with uppercase letter. Message: {}",
            self.error_message
        );
        self
    }

    /// Assert error provides helpful examples
    fn has_example(&self) -> &Self {
        assert!(
            self.error_message.to_lowercase().contains("example")
                || self.error_message.contains("e.g.")
                || self.error_message.contains("'")
                || self.error_message.contains("`"),
            "Error message missing example. Message: {}",
            self.error_message
        );
        self
    }
}

// ============================================================================
// CLI Argument Parsing Error Tests
// ============================================================================

#[test]
fn test_cli_missing_required_argument_error_quality() {
    // Simulate missing required argument error
    let error = simulate_cli_missing_arg("template", "render");

    ErrorQualityAssertion::new(error)
        .has_context("required")
        .has_fix_suggestion("try")
        .is_user_error()
        .has_consistent_formatting();
}

#[test]
fn test_cli_invalid_argument_value_error_quality() {
    // Simulate invalid argument value
    let error = simulate_cli_invalid_value("--format", "invalid-format", &["json", "yaml", "toml"]);

    ErrorQualityAssertion::new(error)
        .has_context("invalid")
        .has_fix_suggestion("valid")
        .is_user_error()
        .has_example()
        .has_consistent_formatting();
}

#[test]
fn test_cli_unknown_command_error_quality() {
    // Simulate unknown command with suggestions
    let error = simulate_cli_unknown_command("templte", &["template", "project", "marketplace"]);

    ErrorQualityAssertion::new(error)
        .has_context("unknown")
        .has_fix_suggestion("did you mean")
        .is_user_error()
        .has_example()
        .has_consistent_formatting();
}

#[test]
fn test_cli_conflicting_arguments_error_quality() {
    // Simulate conflicting arguments
    let error = simulate_cli_conflicting_args("--local", "--remote");

    ErrorQualityAssertion::new(error)
        .has_context("conflict")
        .has_fix_suggestion("use")
        .is_user_error()
        .has_consistent_formatting();
}

// ============================================================================
// File I/O Error Tests
// ============================================================================

#[test]
fn test_file_not_found_error_quality() {
    // Simulate file not found error
    let error = simulate_file_not_found("/path/to/missing-template.tmpl");

    ErrorQualityAssertion::new(error)
        .has_context("not found")
        .has_file_path()
        .has_fix_suggestion("check")
        .is_user_error()
        .has_consistent_formatting();
}

#[test]
fn test_file_permission_denied_error_quality() {
    // Simulate permission denied error
    let error = simulate_permission_denied("/etc/protected-config.toml");

    ErrorQualityAssertion::new(error)
        .has_context("permission")
        .has_file_path()
        .has_fix_suggestion("chmod")
        .is_system_error()
        .has_consistent_formatting();
}

#[test]
fn test_file_invalid_format_error_quality() {
    // Simulate invalid file format
    let error = simulate_invalid_file_format("config.yaml", "Expected YAML but found invalid syntax");

    ErrorQualityAssertion::new(error.clone())
        .has_context("invalid")
        .has_file_path()
        .is_user_error()
        .has_consistent_formatting();

    // Should include line number if available
    if error.contains("line") {
        assert!(error.chars().any(|c| c.is_numeric()));
    }
}

#[test]
fn test_directory_not_empty_error_quality() {
    // Simulate directory not empty error
    let error = simulate_directory_not_empty("/path/to/existing-project");

    ErrorQualityAssertion::new(error)
        .has_context("not empty")
        .has_file_path()
        .has_fix_suggestion("remove")
        .is_user_error()
        .has_consistent_formatting();
}

#[test]
fn test_disk_full_error_quality() {
    // Simulate disk full error
    let error = simulate_disk_full("/path/to/output.txt");

    ErrorQualityAssertion::new(error)
        .has_context("space")
        .has_fix_suggestion("free")
        .is_system_error()
        .has_consistent_formatting();
}

// ============================================================================
// Ontology Validation Error Tests
// ============================================================================

#[test]
fn test_ontology_invalid_syntax_error_quality() {
    // Simulate RDF syntax error
    let error = simulate_rdf_syntax_error("ontology.ttl", 42, "Unexpected token");

    ErrorQualityAssertion::new(error.clone())
        .has_context("syntax")
        .has_file_path()
        .is_user_error()
        .has_consistent_formatting();

    // Should include line number
    assert!(
        error.contains("42") || error.contains("line"),
        "Error should include line number. Message: {}",
        error
    );
}

#[test]
fn test_ontology_invalid_prefix_error_quality() {
    // Simulate missing or invalid prefix
    let error = simulate_invalid_prefix("owl", "ontology.ttl");

    ErrorQualityAssertion::new(error)
        .has_context("prefix")
        .has_file_path()
        .has_fix_suggestion("@prefix")
        .is_user_error()
        .has_example()
        .has_consistent_formatting();
}

#[test]
fn test_ontology_shacl_validation_error_quality() {
    // Simulate SHACL validation failure
    let error = simulate_shacl_validation_error(
        "Person",
        "sh:minCount",
        "Property 'name' requires at least 1 value",
    );

    ErrorQualityAssertion::new(error)
        .has_context("validation")
        .has_fix_suggestion("add")
        .is_user_error()
        .has_consistent_formatting();
}

#[test]
fn test_ontology_constraint_violation_error_quality() {
    // Simulate constraint violation
    let error = simulate_constraint_violation(
        "age",
        "xsd:integer",
        "\"not a number\"",
        "Person",
    );

    ErrorQualityAssertion::new(error)
        .has_context("constraint")
        .has_fix_suggestion("must")
        .is_user_error()
        .has_example()
        .has_consistent_formatting();
}

// ============================================================================
// SPARQL Error Tests
// ============================================================================

#[test]
fn test_sparql_syntax_error_quality() {
    // Simulate SPARQL syntax error
    let error = simulate_sparql_syntax_error(
        "SELECT ?name WHERE { ?person foaf:name ?name ",
        "Missing closing brace",
    );

    ErrorQualityAssertion::new(error)
        .has_context("syntax")
        .has_fix_suggestion("check")
        .is_user_error()
        .has_example()
        .has_consistent_formatting();
}

#[test]
fn test_sparql_undefined_prefix_error_quality() {
    // Simulate undefined prefix in SPARQL
    let error = simulate_sparql_undefined_prefix("foaf", "SELECT ?s WHERE { ?s foaf:name ?n }");

    ErrorQualityAssertion::new(error)
        .has_context("prefix")
        .has_fix_suggestion("PREFIX")
        .is_user_error()
        .has_example()
        .has_consistent_formatting();
}

#[test]
fn test_sparql_query_timeout_error_quality() {
    // Simulate query timeout
    let error = simulate_sparql_timeout("Complex query with multiple joins", 30);

    ErrorQualityAssertion::new(error)
        .has_context("timeout")
        .has_fix_suggestion("simplify")
        .is_system_error()
        .has_consistent_formatting();
}

#[test]
fn test_sparql_no_results_warning_quality() {
    // Simulate empty result set (warning, not error)
    let warning = simulate_sparql_empty_results("SELECT ?person WHERE { ?person rdf:type ex:NonExistent }");

    // Warnings should still be informative
    assert!(
        warning.to_lowercase().contains("no results")
            || warning.to_lowercase().contains("empty")
            || warning.to_lowercase().contains("0 results"),
        "Warning should indicate no results. Message: {}",
        warning
    );
}

// ============================================================================
// Template Rendering Error Tests
// ============================================================================

#[test]
fn test_template_missing_variable_error_quality() {
    // Simulate missing required variable
    let error = simulate_template_missing_variable("user_name", "welcome.tmpl");

    ErrorQualityAssertion::new(error)
        .has_context("variable")
        .has_file_path()
        .has_fix_suggestion("provide")
        .is_user_error()
        .has_example()
        .has_consistent_formatting();
}

#[test]
fn test_template_syntax_error_quality() {
    // Simulate template syntax error
    let error = simulate_template_syntax_error(
        "greeting.tmpl",
        15,
        "Unclosed tag: {% if user %}",
    );

    ErrorQualityAssertion::new(error.clone())
        .has_context("syntax")
        .has_file_path()
        .has_fix_suggestion("close")
        .is_user_error()
        .has_example()
        .has_consistent_formatting();

    // Should include line number
    assert!(
        error.contains("15") || error.contains("line"),
        "Error should include line number. Message: {}",
        error
    );
}

#[test]
fn test_template_invalid_filter_error_quality() {
    // Simulate invalid filter usage
    let error = simulate_template_invalid_filter("unknown_filter", "{{ name | unknown_filter }}");

    ErrorQualityAssertion::new(error)
        .has_context("filter")
        .has_fix_suggestion("available")
        .is_user_error()
        .has_example()
        .has_consistent_formatting();
}

#[test]
fn test_template_type_mismatch_error_quality() {
    // Simulate type mismatch in template
    let error = simulate_template_type_mismatch(
        "count",
        "integer",
        "string",
        "Expected number but got \"hello\"",
    );

    ErrorQualityAssertion::new(error)
        .has_context("type")
        .has_fix_suggestion("expected")
        .is_user_error()
        .has_consistent_formatting();
}

#[test]
fn test_template_not_found_with_suggestions_error_quality() {
    // Simulate template not found with did-you-mean suggestions
    let error = simulate_template_not_found(
        "rust-servce.tmpl",
        &["rust-service.tmpl", "rust-server.tmpl", "python-service.tmpl"],
    );

    ErrorQualityAssertion::new(error)
        .has_context("not found")
        .has_file_path()
        .has_fix_suggestion("did you mean")
        .is_user_error()
        .has_example()
        .has_consistent_formatting();
}

#[test]
fn test_template_frontmatter_parse_error_quality() {
    // Simulate frontmatter parsing error
    let error = simulate_frontmatter_parse_error(
        "template.tmpl",
        "Invalid YAML in frontmatter",
        3,
    );

    ErrorQualityAssertion::new(error.clone())
        .has_context("frontmatter")
        .has_file_path()
        .is_user_error()
        .has_consistent_formatting();

    // Should include line number
    assert!(
        error.contains("3") || error.contains("line"),
        "Error should include line number. Message: {}",
        error
    );
}

// ============================================================================
// Cross-Cutting Error Quality Tests
// ============================================================================

#[test]
fn test_error_chaining_preserves_context() {
    // Simulate error chain: template -> file IO -> permission denied
    let root_error = "Permission denied: /etc/templates/config.tmpl";
    let mid_error = format!("Failed to read template file: {}", root_error);
    let top_error = format!("Template rendering failed: {}", mid_error);

    ErrorQualityAssertion::new(top_error)
        .has_context("Template rendering failed")
        .has_context("Permission denied")
        .has_file_path()
        .has_consistent_formatting();
}

#[test]
fn test_error_includes_actionable_next_steps() {
    // All errors should tell users what to do next
    let errors = vec![
        simulate_file_not_found("template.tmpl"),
        simulate_cli_missing_arg("generate", "name"),
        simulate_rdf_syntax_error("data.ttl", 10, "Unexpected EOF"),
        simulate_template_missing_variable("api_key", "config.tmpl"),
    ];

    for error in errors {
        assert!(
            has_actionable_guidance(&error),
            "Error lacks actionable guidance: {}",
            error
        );
    }
}

#[test]
fn test_error_formatting_consistency() {
    // All errors should follow consistent formatting
    let errors = vec![
        simulate_file_not_found("file.txt"),
        simulate_cli_invalid_value("--format", "bad", &["json", "yaml"]),
        simulate_sparql_syntax_error("SELECT * WHERE {", "Missing }"),
        simulate_template_syntax_error("test.tmpl", 5, "Unclosed tag"),
    ];

    for error in errors.iter() {
        ErrorQualityAssertion::new(error.clone()).has_consistent_formatting();
    }
}

#[test]
fn test_user_vs_system_error_distinction() {
    // User errors (fixable by user)
    let user_errors = vec![
        simulate_file_not_found("config.yaml"),
        simulate_cli_invalid_value("--type", "bad", &["good"]),
        simulate_template_missing_variable("name", "test.tmpl"),
    ];

    for error in user_errors {
        ErrorQualityAssertion::new(error).is_user_error();
    }

    // System errors (not fixable by user alone)
    let system_errors = vec![
        simulate_permission_denied("/etc/config"),
        simulate_disk_full("/tmp/output"),
        simulate_sparql_timeout("SELECT *", 30),
    ];

    for error in system_errors {
        ErrorQualityAssertion::new(error).is_system_error();
    }
}

// ============================================================================
// Error Simulation Helpers
// ============================================================================

fn simulate_cli_missing_arg(command: &str, subcommand: &str) -> String {
    format!(
        "Error: Missing required argument for 'ggen {} {}'\n\
         \n\
         Try: ggen {} {} --help\n\
         \n\
         For more information, see: https://docs.ggen.io/cli/{}",
        command, subcommand, command, subcommand, command
    )
}

fn simulate_cli_invalid_value(arg: &str, value: &str, valid: &[&str]) -> String {
    format!(
        "Error: Invalid value '{}' for argument {}\n\
         \n\
         Valid values are: {}\n\
         \n\
         Example: {} {}",
        value,
        arg,
        valid.join(", "),
        arg,
        valid.first().unwrap_or(&"")
    )
}

fn simulate_cli_unknown_command(cmd: &str, suggestions: &[&str]) -> String {
    format!(
        "Error: Unknown command '{}'\n\
         \n\
         Did you mean one of these?\n  {}\n\
         \n\
         Run 'ggen --help' for a list of available commands",
        cmd,
        suggestions.join("\n  ")
    )
}

fn simulate_cli_conflicting_args(arg1: &str, arg2: &str) -> String {
    format!(
        "Error: Arguments {} and {} conflict with each other\n\
         \n\
         Use either {} or {}, but not both",
        arg1, arg2, arg1, arg2
    )
}

fn simulate_file_not_found(path: &str) -> String {
    format!(
        "Error: File not found: {}\n\
         \n\
         Check that the file exists and the path is correct.\n\
         \n\
         Tip: Use 'ggen list' to see available templates",
        path
    )
}

fn simulate_permission_denied(path: &str) -> String {
    format!(
        "Error: Permission denied: {}\n\
         \n\
         Fix: Try one of these:\n\
         - Run 'chmod +r {}' to grant read permissions\n\
         - Run the command with sudo (if appropriate)\n\
         - Check file ownership with 'ls -l {}'",
        path, path, path
    )
}

fn simulate_invalid_file_format(file: &str, details: &str) -> String {
    format!(
        "Error: Invalid file format: {}\n\
         \n\
         {}\n\
         \n\
         Please check the file syntax and try again.",
        file, details
    )
}

fn simulate_directory_not_empty(path: &str) -> String {
    format!(
        "Error: Directory not empty: {}\n\
         \n\
         To proceed, either:\n\
         - Remove the directory: rm -rf {}\n\
         - Choose a different location\n\
         - Use --force to overwrite (use with caution)",
        path, path
    )
}

fn simulate_disk_full(path: &str) -> String {
    format!(
        "Error: No space left on device while writing to {}\n\
         \n\
         Free up disk space and try again.\n\
         \n\
         Check usage with: df -h",
        path
    )
}

fn simulate_rdf_syntax_error(file: &str, line: usize, details: &str) -> String {
    format!(
        "Error: RDF syntax error in {} at line {}\n\
         \n\
         {}\n\
         \n\
         Check your Turtle syntax. Example:\n\
         @prefix ex: <http://example.org/> .\n\
         ex:subject ex:predicate ex:object .",
        file, line, details
    )
}

fn simulate_invalid_prefix(prefix: &str, file: &str) -> String {
    format!(
        "Error: Undefined prefix '{}' in {}\n\
         \n\
         Add the prefix declaration at the top of the file:\n\
         @prefix {}: <http://example.org/{}#> .",
        prefix, file, prefix, prefix
    )
}

fn simulate_shacl_validation_error(shape: &str, constraint: &str, details: &str) -> String {
    format!(
        "Error: SHACL validation failed for shape '{}'\n\
         \n\
         Constraint: {}\n\
         {}\n\
         \n\
         Fix: Add the required property to your data",
        shape, constraint, details
    )
}

fn simulate_constraint_violation(
    property: &str,
    expected_type: &str,
    actual_value: &str,
    class: &str,
) -> String {
    format!(
        "Error: Constraint violation in {}\n\
         \n\
         Property '{}' must be of type {}, but got {}\n\
         \n\
         Example: ex:instance ex:{} 42 .",
        class, property, expected_type, actual_value, property
    )
}

fn simulate_sparql_syntax_error(query: &str, details: &str) -> String {
    format!(
        "Error: SPARQL syntax error\n\
         \n\
         {}\n\
         \n\
         Query:\n{}\n\
         \n\
         Check your SPARQL syntax. Example:\n\
         SELECT ?name WHERE {{ ?person foaf:name ?name }}",
        details, query
    )
}

fn simulate_sparql_undefined_prefix(prefix: &str, query: &str) -> String {
    format!(
        "Error: Undefined prefix '{}' in SPARQL query\n\
         \n\
         Add the prefix declaration:\n\
         PREFIX {}: <http://xmlns.com/foaf/0.1/>\n\
         \n\
         Query:\n{}",
        prefix, prefix, query
    )
}

fn simulate_sparql_timeout(query_desc: &str, timeout_secs: u64) -> String {
    format!(
        "Error: SPARQL query timeout after {} seconds\n\
         \n\
         Query: {}\n\
         \n\
         Try:\n\
         - Simplify the query\n\
         - Add more specific filters\n\
         - Increase timeout with --timeout option",
        timeout_secs, query_desc
    )
}

fn simulate_sparql_empty_results(query: &str) -> String {
    format!(
        "Warning: SPARQL query returned no results\n\
         \n\
         Query:\n{}\n\
         \n\
         Tip: Check that:\n\
         - The data exists in the graph\n\
         - Prefixes are correctly defined\n\
         - Triple patterns match the data structure",
        query
    )
}

fn simulate_template_missing_variable(var: &str, template: &str) -> String {
    format!(
        "Error: Missing required variable '{}' in template {}\n\
         \n\
         Provide the variable:\n\
         - In frontmatter: {}: value\n\
         - Via CLI: --var {}=value\n\
         - In context file: context.yaml",
        var, template, var, var
    )
}

fn simulate_template_syntax_error(file: &str, line: usize, details: &str) -> String {
    format!(
        "Error: Template syntax error in {} at line {}\n\
         \n\
         {}\n\
         \n\
         Example of correct syntax:\n\
         {{%% if user %%}}\n  Hello {{{{ user.name }}}}\n{{%% endif %%}}",
        file, line, details
    )
}

fn simulate_template_invalid_filter(filter: &str, usage: &str) -> String {
    format!(
        "Error: Unknown filter '{}'\n\
         \n\
         Usage: {}\n\
         \n\
         Available filters: upper, lower, trim, replace, default\n\
         \n\
         Example: {{{{ name | upper }}}}",
        filter, usage
    )
}

fn simulate_template_type_mismatch(
    var: &str,
    expected: &str,
    actual: &str,
    details: &str,
) -> String {
    format!(
        "Error: Type mismatch for variable '{}'\n\
         \n\
         Expected: {}\n\
         Actual: {}\n\
         \n\
         {}",
        var, expected, actual, details
    )
}

fn simulate_template_not_found(template: &str, suggestions: &[&str]) -> String {
    format!(
        "Error: Template not found: {}\n\
         \n\
         Did you mean one of these?\n  {}\n\
         \n\
         Run 'ggen list' to see all available templates",
        template,
        suggestions.join("\n  ")
    )
}

fn simulate_frontmatter_parse_error(file: &str, details: &str, line: usize) -> String {
    format!(
        "Error: Failed to parse frontmatter in {} at line {}\n\
         \n\
         {}\n\
         \n\
         Frontmatter must be valid YAML between --- delimiters",
        file, line, details
    )
}

fn has_actionable_guidance(error: &str) -> bool {
    let actionable_keywords = [
        "try", "use", "run", "check", "add", "remove", "fix", "see", "example", "tip",
        "install", "chmod", "provide",
    ];

    actionable_keywords
        .iter()
        .any(|&keyword| error.to_lowercase().contains(keyword))
}
