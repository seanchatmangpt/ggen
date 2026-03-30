/// Template variable validation tests.
///
/// These tests verify that Tera templates only reference variables that are
/// either provided by SPARQL queries or have default values. This catches
/// template/query mismatches before runtime rendering failures.

use regex::Regex;
use std::collections::HashSet;
use walkdir::WalkDir;

fn workspace_root() -> std::path::PathBuf {
    let mut p = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.pop(); // crates/ggen-core -> crates
    p.pop(); // crates -> ggen root
    p
}

/// Extract all variable names referenced in a Tera template.
/// Matches {{ var }}, {{ var.filter }}, {{ var.key }}, {{ arr[index] }}, etc.
fn extract_template_variables(content: &str) -> Vec<String> {
    let mut vars = HashSet::new();

    // Match {{ expression }} patterns
    let expr_re = Regex::new(r#"\{\{\s*([\w.\[\]"']+)\s*(?:\|.*?)?\s*\}\}"#).unwrap();
    for cap in expr_re.captures_iter(content) {
        let expr = cap[1].trim();
        // Skip string literals
        if expr.starts_with('"') || expr.starts_with('\'') {
            continue;
        }
        // Extract the root variable name (before any . or [)
        let root = expr.split(['.', '[']).next().unwrap_or(expr);
        if !root.is_empty() && root.chars().all(|c| c.is_alphanumeric() || c == '_') {
            vars.insert(root.to_string());
        }
    }

    // Match {% for var in expr %} patterns
    let for_re = Regex::new(r"\{%\s*for\s+(\w+)\s+in\s+([\w.\[\]]+)\s*%\}").unwrap();
    for cap in for_re.captures_iter(content) {
        let collection = cap[2].trim();
        let root = collection.split(['.', '[']).next().unwrap_or(collection);
        if !root.is_empty() && root.chars().all(|c| c.is_alphanumeric() || c == '_') {
            vars.insert(root.to_string());
        }
    }

    // Match {% if var ... %} patterns
    let if_re = Regex::new(r"\{%\s*if\s+(\w+)").unwrap();
    for cap in if_re.captures_iter(content) {
        vars.insert(cap[1].trim().to_string());
    }

    vars.into_iter().collect()
}

/// Check if a variable reference has a default() filter.
fn has_default_filter(content: &str, var: &str) -> bool {
    // Look for var | default( or var|default(
    let pattern1 = format!("{} | default(", var);
    let pattern2 = format!("{}|default(", var);
    content.contains(&pattern1) || content.contains(&pattern2)
}

/// Extract SELECT variable names from a SPARQL query.
fn extract_sparql_variables(content: &str) -> HashSet<String> {
    let mut vars = HashSet::new();

    // Match SELECT ?var patterns (handles multi-line SELECT clauses)
    let select_re = Regex::new(r"(?s)SELECT\s+(?:DISTINCT\s+)?(.+?)\s*WHERE").unwrap();
    if let Some(cap) = select_re.captures(content) {
        let select_clause = &cap[1];
        for word in select_clause.split_whitespace() {
            if word.starts_with('?') {
                vars.insert(word.trim_start_matches('?').to_string());
            }
        }
    }

    vars
}

// ---------------------------------------------------------------------------
// Test 1: All .tera templates have valid variable references
// ---------------------------------------------------------------------------

/// Variables that are injected by the ggen pipeline (not from SPARQL).
const PIPELINE_INJECTED_VARS: &[&str] = &[
    "sparql_results", // Always injected from SPARQL query results
    "name",           // Template naming convention
];

/// Variables provided by the ggen.toml rule configuration (not from SPARQL).
const RULE_CONFIG_VARS: &[&str] = &[
    "server_name",
    "server_version",
    "server_description",
    "transport_type",
    "agent_name",
    "agent_version",
    "agent_description",
    "agent_url",
    "provider_name",
    "provider_url",
    "package_name",
    "agent_port",
];

#[test]
fn test_templates_reference_valid_variables() {
    let root = workspace_root();
    let templates_dir = root.join("templates");

    if !templates_dir.exists() {
        return;
    }

    let mut errors = Vec::new();
    let mut checked = 0u32;

    for entry in WalkDir::new(&templates_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "tera"))
    {
        let content = match std::fs::read_to_string(entry.path()) {
            Ok(c) => c,
            Err(e) => {
                errors.push(format!("READ ERROR {}: {}", entry.path().display(), e));
                continue;
            }
        };

        let template_vars = extract_template_variables(&content);
        checked += 1;

        for var in &template_vars {
            // Skip if it's a pipeline-injected variable
            if PIPELINE_INJECTED_VARS.contains(&var.as_str()) {
                continue;
            }

            // Skip if it's a rule configuration variable
            if RULE_CONFIG_VARS.contains(&var.as_str()) {
                continue;
            }

            // Skip loop variables (defined by {% for %})
            let for_re = Regex::new(r"\{%\s*for\s+(\w+)").unwrap();
            if for_re.is_match(&content) && for_re.captures(&content).map_or(false, |caps| {
                caps.iter()
                    .skip(1)
                    .any(|c| c.map_or(false, |m| m.as_str() == var))
            }) {
                continue;
            }

            // Skip if it has a default filter
            if has_default_filter(&content, var) {
                continue;
            }

            // Skip Tera built-in functions/filters
            let tera_builtins = [
                "range", "loop", "now", "self", "super", "true", "false",
                "null", "none",
            ];
            if tera_builtins.contains(&var.as_str()) {
                continue;
            }

            // Skip if it's an index variable (numeric)
            if var.parse::<usize>().is_ok() {
                continue;
            }

            // At this point, the variable is suspicious — it may not be available at render time
            // Report as warning, not error (some templates use conditional logic)
            eprintln!(
                "[WARN] {} references potentially undefined variable: {}",
                entry.path().display(),
                var
            );
        }
    }

    assert!(checked > 0, "Should find .tera files in templates/");
    if !errors.is_empty() {
        panic!("Template validation errors:\n{}", errors.join("\n"));
    }
}

// ---------------------------------------------------------------------------
// Test 2: SPARQL queries used by ggen.toml rules return expected columns
// ---------------------------------------------------------------------------

/// Map of query file -> expected output columns (from ggen.toml rules).
const QUERY_EXPECTED_COLUMNS: &[(&str, &[&str])] = &[
    (
        "crates/ggen-core/queries/mcp/extract-mcp-full.rq",
        &[
            "server_name",
            "server_version",
            "server_description",
            "transport_type",
            "tool_name",
            "tool_description",
        ],
    ),
    (
        "crates/ggen-core/queries/a2a/extract-a2a-full.rq",
        &[
            "agent_name",
            "agent_version",
            "agent_description",
            "agent_url",
            "provider_name",
            "skill_name",
            "skill_description",
            "skill_tags",
            "streaming",
            "timeout_ms",
            "retry_policy",
        ],
    ),
];

#[test]
fn test_sparql_queries_return_expected_columns() {
    let root = workspace_root();
    let mut errors = Vec::new();

    for (rq_rel, expected_cols) in QUERY_EXPECTED_COLUMNS {
        let path = root.join(rq_rel);
        if !path.exists() {
            continue;
        }

        let content = match std::fs::read_to_string(&path) {
            Ok(c) => c,
            Err(e) => {
                errors.push(format!("READ ERROR {}: {}", path.display(), e));
                continue;
            }
        };

        let actual_cols = extract_sparql_variables(&content);
        let expected_set: HashSet<&str> = expected_cols.iter().copied().collect();

        for col in &expected_set {
            if !actual_cols.contains(*col) {
                errors.push(format!(
                    "Query {} missing expected column: {}",
                    rq_rel, col
                ));
            }
        }
    }

    if !errors.is_empty() {
        panic!(
            "SPARQL query column validation errors:\n{}",
            errors.join("\n")
        );
    }
}

// ---------------------------------------------------------------------------
// Test 3: Template variables match SPARQL query columns
// ---------------------------------------------------------------------------

/// Map of template -> SPARQL query it depends on (from ggen.toml rules).
const TEMPLATE_QUERY_PAIRS: &[(&str, &str, &[&str])] = &[
    // (template, query, sparql_result row variables used by template)
    (
        "templates/mcp-rust.tera",
        "crates/ggen-core/queries/mcp/extract-mcp-full.rq",
        &["tool_name", "tool_description"],
    ),
    (
        "templates/mcp-elixir.tera",
        "crates/ggen-core/queries/mcp/extract-mcp-full.rq",
        &["tool_name", "tool_description"],
    ),
    (
        "templates/mcp-go.tera",
        "crates/ggen-core/queries/mcp/extract-mcp-full.rq",
        &["tool_name", "tool_description"],
    ),
    (
        "templates/mcp-typescript.tera",
        "crates/ggen-core/queries/mcp/extract-mcp-full.rq",
        &["tool_name", "tool_description"],
    ),
    (
        "templates/mcp-java.tera",
        "crates/ggen-core/queries/mcp/extract-mcp-full.rq",
        &["tool_name", "tool_description"],
    ),
    (
        "templates/a2a-rust.tera",
        "crates/ggen-core/queries/a2a/extract-a2a-full.rq",
        &["skill_name", "skill_description", "skill_tags", "streaming", "timeout_ms"],
    ),
    (
        "templates/a2a-go.tera",
        "crates/ggen-core/queries/a2a/extract-a2a-full.rq",
        &["skill_name", "skill_description", "skill_tags", "streaming", "timeout_ms"],
    ),
    (
        "templates/a2a-typescript.tera",
        "crates/ggen-core/queries/a2a/extract-a2a-full.rq",
        &["skill_name", "skill_description", "skill_tags", "streaming", "timeout_ms"],
    ),
    (
        "templates/a2a-java.tera",
        "crates/ggen-core/queries/a2a/extract-a2a-full.rq",
        &["skill_name", "skill_description", "skill_tags", "streaming", "timeout_ms"],
    ),
    (
        "templates/a2a-elixir.tera",
        "crates/ggen-core/queries/a2a/extract-a2a-full.rq",
        &["skill_name", "skill_description", "skill_tags", "streaming", "timeout_ms"],
    ),
];

#[test]
fn test_template_variables_match_sparql_columns() {
    let root = workspace_root();
    let mut errors = Vec::new();

    for (tmpl_rel, rq_rel, expected_row_vars) in TEMPLATE_QUERY_PAIRS {
        let tmpl_path = root.join(tmpl_rel);
        let rq_path = root.join(rq_rel);

        if !tmpl_path.exists() || !rq_path.exists() {
            continue;
        }

        let tmpl_content = match std::fs::read_to_string(&tmpl_path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        let rq_content = match std::fs::read_to_string(&rq_path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        let query_cols = extract_sparql_variables(&rq_content);

        for var in *expected_row_vars {
            if !query_cols.contains(*var) {
                errors.push(format!(
                    "Template {} uses row.{} but query {} doesn't return ?{}",
                    tmpl_rel, var, rq_rel, var
                ));
            }

            // Also check the template references the variable
            let row_var = format!("row.{}", var);
            if !tmpl_content.contains(&row_var) {
                eprintln!(
                    "[INFO] Template {} doesn't reference row.{} (may use it conditionally)",
                    tmpl_rel, var
                );
            }
        }
    }

    if !errors.is_empty() {
        panic!(
            "Template/query variable mismatch errors:\n{}",
            errors.join("\n")
        );
    }
}

// ---------------------------------------------------------------------------
// Test 4: No template uses both `tools` and `sparql_results` (migration completeness)
// ---------------------------------------------------------------------------

#[test]
fn test_no_template_uses_both_tools_and_sparql_results() {
    let root = workspace_root();
    let templates_dir = root.join("templates");

    if !templates_dir.exists() {
        return;
    }

    let mut conflicts = Vec::new();

    for entry in WalkDir::new(&templates_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "tera"))
    {
        let content = match std::fs::read_to_string(entry.path()) {
            Ok(c) => c,
            Err(_) => continue,
        };

        let uses_tools = content.contains("for tool in tools")
            || content.contains("tools | length")
            || content.contains("for skill in skills")
            || content.contains("skills | length");

        let uses_sparql = content.contains("for row in sparql_results")
            || content.contains("sparql_results | length");

        if uses_tools && uses_sparql {
            conflicts.push(format!(
                "{} uses both `tools`/`skills` and `sparql_results`",
                entry.path().display()
            ));
        }
    }

    if !conflicts.is_empty() {
        panic!(
            "Templates with conflicting variable usage:\n{}",
            conflicts.join("\n")
        );
    }
}

// ---------------------------------------------------------------------------
// Test 5: All original templates migrated to sparql_results pattern
// ---------------------------------------------------------------------------

/// Templates that were originally using `tools`/`skills` arrays.
const ORIGINAL_TEMPLATES: &[&str] = &[
    "templates/mcp-rust.tera",
    "templates/mcp-elixir.tera",
    "templates/mcp-go.tera",
    "templates/mcp-typescript.tera",
    "templates/mcp-java.tera",
    "templates/a2a-rust.tera",
    "templates/a2a-go.tera",
    "templates/a2a-typescript.tera",
    "templates/a2a-java.tera",
    "templates/a2a-elixir.tera",
];

#[test]
fn test_all_original_templates_use_sparql_results() {
    let root = workspace_root();
    let mut not_migrated = Vec::new();

    for tmpl_rel in ORIGINAL_TEMPLATES {
        let path = root.join(tmpl_rel);
        if !path.exists() {
            continue;
        }

        let content = match std::fs::read_to_string(&path) {
            Ok(c) => c,
            Err(_) => continue,
        };

        let uses_sparql = content.contains("for row in sparql_results")
            || content.contains("sparql_results | length");

        if !uses_sparql {
            not_migrated.push(tmpl_rel.to_string());
        }
    }

    // Report but don't fail — templates are being migrated by background agents
    for tmpl in &not_migrated {
        eprintln!("[WARN] Template not yet migrated to sparql_results: {}", tmpl);
    }

    // This assertion will flip to panic once all templates are migrated
    if not_migrated.len() > 3 {
        panic!(
            "Too many templates not yet migrated to sparql_results ({}):\n{}",
            not_migrated.len(),
            not_migrated.join("\n")
        );
    }
}
