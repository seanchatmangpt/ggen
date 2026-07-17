//! Static frontmatter/SPARQL cross-checks for templates (`ggen graph validate`).
//!
//! Pure text analysis — no query execution. Three laws:
//!
//! - **FM-TPL-003** — the Tera body consumes a variable the frontmatter's
//!   SPARQL queries do not project.
//! - **FM-TPL-004** — the `to:` output path consumes a variable the
//!   frontmatter's SPARQL queries do not project.
//! - **FM-TPL-005** — an identity `construct:` (CONSTRUCT pattern textually
//!   equal to its WHERE pattern) is a no-op enrichment.
//!
//! `SELECT *` disables the projection checks for that template (the projected
//! set is unknowable at author time — old GGEN-QUERY-002 semantics: the check
//! is skipped, not failed).

use std::{collections::BTreeSet, path::Path};

use crate::{
    error::AppError,
    template::{Frontmatter, Template},
};

/// The variable set a template's SPARQL queries project into the Tera context.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Projection {
    /// At least one query uses `SELECT *`: the projected set is unknowable,
    /// so the unbound-variable checks are disabled for this template.
    Wildcard,
    /// The exact set of context names the renderer will inject.
    Vars(BTreeSet<String>),
}

/// Tera expression keywords / builtins that are never query projections.
const NON_VARS: &[&str] = &["loop", "true", "false", "and", "or", "not", "in", "is"];

/// Return `true` when `s` is a plain identifier (`[A-Za-z_][A-Za-z0-9_]*`).
fn is_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// The root identifier of an expression head: `row.name` → `row`,
/// `name | upper` handled by the caller, literals/numbers → `None`.
/// A root immediately followed by `(` is a function call, not a variable.
fn root_ident(expr: &str) -> Option<String> {
    let expr = expr.trim();
    if expr.is_empty() {
        return None;
    }
    let root: String = expr
        .chars()
        .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
        .collect();
    if !is_identifier(&root) || NON_VARS.contains(&root.as_str()) {
        return None;
    }
    // Function call (e.g. `sparql(query=…)`) — not a context variable.
    if expr[root.len()..].trim_start().starts_with('(') {
        return None;
    }
    Some(root)
}

/// Variables a Tera source *consumes* from the render context.
///
/// Scans `{{ var }}`, `{{ var | filter }}`, `{{ var.field }}` (root only),
/// `{% if var %}` / `{% elif var %}`, and the source of
/// `{% for x in var %}`. Names bound locally by `{% for %}` / `{% set %}`
/// are subtracted; string literals, numbers, and function calls are ignored.
#[must_use]
pub fn consumed_vars(tera_source: &str) -> BTreeSet<String> {
    let mut consumed = BTreeSet::new();
    let mut locals = BTreeSet::new();

    // `{{ … }}` interpolations: root of the pre-filter head.
    for (start, _) in tera_source.match_indices("{{") {
        let Some(rel_end) = tera_source[start..].find("}}") else {
            continue;
        };
        let expr = tera_source[start + 2..start + rel_end].trim();
        let head = expr.split('|').next().unwrap_or("").trim();
        if let Some(name) = root_ident(head) {
            consumed.insert(name);
        }
    }

    // `{% … %}` tags: for/set introduce locals; if/elif/for consume roots.
    for (start, _) in tera_source.match_indices("{%") {
        let Some(rel_end) = tera_source[start..].find("%}") else {
            continue;
        };
        let inner = tera_source[start + 2..start + rel_end]
            .trim()
            .trim_start_matches('-')
            .trim();
        if let Some(rest) = inner.strip_prefix("for ") {
            if let Some(in_idx) = rest.find(" in ") {
                for name in rest[..in_idx].split(',') {
                    let ident = name.trim();
                    if is_identifier(ident) {
                        locals.insert(ident.to_string());
                    }
                }
                if let Some(name) = root_ident(&rest[in_idx + 4..]) {
                    consumed.insert(name);
                }
            }
        } else if let Some(rest) = inner.strip_prefix("set ") {
            let mut parts = rest.splitn(2, '=');
            let name = parts.next().unwrap_or("").trim();
            if is_identifier(name) {
                locals.insert(name.to_string());
            }
            if let Some(rhs) = parts.next() {
                let head = rhs.split('|').next().unwrap_or("").trim();
                if let Some(root) = root_ident(head) {
                    consumed.insert(root);
                }
            }
        } else if let Some(rest) = inner
            .strip_prefix("if ")
            .or_else(|| inner.strip_prefix("elif "))
        {
            let head = rest.trim().trim_start_matches("not ").trim();
            if let Some(name) = root_ident(head) {
                consumed.insert(name);
            }
        }
    }

    for local in locals {
        consumed.remove(&local);
    }
    consumed
}

/// The variables the sync renderer injects for a template's frontmatter.
///
/// Matches `sync.rs`'s render stage exactly: the implicit `results` array,
/// each named `sparql:` key (exposed as a context array under its name),
/// `row` plus the per-row flattened SELECT columns (per-row `to:` mode),
/// and every `?var` projected by each SELECT clause. Any `SELECT *` returns
/// [`Projection::Wildcard`], disabling the check for this template.
#[must_use]
pub fn projected_vars(fm: &Frontmatter) -> Projection {
    let mut vars = BTreeSet::new();
    vars.insert("results".to_string());
    vars.insert("row".to_string());
    for (name, query) in &fm.sparql {
        vars.insert(name.clone());
        match select_projection(query) {
            SelectProjection::Wildcard => return Projection::Wildcard,
            SelectProjection::Vars(cols) => vars.extend(cols),
            SelectProjection::NotSelect => {}
        }
    }
    Projection::Vars(vars)
}

/// Result of scanning one query's SELECT clause.
enum SelectProjection {
    Wildcard,
    Vars(BTreeSet<String>),
    NotSelect,
}

/// Extract the `?var` tokens between `SELECT` and the following `WHERE`/`{`.
fn select_projection(query: &str) -> SelectProjection {
    // Strip comment lines first.
    let stripped: String = query
        .lines()
        .filter(|l| !l.trim_start().starts_with('#'))
        .collect::<Vec<_>>()
        .join(" ");
    let upper = stripped.to_uppercase();
    let Some(sel) = upper.find("SELECT") else {
        return SelectProjection::NotSelect;
    };
    let clause_start = sel + "SELECT".len();
    let clause_end = upper[clause_start..]
        .find("WHERE")
        .or_else(|| upper[clause_start..].find('{'))
        .map_or(stripped.len(), |i| clause_start + i);
    let clause = &stripped[clause_start..clause_end];
    if clause.contains('*') {
        return SelectProjection::Wildcard;
    }
    let mut cols = BTreeSet::new();
    for (idx, _) in clause.match_indices(['?', '$']) {
        let name: String = clause[idx + 1..]
            .chars()
            .take_while(|c| c.is_ascii_alphanumeric() || *c == '_')
            .collect();
        if is_identifier(&name) {
            cols.insert(name);
        }
    }
    SelectProjection::Vars(cols)
}

/// Returns `true` when `query` is an identity CONSTRUCT: `CONSTRUCT WHERE`
/// shorthand, or a CONSTRUCT pattern textually equal to its WHERE pattern
/// modulo whitespace (a no-op enrichment).
#[must_use]
pub fn is_identity_construct(query: &str) -> bool {
    let stripped: String = query
        .lines()
        .filter(|l| !l.trim_start().starts_with('#'))
        .collect::<Vec<_>>()
        .join(" ");
    let upper = stripped.to_uppercase();
    let Some(pos) = upper.find("CONSTRUCT") else {
        return false;
    };

    // Shorthand form: `CONSTRUCT WHERE { … }` is identity by definition.
    if upper[pos + "CONSTRUCT".len()..]
        .trim_start()
        .starts_with("WHERE")
    {
        return true;
    }

    // Explicit form: CONSTRUCT { A } WHERE { B } with A == B modulo whitespace.
    let (Some(s1), Some(e1)) = (upper.find('{'), upper.find('}')) else {
        return false;
    };
    if e1 <= s1 {
        return false;
    }
    let block1 = normalize_ws(&upper[s1 + 1..e1]);
    let Some(where_rel) = upper[e1 + 1..].find("WHERE") else {
        return false;
    };
    let remainder = &upper[e1 + 1 + where_rel + "WHERE".len()..];
    let (Some(s2), Some(e2)) = (remainder.find('{'), remainder.find('}')) else {
        return false;
    };
    if e2 <= s2 {
        return false;
    }
    let block2 = normalize_ws(&remainder[s2 + 1..e2]);
    !block1.is_empty() && block1 == block2
}

/// Collapse all whitespace runs to single spaces and trim.
fn normalize_ws(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

/// Run all static checks on one parsed template. Returns every violation
/// (empty vec = clean). Each error names the template path, the variable,
/// and a remediation.
#[must_use]
pub fn lint_template(tpl_path: &Path, t: &Template) -> Vec<AppError> {
    let mut errors = Vec::new();

    match projected_vars(&t.frontmatter) {
        Projection::Wildcard => {
            // SELECT * disables the unbound-variable checks (old
            // GGEN-QUERY-002 semantics: skip, do not fail).
        }
        Projection::Vars(projected) => {
            for var in consumed_vars(&t.body) {
                if !projected.contains(&var) {
                    errors.push(AppError::fm_tpl(
                        3,
                        format!(
                            "template `{}`: body consumes `{{{{ {var} }}}}` but no \
                             frontmatter SPARQL query projects `?{var}`. \
                             Remediation: add `?{var}` to a SELECT clause or fix the \
                             variable name in the template body.",
                            tpl_path.display()
                        ),
                    ));
                }
            }
            for var in consumed_vars(&t.frontmatter.to) {
                if !projected.contains(&var) {
                    errors.push(AppError::fm_tpl(
                        4,
                        format!(
                            "template `{}`: `to:` path consumes `{{{{ {var} }}}}` but no \
                             frontmatter SPARQL query projects `?{var}`. \
                             Remediation: add `?{var}` to a SELECT clause or fix the \
                             variable name in the `to:` pattern.",
                            tpl_path.display()
                        ),
                    ));
                }
            }
        }
    }

    for (name, query) in &t.frontmatter.sparql {
        let upper = query.to_uppercase();
        if upper.contains("SELECT") && !upper.contains("ORDER BY") {
            errors.push(AppError::fm_tpl(
                10,
                format!(
                    "template `{}`: sparql `{name}` is a SELECT without ORDER BY — \
                     row order is engine-dependent, so generated output is not \
                     deterministic across SPARQL engine versions. \
                     Remediation: add an ORDER BY clause over the projected variables.",
                    tpl_path.display()
                ),
            ));
        }
    }

    if let Some(construct) = t.frontmatter.construct.as_deref() {
        if is_identity_construct(construct) {
            errors.push(AppError::fm_tpl(
                5,
                format!(
                    "template `{}`: `construct:` is an identity CONSTRUCT (its \
                     CONSTRUCT pattern equals its WHERE pattern) — a no-op enrichment. \
                     Remediation: make the CONSTRUCT pattern produce new triples, or \
                     remove the `construct:` block.",
                    tpl_path.display()
                ),
            ));
        }
    }

    errors
}

#[cfg(test)]
#[allow(clippy::expect_used)]
mod tests {
    use super::*;

    fn parse(content: &str) -> Template {
        Template::parse(content).expect("parse template")
    }

    #[test]
    fn consumed_vars_roots_filters_fields_and_tags() {
        let src = "{{ name }} {{ name | upper }} {{ row.field }} \
                   {% if flag %}x{% endif %} {% for x in items %}{{ x }}{% endfor %} \
                   {{ \"literal\" }} {{ 42 }} {{ sparql(query=q) }} {{ loop.index }}";
        let vars = consumed_vars(src);
        assert_eq!(
            vars,
            ["name", "row", "flag", "items"]
                .iter()
                .map(ToString::to_string)
                .collect::<BTreeSet<_>>()
        );
    }

    #[test]
    fn loop_bound_names_are_local_not_consumed() {
        let vars = consumed_vars("{% for row in results %}{{ row.name }}{% endfor %}");
        assert_eq!(
            vars,
            ["results".to_string()].into_iter().collect::<BTreeSet<_>>()
        );
    }

    #[test]
    fn set_binds_local_and_consumes_rhs() {
        let vars = consumed_vars("{% set y = source %}{{ y }}");
        assert_eq!(
            vars,
            ["source".to_string()].into_iter().collect::<BTreeSet<_>>()
        );
    }

    #[test]
    fn projected_vars_includes_names_columns_and_implicits() {
        let t = parse(
            "---\nto: out.rs\nsparql:\n  people: SELECT ?name ?age WHERE { ?s ?p ?o } ORDER BY ?name\n---\nbody",
        );
        let Projection::Vars(vars) = projected_vars(&t.frontmatter) else {
            panic!("expected Vars");
        };
        for expected in ["results", "row", "people", "name", "age"] {
            assert!(vars.contains(expected), "missing {expected}: {vars:?}");
        }
    }

    #[test]
    fn select_star_yields_wildcard() {
        let t = parse("---\nto: out.rs\nsparql:\n  all: SELECT * WHERE { ?s ?p ?o } ORDER BY ?s\n---\n{{ anything_goes }}");
        assert_eq!(projected_vars(&t.frontmatter), Projection::Wildcard);
        assert!(
            lint_template(Path::new("t.tmpl"), &t).is_empty(),
            "wildcard disables check"
        );
    }

    #[test]
    fn unbound_body_var_is_fm_tpl_003() {
        let t = parse(
            "---\nto: out.rs\nsparql:\n  people: SELECT ?name WHERE { ?s ?p ?o } ORDER BY ?name\n---\n{{ typo }}",
        );
        let errs = lint_template(Path::new("t.tmpl"), &t);
        assert_eq!(errs.len(), 1);
        let msg = errs[0].to_string();
        assert!(msg.contains("FM-TPL-003"), "{msg}");
        assert!(msg.contains("typo"), "{msg}");
        assert!(msg.contains("t.tmpl"), "{msg}");
    }

    #[test]
    fn unbound_to_path_var_is_fm_tpl_004() {
        let t = parse(
            "---\nto: \"out/{{ missing }}.rs\"\nsparql:\n  people: SELECT ?name WHERE { ?s ?p ?o } ORDER BY ?name\n---\n{{ name }}",
        );
        let errs = lint_template(Path::new("t.tmpl"), &t);
        assert_eq!(errs.len(), 1);
        let msg = errs[0].to_string();
        assert!(msg.contains("FM-TPL-004"), "{msg}");
        assert!(msg.contains("missing"), "{msg}");
    }

    #[test]
    fn identity_construct_is_fm_tpl_005() {
        let t = parse(
            "---\nto: out.rs\nconstruct: \"CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\"\n---\nstatic",
        );
        let errs = lint_template(Path::new("t.tmpl"), &t);
        assert_eq!(errs.len(), 1);
        assert!(errs[0].to_string().contains("FM-TPL-005"), "{}", errs[0]);
    }

    #[test]
    fn construct_where_shorthand_is_identity() {
        assert!(is_identity_construct("CONSTRUCT WHERE { ?s ?p ?o }"));
    }

    #[test]
    fn non_identity_construct_passes() {
        assert!(!is_identity_construct(
            "CONSTRUCT { ?s <http://example.org/derived> ?o } WHERE { ?s ?p ?o }"
        ));
    }

    #[test]
    fn clean_template_has_no_diagnostics() {
        let t = parse(
            "---\nto: out.rs\nsparql:\n  people: SELECT ?name WHERE { ?s ?p ?o } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}{% endfor %}",
        );
        assert!(lint_template(Path::new("t.tmpl"), &t).is_empty());
    }

    #[test]
    fn select_without_order_by_is_fm_tpl_010() {
        let t = parse(
            "---\nto: out.rs\nsparql:\n  people: SELECT ?name WHERE { ?s ?p ?o }\n---\n{{ name }}",
        );
        let errs = lint_template(Path::new("t.tmpl"), &t);
        assert_eq!(errs.len(), 1);
        let msg = errs[0].to_string();
        assert!(msg.contains("FM-TPL-010"), "{msg}");
        assert!(msg.contains("ORDER BY"), "{msg}");
    }
}
