//! Gall CP39: the general dispatcher, scoped to route only into CP33's
//! already-reviewed bounded path -- never a new, broader write path. Closes
//! the "nothing consumes a pushed signal to trigger an action" gap named in
//! the first-principles plan's item #1, without reopening item #2's
//! rejected "any trigger -> any action" CP21 dispatcher.
//!
//! A signal (identified by its own code -- an `FM-*` sync-refusal code, or a
//! `GGEN-*` diagnostic code) is routed by querying the CONSUMING PROJECT's
//! own `.specify/repo-facts.ttl` (if present) for an `rf:DiagnosticCode`
//! individual whose `rf:code` matches, then reading its `rf:dispatchRoute`.
//! Absent file, absent match, or any value other than the two declared ones
//! all fail closed to `"attended"` -- the safe default is toward the
//! existing human/LLM-reviewed path, never toward autonomy.

use std::path::Path;

use ggen_engine::graph::{DeterministicGraph, EngineQueryResults, EngineValue, GraphEngine};

use crate::error::McpError;

/// A signal's declared dispatch route.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DispatchRoute {
    /// The default. No fact declared, no facts file, or an unrecognized
    /// value -- the existing push-only, human/LLM-reviewed path is
    /// unchanged.
    Attended,
    /// `rf:dispatchRoute "bounded-unattended"` declared for this exact
    /// code. The caller should ALSO attempt a real dispatch via
    /// `crate::tools::unattended_dispatch::try_unattended_apply`, in
    /// addition to (never instead of) its existing push.
    BoundedUnattended,
}

/// Query `root/.specify/repo-facts.ttl` (if present) for the declared
/// `rf:dispatchRoute` of the `rf:DiagnosticCode` individual whose `rf:code`
/// contains `signal_code` (substring match, since this repo's own facts
/// wrap codes in markdown bold, e.g. `"**GGEN-TPL-001**"` -- matching by
/// substring rather than requiring callers to know that formatting detail).
///
/// # Errors
/// Only for a real Turtle parse failure on an EXISTING facts file (a
/// malformed fact is worth surfacing, not silently swallowing) -- a
/// missing facts file is not an error, it's `DispatchRoute::Attended`.
pub fn route_signal(signal_code: &str, root: &Path) -> Result<DispatchRoute, McpError> {
    let facts_path = root.join(".specify/repo-facts.ttl");
    if !facts_path.is_file() {
        return Ok(DispatchRoute::Attended);
    }
    let ttl = std::fs::read_to_string(&facts_path).map_err(|e| {
        McpError::new(
            crate::error::ErrorCategory::GraphLoadError,
            format!("{} unreadable: {e}", facts_path.display()),
        )
    })?;
    let graph = DeterministicGraph::new().map_err(|e| {
        McpError::new(
            crate::error::ErrorCategory::GraphLoadError,
            format!("failed to construct graph engine: {e}"),
        )
    })?;
    graph.insert_turtle(&ttl).map_err(|e| {
        McpError::new(
            crate::error::ErrorCategory::GraphLoadError,
            format!("{} failed to parse as Turtle: {e}", facts_path.display()),
        )
    })?;

    // Reuses the exact local-name-suffix matching technique
    // `pack_capabilities.rs::query_contract_predicate` already established
    // for `rf:triggersAction` (CP30) -- same STRENDS-disjunct approach, no
    // new matching mechanism, applied here to find `rf:code`/`rf:dispatchRoute`
    // regardless of which prefix a project's own facts file uses for `rf:`.
    let sparql = format!(
        r##"
        SELECT ?route WHERE {{
            ?s ?codePred ?code .
            FILTER (STRENDS(STR(?codePred), "#code") || STRENDS(STR(?codePred), "/code"))
            FILTER (CONTAINS(STR(?code), {signal_code:?}))
            ?s ?routePred ?route .
            FILTER (STRENDS(STR(?routePred), "#dispatchRoute") || STRENDS(STR(?routePred), "/dispatchRoute"))
        }}
        "##
    );
    let results = GraphEngine::query(&graph, &sparql)
        .map_err(|e| McpError::new(crate::error::ErrorCategory::GraphLoadError, e.to_string()))?;
    let EngineQueryResults::Solutions(rows) = results else {
        return Ok(DispatchRoute::Attended);
    };
    for row in &rows {
        if let Some(EngineValue::String(route)) = row.get("route") {
            if route == "bounded-unattended" {
                return Ok(DispatchRoute::BoundedUnattended);
            }
        }
    }
    Ok(DispatchRoute::Attended)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn absent_facts_file_is_attended() {
        let tmp = TempDir::new().expect("tempdir");
        let route = route_signal("GGEN-TPL-001", tmp.path()).expect("route_signal");
        assert_eq!(route, DispatchRoute::Attended);
    }

    #[test]
    fn declared_bounded_unattended_route_is_read_back() {
        let tmp = TempDir::new().expect("tempdir");
        std::fs::create_dir_all(tmp.path().join(".specify")).expect("mkdir");
        std::fs::write(
            tmp.path().join(".specify/repo-facts.ttl"),
            r#"@prefix rf: <http://ggen.org/repo-facts#> .
rf:diag_01 a rf:DiagnosticCode ;
    rf:code "**FM-PACK-013**" ;
    rf:dispatchRoute "bounded-unattended" .
"#,
        )
        .expect("write facts");
        let route = route_signal("FM-PACK-013", tmp.path()).expect("route_signal");
        assert_eq!(route, DispatchRoute::BoundedUnattended);
    }

    #[test]
    fn undeclared_code_in_a_real_facts_file_is_attended() {
        let tmp = TempDir::new().expect("tempdir");
        std::fs::create_dir_all(tmp.path().join(".specify")).expect("mkdir");
        std::fs::write(
            tmp.path().join(".specify/repo-facts.ttl"),
            r#"@prefix rf: <http://ggen.org/repo-facts#> .
rf:diag_01 a rf:DiagnosticCode ;
    rf:code "**FM-PACK-013**" ;
    rf:dispatchRoute "bounded-unattended" .
"#,
        )
        .expect("write facts");
        let route = route_signal("FM-PACK-999", tmp.path()).expect("route_signal");
        assert_eq!(
            route,
            DispatchRoute::Attended,
            "a code with no declared fact must fail closed to attended"
        );
    }

    #[test]
    fn unrecognized_route_value_fails_closed_to_attended() {
        let tmp = TempDir::new().expect("tempdir");
        std::fs::create_dir_all(tmp.path().join(".specify")).expect("mkdir");
        std::fs::write(
            tmp.path().join(".specify/repo-facts.ttl"),
            r#"@prefix rf: <http://ggen.org/repo-facts#> .
rf:diag_01 a rf:DiagnosticCode ;
    rf:code "**FM-PACK-013**" ;
    rf:dispatchRoute "some-future-value-this-code-does-not-understand" .
"#,
        )
        .expect("write facts");
        let route = route_signal("FM-PACK-013", tmp.path()).expect("route_signal");
        assert_eq!(route, DispatchRoute::Attended);
    }
}
