//! Public checker facade.
//!
//! The historical checker remains byte-identical in `check.rs` and is mounted as
//! `core`. This facade adds cross-surface GGEN-SRC-004 folding without duplicating
//! or weakening the existing verifier.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use lsp_max::lsp_types::DiagnosticSeverity;

#[path = "check.rs"]
mod core;

pub use core::{
    check_content, discover_law_surfaces, CheckReport, FileReport, NamedCount, RouteSummary,
};
pub(crate) use core::{diag_code, route_source, severity_str, span_str};

#[must_use]
pub fn check_files(paths: &[PathBuf]) -> CheckReport {
    check_files_with_routes(paths, false)
}

#[must_use]
pub fn check_files_with_routes(paths: &[PathBuf], with_routes: bool) -> CheckReport {
    check_files_in_root(Path::new("."), paths, with_routes)
}

#[must_use]
pub fn check_files_in_root(root: &Path, paths: &[PathBuf], with_routes: bool) -> CheckReport {
    let mut report = core::check_files_in_root(root, paths, with_routes);
    let overlay = crate::project_index::BufferOverlay::new();
    let groups = source_contract_groups(root, &overlay);
    let registry = with_routes.then(|| {
        crate::route::RouteRegistry::seeded()
            .with_pack_routes(&crate::route::default_pack_routes_path(root))
    });
    fold_source_contract(&mut report, registry.as_ref(), groups, &overlay);
    if with_routes {
        report.route_summary = Some(summarize_routes(&report.files));
    }
    report
}

/// Capture one request through the same report/route/OCEL path as the headless
/// checker. Rust source requires the project graph, so its live content is
/// admitted as an overlay before GGEN-SRC-004 is evaluated.
pub fn capture_request(
    root: &Path, file_path: &str, content: &str,
    attribution: &crate::intel::events::Attribution,
) {
    if !file_path.ends_with(".rs") {
        core::capture_request(root, file_path, content, attribution);
        return;
    }

    let path = {
        let candidate = PathBuf::from(file_path);
        if candidate.is_absolute() {
            candidate
        } else {
            root.join(candidate)
        }
    };
    let mut overlay = crate::project_index::BufferOverlay::new();
    overlay.insert(path.clone(), content.to_string());

    let mut report = CheckReport {
        files: Vec::new(),
        error_count: 0,
        warning_count: 0,
        route_summary: None,
    };
    let registry = crate::route::RouteRegistry::seeded()
        .with_pack_routes(&crate::route::default_pack_routes_path(root));
    let groups = source_contract_groups(root, &overlay)
        .into_iter()
        .filter(|(anchor, _)| paths_match(&anchor.to_string_lossy(), &path.to_string_lossy()))
        .collect();
    fold_source_contract(&mut report, Some(&registry), groups, &overlay);

    if report.files.is_empty() {
        report.files.push(FileReport {
            path: path.to_string_lossy().to_string(),
            diagnostics: Vec::new(),
            routes: Vec::new(),
        });
    }
    report.capture_attributed(root, attribution);
}

fn source_contract_groups(
    root: &Path, overlay: &crate::project_index::BufferOverlay,
) -> Vec<(PathBuf, Vec<lsp_max_protocol::MaxDiagnostic>)> {
    let Ok(project) = crate::project_index::ProjectIndex::from_root_with_overlay(root, overlay)
    else {
        return Vec::new();
    };
    crate::source_contract::detect(&project, overlay)
}

fn fold_source_contract(
    report: &mut CheckReport, registry: Option<&crate::route::RouteRegistry>,
    groups: Vec<(PathBuf, Vec<lsp_max_protocol::MaxDiagnostic>)>,
    overlay: &crate::project_index::BufferOverlay,
) {
    for (anchor_path, diagnostics) in groups {
        if diagnostics.is_empty() {
            continue;
        }

        let anchor = anchor_path.to_string_lossy().to_string();
        let content = overlay
            .get(&anchor_path)
            .cloned()
            .unwrap_or_else(|| std::fs::read_to_string(&anchor_path).unwrap_or_default());
        report.error_count += diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.lsp.severity == Some(DiagnosticSeverity::ERROR))
            .count();
        report.warning_count += diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.lsp.severity == Some(DiagnosticSeverity::WARNING))
            .count();

        let routes = registry.map_or_else(Vec::new, |registry| {
            diagnostics
                .iter()
                .filter_map(|diagnostic| {
                    crate::route::route_plan_for_diagnostic(registry, &diagnostic.lsp, &content)
                })
                .collect()
        });
        let lsp_diagnostics = diagnostics
            .into_iter()
            .map(|diagnostic| diagnostic.lsp)
            .collect::<Vec<_>>();

        if let Some(existing) = report
            .files
            .iter_mut()
            .find(|file| paths_match(&file.path, &anchor))
        {
            existing.diagnostics.extend(lsp_diagnostics);
            existing.routes.extend(routes);
        } else {
            report.files.push(FileReport {
                path: anchor,
                diagnostics: lsp_diagnostics,
                routes,
            });
        }
    }
}

fn paths_match(left: &str, right: &str) -> bool {
    if left == right {
        return true;
    }
    match (std::fs::canonicalize(left), std::fs::canonicalize(right)) {
        (Ok(left), Ok(right)) => left == right,
        _ => false,
    }
}

fn summarize_routes(files: &[FileReport]) -> RouteSummary {
    let mut routed = 0usize;
    let mut unrouted = 0usize;
    let mut counts: BTreeMap<String, usize> = BTreeMap::new();

    for file in files {
        for diagnostic in &file.diagnostics {
            let code = diagnostic.code.as_ref().map(|code| match code {
                lsp_max::lsp_types::NumberOrString::String(value) => value.clone(),
                lsp_max::lsp_types::NumberOrString::Number(value) => value.to_string(),
            });
            let covered = file.routes.iter().any(|route| {
                route.target.range == diagnostic.range
                    && code.as_ref().is_some_and(|code| code == &route.target.code)
            });
            if covered {
                routed += 1;
            } else {
                unrouted += 1;
            }
        }
        for route in &file.routes {
            *counts.entry(route.route_id.0.clone()).or_default() += 1;
        }
    }

    let mut top_routes = counts
        .into_iter()
        .map(|(name, count)| NamedCount { name, count })
        .collect::<Vec<_>>();
    top_routes.sort_by_key(|entry| std::cmp::Reverse(entry.count));

    RouteSummary {
        routed,
        unrouted,
        top_routes,
    }
}
