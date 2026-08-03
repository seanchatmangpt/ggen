//! Compatibility-preserving extension of the established headless checker.
//!
//! The existing checker remains the authority for all prior diagnostic species.
//! This wrapper adds Rust source surfaces and GGEN-SRC-004 without duplicating or
//! weakening the proven paths in `check.rs`.

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use lsp_max::lsp_types::DiagnosticSeverity;

#[path = "check.rs"]
mod established;

pub use established::{CheckReport, FileReport, NamedCount, RouteSummary};
pub(crate) use established::{diag_code, route_source, severity_str, span_str};

/// Discover established law surfaces plus statically-owned generated Rust source.
/// Dynamic output identities remain outside the admitted set until construction.
#[must_use]
pub fn discover_law_surfaces(root: &Path) -> Vec<PathBuf> {
    let mut found = established::discover_law_surfaces(root);
    if let Ok(project) = crate::project_index::ProjectIndex::from_root(root) {
        for entry in project.rule_entries {
            let output = &entry.output_file;
            if output.contains("{{") || output.contains("{%") {
                continue;
            }
            let path = Path::new(output);
            let path = if path.is_absolute() {
                path.to_path_buf()
            } else {
                root.join(path)
            };
            if path.extension().and_then(|value| value.to_str()) == Some("rs") && path.is_file() {
                found.push(path);
            }
        }
    }
    found.sort();
    found.dedup();
    found
}

/// Capture an in-memory request through the same receipt path as the headless gate.
pub fn capture_request(
    root: &Path, file_path: &str, content: &str, attribution: &crate::intel::events::Attribution,
) {
    let Some(mut file) = check_content(file_path, content) else {
        return;
    };
    attach_routes_for_content(root, &mut file, content);
    let mut report = CheckReport {
        files: vec![file],
        error_count: 0,
        warning_count: 0,
        route_summary: None,
    };
    recount(&mut report, true);
    report.capture_attributed(root, attribution);
}

/// Check already-loaded content, including first-class Rust source surfaces.
#[must_use]
pub fn check_content(path: &str, content: &str) -> Option<FileReport> {
    if path.ends_with(".rs") {
        return Some(FileReport {
            path: path.to_string(),
            diagnostics: crate::analyzers::source_law_analyzer::source_law_diagnostics(content)
                .into_iter()
                .map(|diagnostic| diagnostic.lsp)
                .collect(),
            routes: Vec::new(),
        });
    }
    established::check_content(path, content)
}

/// Check files on disk, aggregating diagnostics.
#[must_use]
pub fn check_files(paths: &[PathBuf]) -> CheckReport {
    check_files_with_routes(paths, false)
}

/// Check files and optionally attach repair routes.
#[must_use]
pub fn check_files_with_routes(paths: &[PathBuf], with_routes: bool) -> CheckReport {
    check_files_in_root(Path::new("."), paths, with_routes)
}

/// Run the established gate, then close the generated Rust module-authority edge.
#[must_use]
pub fn check_files_in_root(root: &Path, paths: &[PathBuf], with_routes: bool) -> CheckReport {
    let mut report = established::check_files_in_root(root, paths, with_routes);

    for path in paths.iter().filter(|path| {
        path.extension().and_then(|value| value.to_str()) == Some("rs")
    }) {
        let Ok(content) = std::fs::read_to_string(path) else {
            continue;
        };
        let mut file = FileReport {
            path: path.to_string_lossy().to_string(),
            diagnostics: crate::analyzers::source_law_analyzer::source_law_diagnostics(&content)
                .into_iter()
                .map(|diagnostic| diagnostic.lsp)
                .collect(),
            routes: Vec::new(),
        };
        if with_routes {
            attach_routes(root, &mut file);
        }
        merge_file(&mut report.files, file);
    }

    if let Ok(project) = crate::project_index::ProjectIndex::from_root(root) {
        let overlay = crate::project_index::BufferOverlay::new();
        for (path, diagnostics) in
            crate::analyzers::source_law_analyzer::detect_src_004(&project, &overlay)
        {
            let mut file = FileReport {
                path: path.to_string_lossy().to_string(),
                diagnostics: diagnostics
                    .into_iter()
                    .map(|diagnostic| diagnostic.lsp)
                    .collect(),
                routes: Vec::new(),
            };
            if with_routes {
                attach_routes(root, &mut file);
            }
            merge_file(&mut report.files, file);
        }
    }

    recount(&mut report, with_routes);
    report
}

fn attach_routes(root: &Path, file: &mut FileReport) {
    let content = std::fs::read_to_string(&file.path).unwrap_or_default();
    attach_routes_for_content(root, file, &content);
}

fn attach_routes_for_content(root: &Path, file: &mut FileReport, content: &str) {
    let registry = crate::route::RouteRegistry::seeded()
        .with_pack_routes(&crate::route::default_pack_routes_path(root));
    file.routes.extend(file.diagnostics.iter().filter_map(|diagnostic| {
        crate::route::route_plan_for_diagnostic(&registry, diagnostic, content)
    }));
}

fn merge_file(files: &mut Vec<FileReport>, incoming: FileReport) {
    if let Some(existing) = files
        .iter_mut()
        .find(|file| paths_match(&file.path, &incoming.path))
    {
        for diagnostic in incoming.diagnostics {
            let duplicate = existing.diagnostics.iter().any(|current| {
                current.code == diagnostic.code
                    && current.range == diagnostic.range
                    && current.message == diagnostic.message
            });
            if !duplicate {
                existing.diagnostics.push(diagnostic);
            }
        }
        for route in incoming.routes {
            let duplicate = existing.routes.iter().any(|current| {
                current.route_id == route.route_id
                    && current.target.code == route.target.code
                    && current.target.range == route.target.range
            });
            if !duplicate {
                existing.routes.push(route);
            }
        }
    } else {
        files.push(incoming);
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

fn recount(report: &mut CheckReport, with_routes: bool) {
    report.error_count = report
        .files
        .iter()
        .flat_map(|file| &file.diagnostics)
        .filter(|diagnostic| diagnostic.severity == Some(DiagnosticSeverity::ERROR))
        .count();
    report.warning_count = report
        .files
        .iter()
        .flat_map(|file| &file.diagnostics)
        .filter(|diagnostic| diagnostic.severity == Some(DiagnosticSeverity::WARNING))
        .count();
    report.route_summary = with_routes.then(|| summarize_routes(&report.files));
}

fn summarize_routes(files: &[FileReport]) -> RouteSummary {
    let mut routed = 0usize;
    let mut unrouted = 0usize;
    let mut counts: BTreeMap<String, usize> = BTreeMap::new();
    for file in files {
        for diagnostic in &file.diagnostics {
            let matched = file.routes.iter().any(|route| {
                route.target.range == diagnostic.range
                    && diagnostic.code.as_ref().is_some_and(|code| match code {
                        lsp_max::lsp_types::NumberOrString::String(value) => {
                            value == &route.target.code
                        }
                        lsp_max::lsp_types::NumberOrString::Number(value) => {
                            value.to_string() == route.target.code
                        }
                    })
            });
            if matched {
                routed += 1;
            } else {
                unrouted += 1;
            }
        }
        for route in &file.routes {
            *counts.entry(route.route_id.0.clone()).or_insert(0) += 1;
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
