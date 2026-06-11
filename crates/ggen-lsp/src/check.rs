//! Headless law-surface gate.
//!
//! The same analyzers that power the interactive server run here against files on
//! disk, producing a serializable [`CheckReport`]. This is the bridge that lets
//! generated hooks (pre-edit/pre-commit) enforce *exactly* the law the editor
//! shows: a non-zero exit on any ERROR diagnostic refuses the motion before it
//! reaches the graph.

use std::path::{Path, PathBuf};

use lsp_max::lsp_types::{Diagnostic, DiagnosticSeverity};
use lsp_max_protocol::MaxDiagnostic;
use serde::Serialize;
use walkdir::WalkDir;

use crate::analyzers::build_analyzer;
use crate::state::FileType;

/// Directory names skipped when discovering law-surface files.
const SKIP_DIRS: &[&str] = &[
    ".git",
    "target",
    "node_modules",
    ".agent-admissibility",
    "dist",
];

/// Diagnostics for a single file.
#[derive(Debug, Clone, Serialize)]
pub struct FileReport {
    /// Path as supplied to the checker.
    pub path: String,
    /// LSP diagnostics produced by the matching analyzer.
    pub diagnostics: Vec<Diagnostic>,
    /// Repair routes for this file's diagnostics. Empty unless `--with-routes`.
    /// Each `RoutePlan` is byte-identical to the editor/MCP channels.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub routes: Vec<crate::route::RoutePlan>,
}

impl FileReport {
    /// Project this file's routes into the canonical [`crate::route::RouteEnvelope`]s
    /// — the same shape the LSP CodeAction `data`, MCP tool, and A2A bridge emit.
    #[must_use]
    pub fn envelopes(&self) -> Vec<crate::route::RouteEnvelope> {
        self.routes
            .iter()
            .map(|p| crate::route::RouteEnvelope::from_plan(p, &self.path))
            .collect()
    }
}

/// Count of diagnostics per failure family / route id (the 80/20 Pareto columns).
#[derive(Debug, Clone, Serialize)]
pub struct NamedCount {
    /// Family or route id.
    pub name: String,
    /// Occurrence count.
    pub count: usize,
}

/// 80/20 rollup of routes across a check run. Present only with `--with-routes`.
#[derive(Debug, Clone, Default, Serialize)]
pub struct RouteSummary {
    /// Diagnostics that had at least one route.
    pub routed: usize,
    /// Diagnostics with no route (uncovered failures — the CI gap; anti-fail-open).
    pub unrouted: usize,
    /// Counts per route id, descending — the Pareto picture.
    pub top_routes: Vec<NamedCount>,
}

/// Aggregate result of a headless check across one or more files.
#[derive(Debug, Clone, Serialize)]
pub struct CheckReport {
    /// Per-file diagnostics (only files recognized as law surfaces appear).
    pub files: Vec<FileReport>,
    /// Total ERROR-severity diagnostics across all files.
    pub error_count: usize,
    /// Total WARNING-severity diagnostics across all files.
    pub warning_count: usize,
    /// 80/20 route rollup. Present only when routes were computed.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub route_summary: Option<RouteSummary>,
}

impl CheckReport {
    /// True if any file produced an ERROR diagnostic — the hook refusal signal.
    #[must_use]
    pub fn has_errors(&self) -> bool {
        self.error_count > 0
    }

    /// Process exit code: 1 if any errors, else 0.
    #[must_use]
    pub fn exit_code(&self) -> i32 {
        i32::from(self.has_errors())
    }

    /// Capture this gate run as agent-edit OCEL events under `root` (best-effort),
    /// attributed to the headless gate. See [`CheckReport::capture_attributed`].
    pub fn capture(&self, root: &Path) {
        self.capture_attributed(root, &crate::intel::events::Attribution::headless());
    }

    /// Capture attributed to a named `agent_id` over the headless transport.
    pub fn capture_as(&self, root: &Path, agent_id: &str) {
        self.capture_attributed(
            root,
            &crate::intel::events::Attribution::for_agent(agent_id),
        );
    }

    /// Capture this gate run as agent-edit OCEL events under `root` with full
    /// [`Attribution`](crate::intel::events::Attribution) (agent + transport +
    /// session). Emits the per-diagnostic chain (`DiagnosticRaised` → optional
    /// `RouteSelected`/`RepairSuggested` → `GatePassed`/`GateFailed` →
    /// `ReceiptEmitted`/`RefusalEmitted`), feeding `ggen lsp mine`. Episode
    /// identity (file|code|run_id) keeps concurrent agents/transports separable;
    /// the attribution tags make "which agent, over which transport, in which
    /// session" explicit (and route success sliceable by transport). Errors are
    /// swallowed — capture must never break the gate.
    pub fn capture_attributed(&self, root: &Path, attribution: &crate::intel::events::Attribution) {
        use crate::intel::events::{
            attach_attribution, diagnostic_raised, gate_result, new_run_id, receipt_emitted,
            refusal_emitted, repair_suggested, route_selected,
        };
        use crate::intel::IntelLog;

        // One run id per check invocation → episodes don't collapse across runs.
        let run_id = new_run_id();
        let mut events = Vec::new();
        let mut seq: u64 = 0;
        for file in &self.files {
            for d in &file.diagnostics {
                let code = diag_code(d);
                let is_error = d.severity == Some(DiagnosticSeverity::ERROR);
                let sev = severity_str(d.severity);
                let span = span_str(d.range);

                seq += 1;
                events.push(diagnostic_raised(
                    &file.path, &code, sev, &span, &run_id, seq,
                ));

                // RouteSelected/RepairSuggested ONLY when a route was actually
                // selected for this diagnostic (with --with_routes). No event ⇒
                // no route_hit_rate inflation.
                if let Some(plan) = file
                    .routes
                    .iter()
                    .find(|r| r.target.range == d.range && r.target.code == code)
                {
                    let source = route_source(&plan.provenance);
                    seq += 1;
                    events.push(route_selected(
                        &file.path,
                        &code,
                        &plan.route_id.0,
                        source,
                        &run_id,
                        seq,
                    ));
                    seq += 1;
                    events.push(repair_suggested(
                        &file.path,
                        &code,
                        &plan.route_id.0,
                        &run_id,
                        seq,
                    ));
                }

                seq += 1;
                events.push(gate_result(
                    &file.path,
                    &code,
                    !is_error,
                    file.diagnostics.len(),
                    &run_id,
                    seq,
                ));

                // Closed episode → receipt; refused episode → refusal.
                seq += 1;
                if is_error {
                    events.push(refusal_emitted(
                        &file.path,
                        &code,
                        file.diagnostics.len(),
                        &run_id,
                        seq,
                    ));
                } else {
                    let receipt_id = receipt_id_for(&file.path, &code, &run_id);
                    events.push(receipt_emitted(
                        &file.path,
                        &code,
                        &receipt_id,
                        &run_id,
                        seq,
                    ));
                }
            }
            // Files with no diagnostics still record a clean gate pass.
            if file.diagnostics.is_empty() {
                seq += 1;
                events.push(gate_result(&file.path, "clean", true, 0, &run_id, seq));
            }
        }
        // Attribute every event (agent + transport + session). Episode identity
        // already keeps concurrent agents/transports separable.
        attach_attribution(&mut events, attribution);
        let _ = IntelLog::at_root(root).append(&events);
    }
}

pub(crate) fn diag_code(d: &Diagnostic) -> String {
    match &d.code {
        Some(lsp_max::lsp_types::NumberOrString::String(s)) => s.clone(),
        Some(lsp_max::lsp_types::NumberOrString::Number(n)) => n.to_string(),
        None => "RDF".to_string(),
    }
}

pub(crate) fn severity_str(sev: Option<DiagnosticSeverity>) -> &'static str {
    match sev {
        Some(DiagnosticSeverity::ERROR) => "error",
        Some(DiagnosticSeverity::WARNING) => "warning",
        _ => "info",
    }
}

pub(crate) fn span_str(range: lsp_max::lsp_types::Range) -> String {
    format!(
        "{}:{}-{}:{}",
        range.start.line, range.start.character, range.end.line, range.end.character
    )
}

pub(crate) fn route_source(p: &crate::route::Provenance) -> &'static str {
    match p {
        crate::route::Provenance::Seeded => "seed",
        crate::route::Provenance::Mined { .. } => "mined",
    }
}

fn receipt_id_for(file: &str, code: &str, run_id: &str) -> String {
    blake3::hash(format!("{file}|{code}|{run_id}").as_bytes()).to_hex()[..16].to_string()
}

/// Capture a single in-memory file's diagnostics + route selections under `root`
/// with `attribution`.
///
/// This is the field-evidence gauge for non-editor (MCP/A2A) route
/// requests, which are otherwise pure projection and leave no trace. Reuses the
/// same capture machinery as the headless gate (full chain, attributed). No-op for
/// a non-law-surface file. Best-effort: never fails the request.
pub fn capture_request(
    root: &Path, file_path: &str, content: &str, attribution: &crate::intel::events::Attribution,
) {
    let Some(mut report) = check_content(file_path, content) else {
        return;
    };
    let registry = crate::route::RouteRegistry::seeded()
        .with_pack_routes(&crate::route::default_pack_routes_path(root));
    report.routes = report
        .diagnostics
        .iter()
        .filter_map(|d| crate::route::route_plan_for_diagnostic(&registry, d, content))
        .collect();
    let check = CheckReport {
        files: vec![report],
        error_count: 0,
        warning_count: 0,
        route_summary: None,
    };
    check.capture_attributed(root, attribution);
}

/// Check already-loaded content for a given path. Returns `None` if the path is
/// not a recognized ggen law surface.
#[must_use]
pub fn check_content(path: &str, content: &str) -> Option<FileReport> {
    let analyzer = build_analyzer(path, content)?;
    Some(FileReport {
        path: path.to_string(),
        diagnostics: analyzer.diagnostics().into_iter().map(|d| d.lsp).collect(),
        routes: Vec::new(),
    })
}

/// Check files on disk, aggregating diagnostics. Unreadable or non-law-surface
/// files are skipped. Returns the aggregate report (no routes).
#[must_use]
pub fn check_files(paths: &[PathBuf]) -> CheckReport {
    check_files_with_routes(paths, false)
}

/// Check files; when `with_routes`, attach a `RoutePlan` per diagnostic that has
/// one and compute the 80/20 `route_summary`.
///
/// Default mode is byte-identical to
/// the historical `check_files` output (routes/summary omitted via serde).
///
/// Promoted routes are loaded relative to the current working directory. Use
/// [`check_files_in_root`] to load them from an explicit project root (required
/// for hermetic tests and for running the gate outside the project root).
#[must_use]
pub fn check_files_with_routes(paths: &[PathBuf], with_routes: bool) -> CheckReport {
    check_files_in_root(std::path::Path::new("."), paths, with_routes)
}

/// Like [`check_files_with_routes`], but loads the promoted-route pack from
/// `root/.agent-admissibility/...` instead of the cwd.
///
/// Making the root explicit
/// keeps pack discovery from silently depending on the process working
/// directory — the headless gate, the editor, and MCP all resolve the SAME
/// routes for a given project root.
#[must_use]
pub fn check_files_in_root(root: &Path, paths: &[PathBuf], with_routes: bool) -> CheckReport {
    // Seeds + promoted routes (relative to `root` = project root), so the headless
    // gate sees the SAME routes as the editor and MCP channels.
    let registry = with_routes.then(|| {
        crate::route::RouteRegistry::seeded()
            .with_pack_routes(&crate::route::default_pack_routes_path(root))
    });
    let mut files = Vec::new();
    let mut error_count = 0usize;
    let mut warning_count = 0usize;

    for path in paths {
        let content = match std::fs::read_to_string(path) {
            Ok(c) => c,
            Err(_) => continue,
        };
        let path_str = path.to_string_lossy().to_string();
        let Some(mut report) = check_content(&path_str, &content) else {
            continue;
        };
        for d in &report.diagnostics {
            match d.severity {
                Some(DiagnosticSeverity::ERROR) => error_count += 1,
                Some(DiagnosticSeverity::WARNING) => warning_count += 1,
                _ => {}
            }
        }
        if let Some(reg) = &registry {
            report.routes = report
                .diagnostics
                .iter()
                .filter_map(|d| crate::route::route_plan_for_diagnostic(reg, d, &content))
                .collect();
        }
        files.push(report);
    }

    // Cross-surface law: GGEN-TPL-001 (unbound projection). The single-file
    // analyzers above run each law surface in isolation; the headless Tera
    // analyzer is built with empty bindings and therefore emits E0024 (syntax)
    // ONLY — never GGEN-TPL-001, which needs the rule's SPARQL SELECT vars it
    // does not have. We supply that cross-surface context here by building the
    // project index from `root` and running the same pure detector the
    // interactive server uses. Read-only: the index already did its I/O and we
    // materialize nothing. Best-effort: a missing/unparseable `ggen.toml` (or a
    // `root` with no manifest, e.g. the cwd default) simply yields no extra
    // diagnostics and never disturbs the single-file reports above.
    error_count += fold_tpl_001(root, &mut files, registry.as_ref());

    // Cross-surface law: GGEN-HARNESS-001 (harness mismatch). The single-file
    // analyzers cannot see whether a declared Cargo.toml [[test]]/[[bench]] `path`
    // resolves to a real proof file on disk. We supply that cross-surface context
    // here by building the harness index from `root` and running the same pure
    // detector the interactive server uses. Read-only; best-effort (a missing
    // Cargo.toml yields no extra diagnostics).
    error_count += fold_harness_001(root, &mut files, registry.as_ref());

    // Cross-surface law: GGEN-OUT-001 (unbound output path). The dual of
    // GGEN-TPL-001 on the ggen.toml/SPARQL surfaces: the single-file analyzers
    // cannot see whether a rule's dynamic `output_file` Tera pattern references a
    // variable the SPARQL SELECT never binds. We supply that cross-surface context
    // here by building the project index from `root` and running the same pure
    // detector the interactive server uses. Read-only; best-effort (a missing
    // `ggen.toml` yields no extra diagnostics).
    error_count += fold_out_001(root, &mut files, registry.as_ref());

    // Cross-surface law: GGEN-RULE-001 (unbound rule file). The foundational
    // binding-integrity check GGEN-TPL-001/GGEN-OUT-001 presuppose — a rule whose
    // query/template {file=...} is missing on disk. The single-file analyzers
    // cannot see a dangling rule binding; we supply that context by building the
    // project index from `root` and surfacing its previously-silent
    // `RuleIndexEntry::issues` channel. Read-only; best-effort (a missing
    // `ggen.toml` yields no extra diagnostics). Appended LAST so the
    // TPL→HARNESS→OUT fold order is unchanged.
    error_count += fold_rule_001(root, &mut files, registry.as_ref());

    // Cross-surface law: GGEN-YIELD-001 (output_file escapes project root).
    // A rule whose output_file pattern resolves outside the project root is a
    // path-injection risk. This cannot be caught by single-file analyzers.
    error_count += fold_yield_001(root, &mut files, registry.as_ref());

    // Cross-surface laws: GGEN-YIELD-003/004/005 (orphaned output, competing
    // authority, remote fetch). All are ERROR severity.
    error_count += fold_yield_003(root, &mut files, registry.as_ref());
    error_count += fold_yield_004(root, &mut files, registry.as_ref());
    error_count += fold_yield_005(root, &mut files, registry.as_ref());

    // Cross-surface advisory: GGEN-QUERY-002 (SELECT * disables TPL-001/OUT-001).
    // WARNING only — does not increment error_count.
    let warn_added = fold_query_002(root, &mut files, registry.as_ref());
    warning_count += warn_added;

    // Cross-surface advisory: GGEN-PACK-001 (pack source disables author-time checks).
    // WARNING only — does not increment error_count.
    let pack_warn_added = fold_pack_001(root, &mut files, registry.as_ref());
    warning_count += pack_warn_added;

    let route_summary = with_routes.then(|| summarize_routes(&files));

    CheckReport {
        files,
        error_count,
        warning_count,
        route_summary,
    }
}

/// Fold GGEN-TPL-001 (unbound-projection) diagnostics from the project index at
/// `root` into `files`, returning the number of newly added ERROR diagnostics
/// (so the caller can keep `error_count` exact).
///
/// For each `(template_path, diags)` the detector returns, the diagnostics are
/// appended to the [`FileReport`] whose path matches that template (added as a
/// new report if the template is not already among `files`). When `registry` is
/// `Some` (i.e. `--with-routes`), each appended diagnostic also gets its
/// `RoutePlan` resolved through the SAME route engine as every other channel,
/// using the template's own content as the route's edit-site context.
///
/// A missing template (`template_content: None`) is skipped by `detect_tpl_001`
/// itself — it stays a [`RuleIndexEntry::issues`] index problem, never
/// GGEN-TPL-001. This function therefore never reclassifies a missing source.
fn fold_tpl_001(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(files, registry, crate::analyzers::detect_tpl_001(&project))
}

/// Fold one cross-surface law's `(anchor_path, diags)` groups into `files`,
/// returning the number of newly added ERROR diagnostics. The single shared body
/// behind [`fold_tpl_001`]/[`fold_harness_001`]/[`fold_out_001`]: skip empty
/// groups, count errors, resolve routes (when `registry` is `Some`) against the
/// ANCHOR's own content read from disk, and append-or-create the matching
/// [`FileReport`]. Index heterogeneity (ProjectIndex vs HarnessIndex) is resolved
/// by the caller, which passes the detector output here.
///
/// The route edit site lives in whichever file the detector named as the anchor
/// (for TPL that is the template path; for HARNESS/OUT the manifest), so route
/// plans are built against that anchor's content — byte-identical to the former
/// per-species folds, which each read their own anchor before this consolidation.
fn fold_species(
    files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
    groups: Vec<(PathBuf, Vec<MaxDiagnostic>)>,
) -> usize {
    let mut added_errors = 0usize;
    for (anchor_path, max_diags) in groups {
        if max_diags.is_empty() {
            continue;
        }
        let anchor_str = anchor_path.to_string_lossy().to_string();
        let anchor_content = std::fs::read_to_string(&anchor_path).unwrap_or_default();

        added_errors += max_diags
            .iter()
            .filter(|d| d.lsp.severity == Some(DiagnosticSeverity::ERROR))
            .count();

        let routes: Vec<crate::route::RoutePlan> = match registry {
            Some(reg) => max_diags
                .iter()
                .filter_map(|d| crate::route::route_plan_for_diagnostic(reg, &d.lsp, &anchor_content))
                .collect(),
            None => Vec::new(),
        };

        let diags: Vec<Diagnostic> = max_diags.into_iter().map(|d| d.lsp).collect();

        if let Some(existing) = files.iter_mut().find(|f| paths_match(&f.path, &anchor_str)) {
            existing.diagnostics.extend(diags);
            existing.routes.extend(routes);
        } else {
            files.push(FileReport {
                path: anchor_str,
                diagnostics: diags,
                routes,
            });
        }
    }
    added_errors
}

/// Fold GGEN-HARNESS-001 (harness-mismatch) diagnostics from the harness index at
/// `root` into `files`, returning the number of newly added ERROR diagnostics (so
/// the caller can keep `error_count` exact).
///
/// For each `(manifest_path, diags)` the detector returns, the diagnostics are
/// appended to the [`FileReport`] whose path matches that manifest (`Cargo.toml`),
/// added as a new report if the manifest is not already among `files`. When
/// `registry` is `Some` (i.e. `--with-routes`), each appended diagnostic also gets
/// its `RoutePlan` resolved through the SAME route engine as every other channel,
/// using the manifest's own content as the route's edit-site context.
///
/// Mirrors [`fold_tpl_001`]. Best-effort: a missing/unparseable `Cargo.toml` (or a
/// `root` with no manifest) yields no extra diagnostics and never disturbs the
/// single-file reports.
fn fold_harness_001(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(index) = crate::harness_index::HarnessIndex::from_root(root) else {
        return 0;
    };
    fold_species(
        files,
        registry,
        crate::analyzers::detect_harness_001(&index),
    )
}

/// Fold GGEN-OUT-001 (unbound-output-path) diagnostics from the project index at
/// `root` into `files`, returning the number of newly added ERROR diagnostics (so
/// the caller can keep `error_count` exact).
///
/// The dual of [`fold_tpl_001`]: for each `(manifest_path, diags)` the detector
/// returns, the diagnostics are appended to the [`FileReport`] whose path matches
/// that `ggen.toml` (added as a new report if the manifest is not already among
/// `files`). When `registry` is `Some` (i.e. `--with-routes`), each appended
/// diagnostic also gets its `RoutePlan` resolved through the SAME route engine as
/// every other channel, using the manifest's own content as the route's edit-site
/// context.
///
/// `detect_out_001` itself skips rules with empty `selected_vars` (`SELECT *` /
/// missing query) and static `output_file` paths, so this function never
/// synthesizes a false positive. Best-effort: a missing/unparseable `ggen.toml`
/// (or a `root` with no manifest) yields no extra diagnostics.
fn fold_out_001(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(files, registry, crate::analyzers::detect_out_001(&project))
}

/// Fold GGEN-RULE-001 (unbound-rule-file) diagnostics from the project index at
/// `root` into `files`, returning the number of newly added ERROR diagnostics (so
/// the caller can keep `error_count` exact).
///
/// For each `(manifest_path, diags)` the detector returns, the diagnostics are
/// appended to the [`FileReport`] whose path matches that `ggen.toml` (added as a
/// new report if the manifest is not already among `files`). When `registry` is
/// `Some` (i.e. `--with-routes`), each appended diagnostic also gets its
/// `RoutePlan` resolved through the SAME route engine as every other channel.
///
/// The FOUNDATIONAL binding-integrity check GGEN-TPL-001/GGEN-OUT-001 presuppose:
/// it surfaces a rule's missing query/template file — the previously-silent
/// [`crate::rule_index::RuleIndexEntry::issues`] — as a lawful diagnostic.
/// Best-effort: a missing/unparseable `ggen.toml` yields no extra diagnostics.
fn fold_rule_001(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(files, registry, crate::analyzers::detect_rule_001(&project))
}

/// Fold GGEN-YIELD-001 (output-path-escape) diagnostics from the project index
/// at `root` into `files`, returning the number of newly added ERROR diagnostics.
///
/// For each `(manifest_path, diags)` the detector returns, the diagnostics are
/// appended to the matching [`FileReport`] (or a new one is created). Best-effort:
/// a missing/unparseable `ggen.toml` yields no extra diagnostics.
fn fold_yield_001(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(files, registry, crate::analyzers::detect_yield_001(&project))
}

fn fold_yield_003(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(files, registry, crate::analyzers::detect_yield_003(&project))
}

fn fold_yield_004(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(files, registry, crate::analyzers::detect_yield_004(&project))
}

fn fold_yield_005(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(files, registry, crate::analyzers::detect_yield_005(&project))
}

/// Fold GGEN-QUERY-002 (SELECT * blindspot) advisories from the project index
/// at `root` into `files`, returning the number of newly added WARNING diagnostics
/// (does NOT count toward `error_count` — the caller adds to `warning_count`).
fn fold_query_002(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    // fold_species counts ERRORs; QUERY-002 is WARNING so we count manually.
    let groups = crate::analyzers::detect_query_002(&project);
    let warn_count: usize = groups
        .iter()
        .flat_map(|(_, diags)| diags)
        .filter(|d| {
            d.lsp.severity
                == Some(lsp_max::lsp_types::DiagnosticSeverity::WARNING)
        })
        .count();
    // Still call fold_species so the diagnostics appear in the file reports.
    fold_species(files, registry, groups);
    warn_count
}

/// Compare two filesystem path strings for "same file" identity. Tries exact
/// string equality first (the common case), then falls back to canonicalized
/// comparison so a relative path supplied to the gate matches the absolute
/// `template_path` the index resolved (e.g. `templates/row.tera` vs
/// Fold GGEN-PACK-001 (pack-source disables author-time checks) advisories from
/// the project index at `root` into `files`, returning the number of newly added
/// WARNING diagnostics (does NOT increment `error_count`).
fn fold_pack_001(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    let groups = crate::analyzers::detect_pack_001(&project);
    let warn_count: usize = groups
        .iter()
        .flat_map(|(_, diags)| diags)
        .filter(|d| {
            d.lsp.severity == Some(lsp_max::lsp_types::DiagnosticSeverity::WARNING)
        })
        .count();
    fold_species(files, registry, groups);
    warn_count
}

/// `/abs/proj/templates/row.tera`). Canonicalization is best-effort: if either
/// path cannot be canonicalized, only the exact-string result stands.
fn paths_match(a: &str, b: &str) -> bool {
    if a == b {
        return true;
    }
    match (std::fs::canonicalize(a), std::fs::canonicalize(b)) {
        (Ok(ca), Ok(cb)) => ca == cb,
        _ => false,
    }
}

fn summarize_routes(files: &[FileReport]) -> RouteSummary {
    use std::collections::BTreeMap;
    let mut routed = 0usize;
    let mut unrouted = 0usize;
    let mut counts: BTreeMap<String, usize> = BTreeMap::new();
    for f in files {
        // A diagnostic is "routed" if a plan targets it (matched by code+range).
        for d in &f.diagnostics {
            let has = f.routes.iter().any(|r| {
                r.target.range == d.range
                    && d.code
                        .as_ref()
                        .map_or(r.target.code.is_empty(), |c| match c {
                            lsp_max::lsp_types::NumberOrString::String(s) => s == &r.target.code,
                            lsp_max::lsp_types::NumberOrString::Number(n) => {
                                n.to_string() == r.target.code
                            }
                        })
            });
            if has {
                routed += 1;
            } else {
                unrouted += 1;
            }
        }
        for r in &f.routes {
            *counts.entry(r.route_id.0.clone()).or_insert(0) += 1;
        }
    }
    let mut top_routes: Vec<NamedCount> = counts
        .into_iter()
        .map(|(name, count)| NamedCount { name, count })
        .collect();
    top_routes.sort_by_key(|n| std::cmp::Reverse(n.count));
    RouteSummary {
        routed,
        unrouted,
        top_routes,
    }
}

/// Recursively discover every ggen law-surface file under `root`
/// (`.ttl`, `.nt`, `.nq`, `.rq`, `.sparql`, `.tera`, `ggen.toml`), skipping
/// build/VCS directories.
///
/// Results are sorted for deterministic output.
///
/// Note: dotdirs are NOT skipped wholesale — ggen specs live under `.specify/`,
/// which is the source-of-truth law surface; only `SKIP_DIRS` are pruned.
#[must_use]
pub fn discover_law_surfaces(root: &Path) -> Vec<PathBuf> {
    let mut found: Vec<PathBuf> = WalkDir::new(root)
        .into_iter()
        .filter_entry(|entry| {
            // Prune build/VCS directories (and their subtrees); keep everything else.
            !(entry.file_type().is_dir()
                && entry
                    .file_name()
                    .to_str()
                    .is_some_and(|name| SKIP_DIRS.contains(&name)))
        })
        .filter_map(std::result::Result::ok)
        .filter(|entry| entry.file_type().is_file())
        .map(walkdir::DirEntry::into_path)
        .filter(|path| FileType::from_path(&path.to_string_lossy()) != FileType::Unknown)
        .collect();
    found.sort();
    found
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bad_sparql_content_reports_error() {
        let report = check_content("query.rq", "SELECT ?s WHERE { VALUES ?s { <http://x> } }")
            .expect("rq is a law surface");
        assert!(report
            .diagnostics
            .iter()
            .any(|d| d.severity == Some(DiagnosticSeverity::ERROR)));
    }

    #[test]
    fn unknown_extension_is_not_a_law_surface() {
        assert!(check_content("notes.md", "# hello").is_none());
    }

    #[test]
    fn discover_finds_law_surfaces_recursively_and_skips_build_dirs() {
        use std::fs;
        let dir = tempfile::TempDir::new().expect("tempdir");
        let root = dir.path();
        fs::create_dir_all(root.join(".specify/specs")).expect("mkdir");
        fs::create_dir_all(root.join("target/junk")).expect("mkdir");
        fs::write(root.join(".specify/specs/feature.ttl"), "@prefix ex: <x> .").expect("w");
        fs::write(root.join("ggen.toml"), "[project]\nname=\"x\"").expect("w");
        fs::write(root.join("target/junk/ignored.ttl"), "@prefix ex: <x> .").expect("w");
        fs::write(root.join("readme.md"), "# not a law surface").expect("w");

        let found = discover_law_surfaces(root);
        let names: Vec<String> = found
            .iter()
            .map(|p| p.to_string_lossy().to_string())
            .collect();

        assert!(
            names.iter().any(|n| n.ends_with("feature.ttl")),
            "must find specs under .specify"
        );
        assert!(names.iter().any(|n| n.ends_with("ggen.toml")));
        assert!(
            !names.iter().any(|n| n.contains("target")),
            "must skip target/"
        );
        assert!(!names.iter().any(|n| n.ends_with("readme.md")));
    }

    #[test]
    fn report_exit_code_reflects_errors() {
        let clean = check_content("ok.toml", "[project]\nname = \"x\"\n").expect("toml");
        let mut report = CheckReport {
            files: vec![clean],
            error_count: 0,
            warning_count: 0,
            route_summary: None,
        };
        assert_eq!(report.exit_code(), 0);
        report.error_count = 2;
        assert_eq!(report.exit_code(), 1);
        assert!(report.has_errors());
    }

    #[test]
    fn root_aware_gate_folds_tpl_001_and_fails() {
        // Arrange — a project whose rule SELECTs `?name` but whose template
        // consumes `title`: a genuine GGEN-TPL-001 unbound projection.
        use std::fs;
        let dir = tempfile::TempDir::new().expect("tempdir");
        let root = dir.path();
        fs::write(root.join("row.tera"), r#"{{ row["title"] }}"#).expect("write template");
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#;
        fs::write(root.join("ggen.toml"), manifest).expect("write manifest");

        // Act — run the headless gate over the template path under `root`.
        let report = check_files_in_root(root, &[root.join("row.tera")], false);

        // Assert — GGEN-TPL-001 ERROR present, counted, and gate fails.
        assert!(report.has_errors(), "GGEN-TPL-001 must make the gate fail");
        assert!(
            report.error_count >= 1,
            "error_count must include the TPL-001 error"
        );
        let tera_report = report
            .files
            .iter()
            .find(|f| f.path.ends_with("row.tera"))
            .expect("template report present");
        assert!(
            tera_report.diagnostics.iter().any(|d| matches!(
                &d.code,
                Some(lsp_max::lsp_types::NumberOrString::String(s)) if s == "GGEN-TPL-001"
            )),
            "the template report must carry a GGEN-TPL-001 diagnostic"
        );
    }

    #[test]
    fn repaired_template_has_no_tpl_001() {
        // Arrange — same project, but the template now consumes `name`, which
        // the SELECT produces: the unbound projection is repaired.
        use std::fs;
        let dir = tempfile::TempDir::new().expect("tempdir");
        let root = dir.path();
        fs::write(root.join("row.tera"), r#"{{ row["name"] }}"#).expect("write template");
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#;
        fs::write(root.join("ggen.toml"), manifest).expect("write manifest");

        // Act
        let report = check_files_in_root(root, &[root.join("row.tera")], false);

        // Assert — no GGEN-TPL-001 anywhere, gate passes.
        assert!(!report.has_errors(), "repaired template must pass the gate");
        assert!(
            !report
                .files
                .iter()
                .any(|f| f.diagnostics.iter().any(|d| matches!(
                    &d.code,
                    Some(lsp_max::lsp_types::NumberOrString::String(s)) if s == "GGEN-TPL-001"
                ))),
            "no GGEN-TPL-001 diagnostic must remain after repair"
        );
    }

    #[test]
    fn missing_template_is_not_reclassified_as_tpl_001() {
        // Arrange — rule references a template file that does not exist. This is
        // an index-level missing-source issue, NOT GGEN-TPL-001.
        use std::fs;
        let dir = tempfile::TempDir::new().expect("tempdir");
        let root = dir.path();
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "broken"
output_file = "broken.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "nope.tera" }
"#;
        fs::write(root.join("ggen.toml"), manifest).expect("write manifest");

        // Act — gate over the (nonexistent) template path; nothing readable, so
        // the single-file pass produces no report, and detect_tpl_001 skips the
        // missing template.
        let report = check_files_in_root(root, &[root.join("nope.tera")], false);

        // Assert — no GGEN-TPL-001 was synthesized for the missing template.
        assert!(
            !report
                .files
                .iter()
                .any(|f| f.diagnostics.iter().any(|d| matches!(
                    &d.code,
                    Some(lsp_max::lsp_types::NumberOrString::String(s)) if s == "GGEN-TPL-001"
                ))),
            "a missing template must stay an index issue, not GGEN-TPL-001"
        );
    }

    #[test]
    fn tpl_001_resolves_a_route_with_routes() {
        // Arrange — unbound projection, routes requested.
        use std::fs;
        let dir = tempfile::TempDir::new().expect("tempdir");
        let root = dir.path();
        fs::write(root.join("row.tera"), r#"{{ row["title"] }}"#).expect("write template");
        let manifest = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#;
        fs::write(root.join("ggen.toml"), manifest).expect("write manifest");

        // Act — with routes on.
        let report = check_files_in_root(root, &[root.join("row.tera")], true);

        // Assert — the GGEN-TPL-001 diagnostic resolved a route through the same
        // seeded route engine the other channels use.
        let tera_report = report
            .files
            .iter()
            .find(|f| f.path.ends_with("row.tera"))
            .expect("template report present");
        assert!(
            !tera_report.routes.is_empty(),
            "GGEN-TPL-001 must resolve a route with --with-routes"
        );
    }

    #[test]
    fn with_routes_attaches_plans_and_summary() {
        use std::fs;
        let dir = tempfile::TempDir::new().expect("tempdir");
        let cfg = dir.path().join("ggen.toml");
        // A genuine ggen config violation (invalid enum), not an LLM section.
        fs::write(&cfg, "[logging]\nlevel = \"verbose\"\n").expect("write");

        let report = check_files_with_routes(&[cfg], true);
        assert!(
            report.route_summary.is_some(),
            "summary present with routes"
        );
        let summary = report.route_summary.expect("summary");
        assert!(
            summary.routed >= 1,
            "the invalid enum value is routed (advisory)"
        );
        assert!(report.files[0]
            .routes
            .iter()
            .any(|r| !r.ordered_steps.is_empty()));
    }
}
