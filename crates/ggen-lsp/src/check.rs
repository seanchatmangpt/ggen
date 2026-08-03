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
        let path_str = path.to_string_lossy().to_string();
        let content = match std::fs::read_to_string(path) {
            Ok(c) => c,
            Err(e) => {
                // A law-surface file that can't be read must refuse the gate, not
                // pass through silently -- an unreadable ggen.toml/.ttl/.rq/.tera
                // is exactly the kind of file this gate exists to check, and a
                // permissions hiccup or encoding problem is not evidence the
                // content is lawful. Non-law-surface paths (e.g. a stray binary
                // the caller passed in) still skip silently, same as before.
                if crate::state::FileType::from_path(&path_str) != crate::state::FileType::Unknown {
                    error_count += 1;
                    files.push(FileReport {
                        path: path_str,
                        diagnostics: vec![crate::analyzers::diag::at(
                            0,
                            0,
                            0,
                            0,
                            DiagnosticSeverity::ERROR,
                            None,
                            format!("law-surface file could not be read: {e}"),
                        )],
                        routes: Vec::new(),
                    });
                }
                continue;
            }
        };
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

    // Fail-open guard (red-team finding F6): every fold_* below discards
    // ProjectIndex::from_root/HarnessIndex::from_root's Err via `let Ok(x) =
    // .. else { return 0 }` -- correct for "no manifest at this root" (the
    // ordinary case when checking a lone law-surface file outside a ggen
    // project), but silently wrong when the manifest file EXISTS and fails to
    // load (e.g. a `ggen.toml` with a `[[generation.rules]]` entry missing
    // the required `output_file` field: syntactically valid TOML, so
    // TomlAnalyzer's raw-syntax check reports nothing, yet ProjectIndex
    // cannot build a rule index from it at all). Surface that failure as its
    // own diagnostic BEFORE the folds run, so a manifest ggen sync itself
    // could not load no longer passes this gate with a clean report.
    error_count += fold_manifest_load_errors(root, &mut files);

    // Cross-surface law: GGEN-TPL-001 (unbound projection). The single-file
    // analyzers above run each law surface in isolation; the headless Tera
    // analyzer is built with empty bindings and therefore emits E0024 (syntax)
    // ONLY — never GGEN-TPL-001, which needs the rule's SPARQL SELECT vars it
    // does not have. We supply that cross-surface context here by building the
    // project index from `root` and running the same pure detector the
    // interactive server uses. Read-only: the index already did its I/O and we
    // materialize nothing. Best-effort: a `root` with no manifest at all (e.g.
    // the cwd default) simply yields no extra diagnostics here and never
    // disturbs the single-file reports above -- a manifest that EXISTS but
    // fails to load is now caught by `fold_manifest_load_errors` above, not
    // silently absorbed by this fold.
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

    // Cross-surface laws: GGEN-SRC-001/002/003 (source-caste path, DO NOT EDIT banners,
    // source-caste comments). All are ERROR severity.
    error_count += fold_src_001(root, &mut files, registry.as_ref());
    error_count += fold_src_002_003(root, &mut files, registry.as_ref());

    let route_summary = with_routes.then(|| summarize_routes(&files));

    CheckReport {
        files,
        error_count,
        warning_count,
        route_summary,
    }
}

/// Surface a `ggen.toml`/`Cargo.toml` that EXISTS but failed to load as its own
/// ERROR diagnostic (red-team finding F6, fail-open).
///
/// Every `fold_*` cross-surface check below discards `ProjectIndex::from_root`
/// / `HarnessIndex::from_root`'s `Err` via `let Ok(x) = .. else { return 0 }`.
/// That is correct for the ordinary "no manifest at this root" case (checking
/// a lone `.rq`/`.tera` file outside a ggen project) — both constructors
/// report that as `Ok` with an empty index, never `Err`. But every OTHER
/// error variant means the manifest file on disk EXISTS and could not be
/// loaded: `IndexError::ManifestParse` (syntactically valid TOML, real
/// `[[generation.rules]]` markers, but a rule fails `GgenManifest`
/// deserialization — e.g. a missing required `output_file` field),
/// `IndexError::AmbiguousSchema`/`UnsupportedSchema`, or
/// `HarnessIndexError::ManifestRead`/`ManifestParse`. That is exactly the kind
/// of law-surface failure this gate exists to catch, and before this function
/// existed it was silently discarded at every one of the ~11 fold_* call
/// sites at once (`TomlAnalyzer` only validates raw untyped TOML syntax, so it
/// never catches a typed-schema deserialization failure either).
///
/// `IndexError::ManifestNotFound` is `ProjectIndex`'s sole "no ggen.toml here"
/// variant and stays silent, unchanged. `HarnessIndex::from_root` has no
/// analogous "not found" error (a missing `Cargo.toml` is already `Ok` with an
/// empty index), so any `Err` from it is surfaced.
fn fold_manifest_load_errors(root: &Path, files: &mut Vec<FileReport>) -> usize {
    let mut added_errors = 0usize;

    if let Err(err) = crate::project_index::ProjectIndex::from_root(root) {
        if !matches!(
            err,
            crate::project_index::IndexError::ManifestNotFound { .. }
        ) {
            let manifest_path = root.join("ggen.toml").to_string_lossy().to_string();
            push_manifest_load_error(
                files,
                &manifest_path,
                format!("GGEN-MANIFEST-001 MANIFEST_LOAD_FAILURE: {err}"),
            );
            added_errors += 1;
        }
    }

    if let Err(err) = crate::harness_index::HarnessIndex::from_root(root) {
        let manifest_path = root.join("Cargo.toml").to_string_lossy().to_string();
        push_manifest_load_error(
            files,
            &manifest_path,
            format!("GGEN-HARNESS-001 MANIFEST_LOAD_FAILURE: {err}"),
        );
        added_errors += 1;
    }

    added_errors
}

/// Append a whole-file ERROR diagnostic to the [`FileReport`] matching `path`
/// (matched the same way [`fold_species`] merges anchors), creating a new
/// report if none matches yet. Mirrors the unreadable-law-surface-file path in
/// [`check_files_in_root`] above.
fn push_manifest_load_error(files: &mut Vec<FileReport>, path: &str, message: String) {
    let diag = crate::analyzers::diag::whole_line(0, DiagnosticSeverity::ERROR, None, message);
    if let Some(existing) = files.iter_mut().find(|f| paths_match(&f.path, path)) {
        existing.diagnostics.push(diag);
    } else {
        files.push(FileReport {
            path: path.to_string(),
            diagnostics: vec![diag],
            routes: Vec::new(),
        });
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
                .filter_map(|d| {
                    crate::route::route_plan_for_diagnostic(reg, &d.lsp, &anchor_content)
                })
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
/// Mirrors [`fold_tpl_001`]. Best-effort: a `root` with no `Cargo.toml` at all
/// yields no extra diagnostics and never disturbs the single-file reports -- a
/// `Cargo.toml` that EXISTS but fails to read/parse is caught by
/// `fold_manifest_load_errors`, not silently absorbed here.
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
/// synthesizes a false positive. Best-effort: a `root` with no manifest at all
/// yields no extra diagnostics here (an existing-but-unparseable `ggen.toml`
/// is caught by `fold_manifest_load_errors`, not silently absorbed here).
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
/// Best-effort: a `root` with no manifest at all yields no extra diagnostics
/// here (see `fold_manifest_load_errors` for the existing-but-broken case).
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
/// a `root` with no manifest at all yields no extra diagnostics here (see
/// `fold_manifest_load_errors` for the existing-but-broken case).
fn fold_yield_001(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(
        files,
        registry,
        crate::analyzers::detect_yield_001(&project),
    )
}

fn fold_yield_003(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(
        files,
        registry,
        crate::analyzers::detect_yield_003(&project),
    )
}

fn fold_yield_004(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(
        files,
        registry,
        crate::analyzers::detect_yield_004(&project),
    )
}

fn fold_yield_005(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(
        files,
        registry,
        crate::analyzers::detect_yield_005(&project),
    )
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
        .filter(|d| d.lsp.severity == Some(lsp_max::lsp_types::DiagnosticSeverity::WARNING))
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
        .filter(|d| d.lsp.severity == Some(lsp_max::lsp_types::DiagnosticSeverity::WARNING))
        .count();
    fold_species(files, registry, groups);
    warn_count
}

fn fold_src_001(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    fold_species(files, registry, crate::analyzers::detect_src_001(&project))
}

fn fold_src_002_003(
    root: &Path, files: &mut Vec<FileReport>, registry: Option<&crate::route::RouteRegistry>,
) -> usize {
    let Ok(project) = crate::project_index::ProjectIndex::from_root(root) else {
        return 0;
    };
    // Collect unique parent directories of each rule's output file.
    let mut dirs: Vec<std::path::PathBuf> = project
        .rule_entries
        .iter()
        .map(|e| {
            let p = std::path::Path::new(&e.output_file);
            if p.is_absolute() {
                p.parent()
                    .map(|x| x.to_path_buf())
                    .unwrap_or_else(|| root.to_path_buf())
            } else {
                root.join(p)
                    .parent()
                    .map(|x| x.to_path_buf())
                    .unwrap_or_else(|| root.to_path_buf())
            }
        })
        .collect();
    // Always include project root itself.
    dirs.push(root.to_path_buf());
    dirs.sort();
    dirs.dedup();
    let mut groups = Vec::new();
    for dir in &dirs {
        groups.extend(crate::analyzers::detect_src_002_003_in_dir(dir));
    }
    fold_species(files, registry, groups)
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

    #[cfg(unix)]
    #[test]
    fn unreadable_law_surface_file_refuses_instead_of_passing_silently() {
        use std::fs;
        use std::os::unix::fs::PermissionsExt;

        let dir = tempfile::TempDir::new().expect("tempdir");
        let ttl = dir.path().join("locked.ttl");
        fs::write(&ttl, "@prefix ex: <http://example.org/> .").expect("write");
        fs::set_permissions(&ttl, fs::Permissions::from_mode(0o000)).expect("chmod 000");

        let report = check_files_in_root(dir.path(), std::slice::from_ref(&ttl), false);

        // Restore permissions so the TempDir can clean itself up on drop.
        fs::set_permissions(&ttl, fs::Permissions::from_mode(0o644)).expect("restore perms");

        assert_eq!(
            report.exit_code(),
            1,
            "an unreadable .ttl must refuse the gate, not silently pass"
        );
        assert!(report.has_errors());
        assert_eq!(
            report.files.len(),
            1,
            "the unreadable file is still reported"
        );
        assert!(report.files[0]
            .diagnostics
            .iter()
            .any(|d| d.severity == Some(DiagnosticSeverity::ERROR)
                && d.message.contains("could not be read")));
    }

    #[test]
    fn unreadable_non_law_surface_path_still_skips_silently() {
        // A missing/unreadable path with an extension that isn't a law surface
        // (e.g. a stray .md the caller passed in) must not be treated as a gate
        // failure -- this preserves the pre-existing "not a law surface" skip.
        let report = check_files_in_root(
            std::path::Path::new("."),
            &[PathBuf::from("/nonexistent/path/notes.md")],
            false,
        );
        assert_eq!(report.exit_code(), 0);
        assert!(report.files.is_empty());
    }

    #[test]
    fn manifest_that_fails_to_deserialize_refuses_the_gate() {
        // Red-team finding F6 (fail-open): a `ggen.toml` that is syntactically
        // valid TOML and classifies as the declarative-rules schema (it has a
        // real `[[generation.rules]]` block) but fails `GgenManifest`
        // deserialization -- here, a rule missing the required `output_file`
        // field -- must refuse the headless gate, not pass through with a
        // clean report. Before the fix, every fold_* cross-surface check
        // discarded `ProjectIndex::from_root`'s `Err(ManifestParse{..})` via
        // `let Ok(project) = .. else { return 0 }`, and nothing else in
        // check.rs ever surfaced that error as a diagnostic.
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
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { inline = "{{ name }}" }
"#;
        fs::write(root.join("ggen.toml"), manifest).expect("write manifest");

        let report = check_files_in_root(root, &[root.join("ggen.toml")], false);

        assert!(
            report.has_errors(),
            "a ggen.toml that fails to deserialize into GgenManifest must fail the gate, not pass silently (got exit_code={}, error_count={})",
            report.exit_code(),
            report.error_count
        );
        assert!(
            report.files.iter().any(|f| f.path.ends_with("ggen.toml")
                && f.diagnostics
                    .iter()
                    .any(|d| d.severity == Some(DiagnosticSeverity::ERROR)
                        && d.message.contains("output_file"))),
            "the ggen.toml report must carry an ERROR diagnostic naming the real parse failure"
        );
    }

    #[test]
    fn manifest_not_found_stays_silent() {
        // The ordinary "no ggen.toml at this root" case (e.g. checking a lone
        // law-surface file outside any ggen project) must remain a silent
        // no-op, unchanged by the F6 fix -- only a manifest that EXISTS but
        // fails to load should refuse the gate. No files are passed in, so
        // this isolates fold_manifest_load_errors itself from any unrelated
        // single-file analyzer diagnostic (e.g. E0013 missing-ORDER-BY).
        let dir = tempfile::TempDir::new().expect("tempdir");
        let root = dir.path();

        let report = check_files_in_root(root, &[], false);

        assert!(
            !report.has_errors(),
            "a project root with no ggen.toml/Cargo.toml at all must not fail the gate \
             (error_count={})",
            report.error_count
        );
        assert!(
            report.files.is_empty(),
            "no diagnostics should be synthesized when there is no manifest to fail on"
        );
    }
}
