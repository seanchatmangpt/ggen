//! LSP Commands — the ggen Language Server and its headless admission gate.
//!
//! - `ggen lsp start`     — interactive Language Server over stdio (for editors).
//! - `ggen lsp check`     — headless gate: validate law-surface files (all of them
//!   by default, or specific `--files`), emit JSON diagnostics, exit non-zero on
//!   any ERROR. This is what generated agent hooks shell out to before an edit/commit.
//! - `ggen lsp emit_pack` — generate the Agent Admissibility Pack (config + hooks + policies).

use std::path::{Path, PathBuf};

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;

#[derive(Serialize)]
pub struct StartOutput {
    pub status: String,
    pub transport: String,
}

#[derive(Serialize)]
pub struct CheckSummary {
    pub error_count: usize,
    pub warning_count: usize,
}

#[derive(Serialize)]
pub struct EmitPackOutput {
    pub out_dir: String,
    pub agents: Vec<String>,
    pub files_written: usize,
}

#[derive(Serialize)]
pub struct MineSummary {
    pub events_analyzed: usize,
    pub failure_edges: usize,
    pub total_edges: usize,
    pub report_path: String,
    pub promoted_count: usize,
    pub promoted_path: String,
}

#[derive(Serialize)]
pub struct InitOutput {
    pub files_written: Vec<String>,
    pub pack_dir: String,
}

/// Start the ggen Language Server for editors (stdio transport).
///
/// Provides live diagnostics, completion, hover, definition, rename, symbols,
/// and folding for .ttl/.nt/.nq, .rq/.sparql, .tera, and ggen.toml. Editors
/// launch this; you normally do not run it by hand.
///
///   ggen lsp start                     Run over stdio (default; the only transport).
#[verb]
fn start(transport: Option<String>) -> Result<StartOutput> {
    let transport_val = transport.unwrap_or_else(|| "stdio".to_string());
    if transport_val != "stdio" {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "Only stdio transport is supported; use --transport stdio".to_string(),
        ));
    }

    crate::runtime::block_on(async move { ggen_lsp::run_stdio().await })
        .map_err(|e: ggen_core::utils::Error| {
            clap_noun_verb::NounVerbError::execution_error(e.to_string())
        })?
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    Ok(StartOutput {
        status: "stopped".to_string(),
        transport: transport_val,
    })
}

/// Validate ggen law-surface files and print JSON diagnostics; exit non-zero on any ERROR.
///
/// Law surfaces: .ttl .nt .nq (RDF), .rq .sparql (SPARQL), .tera (templates), ggen.toml (config).
/// This is the headless admission gate coding-agent hooks call before accepting an edit/commit.
///
/// Modes:
///   ggen lsp check                      Scan the whole project (current dir) — checks ALL law surfaces.
///   ggen lsp check --root crates/foo    Scan a specific directory tree.
///   ggen lsp check --files "a.ttl b.rq" Check only the given paths (whitespace/comma separated).
///
/// Exit code: 0 = clean, 1 = at least one ERROR diagnostic. Build/VCS dirs
/// (target, .git, node_modules, .agent-admissibility, dist) are skipped during scans.
#[verb]
fn check(files: Option<String>, root: Option<String>, with_routes: Option<bool>) -> Result<CheckSummary> {
    let code = run_check(files.as_deref(), root.as_deref(), with_routes.unwrap_or(false))?;
    std::process::exit(code);
}

/// Resolve the file set (explicit `--files`, else scan `--root`/cwd), run the
/// gate, print the JSON report, and return the exit code. Kept out of the verb
/// so the CLI layer stays thin. `--with-routes` attaches repair RoutePlans + an
/// 80/20 route summary.
fn run_check(files: Option<&str>, root: Option<&str>, with_routes: bool) -> Result<i32> {
    let scan_root = root.unwrap_or(".");
    let explicit = files.map(parse_paths).unwrap_or_default();
    let paths = if explicit.is_empty() {
        ggen_lsp::discover_law_surfaces(Path::new(scan_root))
    } else {
        explicit
    };

    let report = ggen_lsp::check_files_with_routes(&paths, with_routes);
    // Capture this gate run as agent-edit OCEL events for `ggen lsp mine` (best-effort).
    report.capture(Path::new(scan_root));
    let json = serde_json::to_string_pretty(&report)
        .unwrap_or_else(|e| format!("{{\"error\":\"serialize failed: {e}\"}}"));
    println!("{json}");
    Ok(report.exit_code())
}

/// Make a project LSP-live in one command: write editor configs + emit the pack.
///
/// Registers `ggen lsp start` as a stdio language server for law surfaces (Helix,
/// Neovim) and emits the Agent Admissibility Pack (hooks + policies). Idempotent —
/// existing editor configs are never clobbered.
///
///   ggen lsp init                          Wire all editors + emit the pack into ".".
///   ggen lsp init --editors "helix neovim" Only the named editors.
///   ggen lsp init --root path              Initialize a specific project root.
#[verb]
fn init(root: Option<String>, editors: Option<String>, agents: Option<String>) -> Result<InitOutput> {
    let root = root.unwrap_or_else(|| ".".to_string());
    let editors = split_list(editors.as_deref());
    let agents = split_list(agents.as_deref());
    let report = ggen_lsp::init_project(Path::new(&root), &editors, &agents)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;
    Ok(InitOutput {
        files_written: report.files_written,
        pack_dir: report.pack_dir,
    })
}

fn split_list(s: Option<&str>) -> Vec<String> {
    s.map(|v| {
        v.split(|c: char| c.is_whitespace() || c == ',')
            .filter(|x| !x.is_empty())
            .map(str::to_string)
            .collect()
    })
    .unwrap_or_default()
}

/// Replay a case from logs+receipts, or verify the promotion binding (offline).
///
/// "If replay cannot reconstruct the claim, the claim is not done."
///   ggen lsp replay --case <id>   Reconstruct one episode (events, route, receipt).
///   ggen lsp replay               Verify the promotion binding (tamper → mismatch).
#[verb]
fn replay(case: Option<String>, root: Option<String>) -> Result<serde_json::Value> {
    let root = root.unwrap_or_else(|| ".".to_string());
    let value = match case {
        Some(id) => serde_json::to_value(ggen_lsp::replay_case(Path::new(&root), &id)),
        None => serde_json::to_value(ggen_lsp::verify_promotion(Path::new(&root))),
    };
    value.map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))
}

/// Measure whether evidence-promoted routes improve authoring outcomes (offline).
///
/// Reads the OCEL event log + promotion history + receipts and emits the IMPROVE-1
/// metrics with an earned verdict. Metrics lacking their backing events report
/// `insufficient_evidence` (never a fabricated number); the verdict is `improving`
/// only when ≥2 cycles show rising measured success — otherwise `insufficient_evidence`.
///
///   ggen lsp metrics            Compute metrics for the current project (.).
///   ggen lsp metrics --root p   Compute for a specific project root.
#[verb]
fn metrics(root: Option<String>) -> Result<serde_json::Value> {
    let root = root.unwrap_or_else(|| ".".to_string());
    let m = ggen_lsp::compute_metrics(Path::new(&root));
    serde_json::to_value(&m)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))
}

/// Mine captured agent-edit events for the dominant failure edges (offline).
///
/// Reads `.ggen/ocel/agent-edit-events.ocel.jsonl`, projects it to RDF, discovers
/// the directly-follows graph via SPARQL, ranks failure edges, and writes
/// `.ggen/ocel/discovery/error-edge-mining.md`. Never runs on the edit hot path.
///
///   ggen lsp mine                Mine the current project (.).
///   ggen lsp mine --root path    Mine a specific project root.
#[verb]
fn mine(root: Option<String>) -> Result<MineSummary> {
    let root = root.unwrap_or_else(|| ".".to_string());
    let report = ggen_lsp::mine(Path::new(&root))
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;
    Ok(MineSummary {
        events_analyzed: report.event_count,
        failure_edges: report.failure_edges.len(),
        total_edges: report.all_edges.len(),
        report_path: report.report_path.to_string_lossy().to_string(),
        promoted_count: report.promoted_count,
        promoted_path: report.promoted_path.to_string_lossy().to_string(),
    })
}

fn parse_paths(files: &str) -> Vec<PathBuf> {
    files
        .split(|c: char| c.is_whitespace() || c == ',')
        .filter(|s| !s.is_empty())
        .map(PathBuf::from)
        .collect()
}

/// Generate an Agent Admissibility Pack: LSP config + coding-agent hooks + SHACL policies.
///
/// The emitted hooks shell out to `ggen lsp check` and refuse edits/commits that
/// violate project canon — so agents author law surfaces under the same rules the
/// LSP shows live.
///
///   ggen lsp emit_pack                              Emit for all agents into .agent-admissibility/.
///   ggen lsp emit_pack --agents "claude-code"       Only the claude-code hook set.
///   ggen lsp emit_pack --out .ci/admissibility      Choose the output directory.
///
/// Agents (comma/space separated): claude-code, cursor, codex, generic.
#[verb]
fn emit_pack(agents: Option<String>, out: Option<String>) -> Result<EmitPackOutput> {
    let agent_list = agents
        .map(|a| {
            a.split(|c: char| c.is_whitespace() || c == ',')
                .filter(|s| !s.is_empty())
                .map(str::to_string)
                .collect::<Vec<_>>()
        })
        .filter(|v| !v.is_empty())
        .unwrap_or_else(|| {
            ggen_lsp::DEFAULT_AGENTS
                .iter()
                .map(|s| (*s).to_string())
                .collect()
        });

    let opts = ggen_lsp::PackOptions {
        agents: agent_list,
        out_dir: PathBuf::from(out.unwrap_or_else(|| ".agent-admissibility".to_string())),
    };

    let report = ggen_lsp::emit_pack(&opts)
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?;

    Ok(EmitPackOutput {
        out_dir: report.out_dir,
        agents: report.agents,
        files_written: report.files_written.len(),
    })
}
