//! Agent noun — the AGI-facing CLI surface over `crate::agent::PackAgent`.
//!
//! `ggen agent <verb>` is the third transport over the one authoritative pack
//! lifecycle facade, alongside the Rust library API (`crate::agent`, ported from
//! `ggen_core::agent` — see that module's doc comment) and the `ggen.packs.*`
//! MCP/A2A tools (`ggen-a2a-mcp`). Every verb emits structured
//! JSON an autonomous agent can parse and chain, covering the full
//! project-bring-up lifecycle an AGI drives to complete a project:
//!
//! ```text
//!   capabilities → search / list / show → resolve → compatibility
//!       → install → status → verify        (and remove)
//! ```
//!
//! Because every verb routes through the same `PackAgent` the MCP/A2A surface
//! uses — not a parallel implementation — the three transports cannot drift, and
//! the durable-state contract (lockfile entry with a non-empty digest, a signed
//! provenance receipt) and the fail-closed error behaviour are identical
//! everywhere.

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;

use crate::agent::{InstallRequest, PackAgent};

// ── helpers ─────────────────────────────────────────────────────────────────

/// Construct an agent rooted at the current working directory (the project root
/// for a CLI invocation, matching where `install` writes the lockfile).
fn agent() -> Result<PackAgent> {
    PackAgent::new()
        .map_err(|e| NounVerbError::execution_error(format!("agent init failed: {}", e)))
}

/// Construct an agent at an explicit `--root`, or the current directory if none
/// is given. Used by the read-only `status` / `verify` verbs.
fn agent_at(root: Option<String>) -> Result<PackAgent> {
    match root {
        Some(r) => Ok(PackAgent::at_root(r)),
        None => agent(),
    }
}

/// Lift a facade error into a CLI error, preserving the message.
fn lift<T>(r: crate::agent::AgentResult<T>) -> Result<T> {
    r.map_err(|e| NounVerbError::execution_error(e.to_string()))
}

/// Serialize a facade outcome to JSON so every verb emits a uniform,
/// agent-parseable result.
fn json<T: serde::Serialize>(value: T) -> Result<serde_json::Value> {
    serde_json::to_value(value)
        .map_err(|e| NounVerbError::execution_error(format!("serialization failed: {}", e)))
}

// ── discovery (read-only) ───────────────────────────────────────────────────

/// Describe the agent's operations and capability surfaces — the discovery entry
/// point an agent calls first to learn the contract.
#[verb]
pub fn capabilities() -> Result<serde_json::Value> {
    json(agent()?.capabilities())
}

/// Relevance-rank packs in the local registry by a text query.
#[verb]
pub fn search(#[arg(index = 1)] query: String, limit: Option<usize>) -> Result<serde_json::Value> {
    json(lift(agent()?.search(&query, limit))?)
}

/// List all packs in the local registry, optionally filtered by category.
#[verb]
pub fn list(category: Option<String>) -> Result<serde_json::Value> {
    json(lift(agent()?.list(category.as_deref()))?)
}

/// Full detail for one pack: metadata, packages, templates, dependencies, and
/// the validation (quality-gate) result.
#[verb]
pub fn show(#[arg(index = 1)] pack_id: String) -> Result<serde_json::Value> {
    json(lift(agent()?.show(&pack_id))?)
}

/// Resolve a capability surface (e.g. `mcp`, `web`) to concrete pack IDs,
/// optionally narrowed by `--projection` and `--runtime`.
#[verb]
pub fn resolve(
    #[arg(index = 1)] surface: String, projection: Option<String>, runtime: Option<String>,
) -> Result<serde_json::Value> {
    json(lift(agent()?.resolve_capability(
        &surface,
        projection.as_deref(),
        runtime.as_deref(),
    ))?)
}

/// Check whether a comma-separated set of packs composes without conflicts
/// (overlapping packages or unloadable packs). The pre-flight before a
/// multi-pack install.
#[verb]
pub fn compatibility(#[arg(index = 1)] packs: String) -> Result<serde_json::Value> {
    let a = agent()?;
    let ids: Vec<String> = packs
        .split(',')
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty())
        .collect();
    let res = crate::runtime::block_on(a.check_compatibility(&ids))
        .map_err(|e| NounVerbError::execution_error(e.to_string()))?;
    json(lift(res)?)
}

// ── installed state / provenance (read-only) ────────────────────────────────

/// Report installed packs from the project lockfile (`--root` to inspect another
/// project; default is the current directory).
#[verb]
pub fn status(root: Option<String>) -> Result<serde_json::Value> {
    json(lift(agent_at(root)?.status())?)
}

/// Verify a provenance receipt against its signing key. Fail-closed: a missing
/// key, malformed receipt, or bad signature yields `is_valid: false`.
#[verb]
pub fn verify(
    #[arg(index = 1)] receipt_path: String, root: Option<String>,
) -> Result<serde_json::Value> {
    json(agent_at(root)?.verify(&receipt_path))
}

// ── mutating lifecycle ──────────────────────────────────────────────────────

/// Install a pack: write the lockfile with a non-empty digest and emit a signed
/// provenance receipt. `--dry_run` previews without writing durable state.
///
/// The "already installed" check and the materialized install directory are
/// both scoped to the project-local `<root>/.ggen/packs/<pack-id>` (BUG-004
/// fix, see `docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md`),
/// matching the project-local lockfile — no dependency on unrelated global
/// machine state.
#[verb]
pub fn install(
    #[arg(index = 1)] pack_id: String, force: Option<bool>, dry_run: Option<bool>,
) -> Result<serde_json::Value> {
    install_impl(pack_id, force, dry_run)
}

/// Real implementation of `install`, factored out of the `#[verb]` fn above so
/// its signature can be reused directly.
fn install_impl(
    pack_id: String, force: Option<bool>, dry_run: Option<bool>,
) -> Result<serde_json::Value> {
    let a = agent()?;
    let req = InstallRequest {
        pack_id,
        force: force.unwrap_or(false),
        dry_run: dry_run.unwrap_or(false),
        emit_receipt: true,
    };
    let res = crate::runtime::block_on(a.install(req))
        .map_err(|e| NounVerbError::execution_error(e.to_string()))?;
    json(lift(res)?)
}

/// Remove a pack from the project lockfile. Fail-closed: a missing lockfile or
/// an absent pack errors and leaves the lockfile intact.
#[verb]
pub fn remove(#[arg(index = 1)] pack_id: String) -> Result<serde_json::Value> {
    json(lift(agent()?.remove(&pack_id))?)
}
