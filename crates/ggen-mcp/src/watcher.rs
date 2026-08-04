//! CP15: the production file-change watcher.
//!
//! CP12/CP16 proved `crate::bridge::push_diagnostics_for_root` can reach a
//! real MCP client -- but nothing called it on a real filesystem event in a
//! running process; `GgenMcpServer::start_stdio`'s own doc comment said so
//! explicitly ("No watcher task is spawned here yet ... building one is out
//! of scope for this checkpoint"). This module is that watcher.
//!
//! ## Why a standalone watcher in `ggen-mcp`, not a call from `ggen-lsp`
//!
//! `ggen-lsp` (the language server, `crates/ggen-lsp/src/server.rs`) and
//! `ggen-mcp` (this crate's MCP server) are separate binaries, launched
//! independently by whatever client speaks LSP vs. MCP to them -- there is
//! no existing IPC between the two live processes, and `crate::bridge`
//! already reflects that: `push_diagnostics_for_root` does not read
//! `ggen-lsp`'s live in-memory document/analyzer state at all. It re-runs
//! `ggen_lsp::check::check_files_in_root` (the same *headless*, file-reading
//! gate the CLI and CI use) as a library call, scoped to the paths it is
//! given. So bridging "a real did_change fires inside a running `ggen-lsp`
//! process" to "a real notification reaches an MCP client" would require
//! building a new cross-process channel from scratch (e.g. `ggen-lsp`
//! shelling out to, or IPC-ing into, a specific `ggen-mcp` process) -- a much
//! larger and riskier change than watching the filesystem directly from the
//! process that already owns the `PeerRegistry`/`DiagnosticStore` and the
//! headless gate call. `notify`/`notify-debouncer-full` are already
//! workspace dependencies (`ggen-engine::watch` uses the identical
//! debounce-loop shape this module mirrors), so this is additive wiring, not
//! a new dependency.
//!
//! ## Scope
//!
//! Watches one root (the server process's current working directory --
//! `ggen-mcp` has no per-connection "project root" concept today; every tool
//! call takes its own `root` parameter instead, see `crate::project_root`).
//! On a debounced batch of filesystem events, calls
//! `push_diagnostics_for_root` with exactly the changed paths (never a
//! full-root rescan), for the same `GGEN-TPL-001` code `crate::bridge`'s own
//! proof already exercises -- widening the code set is CP19, out of scope
//! here.

use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::time::Duration;

use notify::RecursiveMode;
use notify_debouncer_full::new_debouncer;

use crate::bridge::{push_diagnostics_for_root, DiagnosticStore, PeerRegistry};

/// Directories under the watched root whose own changes must never trigger
/// a re-check -- mirrors `ggen_engine::watch::IGNORED_DIRS` (receipt/log
/// writes and VCS bookkeeping would otherwise retrigger themselves).
const IGNORED_DIRS: [&str; 3] = [".git", ".ggen-v2", ".ggen"];

/// Debounce window: filesystem events within this window are batched into
/// one re-check instead of one per touched file -- same value
/// `ggen_engine::watch::DEBOUNCE_WINDOW` uses.
const DEBOUNCE_WINDOW: Duration = Duration::from_millis(500);

/// Diagnostic codes this watcher pushes on. `crate::bridge`'s own end-to-end
/// proof (`tpl_001_diagnostic_reaches_a_real_mcp_client`) already exercises
/// `GGEN-TPL-001`; reusing it here means this watcher is wiring, not new
/// diagnostic-code work.
pub const WATCHED_CODES: [&str; 1] = ["GGEN-TPL-001"];

/// Start watching `root` in a dedicated OS thread, pushing real diagnostics
/// (scoped to the changed paths only) over `peers`/into `store` on every
/// debounced batch of filesystem events.
///
/// Must be called from within a Tokio runtime -- it captures
/// [`tokio::runtime::Handle::current`] so the watcher thread (which cannot
/// itself be `async`, since `notify`'s callback is synchronous) can spawn
/// the real `async` push for each batch back onto the runtime.
///
/// # Errors
/// Returns an error if the underlying filesystem watcher cannot be
/// constructed or cannot watch `root` (e.g. `root` does not exist). A
/// caller that wants "serve anyway, just without live pushes" should log
/// this error rather than propagate it -- `GgenMcpServer::start_stdio` does
/// exactly that, since the watcher is an auxiliary capability, not a
/// prerequisite for the request/response tool surface.
pub fn spawn_root_watcher(
    root: PathBuf, peers: PeerRegistry, store: DiagnosticStore,
) -> anyhow::Result<()> {
    let handle = tokio::runtime::Handle::current();
    let (tx, rx) = mpsc::channel();
    let mut debouncer = new_debouncer(DEBOUNCE_WINDOW, None, tx)?;
    debouncer.watch(&root, RecursiveMode::Recursive)?;

    std::thread::Builder::new()
        .name("ggen-mcp-watcher".to_string())
        .spawn(move || {
            // Keep the debouncer (and its background watch thread) alive for
            // the lifetime of this thread -- dropping it stops the watch.
            let _debouncer = debouncer;
            for result in rx {
                let events = match result {
                    Ok(events) => events,
                    Err(errors) => {
                        for e in errors {
                            tracing::warn!(error = %e, "ggen-mcp watcher: filesystem watch error");
                        }
                        continue;
                    }
                };
                let paths = relevant_paths(&root, events.iter().flat_map(|e| e.paths.clone()));
                if paths.is_empty() {
                    continue;
                }

                let root = root.clone();
                let peers = peers.clone();
                let store = store.clone();
                handle.spawn(async move {
                    match push_diagnostics_for_root(&peers, &store, &root, &paths, &WATCHED_CODES)
                        .await
                    {
                        Ok(outcome) => {
                            if outcome.matched > 0 {
                                tracing::info!(
                                    matched = outcome.matched,
                                    delivered_notifications = outcome.delivered_notifications,
                                    "ggen-mcp watcher: pushed diagnostics for real file change"
                                );
                            }
                        }
                        Err(e) => {
                            tracing::warn!(error = %e, "ggen-mcp watcher: push_diagnostics_for_root failed");
                        }
                    }
                });
            }
        })?;

    Ok(())
}

/// Filter a batch of raw event paths down to real, in-root, non-ignored
/// files -- so a `.git`/`.ggen`/`.ggen-v2` write (including this bridge's
/// own future receipt writes, if any) never retriggers itself, and a path
/// outside `root` (defensive; `notify` should not report these for a
/// recursive watch scoped to `root`) is dropped rather than passed to
/// `check_files_in_root`, which treats every path as relative-to-root.
fn relevant_paths(root: &Path, paths: impl Iterator<Item = PathBuf>) -> Vec<PathBuf> {
    let mut seen = std::collections::HashSet::new();
    paths
        .filter(|p| p.starts_with(root))
        .filter(|p| {
            let rel = p.strip_prefix(root).unwrap_or(p);
            !IGNORED_DIRS.iter().any(|dir| rel.starts_with(dir))
        })
        .filter(|p| seen.insert(p.clone()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ignores_dotggen_and_git_paths() {
        let root = PathBuf::from("/project");
        let paths = vec![
            PathBuf::from("/project/row.tera"),
            PathBuf::from("/project/.ggen-v2/receipt.json"),
            PathBuf::from("/project/.git/index"),
            PathBuf::from("/project/.ggen/lambda_cd.gate"),
        ];
        let kept = relevant_paths(&root, paths.into_iter());
        assert_eq!(kept, vec![PathBuf::from("/project/row.tera")]);
    }

    #[test]
    fn drops_paths_outside_root() {
        let root = PathBuf::from("/project");
        let paths = vec![PathBuf::from("/other/row.tera")];
        let kept = relevant_paths(&root, paths.into_iter());
        assert!(kept.is_empty());
    }

    #[test]
    fn deduplicates_repeated_paths_in_one_batch() {
        let root = PathBuf::from("/project");
        let paths = vec![
            PathBuf::from("/project/row.tera"),
            PathBuf::from("/project/row.tera"),
        ];
        let kept = relevant_paths(&root, paths.into_iter());
        assert_eq!(kept, vec![PathBuf::from("/project/row.tera")]);
    }
}
