//! Filesystem watch mode: re-run the sync pipeline whenever project files
//! change.
//!
//! [`watch`] performs one synchronous [`crate::sync::sync`] up front, then
//! watches `root` (recursively) and re-runs the pipeline on every debounced
//! batch of filesystem events. A debounced batch whose paths *all* fall
//! under `root/.ggen-v2` or `root/.git` is ignored (see [`should_ignore`]) —
//! otherwise sync's own receipt/log writes would retrigger themselves,
//! looping forever.
//!
//! No SIGINT/Ctrl-C handling: the process is expected to be killed to stop
//! watching, matching the reference implementation.

use std::{
    path::{Path, PathBuf},
    sync::mpsc,
    time::Duration,
};

use notify::RecursiveMode;
use notify_debouncer_full::new_debouncer;

use crate::{
    error::{AppError, Result},
    sync::{sync, SyncOptions, SyncReport},
};

/// Directories under the project root whose own changes must never
/// retrigger a sync (receipt/log writes and VCS bookkeeping).
const IGNORED_DIRS: [&str; 2] = [".ggen-v2", ".git"];

/// Debounce window: filesystem events within this window are batched into
/// one re-sync instead of one per touched file.
const DEBOUNCE_WINDOW: Duration = Duration::from_millis(500);

/// Watch `root` for filesystem changes, re-running the sync pipeline on
/// every debounced batch of real (non-self-triggered) changes.
///
/// Runs one synchronous sync first (its outcome is logged to stderr), then
/// blocks forever watching for further changes. A sync failure *during*
/// watching is printed to stderr and does not stop the loop — the point of
/// `--watch` is to keep going after a broken intermediate edit. This
/// function only returns (with `Ok(())`) if the debouncer's event channel
/// disconnects, which in practice means its background thread died.
///
/// # Errors
/// `[FM-WATCH-001]` if the initial sync fails, or `[FM-WATCH-002]` if the
/// underlying filesystem watcher cannot be constructed or cannot watch
/// `root`.
pub fn watch(root: &Path, dry_run: bool) -> Result<()> {
    watch_loop(root, dry_run, |_| {})
}

/// Shared implementation behind [`watch`]. `on_sync` is invoked after every
/// completed sync (the initial one and every re-triggered one), so tests
/// can observe re-sync activity without depending on stderr or timing.
fn watch_loop(root: &Path, dry_run: bool, mut on_sync: impl FnMut(&SyncReport)) -> Result<()> {
    let initial = sync(
        root,
        SyncOptions {
            dry_run,
            ..Default::default()
        },
    )
    .map_err(|e| AppError::fm_watch(1, format!("initial sync failed: {e}")))?;
    eprintln!(
        "ggen sync: {} written, {} skipped",
        initial.written.len(),
        initial.skipped.len()
    );
    on_sync(&initial);

    let (tx, rx) = mpsc::channel();
    let mut debouncer = new_debouncer(DEBOUNCE_WINDOW, None, tx)
        .map_err(|e| AppError::fm_watch(2, format!("failed to create filesystem watcher: {e}")))?;
    debouncer
        .watch(root, RecursiveMode::Recursive)
        .map_err(|e| AppError::fm_watch(2, format!("failed to watch `{}`: {e}", root.display())))?;

    eprintln!(
        "watching {} for changes... (Ctrl-C to stop)",
        root.display()
    );

    for result in rx {
        let events = match result {
            Ok(events) => events,
            Err(errors) => {
                for e in errors {
                    eprintln!("ggen sync: watch error: {e}");
                }
                continue;
            }
        };
        let paths: Vec<PathBuf> = events.iter().flat_map(|e| e.paths.clone()).collect();
        if should_ignore(root, &paths) {
            continue;
        }
        match sync(
            root,
            SyncOptions {
                dry_run,
                ..Default::default()
            },
        ) {
            Ok(report) => {
                eprintln!(
                    "ggen sync: {} written, {} skipped",
                    report.written.len(),
                    report.skipped.len()
                );
                on_sync(&report);
            }
            Err(e) => {
                eprintln!("ggen sync: {e}");
            }
        }
    }

    Ok(())
}

/// True when `paths` is empty, or every path falls under `root/.ggen-v2` or
/// `root/.git` — i.e. the change batch is self-triggered (a receipt/log
/// write, or VCS bookkeeping) and must not cause a re-sync.
fn should_ignore(root: &Path, paths: &[PathBuf]) -> bool {
    if paths.is_empty() {
        return true;
    }
    let ignored: Vec<PathBuf> = IGNORED_DIRS.iter().map(|d| root.join(d)).collect();
    paths
        .iter()
        .all(|p| ignored.iter().any(|dir| p.starts_with(dir)))
}

#[cfg(test)]
mod tests {
    use std::{
        sync::mpsc::{RecvTimeoutError, Sender},
        thread,
    };

    use tempfile::TempDir;

    use super::*;

    #[test]
    fn should_ignore_true_for_receipt_and_git_paths_only() {
        let root = PathBuf::from("/proj");
        let paths = vec![
            root.join(".ggen-v2/receipt.json"),
            root.join(".ggen-v2/receipt-log.jsonl"),
            root.join(".git/index"),
        ];
        assert!(should_ignore(&root, &paths));
    }

    #[test]
    fn should_ignore_false_when_a_real_path_is_present() {
        let root = PathBuf::from("/proj");
        let paths = vec![
            root.join(".ggen-v2/receipt.json"),
            root.join("templates/foo.tmpl"),
        ];
        assert!(!should_ignore(&root, &paths));
    }

    #[test]
    fn should_ignore_true_for_empty_batch() {
        let root = PathBuf::from("/proj");
        assert!(should_ignore(&root, &[]));
    }

    /// Bounded integration test: run `watch_loop` on a background thread
    /// against a real project fixture, edit a template from the main
    /// thread, and assert the resulting re-sync is observed within a hard
    /// deadline via `on_sync`.
    ///
    /// The spawned thread is never joined — `watch_loop` blocks on its
    /// channel forever by design (no SIGINT handling; see module docs), so
    /// joining it would hang this test indefinitely. The thread is reaped
    /// when the test process exits.
    #[test]
    fn watch_loop_resyncs_on_template_change() {
        let dir = TempDir::new().expect("tempdir");
        let root = dir.path().to_path_buf();
        seed_fixture_project(&root);

        let (tx, rx): (Sender<()>, _) = mpsc::channel();
        let watch_root = root.clone();
        thread::spawn(move || {
            // Intentionally not joined; see doc comment above.
            let _ = watch_loop(&watch_root, false, move |_report| {
                let _ = tx.send(());
            });
        });

        // Consume the initial sync's on_sync notification.
        rx.recv_timeout(Duration::from_secs(5))
            .expect("initial sync did not complete within 5s");

        // The watcher arms asynchronously and this test runs under parallel
        // suite load, so a single edit after a fixed sleep is a race. Keep
        // re-touching the template until a re-sync is observed; only a full
        // 30s of ignored edits is a real failure.
        let deadline = std::time::Instant::now() + Duration::from_secs(30);
        loop {
            std::fs::write(
                root.join("templates/greeting.tmpl"),
                "---\nto: out/greeting.txt\nforce: true\n---\nhello again\n",
            )
            .expect("edit template");
            match rx.recv_timeout(Duration::from_millis(500)) {
                Ok(()) => break,
                Err(RecvTimeoutError::Timeout) if std::time::Instant::now() < deadline => {}
                Err(RecvTimeoutError::Timeout) => {
                    panic!("watch_loop did not observe any template edit within 30s")
                }
                Err(RecvTimeoutError::Disconnected) => {
                    panic!("watch_loop's on_sync sender disconnected before re-sync was observed")
                }
            }
        }
    }

    /// Minimal `ggen.toml` + ontology + template project sufficient for
    /// `sync()` to run cleanly.
    fn seed_fixture_project(root: &Path) {
        std::fs::write(
            root.join("ggen.toml"),
            r#"
[project]
name = "watch-fixture"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#,
        )
        .expect("write ggen.toml");
        std::fs::write(
            root.join("ontology.ttl"),
            "@prefix ex: <http://example.org/> .\nex:thing ex:name \"world\" .\n",
        )
        .expect("write ontology.ttl");
        std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
        std::fs::write(
            root.join("templates/greeting.tmpl"),
            "---\nto: out/greeting.txt\nforce: true\n---\nhello\n",
        )
        .expect("write template");
    }
}
