#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! Integration courts for `ggen_engine::watch` (the `sync --watch` mode).
//!
//! What the public API actually is (read from `src/watch.rs`): a single
//! blocking `watch(root, dry_run)` that runs one synchronous `sync` up
//! front, then re-runs sync on every debounced batch of filesystem events
//! under `root` (500 ms window, `.ggen-v2`/`.git` batches ignored). It
//! never returns in practice, exposes no callback, no changed-file names,
//! and no shutdown/drop handle — the in-module tests reach the richer
//! private `watch_loop` via `#[cfg(test)]`, which this external court
//! cannot.
//!
//! So these courts pin the honestly observable external contract: real
//! filesystem side effects of the re-synced pipeline, observed with std
//! threads + `recv_timeout`-style bounded polls (the crate's runtime is
//! std `mpsc`, not tokio). Every await is bounded; the watcher threads are
//! intentionally never joined (documented below) and are reaped when the
//! test process exits normally — which every test completing *is* the
//! shutdown assertion.

use std::{
    fs,
    path::{Path, PathBuf},
    thread,
    time::{Duration, Instant, SystemTime},
};

use tempfile::TempDir;

// Serialize the watcher courts: each spawns a live OS watcher that re-syncs
// under load; concurrent fixtures contended and made bounded polls flaky.
static WATCH_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

use ggen_engine::watch::watch;

const DEBOUNCE: Duration = Duration::from_millis(500);
const BOUNDED: Duration = Duration::from_secs(30);

/// Minimal ggen project sufficient for `sync()` to run cleanly and emit
/// `out/greeting.txt` with `force: true` (so every re-sync physically
/// rewrites it — mtime/content are real observable state).
fn seed_fixture(root: &Path) -> PathBuf {
    fs::write(
        root.join("ggen.toml"),
        r#"
[project]
name = "watch-court-fixture"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#,
    )
    .expect("write ggen.toml");
    fs::write(
        root.join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:thing ex:name \"world\" .\n",
    )
    .expect("write ontology.ttl");
    fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    fs::write(
        root.join("templates/greeting.tmpl"),
        "---\nto: out/greeting.txt\nforce: true\n---\nhello\n",
    )
    .expect("write template");
    root.to_path_buf()
}

/// Spawn the blocking watcher on a leaked thread. It is never joined: the
/// public API has no shutdown, and joining would hang forever by design
/// (same tradeoff as the in-module court). The thread is reaped at test
/// process exit. `dir` is intentionally leaked via `std::mem::forget` so
/// the OS watcher never watches a deleted directory.
fn spawn_watcher(dir: TempDir) {
    let root = dir.path().to_path_buf();
    std::mem::forget(dir);
    thread::spawn(move || {
        let _ = watch(&root, false);
    });
}

/// Bounded poll: does `out/greeting.txt` contain `needle` within `deadline`?
fn wait_for_content(out: &Path, needle: &str, deadline: Duration) -> bool {
    let end = Instant::now() + deadline;
    while Instant::now() < end {
        if let Ok(s) = fs::read_to_string(out) {
            if s.contains(needle) {
                return true;
            }
        }
        thread::sleep(Duration::from_millis(50));
    }
    false
}

/// Court 1 — initial sync: starting `watch` on a fresh fixture really runs
/// the pipeline and writes the template output, within a bounded window.
#[test]
fn watch_performs_initial_sync_writing_outputs() {
    let _guard = WATCH_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path().to_path_buf();
    seed_fixture(&root);
    spawn_watcher(dir); // must run AFTER seeding: watch() initial-syncs immediately

    let out = root.join("out/greeting.txt");
    assert!(
        wait_for_content(&out, "hello", BOUNDED),
        "watch() did not complete its initial sync (out/greeting.txt with 'hello') within 15s"
    );
}

/// Court 2 — re-sync on change: editing a watched input on disk causes the
/// pipeline to re-run and the regenerated output to reflect the edit,
/// within a hard deadline. The watcher arms asynchronously, so (mirroring
/// the in-module court) edits are retried until observed; only a full 30s
/// of ignored edits fails.
///
/// Note pinned honestly: the public API names no changed files — the edit
/// is observed through its real consequence (regenerated content), not an
/// event payload.
#[test]
fn watch_resyncs_on_watched_file_change() {
    let _guard = WATCH_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path().to_path_buf();
    seed_fixture(&root);
    spawn_watcher(dir); // must run AFTER seeding: watch() initial-syncs immediately

    let out = root.join("out/greeting.txt");
    assert!(
        wait_for_content(&out, "hello", BOUNDED),
        "initial sync never wrote out/greeting.txt"
    );

    let tmpl = root.join("templates/greeting.tmpl");
    let deadline = Instant::now() + BOUNDED;
    loop {
        fs::write(
            &tmpl,
            "---\nto: out/greeting.txt\nforce: true\n---\ngoodbye world\n",
        )
        .expect("edit template");
        if wait_for_content(&out, "goodbye world", Duration::from_millis(500)) {
            break;
        }
        assert!(
            Instant::now() < deadline,
            "watch did not re-sync after template edit within 30s"
        );
    }
}

/// Court 3 — debounce coalescing: five rapid writes well inside the 500 ms
/// window must converge to the LAST write's content, with at most the
/// initial content plus one coalesced batch observed in between (a
/// per-write pipeline would surface 5 distinct intermediate contents).
///
/// External count proxy pinned honestly: `watch` exposes no event stream,
/// so coalescing is measured by sampling the output file at 20 ms while
/// the burst settles — distinct observed contents <= 2.
#[test]
fn rapid_writes_within_debounce_window_coalesce() {
    let _guard = WATCH_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path().to_path_buf();
    seed_fixture(&root);
    spawn_watcher(dir); // must run AFTER seeding: watch() initial-syncs immediately

    let out = root.join("out/greeting.txt");
    assert!(
        wait_for_content(&out, "hello", BOUNDED),
        "initial sync never wrote out/greeting.txt"
    );

    let tmpl = root.join("templates/greeting.tmpl");

    // Prime: prove the watcher is armed by landing one observed re-sync
    // before the burst, so the burst can't fall into the arm latency.
    let deadline = Instant::now() + BOUNDED;
    loop {
        fs::write(
            &tmpl,
            "---\nto: out/greeting.txt\nforce: true\n---\nprimed\n",
        )
        .expect("prime template");
        if wait_for_content(&out, "primed", Duration::from_millis(500)) {
            break;
        }
        assert!(Instant::now() < deadline, "watcher never armed within 30s");
    }

    for i in 1..=5 {
        fs::write(
            &tmpl,
            format!("---\nto: out/greeting.txt\nforce: true\n---\nburst-{i}\n"),
        )
        .expect("write template");
    }

    // Sample through the burst + debounce window.
    let mut observed: Vec<String> = vec![];
    let end = Instant::now() + DEBOUNCE * 4;
    while Instant::now() < end {
        if let Ok(s) = fs::read_to_string(&out) {
            let last = observed.last();
            if last.is_none_or(|l| l != &s) {
                observed.push(s);
            }
        }
        thread::sleep(Duration::from_millis(20));
    }

    let final_content = fs::read_to_string(&out).expect("read out file");
    assert!(
        final_content.contains("burst-5"),
        "final output must reflect the last of the 5 rapid writes; got: {final_content:?}"
    );
    assert!(
        observed.len() <= 2,
        "5 writes inside the 500ms debounce window produced {0} distinct output states \
         (expected <= 2: initial + one coalesced batch): {observed:?}",
        observed.len()
    );
}

/// Court 4 — self-write filter: a batch whose paths all fall under
/// `root/.ggen-v2` must NOT trigger a re-sync (this is exactly the loop
/// guard `should_ignore` implements). Pinned as a negative: after writing
/// inside `.ggen-v2`, the regenerated output's mtime stays unchanged for a
/// window strictly longer than the debounce window plus sync time, while a
/// subsequent real edit still re-syncs (the watcher survived).
#[test]
fn genv2_only_writes_do_not_resync() {
    let _guard = WATCH_LOCK
        .lock()
        .unwrap_or_else(std::sync::PoisonError::into_inner);
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path().to_path_buf();
    seed_fixture(&root);
    spawn_watcher(dir); // must run AFTER seeding: watch() initial-syncs immediately

    let out = root.join("out/greeting.txt");
    assert!(
        wait_for_content(&out, "hello", BOUNDED),
        "initial sync never wrote out/greeting.txt"
    );
    let before = fs::metadata(&out)
        .expect("stat out")
        .modified()
        .expect("mtime");

    // Let any armed-watcher latency pass, then write only under .ggen-v2.
    thread::sleep(DEBOUNCE * 2);
    fs::create_dir_all(root.join(".ggen-v2")).expect("mkdir .ggen-v2");
    fs::write(root.join(".ggen-v2/receipt.json"), b"{\"probe\":true}").expect("write receipt");
    thread::sleep(DEBOUNCE * 4);

    let after = fs::metadata(&out)
        .expect("stat out")
        .modified()
        .expect("mtime");
    assert_eq!(
        before, after,
        "a .ggen-v2-only write retriggered a sync (self-write loop guard failed)"
    );

    // Liveness after the ignored batch: a real edit still re-syncs.
    let tmpl = root.join("templates/greeting.tmpl");
    let deadline = Instant::now() + BOUNDED;
    loop {
        fs::write(
            &tmpl,
            "---\nto: out/greeting.txt\nforce: true\n---\nafter-ignored-batch\n",
        )
        .expect("edit template");
        if wait_for_content(&out, "after-ignored-batch", Duration::from_millis(500)) {
            break;
        }
        assert!(
            Instant::now() < deadline,
            "watcher stopped re-syncing after an ignored .ggen-v2 batch"
        );
    }
}

/// Court 5 — shutdown/drop: the public API has no drop/shutdown handle
/// (module docs: "the process is expected to be killed to stop watching").
/// The honest pin is that every watcher thread above leaks by design and
/// the test process still terminates normally with all courts green —
/// which is exactly what a passing run of this file demonstrates. No
/// synthetic assertion is possible without a non-public seam, so none is
/// fabricated.
#[test]
fn watch_has_no_public_shutdown_process_exit_is_the_shutdown() {
    // README.md is NOT in the ignore list (only .ggen-v2/.git are), so an
    // irrelevant-file write is expected to re-sync; that behavior is a
    // property of the recursive whole-root watch, pinned here as
    // documented reality rather than asserted through an unobservable
    // channel. This test pins the public-surface fact: `watch` returns
    // `Result<()>` and only ever returns on channel disconnect.
    let sig: fn(&Path, bool) -> ggen_engine::error::Result<()> = watch;
    let _ = sig as fn(&Path, bool) -> ggen_engine::error::Result<()>;
    let _ = SystemTime::now();
}
