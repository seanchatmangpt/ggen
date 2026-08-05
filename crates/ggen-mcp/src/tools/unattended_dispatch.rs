//! Gall CP33: the bounded unattended-write dispatcher. Grounded in real
//! precedent from three sibling projects with their own receipted/
//! consequence-tracking pipelines (`~/mfw`'s branchless declared-risk-class
//! admit-mask, `~/turbo-fieldfare/kcj-mustar`'s independent-recheck-at-the-
//! dispatch-boundary discipline, `~/wasm4pm`'s cautionary lesson that a
//! classification which doesn't actually gate anything is worse than none)
//! -- see `/Users/sac/.claude/plans/80-20-gall-test-refactor-cheerful-quokka.md`'s
//! CP31-36 for the full research trail.
//!
//! **What this is not**: not CP21's original "any declared trigger->action
//! mapping" dispatcher -- that was assessed and rejected as unsafe. This is
//! deliberately narrower: a write may fire with zero human/LLM decision step
//! ONLY when every one of five conditions holds simultaneously (see
//! `try_unattended_apply`'s doc comment). Anything failing any condition
//! falls through unchanged to the existing CP17/CP28 human/LLM-reviewed
//! path -- this module never weakens or bypasses `write_apply`'s own
//! hash-corroboration gate; it is simply one more caller of that same,
//! unmodified function.

use std::collections::VecDeque;
use std::path::Path;
use std::sync::Arc;
use std::time::{Duration, Instant};

use ggen_engine::config::GgenConfig;
use ggen_engine::pack;
use ggen_engine::sync::{discover_templates, sync, SyncOptions};
use serde::Serialize;
use tokio::sync::Mutex;

use crate::tools::protected_paths::is_protected_path;
use crate::tools::write_apply::{write_apply, WriteApplyParams};

/// Rolling-window circuit breaker (mfw/wasm4pm precedent: bound the *volume*
/// of zero-decision-step writes, not just gate individual attempts) --
/// distinct from mfw's failure-triggered breaker (`PolicyGuard`'s anomaly
/// threshold), since an eligible unattended write can never itself "fail" in
/// the corrupting sense (it can only ever create a nonexistent file); the
/// risk here is velocity, not failure, so this counts attempts, not errors.
#[derive(Debug, Clone)]
pub struct CircuitBreaker {
    inner: Arc<Mutex<VecDeque<Instant>>>,
    max_per_window: usize,
    window: Duration,
}

impl Default for CircuitBreaker {
    fn default() -> Self {
        // Working default, not a derived constant (same honesty discipline
        // as CP28's skip-classification default): 5 unattended writes per
        // 60s per dispatcher instance is generous for the narrow, per-rule
        // eligible class this dispatcher targets, while still catching a
        // pathological repeat-fire loop.
        Self::new(5, Duration::from_secs(60))
    }
}

impl CircuitBreaker {
    #[must_use]
    pub fn new(max_per_window: usize, window: Duration) -> Self {
        Self {
            inner: Arc::new(Mutex::new(VecDeque::new())),
            max_per_window: max_per_window.max(1),
            window,
        }
    }

    /// True iff a new attempt is currently allowed. Does NOT record the
    /// attempt -- call `record` separately, after the attempt is confirmed
    /// eligible on every other ground, so a rejected-for-other-reasons
    /// attempt never consumes rate-limit budget.
    async fn allow(&self) -> bool {
        let mut deque = self.inner.lock().await;
        let now = Instant::now();
        while let Some(&front) = deque.front() {
            if now.duration_since(front) > self.window {
                deque.pop_front();
            } else {
                break;
            }
        }
        deque.len() < self.max_per_window
    }

    async fn record(&self) {
        self.inner.lock().await.push_back(Instant::now());
    }
}

/// R2: a `CircuitBreaker` per distinct project root, so a rate-limit burst
/// on one project can never exhaust another, unrelated project's budget --
/// the real bug the earlier single, process-wide `static CircuitBreaker` in
/// `bridge.rs` had. Unbounded by root count (not FIFO-evicted like
/// `DiagnosticStore`, CP18's precedent for the same class of concern): a
/// real `ggen-mcp` server process is scoped to one project root in every
/// deployment shape this codebase supports today (`start_stdio` takes no
/// root-switching API), so in practice this map never grows past one entry.
/// Revisit with real eviction if that assumption ever changes.
#[derive(Debug, Clone, Default)]
pub struct PerRootCircuitBreaker {
    inner: Arc<Mutex<std::collections::HashMap<std::path::PathBuf, CircuitBreaker>>>,
}

impl PerRootCircuitBreaker {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// The `CircuitBreaker` for exactly this root, creating one with
    /// `CircuitBreaker::default()` on first use.
    pub async fn for_root(&self, root: &Path) -> CircuitBreaker {
        let mut map = self.inner.lock().await;
        map.entry(root.to_path_buf())
            .or_insert_with(CircuitBreaker::default)
            .clone()
    }
}

/// One line of `.ggen/unattended-dispatch-log.jsonl` -- appended for every
/// dispatch attempt, not only successful applies (mfw's "refusal still
/// receipts" precedent: the audit trail must show the dispatcher's full
/// decision history).
#[derive(Debug, Serialize)]
struct DispatchLogEntry<'a> {
    root: String,
    outcome: &'a str,
    reason: Option<&'a str>,
    written: Option<&'a [String]>,
    receipt_path: Option<&'a str>,
}

fn append_audit_log(root: &Path, entry: &DispatchLogEntry<'_>) {
    let Ok(line) = serde_json::to_string(entry) else {
        return;
    };
    let log_path = root.join(".ggen/unattended-dispatch-log.jsonl");
    if let Some(parent) = log_path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    if let Ok(mut f) = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&log_path)
    {
        use std::io::Write as _;
        let _ = writeln!(f, "{line}");
    }
}

/// Outcome of one `try_unattended_apply` call.
#[derive(Debug, PartialEq, Eq)]
pub enum UnattendedApplyOutcome {
    /// The write fired with zero human/LLM decision step. Carries the
    /// root-relative paths that were actually written.
    Applied(Vec<String>),
    /// At least one of the five eligibility conditions did not hold; the
    /// caller should fall through to the existing human/LLM-reviewed path
    /// unchanged. Carries a human-readable reason, never a bare bool.
    NotEligible(String),
}

/// Attempt a bounded, zero-decision-step write for `root`. Every one of
/// these five conditions must hold, checked fresh on every call (never
/// cached, never trusted from a caller -- the kcj-mustar precedent: the
/// dispatch boundary re-verifies itself rather than trusting an upstream
/// claim):
///
/// 1. The project has at least one frontmatter template with
///    `unattended_write_eligible: true` (which the writer already refuses to
///    parse unless `unless_exists: true` is also set -- CP31,
///    `template.rs::parse`).
/// 2. Every such template's `to:` is a static path (no `{{`) -- a real,
///    named v1 limitation: a Tera-templated `to:` (fan-out) is never
///    eligible, since this dispatcher cannot yet enumerate the fanned-out
///    target set ahead of a real sync.
/// 3. None of those static targets exist on disk yet, and none match
///    `protected_paths::is_protected_path`.
/// 4. A fresh dry-run (`sync(root, {dry_run:true})`) succeeds (zero FM-*
///    refusals across the WHOLE project, not just the eligible rule) AND
///    every path in its `report.written` is covered by the eligible-target
///    set from steps 1-3 -- i.e. nothing outside the declared-safe class
///    would actually be written by a real sync right now.
/// 5. The circuit breaker has budget remaining for this call.
///
/// On success, calls the real, unmodified `write_apply` with a hash freshly
/// recomputed by THIS function's own preflight (step 4) -- CP17's
/// hash-corroboration gate runs exactly as it does for any other caller.
pub async fn try_unattended_apply(root: &Path, breaker: &CircuitBreaker) -> UnattendedApplyOutcome {
    let (outcome, receipt_path) = try_unattended_apply_inner(root, breaker).await;
    let entry = match &outcome {
        UnattendedApplyOutcome::Applied(written) => DispatchLogEntry {
            root: root.display().to_string(),
            outcome: "applied",
            reason: None,
            written: Some(written),
            receipt_path: receipt_path.as_deref(),
        },
        UnattendedApplyOutcome::NotEligible(reason) => DispatchLogEntry {
            root: root.display().to_string(),
            outcome: "not_eligible",
            reason: Some(reason),
            written: None,
            receipt_path: None,
        },
    };
    append_audit_log(root, &entry);
    outcome
}

async fn try_unattended_apply_inner(
    root: &Path, breaker: &CircuitBreaker,
) -> (UnattendedApplyOutcome, Option<String>) {
    macro_rules! not_eligible {
        ($($arg:tt)*) => {
            return (UnattendedApplyOutcome::NotEligible(format!($($arg)*)), None)
        };
    }

    let config_path = root.join("ggen.toml");
    let config = match GgenConfig::load(&config_path) {
        Ok(c) => c,
        Err(e) => not_eligible!("cannot load ggen.toml: {e}"),
    };
    let packs = match pack::resolve_read_only(&config, root) {
        Ok(p) => p,
        Err(e) => not_eligible!("cannot resolve packs: {e}"),
    };
    let templates = match discover_templates(root, &config, &packs) {
        Ok(t) => t,
        Err(e) => not_eligible!("cannot discover templates: {e}"),
    };

    let eligible: Vec<&ggen_engine::template::Template> = templates
        .iter()
        .map(|(_, t)| t)
        .filter(|t| t.frontmatter.unattended_write_eligible)
        .collect();
    if eligible.is_empty() {
        not_eligible!("no unattended_write_eligible templates declared in this project");
    }

    let mut eligible_targets: Vec<String> = Vec::new();
    for t in &eligible {
        let to = &t.frontmatter.to;
        if to.contains("{{") {
            not_eligible!(
                "template targeting `{to}` has a Tera-templated `to:` -- dynamic/fan-out \
                 output paths are not yet eligible for unattended dispatch (v1 limitation)"
            );
        }
        let abs = root.join(to);
        if abs.exists() {
            not_eligible!(
                "eligible target `{to}` already exists on disk -- unless_exists guarantees \
                 this dispatcher never overwrites, so an existing file here means this rule \
                 has nothing left to do"
            );
        }
        if is_protected_path(root, Path::new(to)) {
            not_eligible!(
                "eligible target `{to}` matches a protected path -- refusing regardless of \
                 the template's own unattended_write_eligible declaration"
            );
        }
        eligible_targets.push(to.clone());
    }

    let preflight = sync(
        root,
        SyncOptions {
            dry_run: true,
            ..Default::default()
        },
    );
    let report = match preflight {
        Ok(r) => r,
        Err(e) => not_eligible!(
            "project has an active sync refusal, so the whole-project-clean requirement \
             fails (real error: {e})"
        ),
    };

    for written in &report.written {
        let rel = written.display().to_string();
        if !eligible_targets.contains(&rel) {
            not_eligible!(
                "a real sync would also write `{rel}`, which is not covered by any \
                 unattended_write_eligible template -- the whole run must be eligible, not \
                 just part of it"
            );
        }
    }
    if report.written.is_empty() {
        not_eligible!("dry-run reports nothing would be written -- nothing to dispatch");
    }

    if !breaker.allow().await {
        not_eligible!(
            "rate-limited: too many unattended writes for this root in the current window"
        );
    }

    let params = WriteApplyParams::for_unattended_dispatch(
        root.display().to_string(),
        report.graph_hash_hex.clone(),
    );
    match write_apply(&params) {
        Ok(result) => {
            breaker.record().await;
            let receipt_path = Some(result.receipt_path.clone());
            (
                UnattendedApplyOutcome::Applied(
                    result.written.into_iter().map(|w| w.path).collect(),
                ),
                receipt_path,
            )
        }
        Err(e) => (
            UnattendedApplyOutcome::NotEligible(format!(
                "write_apply refused despite passing every prior check (real error: {e}) -- \
                 the project state changed between preflight and apply"
            )),
            None,
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn circuit_breaker_allows_up_to_the_limit_then_refuses() {
        let breaker = CircuitBreaker::new(2, Duration::from_secs(60));
        assert!(breaker.allow().await);
        breaker.record().await;
        assert!(breaker.allow().await);
        breaker.record().await;
        assert!(
            !breaker.allow().await,
            "third attempt should be rate-limited"
        );
    }

    #[tokio::test]
    async fn circuit_breaker_recovers_after_the_window_elapses() {
        let breaker = CircuitBreaker::new(1, Duration::from_millis(50));
        assert!(breaker.allow().await);
        breaker.record().await;
        assert!(!breaker.allow().await);
        tokio::time::sleep(Duration::from_millis(80)).await;
        assert!(breaker.allow().await, "window should have rolled over");
    }

    /// R2: two distinct project roots must never share a rate-limit budget
    /// -- a burst on root A must not exhaust root B's independent budget.
    /// The real bug this fixes: an earlier version used a single, process-
    /// wide `CircuitBreaker` for every root.
    #[tokio::test]
    async fn distinct_roots_have_independent_rate_limit_budgets() {
        let breakers = PerRootCircuitBreaker::new();
        let root_a = std::path::PathBuf::from("/tmp/cp39-root-a");
        let root_b = std::path::PathBuf::from("/tmp/cp39-root-b");

        let breaker_a = breakers.for_root(&root_a).await;
        // Exhaust root A's entire default budget (5 per 60s).
        for _ in 0..5 {
            assert!(breaker_a.allow().await);
            breaker_a.record().await;
        }
        assert!(
            !breaker_a.allow().await,
            "root A's own budget must now be exhausted"
        );

        // Root B, an entirely different project, must have its own, full,
        // untouched budget -- not affected by root A's burst at all.
        let breaker_b = breakers.for_root(&root_b).await;
        assert!(
            breaker_b.allow().await,
            "root B must have an independent, unexhausted budget"
        );

        // Fetching root A's breaker again must return the SAME (still
        // exhausted) instance, not a fresh one -- proves `for_root` really
        // persists state per root rather than handing out a new breaker
        // each call.
        let breaker_a_again = breakers.for_root(&root_a).await;
        assert!(
            !breaker_a_again.allow().await,
            "re-fetching root A's breaker must return the same, still-exhausted instance"
        );
    }

    #[tokio::test]
    async fn no_eligible_templates_is_not_eligible() {
        let tmp = tempfile::TempDir::new().expect("tempdir");
        std::fs::write(
            tmp.path().join("ggen.toml"),
            "[project]\nname = \"empty\"\n[ontology]\nsource = \"model.ttl\"\n\
             [templates]\ndir = \"templates\"\n",
        )
        .expect("write ggen.toml");
        std::fs::write(
            tmp.path().join("model.ttl"),
            "@prefix ex: <http://example.org/> .\nex:a ex:b ex:c .\n",
        )
        .expect("write ontology");
        std::fs::create_dir_all(tmp.path().join("templates")).expect("mkdir");
        let breaker = CircuitBreaker::default();
        let outcome = try_unattended_apply(tmp.path(), &breaker).await;
        assert!(matches!(outcome, UnattendedApplyOutcome::NotEligible(_)));
    }
}
