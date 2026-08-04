//! The referee: invariants that must hold for **any** input.
//!
//! This is the deterministic half of self-play. A case may be arbitrary
//! hostile text — the referee's job is not to decide whether the case is
//! *reasonable* but whether ggen's response to it was *lawful*.
//!
//! Every invariant here is stated so it can be falsified by a concrete
//! observation, and every one of them maps to a failure class that would
//! otherwise be silent. An invariant that could only be violated by an
//! obvious crash would not be worth checking; these are the quiet ones.

use std::collections::BTreeSet;
use std::path::Path;

use serde::{Deserialize, Serialize};

/// The invariant set. Adding a variant means adding a real check in
/// [`referee_verdict`] — the enum is not a wish list.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Invariant {
    /// A tool returned neither a result nor a typed error (panic, hang, or
    /// a dropped response). The most basic liveness property.
    AlwaysAnswers,
    /// A malformed or unexecutable input produced a *successful* response.
    /// This is the fail-open class: the single most dangerous failure mode,
    /// because the caller cannot tell it happened.
    NeverFailOpen,
    /// A write landed outside the consumer project root. Path traversal via
    /// `to:`, symlink escape, or absolute path.
    NeverEscapesRoot,
    /// Reported `row_count` disagreed with an independent count of the same
    /// query. Dishonest counts are what the crate exists to prevent.
    HonestRowCount,
    /// Rows were dropped without `truncated` being set.
    NeverSilentlyTruncates,
    /// A successful apply left no verifying receipt. A success claim with no
    /// durable evidence is decorative completion.
    WriteImpliesReceipt,
    /// Re-running an unchanged sync wrote something new.
    Idempotent,
    /// The dry run's predicted write set disagreed with what the real apply
    /// actually wrote, for the same unmodified case.
    DryRunAgreesWithApply,
}

impl Invariant {
    /// One-line statement of what a violation of this invariant means, used
    /// verbatim in failure output so a red test explains itself without the
    /// reader needing this file.
    #[must_use]
    pub fn statement(self) -> &'static str {
        match self {
            Self::AlwaysAnswers => {
                "every tool call must return a result or a typed error, never nothing"
            }
            Self::NeverFailOpen => {
                "invalid input must produce an error, never a successful-looking empty result"
            }
            Self::NeverEscapesRoot => "no write may land outside the consumer project root",
            Self::HonestRowCount => {
                "reported row_count must equal an independent count of the same query"
            }
            Self::NeverSilentlyTruncates => "dropping rows requires truncated=true",
            Self::WriteImpliesReceipt => "a successful apply must leave a receipt that verifies",
            Self::Idempotent => "an unchanged re-sync must write nothing new",
            Self::DryRunAgreesWithApply => {
                "the dry run's predicted write set must match what apply actually wrote"
            }
        }
    }
}

/// A concrete, located invariant breach.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Violation {
    pub invariant: Invariant,
    /// What was actually observed, specifically enough to act on. Never a
    /// bare restatement of the invariant.
    pub observed: String,
}

/// The referee's ruling on one played case.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Verdict {
    pub violations: Vec<Violation>,
}

impl Verdict {
    #[must_use]
    pub fn clean(&self) -> bool {
        self.violations.is_empty()
    }

    #[must_use]
    pub fn broken(&self) -> BTreeSet<Invariant> {
        self.violations.iter().map(|v| v.invariant).collect()
    }

    fn fail(&mut self, invariant: Invariant, observed: impl Into<String>) {
        self.violations.push(Violation {
            invariant,
            observed: observed.into(),
        });
    }
}

/// Everything observed while playing one case, handed to the referee as
/// plain data. Kept as an owned record (rather than the referee re-running
/// anything) so a verdict is a pure function of observations — the same
/// observations always yield the same ruling, which is what makes corpus
/// replay meaningful.
#[derive(Debug, Clone, Default)]
pub struct Observation {
    /// `Some(true)` = query tool reported success, `Some(false)` = typed
    /// error, `None` = no answer at all.
    pub query_ok: Option<bool>,
    /// `row_count` the tool reported.
    pub reported_rows: Option<usize>,
    /// Rows actually present in the response body.
    pub returned_rows: Option<usize>,
    /// The tool's `truncated` flag.
    pub truncated: Option<bool>,
    /// Independent recount of the same query, obtained by a second path
    /// (a `COUNT(*)` aggregate) rather than by trusting the first answer.
    pub independent_rows: Option<usize>,
    /// Whether the SPARQL is well-formed, decided by the syntax gate rather
    /// than by the referee re-parsing it.
    pub syntax_valid: Option<bool>,
    /// Apply reported success.
    pub applied_ok: Option<bool>,
    /// Files the apply reported writing, project-root-relative.
    pub written: Vec<String>,
    /// Absolute paths that actually changed on disk anywhere the harness
    /// watched — including outside the root. This is how root escape is
    /// detected by observation instead of by trusting the tool's report.
    pub changed_outside_root: Vec<String>,
    /// A receipt exists at the reported path and verified.
    pub receipt_verified: Option<bool>,
    /// Files written by an immediate second apply of unchanged input.
    /// `None` means the second apply did not produce a count at all --
    /// which is a finding, not a reason to skip the idempotence check.
    pub second_apply_written: Option<usize>,
    /// Why the second apply produced no count, when it produced none. A
    /// re-sync that *errors* is at least as serious as one that writes
    /// extra files, and originally this harness dropped that error on the
    /// floor: `if let Some(n) = second_apply_written` silently skipped the
    /// check, so a project whose re-sync fails looked identical to one that
    /// is perfectly idempotent. Found by the vacuity audit.
    pub second_apply_error: Option<String>,
    /// `Some(true)` = dry-run tool reported success, `Some(false)` = typed
    /// error, `None` = no answer at all. Originally this call's whole
    /// `Result` was discarded (`let _ = sync_dry_run(...)`), so a broken,
    /// panicking, or wrong dry-run tool played through self-play with zero
    /// chance of being flagged despite `ggen_sync_dry_run` being one of the
    /// two tools (with `ggen_query_preview`) this crate exists to make
    /// trustworthy. Found by the vacuity audit.
    pub dry_run_ok: Option<bool>,
    /// Root-relative paths the dry run predicted it would write, when the
    /// dry run itself succeeded. Compared against `written` below so a dry
    /// run that disagrees with what apply actually does is a finding, not
    /// silent.
    pub dry_run_would_write: Option<BTreeSet<String>>,
}

/// Rule on one case's observations.
///
/// Silence is never treated as success: an absent observation for a step
/// that was supposed to run is itself an [`Invariant::AlwaysAnswers`]
/// violation, so a harness bug (a dropped response, a timeout) surfaces as
/// a finding rather than as an accidental pass.
#[must_use]
pub fn referee_verdict(obs: &Observation) -> Verdict {
    let mut v = Verdict::default();

    // Liveness. A missing query answer means the tool never replied.
    match obs.query_ok {
        None => v.fail(
            Invariant::AlwaysAnswers,
            "query tool produced no response (panic, hang, or dropped frame)",
        ),
        Some(ok) => {
            // Fail-open: syntactically invalid input that reported success.
            if ok && obs.syntax_valid == Some(false) {
                v.fail(
                    Invariant::NeverFailOpen,
                    "query is syntactically invalid yet the tool reported ok:true",
                );
            }
            if ok {
                // Honest counting, checked against an independent recount
                // rather than against itself.
                if let (Some(reported), Some(independent)) =
                    (obs.reported_rows, obs.independent_rows)
                {
                    if reported != independent {
                        v.fail(
                            Invariant::HonestRowCount,
                            format!(
                                "reported row_count={reported} but an independent \
                                 COUNT(*) of the same query returned {independent}"
                            ),
                        );
                    }
                }
                // Truncation must be declared.
                if let (Some(reported), Some(returned), Some(truncated)) =
                    (obs.reported_rows, obs.returned_rows, obs.truncated)
                {
                    if returned < reported && !truncated {
                        v.fail(
                            Invariant::NeverSilentlyTruncates,
                            format!("returned {returned} of {reported} rows with truncated=false"),
                        );
                    }
                }
            }
        }
    }

    // Liveness for the dry-run tool: whenever the harness got far enough to
    // call apply at all (obs.applied_ok is Some, regardless of its value),
    // the dry run must have run first and produced an answer. A missing
    // dry_run_ok here means the dry-run tool's own Result went unobserved
    // -- exactly what `let _ = sync_dry_run(...)` used to do, silently.
    if obs.applied_ok.is_some() && obs.dry_run_ok.is_none() {
        v.fail(
            Invariant::AlwaysAnswers,
            "the template write succeeded and apply ran, but the dry-run tool \
             produced no response before it (panic, hang, or dropped frame)",
        );
    }

    // Containment: judged on what actually moved on disk, not on what the
    // tool said it wrote.
    if !obs.changed_outside_root.is_empty() {
        v.fail(
            Invariant::NeverEscapesRoot,
            format!(
                "{} path(s) changed outside the consumer root: {}",
                obs.changed_outside_root.len(),
                obs.changed_outside_root.join(", ")
            ),
        );
    }

    // Evidence: a successful write must be receipted.
    if obs.applied_ok == Some(true) && !obs.written.is_empty() {
        match obs.receipt_verified {
            Some(true) => {}
            Some(false) => v.fail(
                Invariant::WriteImpliesReceipt,
                format!(
                    "apply wrote {} file(s) but the receipt did not verify",
                    obs.written.len()
                ),
            ),
            None => v.fail(
                Invariant::WriteImpliesReceipt,
                format!(
                    "apply wrote {} file(s) and no receipt was found at all",
                    obs.written.len()
                ),
            ),
        }
        // Idempotence, only meaningful once a first apply succeeded --
        // but "the second apply never reported" is itself a violation, not
        // a licence to skip the check.
        match (obs.second_apply_written, obs.second_apply_error.as_deref()) {
            (Some(0), _) => {}
            (Some(n), _) => v.fail(
                Invariant::Idempotent,
                format!("re-syncing unchanged input wrote {n} more file(s)"),
            ),
            (None, Some(err)) => v.fail(
                Invariant::Idempotent,
                format!("re-syncing unchanged input failed: {err}"),
            ),
            (None, None) => v.fail(
                Invariant::Idempotent,
                "the second apply produced neither a count nor an error".to_string(),
            ),
        }

        // The dry run and the real apply ran against the SAME unmodified
        // template, back to back -- their write sets must agree. A dry run
        // that predicts a different set of files than apply actually wrote
        // means the "preview" tool cannot be trusted as a preview.
        if let Some(predicted) = &obs.dry_run_would_write {
            let actual: BTreeSet<String> = obs.written.iter().cloned().collect();
            if *predicted != actual {
                let apply_only: Vec<&String> = actual.difference(predicted).collect();
                let dry_run_only: Vec<&String> = predicted.difference(&actual).collect();
                v.fail(
                    Invariant::DryRunAgreesWithApply,
                    format!(
                        "dry run predicted {} file(s), apply actually wrote {}; \
                         apply wrote but dry run did not predict: {apply_only:?}; \
                         dry run predicted but apply did not write: {dry_run_only:?}",
                        predicted.len(),
                        actual.len(),
                    ),
                );
            }
        }
    }

    v
}

/// Recursive (relative path -> BLAKE3) fingerprint of a directory tree.
/// Shared by the harness so "did anything change" is answered by hashing
/// real bytes rather than by trusting mtimes or tool self-reports.
#[must_use]
pub fn fingerprint(root: &Path) -> std::collections::BTreeMap<String, String> {
    let mut out = std::collections::BTreeMap::new();
    walk(root, root, &mut out);
    out
}

fn walk(base: &Path, dir: &Path, out: &mut std::collections::BTreeMap<String, String>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            walk(base, &path, out);
        } else if let Ok(bytes) = std::fs::read(&path) {
            let rel = path
                .strip_prefix(base)
                .unwrap_or(&path)
                .display()
                .to_string();
            out.insert(rel, blake3::hash(&bytes).to_hex().to_string());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn silence_is_a_violation_not_a_pass() {
        let v = referee_verdict(&Observation::default());
        assert!(v.broken().contains(&Invariant::AlwaysAnswers));
    }

    #[test]
    fn invalid_syntax_reported_ok_is_fail_open() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(false),
            ..Default::default()
        };
        assert!(referee_verdict(&obs)
            .broken()
            .contains(&Invariant::NeverFailOpen));
    }

    #[test]
    fn dishonest_row_count_is_caught() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            reported_rows: Some(0),
            independent_rows: Some(113),
            ..Default::default()
        };
        // The literal shape of the incident this whole crate exists for.
        assert!(referee_verdict(&obs)
            .broken()
            .contains(&Invariant::HonestRowCount));
    }

    #[test]
    fn dropping_rows_without_saying_so_is_caught() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            reported_rows: Some(5000),
            returned_rows: Some(4096),
            truncated: Some(false),
            ..Default::default()
        };
        assert!(referee_verdict(&obs)
            .broken()
            .contains(&Invariant::NeverSilentlyTruncates));
    }

    #[test]
    fn writing_outside_root_is_caught() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            changed_outside_root: vec!["/etc/passwd".into()],
            ..Default::default()
        };
        assert!(referee_verdict(&obs)
            .broken()
            .contains(&Invariant::NeverEscapesRoot));
    }

    #[test]
    fn write_without_receipt_is_decorative_completion() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            applied_ok: Some(true),
            written: vec!["out/x.txt".into()],
            receipt_verified: None,
            ..Default::default()
        };
        assert!(referee_verdict(&obs)
            .broken()
            .contains(&Invariant::WriteImpliesReceipt));
    }

    #[test]
    fn non_idempotent_resync_is_caught() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            applied_ok: Some(true),
            written: vec!["out/x.txt".into()],
            receipt_verified: Some(true),
            second_apply_written: Some(3),
            ..Default::default()
        };
        assert!(referee_verdict(&obs)
            .broken()
            .contains(&Invariant::Idempotent));
    }

    /// The bug the vacuity audit found: a re-sync that ERRORS must be a
    /// violation. Previously this was silently skipped and reported clean.
    #[test]
    fn a_failing_second_apply_is_a_violation_not_a_skip() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            applied_ok: Some(true),
            written: vec!["out/x.txt".into()],
            receipt_verified: Some(true),
            second_apply_written: None,
            second_apply_error: Some("[FM-WRITE-005] differing content".into()),
            ..Default::default()
        };
        let v = referee_verdict(&obs);
        assert!(v.broken().contains(&Invariant::Idempotent));
        assert!(
            v.violations
                .iter()
                .any(|x| x.observed.contains("FM-WRITE-005")),
            "the underlying error must be reported, not swallowed: {:?}",
            v.violations
        );
    }

    /// Neither a count nor an error is the worst case: the harness lost
    /// track entirely. It must not read as success.
    #[test]
    fn a_silent_second_apply_is_a_violation() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            applied_ok: Some(true),
            written: vec!["out/x.txt".into()],
            receipt_verified: Some(true),
            second_apply_written: None,
            second_apply_error: None,
            ..Default::default()
        };
        assert!(referee_verdict(&obs)
            .broken()
            .contains(&Invariant::Idempotent));
    }

    /// The bug this pass found: `let _ = sync_dry_run(...)` discarded the
    /// dry run's Result entirely, so a dry-run tool that never answered
    /// (panic, hang, dropped frame) was indistinguishable from one that
    /// answered cleanly. Reaching apply at all (`applied_ok: Some(_)`)
    /// proves the harness got past the dry-run call site, so a missing
    /// `dry_run_ok` there is itself a liveness violation.
    #[test]
    fn a_missing_dry_run_answer_is_a_liveness_violation() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            applied_ok: Some(true),
            written: vec!["out/x.txt".into()],
            receipt_verified: Some(true),
            second_apply_written: Some(0),
            dry_run_ok: None,
            dry_run_would_write: None,
            ..Default::default()
        };
        assert!(referee_verdict(&obs)
            .broken()
            .contains(&Invariant::AlwaysAnswers));
    }

    /// The other half of the same gap: even when the dry run DID answer,
    /// nothing previously checked whether its prediction agreed with what
    /// apply actually wrote. A dry run that predicts a different file set
    /// than the real apply must be a finding.
    #[test]
    fn a_dry_run_that_disagrees_with_apply_is_caught() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            applied_ok: Some(true),
            written: vec!["out/x.txt".into()],
            receipt_verified: Some(true),
            second_apply_written: Some(0),
            dry_run_ok: Some(true),
            dry_run_would_write: Some(["out/wrong.txt".to_string()].into_iter().collect()),
            ..Default::default()
        };
        let v = referee_verdict(&obs);
        assert!(v.broken().contains(&Invariant::DryRunAgreesWithApply));
        assert!(
            v.violations
                .iter()
                .any(|x| x.observed.contains("out/x.txt") && x.observed.contains("out/wrong.txt")),
            "both the actually-written and only-predicted paths must be named: {:?}",
            v.violations
        );
    }

    /// A dry run that agrees exactly with what apply wrote must stay clean
    /// -- the new invariant must not fire on the lawful case.
    #[test]
    fn a_dry_run_that_agrees_with_apply_is_clean() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            applied_ok: Some(true),
            written: vec!["out/x.txt".into()],
            receipt_verified: Some(true),
            second_apply_written: Some(0),
            dry_run_ok: Some(true),
            dry_run_would_write: Some(["out/x.txt".to_string()].into_iter().collect()),
            ..Default::default()
        };
        let v = referee_verdict(&obs);
        assert!(
            !v.broken().contains(&Invariant::DryRunAgreesWithApply),
            "an agreeing dry run must not be flagged: {:?}",
            v.violations
        );
        assert!(
            !v.broken().contains(&Invariant::AlwaysAnswers),
            "a dry run that did answer must not be flagged as missing: {:?}",
            v.violations
        );
    }

    #[test]
    fn a_lawful_run_is_clean() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            reported_rows: Some(2),
            returned_rows: Some(2),
            truncated: Some(false),
            independent_rows: Some(2),
            applied_ok: Some(true),
            written: vec!["out/x.txt".into()],
            receipt_verified: Some(true),
            second_apply_written: Some(0),
            second_apply_error: None,
            changed_outside_root: Vec::new(),
            dry_run_ok: Some(true),
            dry_run_would_write: Some(["out/x.txt".to_string()].into_iter().collect()),
        };
        let v = referee_verdict(&obs);
        assert!(v.clean(), "expected no violations, got {:?}", v.violations);
    }

    /// A zero-row query is lawful, not a violation — the distinction the
    /// whole crate turns on. Zero rows honestly reported is a correct
    /// answer; zero rows silently substituted for 113 is the bug.
    #[test]
    fn honest_zero_rows_is_lawful() {
        let obs = Observation {
            query_ok: Some(true),
            syntax_valid: Some(true),
            reported_rows: Some(0),
            returned_rows: Some(0),
            truncated: Some(false),
            independent_rows: Some(0),
            ..Default::default()
        };
        assert!(referee_verdict(&obs).clean());
    }
}
