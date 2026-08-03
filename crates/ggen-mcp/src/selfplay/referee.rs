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
    pub second_apply_written: Option<usize>,
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
        // Idempotence, only meaningful once a first apply succeeded.
        if let Some(second) = obs.second_apply_written {
            if second > 0 {
                v.fail(
                    Invariant::Idempotent,
                    format!("re-syncing unchanged input wrote {second} more file(s)"),
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
            changed_outside_root: Vec::new(),
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
