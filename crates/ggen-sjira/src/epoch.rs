//! Admission-side epoch gate — native Rust port of
//! `GgenIgniter.SemanticJira.EpochPlan` (`epoch_plan.ex`).
//!
//! Refuses a WorkOrder candidate whose PLAN would carry pre-watermark
//! implementation into a post-watermark epoch, before any construction
//! happens. This is the cheap refusal direction of the epoch boundary law
//! `Admit(f) ⟺ FreshEpoch(f)`; the tree-side proof (`EpochFreshness.check`)
//! is the expensive direction and lives in the presenting system — promotion
//! requires both.
//!
//! ## Opt-in, two levels (no-regression contract)
//!
//! The gate applies ONLY to candidates carrying a nonempty `"epoch"` value:
//! the candidate opts itself in, and the run supplies the stamped watermark.
//! An epoch-carrying candidate with no (or malformed) watermark is refused
//! [`Refusal::WatermarkUnavailable`] — fail closed — while candidates without
//! `"epoch"` behave byte-identically to a run without the gate.
//!
//! IO-free. `#![forbid(unsafe_code)]` (crate-level).

use serde_json::Value;

/// One refusal code of the epoch plan gate.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum Refusal {
    #[error("EPOCH_LEGACY_EDIT")]
    LegacyEdit,
    #[error("EPOCH_UNATTRIBUTED_IMPLEMENTATION")]
    UnattributedImplementation,
    #[error("EPOCH_PLAN_REUSES_PRE_WATERMARK_ARTIFACT")]
    ReusesPreWatermarkArtifact,
    #[error("EPOCH_WATERMARK_UNAVAILABLE")]
    WatermarkUnavailable,
}

/// Judges one decoded candidate (`order`) against a stamped watermark
/// (`decoded watermark.json`), or `None` when no manifest is available.
///
/// `Ok(())` when the gate does not apply (no `"epoch"`) or the plan is
/// lawful; `Err(Refusal)` otherwise. Violation precedence is the rule order
/// below: legacy edit, then unattributed implementation, then pre-watermark
/// source reuse.
pub fn check(order: &Value, watermark: Option<&Value>) -> Result<(), Refusal> {
    let Some(epoch) = order.get("epoch").and_then(|v| v.as_str()) else {
        return Ok(());
    };
    if epoch.is_empty() {
        return Ok(());
    }

    let Some(wm) = watermark else {
        return Err(Refusal::WatermarkUnavailable);
    };
    let Some(files) = wm.get("files").and_then(|v| v.as_array()) else {
        return Err(Refusal::WatermarkUnavailable);
    };

    let plan_touches = listify(order.get("plan_touches"));
    let manufacture_plan = order
        .get("manufacture_plan")
        .and_then(|v| v.as_object())
        .cloned()
        .unwrap_or_default();
    let source_artifacts = listify(order.get("source_artifacts"));
    let legacy_paths: std::collections::BTreeSet<&str> = files
        .iter()
        .filter_map(|f| f.get("path").and_then(|p| p.as_str()))
        .collect();
    let matchers: Vec<GlobMatcher> = impl_globs(wm).iter().map(|g| glob_matcher(g)).collect();

    if plan_touches
        .iter()
        .any(|p| legacy_paths.contains(p.as_str()) && !fresh_plan(p, &manufacture_plan))
    {
        return Err(Refusal::LegacyEdit);
    }
    if plan_touches.iter().any(|p| {
        !legacy_paths.contains(p.as_str())
            && matchers.iter().any(|m| m.matches(p))
            && !fresh_plan(p, &manufacture_plan)
    }) {
        return Err(Refusal::UnattributedImplementation);
    }
    if source_artifacts
        .iter()
        .any(|p| legacy_paths.contains(p.as_str()) && !fresh_plan(p, &manufacture_plan))
    {
        return Err(Refusal::ReusesPreWatermarkArtifact);
    }
    Ok(())
}

/// The typed refusal string a code renders as in the admit verdict line.
pub fn refusal_code_string(code: &Refusal) -> String {
    code.to_string()
}

// ── plan normalization ────────────────────────────────────────────────────

fn listify(value: Option<&Value>) -> Vec<String> {
    match value {
        None | Some(Value::Null) => vec![],
        Some(Value::String(s)) => vec![s.clone()],
        Some(Value::Array(items)) => items
            .iter()
            .filter_map(|v| v.as_str().map(str::to_string))
            .collect(),
        Some(_) => vec![],
    }
}

fn fresh_plan(path: &str, manufacture_plan: &serde_json::Map<String, Value>) -> bool {
    const FRESH_PLANS: [&str; 2] = ["generated", "residue"];
    manufacture_plan
        .get(path)
        .and_then(|v| v.as_str())
        .map(|p| FRESH_PLANS.contains(&p))
        .unwrap_or(false)
}

// ── implementation-plane globs ────────────────────────────────────────────

/// The watermark's implementation-plane glob set: the multi-plane
/// `implementation_globs` list when present, else the historical single
/// `implementation_glob`, UNION the default set — the same recorded-∪-default
/// semantics the court applies, so a plan line can never pass admission and
/// then be refused by the tree check. Glob dialect: `**` crosses directories
/// (and `**/` may match none — `lib/**/*.ex` includes `lib/foo.ex`), `*`
/// stays within one, `?` is one non-slash character.
fn impl_globs(watermark: &Value) -> Vec<String> {
    const DEFAULT_GLOBS: [&str; 3] = [
        "lib/**/*.ex",
        "config/**/*.exs",
        "priv/repo/migrations/**/*.exs",
    ];
    let mut globs: Vec<String> = if let Some(list) = watermark
        .get("implementation_globs")
        .and_then(|v| v.as_array())
    {
        list.iter()
            .filter_map(|v| v.as_str().map(str::to_string))
            .collect()
    } else if let Some(g) = watermark.get("implementation_glob").and_then(|v| v.as_str()) {
        vec![g.to_string()]
    } else {
        vec![]
    };
    for d in DEFAULT_GLOBS {
        if !globs.iter().any(|g| g == d) {
            globs.push(d.to_string());
        }
    }
    globs
}

/// Compiled glob matcher with the court's dialect (`**` crosses `/`).
pub struct GlobMatcher {
    tokens: Vec<Tok>,
}

enum Tok {
    DStarSlash,
    DStar,
    Star,
    QMark,
    Lit(char),
}

/// Compiles a glob string into a matcher.
pub fn glob_matcher(glob: &str) -> GlobMatcher {
    let chars: Vec<char> = glob.chars().collect();
    let mut tokens = Vec::new();
    let mut i = 0;
    while i < chars.len() {
        match chars[i] {
            '*' if i + 1 < chars.len() && chars[i + 1] == '*' => {
                if i + 2 < chars.len() && chars[i + 2] == '/' {
                    tokens.push(Tok::DStarSlash);
                    i += 3;
                } else {
                    tokens.push(Tok::DStar);
                    i += 2;
                }
            }
            '*' => {
                tokens.push(Tok::Star);
                i += 1;
            }
            '?' => {
                tokens.push(Tok::QMark);
                i += 1;
            }
            c => {
                tokens.push(Tok::Lit(c));
                i += 1
            }
        }
    }
    GlobMatcher { tokens }
}

impl GlobMatcher {
    pub fn matches(&self, path: &str) -> bool {
        let path_chars: Vec<char> = path.chars().collect();
        match_from(&self.tokens, 0, &path_chars, 0)
    }
}

fn match_from(toks: &[Tok], ti: usize, path: &[char], pi: usize) -> bool {
    if ti == toks.len() {
        return pi == path.len();
    }
    match &toks[ti] {
        Tok::Lit(c) => pi < path.len() && path[pi] == *c && match_from(toks, ti + 1, path, pi + 1),
        Tok::QMark => {
            pi < path.len() && path[pi] != '/' && match_from(toks, ti + 1, path, pi + 1)
        }
        Tok::Star => {
            let mut k = pi;
            loop {
                if match_from(toks, ti + 1, path, k) {
                    return true;
                }
                if k >= path.len() || path[k] == '/' {
                    return false;
                }
                k += 1;
            }
        }
        Tok::DStar => {
            for k in pi..=path.len() {
                if match_from(toks, ti + 1, path, k) {
                    return true;
                }
            }
            false
        }
        Tok::DStarSlash => {
            // `(?:.*/)?` — may match nothing, else whole directories.
            if match_from(toks, ti + 1, path, pi) {
                return true;
            }
            for k in (pi + 1)..=path.len() {
                if path[k - 1] == '/' && match_from(toks, ti + 1, path, k) {
                    return true;
                }
            }
            false
        }
    }
}
