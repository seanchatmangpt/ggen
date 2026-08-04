//! A self-play *case*: one adversarial move against one pack.
//!
//! Cases are the corpus format. They are deliberately small and fully
//! declarative — everything needed to reproduce a finding, and nothing
//! else — so that a case committed today still replays years from now
//! without depending on the generator that found it.

use serde::{Deserialize, Serialize};

/// Where a case came from. Recorded because provenance changes how a
/// finding is read: a hand-written case encodes a human's intent, a
/// Gemma-generated one encodes only "this tripped the referee", and
/// conflating the two would let an LLM's guess masquerade as a
/// deliberate specification.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CaseOrigin {
    /// Written by hand to pin a specific known failure mode.
    Handwritten,
    /// Emitted by the local Gemma explorer and kept because it tripped an
    /// invariant. The model's *reasoning* is not evidence; only the
    /// referee's verdict on the resulting bytes is.
    Gemma,
    /// Derived by minimizing another case while preserving its violation.
    Minimized,
}

/// One playable case.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Case {
    /// Stable identifier; also the corpus filename stem.
    pub id: String,
    /// Pack directory name under `packs/` whose ontology is the board.
    /// A case is meaningless without the graph it was written against.
    pub pack: String,
    /// SPARQL executed against the pack's loaded graph.
    pub sparql: String,
    /// Frontmatter `to:` for the template under test.
    pub to: String,
    /// Tera template body.
    pub body: String,
    pub origin: CaseOrigin,
    /// Which invariant this case was kept for. `None` for cases retained
    /// as passing baselines (a corpus of only-failures would lose the
    /// ability to detect a regression that starts rejecting valid input).
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub expected_violation: Option<String>,
    /// Free-text provenance: what this case is probing, in one line.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub note: Option<String>,
}

impl Case {
    /// Render this case's template file content (frontmatter + body) exactly
    /// as it would appear on disk in a real consumer project.
    ///
    /// Uses the same `---` fenced-YAML shape `Template::parse` expects, so a
    /// case exercises the real parser rather than a test-only construction
    /// path.
    #[must_use]
    pub fn template_file(&self) -> String {
        // Block scalar for the query so arbitrary SPARQL (colons, quotes,
        // braces, newlines) survives YAML without the case format having to
        // escape it — the generator emits hostile text on purpose.
        let indented: String = self
            .sparql
            .lines()
            .map(|l| format!("    {l}\n"))
            .collect::<String>();
        format!(
            "---\nto: {}\nsparql:\n  probe: |\n{}---\n{}",
            self.to, indented, self.body
        )
    }
}
