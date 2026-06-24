//! Andon-signal remediation decision logic (Lever 3).
//!
//! Parses the `andon_signal` JSON field from `SyncResult` and decides what
//! the dispatch pipeline should do without blocking the dispatch loop.
//! Red signals suppress the push and emit an OCEL `dispatch:needs-remediation`
//! event; yellow signals warn but continue; green/absent proceed normally.

use serde_json::Value;

/// What the dispatch pipeline should do after detecting an andon signal.
#[derive(Debug, PartialEq)]
pub enum RemediationDecision {
    /// Signal is green, absent, or unrecognised — proceed normally.
    Proceed,
    /// Signal is yellow — log a warning but still proceed with the push.
    Warn { message: String },
    /// Signal is red — human remediation required; suppress the push.
    NeedsHuman { code: String, steps: Vec<String> },
}

/// Inspect the `andon_signal` JSON emitted by `SyncExecutor` and decide what
/// the dispatch pipeline should do.
///
/// JSON structure (serde `tag = "level"` with inner fields at top level):
/// - Red:    `{"level":"red",    "code":"...", "message":"...", "recovery_steps":[...]}`
/// - Yellow: `{"level":"yellow", "code":"...", "message":"...", "suggestion":"..."}`
/// - Green:  `{"level":"green"}`
pub fn decide(andon: &Value) -> RemediationDecision {
    let level = andon
        .get("level")
        .and_then(|v| v.as_str())
        .unwrap_or("green");

    match level {
        "red" => {
            let code = andon
                .get("code")
                .and_then(|v| v.as_str())
                .unwrap_or("UNKNOWN")
                .to_owned();
            let steps: Vec<String> = andon
                .get("recovery_steps")
                .and_then(|v| v.as_array())
                .map(|arr| {
                    arr.iter()
                        .filter_map(|s| s.as_str().map(|s| s.to_owned()))
                        .collect()
                })
                .unwrap_or_default();
            RemediationDecision::NeedsHuman { code, steps }
        }
        "yellow" => {
            let message = andon
                .get("message")
                .and_then(|v| v.as_str())
                .unwrap_or("yellow andon signal")
                .to_owned();
            RemediationDecision::Warn { message }
        }
        _ => RemediationDecision::Proceed,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn red_andon_returns_needs_human_with_code_and_steps() {
        let andon = json!({
            "level": "red",
            "code": "MANIFEST_INVALID",
            "message": "ggen.toml is missing [ontology] section",
            "context": "file: ggen.toml",
            "recovery_steps": ["Open ggen.toml", "Add [ontology] section"],
            "documentation_link": "https://ggen.dev/docs"
        });
        match decide(&andon) {
            RemediationDecision::NeedsHuman { code, steps } => {
                assert_eq!(code, "MANIFEST_INVALID");
                assert_eq!(steps, vec!["Open ggen.toml", "Add [ontology] section"]);
            }
            other => panic!("expected NeedsHuman, got {:?}", other),
        }
    }

    #[test]
    fn yellow_andon_returns_warn_with_message() {
        let andon = json!({
            "level": "yellow",
            "code": "UNUSED_FILES",
            "message": "3 files are unused",
            "suggestion": "Remove unused templates"
        });
        match decide(&andon) {
            RemediationDecision::Warn { message } => {
                assert!(message.contains("unused"), "message: {}", message);
            }
            other => panic!("expected Warn, got {:?}", other),
        }
    }

    #[test]
    fn green_andon_returns_proceed() {
        let andon = json!({"level": "green"});
        assert_eq!(decide(&andon), RemediationDecision::Proceed);
    }

    #[test]
    fn missing_level_defaults_to_proceed() {
        let andon = json!({"code": "SOME_CODE", "message": "something"});
        assert_eq!(decide(&andon), RemediationDecision::Proceed);
    }

    #[test]
    fn red_with_no_steps_returns_needs_human_with_empty_steps() {
        let andon = json!({"level": "red", "code": "ERR_001"});
        match decide(&andon) {
            RemediationDecision::NeedsHuman { code, steps } => {
                assert_eq!(code, "ERR_001");
                assert!(steps.is_empty());
            }
            other => panic!("expected NeedsHuman, got {:?}", other),
        }
    }

    #[test]
    fn red_with_non_string_steps_filtered_out() {
        let andon = json!({
            "level": "red",
            "code": "ERR",
            "recovery_steps": ["step one", 42, null, "step two"]
        });
        match decide(&andon) {
            RemediationDecision::NeedsHuman { steps, .. } => {
                assert_eq!(steps, vec!["step one", "step two"]);
            }
            other => panic!("expected NeedsHuman, got {:?}", other),
        }
    }
}
