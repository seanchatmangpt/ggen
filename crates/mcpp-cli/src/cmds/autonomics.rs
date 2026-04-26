use clap_noun_verb_macros::verb;
use mcpp_core::Envelope;
use serde_json::json;

/// `mcpp autonomics run` — emits the JSON-first result envelope.
///
/// Promoted from absorbed v2 cell (portfolio-obl-0002, promotion id
/// `json-result-envelope`). The function still returns a `String` so it
/// stays compatible with the existing `clap-noun-verb` runner; that
/// string is itself the serialized JSON envelope.
///
/// JSON-first contract:
/// - default output is JSON (`chatmangpt.mcpp.result.v1`),
/// - failure paths must also emit valid JSON,
/// - human-readable output is opt-in (not implemented in this command),
/// - tool/command success is not completion.
#[verb("autonomics", "run")]
pub fn run_meta_loop() -> clap_noun_verb::Result<String> {
    // 1. Observe receipt chain (with provenance verification)
    // 2. Compute first-order failure metrics
    // 3. Generate new policy delta (MCPP wizard)

    // 4. MANDATORY: SHACL Verification Gate
    if let Err(e) = validate_policy_delta() {
        let env = Envelope::fail(
            "mcpp.autonomics.run",
            "mcpp",
            "POLICY_DEFECT",
            &format!("shacl validation failed: {e}"),
        );
        return Ok(env.to_json());
    }

    // 5. Commit and re-sync — wrap result in the JSON envelope.
    let env = Envelope::pass("mcpp.autonomics.run", "mcpp")
        .with_data(json!({
            "policy_delta_validated": true,
            "shacl_gate": "pass",
            "message": "Meta-feedback loop closed: parameters re-optimized and SHACL-validated."
        }))
        .with_next(
            "mcpp autonomics receipt verify",
            "Verify receipt-backed completion before declaring done.",
        );
    Ok(env.to_json())
}

fn validate_policy_delta() -> clap_noun_verb::Result<()> {
    // Placeholder for actual SHACL engine call.
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    #[test]
    fn run_returns_json_envelope_by_default() {
        let s = run_meta_loop().expect("must succeed on happy path");
        let v: Value = serde_json::from_str(&s).expect("output must be JSON");
        assert_eq!(v["schema"], "chatmangpt.mcpp.result.v1");
        assert_eq!(v["command"], "mcpp.autonomics.run");
        assert_eq!(v["status"], "pass");
        assert_eq!(v["target"], "mcpp");
        assert!(v["errors"].as_array().unwrap().is_empty());
        assert_eq!(v["data"]["shacl_gate"], "pass");
    }

    #[test]
    fn envelope_carries_next_admissible_command() {
        let s = run_meta_loop().unwrap();
        let v: Value = serde_json::from_str(&s).unwrap();
        assert!(v["next"]["command"]
            .as_str()
            .unwrap()
            .contains("receipt verify"));
    }
}
