/// config_policy: blocks weakening of enforcement settings.
///
/// An agent that can disable hooks can bypass all other checks.
/// Config drift is silent and cumulative — gate it structurally.
use super::Violation;
use serde_json::Value;

pub fn check(tool_input: &Option<Value>) -> Vec<Violation> {
    let mut violations = vec![];

    let input = match tool_input {
        Some(v) => v,
        None => return violations,
    };

    // Check if hooks are being removed or disabled
    if let Some(new_content) = input.get("new_string").or_else(|| input.get("content")) {
        if let Some(content_str) = new_content.as_str() {
            violations.extend(check_settings_content(content_str));
        }
    }

    violations
}

fn check_settings_content(content: &str) -> Vec<Violation> {
    let mut violations = vec![];

    // Detect addition of prompt/agent hooks for policy (re-introduces AI judgment layer)
    let lower = content.to_lowercase();
    if lower.contains("\"type\": \"prompt\"") || lower.contains("\"type\":\"prompt\"") {
        violations.push(Violation {
            pattern: "type: prompt".to_string(),
            location: ".claude/settings.json".to_string(),
            rule: "prompt hooks re-introduce an AI judgment layer for policy — use command hooks with Rust binary only".to_string(),
        });
    }
    if lower.contains("\"type\": \"agent\"") || lower.contains("\"type\":\"agent\"") {
        violations.push(Violation {
            pattern: "type: agent".to_string(),
            location: ".claude/settings.json".to_string(),
            rule: "agent hooks re-introduce an AI judgment layer for policy — use command hooks with Rust binary only".to_string(),
        });
    }

    // Detect removal of enforcement hooks.
    // If the content looks like a settings file (has "hooks" key) but the PreToolUse
    // and PostToolUse enforcement is missing, block the write.
    if lower.contains("\"hooks\"") {
        let has_pre = lower.contains("pretooluse");
        let has_post = lower.contains("posttooluse");
        let has_truth_gate = lower.contains("truth-gate");

        if !has_pre {
            violations.push(Violation {
                pattern: "missing PreToolUse".to_string(),
                location: ".claude/settings.json".to_string(),
                rule: "PreToolUse hook removed — enforcement gate must remain active; restore the truth-gate PreToolUse hook".to_string(),
            });
        }
        if !has_post {
            violations.push(Violation {
                pattern: "missing PostToolUse".to_string(),
                location: ".claude/settings.json".to_string(),
                rule: "PostToolUse hook removed — enforcement gate must remain active; restore the truth-gate PostToolUse hook".to_string(),
            });
        }
        if has_pre && has_post && !has_truth_gate {
            violations.push(Violation {
                pattern: "truth-gate not referenced".to_string(),
                location: ".claude/settings.json".to_string(),
                rule: "Hooks present but truth-gate binary not referenced — enforcement has been replaced; restore truth-gate command".to_string(),
            });
        }
    }

    violations
}
