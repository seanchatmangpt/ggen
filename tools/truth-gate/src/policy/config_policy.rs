/// config_policy: blocks weakening of enforcement settings.
///
/// An agent that can disable hooks can bypass all other checks.
/// Config drift is silent and cumulative — gate it structurally.
use super::Violation;
use serde_json::Value;

pub fn check(tool_input: &Option<Value>, file_path: &str) -> Vec<Violation> {
    let mut violations = vec![];

    let input = match tool_input {
        Some(v) => v,
        None => return violations,
    };

    // Check if hooks are being removed or disabled
    if let Some(new_content) = input
        .get("new_string")
        .or_else(|| input.get("content"))
        .or_else(|| input.get("CodeContent"))
        .or_else(|| input.get("codeContent"))
        .or_else(|| input.get("ReplacementContent"))
        .or_else(|| input.get("replacementContent"))
    {
        if let Some(content_str) = new_content.as_str() {
            violations.extend(check_settings_content(content_str, file_path));
        }
    }

    // Handle multi_replace_file_content chunks
    if let Some(chunks) = input
        .get("ReplacementChunks")
        .or_else(|| input.get("replacementChunks"))
        .and_then(|c| c.as_array())
    {
        for chunk in chunks {
            if let Some(chunk_content) = chunk
                .get("ReplacementContent")
                .or_else(|| chunk.get("replacementContent"))
                .and_then(|v| v.as_str())
            {
                violations.extend(check_settings_content(chunk_content, file_path));
            }
        }
    }

    violations
}

fn check_settings_content(content: &str, file_path: &str) -> Vec<Violation> {
    let mut violations = vec![];
    let lower = content.to_lowercase();
    let file_path_lower = file_path.to_lowercase();
    let is_gemini = file_path_lower.contains(".gemini") || lower.contains("beforetool") || lower.contains("aftertool");

    let default_location = if is_gemini {
        ".gemini/settings.json"
    } else {
        ".claude/settings.json"
    };

    let location = if file_path.is_empty() {
        default_location
    } else {
        file_path
    };

    // Detect addition of prompt/agent hooks for policy (re-introduces AI judgment layer)
    if lower.contains("\"type\": \"prompt\"") || lower.contains("\"type\":\"prompt\"") {
        violations.push(Violation {
            pattern: "type: prompt".to_string(),
            location: location.to_string(),
            rule: "prompt hooks re-introduce an AI judgment layer for policy — use command hooks with Rust binary only".to_string(),
        });
    }
    if lower.contains("\"type\": \"agent\"") || lower.contains("\"type\":\"agent\"") {
        violations.push(Violation {
            pattern: "type: agent".to_string(),
            location: location.to_string(),
            rule: "agent hooks re-introduce an AI judgment layer for policy — use command hooks with Rust binary only".to_string(),
        });
    }

    // Detect removal of enforcement hooks.
    // If the content looks like a settings file (has "hooks" key) but the enforcement hooks
    // are missing, block the write.
    if lower.contains("\"hooks\"") {
        if is_gemini {
            let has_before = lower.contains("beforetool");
            let has_after = lower.contains("aftertool");
            let has_truth_gate = lower.contains("truth-gate");

            if !has_before {
                violations.push(Violation {
                    pattern: "missing BeforeTool".to_string(),
                    location: location.to_string(),
                    rule: "BeforeTool hook removed — enforcement gate must remain active; restore the truth-gate BeforeTool hook".to_string(),
                });
            }
            if !has_after {
                violations.push(Violation {
                    pattern: "missing AfterTool".to_string(),
                    location: location.to_string(),
                    rule: "AfterTool hook removed — enforcement gate must remain active; restore the truth-gate AfterTool hook".to_string(),
                });
            }
            if has_before && has_after && !has_truth_gate {
                violations.push(Violation {
                    pattern: "truth-gate not referenced".to_string(),
                    location: location.to_string(),
                    rule: "Hooks present but truth-gate binary not referenced — enforcement has been replaced; restore truth-gate command".to_string(),
                });
            }
        } else {
            let has_pre = lower.contains("pretooluse");
            let has_post = lower.contains("posttooluse");
            let has_truth_gate = lower.contains("truth-gate");

            if !has_pre {
                violations.push(Violation {
                    pattern: "missing PreToolUse".to_string(),
                    location: location.to_string(),
                    rule: "PreToolUse hook removed — enforcement gate must remain active; restore the truth-gate PreToolUse hook".to_string(),
                });
            }
            if !has_post {
                violations.push(Violation {
                    pattern: "missing PostToolUse".to_string(),
                    location: location.to_string(),
                    rule: "PostToolUse hook removed — enforcement gate must remain active; restore the truth-gate PostToolUse hook".to_string(),
                });
            }
            if has_pre && has_post && !has_truth_gate {
                violations.push(Violation {
                    pattern: "truth-gate not referenced".to_string(),
                    location: location.to_string(),
                    rule: "Hooks present but truth-gate binary not referenced — enforcement has been replaced; restore truth-gate command".to_string(),
                });
            }
        }
    }

    violations
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_gemini_settings_valid() {
        let input = json!({
            "content": r#"{
                "hooks": {
                    "BeforeTool": [
                        {
                            "matcher": "replace|write_file",
                            "hooks": [
                                {
                                    "name": "truth-gate-guard",
                                    "type": "command",
                                    "command": "./tools/truth-gate/target/release/truth-gate"
                                }
                            ]
                        }
                    ],
                    "AfterTool": [
                        {
                            "matcher": "replace|write_file",
                            "hooks": [
                                {
                                    "name": "truth-gate-emitter",
                                    "type": "command",
                                    "command": "./tools/truth-gate/target/release/truth-gate"
                                }
                            ]
                        }
                    ]
                }
            }"#
        });
        let violations = check(&Some(input), ".gemini/settings.json");
        assert!(violations.is_empty());
    }

    #[test]
    fn test_gemini_settings_missing_before() {
        let input = json!({
            "content": r#"{
                "hooks": {
                    "AfterTool": [
                        {
                            "matcher": "replace|write_file",
                            "hooks": [
                                {
                                    "name": "truth-gate-emitter",
                                    "type": "command",
                                    "command": "./tools/truth-gate/target/release/truth-gate"
                                }
                            ]
                        }
                    ]
                }
            }"#
        });
        let violations = check(&Some(input), ".gemini/settings.json");
        assert!(!violations.is_empty());
        assert_eq!(violations[0].pattern, "missing BeforeTool");
        assert_eq!(violations[0].location, ".gemini/settings.json");
    }

    #[test]
    fn test_gemini_settings_prompt_hook() {
        let input = json!({
            "content": r#"{
                "hooks": {
                    "BeforeTool": [
                        {
                            "matcher": "replace|write_file",
                            "hooks": [
                                {
                                    "name": "truth-gate-guard",
                                    "type": "prompt",
                                    "prompt": "do something"
                                }
                            ]
                        }
                    ],
                    "AfterTool": [
                        {
                            "matcher": "replace|write_file",
                            "hooks": [
                                {
                                    "name": "truth-gate-emitter",
                                    "type": "command",
                                    "command": "./tools/truth-gate/target/release/truth-gate"
                                }
                            ]
                        }
                    ]
                }
            }"#
        });
        let violations = check(&Some(input), ".gemini/settings.json");
        assert!(!violations.is_empty());
        assert_eq!(violations[0].pattern, "type: prompt");
    }
}
