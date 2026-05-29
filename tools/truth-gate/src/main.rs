/// truth-gate — CodeManufactory structural enforcement gate.
///
/// Modes:
///   (stdin)   Claude Code hook mode: reads JSON from stdin, exits 0/2
///   --ci      CI scan mode: walks src/ + tests/, reports all violations
///
/// Exit codes:
///   0 = allow / clean
///   2 = block / violations found
use std::io::{self, Read};
use std::path::Path;

use serde::{Deserialize, Serialize};
use serde_json::Value;

mod policy;
use policy::{
    config_policy, evidence_policy, test_policy, Violation,
};

// ─── Hook input ──────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
struct HookInput {
    hook_event_name: Option<String>,
    tool_name: Option<String>,
    tool_input: Option<Value>,
    /// Present on FileChanged and ConfigChange
    file_path: Option<String>,
    cwd: Option<String>,
}

// ─── Hook output (stdout JSON) ────────────────────────────────────────────────

#[derive(Debug, Serialize)]
struct HookOutput {
    decision: &'static str,
    #[serde(rename = "hookSpecificOutput")]
    hook_specific_output: HookSpecificOutput,
}

#[derive(Debug, Serialize)]
struct HookSpecificOutput {
    #[serde(rename = "hookEventName")]
    hook_event_name: String,
    #[serde(rename = "additionalContext")]
    additional_context: String,
}

// ─── CI scan mode ────────────────────────────────────────────────────────────

/// Run a full or targeted scan.
///
/// `paths` — if empty, scan `src/` + `tests/` in cwd.
///           if non-empty, scan exactly those files/dirs.
fn ci_scan(paths: Vec<String>) -> i32 {
    let mut all_violations: Vec<(String, Violation)> = vec![];

    if paths.is_empty() {
        let cwd = std::env::current_dir().unwrap_or_default();
        for root in &["src", "tests"] {
            let dir = cwd.join(root);
            if dir.exists() {
                collect_violations(&dir, &mut all_violations);
            }
        }
    } else {
        for path in &paths {
            let p = Path::new(path);
            if p.is_dir() {
                collect_violations(p, &mut all_violations);
            } else if let Ok(content) = std::fs::read_to_string(p) {
                let path_str = p.to_string_lossy().to_string();
                if is_python_source(&path_str) {
                    for v in test_policy::check(&content, &path_str) {
                        all_violations.push((path_str.clone(), v));
                    }
                    for v in evidence_policy::check(&content, &path_str) {
                        all_violations.push((path_str.clone(), v));
                    }
                }
            }
        }
    }

    if all_violations.is_empty() {
        println!("truth-gate: OK — no violations found");
        return 0;
    }

    eprintln!("truth-gate: BLOCKED — {} violation(s) found\n", all_violations.len());
    for (file, v) in &all_violations {
        eprintln!("  • {}\n    @ {}:{}\n    {}\n", v.pattern, file, v.location, v.rule);
    }
    eprintln!("The easiest way to pass tests must be doing the real thing.");
    2
}

fn collect_violations(dir: &Path, out: &mut Vec<(String, Violation)>) {
    let entries = match std::fs::read_dir(dir) {
        Ok(e) => e,
        Err(_) => return,
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect_violations(&path, out);
        } else {
            let path_str = path.to_string_lossy().to_string();

            if path.extension().and_then(|e| e.to_str()) == Some("py") {
                if !is_python_source(&path_str) {
                    continue;
                }
                if let Ok(content) = std::fs::read_to_string(&path) {
                    for v in test_policy::check(&content, &path_str) {
                        out.push((path_str.clone(), v));
                    }
                    for v in evidence_policy::check(&content, &path_str) {
                        out.push((path_str.clone(), v));
                    }
                }
            } else if should_scan_non_python(&path_str) {
                // Scan high-risk non-Python files for TODO/FIXME patterns only
                if let Ok(content) = std::fs::read_to_string(&path) {
                    for v in test_policy::check(&content, &path_str) {
                        out.push((path_str.clone(), v));
                    }
                }
            }
        }
    }
}

// ─── Entry point ─────────────────────────────────────────────────────────────

fn main() {
    // CI scan mode: truth-gate --ci [file1.py file2.py ...]
    if std::env::args().any(|a| a == "--ci") {
        let extra_paths: Vec<String> = std::env::args()
            .skip(1)
            .filter(|a| a != "--ci")
            .collect();
        std::process::exit(ci_scan(extra_paths));
    }

    let mut raw = String::new();
    io::stdin()
        .read_to_string(&mut raw)
        .expect("truth-gate: failed to read stdin");

    let hook: HookInput = match serde_json::from_str(&raw) {
        Ok(h) => h,
        Err(_) => {
            // Unparseable input — allow and stay out of the way
            std::process::exit(0);
        }
    };

    let event = hook.hook_event_name.as_deref().unwrap_or("");
    let tool = hook.tool_name.as_deref().unwrap_or("");

    let violations: Vec<Violation> = match event {
        "PreToolUse" | "BeforeTool" => check_pre_tool_use(tool, &hook.tool_input, &hook.file_path),
        "PostToolUse" | "AfterTool" => check_post_tool_use(tool, &hook.tool_input, &hook.file_path),
        "ConfigChange" => config_policy::check(&hook.tool_input, hook.file_path.as_deref().unwrap_or("")),
        "FileChanged" => {
            let file_path = hook.file_path.as_deref().unwrap_or("");
            if file_path.ends_with("settings.json") && (file_path.contains(".claude") || file_path.contains(".gemini")) {
                if let Ok(content) = std::fs::read_to_string(file_path) {
                    let wrapped = serde_json::json!({ "content": content });
                    config_policy::check(&Some(wrapped), file_path)
                } else {
                    vec![]
                }
            } else if !is_python_source(file_path) {
                vec![]
            } else if let Ok(content) = std::fs::read_to_string(file_path) {
                let mut violations = vec![];
                violations.extend(test_policy::check(&content, file_path));
                violations.extend(evidence_policy::check(&content, file_path));
                violations
            } else {
                vec![]
            }
        }
        _ => vec![],
    };

    if violations.is_empty() {
        std::process::exit(0);
    }

    let reasons: Vec<String> = violations
        .iter()
        .map(|v| format!("  • {}\n    @ {}\n    {}", v.pattern, v.location, v.rule))
        .collect();

    let message = format!(
        "truth-gate: BLOCKED — real execution required\n\n{}\n\nThe easiest way to pass tests must be doing the real thing.\nRemove fake evidence paths and cross real boundaries instead.",
        reasons.join("\n\n")
    );

    // stderr for Claude's in-context explanation
    eprintln!("{}", message);

    // stdout structured JSON for Claude Code hook protocol
    let output = HookOutput {
        decision: "block",
        hook_specific_output: HookSpecificOutput {
            hook_event_name: event.to_string(),
            additional_context: message.clone(),
        },
    };
    println!("{}", serde_json::to_string(&output).unwrap_or_default());

    std::process::exit(2);
}

// ─── PreToolUse ──────────────────────────────────────────────────────────────

fn check_pre_tool_use(
    tool: &str,
    input: &Option<Value>,
    _file_path: &Option<String>,
) -> Vec<Violation> {
    if !matches!(
        tool,
        "Edit"
            | "Write"
            | "replace_file_content"
            | "write_to_file"
            | "multi_replace_file_content"
            | "replace"
            | "write_file"
    ) {
        return vec![];
    }

    let input = match input {
        Some(v) => v,
        None => return vec![],
    };

    let file_path = input
        .get("file_path")
        .or_else(|| input.get("TargetFile"))
        .or_else(|| input.get("targetFile"))
        .or_else(|| input.get("path"))
        .and_then(|v| v.as_str())
        .unwrap_or_else(|| _file_path.as_deref().unwrap_or(""));

    // Settings file — run config policy
    if file_path.ends_with("settings.json") && (file_path.contains(".claude") || file_path.contains(".gemini")) {
        return config_policy::check(&Some(input.clone()), file_path);
    }

    // Only check Python source files
    if !is_python_source(file_path) {
        return vec![];
    }

    // Extract all content pieces being written or replaced
    let mut contents = vec![];
    if let Some(c) = input
        .get("new_string")
        .or_else(|| input.get("content"))
        .or_else(|| input.get("CodeContent"))
        .or_else(|| input.get("codeContent"))
        .or_else(|| input.get("ReplacementContent"))
        .or_else(|| input.get("replacementContent"))
        .and_then(|v| v.as_str())
    {
        contents.push(c);
    }

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
                contents.push(chunk_content);
            }
        }
    }

    let mut violations = vec![];
    for content in contents {
        violations.extend(test_policy::check(content, file_path));
        violations.extend(evidence_policy::check(content, file_path));
    }
    violations
}

// ─── PostToolUse ─────────────────────────────────────────────────────────────

fn check_post_tool_use(
    tool: &str,
    input: &Option<Value>,
    _file_path: &Option<String>,
) -> Vec<Violation> {
    if !matches!(
        tool,
        "Edit"
            | "Write"
            | "replace_file_content"
            | "write_to_file"
            | "multi_replace_file_content"
            | "replace"
            | "write_file"
    ) {
        return vec![];
    }

    let input = match input {
        Some(v) => v,
        None => return vec![],
    };

    let file_path = input
        .get("file_path")
        .or_else(|| input.get("TargetFile"))
        .or_else(|| input.get("targetFile"))
        .or_else(|| input.get("path"))
        .and_then(|v| v.as_str())
        .unwrap_or_else(|| _file_path.as_deref().unwrap_or(""));

    if !is_python_source(file_path) {
        return vec![];
    }

    // PostToolUse: re-read the actual file and run the full policy.
    // This catches violations introduced by edits that were not in the new_string fragment
    // (e.g., the file had an existing import that combined with the new content becomes
    // a violation, or evidence fakes slipped through a partial-content PreToolUse check).
    let content = match std::fs::read_to_string(file_path) {
        Ok(c) => c,
        Err(e) => {
            // File became unreadable after write — this is suspicious
            return vec![Violation {
                pattern: "file unreadable after write".to_string(),
                location: format!("{}:{}", file_path, line!()),
                rule: format!("PostToolUse: File became unreadable after write: {}", e),
            }];
        }
    };

    let mut violations = vec![];
    violations.extend(test_policy::check(&content, file_path));
    violations.extend(evidence_policy::check(&content, file_path));
    violations
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

/// Returns true for Python files that should be policy-checked.
///
/// Included (full policy):
///   src/ostar/**  except src/ostar/testing/ (framework code)
///   tests/chicago_tdd/**
///   tests/integration/**
///   tests/live/**
///
/// Excluded:
///   tests/archive/        — quarantined legacy; intentionally uses mocks
///   tests/smoke/          — infrastructure smoke tests; subprocess/OTel mocks are legitimate
///   src/ostar/testing/    — the anti-mock framework itself
///   tools/                — Rust source, not Python policy scope
fn is_python_source(path: &str) -> bool {
    if !path.ends_with(".py") {
        return false;
    }
    let p = path.replace('\\', "/");

    // Must be under src/ or tests/
    // Check both relative paths (tests/...) and absolute paths (.../tests/...)
    if !p.contains("src/") && !p.contains("tests/") {
        return false;
    }

    // Skip Rust tooling
    if p.contains("tools/") {
        return false;
    }

    // Skip quarantined / infrastructure / framework-meta paths
    if p.contains("tests/archive/")
        || p.contains("tests/smoke/")
        || p.contains("src/ostar/testing/")
        || p.contains("tests/chicago_tdd/microframework/")
    {
        return false;
    }

    true
}

/// Returns true for non-Python files that should be scanned for policy violations.
///
/// High-risk files that can bypass truth-gate enforcement:
///   - GitHub workflows (.yml/.yaml in .github/workflows/) — can disable CI
///   - pre-commit config (.pre-commit-config.yaml) — can disable hooks
///   - pyproject.toml — contains coverage thresholds that could be weakened
///   - Cargo.toml — build configuration
fn should_scan_non_python(path: &str) -> bool {
    let p = path.replace('\\', "/");

    // High-risk: GitHub workflows (can disable truth-gate)
    if (p.ends_with(".yml") || p.ends_with(".yaml"))
        && (p.contains(".github/workflows/") || p.ends_with(".pre-commit-config.yaml"))
    {
        return true;
    }

    // High-risk: pyproject.toml (contains coverage thresholds)
    if p.ends_with("pyproject.toml") {
        return true;
    }

    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::fs;

    #[test]
    fn test_check_pre_tool_use_replace_file_content() {
        let input = json!({
            "TargetFile": "tests/chicago_tdd/test_something.py",
            "ReplacementContent": "from unittest.mock import patch\n"
        });
        let violations = check_pre_tool_use("replace_file_content", &Some(input), &None);
        assert!(!violations.is_empty(), "Should have caught unittest.mock import violation");
        assert_eq!(violations[0].pattern, "from unittest.mock import");
    }

    #[test]
    fn test_check_pre_tool_use_multi_replace_file_content() {
        let input = json!({
            "TargetFile": "tests/chicago_tdd/test_something.py",
            "ReplacementChunks": [
                {
                    "ReplacementContent": "class MockThing:\n    pass\n"
                }
            ]
        });
        let violations = check_pre_tool_use("multi_replace_file_content", &Some(input), &None);
        assert!(!violations.is_empty(), "Should have caught class Mock violation");
        assert_eq!(violations[0].pattern, "class Mock");
    }

    #[test]
    fn test_check_pre_tool_use_write_to_file() {
        let input = json!({
            "TargetFile": "tests/chicago_tdd/test_something.py",
            "CodeContent": "def test_foo():\n    # TODO: write test\n"
        });
        let violations = check_pre_tool_use("write_to_file", &Some(input), &None);
        assert!(!violations.is_empty(), "Should have caught TODO violation");
        assert_eq!(violations[0].pattern, "# TODO");
    }

    #[test]
    fn test_check_post_tool_use_after_tool() {
        let temp_dir = std::env::temp_dir();
        let test_subdir = temp_dir.join("tests");
        fs::create_dir_all(&test_subdir).ok();
        let file_path = test_subdir.join("test_temp_post_use.py");
        fs::write(&file_path, "fake_span = True\n").unwrap();

        let input = json!({
            "TargetFile": file_path.to_str().unwrap()
        });

        let violations = check_post_tool_use("replace_file_content", &Some(input), &None);
        fs::remove_file(file_path).ok();

        assert!(!violations.is_empty(), "Should have caught fake_span violation in PostToolUse");
        assert_eq!(violations[0].pattern, "fake_span");
    }
}

