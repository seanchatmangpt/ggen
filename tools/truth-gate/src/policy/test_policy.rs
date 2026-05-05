/// test_policy: blocks introduction of mock/stub/label patterns into Python source.
///
/// These patterns let a test pass without real execution — they are structurally
/// equivalent to lying about what happened.
use super::Violation;

/// (pattern, rule explanation)
/// Ordered by severity — imports are definitive; labels are gaming surfaces.
const FORBIDDEN: &[(&str, &str)] = &[
    // Mock imports — definitive signal that a test double is in use
    (
        "from unittest.mock import",
        "unittest.mock — synthetic doubles cannot prove real execution",
    ),
    (
        "from unittest import mock",
        "unittest.mock — synthetic doubles cannot prove real execution",
    ),
    (
        "import mock",
        "mock package — synthetic doubles cannot prove real execution",
    ),
    // Patch / monkeypatch — replaces the real thing with a fake path
    (
        "@patch(",
        "@patch — monkeypatching hides real execution behind a fake surface",
    ),
    (
        "patch.object(",
        "patch.object — monkeypatching hides real execution behind a fake surface",
    ),
    (
        "patch.dict(",
        "patch.dict — monkeypatching hides real execution behind a fake surface",
    ),
    (
        "monkeypatch.",
        "monkeypatch — pytest fixture replaces real execution with a stub",
    ),
    // Mock instantiation — MagicMock/Mock/AsyncMock are synthetic doubles
    (
        "MagicMock(",
        "MagicMock — synthetic double; cannot prove real boundary was crossed",
    ),
    (
        "AsyncMock(",
        "AsyncMock — synthetic double; cannot prove real async boundary was crossed",
    ),
    // Bare Mock() base class — agents may use this instead of MagicMock
    // (word-boundary checked separately in the loop to avoid false positives)
    (
        "= Mock(",
        "Mock() — synthetic double; cannot prove real boundary was crossed",
    ),
    // Label-gaming — naming something "chicago" or "real" satisfies the label not the behavior
    (
        "@chicago_tdd",
        "@chicago_tdd label — label-gaming replaces boundary enforcement; remove the label, enforce the boundary",
    ),
    (
        "pytest.mark.chicago_tdd",
        "chicago_tdd marker — label-gaming replaces boundary enforcement",
    ),
    // Placeholder work — deferred defects
    (
        "# TODO",
        "TODO placeholder — placeholder is a defect, not future work",
    ),
    (
        "# FIXME",
        "FIXME placeholder — fix now or delete; placeholders accrue and decay",
    ),
    // Custom Mock/Fake/Stub classes — hand-written mocks bypass the filter
    (
        "class Mock",
        "Mock class — custom mock definitions bypass unittest.mock filter; use real execution",
    ),
    (
        "class Fake",
        "Fake class — custom fake definitions bypass framework checks; use real execution",
    ),
    (
        "class Stub",
        "Stub class — custom stub definitions replace real behavior; use real execution",
    ),
    (
        "def mock_",
        "mock_* function — factory for creating synthetic doubles; use real execution",
    ),
    (
        "def fake_",
        "fake_* function — factory for creating synthetic fakes; use real execution",
    ),
];

pub fn check(content: &str, file_path: &str) -> Vec<Violation> {
    let mut violations = vec![];

    for (line_num_0, line) in content.lines().enumerate() {
        let trimmed = line.trim();

        // Skip pure-comment lines that are documenting bad patterns (e.g. in docstrings)
        // We only want to catch actual code lines.
        if trimmed.starts_with('#') {
            // Still flag TODO/FIXME even in comments — those are the primary case
            for (pattern, rule) in FORBIDDEN {
                if *pattern == "# TODO" || *pattern == "# FIXME" {
                    if trimmed.starts_with(pattern) {
                        violations.push(Violation {
                            pattern: pattern.to_string(),
                            location: format!("{}:{}", file_path, line_num_0 + 1),
                            rule: rule.to_string(),
                        });
                    }
                }
            }
            continue;
        }

        for (pattern, rule) in FORBIDDEN {
            if *pattern == "# TODO" || *pattern == "# FIXME" {
                continue; // handled above
            }

            // "import mock" needs a word-boundary check — "import mock_helper" must not match
            if *pattern == "import mock" {
                if trimmed.starts_with("import mock") {
                    let after = &trimmed["import mock".len()..];
                    let word_boundary = after.is_empty()
                        || !after.starts_with(|c: char| c.is_alphanumeric() || c == '_');
                    if word_boundary {
                        violations.push(Violation {
                            pattern: pattern.to_string(),
                            location: format!("{}:{}", file_path, line_num_0 + 1),
                            rule: rule.to_string(),
                        });
                    }
                }
                continue;
            }

            if trimmed.contains(pattern) {
                violations.push(Violation {
                    pattern: pattern.to_string(),
                    location: format!("{}:{}", file_path, line_num_0 + 1),
                    rule: rule.to_string(),
                });
            }
        }
    }

    violations
}
