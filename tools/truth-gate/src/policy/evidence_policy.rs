/// evidence_policy: blocks introduction of synthetic evidence factories.
///
/// Fake spans, fake receipts, fake OCEL events — all of these let a test claim
/// process conformance without any real process having occurred.
use super::Violation;

const FORBIDDEN: &[(&str, &str)] = &[
    // Fake span / trace factories
    (
        "_make_spans(",
        "synthetic span factory — OCEL must be derived from real OTel traces, not fabricated",
    ),
    (
        "fake_span",
        "fake span — spans must come from real instrumented execution",
    ),
    (
        "fake_trace",
        "fake trace — traces must come from real instrumented execution",
    ),
    (
        "synthetic_trace",
        "synthetic trace — traces must come from real instrumented execution",
    ),
    (
        "fabricate_",
        "fabricated evidence — no synthetic proof surfaces of any kind",
    ),
    (
        "mock_ocel",
        "mock OCEL — event logs must be derived from real execution, not constructed",
    ),
    (
        "fake_receipt",
        "fake receipt — receipts must be produced by real manufacturing operators",
    ),
    (
        "synthetic_receipt",
        "synthetic receipt — receipts must be produced by real manufacturing operators",
    ),
    // Fake LLM / MCP boundaries
    (
        "FakeLLM(",
        "FakeLLM — LLM boundary must be real; use a real model or skip the test",
    ),
    (
        "MockLLM(",
        "MockLLM — LLM boundary must be real; use a real model or skip the test",
    ),
    (
        "FakeMCP(",
        "FakeMCP — MCP boundary must be real; use a real server or skip the test",
    ),
    (
        "MockMCP(",
        "MockMCP — MCP boundary must be real; use a real server or skip the test",
    ),
];

pub fn check(content: &str, file_path: &str) -> Vec<Violation> {
    let mut violations = vec![];

    for (line_num_0, line) in content.lines().enumerate() {
        let trimmed = line.trim();
        if trimmed.starts_with('#') {
            continue; // skip comment lines
        }

        for (pattern, rule) in FORBIDDEN {
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
