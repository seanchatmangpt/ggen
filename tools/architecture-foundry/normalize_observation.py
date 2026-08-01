#!/usr/bin/env python3
"""Normalize the initial Workstream B verifier implementation.

The script is intentionally idempotent and exists only for the bounded bootstrap commit that
separates raw observer evidence from the generated admission report before exact-head testing.
"""

from pathlib import Path

PATH = Path(__file__).parent / "src" / "bin" / "admit_observation.rs"


def normalize(text: str, old: str, new: str) -> str:
    if old in text:
        return text.replace(old, new, 1)
    if new not in text:
        raise SystemExit(f"normalization anchor missing: {old!r}")
    return text


def main() -> None:
    text = PATH.read_text()

    replacements = [
        (
            '    if workstream.dependencies != ["A"] {',
            '    if workstream.dependencies.len() != 1 || workstream.dependencies[0] != "A" {',
        ),
        (
            '    let report_bytes = git_show(&cli.source, &evidence_commit, REPORT_PATH)?;',
            '    let observer_report_bytes = git_show(&cli.source, &evidence_commit, REPORT_PATH)?;',
        ),
        (
            '    let observer_classes = parse_observer_classes(&report_bytes)?;',
            '    let observer_classes = parse_observer_classes(&observer_report_bytes)?;',
        ),
        (
            '    write_new(&raw_report_path, &report_bytes)?;',
            '    write_new(&raw_report_path, &observer_report_bytes)?;',
        ),
        (
            '        digest_bytes(&report_bytes),\n    );\n    evidence_digests.insert(\n        "observer-reports.json"',
            '        digest_bytes(&observer_report_bytes),\n    );\n    evidence_digests.insert(\n        "observer-reports.json"',
        ),
        (
            '    let report_bytes = canonical_json(&report)?;\n    let report_digest = digest_bytes(&report_bytes);\n    write_new(&report_path, &report_bytes)?;',
            '    let admission_report_bytes = canonical_json(&report)?;\n    let report_digest = digest_bytes(&admission_report_bytes);\n    write_new(&report_path, &admission_report_bytes)?;',
        ),
        (
            '        digest_bytes(&report_bytes),\n    );\n\n    let mut outputs',
            '        digest_bytes(&observer_report_bytes),\n    );\n\n    let mut outputs',
        ),
        (
            '            "foundry/evidence/B/observer-class-report.md",\n            report_bytes.as_slice(),',
            '            "foundry/evidence/B/observer-class-report.md",\n            observer_report_bytes.as_slice(),',
        ),
        (
            '            "foundry/workstreams/B/admission-report.json",\n            report_bytes.as_slice(),',
            '            "foundry/workstreams/B/admission-report.json",\n            admission_report_bytes.as_slice(),',
        ),
    ]

    for old, new in replacements:
        text = normalize(text, old, new)

    PATH.write_text(text)


if __name__ == "__main__":
    main()
