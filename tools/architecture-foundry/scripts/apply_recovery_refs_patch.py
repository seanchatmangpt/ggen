#!/usr/bin/env python3
"""Deterministically strengthen C archive decisions with verified Git recovery refs."""

from pathlib import Path

TARGET = Path("tools/architecture-foundry/src/bin/admit_capabilities.rs")


def replace_once(text: str, old: str, new: str, label: str) -> str:
    if new in text:
        return text
    count = text.count(old)
    if count != 1:
        raise SystemExit(f"PATCH_REFUSED[{label}]: expected one match, observed {count}")
    return text.replace(old, new, 1)


def main() -> None:
    text = TARGET.read_text(encoding="utf-8")

    text = replace_once(
        text,
        "use std::path::{Path, PathBuf};\n",
        "use std::path::{Path, PathBuf};\nuse std::process::Command;\n",
        "command-import",
    )
    text = replace_once(
        text,
        "    evidence_refs: Vec<String>,\n}\n\n#[derive(Debug, Parser)]",
        "    evidence_refs: Vec<String>,\n    recovery_refs: Vec<String>,\n}\n\n#[derive(Debug, Parser)]",
        "recovery-field",
    )
    text = replace_once(
        text,
        """    let disposition_decision_count = apply_disposition_decisions(\n        &mut capabilities,\n        &decision_bytes,\n        &source.head,\n    )?;""",
        """    let disposition_decision_count = apply_disposition_decisions(\n        &mut capabilities,\n        &decision_bytes,\n        &source.head,\n        &cli.source,\n    )?;""",
        "source-repo-call",
    )
    text = replace_once(
        text,
        """fn apply_disposition_decisions(\n    capabilities: &mut [CapabilityRecord], bytes: &[u8], source_head: &str,\n) -> Result<usize> {""",
        """fn apply_disposition_decisions(\n    capabilities: &mut [CapabilityRecord], bytes: &[u8], source_head: &str,\n    source_repo: &Path,\n) -> Result<usize> {""",
        "source-repo-signature",
    )
    text = replace_once(
        text,
        """        if decision.rationale.trim().is_empty() || decision.evidence_refs.is_empty() {\n            bail!(\n                \"DISPOSITION_DECISION_EVIDENCE_MISSING: {}\",\n                decision.capability_id\n            );\n        }\n        if decision\n            .evidence_refs\n            .iter()\n            .any(|reference| reference.trim().is_empty())\n        {\n            bail!(\n                \"DISPOSITION_DECISION_EVIDENCE_EMPTY: {}\",\n                decision.capability_id\n            );\n        }""",
        """        if decision.rationale.trim().is_empty()\n            || decision.evidence_refs.is_empty()\n            || decision.recovery_refs.is_empty()\n        {\n            bail!(\n                \"DISPOSITION_DECISION_EVIDENCE_MISSING: {}\",\n                decision.capability_id\n            );\n        }\n        if decision\n            .evidence_refs\n            .iter()\n            .chain(decision.recovery_refs.iter())\n            .any(|reference| reference.trim().is_empty())\n        {\n            bail!(\n                \"DISPOSITION_DECISION_EVIDENCE_EMPTY: {}\",\n                decision.capability_id\n            );\n        }""",
        "recovery-validation",
    )
    text = replace_once(
        text,
        """        if capability.archive_path.trim().is_empty()\n            || capability.historical_source_commit.trim().is_empty()\n        {\n            bail!(\n                \"ARCHIVE_POLICY_RECOVERY_EVIDENCE_MISSING: {}\",\n                decision.capability_id\n            );\n        }""",
        """        if capability.historical_source_commit.trim().is_empty() {\n            bail!(\n                \"ARCHIVE_POLICY_HISTORICAL_SOURCE_MISSING: {}\",\n                decision.capability_id\n            );\n        }\n        verify_recovery_refs(source_repo, decision)?;""",
        "archive-recovery-policy",
    )

    marker = "fn parse_turtle_string(object: &str) -> Result<String> {"
    if "fn verify_recovery_refs(" not in text:
        if text.count(marker) != 1:
            raise SystemExit("PATCH_REFUSED[recovery-helper-anchor]")
        helper = r'''fn verify_recovery_refs(source_repo: &Path, decision: &DispositionDecision) -> Result<()> {
    for reference in &decision.recovery_refs {
        let revision = reference
            .strip_prefix("ggen@")
            .with_context(|| {
                format!(
                    "ARCHIVE_RECOVERY_REFERENCE_INVALID: {}:{}",
                    decision.capability_id, reference
                )
            })?
            .split(':')
            .next()
            .unwrap_or_default();
        if revision.len() < 7 || !revision.chars().all(|character| character.is_ascii_hexdigit()) {
            bail!(
                "ARCHIVE_RECOVERY_REVISION_INVALID: {}:{}",
                decision.capability_id,
                reference
            );
        }
        let object = format!("{revision}^{{commit}}");
        let status = Command::new("git")
            .arg("-C")
            .arg(source_repo)
            .arg("cat-file")
            .arg("-e")
            .arg(&object)
            .status()
            .with_context(|| {
                format!(
                    "ARCHIVE_RECOVERY_GIT_FAILED: {}:{}",
                    decision.capability_id, reference
                )
            })?;
        if !status.success() {
            bail!(
                "ARCHIVE_RECOVERY_COMMIT_MISSING: {}:{}",
                decision.capability_id,
                reference
            );
        }
    }
    Ok(())
}

'''
        text = text.replace(marker, helper + marker, 1)

    TARGET.write_text(text, encoding="utf-8")


if __name__ == "__main__":
    main()
