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
        """    let disposition_decision_count =
        apply_disposition_decisions(&mut capabilities, &decision_bytes, &source.head)?;""",
        """    let disposition_decision_count = apply_disposition_decisions(
        &mut capabilities,
        &decision_bytes,
        &source.head,
        &cli.source,
    )?;""",
        "source-repo-call",
    )
    text = replace_once(
        text,
        """fn apply_disposition_decisions(
    capabilities: &mut [CapabilityRecord], bytes: &[u8], source_head: &str,
) -> Result<usize> {""",
        """fn apply_disposition_decisions(
    capabilities: &mut [CapabilityRecord], bytes: &[u8], source_head: &str,
    source_repo: &Path,
) -> Result<usize> {""",
        "source-repo-signature",
    )
    text = replace_once(
        text,
        """        if decision.rationale.trim().is_empty() || decision.evidence_refs.is_empty() {
            bail!(
                \"DISPOSITION_DECISION_EVIDENCE_MISSING: {}\",
                decision.capability_id
            );
        }
        if decision
            .evidence_refs
            .iter()
            .any(|reference| reference.trim().is_empty())
        {
            bail!(
                \"DISPOSITION_DECISION_EVIDENCE_EMPTY: {}\",
                decision.capability_id
            );
        }""",
        """        if decision.rationale.trim().is_empty()
            || decision.evidence_refs.is_empty()
            || decision.recovery_refs.is_empty()
        {
            bail!(
                \"DISPOSITION_DECISION_EVIDENCE_MISSING: {}\",
                decision.capability_id
            );
        }
        if decision
            .evidence_refs
            .iter()
            .chain(decision.recovery_refs.iter())
            .any(|reference| reference.trim().is_empty())
        {
            bail!(
                \"DISPOSITION_DECISION_EVIDENCE_EMPTY: {}\",
                decision.capability_id
            );
        }""",
        "recovery-validation",
    )
    text = replace_once(
        text,
        """        if capability.archive_path.trim().is_empty()
            || capability.historical_source_commit.trim().is_empty()
        {
            bail!(
                \"ARCHIVE_POLICY_RECOVERY_EVIDENCE_MISSING: {}\",
                decision.capability_id
            );
        }""",
        """        if capability.historical_source_commit.trim().is_empty() {
            bail!(
                \"ARCHIVE_POLICY_HISTORICAL_SOURCE_MISSING: {}\",
                decision.capability_id
            );
        }
        verify_recovery_refs(source_repo, decision)?;""",
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
