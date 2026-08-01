#!/usr/bin/env python3
"""One-time deterministic patch adding receipted C-stage disposition decisions.

The patch is intentionally structural and idempotent. It preserves Workstream B's
observation evidence and teaches Workstream C to consume a separate, exact-head
bound decision overlay for capabilities whose observed disposition remains unknown.
"""

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
        "use serde::Serialize;",
        "use serde::{Deserialize, Serialize};",
        "serde-import",
    )

    text = replace_once(
        text,
        'const VERIFIER_ID: &str = "ggen-foundry-admit-capabilities/v1";\n',
        '''const VERIFIER_ID: &str = "ggen-foundry-admit-capabilities/v1";\nconst DISPOSITION_DECISION_SCHEMA: &str =\n    "ggen.enterprise-architecture-foundry.disposition-decisions/1";\nconst ARCHIVE_POLICY_ID: &str = "REMOVED_WITHOUT_REPLACEMENT_ARCHIVE";\n\n#[derive(Debug, Deserialize)]\nstruct DispositionDecisionFile {\n    schema_version: String,\n    source_authority_sha: String,\n    decisions: Vec<DispositionDecision>,\n}\n\n#[derive(Debug, Deserialize)]\nstruct DispositionDecision {\n    capability_id: String,\n    expected_disposition: String,\n    resolved_disposition: String,\n    policy_id: String,\n    rationale: String,\n    evidence_refs: Vec<String>,\n}\n''',
        "decision-structs",
    )

    text = replace_once(
        text,
        '''    #[arg(long)]\n    corpus: PathBuf,\n}''',
        '''    #[arg(long)]\n    corpus: PathBuf,\n    #[arg(\n        long,\n        default_value = "foundry/evidence/C/disposition-decisions.json"\n    )]\n    disposition_decisions: PathBuf,\n}''',
        "cli-overlay-path",
    )

    text = replace_once(
        text,
        '''    predicates: BTreeMap<String, YamlValue>,\n    evidence_digest: String,\n}''',
        '''    predicates: BTreeMap<String, YamlValue>,\n    evidence_digest: String,\n    disposition_decision_count: usize,\n    disposition_decision_digest: String,\n}''',
        "report-fields",
    )

    text = replace_once(
        text,
        '''    let capabilities = parse_capabilities(&evidence_bytes)?;\n    if capabilities.len() != 65 {''',
        '''    let mut capabilities = parse_capabilities(&evidence_bytes)?;\n    let decision_path = cli.corpus.join(&cli.disposition_decisions);\n    let decision_bytes = fs::read(&decision_path).with_context(|| {\n        format!(\n            "DISPOSITION_DECISIONS_MISSING: {}",\n            decision_path.display()\n        )\n    })?;\n    let disposition_decision_digest = digest_bytes(&decision_bytes);\n    let disposition_decision_count = apply_disposition_decisions(\n        &mut capabilities,\n        &decision_bytes,\n        &source.head,\n    )?;\n    if capabilities.len() != 65 {''',
        "load-overlay",
    )

    text = replace_once(
        text,
        '''        predicates: workstream.predicates.clone(),\n        evidence_digest: evidence_digest.clone(),\n    };''',
        '''        predicates: workstream.predicates.clone(),\n        evidence_digest: evidence_digest.clone(),\n        disposition_decision_count,\n        disposition_decision_digest: disposition_decision_digest.clone(),\n    };''',
        "report-values",
    )

    text = replace_once(
        text,
        '''    inputs.insert("capability-evidence".to_string(), evidence_digest);\n''',
        '''    inputs.insert("capability-evidence".to_string(), evidence_digest);\n    inputs.insert(\n        "disposition-decisions".to_string(),\n        disposition_decision_digest,\n    );\n''',
        "receipt-input",
    )

    text = replace_once(
        text,
        '''    let admitted_owner = derive_owner(\n        &disposition,\n        &replacement_owner,\n        &historical_semantic_owner,\n        &owning_subsystem,\n    )?;''',
        '''    let admitted_owner = if disposition == "DISPOSITION_UNKNOWN" {\n        String::new()\n    } else {\n        derive_owner(\n            &disposition,\n            &replacement_owner,\n            &historical_semantic_owner,\n            &owning_subsystem,\n        )?\n    };''',
        "defer-unknown-owner",
    )

    marker = "fn parse_turtle_string(object: &str) -> Result<String> {"
    if "fn apply_disposition_decisions(" not in text:
        if text.count(marker) != 1:
            raise SystemExit("PATCH_REFUSED[decision-function-anchor]")
        function = r'''fn apply_disposition_decisions(
    capabilities: &mut [CapabilityRecord], bytes: &[u8], source_head: &str,
) -> Result<usize> {
    let decision_file: DispositionDecisionFile =
        serde_json::from_slice(bytes).context("DISPOSITION_DECISIONS_SCHEMA_INVALID")?;
    if decision_file.schema_version != DISPOSITION_DECISION_SCHEMA {
        bail!(
            "DISPOSITION_DECISIONS_SCHEMA_UNSUPPORTED: {}",
            decision_file.schema_version
        );
    }
    if decision_file.source_authority_sha != source_head {
        bail!(
            "DISPOSITION_DECISIONS_HEAD_STALE: expected {source_head}, observed {}",
            decision_file.source_authority_sha
        );
    }

    let mut capability_indexes = BTreeMap::new();
    for (index, capability) in capabilities.iter().enumerate() {
        capability_indexes.insert(capability.capability_id.clone(), index);
    }

    let mut seen = BTreeSet::new();
    for decision in &decision_file.decisions {
        if !seen.insert(decision.capability_id.clone()) {
            bail!(
                "DISPOSITION_DECISION_DUPLICATE: {}",
                decision.capability_id
            );
        }
        if decision.expected_disposition != "DISPOSITION_UNKNOWN"
            || decision.resolved_disposition != "ARCHIVED"
            || decision.policy_id != ARCHIVE_POLICY_ID
        {
            bail!(
                "DISPOSITION_DECISION_POLICY_INVALID: {}",
                decision.capability_id
            );
        }
        if decision.rationale.trim().is_empty() || decision.evidence_refs.is_empty() {
            bail!(
                "DISPOSITION_DECISION_EVIDENCE_MISSING: {}",
                decision.capability_id
            );
        }
        if decision
            .evidence_refs
            .iter()
            .any(|reference| reference.trim().is_empty())
        {
            bail!(
                "DISPOSITION_DECISION_EVIDENCE_EMPTY: {}",
                decision.capability_id
            );
        }

        let index = capability_indexes
            .get(&decision.capability_id)
            .copied()
            .with_context(|| {
                format!(
                    "DISPOSITION_DECISION_CAPABILITY_MISSING: {}",
                    decision.capability_id
                )
            })?;
        let capability = &mut capabilities[index];
        if capability.disposition != decision.expected_disposition {
            bail!(
                "DISPOSITION_DECISION_PRECONDITION_DRIFT: {} expected {}, observed {}",
                decision.capability_id,
                decision.expected_disposition,
                capability.disposition
            );
        }
        if !capability.replacement_owner.trim().is_empty() {
            bail!(
                "ARCHIVE_POLICY_REPLACEMENT_OWNER_PRESENT: {}",
                decision.capability_id
            );
        }
        if capability.archive_path.trim().is_empty()
            || capability.historical_source_commit.trim().is_empty()
        {
            bail!(
                "ARCHIVE_POLICY_RECOVERY_EVIDENCE_MISSING: {}",
                decision.capability_id
            );
        }

        capability.disposition = decision.resolved_disposition.clone();
        capability.admitted_owner = derive_owner(
            &capability.disposition,
            &capability.replacement_owner,
            &capability.historical_semantic_owner,
            &capability.owning_subsystem,
        )?;
    }

    let unresolved: Vec<_> = capabilities
        .iter()
        .filter(|capability| capability.disposition == "DISPOSITION_UNKNOWN")
        .map(|capability| capability.capability_id.as_str())
        .collect();
    if !unresolved.is_empty() {
        bail!("UNKNOWN_DISPOSITION_UNRESOLVED: {}", unresolved.join(","));
    }

    Ok(decision_file.decisions.len())
}

'''
        text = text.replace(marker, function + marker, 1)

    TARGET.write_text(text, encoding="utf-8")


if __name__ == "__main__":
    main()
