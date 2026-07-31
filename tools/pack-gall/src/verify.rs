use crate::io::{digest_json, digest_path};
use crate::model::{
    Catalog, Checkpoint, Observation, Receipt, VerifierReport, OBSERVATION_SCHEMA,
    RECEIPT_SCHEMA, VERIFIER_SCHEMA,
};
use crate::observe::{load_contract, observe};
use crate::resolver::resolve;
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;

pub fn verify(root: &Path, observed: &Observation) -> Result<VerifierReport, String> {
    let recomputed = observe(root)?;
    let observation_digest = digest_json(observed)?;
    let replay_digest = digest_json(&recomputed)?;
    let same_observation = observed == &recomputed && observation_digest == replay_digest;
    let command_ok = observed
        .command_matrix
        .iter()
        .all(|matrix| matrix.missing_verbs.is_empty());
    let schema_ok = observed
        .schema_matrix
        .iter()
        .all(|matrix| matrix.missing_tokens.is_empty());
    let ownership_ok = observed.ownership.len() == observed.surfaces.len();
    let corpus_ok = observed.corpus.provider_ids
        == vec!["aws".to_string(), "azure".to_string(), "gcp".to_string()]
        && observed.corpus.group_count >= 18
        && observed.corpus.unique_pack_count > observed.corpus.group_count;
    let resolutions_ok = observed.corpus.representative_resolutions.len() == 3
        && observed
            .corpus
            .representative_resolutions
            .iter()
            .all(|resolution| {
                !resolution.resolved_groups.is_empty()
                    && !resolution.packs.is_empty()
                    && resolution.plan_digest.len() == 64
            });
    let canonical_schema_ok = verify_schema_shape(
        &root.join("tools/pack-gall/contracts/pack-kernel.schema.json"),
        &["pack_identity", "bblock_group", "resolution_plan"],
    )?;
    let verifier_schema_ok = verify_schema_shape(
        &root.join("tools/pack-gall/contracts/verifier-report.schema.json"),
        &[
            "schema",
            "source_digest",
            "observation_digest",
            "checkpoints",
            "standing",
        ],
    )?;
    let cycle_refused = cycle_is_refused(root)?;
    let checkpoints = vec![
        checkpoint(
            "G0.1",
            "Fence inventory",
            observed.surfaces.len() >= 10,
            vec![format!("{} required surfaces hashed", observed.surfaces.len())],
        ),
        checkpoint(
            "G0.2",
            "Command behavior matrix",
            command_ok,
            observed
                .command_matrix
                .iter()
                .map(|matrix| format!("{}:{} verbs", matrix.noun, matrix.observed_verbs.len()))
                .collect(),
        ),
        checkpoint(
            "G0.3",
            "Schema and digest matrix",
            schema_ok,
            observed
                .schema_matrix
                .iter()
                .map(|matrix| format!("{}:{} tokens", matrix.path, matrix.required_tokens.len()))
                .collect(),
        ),
        checkpoint(
            "G0.4",
            "Exclusive ownership map",
            ownership_ok,
            vec![format!("{} unique owners", observed.ownership.len())],
        ),
        checkpoint(
            "G0.5",
            "Real compatibility corpus",
            corpus_ok,
            vec![format!(
                "{} providers, {} groups, {} packs",
                observed.corpus.provider_ids.len(),
                observed.corpus.group_count,
                observed.corpus.unique_pack_count
            )],
        ),
        checkpoint(
            "G0.6",
            "Machine-readable equivalence report",
            observed.schema == OBSERVATION_SCHEMA,
            vec![format!("observation digest {observation_digest}")],
        ),
        checkpoint(
            "G1.1",
            "Canonical pack-kernel schema",
            canonical_schema_ok,
            vec![format!(
                "schema digest {}",
                observed.canonical_schema_digest
            )],
        ),
        checkpoint(
            "G1.2",
            "Deterministic read-only resolver",
            resolutions_ok,
            observed
                .corpus
                .representative_resolutions
                .iter()
                .map(|resolution| format!("{}:{}", resolution.provider, resolution.plan_digest))
                .collect(),
        ),
        checkpoint(
            "G1.3",
            "Replay and cycle refusal",
            same_observation && cycle_refused,
            vec![
                format!("replay digest {replay_digest}"),
                format!("cycle refused {cycle_refused}"),
            ],
        ),
        checkpoint(
            "G2.1",
            "External verifier contract",
            verifier_schema_ok,
            vec![format!(
                "verifier schema digest {}",
                observed.verifier_schema_digest
            )],
        ),
    ];
    let all_passed = checkpoints.iter().all(|checkpoint| checkpoint.passed);
    Ok(VerifierReport {
        schema: VERIFIER_SCHEMA.to_string(),
        source_digest: observed.source_digest.clone(),
        observation_digest,
        checkpoints,
        standing: if all_passed {
            "PARTIAL_ALIVE"
        } else {
            "BUILD_BROKEN"
        }
        .to_string(),
    })
}

pub fn issue_receipt(
    report_path: &Path,
    observation_path: &Path,
    report: &VerifierReport,
) -> Result<Receipt, String> {
    let mut artifacts = BTreeMap::new();
    artifacts.insert(
        observation_path.display().to_string(),
        digest_path(observation_path)?,
    );
    artifacts.insert(report_path.display().to_string(), digest_path(report_path)?);
    let body = serde_json::json!({
        "schema": RECEIPT_SCHEMA,
        "operation": "pack-gall-verify",
        "previous_digest": "GENESIS",
        "artifacts": &artifacts,
        "source_digest": &report.source_digest,
        "observation_digest": &report.observation_digest,
        "standing": &report.standing,
    });
    Ok(Receipt {
        schema: RECEIPT_SCHEMA.to_string(),
        operation: "pack-gall-verify".to_string(),
        previous_digest: "GENESIS".to_string(),
        artifacts,
        digest_algorithm: "blake3".to_string(),
        digest: digest_json(&body)?,
    })
}

fn cycle_is_refused(root: &Path) -> Result<bool, String> {
    let contract = load_contract(root)?;
    let bytes = fs::read(root.join(contract.catalog_path)).map_err(|e| e.to_string())?;
    let mut catalog: Catalog = serde_json::from_slice(&bytes).map_err(|e| e.to_string())?;
    if catalog.groups.len() < 2 {
        return Ok(false);
    }
    let first = catalog.groups[0].id.clone();
    let last = catalog
        .groups
        .last()
        .map(|group| group.id.clone())
        .ok_or("catalog has no groups")?;
    catalog.groups[0].dependencies.push(last);
    catalog
        .groups
        .last_mut()
        .ok_or("catalog has no groups")?
        .dependencies
        .push(first.clone());
    Ok(resolve(&catalog, &first, &catalog.providers[0].id).is_err())
}

fn verify_schema_shape(path: &Path, required_properties: &[&str]) -> Result<bool, String> {
    let bytes = fs::read(path).map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    let value: Value = serde_json::from_slice(&bytes)
        .map_err(|e| format!("cannot parse {}: {e}", path.display()))?;
    let required = value
        .get("required")
        .and_then(Value::as_array)
        .ok_or_else(|| format!("{} has no required array", path.display()))?;
    let set: BTreeSet<_> = required.iter().filter_map(Value::as_str).collect();
    Ok(required_properties
        .iter()
        .all(|property| set.contains(property)))
}

fn checkpoint(id: &str, title: &str, passed: bool, evidence: Vec<String>) -> Checkpoint {
    Checkpoint {
        id: id.to_string(),
        title: title.to_string(),
        passed,
        state: if passed {
            "PARTIAL_ALIVE"
        } else {
            "BUILD_BROKEN"
        }
        .to_string(),
        evidence,
    }
}
