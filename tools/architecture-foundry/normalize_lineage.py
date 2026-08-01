#!/usr/bin/env python3
"""Teach the permanent verifier both batch-lineage and component-extraction lineage schemas."""

from pathlib import Path

PATH = Path(__file__).parent / "src" / "lib.rs"

OLD = '''fn verify_lineage_record(source_path: &Path, corpus_path: &Path, path: &Path) -> Result<()> {
    let record: LineageRecord = serde_json::from_slice(&read(path)?)?;
    if record.schema_version != LINEAGE_SCHEMA {
        return refusal(
            "LINEAGE_SCHEMA_INVALID",
            format!("{} has schema {}", path.display(), record.schema_version),
        );
    }
    let source_relative = safe_relative(&record.source_path)?;
    let destination_relative = safe_relative(&record.destination_path)?;
    let source_digest = digest_file(&source_path.join(source_relative))?;
    let destination_digest = digest_file(&corpus_path.join(destination_relative))?;
    if source_digest != record.content_digest || destination_digest != record.content_digest {
        return refusal(
            "LINEAGE_DIGEST_MISMATCH",
            format!("component {} content changed", record.component_id),
        );
    }
    Ok(())
}
'''

NEW = '''fn verify_lineage_record(source_path: &Path, corpus_path: &Path, path: &Path) -> Result<()> {
    let bytes = read(path)?;
    let value: serde_json::Value = serde_json::from_slice(&bytes)?;
    let schema = value
        .get("schema_version")
        .and_then(serde_json::Value::as_str)
        .unwrap_or_default();

    if schema == LINEAGE_SCHEMA {
        let record: LineageRecord = serde_json::from_value(value)?;
        let source_relative = safe_relative(&record.source_path)?;
        let destination_relative = safe_relative(&record.destination_path)?;
        let source_digest = digest_file(&source_path.join(source_relative))?;
        let destination_digest = digest_file(&corpus_path.join(destination_relative))?;
        if source_digest != record.content_digest || destination_digest != record.content_digest {
            return refusal(
                "LINEAGE_DIGEST_MISMATCH",
                format!("component {} content changed", record.component_id),
            );
        }
        return Ok(());
    }

    if schema == "ggen.enterprise-architecture-foundry.extraction-admission/1" {
        let capability_id = value
            .get("capability_id")
            .and_then(serde_json::Value::as_str)
            .unwrap_or("<unknown>");
        let destination = value
            .get("destination_path")
            .and_then(serde_json::Value::as_str)
            .ok_or_else(|| FoundryError::Refusal {
                code: "LINEAGE_DESTINATION_MISSING".to_string(),
                message: capability_id.to_string(),
            })?;
        let expected_manifest = value
            .get("manifest_digest")
            .and_then(serde_json::Value::as_str)
            .ok_or_else(|| FoundryError::Refusal {
                code: "LINEAGE_MANIFEST_DIGEST_MISSING".to_string(),
                message: capability_id.to_string(),
            })?;
        let manifest_relative = safe_relative(destination)?.join("component-manifest.json");
        let observed_manifest = digest_file(&corpus_path.join(manifest_relative))?;
        if observed_manifest != expected_manifest {
            return refusal(
                "LINEAGE_MANIFEST_DIGEST_MISMATCH",
                format!("component {capability_id} manifest changed"),
            );
        }
        let blob_digests = value
            .get("blob_digests")
            .and_then(serde_json::Value::as_array)
            .ok_or_else(|| FoundryError::Refusal {
                code: "LINEAGE_BLOB_DIGESTS_MISSING".to_string(),
                message: capability_id.to_string(),
            })?;
        for digest in blob_digests {
            let expected = digest.as_str().ok_or_else(|| FoundryError::Refusal {
                code: "LINEAGE_BLOB_DIGEST_INVALID".to_string(),
                message: capability_id.to_string(),
            })?;
            let blob = corpus_path.join("foundry/blobs/blake3").join(expected);
            let observed = digest_file(&blob)?;
            if observed != expected {
                return refusal(
                    "LINEAGE_BLOB_DIGEST_MISMATCH",
                    format!("component {capability_id} blob {expected} changed"),
                );
            }
        }
        return Ok(());
    }

    refusal(
        "LINEAGE_SCHEMA_INVALID",
        format!("{} has schema {schema}", path.display()),
    )
}
'''


def main() -> None:
    text = PATH.read_text()
    if OLD in text:
        text = text.replace(OLD, NEW, 1)
    elif NEW not in text:
        raise SystemExit("lineage verifier anchor missing")
    PATH.write_text(text)


if __name__ == "__main__":
    main()
