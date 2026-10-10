use crate::{Artifact, Evidence, Manifest, Refusal};
use rayon::prelude::*;
use std::collections::HashSet;

pub fn validate_manifest_concurrently(manifest: &Manifest) -> Result<(), Refusal> {
    if manifest.schema.is_empty() {
        return Err(Refusal::ValidationFailed(
            "Schema cannot be empty".to_string(),
        ));
    }
    if !manifest.schema.starts_with("mfact/") {
        return Err(Refusal::InvalidFormat(format!(
            "Schema must start with 'mfact/', found: {}",
            manifest.schema
        )));
    }
    if manifest.release.is_empty() {
        return Err(Refusal::ValidationFailed(
            "Release cannot be empty".to_string(),
        ));
    }

    validate_artifacts_concurrently(&manifest.artifacts)?;
    validate_evidence_concurrently(&manifest.evidence)?;
    validate_trusted_base(&manifest.trusted_base)?;

    Ok(())
}

fn validate_artifacts_concurrently(artifacts: &[Artifact]) -> Result<(), Refusal> {
    // 1. Check structural constraints concurrently and deterministically
    let err = artifacts.par_iter().find_map_first(|artifact| {
        if artifact.name.is_empty() {
            return Some(Refusal::ValidationFailed(
                "Artifact name cannot be empty".to_string(),
            ));
        }
        if let Err(e) = validate_hash(&artifact.hash) {
            return Some(e);
        }
        None
    });

    if let Some(e) = err {
        return Err(e);
    }

    // 2. Check for duplicate names
    let mut names = HashSet::new();
    for artifact in artifacts {
        if !names.insert(&artifact.name) {
            return Err(Refusal::DuplicateElement(format!(
                "Artifact name duplicated: {}",
                artifact.name
            )));
        }
    }

    Ok(())
}

fn validate_evidence_concurrently(evidences: &[Evidence]) -> Result<(), Refusal> {
    let err = evidences.par_iter().find_map_first(|evidence| {
        if evidence.subject.is_empty() {
            return Some(Refusal::ValidationFailed(
                "Evidence subject cannot be empty".to_string(),
            ));
        }
        if evidence.kind.is_empty() {
            return Some(Refusal::ValidationFailed(
                "Evidence kind cannot be empty".to_string(),
            ));
        }
        if let Err(e) = validate_hash(&evidence.hash) {
            return Some(e);
        }
        None
    });

    if let Some(e) = err {
        return Err(e);
    }

    Ok(())
}

fn validate_trusted_base(base: &[String]) -> Result<(), Refusal> {
    let mut elements = HashSet::new();
    for item in base {
        if item.is_empty() {
            return Err(Refusal::ValidationFailed(
                "Trusted base item cannot be empty".to_string(),
            ));
        }
        if !elements.insert(item) {
            return Err(Refusal::DuplicateElement(format!(
                "Trusted base item duplicated: {}",
                item
            )));
        }
    }
    Ok(())
}

fn validate_hash(hash: &str) -> Result<(), Refusal> {
    if hash.len() != 64 {
        return Err(Refusal::InvalidHash(format!(
            "Hash must be exactly 64 chars, got {}",
            hash.len()
        )));
    }
    for c in hash.chars() {
        if !c.is_ascii_hexdigit() {
            return Err(Refusal::InvalidHash(format!(
                "Hash contains non-hex character: {}",
                c
            )));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Artifact, Evidence, Manifest};

    fn dummy_manifest() -> Manifest {
        Manifest {
            schema: "mfact/1.0".to_string(),
            release: "v1".to_string(),
            declaration_source: "src".to_string(),
            lean_source_origin: "origin".to_string(),
            trusted_base: vec![],
            llm_trusted_base: false,
            scope: "scope".to_string(),
            run_identifier: "run".to_string(),
            quadrature: "quad".to_string(),
            artifacts: vec![],
            evidence: vec![],
            stated_not_proven: vec![],
            fold_hash: "a".repeat(64),
        }
    }

    #[test]
    fn test_validate_hash_valid() {
        let h = "a".repeat(64);
        assert!(validate_hash(&h).is_ok());
    }

    #[test]
    fn test_validate_hash_invalid_len() {
        let h = "a".repeat(63);
        assert!(matches!(validate_hash(&h), Err(Refusal::InvalidHash(_))));
    }

    #[test]
    fn test_validate_hash_invalid_char() {
        let h = "z".repeat(64);
        assert!(matches!(validate_hash(&h), Err(Refusal::InvalidHash(_))));
    }

    #[test]
    fn test_manifest_valid() {
        let m = dummy_manifest();
        assert!(validate_manifest_concurrently(&m).is_ok());
    }

    #[test]
    fn test_manifest_invalid_schema() {
        let mut m = dummy_manifest();
        m.schema = "".to_string();
        assert!(matches!(
            validate_manifest_concurrently(&m),
            Err(Refusal::ValidationFailed(_))
        ));

        m.schema = "other/1.0".to_string();
        assert!(matches!(
            validate_manifest_concurrently(&m),
            Err(Refusal::InvalidFormat(_))
        ));
    }

    #[test]
    fn test_manifest_invalid_release() {
        let mut m = dummy_manifest();
        m.release = "".to_string();
        assert!(matches!(
            validate_manifest_concurrently(&m),
            Err(Refusal::ValidationFailed(_))
        ));
    }

    #[test]
    fn test_manifest_invalid_artifact_name() {
        let mut m = dummy_manifest();
        m.artifacts.push(Artifact {
            name: "".to_string(),
            hash: "a".repeat(64),
            axioms: vec![],
            proven: true,
        });
        assert!(matches!(
            validate_manifest_concurrently(&m),
            Err(Refusal::ValidationFailed(_))
        ));
    }

    #[test]
    fn test_manifest_duplicate_artifact_name() {
        let mut m = dummy_manifest();
        let art = Artifact {
            name: "test".to_string(),
            hash: "a".repeat(64),
            axioms: vec![],
            proven: true,
        };
        m.artifacts.push(art.clone());
        m.artifacts.push(art);
        assert!(matches!(
            validate_manifest_concurrently(&m),
            Err(Refusal::DuplicateElement(_))
        ));
    }

    #[test]
    fn test_manifest_invalid_evidence_subject() {
        let mut m = dummy_manifest();
        m.evidence.push(Evidence {
            subject: "".to_string(),
            kind: "test".to_string(),
            hash: "a".repeat(64),
        });
        assert!(matches!(
            validate_manifest_concurrently(&m),
            Err(Refusal::ValidationFailed(_))
        ));
    }

    #[test]
    fn test_manifest_invalid_evidence_kind() {
        let mut m = dummy_manifest();
        m.evidence.push(Evidence {
            subject: "test".to_string(),
            kind: "".to_string(),
            hash: "a".repeat(64),
        });
        assert!(matches!(
            validate_manifest_concurrently(&m),
            Err(Refusal::ValidationFailed(_))
        ));
    }

    #[test]
    fn test_manifest_duplicate_trusted_base() {
        let mut m = dummy_manifest();
        m.trusted_base = vec!["base1".to_string(), "base1".to_string()];
        assert!(matches!(
            validate_manifest_concurrently(&m),
            Err(Refusal::DuplicateElement(_))
        ));
    }

    #[test]
    fn test_manifest_empty_trusted_base_item() {
        let mut m = dummy_manifest();
        m.trusted_base = vec!["".to_string()];
        assert!(matches!(
            validate_manifest_concurrently(&m),
            Err(Refusal::ValidationFailed(_))
        ));
    }
}
