use serde::{Deserialize, Serialize};
use thiserror::Error;

pub mod receipt;
pub mod validate;
#[derive(Error, Debug)]
pub enum Refusal {
    #[error("Missing expected property: {0}")]
    MissingProperty(String),
    #[error("I/O Error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Serialization Error: {0}")]
    Serialization(#[from] serde_json::Error),
    #[error("Invalid hash length or format: {0}")]
    InvalidHash(String),
    #[error("Validation Error: {0}")]
    ValidationFailed(String),
    #[error("Duplicate Element: {0}")]
    DuplicateElement(String),
    #[error("Invalid Format: {0}")]
    InvalidFormat(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Artifact {
    pub name: String,
    pub hash: String,
    pub axioms: Vec<String>,
    pub proven: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Evidence {
    pub kind: String,
    pub subject: String,
    pub hash: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Manifest {
    pub schema: String,
    pub release: String,
    pub declaration_source: String,
    pub lean_source_origin: String,
    pub trusted_base: Vec<String>,
    pub llm_trusted_base: bool,
    pub scope: String,
    pub run_identifier: String,
    pub quadrature: String,
    pub artifacts: Vec<Artifact>,
    pub evidence: Vec<Evidence>,
    pub stated_not_proven: Vec<String>,
    pub fold_hash: String,
}

pub fn hash_bytes(data: &[u8]) -> String {
    let mut hasher = blake3::Hasher::new();
    hasher.update(data);
    hasher.finalize().to_string()
}

pub fn compute_genesis_fold(release: &str, artifacts: &[Artifact]) -> Result<String, Refusal> {
    let seed = format!("mfact-{}-genesis", release);
    let mut acc = hash_bytes(seed.as_bytes());

    for artifact in artifacts {
        if artifact.hash.is_empty() {
            return Err(Refusal::MissingProperty(format!(
                "Hash missing for artifact {}",
                artifact.name
            )));
        }
        let mut combined = String::new();
        combined.push_str(&acc);
        combined.push_str(&artifact.hash);
        acc = hash_bytes(combined.as_bytes());
    }

    Ok(acc)
}

pub fn parse_manifest(content: &str) -> Result<Manifest, Refusal> {
    let manifest: Manifest = serde_json::from_str(content)?;
    Ok(manifest)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hash_bytes() {
        let h = hash_bytes(b"hello world");
        // Output of `b3sum <<< "hello world"` without newline
        assert_eq!(h.len(), 64);
    }

    #[test]
    fn test_compute_genesis_fold() {
        let artifacts = vec![
            Artifact {
                name: "Decl1".to_string(),
                hash: hash_bytes(b"content1"),
                axioms: vec![],
                proven: true,
            },
            Artifact {
                name: "Decl2".to_string(),
                hash: hash_bytes(b"content2"),
                axioms: vec![],
                proven: false,
            },
        ];

        let acc = compute_genesis_fold("v26.7.7", &artifacts).unwrap();
        assert_eq!(acc.len(), 64);

        // Genesis step logic
        let seed = "mfact-v26.7.7-genesis";
        let mut expected = hash_bytes(seed.as_bytes());
        expected = hash_bytes(format!("{}{}", expected, artifacts[0].hash).as_bytes());
        expected = hash_bytes(format!("{}{}", expected, artifacts[1].hash).as_bytes());

        assert_eq!(acc, expected);
    }

    #[test]
    fn test_compute_genesis_fold_empty_hash() {
        let artifacts = vec![Artifact {
            name: "Decl1".to_string(),
            hash: "".to_string(),
            axioms: vec![],
            proven: true,
        }];
        let result = compute_genesis_fold("v26.7.7", &artifacts);
        assert!(matches!(result, Err(Refusal::MissingProperty(_))));
    }
}
