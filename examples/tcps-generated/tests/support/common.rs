use std::fmt;

use serde::Serialize;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EvidenceDigest([u8; 32]);

impl EvidenceDigest {
    pub const fn from_bytes(bytes: [u8; 32]) -> Self {
        Self(bytes)
    }

    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }

    pub fn to_hex(self) -> String {
        hex::encode(self.0)
    }
}

impl fmt::Debug for EvidenceDigest {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&hex::encode(self.0))
    }
}

impl fmt::Display for EvidenceDigest {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&hex::encode(self.0))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EvidenceError {
    InvalidData { identity: String, message: String },
}

impl fmt::Display for EvidenceError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidData { identity, message } => {
                write!(formatter, "INVALID_EVIDENCE:{identity}:{message}")
            }
        }
    }
}

pub fn domain_digest(domain: &str, fields: &[&[u8]]) -> EvidenceDigest {
    let mut hasher = blake3::Hasher::new();
    hasher.update(domain.as_bytes());
    hasher.update(&[0]);
    for field in fields {
        hasher.update(&(field.len() as u64).to_le_bytes());
        hasher.update(field);
    }
    EvidenceDigest(*hasher.finalize().as_bytes())
}

pub fn merkle_root(domain: &str, leaves: &[EvidenceDigest]) -> Result<EvidenceDigest, EvidenceError> {
    if leaves.is_empty() {
        return Err(EvidenceError::InvalidData {
            identity: domain.to_owned(),
            message: "empty Merkle population".to_owned(),
        });
    }

    let mut level = leaves.to_vec();
    while level.len() > 1 {
        let mut next = Vec::with_capacity(level.len().div_ceil(2));
        for pair in level.chunks(2) {
            let right = pair.get(1).copied().unwrap_or(pair[0]);
            next.push(domain_digest(
                &format!("{domain}/node"),
                &[pair[0].as_bytes(), right.as_bytes()],
            ));
        }
        level = next;
    }
    Ok(level[0])
}

pub fn serialize_json<T: Serialize>(value: &T) -> Result<Vec<u8>, EvidenceError> {
    serde_json::to_vec(value).map_err(|error| EvidenceError::InvalidData {
        identity: "json".to_owned(),
        message: error.to_string(),
    })
}
