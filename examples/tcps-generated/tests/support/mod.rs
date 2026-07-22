use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};

use serde::Serialize;
use sha2::{Digest as _, Sha256};

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
    Missing { path: PathBuf, message: String },
    Empty { path: PathBuf },
    InvalidUtf8 { path: PathBuf },
    InvalidData { identity: String, message: String },
}

impl fmt::Display for EvidenceError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Missing { path, message } => {
                write!(formatter, "MISSING_ARTIFACT:{}:{message}", path.display())
            }
            Self::Empty { path } => write!(formatter, "EMPTY_ARTIFACT:{}", path.display()),
            Self::InvalidUtf8 { path } => {
                write!(formatter, "INVALID_UTF8:{}", path.display())
            }
            Self::InvalidData { identity, message } => {
                write!(formatter, "INVALID_EVIDENCE:{identity}:{message}")
            }
        }
    }
}

pub fn read_nonempty(root: &Path, relative: &str) -> Result<Vec<u8>, EvidenceError> {
    let path = root.join(relative);
    let bytes = fs::read(&path).map_err(|error| EvidenceError::Missing {
        path: path.clone(),
        message: error.to_string(),
    })?;
    if bytes.is_empty() {
        return Err(EvidenceError::Empty { path });
    }
    Ok(bytes)
}

pub fn utf8<'a>(relative: &str, bytes: &'a [u8]) -> Result<&'a str, EvidenceError> {
    std::str::from_utf8(bytes).map_err(|_| EvidenceError::InvalidUtf8 {
        path: PathBuf::from(relative),
    })
}

pub fn sha256_hex(bytes: &[u8]) -> String {
    hex::encode(Sha256::digest(bytes))
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

pub fn rendezvous_shard(identity: &str, shard_count: usize) -> Result<usize, EvidenceError> {
    if shard_count == 0 {
        return Err(EvidenceError::InvalidData {
            identity: identity.to_owned(),
            message: "zero shard count".to_owned(),
        });
    }

    let mut winner = 0_usize;
    let mut winner_score = [0_u8; 32];
    for shard in 0..shard_count {
        let shard_bytes = (shard as u64).to_le_bytes();
        let score = domain_digest("tcps/rendezvous/v1", &[identity.as_bytes(), &shard_bytes]);
        if shard == 0 || score.as_bytes() > &winner_score {
            winner = shard;
            winner_score = *score.as_bytes();
        }
    }
    Ok(winner)
}

pub fn serialize_json<T: Serialize>(value: &T) -> Result<Vec<u8>, EvidenceError> {
    serde_json::to_vec(value).map_err(|error| EvidenceError::InvalidData {
        identity: "json".to_owned(),
        message: error.to_string(),
    })
}
