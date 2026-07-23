use std::fs;
use std::path::Path;

use sha1::Sha1;
use sha2::{Digest as _, Sha256};

use super::common::EvidenceError;

pub fn read_nonempty(root: &Path, relative: &str) -> Result<Vec<u8>, EvidenceError> {
    let path = root.join(relative);
    let bytes = fs::read(&path).map_err(|error| EvidenceError::InvalidData {
        identity: relative.to_owned(),
        message: format!("MISSING_ARTIFACT:{}:{error}", path.display()),
    })?;
    if bytes.is_empty() {
        return Err(EvidenceError::InvalidData {
            identity: relative.to_owned(),
            message: format!("EMPTY_ARTIFACT:{}", path.display()),
        });
    }
    Ok(bytes)
}

pub fn utf8<'a>(relative: &str, bytes: &'a [u8]) -> Result<&'a str, EvidenceError> {
    std::str::from_utf8(bytes).map_err(|error| EvidenceError::InvalidData {
        identity: relative.to_owned(),
        message: format!("INVALID_UTF8:{error}"),
    })
}

pub fn sha256_hex(bytes: &[u8]) -> String {
    hex::encode(Sha256::digest(bytes))
}

pub fn git_blob_oid(bytes: &[u8]) -> String {
    let header = format!("blob {}\0", bytes.len());
    let mut hasher = Sha1::new();
    hasher.update(header.as_bytes());
    hasher.update(bytes);
    hex::encode(hasher.finalize())
}
