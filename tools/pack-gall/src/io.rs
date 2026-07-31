use crate::model::{Observation, OBSERVATION_SCHEMA};
use serde::Serialize;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

pub fn write_json<T: Serialize>(path: &Path, value: &T) -> Result<(), String> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .map_err(|e| format!("cannot create {}: {e}", parent.display()))?;
    }
    let bytes = serde_json::to_vec_pretty(value).map_err(|e| e.to_string())?;
    let tmp = path.with_extension("tmp");
    fs::write(&tmp, bytes).map_err(|e| format!("cannot write {}: {e}", tmp.display()))?;
    fs::rename(&tmp, path).map_err(|e| format!("cannot replace {}: {e}", path.display()))?;
    Ok(())
}

pub fn read_observation(path: &Path) -> Result<Observation, String> {
    let bytes = fs::read(path).map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    let observed: Observation = serde_json::from_slice(&bytes)
        .map_err(|e| format!("cannot parse {}: {e}", path.display()))?;
    if observed.schema != OBSERVATION_SCHEMA {
        return Err(format!("unsupported observation schema {}", observed.schema));
    }
    Ok(observed)
}

pub fn parse_args(args: &[String], keys: &[&str]) -> Result<BTreeMap<String, PathBuf>, String> {
    let mut values = BTreeMap::new();
    let mut index = 1;
    while index < args.len() {
        let key = &args[index];
        if !keys.contains(&key.as_str()) {
            return Err(format!("unsupported argument {key}"));
        }
        let value = args
            .get(index + 1)
            .ok_or_else(|| format!("missing value for {key}"))?;
        values.insert(key.clone(), PathBuf::from(value));
        index += 2;
    }
    for key in keys {
        if !values.contains_key(*key) {
            return Err(format!("required argument {key} missing"));
        }
    }
    Ok(values)
}

pub fn digest_bytes(bytes: &[u8]) -> String {
    blake3::hash(bytes).to_hex().to_string()
}

pub fn digest_json<T: Serialize>(value: &T) -> Result<String, String> {
    let bytes = serde_json::to_vec(value).map_err(|e| e.to_string())?;
    Ok(digest_bytes(&bytes))
}

pub fn digest_path(path: &Path) -> Result<String, String> {
    let bytes = fs::read(path).map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    Ok(digest_bytes(&bytes))
}
