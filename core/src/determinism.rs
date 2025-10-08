use std::collections::BTreeMap;

pub fn stable_sort_map<K: Ord, V>(map: &BTreeMap<K, V>) -> Vec<(&K, &V)> {
    map.iter().collect()
}

pub fn canonicalize_string(s: &str) -> String {
    s.to_string()
}

pub fn stable_hash(input: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(input);
    format!("{:x}", hasher.finalize())
}
