//! BLAKE3 cryptographic state hashing for graphs.

use oxigraph::model::Quad;

/// Computes the deterministic hash of a set of quads.
pub fn hash_quads(quads: &[Quad]) -> [u8; 32] {
    let canonical = super::canonical::canonicalize_quads(quads);
    let mut hasher = blake3::Hasher::new();
    for q in canonical {
        hasher.update(q.to_string().as_bytes());
        hasher.update(b"\n");
    }
    hasher.finalize().into()
}

/// Computes the hash of a set of additions and deletions (an RDF delta).
pub fn hash_delta(additions: &[String], deletions: &[String]) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    for add in additions {
        hasher.update(b"+");
        hasher.update(add.as_bytes());
    }
    for del in deletions {
        hasher.update(b"-");
        hasher.update(del.as_bytes());
    }
    hasher.finalize().into()
}
