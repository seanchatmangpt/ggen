//! `crypto:` namespace builtins (`http://www.w3.org/2000/10/swap/crypto#`).
//!
//! Unary functional builtins: subject is the string (or literal) to hash,
//! object is the lowercase-hex digest string. Backed by the pure-Rust
//! `sha1`/`sha2`/`md5` crates (no C dependencies).

use super::{eval_functional, intern_string, lexical_value, resolve_operand};
use crate::{Binding, Triple};
use sha1::Digest as Sha1Digest;
use sha2::{Sha256, Sha512};

/// `crypto:sha` is cwm/EYE's legacy name for a SHA-1 digest.
pub(crate) const CRYPTO_SHA: &str = "<http://www.w3.org/2000/10/swap/crypto#sha>";
pub(crate) const CRYPTO_SHA256: &str = "<http://www.w3.org/2000/10/swap/crypto#sha256>";
pub(crate) const CRYPTO_SHA512: &str = "<http://www.w3.org/2000/10/swap/crypto#sha512>";
pub(crate) const CRYPTO_MD5: &str = "<http://www.w3.org/2000/10/swap/crypto#md5>";

fn eval_hash(
    pattern: &Triple, bindings: &Binding, digest: impl Fn(&str) -> String,
) -> Option<Binding> {
    eval_functional(pattern, bindings, |pattern, bindings, row| {
        let s = resolve_operand(&pattern.s, bindings, row)?;
        let lex = lexical_value(s)?;
        Some(intern_string(digest(&lex)))
    })
}

fn hex(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{:02x}", b)).collect()
}

pub(crate) fn eval_sha(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_hash(pattern, bindings, |s| {
        let mut hasher = sha1::Sha1::new();
        hasher.update(s.as_bytes());
        hex(&hasher.finalize())
    })
}

pub(crate) fn eval_sha256(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_hash(pattern, bindings, |s| {
        let mut hasher = Sha256::new();
        hasher.update(s.as_bytes());
        hex(&hasher.finalize())
    })
}

pub(crate) fn eval_sha512(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_hash(pattern, bindings, |s| {
        let mut hasher = Sha512::new();
        hasher.update(s.as_bytes());
        hex(&hasher.finalize())
    })
}

pub(crate) fn eval_md5(pattern: &Triple, bindings: &Binding) -> Option<Binding> {
    eval_hash(pattern, bindings, |s| hex(&*md5::compute(s.as_bytes())))
}
