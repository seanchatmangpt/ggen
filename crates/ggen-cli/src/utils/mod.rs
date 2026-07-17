//! Shared utilities native to `ggen-cli`.
//!
//! This module currently holds the generic string-message [`error`] type used
//! pervasively as the CLI's internal `Result<T>` before commands map it to
//! `clap_noun_verb::NounVerbError`/`crate::error::GgenError` at the verb boundary.
//!
//! Ported from `ggen-core/src/utils/error.rs` during the ggen-core retirement
//! migration (`specs/014-ggen-core-replacement`, task T035): this type has zero
//! counterpart in `ggen-config`/`ggen-marketplace`/`ggen-engine`, so it moves into
//! its sole remaining consumer rather than being invented as new shared surface.

pub mod error;

pub use error::{Context, Error, Result};

/// Hex-encoded SHA-256 digest of `data`.
///
/// Ported (as a single 4-line function, not a whole module) from
/// `ggen-core/src/pqc.rs::calculate_sha256` during the ggen-core retirement
/// migration (task T039): `cmds/capability.rs` and `cmds/packs.rs` both use it to
/// pin a placeholder-version lockfile entry's `integrity` field. The rest of
/// `pqc.rs` (`PqcSigner`/`PqcVerifier`, post-quantum signing) has no caller in
/// `ggen-cli` and is out of scope here.
#[must_use]
pub fn sha256_hex(data: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(data);
    format!("{:x}", hasher.finalize())
}
