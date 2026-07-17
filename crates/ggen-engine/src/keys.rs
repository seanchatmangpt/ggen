//! Ed25519 signing/verifying-key resolution for receipt signing
//! (specs/014-ggen-core-replacement, T063).
//!
//! # Key policy (precedence order, identical for signing and verifying)
//!
//! 1. `GGEN_SIGNING_KEY` environment variable — 64 lowercase/uppercase hex
//!    characters (the 32-byte ed25519 secret seed). A malformed value (wrong
//!    length, non-hex characters) is a hard error: it never silently falls
//!    through to the file or a fresh key.
//! 2. `<project_root>/.ggen/keys/signing.key` — same hex-seed format. Read if
//!    present; a malformed file is a hard error.
//! 3. Neither present (signing only — verification never generates a key,
//!    see [`resolve_verifying_key`]): generate a fresh ed25519 keypair, write
//!    `.ggen/keys/signing.key` and `.ggen/keys/verifying.key` (creating
//!    `.ggen/keys/` if needed), and use the fresh key.
//!
//! An existing `signing.key` is **never overwritten** — key generation uses
//! `OpenOptions::create_new` so a losing race falls back to reading whatever
//! the winner just wrote, rather than clobbering it. On Unix, `signing.key`
//! is written with `0o600` permissions (best-effort; not enforced on
//! non-Unix targets).

use std::path::{Path, PathBuf};

use ed25519_dalek::SigningKey;

use crate::error::{AppError, Result};

/// Environment variable carrying a hex-encoded ed25519 seed, taking
/// precedence over the on-disk key file.
pub(crate) const GGEN_SIGNING_KEY_ENV: &str = "GGEN_SIGNING_KEY";

const KEYS_DIR_REL: &str = ".ggen/keys";
const SIGNING_KEY_FILE: &str = "signing.key";
const VERIFYING_KEY_FILE: &str = "verifying.key";

fn keys_dir(project_root: &Path) -> PathBuf {
    project_root.join(KEYS_DIR_REL)
}

/// Decode a 64-lowercase/uppercase-hex-character string into a 32-byte
/// ed25519 seed/key. `source` names the origin (env var or file path) for a
/// precise error message — never a silent fallback.
fn decode_key_hex(source: &str, raw: &str) -> Result<[u8; 32]> {
    let trimmed = raw.trim();
    let bytes = hex::decode(trimmed).map_err(|e| {
        AppError::fm_key(
            1,
            format!(
                "{source}: not valid hex ({e}). \
                 Remediation: the key must be exactly 64 hex characters (32 bytes)."
            ),
        )
    })?;
    bytes.try_into().map_err(|v: Vec<u8>| {
        AppError::fm_key(
            2,
            format!(
                "{source}: expected a 32-byte ed25519 key (64 hex chars), got {} bytes. \
                 Remediation: regenerate or re-copy the key -- it must be exactly 64 hex characters.",
                v.len()
            ),
        )
    })
}

/// Resolve the ed25519 signing key per the key policy documented on this
/// module: `GGEN_SIGNING_KEY` env var, else `.ggen/keys/signing.key`, else
/// generate-and-persist a fresh keypair under `.ggen/keys/`.
///
/// # Errors
/// - `GGEN_SIGNING_KEY` is set but malformed: hard error, never falls back.
/// - `.ggen/keys/signing.key` exists but is malformed: hard error.
/// - Any I/O failure creating `.ggen/keys/` or persisting a fresh keypair.
pub(crate) fn resolve_signing_key(project_root: &Path) -> Result<SigningKey> {
    if let Ok(env_hex) = std::env::var(GGEN_SIGNING_KEY_ENV) {
        let seed = decode_key_hex(&format!("{GGEN_SIGNING_KEY_ENV} env var"), &env_hex)?;
        return Ok(SigningKey::from_bytes(&seed));
    }

    let signing_path = keys_dir(project_root).join(SIGNING_KEY_FILE);
    match std::fs::read_to_string(&signing_path) {
        Ok(raw) => {
            let seed = decode_key_hex(&signing_path.display().to_string(), &raw)?;
            Ok(SigningKey::from_bytes(&seed))
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            generate_and_persist_keypair(project_root)
        }
        Err(e) => Err(AppError::fm_key(
            3,
            format!("signing key `{}` unreadable: {e}", signing_path.display()),
        )),
    }
}

/// Resolve the ed25519 verifying (public) key with the same precedence used
/// for signing: `GGEN_SIGNING_KEY` (deriving the public key from the same
/// seed, so a caller signing via the env var can also verify via it), else
/// `.ggen/keys/verifying.key`.
///
/// Unlike [`resolve_signing_key`], this never generates a fresh keypair --
/// verifying against a nonexistent key is a hard error naming the missing
/// file/env var, never a silently-generated new key (which would make every
/// prior signature unverifiable without ever telling the caller why).
///
/// # Errors
/// - `GGEN_SIGNING_KEY` is set but malformed: hard error.
/// - Neither the env var nor `.ggen/keys/verifying.key` is available/valid.
pub(crate) fn resolve_verifying_key(project_root: &Path) -> Result<ed25519_dalek::VerifyingKey> {
    if let Ok(env_hex) = std::env::var(GGEN_SIGNING_KEY_ENV) {
        let seed = decode_key_hex(&format!("{GGEN_SIGNING_KEY_ENV} env var"), &env_hex)?;
        return Ok(SigningKey::from_bytes(&seed).verifying_key());
    }

    let verifying_path = keys_dir(project_root).join(VERIFYING_KEY_FILE);
    let raw = std::fs::read_to_string(&verifying_path).map_err(|e| {
        AppError::fm_key(
            4,
            format!(
                "verifying key `{}` unreadable: {e}. \
                 Remediation: run a real (non-dry-run) sync first to generate `.ggen/keys/`, \
                 or set GGEN_SIGNING_KEY.",
                verifying_path.display()
            ),
        )
    })?;
    let bytes = decode_key_hex(&verifying_path.display().to_string(), &raw)?;
    ed25519_dalek::VerifyingKey::from_bytes(&bytes).map_err(|e| {
        AppError::fm_key(
            5,
            format!(
                "verifying key `{}` is not a valid ed25519 public key: {e}",
                verifying_path.display()
            ),
        )
    })
}

/// Generate a fresh ed25519 keypair and persist it under
/// `<project_root>/.ggen/keys/`. Never overwrites an existing
/// `signing.key`: uses `create_new` so a concurrent racer's file wins and
/// this call falls back to reading it instead of clobbering it.
fn generate_and_persist_keypair(project_root: &Path) -> Result<SigningKey> {
    let dir = keys_dir(project_root);
    std::fs::create_dir_all(&dir)
        .map_err(|e| AppError::fm_key(6, format!("cannot create `{}`: {e}", dir.display())))?;

    let signing_path = dir.join(SIGNING_KEY_FILE);
    let verifying_path = dir.join(VERIFYING_KEY_FILE);

    let mut csprng = rand::rngs::OsRng;
    let signing_key = SigningKey::generate(&mut csprng);
    let signing_hex = hex::encode(signing_key.to_bytes());
    let verifying_hex = hex::encode(signing_key.verifying_key().to_bytes());

    match write_new_file(&signing_path, signing_hex.as_bytes()) {
        Ok(()) => {
            restrict_to_owner(&signing_path)?;
            // Best-effort: if verifying.key already exists (e.g. from a
            // previous partial run) leave it; otherwise write the matching
            // public key. Losing this specific race is harmless -- an
            // absent/mismatched verifying.key surfaces as a clear
            // `resolve_verifying_key` error, never a silent wrong-key verify.
            if let Err(e) = write_new_file(&verifying_path, verifying_hex.as_bytes()) {
                if e.kind() != std::io::ErrorKind::AlreadyExists {
                    return Err(AppError::fm_key(
                        7,
                        format!("cannot write `{}`: {e}", verifying_path.display()),
                    ));
                }
            }
            Ok(signing_key)
        }
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {
            // Lost the race: another process/run created signing.key first.
            // Never overwrite it -- read whatever it actually contains.
            let raw = std::fs::read_to_string(&signing_path).map_err(|e| {
                AppError::fm_key(
                    3,
                    format!("signing key `{}` unreadable: {e}", signing_path.display()),
                )
            })?;
            let seed = decode_key_hex(&signing_path.display().to_string(), &raw)?;
            Ok(SigningKey::from_bytes(&seed))
        }
        Err(e) => Err(AppError::fm_key(
            8,
            format!("cannot write `{}`: {e}", signing_path.display()),
        )),
    }
}

/// Write `contents` to `path` only if `path` does not already exist
/// (`O_CREAT | O_EXCL` semantics via [`std::fs::OpenOptions::create_new`]) --
/// the primitive that gives "never overwrite an existing key file" an actual
/// filesystem-level guarantee rather than a check-then-write race.
fn write_new_file(path: &Path, contents: &[u8]) -> std::io::Result<()> {
    use std::io::Write as _;
    let mut f = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)?;
    f.write_all(contents)
}

/// Best-effort `0o600` (owner read/write only) on Unix; a no-op on other
/// targets (never blocks key generation on non-Unix support).
#[cfg(unix)]
fn restrict_to_owner(path: &Path) -> Result<()> {
    use std::os::unix::fs::PermissionsExt as _;
    std::fs::set_permissions(path, std::fs::Permissions::from_mode(0o600)).map_err(|e| {
        AppError::fm_key(
            9,
            format!("cannot set permissions on `{}`: {e}", path.display()),
        )
    })
}

#[cfg(not(unix))]
fn restrict_to_owner(_path: &Path) -> Result<()> {
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Guards serializing every test in this module that touches the
    /// process-global `GGEN_SIGNING_KEY` env var (std::env is process-wide;
    /// concurrent test threads mutating it would race).
    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
        LOCK.get_or_init(|| std::sync::Mutex::new(()))
            .lock()
            .unwrap_or_else(|e| e.into_inner())
    }

    fn clear_env() {
        std::env::remove_var(GGEN_SIGNING_KEY_ENV);
    }

    #[test]
    fn generates_and_persists_a_fresh_keypair_on_first_resolve() {
        let _guard = env_lock();
        clear_env();
        let dir = tempfile::tempdir().expect("tempdir");

        let key = resolve_signing_key(dir.path()).expect("resolve");

        let signing_path = dir.path().join(".ggen/keys/signing.key");
        let verifying_path = dir.path().join(".ggen/keys/verifying.key");
        assert!(signing_path.exists());
        assert!(verifying_path.exists());

        let persisted_hex = std::fs::read_to_string(&signing_path).expect("read");
        assert_eq!(persisted_hex.trim(), hex::encode(key.to_bytes()));

        let verifying_hex = std::fs::read_to_string(&verifying_path).expect("read");
        assert_eq!(
            verifying_hex.trim(),
            hex::encode(key.verifying_key().to_bytes())
        );
    }

    #[test]
    fn never_overwrites_an_existing_signing_key_file() {
        let _guard = env_lock();
        clear_env();
        let dir = tempfile::tempdir().expect("tempdir");
        let keys_dir = dir.path().join(".ggen/keys");
        std::fs::create_dir_all(&keys_dir).expect("mkdir");
        let fixed_seed_hex = "ab".repeat(32);
        std::fs::write(keys_dir.join("signing.key"), &fixed_seed_hex).expect("write");

        let key = resolve_signing_key(dir.path()).expect("resolve");

        assert_eq!(hex::encode(key.to_bytes()), fixed_seed_hex);
        // The file on disk must be untouched (same content, not regenerated).
        let after = std::fs::read_to_string(keys_dir.join("signing.key")).expect("read");
        assert_eq!(after, fixed_seed_hex);
    }

    #[test]
    fn env_var_takes_precedence_over_the_file() {
        let _guard = env_lock();
        let dir = tempfile::tempdir().expect("tempdir");
        let keys_dir = dir.path().join(".ggen/keys");
        std::fs::create_dir_all(&keys_dir).expect("mkdir");
        std::fs::write(keys_dir.join("signing.key"), "cd".repeat(32)).expect("write");

        let env_seed_hex = "ef".repeat(32);
        std::env::set_var(GGEN_SIGNING_KEY_ENV, &env_seed_hex);
        let result = resolve_signing_key(dir.path());
        clear_env();

        let key = result.expect("resolve");
        assert_eq!(hex::encode(key.to_bytes()), env_seed_hex);
    }

    #[test]
    fn malformed_env_var_is_a_hard_error_never_falls_back_to_file() {
        let _guard = env_lock();
        let dir = tempfile::tempdir().expect("tempdir");
        let keys_dir = dir.path().join(".ggen/keys");
        std::fs::create_dir_all(&keys_dir).expect("mkdir");
        std::fs::write(keys_dir.join("signing.key"), "cd".repeat(32)).expect("write");

        std::env::set_var(GGEN_SIGNING_KEY_ENV, "not-hex-and-wrong-length");
        let result = resolve_signing_key(dir.path());
        clear_env();

        assert!(result.is_err(), "malformed env var must be a hard error");
    }

    #[test]
    fn malformed_key_file_is_a_hard_error() {
        let _guard = env_lock();
        clear_env();
        let dir = tempfile::tempdir().expect("tempdir");
        let keys_dir = dir.path().join(".ggen/keys");
        std::fs::create_dir_all(&keys_dir).expect("mkdir");
        std::fs::write(keys_dir.join("signing.key"), "too-short").expect("write");

        let result = resolve_signing_key(dir.path());
        assert!(result.is_err(), "malformed key file must be a hard error");
    }

    #[test]
    fn resolve_verifying_key_never_generates_and_errors_when_absent() {
        let _guard = env_lock();
        clear_env();
        let dir = tempfile::tempdir().expect("tempdir");

        let result = resolve_verifying_key(dir.path());

        assert!(result.is_err());
        assert!(
            !dir.path().join(".ggen/keys/verifying.key").exists(),
            "verify must never generate a key as a side effect"
        );
    }

    #[test]
    fn signing_and_verifying_keys_from_the_same_resolve_match() {
        let _guard = env_lock();
        clear_env();
        let dir = tempfile::tempdir().expect("tempdir");

        let signing = resolve_signing_key(dir.path()).expect("resolve signing");
        let verifying = resolve_verifying_key(dir.path()).expect("resolve verifying");

        assert_eq!(verifying, signing.verifying_key());
    }

    #[cfg(unix)]
    #[test]
    fn signing_key_file_is_written_with_owner_only_permissions() {
        use std::os::unix::fs::PermissionsExt as _;
        let _guard = env_lock();
        clear_env();
        let dir = tempfile::tempdir().expect("tempdir");

        resolve_signing_key(dir.path()).expect("resolve");

        let meta = std::fs::metadata(dir.path().join(".ggen/keys/signing.key")).expect("meta");
        assert_eq!(meta.permissions().mode() & 0o777, 0o600);
    }
}
