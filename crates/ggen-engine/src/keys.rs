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
//!
//! # Write-failure / partial-key self-healing
//!
//! If a key write fails partway through (disk full, interrupted, killed
//! process, ...), [`write_new_file`] removes the partially-written file
//! before propagating the error, and `fsync`s (`File::sync_all`) a
//! successful write before returning -- so a failed or interrupted write
//! never leaves a 0-byte/partial key file behind for a future run to trip
//! over. As a second line of defense, if [`resolve_signing_key`] ever finds
//! a pre-existing `signing.key` that is present but empty (the signature of
//! an old poisoned file from before this self-healing existed, or of a
//! write that failed by some other means), it deletes the file and returns
//! a [`FM-KEY-002`] error explaining that -- so the file is gone and the
//! *next* call regenerates a fresh keypair instead of refusing forever.
//!
//! # Stale-verifying-key detection (`FM-KEY-010`/`FM-KEY-011`)
//!
//! `generate_and_persist_keypair` only ever runs when `signing.key` was
//! *missing* (see [`resolve_signing_key`]). If `verifying.key` is already
//! present in that state, it can only be one of two things: a benign
//! concurrent-writer race (another in-flight call generated a keypair and
//! wrote its own matching `verifying.key` first), or a stale public key
//! left behind by an *older, unrelated* keypair whose `signing.key` was
//! since deleted or lost. Treating both cases as harmless -- as a bare
//! `AlreadyExists` check on the write does -- would silently pair the
//! brand-new signing key with the old verifying key on disk: exactly the
//! forbidden "Regenerating signing key without rotating verifying key"
//! anti-pattern (`docs/DEFINITION_OF_DONE_RELEASE.json`), and
//! [`resolve_verifying_key`] cannot catch it after the fact since it only
//! checks that the file is present and well-formed, never that it
//! corresponds to any particular signing key. So before accepting the
//! `AlreadyExists` as harmless, the freshly-generated signing key's
//! derived public key is compared byte-for-byte against what is actually
//! on disk; only an exact match is treated as the benign race. A mismatch
//! (or a pre-existing `verifying.key` that cannot even be read/decoded) is
//! a hard [`FM-KEY-011`] (or [`FM-KEY-010`]) error, and the just-written
//! `signing.key` is rolled back so the project is left in the same
//! recoverable, pre-call state (`signing.key` absent) rather than an
//! orphaned new signing key silently masking the problem on every
//! subsequent call.

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
        Ok(raw) => match decode_key_hex(&signing_path.display().to_string(), &raw) {
            Ok(seed) => Ok(SigningKey::from_bytes(&seed)),
            Err(_) if raw.trim().is_empty() => {
                // Self-heal: an empty/0-byte signing.key is the signature
                // of a previous key-generation write that failed partway
                // through and left a poisoned file behind (write_new_file
                // now cleans up *new* write failures itself -- this branch
                // covers files poisoned before that fix existed, or by any
                // other means). Delete it now so the file is gone and the
                // *next* resolve call regenerates a fresh keypair instead
                // of refusing forever with the same [FM-KEY-002].
                let _ = std::fs::remove_file(&signing_path);
                Err(AppError::fm_key(
                    2,
                    format!(
                        "signing key `{}` was present but empty/invalid (likely a partial \
                         write left behind by an earlier interrupted key generation). It has \
                         been deleted -- re-run to generate a fresh key.",
                        signing_path.display()
                    ),
                ))
            }
            Err(e) => Err(e),
        },
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
    let mut csprng = rand::rngs::OsRng;
    let signing_key = SigningKey::generate(&mut csprng);
    persist_keypair(project_root, signing_key)
}

/// Persist `signing_key` under `<project_root>/.ggen/keys/`, writing the
/// matching `verifying.key` alongside it. Split out from
/// [`generate_and_persist_keypair`] purely so the write/mismatch-detection
/// logic below can be exercised directly against a caller-chosen key in
/// tests, independent of `OsRng` -- this is the exact code path
/// `generate_and_persist_keypair` calls with a randomly generated key, not
/// a test-only branch or a mock.
///
/// See the module-level "Stale-verifying-key detection" doc for why an
/// `AlreadyExists` on `verifying.key` is not, by itself, treated as
/// harmless.
fn persist_keypair(project_root: &Path, signing_key: SigningKey) -> Result<SigningKey> {
    let dir = keys_dir(project_root);
    std::fs::create_dir_all(&dir)
        .map_err(|e| AppError::fm_key(6, format!("cannot create `{}`: {e}", dir.display())))?;

    let signing_path = dir.join(SIGNING_KEY_FILE);
    let verifying_path = dir.join(VERIFYING_KEY_FILE);

    let signing_hex = hex::encode(signing_key.to_bytes());
    let verifying_hex = hex::encode(signing_key.verifying_key().to_bytes());

    match write_new_file(&signing_path, signing_hex.as_bytes()) {
        Ok(()) => {
            restrict_to_owner(&signing_path)?;
            if let Err(e) = write_new_file(&verifying_path, verifying_hex.as_bytes()) {
                if e.kind() != std::io::ErrorKind::AlreadyExists {
                    return Err(AppError::fm_key(
                        7,
                        format!("cannot write `{}`: {e}", verifying_path.display()),
                    ));
                }
                // verifying.key already exists. Confirm it actually derives
                // from the signing key we just wrote before treating this
                // as the benign concurrent-writer race -- never assume a
                // same-named file on disk is the right one.
                if let Err(mismatch_err) =
                    confirm_verifying_key_matches(&verifying_path, &signing_key)
                {
                    // Roll back the brand-new signing.key so the project is
                    // left in the same recoverable state it was in before
                    // this call (signing.key absent) instead of an orphaned
                    // new signing key silently masking the mismatch on
                    // every subsequent resolve.
                    let _ = std::fs::remove_file(&signing_path);
                    return Err(mismatch_err);
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

/// Confirm a pre-existing `verifying.key` at `verifying_path` actually
/// derives from `signing_key` -- called only when writing a freshly
/// generated `signing_key`'s matching verifying key lost a race to a file
/// that was already there. `[FM-KEY-010]` covers the file being unreadable
/// or not a valid ed25519 public key at all (cannot even attempt the
/// comparison); `[FM-KEY-011]` covers a well-formed key that provably
/// belongs to a different keypair.
fn confirm_verifying_key_matches(verifying_path: &Path, signing_key: &SigningKey) -> Result<()> {
    let raw = std::fs::read_to_string(verifying_path).map_err(|e| {
        AppError::fm_key(
            10,
            format!(
                "a new signing key was generated, but the pre-existing `{}` could not be read \
                 back to confirm it matches ({e}). Remediation: inspect `{}` manually -- \
                 delete it if stale (a matching verifying.key will be written on the next \
                 resolve) or restore the original signing.key instead of letting a new one be \
                 generated.",
                verifying_path.display(),
                verifying_path.display(),
            ),
        )
    })?;
    let existing_bytes = decode_key_hex(&verifying_path.display().to_string(), &raw)?;
    let existing = ed25519_dalek::VerifyingKey::from_bytes(&existing_bytes).map_err(|e| {
        AppError::fm_key(
            10,
            format!(
                "a new signing key was generated, but the pre-existing `{}` is not a valid \
                 ed25519 public key ({e}), so it cannot be confirmed to match. Remediation: \
                 inspect `{}` manually -- delete it if stale or restore the original \
                 signing.key instead of letting a new one be generated.",
                verifying_path.display(),
                verifying_path.display(),
            ),
        )
    })?;

    if existing != signing_key.verifying_key() {
        return Err(AppError::fm_key(
            11,
            format!(
                "a new signing key was generated, but the existing `{}` belongs to a \
                 DIFFERENT keypair -- its public key does not match the new signing key's \
                 derived public key. Left unresolved, every future `ggen receipt verify` \
                 would fail with a generic 'signature invalid' error, with no indication the \
                 real cause is this out-of-sync keypair. Remediation: either delete the stale \
                 `{}` if the original signing.key is truly gone (a fresh, matching \
                 verifying.key will be written automatically on the next resolve), or restore \
                 the original signing.key that matches this verifying.key instead of letting a \
                 new one be generated.",
                verifying_path.display(),
                verifying_path.display(),
            ),
        ));
    }
    Ok(())
}

/// Write `contents` to `path` only if `path` does not already exist
/// (`O_CREAT | O_EXCL` semantics via [`std::fs::OpenOptions::create_new`]) --
/// the primitive that gives "never overwrite an existing key file" an actual
/// filesystem-level guarantee rather than a check-then-write race.
///
/// On a write or fsync failure (disk full, interrupted, ...), the
/// partially-written file that `create_new` already put on disk is removed
/// before the error is propagated -- otherwise a 0-byte/partial key file
/// would be left behind, permanently poisoning every future resolve with an
/// unrecoverable `[FM-KEY-002]` since nothing else would ever delete it. On
/// success, the file is `fsync`'d (`File::sync_all`) before returning so the
/// key is durably on disk -- protecting against a process kill between
/// `write_all` returning and the data actually reaching disk, a different
/// corruption mode than the write failing outright.
fn write_new_file(path: &Path, contents: &[u8]) -> std::io::Result<()> {
    use std::io::Write as _;
    let mut f = std::fs::OpenOptions::new()
        .write(true)
        .create_new(true)
        .open(path)?;
    match f.write_all(contents).and_then(|()| f.sync_all()) {
        Ok(()) => Ok(()),
        Err(e) => {
            let _ = std::fs::remove_file(path);
            Err(e)
        }
    }
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
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests {
    use super::*;

    /// Guards serializing every test in this module that touches the
    /// process-global `GGEN_SIGNING_KEY` env var (`std::env` is process-wide;
    /// concurrent test threads mutating it would race).
    fn env_lock() -> std::sync::MutexGuard<'static, ()> {
        static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
        LOCK.get_or_init(|| std::sync::Mutex::new(()))
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
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

    /// Reproduces the exact permanent-poisoning symptom this module used to
    /// have: a `signing.key` left 0 bytes by an earlier interrupted write
    /// (disk full, process killed mid-write, ...) used to make every future
    /// `resolve_signing_key` call fail forever with `[FM-KEY-002]`, since
    /// nothing ever deleted the bad file. This constructs that exact
    /// precondition directly (a 0-byte `signing.key`, matching real
    /// `write_new_file` failure output byte-for-byte) and drives the real
    /// recovery path: (a) the self-heal in `resolve_signing_key` deletes the
    /// poisoned file and reports why in the `[FM-KEY-002]` error, and (b) a
    /// *subsequent* `resolve_signing_key` call on the same project root
    /// succeeds and persists a fresh, valid keypair -- no manual `rm`
    /// required.
    #[test]
    fn poisoned_zero_byte_signing_key_self_heals_on_next_resolve() {
        let _guard = env_lock();
        clear_env();
        let dir = tempfile::tempdir().expect("tempdir");
        let keys_dir = dir.path().join(".ggen/keys");
        std::fs::create_dir_all(&keys_dir).expect("mkdir");
        let signing_path = keys_dir.join("signing.key");
        // The exact poisoned precondition: create_new succeeded, write_all
        // never got (or never finished) writing any bytes.
        std::fs::write(&signing_path, b"").expect("write empty file");
        assert_eq!(
            std::fs::metadata(&signing_path).expect("meta").len(),
            0,
            "precondition: signing.key must be a real 0-byte file on disk"
        );

        // First call: self-heals -- deletes the poisoned file, still fails
        // *this* call (no valid key existed to return), but the error
        // explains the file was invalid and has been removed.
        let first = resolve_signing_key(dir.path());
        let err_msg = first
            .expect_err("0-byte signing.key must fail the call that finds it")
            .to_string();
        assert!(
            err_msg.contains("[FM-KEY-002]"),
            "expected [FM-KEY-002], got: {err_msg}"
        );
        assert!(
            err_msg.contains("empty") && err_msg.contains("deleted"),
            "error should explain the file was empty/invalid and has been deleted, got: {err_msg}"
        );
        assert!(
            !signing_path.exists(),
            "self-heal must actually remove the poisoned 0-byte file from disk"
        );

        // Second call on the same project root: no manual intervention
        // happened in between -- this must now succeed and persist a fresh,
        // valid keypair, proving the project is no longer permanently
        // poisoned.
        let second =
            resolve_signing_key(dir.path()).expect("second resolve must self-heal and succeed");
        assert_eq!(second.to_bytes().len(), 32);
        let persisted = std::fs::read_to_string(&signing_path).expect("read regenerated key");
        assert_eq!(
            persisted.trim(),
            hex::encode(second.to_bytes()),
            "regenerated signing.key on disk must match the key just resolved"
        );
        assert_eq!(
            std::fs::metadata(&signing_path).expect("meta").len(),
            64,
            "regenerated signing.key must be a full 64-hex-char key, not another 0-byte file"
        );
    }

    /// Scoping guard: self-healing must only trigger for a genuinely
    /// empty/0-byte file (the specific signature of an interrupted write).
    /// A non-empty-but-malformed file (e.g. hand-edited or truncated mid
    /// content) is a different failure -- almost certainly a human/config
    /// mistake, not write-failure poisoning -- and must stay a hard error
    /// that a human investigates, not something silently deleted.
    #[test]
    fn non_empty_malformed_signing_key_is_not_auto_deleted() {
        let _guard = env_lock();
        clear_env();
        let dir = tempfile::tempdir().expect("tempdir");
        let keys_dir = dir.path().join(".ggen/keys");
        std::fs::create_dir_all(&keys_dir).expect("mkdir");
        let signing_path = keys_dir.join("signing.key");
        std::fs::write(&signing_path, "too-short").expect("write");

        let result = resolve_signing_key(dir.path());

        assert!(
            result.is_err(),
            "malformed key file must still be a hard error"
        );
        assert!(
            signing_path.exists(),
            "a non-empty malformed key file must NOT be auto-deleted -- only the \
             0-byte/write-failure case self-heals"
        );
        let after = std::fs::read_to_string(&signing_path).expect("read");
        assert_eq!(after, "too-short", "file content must be untouched");
    }

    /// `write_new_file` must not leave a target file behind on any failure
    /// path. This drives it through a real (non-mocked) I/O failure -- a
    /// read-only parent directory, so `create_new`'s `open()` itself is
    /// refused by the OS (`EACCES`) -- and asserts nothing was created.
    ///
    /// Note: this exercises `write_new_file`'s pre-existing `open()?`
    /// failure, not the *new* write/fsync-failure cleanup branch added by
    /// this change (that branch requires `open()` to succeed and then
    /// `write_all`/`sync_all` to fail after the file already exists on
    /// disk -- e.g. a real disk-full or killed-mid-write condition -- which
    /// has no portable, deterministic, dependency-free way to trigger from
    /// a unit test on this platform). The 0-byte-file self-heal tests above
    /// cover the resulting real-world symptom (a poisoned `signing.key`
    /// left by exactly that kind of failure) end to end via its actual
    /// on-disk precondition.
    #[cfg(unix)]
    #[test]
    fn write_new_file_leaves_no_file_behind_when_open_fails() {
        use std::os::unix::fs::PermissionsExt as _;

        let _guard = env_lock();
        let dir = tempfile::tempdir().expect("tempdir");
        let target = dir.path().join("signing.key");

        let original_mode = std::fs::metadata(dir.path())
            .expect("meta")
            .permissions()
            .mode();
        std::fs::set_permissions(dir.path(), std::fs::Permissions::from_mode(0o555))
            .expect("make dir read-only");

        let result = write_new_file(&target, b"deadbeef");

        // Restore permissions before any assertion can panic and leak a
        // non-writable tempdir past the test.
        std::fs::set_permissions(dir.path(), std::fs::Permissions::from_mode(original_mode))
            .expect("restore dir permissions");

        assert!(
            result.is_err(),
            "write into a read-only directory must fail"
        );
        assert!(
            !target.exists(),
            "no file must be left behind when write_new_file fails"
        );
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

    /// Reproduces the exact security bug this change fixes: `signing.key` is
    /// missing (so `resolve_signing_key` must generate a brand-new random
    /// keypair) but `verifying.key` from a DIFFERENT, older/unrelated
    /// keypair is already sitting on disk. Before this fix,
    /// `generate_and_persist_keypair` treated the `AlreadyExists` on
    /// `verifying.key` as an always-harmless race and returned the new
    /// signing key without ever checking it against the pre-existing
    /// verifying key -- silently producing a keypair whose two halves do
    /// not match, which would only ever surface later as an opaque
    /// "signature invalid" from `ggen receipt verify`. This is the exact
    /// forbidden anti-pattern named in
    /// `docs/DEFINITION_OF_DONE_RELEASE.json`: "Regenerating signing key
    /// without rotating verifying key (breaks verification)".
    #[test]
    fn generate_and_persist_keypair_rejects_a_stale_verifying_key_from_a_different_keypair() {
        let _guard = env_lock();
        clear_env();
        let dir = tempfile::tempdir().expect("tempdir");
        let keys_dir = dir.path().join(".ggen/keys");
        std::fs::create_dir_all(&keys_dir).expect("mkdir");

        // An older/unrelated keypair's public half is on disk. Its
        // signing.key is absent -- matching the exact real-world
        // precondition this bug requires (generate_and_persist_keypair is
        // only ever reached when signing.key is NotFound; see
        // resolve_signing_key).
        let stale_seed = [0xAAu8; 32];
        let stale_signing_key = SigningKey::from_bytes(&stale_seed);
        let verifying_path = keys_dir.join("verifying.key");
        std::fs::write(
            &verifying_path,
            hex::encode(stale_signing_key.verifying_key().to_bytes()),
        )
        .expect("write stale verifying.key");

        // No signing.key exists -> resolve_signing_key generates a
        // brand-new random keypair. Its derived public key will not match
        // the stale one already on disk (collision probability 2^-256).
        let result = resolve_signing_key(dir.path());

        let err_msg = result
            .expect_err(
                "a stale, non-matching verifying.key must be a hard error, never a silent \
                 mismatched keypair",
            )
            .to_string();
        assert!(
            err_msg.contains("[FM-KEY-011]"),
            "expected [FM-KEY-011], got: {err_msg}"
        );
        assert!(
            err_msg.contains("DIFFERENT keypair"),
            "error should explain the keypair mismatch, got: {err_msg}"
        );

        // The freshly-generated signing.key must be rolled back -- leaving
        // it on disk would let every subsequent resolve_signing_key call
        // silently succeed with the same broken (mismatched) pairing.
        assert!(
            !keys_dir.join("signing.key").exists(),
            "the mismatched signing.key must be rolled back, not left on disk"
        );

        // The stale verifying.key on disk must be untouched.
        let after = std::fs::read_to_string(&verifying_path).expect("read");
        assert_eq!(
            after.trim(),
            hex::encode(stale_signing_key.verifying_key().to_bytes()),
            "the pre-existing verifying.key must not be modified"
        );
    }

    /// The legitimate counterpart to the mismatch test above: if the
    /// pre-existing `verifying.key` on disk actually *does* correspond to
    /// the signing key being persisted (e.g. a retried call that
    /// regenerates the same keypair), persistence must still succeed --
    /// the new mismatch check must not reject a genuinely matching pair.
    /// `persist_keypair` is called directly (rather than through
    /// `resolve_signing_key`/`generate_and_persist_keypair`) with a fixed
    /// seed so the "regenerated" key is deterministic and provably equal
    /// across both calls -- `OsRng` gives no such control from a test.
    #[test]
    fn persist_keypair_succeeds_when_existing_verifying_key_matches_the_regenerated_signing_key() {
        let _guard = env_lock();
        clear_env();
        let dir = tempfile::tempdir().expect("tempdir");
        let fixed_seed = [0x11u8; 32];

        // First call persists both signing.key and verifying.key for this
        // fixed key.
        let first = persist_keypair(dir.path(), SigningKey::from_bytes(&fixed_seed))
            .expect("first persist");
        assert_eq!(first.to_bytes(), fixed_seed);

        // Simulate a retried call after signing.key alone was lost:
        // verifying.key on disk still matches this exact key material --
        // a legitimate, idempotent regeneration, not a stale/unrelated
        // keypair.
        std::fs::remove_file(dir.path().join(".ggen/keys/signing.key"))
            .expect("remove signing.key");

        let second = persist_keypair(dir.path(), SigningKey::from_bytes(&fixed_seed)).expect(
            "second persist must succeed: the existing verifying.key matches the regenerated \
             signing key",
        );
        assert_eq!(second.to_bytes(), fixed_seed);

        let signing_path = dir.path().join(".ggen/keys/signing.key");
        let verifying_path = dir.path().join(".ggen/keys/verifying.key");
        assert!(signing_path.exists());
        let persisted_verifying = std::fs::read_to_string(&verifying_path).expect("read");
        assert_eq!(
            persisted_verifying.trim(),
            hex::encode(second.verifying_key().to_bytes())
        );
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
