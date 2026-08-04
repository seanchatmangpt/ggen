//! Pack trust-tier enforcement, exercised end-to-end through this crate's
//! real public install path.
//!
//! # Why this is library-level, not subprocess-level
//!
//! This test was written to answer: "is there a real, CLI-invoked subprocess
//! test proving a low-trust pack install is refused?" The answer is no, and
//! this file does not fabricate one. Concretely:
//!
//! - `ggen-marketplace` itself has no `[[bin]]` target — it is a library
//!   only (confirmed via `crates/ggen-marketplace/Cargo.toml`), so there is
//!   no `ggen-marketplace` binary to spawn with `CliHarness`/`assert_cmd`/
//!   `std::process::Command`.
//! - The one real CLI surface that reaches pack install is the `ggen` binary
//!   (`crates/ggen-cli`), via `crates/ggen-cli/src/cmds/pack.rs`. But that
//!   call site invokes `install_pack_by_id` (profile = `None`), never
//!   `install_pack_by_id_with_profile(..., Some(&profile))` — the live CLI
//!   does not currently thread a trust-tier `Profile` through at all. A
//!   subprocess test against `ggen pack add` could only ever prove the
//!   "no profile" floor (Blocked-tier refusal), not profile-gated
//!   enforcement, and would live in `ggen-cli`'s own test suite regardless
//!   (this crate cannot spawn a binary it doesn't own).
//!
//! What this file proves instead: real, in-process, end-to-end calls into
//! this crate's own **public** API — `install_pack_by_id_with_profile` —
//! which is documented (see its doc comment in
//! `crates/ggen-marketplace/src/marketplace/install.rs`) to enforce trust
//! tier via the exact same shared `evaluate_trust_tier` helper that
//! `Installer::verify_trust_tier` uses for the marketplace-registry path.
//! `Installer::verify_trust_tier` itself is `pub(crate)` and cannot be
//! called from this external integration-test crate; `install_pack_by_id_with_profile`
//! is the real, public, externally-reachable entry point that exercises the
//! identical enforcement logic.
//!
//! Real collaborators used, no mocks: a real temp `GGEN_PACKS_DIR` directory
//! containing a real pack TOML file, a real temp install target directory,
//! and the real `install_pack_by_id_with_profile` async function performing
//! real filesystem I/O.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use ggen_marketplace::marketplace::install::{install_pack_by_id_with_profile, InstallByIdInput};
use ggen_marketplace::marketplace::profile::regulated_finance_profile;
use serial_test::serial;
use tempfile::TempDir;

/// Restore `GGEN_PACKS_DIR` to its prior value (or unset it) on drop, so
/// mutating this process-global env var in one test can't leak into another
/// test in this binary.
struct GgenPacksDirGuard {
    previous: Option<std::ffi::OsString>,
}

impl GgenPacksDirGuard {
    fn set(value: &std::path::Path) -> Self {
        let previous = std::env::var_os("GGEN_PACKS_DIR");
        std::env::set_var("GGEN_PACKS_DIR", value);
        Self { previous }
    }
}

impl Drop for GgenPacksDirGuard {
    fn drop(&mut self) {
        match &self.previous {
            None => std::env::remove_var("GGEN_PACKS_DIR"),
            Some(v) => std::env::set_var("GGEN_PACKS_DIR", v),
        }
    }
}

fn write_local_test_pack(packs_dir: &std::path::Path, id: &str, version: &str) {
    let toml = format!(
        r#"[pack]
id = "{id}"
name = "Test {id}"
version = "{version}"
description = "ggen-marketplace tests/ trust-tier e2e fixture"
category = "test"
license = "MIT"
production_ready = true
packages = ["{id}-core"]
"#
    );
    std::fs::write(packs_dir.join(format!("{id}.toml")), toml).unwrap();
}

/// A pack with no attested trust tier (evaluated at `TrustTier::Experimental`,
/// the documented floor for bare-id local packs) must be REFUSED — a real
/// `Err`, not a warning — when installed under a profile that requires
/// `EnterpriseCertified` (`regulated_finance_profile()`). The install must
/// also fail *before* any filesystem mutation: the target install directory
/// must not be created.
#[tokio::test]
#[serial(GGEN_PACKS_DIR)]
async fn low_trust_pack_is_refused_under_regulated_finance_profile() {
    let packs_registry_dir = TempDir::new().unwrap();
    let _guard = GgenPacksDirGuard::set(packs_registry_dir.path());
    write_local_test_pack(
        packs_registry_dir.path(),
        "io.ggen.tests.low-trust",
        "1.0.0",
    );

    let target = TempDir::new().unwrap();
    let target_dir = target.path().join("install-target");

    let input = InstallByIdInput {
        pack_id: "io.ggen.tests.low-trust".to_string(),
        target_dir: Some(target_dir.clone()),
        force: false,
        dry_run: false,
    };

    let profile = regulated_finance_profile();
    assert_eq!(
        profile.trust_requirements,
        ggen_marketplace::marketplace::trust::TrustTier::EnterpriseCertified,
        "sanity check: regulated_finance_profile must require EnterpriseCertified"
    );

    let result = install_pack_by_id_with_profile(&input, Some(&profile)).await;

    let err = result.expect_err(
        "a pack below the profile's required trust tier must be refused with a real Err, \
         not silently installed",
    );
    let err_msg = err.to_string();
    assert!(
        err_msg.contains("trust tier"),
        "refusal error should reference the trust-tier check, got: {err_msg}"
    );

    assert!(
        !target_dir.exists(),
        "trust-tier enforcement must run before any filesystem write -- a refused install \
         must not create the target install directory"
    );
}

/// The same pack, installed with no profile supplied at all, must be
/// ACCEPTED: the documented "no profile" default is to allow Experimental
/// and higher (everything except `Blocked`). This is the positive control
/// proving the refusal above is caused by the profile's trust requirement,
/// not by some unrelated failure in the install path.
#[tokio::test]
#[serial(GGEN_PACKS_DIR)]
async fn same_pack_is_accepted_with_no_profile_supplied() {
    let packs_registry_dir = TempDir::new().unwrap();
    let _guard = GgenPacksDirGuard::set(packs_registry_dir.path());
    write_local_test_pack(
        packs_registry_dir.path(),
        "io.ggen.tests.default-ok",
        "1.0.0",
    );

    let target = TempDir::new().unwrap();
    let target_dir = target.path().join("install-target");

    let input = InstallByIdInput {
        pack_id: "io.ggen.tests.default-ok".to_string(),
        target_dir: Some(target_dir.clone()),
        force: false,
        dry_run: false,
    };

    let output = install_pack_by_id_with_profile(&input, None)
        .await
        .expect("install with no profile must succeed under the documented default");

    assert_eq!(output.pack_id, "io.ggen.tests.default-ok");
    assert!(
        !output.digest.is_empty(),
        "a real, non-dry-run, accepted install must pin a non-empty digest"
    );
    assert!(
        target_dir.exists(),
        "an accepted install must materialize the real target install directory"
    );
}
