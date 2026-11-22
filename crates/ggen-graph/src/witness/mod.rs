//! Offline `verify_witness` WASM verifier stub (ECOSYSTEM-SPEC-27.1.1 Gate 1).
//!
//! Plane 4 (`verify_witness` ≤ 250KB) is owned by the affidavit / Truex / witness
//! plane. ggen may **pack or court** an artifact; it must not claim runtime
//! `ALIVE` without a size-court receipt, and it does not host the production
//! WASM runtime (see `docs/rfc/v27.1.1/VERIFY-WITNESS-PRODUCTION.md`).
//!
//! This module encodes:
//! 1. the verify API surface (`verify_witness` + size court);
//! 2. a hard size budget of `< 262_144` bytes (Gate-1 matrix);
//! 3. typed `UNSUPPORTED` when the production toolchain / artifact is absent.

use std::path::{Path, PathBuf};

/// Gate-1 exclusive size ceiling: artifact MUST be **&lt; 262_144** bytes
/// (`docs/rfc/v27.1.1/GATE-1-GAP-MATRIX.md`).
pub const VERIFY_WITNESS_SIZE_BUDGET_BYTES: usize = 262_144;

/// Canonical artifact basename for the offline verifier stub.
pub const VERIFY_WITNESS_ARTIFACT_NAME: &str = "verify_witness.wasm";

/// WASM magic (`\0asm`).
pub const WASM_MAGIC: &[u8; 4] = b"\0asm";

/// How an artifact was obtained for the size court.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArtifactProvenance {
    /// In-repo placeholder under `crates/ggen-graph/fixtures/witness/`.
    Placeholder,
    /// Bytes supplied by the caller (pack emit / external plane).
    CallerProvided,
    /// Path resolved from env / explicit production path.
    ExternalPath,
}

/// Standing vocabulary for the witness size court (AGENTS.md §2).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WitnessStanding {
    /// Size court passed on an in-repo placeholder or admitted bytes — gate encoded,
    /// production runtime still out-of-repo.
    PartialAlive,
    /// Production WASM toolchain / built artifact is not available in this checkout.
    Unsupported,
}

/// Machine-readable admit receipt for a successful size court.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SizeCourtReceipt {
    pub size_bytes: usize,
    pub budget_bytes: usize,
    pub provenance: ArtifactProvenance,
    pub standing: WitnessStanding,
}

/// Result of the structural `verify_witness` stub (not cryptographic proof).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifyWitnessReceipt {
    pub size: SizeCourtReceipt,
    /// True when the leading four bytes are WASM magic.
    pub wasm_magic_ok: bool,
}

/// Typed refusals / unsupported outcomes for the witness surface.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum WitnessRefusal {
    /// Artifact size is ≥ [`VERIFY_WITNESS_SIZE_BUDGET_BYTES`].
    OverBudget {
        size_bytes: usize,
        budget_bytes: usize,
    },
    /// Bytes are empty or missing WASM magic — fail closed.
    InvalidWasmMagic,
    /// Production toolchain / built artifact absent; placeholder may still court.
    Unsupported { reason: String },
}

/// Court the byte length of a `verify_witness` artifact against the Gate-1 budget.
///
/// Admits when `artifact.len() < VERIFY_WITNESS_SIZE_BUDGET_BYTES`.
/// Refuses with [`WitnessRefusal::OverBudget`] otherwise.
pub fn court_verify_witness_size(
    artifact: &[u8], provenance: ArtifactProvenance,
) -> Result<SizeCourtReceipt, WitnessRefusal> {
    let size_bytes = artifact.len();
    if size_bytes >= VERIFY_WITNESS_SIZE_BUDGET_BYTES {
        return Err(WitnessRefusal::OverBudget {
            size_bytes,
            budget_bytes: VERIFY_WITNESS_SIZE_BUDGET_BYTES,
        });
    }
    let standing = match provenance {
        ArtifactProvenance::Placeholder => WitnessStanding::PartialAlive,
        ArtifactProvenance::CallerProvided | ArtifactProvenance::ExternalPath => {
            WitnessStanding::PartialAlive
        }
    };
    Ok(SizeCourtReceipt {
        size_bytes,
        budget_bytes: VERIFY_WITNESS_SIZE_BUDGET_BYTES,
        provenance,
        standing,
    })
}

/// Structural verify stub: WASM magic + size court.
///
/// This is **CONSTRUCT / verify**, not DO. Cryptographic witness adequacy and
/// runtime execution live on the out-of-repo production path.
pub fn verify_witness(
    artifact: &[u8], provenance: ArtifactProvenance,
) -> Result<VerifyWitnessReceipt, WitnessRefusal> {
    if artifact.len() < 4 || &artifact[..4] != WASM_MAGIC.as_slice() {
        return Err(WitnessRefusal::InvalidWasmMagic);
    }
    let size = court_verify_witness_size(artifact, provenance)?;
    Ok(VerifyWitnessReceipt {
        size,
        wasm_magic_ok: true,
    })
}

/// Resolve the in-repo placeholder path (relative to this crate's `fixtures/`).
pub fn placeholder_artifact_path() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("fixtures/witness/verify_witness.placeholder.wasm")
}

/// Load the in-repo placeholder and run [`verify_witness`].
///
/// Standing on success is [`WitnessStanding::PartialAlive`] — the Gate-1 size
/// court is encoded; production build remains out-of-repo.
pub fn verify_placeholder_witness() -> Result<VerifyWitnessReceipt, WitnessRefusal> {
    let path = placeholder_artifact_path();
    let bytes = std::fs::read(&path).map_err(|e| WitnessRefusal::Unsupported {
        reason: format!(
            "placeholder verify_witness artifact missing at {}: {e}",
            path.display()
        ),
    })?;
    verify_witness(&bytes, ArtifactProvenance::Placeholder)
}

/// Attempt to load a **production** artifact.
///
/// Looks for `GGEN_VERIFY_WITNESS_WASM` (absolute path) or `path` if provided.
/// When neither yields a readable file, returns typed
/// [`WitnessRefusal::Unsupported`] — the Gate-1 size court still exists via
/// [`verify_placeholder_witness`].
pub fn load_production_verify_witness(
    path: Option<&Path>,
) -> Result<(Vec<u8>, PathBuf), WitnessRefusal> {
    let resolved = path
        .map(Path::to_path_buf)
        .or_else(|| std::env::var_os("GGEN_VERIFY_WITNESS_WASM").map(PathBuf::from));

    let Some(resolved) = resolved else {
        return Err(WitnessRefusal::Unsupported {
            reason: "production verify_witness.wasm not configured \
                     (set GGEN_VERIFY_WITNESS_WASM or pass an explicit path); \
                     see docs/rfc/v27.1.1/VERIFY-WITNESS-PRODUCTION.md"
                .to_string(),
        });
    };

    let bytes = std::fs::read(&resolved).map_err(|e| WitnessRefusal::Unsupported {
        reason: format!(
            "production verify_witness artifact unreadable at {}: {e}",
            resolved.display()
        ),
    })?;
    Ok((bytes, resolved))
}

/// Verify a production artifact when present; otherwise typed `UNSUPPORTED`.
pub fn verify_production_witness(
    path: Option<&Path>,
) -> Result<VerifyWitnessReceipt, WitnessRefusal> {
    let (bytes, _resolved) = load_production_verify_witness(path)?;
    verify_witness(&bytes, ArtifactProvenance::ExternalPath)
}

/// Report whether a wasm32 production toolchain appears installed.
///
/// Presence of `wasm32-unknown-unknown` in `rustc --print target-list` is **not**
/// enough; this checks `rustup target list --installed` when `rustup` exists.
/// Absence → callers should treat production builds as [`WitnessStanding::Unsupported`].
pub fn production_wasm_toolchain_available() -> bool {
    let Ok(output) = std::process::Command::new("rustup")
        .args(["target", "list", "--installed"])
        .output()
    else {
        return false;
    };
    if !output.status.success() {
        return false;
    }
    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout.lines().any(|l| l.trim() == "wasm32-unknown-unknown")
}

/// Typed standing when asking for a **built** production wasm without toolchain.
pub fn production_build_standing() -> Result<WitnessStanding, WitnessRefusal> {
    if production_wasm_toolchain_available() {
        // Toolchain present still does not imply a built artifact — size court
        // must run on real bytes via verify_production_witness / caller bytes.
        Ok(WitnessStanding::PartialAlive)
    } else {
        Err(WitnessRefusal::Unsupported {
            reason: "wasm32-unknown-unknown not installed via rustup; \
                     production verify_witness build is UNSUPPORTED in this \
                     environment — size court remains encoded on the placeholder"
                .to_string(),
        })
    }
}

#[cfg(test)]
mod unit_tests {
    use super::*;

    #[test]
    fn budget_constant_matches_gate1_matrix() {
        assert_eq!(VERIFY_WITNESS_SIZE_BUDGET_BYTES, 262_144);
    }

    #[test]
    fn over_budget_is_refused() {
        let bloated = vec![0u8; VERIFY_WITNESS_SIZE_BUDGET_BYTES];
        match court_verify_witness_size(&bloated, ArtifactProvenance::CallerProvided) {
            Err(WitnessRefusal::OverBudget {
                size_bytes,
                budget_bytes,
            }) => {
                assert_eq!(size_bytes, VERIFY_WITNESS_SIZE_BUDGET_BYTES);
                assert_eq!(budget_bytes, VERIFY_WITNESS_SIZE_BUDGET_BYTES);
            }
            other => panic!("expected OverBudget, got {other:?}"),
        }
    }
}
