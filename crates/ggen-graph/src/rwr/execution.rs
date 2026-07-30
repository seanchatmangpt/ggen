//! Bounded machinery, atomic automation, and cryptographic actuation receipts.

use crate::rwr::matrix::{Dimension, ALL_DIMENSIONS, MATRIX_VERSION};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fs::{self, File};
use std::io::{Read, Write};
use std::path::{Path, PathBuf};

fn put_len_prefixed(hasher: &mut blake3::Hasher, value: &[u8]) {
    hasher.update(&(value.len() as u64).to_le_bytes());
    hasher.update(value);
}

/// A bounded artifact construction requested by admitted state.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Action {
    /// Stable, path-safe action identity.
    pub id: String,
    /// Maturity dimension whose mechanism is being exercised.
    pub dimension: Dimension,
    /// Externalized artifact bytes.
    pub payload: Vec<u8>,
    /// Claimed payload digest. Grants refuse a mismatch.
    pub expected_payload_digest: [u8; 32],
}

impl Action {
    /// Create an action and bind its expected payload digest.
    #[must_use]
    pub fn new(id: impl Into<String>, dimension: Dimension, payload: Vec<u8>) -> Self {
        let expected_payload_digest = blake3::hash(&payload).into();
        Self {
            id: id.into(),
            dimension,
            payload,
            expected_payload_digest,
        }
    }

    /// Verify and return the action digest used by execution grants.
    pub fn digest(&self) -> Result<[u8; 32], ExecutionError> {
        if !is_safe_id(&self.id) {
            return Err(ExecutionError::InvalidActionId(self.id.clone()));
        }
        let observed_payload_digest: [u8; 32] = blake3::hash(&self.payload).into();
        if observed_payload_digest != self.expected_payload_digest {
            return Err(ExecutionError::PayloadDigestMismatch(self.id.clone()));
        }
        let mut hasher = blake3::Hasher::new();
        put_len_prefixed(&mut hasher, b"rwr-action/v1");
        put_len_prefixed(&mut hasher, self.id.as_bytes());
        hasher.update(&[self.dimension as u8]);
        hasher.update(&self.expected_payload_digest);
        Ok(hasher.finalize().into())
    }
}

fn is_safe_id(id: &str) -> bool {
    !id.is_empty()
        && id.len() <= 128
        && id
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'-' || byte == b'_')
}

/// Executable policy bounding the foundation machine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecutionPolicy {
    allowed_dimensions: BTreeSet<Dimension>,
    max_payload_bytes: usize,
}

impl ExecutionPolicy {
    /// Admit all full-matrix dimensions with an explicit payload bound.
    #[must_use]
    pub fn full_level5(max_payload_bytes: usize) -> Self {
        Self {
            allowed_dimensions: ALL_DIMENSIONS.into_iter().collect(),
            max_payload_bytes,
        }
    }

    /// Construct a narrower policy.
    #[must_use]
    pub fn new(
        allowed_dimensions: impl IntoIterator<Item = Dimension>,
        max_payload_bytes: usize,
    ) -> Self {
        Self {
            allowed_dimensions: allowed_dimensions.into_iter().collect(),
            max_payload_bytes,
        }
    }

    fn admit(&self, action: &Action) -> Result<(), ExecutionError> {
        if !self.allowed_dimensions.contains(&action.dimension) {
            return Err(ExecutionError::DimensionRefused(action.dimension));
        }
        if action.payload.len() > self.max_payload_bytes {
            return Err(ExecutionError::PayloadBoundExceeded {
                observed: action.payload.len(),
                maximum: self.max_payload_bytes,
            });
        }
        action.digest().map(|_| ())
    }
}

/// Derived authority to execute exactly one action.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ExecutionGrant {
    /// Grant schema.
    pub schema: String,
    /// Action digest authorized by the grant.
    pub action_digest: [u8; 32],
    /// Matrix contract against which authority was derived.
    pub matrix_version: String,
    /// BLAKE3 binding the grant.
    pub grant_digest: [u8; 32],
}

impl ExecutionGrant {
    fn derive(action_digest: [u8; 32]) -> Self {
        let matrix_version = MATRIX_VERSION.to_string();
        let grant_digest = grant_digest(&action_digest, &matrix_version);
        Self {
            schema: "rwr-execution-grant/v1".to_string(),
            action_digest,
            matrix_version,
            grant_digest,
        }
    }

    /// Verify the grant and its relationship to an action.
    pub fn verify_for(&self, action: &Action) -> Result<(), ExecutionError> {
        if self.schema != "rwr-execution-grant/v1"
            || self.matrix_version != MATRIX_VERSION
            || self.grant_digest != grant_digest(&self.action_digest, &self.matrix_version)
        {
            return Err(ExecutionError::GrantDigestMismatch);
        }
        if self.action_digest != action.digest()? {
            return Err(ExecutionError::GrantActionMismatch);
        }
        Ok(())
    }
}

fn grant_digest(action_digest: &[u8; 32], matrix_version: &str) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    put_len_prefixed(&mut hasher, b"rwr-execution-grant/v1");
    hasher.update(action_digest);
    put_len_prefixed(&mut hasher, matrix_version.as_bytes());
    hasher.finalize().into()
}

/// Policy-owning machinery that derives bounded execution authority.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FoundationMachine {
    policy: ExecutionPolicy,
}

impl FoundationMachine {
    /// Construct the machine with executable policy.
    #[must_use]
    pub fn new(policy: ExecutionPolicy) -> Self {
        Self { policy }
    }

    /// Admit an action and derive authority for exactly that action.
    pub fn derive_grant(&self, action: &Action) -> Result<ExecutionGrant, ExecutionError> {
        self.policy.admit(action)?;
        Ok(ExecutionGrant::derive(action.digest()?))
    }
}

/// Cryptographic receipt of one committed filesystem actuation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ActuationReceipt {
    /// Receipt schema.
    pub schema: String,
    /// Action that caused the consequence.
    pub action_id: String,
    /// Matrix dimension exercised by the consequence.
    pub dimension: Dimension,
    /// Artifact location relative to the actuator root.
    pub artifact_path: String,
    /// BLAKE3 of the committed artifact.
    pub payload_digest: [u8; 32],
    /// Grant authorizing the action.
    pub grant_digest: [u8; 32],
    /// BLAKE3 binding all receipt fields.
    pub receipt_digest: [u8; 32],
}

impl ActuationReceipt {
    fn issue(action: &Action, grant: &ExecutionGrant, artifact_path: String) -> Self {
        let receipt_digest = actuation_receipt_digest(
            &action.id,
            action.dimension,
            &artifact_path,
            &action.expected_payload_digest,
            &grant.grant_digest,
        );
        Self {
            schema: "rwr-actuation-receipt/v1".to_string(),
            action_id: action.id.clone(),
            dimension: action.dimension,
            artifact_path,
            payload_digest: action.expected_payload_digest,
            grant_digest: grant.grant_digest,
            receipt_digest,
        }
    }

    /// Verify the cryptographic receipt fields.
    pub fn verify(&self) -> Result<(), ExecutionError> {
        if self.schema != "rwr-actuation-receipt/v1" {
            return Err(ExecutionError::ReceiptDigestMismatch);
        }
        let expected = actuation_receipt_digest(
            &self.action_id,
            self.dimension,
            &self.artifact_path,
            &self.payload_digest,
            &self.grant_digest,
        );
        if self.receipt_digest != expected {
            return Err(ExecutionError::ReceiptDigestMismatch);
        }
        Ok(())
    }
}

fn actuation_receipt_digest(
    action_id: &str,
    dimension: Dimension,
    artifact_path: &str,
    payload_digest: &[u8; 32],
    grant_digest: &[u8; 32],
) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    put_len_prefixed(&mut hasher, b"rwr-actuation-receipt/v1");
    put_len_prefixed(&mut hasher, action_id.as_bytes());
    hasher.update(&[dimension as u8]);
    put_len_prefixed(&mut hasher, artifact_path.as_bytes());
    hasher.update(payload_digest);
    hasher.update(grant_digest);
    hasher.finalize().into()
}

/// Real filesystem actuator using an atomic committed transaction directory.
///
/// The artifact and its receipt are written into one staging directory and become
/// visible together through a same-filesystem directory rename. This bounds the
/// first implementation of zero-unreceipted actuation to the actuator root.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FilesystemActuator {
    root: PathBuf,
}

impl FilesystemActuator {
    /// Create an actuator rooted at a bounded filesystem location.
    #[must_use]
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    /// Commit an authorized action and return its cryptographic receipt.
    pub fn actuate(
        &self,
        grant: &ExecutionGrant,
        action: &Action,
    ) -> Result<ActuationReceipt, ExecutionError> {
        grant.verify_for(action)?;
        fs::create_dir_all(self.root.join(".staging"))?;
        fs::create_dir_all(self.root.join("committed"))?;

        let staging = self.root.join(".staging").join(&action.id);
        let committed = self.root.join("committed").join(&action.id);
        if staging.exists() {
            return Err(ExecutionError::StagingCollision(action.id.clone()));
        }
        if committed.exists() {
            return Err(ExecutionError::ReplayRefused(action.id.clone()));
        }

        fs::create_dir(&staging)?;
        let result = self.stage_and_commit(&staging, &committed, grant, action);
        if result.is_err() && staging.exists() {
            let _cleanup_result = fs::remove_dir_all(&staging);
        }
        result
    }

    fn stage_and_commit(
        &self,
        staging: &Path,
        committed: &Path,
        grant: &ExecutionGrant,
        action: &Action,
    ) -> Result<ActuationReceipt, ExecutionError> {
        let artifact_path = format!("committed/{}/artifact.bin", action.id);
        let receipt = ActuationReceipt::issue(action, grant, artifact_path);

        let mut artifact = File::create(staging.join("artifact.bin"))?;
        artifact.write_all(&action.payload)?;
        artifact.sync_all()?;

        let receipt_bytes = serde_json::to_vec_pretty(&receipt)
            .map_err(|error| ExecutionError::Serialization(error.to_string()))?;
        let mut receipt_file = File::create(staging.join("receipt.json"))?;
        receipt_file.write_all(&receipt_bytes)?;
        receipt_file.sync_all()?;

        fs::rename(staging, committed)?;
        self.verify_committed(&receipt)?;
        Ok(receipt)
    }

    /// Verify the committed artifact and persisted receipt against one another.
    pub fn verify_committed(&self, expected: &ActuationReceipt) -> Result<(), ExecutionError> {
        expected.verify()?;
        let committed_dir = self.root.join("committed").join(&expected.action_id);
        let mut receipt_bytes = Vec::new();
        File::open(committed_dir.join("receipt.json"))?.read_to_end(&mut receipt_bytes)?;
        let persisted: ActuationReceipt = serde_json::from_slice(&receipt_bytes)
            .map_err(|error| ExecutionError::Serialization(error.to_string()))?;
        if persisted != *expected {
            return Err(ExecutionError::PersistedReceiptMismatch);
        }

        let mut artifact = Vec::new();
        File::open(committed_dir.join("artifact.bin"))?.read_to_end(&mut artifact)?;
        let digest: [u8; 32] = blake3::hash(&artifact).into();
        if digest != expected.payload_digest {
            return Err(ExecutionError::CommittedArtifactMismatch);
        }
        Ok(())
    }

    /// Root used by this bounded actuator.
    #[must_use]
    pub fn root(&self) -> &Path {
        &self.root
    }
}

/// In-memory verifier for duplicate replay of already committed receipts.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ReplayVerifier {
    seen: BTreeSet<[u8; 32]>,
}

impl ReplayVerifier {
    /// Verify a receipt exactly once.
    pub fn verify_once(&mut self, receipt: &ActuationReceipt) -> Result<(), ExecutionError> {
        receipt.verify()?;
        if !self.seen.insert(receipt.receipt_digest) {
            return Err(ExecutionError::ReceiptReplayRefused);
        }
        Ok(())
    }
}

/// Typed execution failures. No failure collapses into success.
#[derive(Debug, thiserror::Error)]
pub enum ExecutionError {
    /// Action identity cannot safely address a bounded transaction directory.
    #[error("invalid action id: {0}")]
    InvalidActionId(String),
    /// Payload bytes no longer match the action digest.
    #[error("payload digest mismatch for action: {0}")]
    PayloadDigestMismatch(String),
    /// Policy excludes the requested dimension.
    #[error("dimension refused by execution policy: {0:?}")]
    DimensionRefused(Dimension),
    /// Payload exceeds the admitted bound.
    #[error("payload bound exceeded: observed {observed}, maximum {maximum}")]
    PayloadBoundExceeded {
        /// Observed bytes.
        observed: usize,
        /// Maximum admitted bytes.
        maximum: usize,
    },
    /// Grant fields do not verify.
    #[error("execution grant digest mismatch")]
    GrantDigestMismatch,
    /// Grant authorizes a different action.
    #[error("execution grant does not authorize this action")]
    GrantActionMismatch,
    /// A stale or concurrent staging transaction exists.
    #[error("staging transaction already exists: {0}")]
    StagingCollision(String),
    /// A committed action identity cannot be actuated twice.
    #[error("actuation replay refused: {0}")]
    ReplayRefused(String),
    /// Receipt fields do not verify.
    #[error("actuation receipt digest mismatch")]
    ReceiptDigestMismatch,
    /// Persisted receipt differs from the issued receipt.
    #[error("persisted receipt mismatch")]
    PersistedReceiptMismatch,
    /// Committed bytes differ from the receipted payload.
    #[error("committed artifact digest mismatch")]
    CommittedArtifactMismatch,
    /// A receipt was replayed to the replay verifier.
    #[error("receipt replay refused")]
    ReceiptReplayRefused,
    /// JSON serialization or parsing failed.
    #[error("serialization failed: {0}")]
    Serialization(String),
    /// Real filesystem boundary failed.
    #[error("filesystem boundary failed: {0}")]
    Io(#[from] std::io::Error),
}
