//! Typed refusals for architecture machinery.

use thiserror::Error;

/// Crate-wide result type.
pub type Result<T> = std::result::Result<T, ArchitectureError>;

/// Fail-closed architecture errors.
#[derive(Debug, Error)]
pub enum ArchitectureError {
    /// An asset identifier already exists in the registry.
    #[error("architecture asset `{0}` is already registered")]
    DuplicateAsset(String),

    /// An asset identifier was not found.
    #[error("architecture asset `{0}` is not registered")]
    UnknownAsset(String),

    /// An asset identifier was empty or otherwise invalid.
    #[error("invalid architecture asset identifier: {0}")]
    InvalidAssetId(String),

    /// A lifecycle transition violated the declared state machine.
    #[error("invalid lifecycle transition for `{asset_id}`: {from} -> {to}")]
    InvalidTransition {
        /// Subject asset.
        asset_id: String,
        /// Current state.
        from: String,
        /// Requested state.
        to: String,
    },

    /// A dependency references a missing asset.
    #[error("asset `{asset_id}` depends on missing asset `{dependency_id}`")]
    DanglingDependency {
        /// Dependent asset.
        asset_id: String,
        /// Missing dependency.
        dependency_id: String,
    },

    /// The architecture dependency graph contains a cycle.
    #[error("architecture dependency cycle detected among: {0}")]
    DependencyCycle(String),

    /// The architecture policy attempted to permit direct actuation.
    #[error("direct autonomic actuation is constitutionally forbidden; emit an intent instead")]
    DirectActuationForbidden,

    /// A deterministic receipt could not be serialized.
    #[error("receipt serialization failed: {0}")]
    Serialization(#[from] serde_json::Error),
}
