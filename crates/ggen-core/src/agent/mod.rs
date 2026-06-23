//! Agent-facing facade over the packs + marketplace subsystems.
//!
//! This module is the AGI/agent integration surface for ggen's package system.
//! Where the CLI returns human-oriented strings and the underlying pipeline
//! returns engine-internal types, [`PackAgent`] returns a *small, stable,
//! `serde`-serializable* contract ([`types`]) designed for autonomous
//! consumption over MCP, A2A, or a direct library call.
//!
//! ## Design contract
//!
//! - **One authoritative path.** The facade wraps the same durable-state writers
//!   the CLI uses (`domain::packs::install::install_pack`, the lockfile, the
//!   receipt emitter). It does not fork a second install path, so it can only
//!   *deepen* authority, never bypass it.
//! - **Evidence-bearing outcomes.** Every mutating result carries the artifacts
//!   it produced (digest, lockfile path, signed receipt) so an agent can prove
//!   the state transition happened instead of trusting a status string.
//! - **Fail-closed, typed errors.** Refusals (missing pack, empty digest,
//!   absent lockfile, bad signature) are surfaced as [`types::AgentError`]
//!   variants the agent can branch on — never swallowed into a fake success.
//! - **Discovery-first.** [`PackAgent::capabilities`] describes the available
//!   operations and capability surfaces so an agent learns the contract without
//!   out-of-band documentation.
//!
//! ## Example
//!
//! ```no_run
//! use ggen_core::agent::{PackAgent, InstallRequest};
//!
//! # async fn run() -> Result<(), Box<dyn std::error::Error>> {
//! let agent = PackAgent::new()?;
//!
//! // Discover, then search, then install with provenance.
//! let _caps = agent.capabilities();
//! let hits = agent.search("rust", Some(5))?;
//! if let Some(top) = hits.first() {
//!     let outcome = agent.install(InstallRequest::new(&top.pack.id)).await?;
//!     assert!(!outcome.digest.is_empty());
//!     assert!(outcome.receipt.is_some());
//! }
//! # Ok(())
//! # }
//! ```

pub mod facade;
pub mod receipt;
pub mod types;

pub use facade::PackAgent;
pub use receipt::{
    emit_install_receipt, verify_install_receipt, PackInstallClosure, PackReceiptError,
};
pub use types::{
    AgentError, AgentResult, AgentStatus, Capabilities, CapabilityRef, CompatibilityOutcome,
    DependencyRef, InstallOutcome, InstallRequest, InstalledPackRef, OperationRef, PackDetail,
    PackRef, PackValidation, ReceiptRef, RemoveOutcome, ResolveOutcome, SearchHit, VerifyOutcome,
};
