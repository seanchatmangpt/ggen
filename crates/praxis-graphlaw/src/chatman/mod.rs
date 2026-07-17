//! Chatman engine: invocation ABI, Triple8 universe, admission tables, and routing.
//!
//! Module layout (one lane owns each file; `abi` is the cross-lane contract):
//! - [`abi`] — invocation envelopes, receipts, and the refusal taxonomy.
//! - [`triple8`] — bounded Triple8 term universe.
//! - [`admission8`] — admission tables for hook patterns and OCEL events.
//! - [`router`] — least-expressive dialect routing.
//! - [`engine`] — the engine loop over admitted invocations.
//! - [`bridge`] — boundary bridge to external process substrates.
//! - [`closure`] — parent-child closure law for recursive workflow sockets
//!   (PRD v26.7.11 §9, PROJ-759).
//! - [`compensation`] — compensation-as-workflow manufacture path (PRD
//!   v26.7.11 §10, PROJ-759).
//! - [`quarantine`] — index of the security-review quarantine boundaries
//!   that have real code correlates (and which named ones do not).

pub mod abi;
pub mod admission8;
pub mod bridge;
pub mod closure;
pub mod compensation;
pub mod engine;
pub mod powl_projection;
pub mod quarantine;
pub mod router;
pub mod triple8;

pub use abi::Refusal;
