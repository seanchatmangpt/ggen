//! Layout pre-flight surfaces for Gate-1 photolithography (ECOSYSTEM-SPEC-27.1.1 D2/D4/D5).
//!
//! Narrow AOT geometry + brand rhythm stubs — not a parallel UI stack.
//! [`remediation_bound`] mirrors zcode-cli autonomic retry caps without claiming
//! out-of-repo CLI `ALIVE` (see `docs/rfc/v27.1.1/ZCODE-POWL-OCEL-STANDING.md`).
//! [`ui_layout`] consumes [`taffy_aot::AotSealReceipt`] digests (in-repo D5 fence).

pub mod brand;
pub mod remediation_bound;
pub mod taffy_aot;
pub mod ui_layout;
