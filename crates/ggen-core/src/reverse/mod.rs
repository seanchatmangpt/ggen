//! Reverse pipeline (`ggen reverse`): code/cheat → RDF authority + Tera candidates.
//!
//! The inverse of the forward μ₁–μ₅ pipeline. Where `ggen sync` projects RDF →
//! code, this module observes existing code (and, in later arms, detected
//! defects) and back-writes the discovered RDF authority graph and Tera
//! template candidates that would produce it — turning ad-hoc artifacts into
//! admitted standard work.
//!
//! Arms (each an independently testable slice):
//! - [`scan`] — ARM 1: code → discovered authority graph (live).
//! - `cheats` — ARM 2: diagnostics/ledger → defect graph + candidate SHACL (planned).
//! - `templates` — ARM 3: discovered code → Tera template candidates (planned).
//!
//! Core logic lives here (always compiled); the CLI maps the returned neutral
//! [`events::ReverseEvent`]s to the OCEL log.

pub mod cheats;
pub mod events;
pub mod receipt;
pub mod scan;
pub mod templates;
pub mod turtle;
pub mod vocab;

pub use cheats::{
    extract_defects, parse_ledger, DefectSpeciesInput, LedgerDefect, ReverseCheatsReport,
};
pub use events::ReverseEvent;
pub use receipt::ReverseReceipt;
pub use scan::{scan_to_authority, ReverseScanReport};
pub use templates::{
    check_bindings, infer_candidates, BindingContract, ReverseTemplatesReport, TemplateCandidate,
};
pub use turtle::{Object, TripleSet};
