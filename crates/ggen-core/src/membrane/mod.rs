//! Membrane system for Genesis embedded runtime integrations.
//!
//! Provides the outer membrane layer, adapter bindings, and various projection layers:
//! - W3C PROV lineage projection
//! - OCEL event logging
//! - RDF Turtle projection
//! - SHACL shape validation reports

pub mod core;
pub mod ocel;
pub mod prov;
pub mod rdf;
pub mod shacl;

pub use self::core::{
    BoundaryCrossing, GenesisCore, GgenMembrane, InterchangeablePart, VectorClock,
};
pub use self::ocel::{OCELEvent, OCEL, OCELObject, OcelValue};
pub use self::prov::{ProvDocument, ProvRelation};
pub use self::rdf::RdfMembraneProjector;
pub use self::shacl::{MembraneShaclValidator, MEMBRANE_SHACL_SHAPES};
