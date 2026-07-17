//! Embedded/standard ontology support, ported out of `ggen-core`.
//!
//! Ported during the ggen-core retirement migration (`specs/014-ggen-core-replacement`,
//! Phase 3 "non-colliding noun re-points") specifically to get `ggen-cli`'s `cmds/ontology.rs`
//! off its `ggen_core::ontology::{CoreOntologyBundle, OntologyLoader}` /
//! `ggen_core::validation::StandardOntology` / `ggen_core::domain::ontology::
//! get_standard_namespaces` imports. `ggen-cli` already depends on `ggen-marketplace`, so this
//! adds no new workspace dependency edge.
//!
//! - [`core_bundle`] — `CoreOntologyBundle`/`OntologyMetadata`: the three W3C core stubs
//!   (rdf/rdfs/owl) embedded as inline byte literals, zero file dependency.
//! - [`resolver`] — `OntologyResolver`: local-filesystem/registry-path fallback resolution,
//!   used by `loader`.
//! - [`loader`] — `OntologyLoader`: unified fallback-chain loader (core bundle → bundled
//!   standards → filesystem). Embeds `foaf.ttl` and `dublin-core-elements-1.1.ttl` (copied
//!   into `ggen-marketplace/ontologies/`) via `include_str!`.
//! - [`standard_ontologies`] — `StandardOntology` enum (schema.org/FOAF/Dublin
//!   Core/SKOS/Big Five) plus `get_standard_namespaces()`/`NamespaceInfo`. See that module's
//!   doc comment for what was intentionally dropped from the ggen-core original
//!   (`StandardOntologyValidator`, `OntologyScreeningConfig` — unreachable from
//!   `cmds/ontology.rs`).

pub mod core_bundle;
pub mod loader;
pub mod resolver;
pub mod standard_ontologies;

pub use core_bundle::{CoreOntologyBundle, OntologyMetadata, OntologyStats};
pub use loader::OntologyLoader;
pub use standard_ontologies::{get_standard_namespaces, NamespaceInfo, StandardOntology};
