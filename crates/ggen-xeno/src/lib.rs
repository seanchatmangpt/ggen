//! # Xenobiological Ontology Adapter
//!
//! A speculative framework for alien knowledge representation systems and
//! universal semantic translation for non-human intelligence communication protocols.
//!
//! ## Overview
//!
//! This crate provides a compatibility layer for hypothetical extraterrestrial
//! knowledge systems, enabling interoperability between human and alien ontologies.
//! It explores exo-computing paradigms and provides tools for interstellar
//! software archaeology missions.
//!
//! ## Architecture
//!
//! - **Ontology Abstraction**: Core traits for alien knowledge representation
//! - **Semantic Translation**: Universal translator between ontological paradigms
//! - **Alien Systems**: Reference implementations (Crystalline, Quantum, Collective, Temporal)
//! - **Exo-Computing**: Support for non-human computational models
//! - **Software Archaeology**: Tools for analyzing ancient alien code
//!
//! ## Example
//!
//! ```rust
//! use ggen_xeno::ontology::XenoOntology;
//! use ggen_xeno::translator::UniversalTranslator;
//! use ggen_xeno::systems::crystalline::CrystallineOntology;
//!
//! # fn main() -> anyhow::Result<()> {
//! // Create a crystalline (silicon-based) knowledge system
//! let crystalline = CrystallineOntology::new("Alpha-Centauri-Prime");
//!
//! // Initialize universal semantic translator
//! let translator = UniversalTranslator::new();
//!
//! // Translate between human and alien ontologies
//! // let human_concept = translator.to_human(&crystalline_concept)?;
//! # Ok(())
//! # }
//! ```

#![warn(missing_docs)]
#![warn(clippy::all)]
#![warn(clippy::pedantic)]

pub mod ontology;
pub mod translator;
pub mod systems;
pub mod paradigms;
pub mod archaeology;

// Re-export commonly used types
pub use ontology::{XenoOntology, Concept, Relation, OntologyMetadata};
pub use translator::UniversalTranslator;
pub use paradigms::ComputationalParadigm;
