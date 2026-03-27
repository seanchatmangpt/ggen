//! Reverse synchronization - Code to RDF extraction
//!
//! This module provides functionality to extract service definitions from source code
//! in multiple languages (Rust, Elixir, Go) and convert them into RDF format for
//! integration with the ggen ontology system.

pub mod ast_extractor;

pub use ast_extractor::{
    convert_to_rdf, extract_elixir_genserver, extract_go_service, extract_rust_service, Field,
    Language, Method, ServiceDef,
};
