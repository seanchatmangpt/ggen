//! SPARQL query utilities for domain-specific RDF extraction
//!
//! This module provides SPARQL query builders for extracting domain-specific
//! data from RDF ontologies. Each submodule focuses on a specific domain or
//! programming language.

pub mod erlang;

pub use erlang::{
    query_config_params, query_dependencies, query_gen_server_state, query_modules,
    query_supervision_tree, validate_query,
};
