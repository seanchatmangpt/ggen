//! Code generation module
//!
//! This module provides generators for different target languages and frameworks,
//! converting OntologySchema into working code for TypeScript, GraphQL, SQL, and more.

pub mod typescript;

pub use typescript::TypeScriptGenerator;
