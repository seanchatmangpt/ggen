//! Minimal TOML config loading abstractions for ggen.
//!
//! Two responsibilities:
//!
//! 1. **Generic loader** — [`TomlLoader`] loads any `T: DeserializeOwned` from a TOML file
//!    or string, with `${VAR}` / `$VAR` environment-variable expansion and upward directory
//!    search ([`find_config_file`]).
//!
//! 2. **Pipeline schema** — typed structs for the `ggen.toml` format:
//!    [`GgenToml`], [`GenerationSection`], [`GenerationRule`], [`PipelineStage`],
//!    [`QuerySource`], [`TemplateSource`], [`OutputMode`].
//!
//! # Minimal dependencies
//!
//! `serde`, `toml`, `thiserror` — no crypto, no async runtime, no serde_json.
//!
//! # Example
//!
//! ```rust
//! use ggen_toml::{TomlLoader, GgenToml};
//!
//! let toml = r#"
//! [project]
//! name = "my-project"
//! version = "0.1.0"
//!
//! [generation]
//! output_dir = "src/gen"
//!
//! [[generation.rules]]
//! name = "structs"
//! query = { file = "queries/structs.rq" }
//! template = { file = "templates/structs.tera" }
//! output_file = "structs.rs"
//! "#;
//!
//! let cfg: GgenToml = TomlLoader::from_str(toml).unwrap();
//! assert_eq!(cfg.project.name, "my-project");
//! let gen = cfg.generation.unwrap();
//! assert_eq!(gen.rules[0].name, "structs");
//! ```

pub mod error;
pub mod loader;
pub mod schema;

pub use error::{ConfigError, Result};
pub use loader::{expand_env_vars, find_config_file, TomlLoader};
pub use schema::{
    GenerationRule, GenerationSection, GgenToml, OntologyConfig, OutputMode, OutputSection,
    PipelineStage, ProjectConfig, QuerySource, RdfSection, SyncConfig, TemplateSource,
    TemplatesSection, ValidationConfig, Validate,
};
