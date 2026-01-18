//! Generation safety module - Poka-Yoke controls for code generation
//!
//! This module implements the behavior for ggen.toml configuration sections:
//! - `[generation].protected_paths` - paths that MUST NEVER be overwritten
//! - `[generation].regenerate_paths` - paths safe to overwrite
//! - `[generation.poka_yoke].warning_headers` - header injection in generated files
//! - `[codeowners]` - CODEOWNERS file generation from OWNERS files

pub mod codeowners;
pub mod headers;
pub mod protection;

pub use codeowners::{
    generate_codeowners, generate_codeowners_default, should_regenerate, CodeownersConfig,
    CodeownersResult,
};
pub use headers::{
    format_header_for_extension, inject_warning_header, GenerationSafetyConfig,
    HeaderInjectionConfig, PokaYokeSettings,
};
pub use protection::{validate_generation_write, GenerationWriteResult, PathProtectionValidator};
