//! Template resolver for gpack:template syntax
//!
//! This module provides template resolution functionality.
//! Note: The pack resolution is now handled by `PackResolver` in `pack_resolver.rs`.

use ggen_utils::error::{Error, Result};
use std::path::PathBuf;

/// Template resolver for gpack:template syntax
#[derive(Debug, Clone)]
pub struct TemplateResolver {
    _private: (),
}

/// Resolved template source
#[derive(Debug, Clone)]
pub struct TemplateSource {
    pub pack_id: String,
    pub template_path: PathBuf,
}

/// Template search result
#[derive(Debug, Clone)]
pub struct TemplateSearchResult {
    pub pack_id: String,
    pub template_path: PathBuf,
}

impl TemplateResolver {
    /// Create a new template resolver (stub - compatible with existing API)
    pub fn new(_cache_manager: crate::cache::CacheManager, _lockfile_manager: crate::config::LockfileManager) -> Self {
        Self { _private: () }
    }

    /// Resolve a template reference (stub)
    pub fn resolve(&self, reference: &str) -> Result<TemplateSource> {
        // This is a stub. Use PackResolver for actual pack template resolution.
        Err(Error::new(&format!(
            "TemplateResolver is a stub. Cannot resolve '{}'. Use PackResolver instead.",
            reference
        )))
    }

    /// Search templates (stub)
    pub fn search_templates(&self, _query: Option<&str>) -> Result<Vec<TemplateSearchResult>> {
        Ok(Vec::new())
    }
}
