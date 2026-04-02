//! Template resolver for pack_id:template_path syntax
//!
//! This module provides template resolution from cached packs.
//! Templates can be referenced as `pack_id:template_path` in ggen.toml
//! and will be loaded from the pack cache at `~/.cache/ggen/packs/<pack-id>/templates/`.

use ggen_utils::error::{Error, Result};
use std::path::PathBuf;
use tracing::{debug, info};

/// Template resolver for pack_id:template_path syntax
#[derive(Debug, Clone)]
pub struct TemplateResolver {
    /// Cache directory where packs are stored
    cache_dir: PathBuf,
}

/// Resolved template source with content
#[derive(Debug, Clone)]
pub struct TemplateSource {
    /// Pack ID that provides this template
    pub pack_id: String,
    /// Relative path to template within pack
    pub template_path: PathBuf,
    /// Full filesystem path to cached template
    pub full_path: PathBuf,
    /// Template content
    pub content: String,
}

/// Template search result
#[derive(Debug, Clone)]
pub struct TemplateSearchResult {
    pub pack_id: String,
    pub template_path: PathBuf,
}

impl TemplateResolver {
    /// Create a new template resolver with default cache location
    ///
    /// # Errors
    ///
    /// Returns error if cache directory cannot be resolved.
    pub fn new() -> Result<Self> {
        let cache_dir = std::env::var_os("GGEN_PACK_CACHE_DIR")
            .map(PathBuf::from)
            .or_else(|| dirs::cache_dir().map(|d| d.join("ggen").join("packs")))
            .ok_or_else(|| {
                Error::new("Cannot resolve pack cache directory: set HOME or GGEN_PACK_CACHE_DIR")
            })?;

        Ok(Self { cache_dir })
    }

    /// Create a new template resolver with custom cache directory
    pub fn with_cache_dir(cache_dir: PathBuf) -> Self {
        Self { cache_dir }
    }

    /// Resolve a template reference from `pack_id:template_path` syntax
    ///
    /// # Arguments
    ///
    /// * `reference` - Template reference in format `pack_id:template_path`
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Reference format is invalid
    /// - Pack is not cached
    /// - Template file not found
    /// - Template cannot be read
    ///
    /// # Examples
    ///
    /// ```
    /// let resolver = TemplateResolver::new()?;
    /// let template = resolver.resolve("surface-mcp:handlers.rs.tera")?;
    /// assert_eq!(template.pack_id, "surface-mcp");
    /// ```
    pub fn resolve(&self, reference: &str) -> Result<TemplateSource> {
        // Parse pack_id:template_path syntax
        let (pack_id, template_path) = self.parse_reference(reference)?;

        // Build full path to cached template
        let pack_cache_dir = self.cache_dir.join(&pack_id);
        let templates_dir = pack_cache_dir.join("templates");
        let full_path = templates_dir.join(&template_path);

        // Verify pack exists in cache
        if !pack_cache_dir.exists() {
            return Err(Error::new(&format!(
                "Pack '{}' is not cached. Run 'ggen packs install {}' first.",
                pack_id, pack_id
            )));
        }

        // Verify template file exists
        if !full_path.exists() {
            return Err(Error::new(&format!(
                "Template '{}' not found in pack '{}'. Available templates:\n{}",
                template_path.display(),
                pack_id,
                self.list_available_templates(&pack_id)?
            )));
        }

        // Read template content
        let content = std::fs::read_to_string(&full_path).map_err(|e| {
            Error::new(&format!(
                "Failed to read template '{}': {}",
                full_path.display(),
                e
            ))
        })?;

        info!(
            "Resolved template: {} -> {}",
            reference,
            full_path.display()
        );

        Ok(TemplateSource {
            pack_id,
            template_path,
            full_path,
            content,
        })
    }

    /// Check if a reference is a pack template reference
    pub fn is_pack_reference(reference: &str) -> bool {
        reference.contains(':')
    }

    /// Parse pack_id:template_path reference
    fn parse_reference(&self, reference: &str) -> Result<(String, PathBuf)> {
        if !Self::is_pack_reference(reference) {
            return Err(Error::new(&format!(
                "Invalid template reference '{}'. Expected format: pack_id:template_path",
                reference
            )));
        }

        let parts: Vec<&str> = reference.splitn(2, ':').collect();
        let pack_id = parts[0].to_string();
        let template_path = PathBuf::from(parts[1]);

        if pack_id.is_empty() {
            return Err(Error::new("Pack ID cannot be empty"));
        }

        if template_path.as_os_str().is_empty() {
            return Err(Error::new("Template path cannot be empty"));
        }

        Ok((pack_id, template_path))
    }

    /// List available templates for a pack
    fn list_available_templates(&self, pack_id: &str) -> Result<String> {
        let templates_dir = self.cache_dir.join(pack_id).join("templates");

        if !templates_dir.exists() {
            return Ok(format!("  (no templates directory in pack '{}')", pack_id));
        }

        let mut templates = Vec::new();

        // Walk templates directory
        if let Ok(entries) = std::fs::read_dir(&templates_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() {
                    if let Ok(relative) = path.strip_prefix(&templates_dir) {
                        templates.push(format!("  - {}", relative.display()));
                    }
                }
            }
        }

        if templates.is_empty() {
            Ok(format!("  (no templates found in pack '{}')", pack_id))
        } else {
            Ok(templates.join("\n"))
        }
    }

    /// Search templates across all cached packs
    ///
    /// # Arguments
    ///
    /// * `query` - Optional search query to filter results
    ///
    /// # Errors
    ///
    /// Returns error if cache directory cannot be read.
    pub fn search_templates(&self, query: Option<&str>) -> Result<Vec<TemplateSearchResult>> {
        let mut results = Vec::new();

        // Read all pack directories
        if !self.cache_dir.exists() {
            return Ok(results);
        }

        let pack_entries = std::fs::read_dir(&self.cache_dir)
            .map_err(|e| Error::new(&format!("Failed to read cache directory: {}", e)))?;

        for pack_entry in pack_entries.flatten() {
            let pack_dir = pack_entry.path();
            if !pack_dir.is_dir() {
                continue;
            }

            let pack_id = pack_dir
                .file_name()
                .and_then(|n| n.to_str())
                .unwrap_or("unknown")
                .to_string();

            let templates_dir = pack_dir.join("templates");
            if !templates_dir.exists() {
                continue;
            }

            // Read templates
            if let Ok(template_entries) = std::fs::read_dir(&templates_dir) {
                for template_entry in template_entries.flatten() {
                    let template_path = template_entry.path();
                    if !template_path.is_file() {
                        continue;
                    }

                    let relative_path = template_path
                        .strip_prefix(&templates_dir)
                        .unwrap_or(&template_path);

                    // Apply query filter
                    if let Some(q) = query {
                        let search_text = format!("{}:{}", pack_id, relative_path.display());
                        if !search_text.to_lowercase().contains(&q.to_lowercase()) {
                            continue;
                        }
                    }

                    results.push(TemplateSearchResult {
                        pack_id: pack_id.clone(),
                        template_path: relative_path.to_path_buf(),
                    });
                }
            }
        }

        debug!(
            "Found {} templates matching query '{:?}'",
            results.len(),
            query
        );

        Ok(results)
    }
}

impl Default for TemplateResolver {
    fn default() -> Self {
        Self::new().unwrap_or_else(|e| {
            panic!("Failed to create default TemplateResolver: {}", e);
        })
    }
}
