use anyhow::{Context, Result};
use std::path::{Path, PathBuf};
use glob::glob;

use crate::cache::{CacheManager, CachedPack};
use crate::lockfile::LockfileManager;

/// Template resolver for rpack:template syntax
#[derive(Debug, Clone)]
pub struct TemplateResolver {
    cache_manager: CacheManager,
    lockfile_manager: LockfileManager,
}

/// Resolved template source
#[derive(Debug, Clone)]
pub struct TemplateSource {
    pub pack_id: String,
    pub template_path: PathBuf,
    pub pack: CachedPack,
    pub manifest: Option<crate::cache::RpackManifest>,
}

/// Template search result
#[derive(Debug, Clone)]
pub struct TemplateSearchResult {
    pub pack_id: String,
    pub template_path: PathBuf,
    pub pack_name: String,
    pub pack_description: String,
}

impl TemplateResolver {
    /// Create a new template resolver
    pub fn new(cache_manager: CacheManager, lockfile_manager: LockfileManager) -> Self {
        Self {
            cache_manager,
            lockfile_manager,
        }
    }

    /// Resolve a template reference in the format "pack_id:template_path"
    pub fn resolve(&self, template_ref: &str) -> Result<TemplateSource> {
        let (pack_id, template_path) = self.parse_template_ref(template_ref)?;
        
        // Get pack from lockfile
        let lock_entry = self.lockfile_manager.get(&pack_id)?
            .with_context(|| format!("Pack '{}' not found in lockfile", pack_id))?;
        
        // Load cached pack
        let cached_pack = self.cache_manager.load_cached(&pack_id, &lock_entry.version)
            .with_context(|| format!("Pack '{}' not found in cache", pack_id))?;
        
        // Resolve template path
        let full_template_path = self.resolve_template_path(&cached_pack, &template_path)?;
        
        // Verify template exists
        if !full_template_path.exists() {
            anyhow::bail!("Template '{}' not found in pack '{}'", template_path, pack_id);
        }
        
        let manifest = cached_pack.manifest.clone();
        
        Ok(TemplateSource {
            pack_id,
            template_path: full_template_path,
            pack: cached_pack,
            manifest,
        })
    }

    /// Parse a template reference into pack ID and template path
    fn parse_template_ref(&self, template_ref: &str) -> Result<(String, String)> {
        let parts: Vec<&str> = template_ref.split(':').collect();
        
        if parts.len() != 2 {
            anyhow::bail!("Invalid template reference format: '{}'. Expected 'pack_id:template_path'", template_ref);
        }
        
        let pack_id = parts[0].to_string();
        let template_path = parts[1].to_string();
        
        if pack_id.is_empty() {
            anyhow::bail!("Empty pack ID in template reference: '{}'", template_ref);
        }
        
        if template_path.is_empty() {
            anyhow::bail!("Empty template path in template reference: '{}'", template_ref);
        }
        
        Ok((pack_id, template_path))
    }

    /// Resolve template path relative to pack directory
    fn resolve_template_path(&self, cached_pack: &CachedPack, template_path: &str) -> Result<PathBuf> {
        // Start with templates directory
        let mut full_path = cached_pack.path.join("templates");
        
        // Add template path components
        for component in template_path.split('/') {
            if component == ".." {
                anyhow::bail!("Template path cannot contain '..': {}", template_path);
            }
            if component.is_empty() {
                continue;
            }
            full_path = full_path.join(component);
        }
        
        Ok(full_path)
    }

    /// Search for templates across all installed packs
    pub fn search_templates(&self, query: Option<&str>) -> Result<Vec<TemplateSearchResult>> {
        let installed_packs = self.lockfile_manager.installed_packs()?;
        let mut results = Vec::new();
        
        for (pack_id, lock_entry) in installed_packs {
            if let Ok(cached_pack) = self.cache_manager.load_cached(&pack_id, &lock_entry.version) {
                let pack_templates = self.find_templates_in_pack(&cached_pack)?;
                
                for template_path in pack_templates {
                    let template_name = template_path.file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or("unknown");
                    
                    // Filter by query if provided
                    if let Some(query) = query {
                        let query_lower = query.to_lowercase();
                        if !template_name.to_lowercase().contains(&query_lower) {
                            continue;
                        }
                    }
                    
                    results.push(TemplateSearchResult {
                        pack_id: pack_id.clone(),
                        template_path: template_path.clone(),
                        pack_name: cached_pack.manifest.as_ref()
                            .map(|m| m.rpack.name.clone())
                            .unwrap_or_else(|| pack_id.clone()),
                        pack_description: cached_pack.manifest.as_ref()
                            .map(|m| m.rpack.description.clone())
                            .unwrap_or_else(|| "No description".to_string()),
                    });
                }
            }
        }
        
        // Sort by pack name, then template name
        results.sort_by(|a, b| {
            a.pack_name.cmp(&b.pack_name)
                .then_with(|| a.template_path.cmp(&b.template_path))
        });
        
        Ok(results)
    }

    /// Find all templates in a pack
    fn find_templates_in_pack(&self, cached_pack: &CachedPack) -> Result<Vec<PathBuf>> {
        let templates_dir = cached_pack.path.join("templates");
        let mut templates = Vec::new();
        
        if !templates_dir.exists() {
            return Ok(templates);
        }
        
        // Use glob to find all .tmpl files
        let pattern = templates_dir.join("**").join("*.tmpl");
        let pattern_str = pattern.to_string_lossy();
        
        for entry in glob(&pattern_str)
            .context("Failed to glob template files")? {
            let entry = entry.context("Failed to read glob entry")?;
            
            if entry.is_file() {
                templates.push(entry);
            }
        }
        
        Ok(templates)
    }

    /// Get available templates for a specific pack
    pub fn get_pack_templates(&self, pack_id: &str) -> Result<Vec<String>> {
        let lock_entry = self.lockfile_manager.get(pack_id)?
            .with_context(|| format!("Pack '{}' not found in lockfile", pack_id))?;
        
        let cached_pack = self.cache_manager.load_cached(pack_id, &lock_entry.version)
            .with_context(|| format!("Pack '{}' not found in cache", pack_id))?;
        
        let templates_dir = cached_pack.path.join("templates");
        let mut template_paths = Vec::new();
        
        if !templates_dir.exists() {
            return Ok(template_paths);
        }
        
        // Find all .tmpl files and convert to relative paths
        let pattern = templates_dir.join("**").join("*.tmpl");
        let pattern_str = pattern.to_string_lossy();
        
        for entry in glob(&pattern_str)
            .context("Failed to glob template files")? {
            let entry = entry.context("Failed to read glob entry")?;
            
            if entry.is_file() {
                // Get relative path from templates directory
                let relative_path = entry.strip_prefix(&templates_dir)
                    .context("Failed to get relative template path")?;
                
                template_paths.push(relative_path.to_string_lossy().to_string());
            }
        }
        
        // Sort for consistent output
        template_paths.sort();
        
        Ok(template_paths)
    }

    /// Get template information including frontmatter
    pub fn get_template_info(&self, template_ref: &str) -> Result<TemplateInfo> {
        let template_source = self.resolve(template_ref)?;
        
        // Read template content
        let content = std::fs::read_to_string(&template_source.template_path)
            .context("Failed to read template file")?;
        
        // Parse frontmatter if present
        let (frontmatter, template_content) = self.parse_frontmatter(&content)?;
        
        Ok(TemplateInfo {
            pack_id: template_source.pack_id,
            template_path: template_source.template_path,
            frontmatter,
            content: template_content,
            pack_info: template_source.manifest.map(|m| m.rpack),
        })
    }

    /// Parse frontmatter from template content
    fn parse_frontmatter(&self, content: &str) -> Result<(Option<serde_yaml::Value>, String)> {
        use gray_matter::Matter;
        
        let matter = Matter::<gray_matter::engine::YAML>::new();
        let parsed = matter.parse(content)?;
        
        let frontmatter = parsed.data.map(|data: serde_yaml::Value| data.into());
        let content = parsed.content;
        
        Ok((frontmatter, content))
    }

    /// Validate that all referenced templates exist
    pub fn validate_templates(&self) -> Result<Vec<TemplateValidationError>> {
        let installed_packs = self.lockfile_manager.installed_packs()?;
        let mut errors = Vec::new();
        
        for (pack_id, lock_entry) in installed_packs {
            if let Ok(cached_pack) = self.cache_manager.load_cached(&pack_id, &lock_entry.version) {
                // Check if templates directory exists
                let templates_dir = cached_pack.path.join("templates");
                if !templates_dir.exists() {
                    errors.push(TemplateValidationError {
                        pack_id: pack_id.clone(),
                        error: "Templates directory not found".to_string(),
                    });
                    continue;
                }
                
                // Check if any templates exist
                let templates = self.find_templates_in_pack(&cached_pack)?;
                if templates.is_empty() {
                    errors.push(TemplateValidationError {
                        pack_id: pack_id.clone(),
                        error: "No template files found".to_string(),
                    });
                }
                
                // Validate each template
                for template_path in templates {
                    if let Err(e) = self.validate_template_file(&template_path) {
                        errors.push(TemplateValidationError {
                            pack_id: pack_id.clone(),
                            error: format!("Template '{}': {}", 
                                template_path.file_name().unwrap_or_default().to_string_lossy(), e),
                        });
                    }
                }
            }
        }
        
        Ok(errors)
    }

    /// Validate a single template file
    fn validate_template_file(&self, template_path: &Path) -> Result<()> {
        let content = std::fs::read_to_string(template_path)
            .context("Failed to read template file")?;
        
        // Check if file is not empty
        if content.trim().is_empty() {
            anyhow::bail!("Template file is empty");
        }
        
        // Try to parse frontmatter
        let (frontmatter, _) = self.parse_frontmatter(&content)?;
        
        // Validate frontmatter structure if present
        if let Some(fm) = frontmatter {
            if let Some(fm_map) = fm.as_mapping() {
                // Check for required fields
                if let Some(to_field) = fm_map.get("to") {
                    if !to_field.is_string() {
                        anyhow::bail!("Frontmatter 'to' field must be a string");
                    }
                }
            }
        }
        
        Ok(())
    }
}

/// Template information
#[derive(Debug, Clone)]
pub struct TemplateInfo {
    pub pack_id: String,
    pub template_path: PathBuf,
    pub frontmatter: Option<serde_yaml::Value>,
    pub content: String,
    pub pack_info: Option<crate::cache::RpackInfo>,
}

/// Template validation error
#[derive(Debug, Clone)]
pub struct TemplateValidationError {
    pub pack_id: String,
    pub error: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use std::fs;

    #[test]
    fn test_parse_template_ref() {
        let temp_dir = TempDir::new().unwrap();
        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);
        
        let (pack_id, template_path) = resolver.parse_template_ref("io.rgen.test:main.tmpl").unwrap();
        assert_eq!(pack_id, "io.rgen.test");
        assert_eq!(template_path, "main.tmpl");
    }

    #[test]
    fn test_parse_template_ref_invalid() {
        let temp_dir = TempDir::new().unwrap();
        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);
        
        // Invalid format
        assert!(resolver.parse_template_ref("invalid").is_err());
        
        // Empty pack ID
        assert!(resolver.parse_template_ref(":template.tmpl").is_err());
        
        // Empty template path
        assert!(resolver.parse_template_ref("pack:").is_err());
    }

    #[test]
    fn test_resolve_template_path() {
        let temp_dir = TempDir::new().unwrap();
        let pack_dir = temp_dir.path().join("pack");
        let templates_dir = pack_dir.join("templates");
        fs::create_dir_all(&templates_dir).unwrap();
        
        let cached_pack = CachedPack {
            id: "io.rgen.test".to_string(),
            version: "1.0.0".to_string(),
            path: pack_dir,
            sha256: "abc123".to_string(),
            manifest: None,
        };
        
        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);
        
        let resolved_path = resolver.resolve_template_path(&cached_pack, "main.tmpl").unwrap();
        assert_eq!(resolved_path, templates_dir.join("main.tmpl"));
    }

    #[test]
    fn test_resolve_template_path_security() {
        let temp_dir = TempDir::new().unwrap();
        let pack_dir = temp_dir.path().join("pack");
        let cached_pack = CachedPack {
            id: "io.rgen.test".to_string(),
            version: "1.0.0".to_string(),
            path: pack_dir,
            sha256: "abc123".to_string(),
            manifest: None,
        };
        
        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);
        
        // Should reject path traversal
        assert!(resolver.resolve_template_path(&cached_pack, "../outside.tmpl").is_err());
    }
}