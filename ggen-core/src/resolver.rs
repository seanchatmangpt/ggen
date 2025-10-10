use anyhow::{Context, Result};
use glob::glob;
use std::path::PathBuf;

use crate::cache::{CacheManager, CachedPack};
use crate::lockfile::LockfileManager;

/// Template resolver for gpack:template syntax
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
    pub manifest: Option<crate::gpack::GpackManifest>,
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
        let lock_entry = self
            .lockfile_manager
            .get(&pack_id)?
            .with_context(|| format!("Pack '{}' not found in lockfile", pack_id))?;

        // Load cached pack
        let cached_pack = self
            .cache_manager
            .load_cached(&pack_id, &lock_entry.version)
            .with_context(|| format!("Pack '{}' not found in cache", pack_id))?;

        // Resolve template path
        let full_template_path = self.resolve_template_path(&cached_pack, &template_path)?;

        // Verify template exists
        if !full_template_path.exists() {
            anyhow::bail!(
                "Template '{}' not found in pack '{}'",
                template_path,
                pack_id
            );
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
            anyhow::bail!(
                "Invalid template reference format: '{}'. Expected 'pack_id:template_path'",
                template_ref
            );
        }

        let pack_id = parts[0].to_string();
        let template_path = parts[1].to_string();

        if pack_id.is_empty() {
            anyhow::bail!("Empty pack ID in template reference: '{}'", template_ref);
        }

        if template_path.is_empty() {
            anyhow::bail!(
                "Empty template path in template reference: '{}'",
                template_ref
            );
        }

        Ok((pack_id, template_path))
    }

    /// Resolve template path relative to pack directory
    fn resolve_template_path(
        &self, cached_pack: &CachedPack, template_path: &str,
    ) -> Result<PathBuf> {
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
            if let Ok(cached_pack) = self
                .cache_manager
                .load_cached(&pack_id, &lock_entry.version)
            {
                let pack_templates = self.find_templates_in_pack(&cached_pack)?;

                for template_path in pack_templates {
                    let template_name = template_path
                        .file_name()
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
                        pack_name: cached_pack
                            .manifest
                            .as_ref()
                            .map(|m| m.metadata.name.clone())
                            .unwrap_or_else(|| pack_id.clone()),
                        pack_description: cached_pack
                            .manifest
                            .as_ref()
                            .map(|m| m.metadata.description.clone())
                            .unwrap_or_else(|| "No description".to_string()),
                    });
                }
            }
        }

        // Sort by pack name, then template name
        results.sort_by(|a, b| {
            a.pack_name
                .cmp(&b.pack_name)
                .then_with(|| a.template_path.cmp(&b.template_path))
        });

        Ok(results)
    }

    /// Find all templates in a pack using manifest discovery
    fn find_templates_in_pack(&self, cached_pack: &CachedPack) -> Result<Vec<PathBuf>> {
        if let Some(manifest) = &cached_pack.manifest {
            manifest.discover_templates(&cached_pack.path)
        } else {
            // Fallback to default convention if no manifest
            let conventions = crate::gpack::PackConventions::default();
            let mut templates = Vec::new();

            for pattern in conventions.template_patterns {
                let full_pattern = cached_pack.path.join(pattern);
                for entry in glob(&full_pattern.to_string_lossy())? {
                    templates.push(entry?);
                }
            }

            templates.sort();
            Ok(templates)
        }
    }

    /// Get available templates for a specific pack
    pub fn get_pack_templates(&self, pack_id: &str) -> Result<Vec<String>> {
        let lock_entry = self
            .lockfile_manager
            .get(pack_id)?
            .with_context(|| format!("Pack '{}' not found in lockfile", pack_id))?;

        let cached_pack = self
            .cache_manager
            .load_cached(pack_id, &lock_entry.version)
            .with_context(|| format!("Pack '{}' not found in cache", pack_id))?;

        // Use manifest discovery to find templates
        let templates = self.find_templates_in_pack(&cached_pack)?;
        let templates_dir = cached_pack.path.join("templates");
        let mut template_paths = Vec::new();

        for template_path in templates {
            // Get relative path from templates directory
            let relative_path = template_path
                .strip_prefix(&templates_dir)
                .context("Failed to get relative template path")?;

            template_paths.push(relative_path.to_string_lossy().to_string());
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
            pack_info: template_source.manifest.map(|m| m.metadata),
        })
    }

    /// Parse frontmatter from template content
    fn parse_frontmatter(&self, content: &str) -> Result<(Option<serde_yaml::Value>, String)> {
        use gray_matter::Matter;

        let matter = Matter::<gray_matter::engine::YAML>::new();
        let parsed = matter.parse(content)?;

        let frontmatter = parsed.data.map(|data: serde_yaml::Value| data);
        let content = parsed.content;

        Ok((frontmatter, content))
    }
}

/// Template information
#[derive(Debug, Clone)]
pub struct TemplateInfo {
    pub pack_id: String,
    pub template_path: PathBuf,
    pub frontmatter: Option<serde_yaml::Value>,
    pub content: String,
    pub pack_info: Option<crate::gpack::GpackMetadata>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_parse_template_ref() {
        let temp_dir = TempDir::new().unwrap();
        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        let (pack_id, template_path) = resolver
            .parse_template_ref("io.ggen.test:main.tmpl")
            .unwrap();
        assert_eq!(pack_id, "io.ggen.test");
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
            id: "io.ggen.test".to_string(),
            version: "1.0.0".to_string(),
            path: pack_dir,
            sha256: "abc123".to_string(),
            manifest: None,
        };

        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        let resolved_path = resolver
            .resolve_template_path(&cached_pack, "main.tmpl")
            .unwrap();
        assert_eq!(resolved_path, templates_dir.join("main.tmpl"));
    }

    #[test]
    fn test_resolve_template_path_security() {
        let temp_dir = TempDir::new().unwrap();
        let pack_dir = temp_dir.path().join("pack");
        let cached_pack = CachedPack {
            id: "io.ggen.test".to_string(),
            version: "1.0.0".to_string(),
            path: pack_dir,
            sha256: "abc123".to_string(),
            manifest: None,
        };

        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        // Should reject path traversal
        assert!(resolver
            .resolve_template_path(&cached_pack, "../outside.tmpl")
            .is_err());
    }

    #[test]
    fn test_template_resolver_new() {
        let temp_dir = TempDir::new().unwrap();
        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());

        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        // Resolver should be created successfully
        assert!(resolver.cache_manager.cache_dir().exists());
    }

    #[test]
    fn test_resolve_template_path_nested() {
        let temp_dir = TempDir::new().unwrap();
        let pack_dir = temp_dir.path().join("pack");
        let templates_dir = pack_dir.join("templates");
        fs::create_dir_all(&templates_dir).unwrap();

        let cached_pack = CachedPack {
            id: "io.ggen.test".to_string(),
            version: "1.0.0".to_string(),
            path: pack_dir,
            sha256: "abc123".to_string(),
            manifest: None,
        };

        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        let resolved_path = resolver
            .resolve_template_path(&cached_pack, "nested/sub.tmpl")
            .unwrap();
        assert_eq!(resolved_path, templates_dir.join("nested").join("sub.tmpl"));
    }

    #[test]
    fn test_resolve_template_path_empty_components() {
        let temp_dir = TempDir::new().unwrap();
        let pack_dir = temp_dir.path().join("pack");
        let templates_dir = pack_dir.join("templates");
        fs::create_dir_all(&templates_dir).unwrap();

        let cached_pack = CachedPack {
            id: "io.ggen.test".to_string(),
            version: "1.0.0".to_string(),
            path: pack_dir,
            sha256: "abc123".to_string(),
            manifest: None,
        };

        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        // Should handle empty path components
        let resolved_path = resolver
            .resolve_template_path(&cached_pack, "a//b/")
            .unwrap();
        assert_eq!(resolved_path, templates_dir.join("a").join("b"));
    }

    #[test]
    fn test_parse_frontmatter_basic() {
        let temp_dir = TempDir::new().unwrap();
        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        let content = r#"---
to: "output.txt"
vars:
  name: "Test"
---
Hello {{ name }}
"#;

        let (frontmatter, template_content) = resolver.parse_frontmatter(content).unwrap();

        assert!(frontmatter.is_some());
        assert!(template_content.contains("Hello {{ name }}"));
    }

    #[test]
    fn test_parse_frontmatter_no_frontmatter() {
        let temp_dir = TempDir::new().unwrap();
        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        let content = "Hello World";

        let (frontmatter, template_content) = resolver.parse_frontmatter(content).unwrap();

        assert!(frontmatter.is_none());
        assert_eq!(template_content, "Hello World");
    }

    #[test]
    fn test_find_templates_in_pack_with_manifest() {
        let temp_dir = TempDir::new().unwrap();
        let pack_dir = temp_dir.path().join("pack");
        let templates_dir = pack_dir.join("templates");
        fs::create_dir_all(&templates_dir).unwrap();

        // Create template files
        fs::write(templates_dir.join("main.tmpl"), "template1").unwrap();
        fs::write(templates_dir.join("sub.tmpl"), "template2").unwrap();

        let manifest = crate::gpack::GpackManifest {
            metadata: crate::gpack::GpackMetadata {
                id: "io.ggen.test".to_string(),
                name: "test-pack".to_string(),
                version: "1.0.0".to_string(),
                description: "Test pack".to_string(),
                license: "MIT".to_string(),
                ggen_compat: "1.0.0".to_string(),
            },
            dependencies: std::collections::BTreeMap::new(),
            templates: crate::gpack::TemplatesConfig {
                patterns: vec![
                    "templates/main.tmpl".to_string(),
                    "templates/sub.tmpl".to_string(),
                ],
                includes: vec![],
            },
            macros: crate::gpack::MacrosConfig::default(),
            rdf: crate::gpack::RdfConfig {
                base: None,
                prefixes: std::collections::BTreeMap::new(),
                patterns: vec![],
                inline: vec![],
            },
            queries: crate::gpack::QueriesConfig::default(),
            shapes: crate::gpack::ShapesConfig::default(),
            preset: crate::gpack::PresetConfig::default(),
        };

        let cached_pack = CachedPack {
            id: "io.ggen.test".to_string(),
            version: "1.0.0".to_string(),
            path: pack_dir,
            sha256: "abc123".to_string(),
            manifest: Some(manifest),
        };

        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        let templates = resolver.find_templates_in_pack(&cached_pack).unwrap();

        assert_eq!(templates.len(), 2);
        assert!(templates.iter().any(|t| t.ends_with("main.tmpl")));
        assert!(templates.iter().any(|t| t.ends_with("sub.tmpl")));
    }

    #[test]
    fn test_find_templates_in_pack_without_manifest() {
        let temp_dir = TempDir::new().unwrap();
        let pack_dir = temp_dir.path().join("pack");
        let templates_dir = pack_dir.join("templates");
        fs::create_dir_all(&templates_dir).unwrap();

        // Create template files
        fs::write(templates_dir.join("main.tmpl"), "template1").unwrap();
        fs::write(templates_dir.join("sub.tmpl"), "template2").unwrap();

        let cached_pack = CachedPack {
            id: "io.ggen.test".to_string(),
            version: "1.0.0".to_string(),
            path: pack_dir,
            sha256: "abc123".to_string(),
            manifest: None,
        };

        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        let templates = resolver.find_templates_in_pack(&cached_pack).unwrap();

        assert_eq!(templates.len(), 2);
        assert!(templates.iter().any(|t| t.ends_with("main.tmpl")));
        assert!(templates.iter().any(|t| t.ends_with("sub.tmpl")));
    }

    #[test]
    fn test_search_templates_empty() {
        let temp_dir = TempDir::new().unwrap();
        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        let results = resolver.search_templates(None).unwrap();
        assert!(results.is_empty());
    }

    #[test]
    fn test_get_pack_templates_nonexistent_pack() {
        let temp_dir = TempDir::new().unwrap();
        let cache_manager = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
        let lockfile_manager = LockfileManager::new(temp_dir.path());
        let resolver = TemplateResolver::new(cache_manager, lockfile_manager);

        // Should fail for nonexistent pack
        assert!(resolver.get_pack_templates("nonexistent.pack").is_err());
    }
}
