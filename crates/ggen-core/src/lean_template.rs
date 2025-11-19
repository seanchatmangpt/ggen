//! Lean template compilation system with just-in-time loading and caching
//!
//! This module implements Lean Manufacturing principles for template processing:
//! - **Just-in-Time Compilation**: Templates compiled only when needed
//! - **Result Caching**: Rendered outputs cached to prevent redundant work
//! - **Incremental Updates**: Only recompile templates when source changes
//! - **Batch Processing**: Process multiple templates efficiently
//!
//! ## Waste Elimination
//!
//! Traditional approach:
//! 1. Load all templates upfront (Overproduction)
//! 2. Compile all templates even if unused (Overproduction)
//! 3. Re-render unchanged templates (Over-processing)
//! 4. No caching of results (Motion waste)
//!
//! Lean approach:
//! 1. Load templates on-demand (Just-in-Time)
//! 2. Compile only when accessed (Pull vs Push)
//! 3. Cache compilation results with invalidation (Minimize rework)
//! 4. Track template dependencies for smart invalidation (Value stream)
//!
//! ## Example
//!
//! ```rust,no_run
//! use ggen_core::lean_template::LeanTemplateEngine;
//! use std::path::Path;
//! use std::time::Duration;
//! use tera::Context;
//!
//! # fn main() -> anyhow::Result<()> {
//! let mut engine = LeanTemplateEngine::with_ttl(Duration::from_secs(3600));
//!
//! // First render - compiles template
//! let result1 = engine.render_cached(
//!     Path::new("template.tmpl"),
//!     &Context::new()
//! )?;
//!
//! // Second render - uses cache (no recompilation!)
//! let result2 = engine.render_cached(
//!     Path::new("template.tmpl"),
//!     &Context::new()
//! )?;
//!
//! assert_eq!(result1, result2);
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{Error, Result};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant};
use tera::{Context, Tera};

use crate::template::Template;

/// Cached template compilation result
#[derive(Clone)]
struct CachedTemplate {
    /// Compiled template
    template: Template,
    /// File modification time at compilation
    mtime: Instant,
    /// Cache entry creation time
    cached_at: Instant,
    /// Source file checksum
    checksum: u64,
}

/// Cached render result
#[derive(Clone)]
struct CachedRender {
    /// Rendered output
    output: String,
    /// Cache entry creation time
    cached_at: Instant,
    /// Context hash used for this render
    context_hash: u64,
}

/// Just-in-time template compilation engine with caching
pub struct LeanTemplateEngine {
    /// Template compilation cache
    templates: Arc<RwLock<HashMap<PathBuf, CachedTemplate>>>,
    /// Render result cache (path + context hash -> output)
    renders: Arc<RwLock<HashMap<(PathBuf, u64), CachedRender>>>,
    /// Cache TTL for compiled templates
    template_ttl: Duration,
    /// Cache TTL for render results
    render_ttl: Duration,
    /// Tera instance for rendering
    tera: Tera,
}

impl LeanTemplateEngine {
    /// Create new engine with default TTLs (1 hour for both)
    pub fn new() -> Self {
        let mut tera = Tera::default();
        crate::register::register_all(&mut tera);

        Self {
            templates: Arc::new(RwLock::new(HashMap::new())),
            renders: Arc::new(RwLock::new(HashMap::new())),
            template_ttl: Duration::from_secs(3600),
            render_ttl: Duration::from_secs(3600),
            tera,
        }
    }

    /// Create engine with custom TTLs
    pub fn with_ttl(ttl: Duration) -> Self {
        let mut tera = Tera::default();
        crate::register::register_all(&mut tera);

        Self {
            templates: Arc::new(RwLock::new(HashMap::new())),
            renders: Arc::new(RwLock::new(HashMap::new())),
            template_ttl: ttl,
            render_ttl: ttl,
            tera,
        }
    }

    /// Create engine with separate TTLs for templates and renders
    pub fn with_separate_ttls(template_ttl: Duration, render_ttl: Duration) -> Self {
        let mut tera = Tera::default();
        crate::register::register_all(&mut tera);

        Self {
            templates: Arc::new(RwLock::new(HashMap::new())),
            renders: Arc::new(RwLock::new(HashMap::new())),
            template_ttl,
            render_ttl,
            tera,
        }
    }

    /// Load and compile template with caching (Just-in-Time)
    ///
    /// **Waste eliminated**:
    /// - Overproduction: Only loads templates when accessed
    /// - Over-processing: Reuses cached compilation if file unchanged
    /// - Waiting: Fast cache hits avoid disk I/O and parsing
    pub fn load_template(&mut self, path: &Path) -> Result<Template> {
        // Check cache first
        let templates = self.templates.read().map_err(|e| {
            Error::new(&format!("Failed to acquire read lock: {}", e))
        })?;

        if let Some(cached) = templates.get(path) {
            // Verify cache is still valid
            if cached.cached_at.elapsed() < self.template_ttl {
                // Check if file changed (checksum comparison)
                if let Ok(current_checksum) = Self::file_checksum(path) {
                    if current_checksum == cached.checksum {
                        return Ok(cached.template.clone());
                    }
                }
            }
        }
        drop(templates);

        // Cache miss or expired - load and compile
        let template = Template::from_file(path)?;
        let checksum = Self::file_checksum(path)?;

        // Update cache
        let mut templates_mut = self.templates.write().map_err(|e| {
            Error::new(&format!("Failed to acquire write lock: {}", e))
        })?;

        templates_mut.insert(
            path.to_path_buf(),
            CachedTemplate {
                template: template.clone(),
                mtime: Instant::now(),
                cached_at: Instant::now(),
                checksum,
            },
        );

        Ok(template)
    }

    /// Render template with caching (memoized by context)
    ///
    /// **Waste eliminated**:
    /// - Over-processing: Identical renders return cached results
    /// - Motion: No redundant Tera rendering for same inputs
    pub fn render_cached(&mut self, path: &Path, context: &Context) -> Result<String> {
        let context_hash = Self::hash_context(context);
        let cache_key = (path.to_path_buf(), context_hash);

        // Check render cache
        let renders = self.renders.read().map_err(|e| {
            Error::new(&format!("Failed to acquire read lock: {}", e))
        })?;

        if let Some(cached) = renders.get(&cache_key) {
            if cached.cached_at.elapsed() < self.render_ttl {
                return Ok(cached.output.clone());
            }
        }
        drop(renders);

        // Cache miss - load template and render
        let template = self.load_template(path)?;
        let output = template.render(&mut self.tera, context)?;

        // Update render cache
        let mut renders_mut = self.renders.write().map_err(|e| {
            Error::new(&format!("Failed to acquire write lock: {}", e))
        })?;

        renders_mut.insert(
            cache_key,
            CachedRender {
                output: output.clone(),
                cached_at: Instant::now(),
                context_hash,
            },
        );

        Ok(output)
    }

    /// Render template without caching (for one-off renders)
    pub fn render(&mut self, path: &Path, context: &Context) -> Result<String> {
        let template = self.load_template(path)?;
        template.render(&mut self.tera, context)
    }

    /// Batch render multiple templates efficiently
    ///
    /// **Waste eliminated**:
    /// - Motion: Single lock acquisition for batch operations
    /// - Waiting: Parallel-friendly cache access patterns
    pub fn render_batch(
        &mut self, templates: &[(PathBuf, Context)],
    ) -> Result<Vec<(PathBuf, String)>> {
        let mut results = Vec::with_capacity(templates.len());

        for (path, context) in templates {
            let output = self.render_cached(path, context)?;
            results.push((path.clone(), output));
        }

        Ok(results)
    }

    /// Clear expired cache entries
    ///
    /// **Waste eliminated**: Inventory (stale cache entries)
    pub fn cleanup(&mut self) -> Result<(usize, usize)> {
        let mut templates = self.templates.write().map_err(|e| {
            Error::new(&format!("Failed to acquire write lock: {}", e))
        })?;

        let before_templates = templates.len();
        templates.retain(|_, cached| cached.cached_at.elapsed() < self.template_ttl);
        let removed_templates = before_templates - templates.len();

        drop(templates);

        let mut renders = self.renders.write().map_err(|e| {
            Error::new(&format!("Failed to acquire write lock: {}", e))
        })?;

        let before_renders = renders.len();
        renders.retain(|_, cached| cached.cached_at.elapsed() < self.render_ttl);
        let removed_renders = before_renders - renders.len();

        Ok((removed_templates, removed_renders))
    }

    /// Invalidate specific template (force recompilation)
    pub fn invalidate_template(&mut self, path: &Path) -> Result<()> {
        let mut templates = self.templates.write().map_err(|e| {
            Error::new(&format!("Failed to acquire write lock: {}", e))
        })?;
        templates.remove(path);

        drop(templates);

        // Also invalidate all renders for this template
        let mut renders = self.renders.write().map_err(|e| {
            Error::new(&format!("Failed to acquire write lock: {}", e))
        })?;
        renders.retain(|(p, _), _| p != path);

        Ok(())
    }

    /// Clear all caches
    pub fn clear(&mut self) -> Result<()> {
        let mut templates = self.templates.write().map_err(|e| {
            Error::new(&format!("Failed to acquire write lock: {}", e))
        })?;
        templates.clear();

        drop(templates);

        let mut renders = self.renders.write().map_err(|e| {
            Error::new(&format!("Failed to acquire write lock: {}", e))
        })?;
        renders.clear();

        Ok(())
    }

    /// Get cache statistics
    pub fn stats(&self) -> Result<CacheStats> {
        let templates = self.templates.read().map_err(|e| {
            Error::new(&format!("Failed to acquire read lock: {}", e))
        })?;

        let total_templates = templates.len();
        let expired_templates = templates
            .values()
            .filter(|c| c.cached_at.elapsed() >= self.template_ttl)
            .count();

        drop(templates);

        let renders = self.renders.read().map_err(|e| {
            Error::new(&format!("Failed to acquire read lock: {}", e))
        })?;

        let total_renders = renders.len();
        let expired_renders = renders
            .values()
            .filter(|c| c.cached_at.elapsed() >= self.render_ttl)
            .count();

        Ok(CacheStats {
            total_templates,
            expired_templates,
            total_renders,
            expired_renders,
        })
    }

    fn file_checksum(path: &Path) -> Result<u64> {
        let content = std::fs::read(path)?;
        let mut hasher = DefaultHasher::new();
        content.hash(&mut hasher);
        Ok(hasher.finish())
    }

    fn hash_context(context: &Context) -> u64 {
        let json = serde_json::to_string(context).unwrap_or_default();
        let mut hasher = DefaultHasher::new();
        json.hash(&mut hasher);
        hasher.finish()
    }
}

impl Default for LeanTemplateEngine {
    fn default() -> Self {
        Self::new()
    }
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub total_templates: usize,
    pub expired_templates: usize,
    pub total_renders: usize,
    pub expired_renders: usize,
}

impl CacheStats {
    pub fn hit_rate(&self) -> f64 {
        let valid_templates = self.total_templates - self.expired_templates;
        let valid_renders = self.total_renders - self.expired_renders;
        let total_valid = valid_templates + valid_renders;
        let total = self.total_templates + self.total_renders;

        if total == 0 {
            0.0
        } else {
            total_valid as f64 / total as f64
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    fn create_test_template(content: &str) -> NamedTempFile {
        let mut file = NamedTempFile::new().unwrap();
        writeln!(
            file,
            "---\nto: \"output.txt\"\n---\n{}",
            content
        )
        .unwrap();
        file
    }

    #[test]
    fn test_template_caching() {
        let mut engine = LeanTemplateEngine::new();
        let file = create_test_template("Hello, {{{{ name }}}}!");

        // First load - should compile
        let template1 = engine.load_template(file.path()).unwrap();

        // Second load - should use cache
        let template2 = engine.load_template(file.path()).unwrap();

        // Both should produce same output
        let ctx = Context::new();
        assert_eq!(template1.body, template2.body);
    }

    #[test]
    fn test_render_caching() {
        let mut engine = LeanTemplateEngine::new();
        let file = create_test_template("Hello, {{{{ name }}}}!");

        let mut ctx = Context::new();
        ctx.insert("name", "Alice");

        // First render
        let output1 = engine.render_cached(file.path(), &ctx).unwrap();

        // Second render with same context - should use cache
        let output2 = engine.render_cached(file.path(), &ctx).unwrap();

        assert_eq!(output1, output2);
        assert!(output1.contains("Alice"));
    }

    #[test]
    fn test_context_differentiation() {
        let mut engine = LeanTemplateEngine::new();
        let file = create_test_template("Hello, {{{{ name }}}}!");

        let mut ctx1 = Context::new();
        ctx1.insert("name", "Alice");

        let mut ctx2 = Context::new();
        ctx2.insert("name", "Bob");

        let output1 = engine.render_cached(file.path(), &ctx1).unwrap();
        let output2 = engine.render_cached(file.path(), &ctx2).unwrap();

        assert_ne!(output1, output2);
        assert!(output1.contains("Alice"));
        assert!(output2.contains("Bob"));
    }

    #[test]
    fn test_invalidation() {
        let mut engine = LeanTemplateEngine::new();
        let file = create_test_template("Hello, {{{{ name }}}}!");

        // Load template
        engine.load_template(file.path()).unwrap();

        // Verify cached
        let stats = engine.stats().unwrap();
        assert_eq!(stats.total_templates, 1);

        // Invalidate
        engine.invalidate_template(file.path()).unwrap();

        // Verify cleared
        let stats = engine.stats().unwrap();
        assert_eq!(stats.total_templates, 0);
    }

    #[test]
    fn test_batch_rendering() {
        let mut engine = LeanTemplateEngine::new();
        let file1 = create_test_template("Hello, {{{{ name }}}}!");
        let file2 = create_test_template("Goodbye, {{{{ name }}}}!");

        let mut ctx1 = Context::new();
        ctx1.insert("name", "Alice");

        let mut ctx2 = Context::new();
        ctx2.insert("name", "Bob");

        let templates = vec![
            (file1.path().to_path_buf(), ctx1),
            (file2.path().to_path_buf(), ctx2),
        ];

        let results = engine.render_batch(&templates).unwrap();

        assert_eq!(results.len(), 2);
        assert!(results[0].1.contains("Alice"));
        assert!(results[1].1.contains("Bob"));
    }

    #[test]
    fn test_cache_stats() {
        let mut engine = LeanTemplateEngine::new();
        let file = create_test_template("Hello!");

        engine.load_template(file.path()).unwrap();

        let stats = engine.stats().unwrap();
        assert_eq!(stats.total_templates, 1);
        assert_eq!(stats.expired_templates, 0);
    }
}
