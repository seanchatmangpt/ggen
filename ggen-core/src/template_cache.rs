//! Template caching system for performance optimization
//!
//! Provides LRU caching for parsed templates and compiled RDF graphs
//! to avoid redundant parsing and improve generation performance.

use anyhow::Result;
use lru::LruCache;
use std::num::NonZeroUsize;
use std::path::Path;
use std::sync::{Arc, Mutex};

use crate::template::Template;

/// Template cache with LRU eviction policy
pub struct TemplateCache {
    cache: Arc<Mutex<LruCache<String, Arc<Template>>>>,
}

impl TemplateCache {
    /// Create a new template cache with specified capacity
    pub fn new(capacity: usize) -> Self {
        let cap = NonZeroUsize::new(capacity).unwrap_or(NonZeroUsize::new(100).unwrap());
        Self {
            cache: Arc::new(Mutex::new(LruCache::new(cap))),
        }
    }

    /// Get template from cache or parse if not present
    pub fn get_or_parse(&self, path: &Path) -> Result<Arc<Template>> {
        let path_str = path.to_string_lossy().to_string();

        let mut cache = self
            .cache
            .lock()
            .map_err(|e| anyhow::anyhow!("Cache lock poisoned: {}", e))?;

        if let Some(template) = cache.get(&path_str) {
            return Ok(Arc::clone(template));
        }

        // Parse template
        let content = std::fs::read_to_string(path)
            .map_err(|e| anyhow::anyhow!("Failed to read template {}: {}", path.display(), e))?;
        let template = Template::parse(&content)?;
        let arc_template = Arc::new(template);

        cache.put(path_str, Arc::clone(&arc_template));

        Ok(arc_template)
    }

    /// Clear all cached templates
    pub fn clear(&self) -> Result<()> {
        let mut cache = self
            .cache
            .lock()
            .map_err(|e| anyhow::anyhow!("Cache lock poisoned: {}", e))?;
        cache.clear();
        Ok(())
    }

    /// Get cache statistics
    pub fn stats(&self) -> Result<CacheStats> {
        let cache = self
            .cache
            .lock()
            .map_err(|e| anyhow::anyhow!("Cache lock poisoned: {}", e))?;

        Ok(CacheStats {
            size: cache.len(),
            capacity: cache.cap().get(),
        })
    }
}

impl Default for TemplateCache {
    fn default() -> Self {
        Self::new(100)
    }
}

/// Cache statistics
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub size: usize,
    pub capacity: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_template_cache_new() {
        let cache = TemplateCache::new(50);
        let stats = cache.stats().unwrap();
        assert_eq!(stats.capacity, 50);
        assert_eq!(stats.size, 0);
    }

    #[test]
    fn test_template_cache_default() {
        let cache = TemplateCache::default();
        let stats = cache.stats().unwrap();
        assert_eq!(stats.capacity, 100);
        assert_eq!(stats.size, 0);
    }

    #[test]
    fn test_get_or_parse() -> Result<()> {
        let cache = TemplateCache::new(10);

        let mut temp = NamedTempFile::new()?;
        writeln!(
            temp,
            r#"---
to: "output.rs"
---
fn main() {{}}"#
        )?;

        // First access - should parse
        let template1 = cache.get_or_parse(temp.path())?;
        assert_eq!(cache.stats()?.size, 1);

        // Second access - should hit cache
        let template2 = cache.get_or_parse(temp.path())?;
        assert_eq!(cache.stats()?.size, 1);

        // Should be the same Arc
        assert!(Arc::ptr_eq(&template1, &template2));

        Ok(())
    }

    #[test]
    fn test_cache_clear() -> Result<()> {
        let cache = TemplateCache::new(10);

        let mut temp = NamedTempFile::new()?;
        writeln!(
            temp,
            r#"---
to: "output.rs"
---
fn main() {{}}"#
        )?;

        cache.get_or_parse(temp.path())?;
        assert_eq!(cache.stats()?.size, 1);

        cache.clear()?;
        assert_eq!(cache.stats()?.size, 0);

        Ok(())
    }

    #[test]
    fn test_cache_eviction() -> Result<()> {
        let cache = TemplateCache::new(2);

        // Create 3 temp files
        let mut temp1 = NamedTempFile::new()?;
        let mut temp2 = NamedTempFile::new()?;
        let mut temp3 = NamedTempFile::new()?;

        writeln!(temp1, "---\nto: '1.rs'\n---\nfile1")?;
        writeln!(temp2, "---\nto: '2.rs'\n---\nfile2")?;
        writeln!(temp3, "---\nto: '3.rs'\n---\nfile3")?;

        // Fill cache
        cache.get_or_parse(temp1.path())?;
        cache.get_or_parse(temp2.path())?;
        assert_eq!(cache.stats()?.size, 2);

        // This should evict temp1 (LRU)
        cache.get_or_parse(temp3.path())?;
        assert_eq!(cache.stats()?.size, 2);

        Ok(())
    }
}
