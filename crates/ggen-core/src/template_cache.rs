//! Template caching system for performance optimization
//!
//! Provides LRU caching for parsed templates and compiled RDF graphs
//! to avoid redundant parsing and improve generation performance.

use ggen_utils::error::{Error, Result};
use lru::LruCache;
use std::num::NonZeroUsize;
use std::path::Path;
use std::sync::{Arc, Mutex};

use crate::template::Template;

/// Template cache with LRU eviction policy
///
/// Provides thread-safe LRU (Least Recently Used) caching for parsed templates
/// to improve generation performance by avoiding redundant parsing.
///
/// # Features
///
/// - **Thread-safe**: Uses `Arc<Mutex<...>>` for concurrent access
/// - **LRU eviction**: Automatically evicts least recently used templates when capacity is reached
/// - **Automatic parsing**: Parses templates on cache miss
/// - **Statistics**: Provides cache size and capacity information
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::template_cache::TemplateCache;
/// use std::path::Path;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// // Create cache with capacity of 50 templates
/// let cache = TemplateCache::new(50);
///
/// // Get template (parses if not cached)
/// let template = cache.get_or_parse(Path::new("template.tmpl"))?;
///
/// // Check cache statistics
/// let stats = cache.stats()?;
/// println!("Cache: {}/{} templates", stats.size, stats.capacity);
/// # Ok(())
/// # }
/// ```
pub struct TemplateCache {
    cache: Arc<Mutex<LruCache<String, Arc<Template>>>>,
}

impl TemplateCache {
    /// Create a new template cache with specified capacity
    ///
    /// Creates a cache that can hold up to `capacity` templates. When the cache
    /// is full, the least recently used template is evicted to make room.
    ///
    /// # Arguments
    ///
    /// * `capacity` - Maximum number of templates to cache (minimum 1)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::template_cache::TemplateCache;
    ///
    /// # fn main() {
    /// // Create cache with capacity of 100 templates
    /// let cache = TemplateCache::new(100);
    ///
    /// // Or use default (100 templates)
    /// let default_cache = TemplateCache::default();
    /// # }
    /// ```
    pub fn new(capacity: usize) -> Self {
        let cap = NonZeroUsize::new(capacity).unwrap_or(NonZeroUsize::new(100).unwrap());
        Self {
            cache: Arc::new(Mutex::new(LruCache::new(cap))),
        }
    }

    /// Get template from cache or parse if not present
    ///
    /// Returns a cached template if available, otherwise parses the template
    /// from the file system and caches it for subsequent use.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the template file
    ///
    /// # Returns
    ///
    /// An `Arc<Template>` containing the parsed template. The same `Arc` is
    /// returned for subsequent calls with the same path (until eviction).
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::template_cache::TemplateCache;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let cache = TemplateCache::new(10);
    ///
    /// // First call - parses and caches
    /// let template1 = cache.get_or_parse(Path::new("template.tmpl"))?;
    ///
    /// // Second call - returns cached version
    /// let template2 = cache.get_or_parse(Path::new("template.tmpl"))?;
    ///
    /// // Same Arc (shared reference)
    /// assert!(std::sync::Arc::ptr_eq(&template1, &template2));
    /// # Ok(())
    /// # }
    /// ```
    pub fn get_or_parse(&self, path: &Path) -> Result<Arc<Template>> {
        let path_str = path.to_string_lossy().to_string();

        let mut cache = self
            .cache
            .lock()
            .map_err(|_| Error::new("Cache lock poisoned"))?;

        if let Some(template) = cache.get(&path_str) {
            return Ok(Arc::clone(template));
        }

        // Parse template
        let content = std::fs::read_to_string(path).map_err(|e| {
            Error::with_source(
                &format!("Failed to read template {}", path.display()),
                Box::new(e),
            )
        })?;
        let template = Template::parse(&content)?;
        let arc_template = Arc::new(template);

        cache.put(path_str, Arc::clone(&arc_template));

        Ok(arc_template)
    }

    /// Clear all cached templates
    ///
    /// Removes all templates from the cache, freeing memory. The cache capacity
    /// remains unchanged.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::template_cache::TemplateCache;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let cache = TemplateCache::new(10);
    /// cache.get_or_parse(Path::new("template.tmpl"))?;
    ///
    /// assert_eq!(cache.stats()?.size, 1);
    ///
    /// cache.clear()?;
    /// assert_eq!(cache.stats()?.size, 0);
    /// # Ok(())
    /// # }
    /// ```
    pub fn clear(&self) -> Result<()> {
        let mut cache = self
            .cache
            .lock()
            .map_err(|_| Error::new("Cache lock poisoned"))?;
        cache.clear();
        Ok(())
    }

    /// Get cache statistics
    ///
    /// Returns information about the current cache state, including size
    /// (number of cached templates) and capacity (maximum cache size).
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::template_cache::TemplateCache;
    /// use std::path::Path;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let cache = TemplateCache::new(50);
    ///
    /// let stats = cache.stats()?;
    /// println!("Cache: {}/{} templates", stats.size, stats.capacity);
    /// # Ok(())
    /// # }
    /// ```
    pub fn stats(&self) -> Result<CacheStats> {
        let cache = self
            .cache
            .lock()
            .map_err(|_| Error::new("Cache lock poisoned"))?;

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
///
/// Provides information about the current state of a template cache.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::template_cache::{TemplateCache, CacheStats};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let cache = TemplateCache::new(100);
/// let stats = cache.stats()?;
///
/// println!("Cache size: {}/{}", stats.size, stats.capacity);
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone)]
pub struct CacheStats {
    pub size: usize,
    pub capacity: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::{async_test, test};
    use std::io::Write;
    use tempfile::NamedTempFile;

    test!(test_template_cache_new, {
        let cache = TemplateCache::new(50);
        let stats = cache.stats().unwrap();
        assert_eq!(stats.capacity, 50);
        assert_eq!(stats.size, 0);
    });

    test!(test_template_cache_default, {
        let cache = TemplateCache::default();
        let stats = cache.stats().unwrap();
        assert_eq!(stats.capacity, 100);
        assert_eq!(stats.size, 0);
    });

    test!(test_get_or_parse, {
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
    });

    test!(test_cache_clear, {
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
    });

    test!(test_cache_eviction, {
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
    });
}
