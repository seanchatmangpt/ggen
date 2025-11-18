//! Streaming file generator for memory-efficient template processing
//!
//! Processes templates one at a time instead of loading entire file tree
//! into memory, enabling constant memory usage regardless of project size.

use ggen_utils::error::{Error, Result};
use std::path::{Path, PathBuf};
use tera::Context;
use walkdir::WalkDir;

use crate::pipeline::Pipeline;
use crate::template_cache::TemplateCache;

/// Streaming generator that processes templates one at a time
///
/// Provides memory-efficient template processing by processing templates
/// sequentially rather than loading the entire file tree into memory.
/// This enables constant memory usage regardless of project size.
///
/// # Features
///
/// - **Memory efficient**: Processes one template at a time
/// - **LRU caching**: Caches parsed templates for reuse
/// - **Streaming processing**: Walks directory tree and processes templates on-the-fly
/// - **Error resilience**: Continues processing even if individual templates fail
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::streaming_generator::StreamingGenerator;
/// use tera::Context;
/// use std::path::PathBuf;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let generator = StreamingGenerator::new(
///     PathBuf::from("templates"),
///     PathBuf::from("output")
/// )?;
///
/// let mut vars = Context::new();
/// vars.insert("name", "MyApp");
///
/// let result = generator.generate_all(&vars)?;
/// println!("Generated {} files", result.success_count);
/// # Ok(())
/// # }
/// ```
pub struct StreamingGenerator {
    template_dir: PathBuf,
    output_dir: PathBuf,
    cache: TemplateCache,
    pipeline: Pipeline,
}

impl StreamingGenerator {
    /// Create a new streaming generator
    ///
    /// Creates a generator with default cache capacity (100 templates).
    ///
    /// # Arguments
    ///
    /// * `template_dir` - Directory containing template files (`.tmpl`)
    /// * `output_dir` - Directory where generated files will be written
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::streaming_generator::StreamingGenerator;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let generator = StreamingGenerator::new(
    ///     PathBuf::from("templates"),
    ///     PathBuf::from("output")
    /// )?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn new(template_dir: PathBuf, output_dir: PathBuf) -> Result<Self> {
        Ok(Self {
            template_dir,
            output_dir,
            cache: TemplateCache::default(),
            pipeline: Pipeline::new()?,
        })
    }

    /// Create with custom cache capacity
    ///
    /// Creates a generator with a specified cache capacity for parsed templates.
    ///
    /// # Arguments
    ///
    /// * `template_dir` - Directory containing template files
    /// * `output_dir` - Directory where generated files will be written
    /// * `cache_capacity` - Maximum number of templates to cache
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::streaming_generator::StreamingGenerator;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// // Create generator with larger cache for many templates
    /// let generator = StreamingGenerator::with_cache_capacity(
    ///     PathBuf::from("templates"),
    ///     PathBuf::from("output"),
    ///     500
    /// )?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn with_cache_capacity(
        template_dir: PathBuf, output_dir: PathBuf, cache_capacity: usize,
    ) -> Result<Self> {
        Ok(Self {
            template_dir,
            output_dir,
            cache: TemplateCache::new(cache_capacity),
            pipeline: Pipeline::new()?,
        })
    }

    /// Generate all template files in streaming fashion
    ///
    /// Walks the template directory and processes each `.tmpl` file sequentially,
    /// generating output files. Templates are processed one at a time to minimize
    /// memory usage.
    ///
    /// # Arguments
    ///
    /// * `vars` - Tera context containing template variables
    ///
    /// # Returns
    ///
    /// A `GenerationResult` containing statistics about the generation run,
    /// including success count, error count, and generated file paths.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::streaming_generator::StreamingGenerator;
    /// use tera::Context;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let mut generator = StreamingGenerator::new(
    ///     PathBuf::from("templates"),
    ///     PathBuf::from("output")
    /// )?;
    ///
    /// let mut vars = Context::new();
    /// vars.insert("project_name", "MyApp");
    ///
    /// let result = generator.generate_all(&vars)?;
    /// println!("Generated {} files successfully", result.success_count);
    /// println!("Errors: {}", result.error_count);
    /// println!("Throughput: {:.2} files/sec", result.throughput());
    /// # Ok(())
    /// # }
    /// ```
    pub fn generate_all(&mut self, vars: &Context) -> Result<GenerationResult> {
        let mut result = GenerationResult::default();
        let start = std::time::Instant::now();

        for entry in WalkDir::new(&self.template_dir) {
            let entry =
                entry.map_err(|e| Error::new(&format!("Failed to read directory entry: {}", e)))?;

            if !entry.file_type().is_file() {
                continue;
            }

            let path = entry.path();
            if path.extension() != Some(std::ffi::OsStr::new("tmpl")) {
                continue;
            }

            match self.process_template(path, vars) {
                Ok(output_path) => {
                    result.success_count += 1;
                    result.generated_files.push(output_path);
                }
                Err(e) => {
                    result.error_count += 1;
                    result
                        .errors
                        .push(format!("Failed to process {}: {}", path.display(), e));
                }
            }

            // Template is dropped here, freeing memory immediately
        }

        result.duration = start.elapsed();
        Ok(result)
    }

    /// Process a single template file
    fn process_template(&mut self, template_path: &Path, vars: &Context) -> Result<PathBuf> {
        // Get from cache or parse
        let template = self.cache.get_or_parse(template_path)?;

        // Clone template for mutation (Arc<Template> -> Template)
        let mut template = (*template).clone();

        // Render frontmatter
        template.render_frontmatter(&mut self.pipeline.tera, vars)?;

        // Process RDF graph if needed (inline RDF and SPARQL only)
        // ❌ REMOVED: template.front.rdf check - RDF files now via CLI/API
        if !template.front.rdf_inline.is_empty() || !template.front.sparql.is_empty() {
            template.process_graph(
                &mut self.pipeline.graph,
                &mut self.pipeline.tera,
                vars,
                template_path,
            )?;
        }

        // Render template
        let rendered = template.render(&mut self.pipeline.tera, vars)?;

        // Determine output path
        let output_path = if let Some(to_path) = &template.front.to {
            let rendered_to = self.pipeline.tera.render_str(to_path, vars)?;
            self.output_dir.join(rendered_to)
        } else {
            let filename = template_path
                .file_stem()
                .ok_or_else(|| Error::new("Template has no filename"))?
                .to_string_lossy();
            self.output_dir.join(format!("{}.out", filename))
        };

        // Ensure parent directory exists
        if let Some(parent) = output_path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                Error::new(&format!(
                    "Failed to create output directory {}: {}",
                    parent.display(),
                    e
                ))
            })?;
        }

        // Write output file
        std::fs::write(&output_path, rendered).map_err(|e| {
            Error::new(&format!(
                "Failed to write output file {}: {}",
                output_path.display(),
                e
            ))
        })?;

        Ok(output_path)
    }

    /// Get cache statistics
    ///
    /// Returns information about the template cache, including current size
    /// and capacity.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::streaming_generator::StreamingGenerator;
    /// use std::path::PathBuf;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let generator = StreamingGenerator::new(
    ///     PathBuf::from("templates"),
    ///     PathBuf::from("output")
    /// )?;
    ///
    /// let stats = generator.cache_stats()?;
    /// println!("Cache: {}/{} templates", stats.size, stats.capacity);
    /// # Ok(())
    /// # }
    /// ```
    pub fn cache_stats(&self) -> Result<crate::template_cache::CacheStats> {
        self.cache.stats()
    }
}

/// Result of streaming generation
///
/// Contains statistics and results from a streaming generation run,
/// including success/error counts, generated file paths, and performance metrics.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::streaming_generator::{StreamingGenerator, GenerationResult};
/// use tera::Context;
/// use std::path::PathBuf;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let mut generator = StreamingGenerator::new(
///     PathBuf::from("templates"),
///     PathBuf::from("output")
/// )?;
///
/// let result = generator.generate_all(&Context::new())?;
/// println!("Success rate: {:.1}%", result.success_rate());
/// println!("Throughput: {:.2} files/sec", result.throughput());
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Default)]
pub struct GenerationResult {
    pub success_count: usize,
    pub error_count: usize,
    pub generated_files: Vec<PathBuf>,
    pub errors: Vec<String>,
    pub duration: std::time::Duration,
}

impl GenerationResult {
    /// Total number of templates processed
    ///
    /// Returns the sum of successful and failed template generations.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::streaming_generator::GenerationResult;
    ///
    /// # fn main() {
    /// let mut result = GenerationResult::default();
    /// result.success_count = 8;
    /// result.error_count = 2;
    ///
    /// assert_eq!(result.total_count(), 10);
    /// # }
    /// ```
    pub fn total_count(&self) -> usize {
        self.success_count + self.error_count
    }

    /// Success rate as percentage
    ///
    /// Calculates the percentage of templates that were successfully generated.
    /// Returns 0.0 if no templates were processed.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::streaming_generator::GenerationResult;
    ///
    /// # fn main() {
    /// let mut result = GenerationResult::default();
    /// result.success_count = 9;
    /// result.error_count = 1;
    ///
    /// assert_eq!(result.success_rate(), 90.0);
    /// # }
    /// ```
    pub fn success_rate(&self) -> f64 {
        if self.total_count() == 0 {
            return 0.0;
        }
        (self.success_count as f64 / self.total_count() as f64) * 100.0
    }

    /// Generation throughput (files per second)
    ///
    /// Calculates the number of files generated per second based on the
    /// generation duration. Returns 0.0 if duration is zero.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::streaming_generator::GenerationResult;
    /// use std::time::Duration;
    ///
    /// # fn main() {
    /// let mut result = GenerationResult::default();
    /// result.success_count = 10;
    /// result.duration = Duration::from_secs(2);
    ///
    /// assert_eq!(result.throughput(), 5.0); // 10 files in 2 seconds
    /// # }
    /// ```
    pub fn throughput(&self) -> f64 {
        if self.duration.as_secs_f64() == 0.0 {
            return 0.0;
        }
        self.success_count as f64 / self.duration.as_secs_f64()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    fn create_test_template(dir: &Path, name: &str, content: &str) -> Result<PathBuf> {
        let path = dir.join(format!("{}.tmpl", name));
        std::fs::write(&path, content)?;
        Ok(path)
    }

    #[test]
    fn test_streaming_generator_new() {
        let temp_dir = TempDir::new()?;
        let output_dir = TempDir::new()?;

        let generator = StreamingGenerator::new(
            temp_dir.path().to_path_buf(),
            output_dir.path().to_path_buf(),
        )?;

        assert_eq!(generator.template_dir, temp_dir.path());
        assert_eq!(generator.output_dir, output_dir.path());

        Ok(())
    }

    #[test]
    fn test_streaming_generator_single_file() {
        let temp_dir = TempDir::new()?;
        let output_dir = TempDir::new()?;

        create_test_template(
            temp_dir.path(),
            "test",
            r#"---
to: "output.rs"
---
fn main() { println!("Hello, {{ name }}!"); }
"#,
        )?;

        let mut generator = StreamingGenerator::new(
            temp_dir.path().to_path_buf(),
            output_dir.path().to_path_buf(),
        )?;

        let mut vars = Context::new();
        vars.insert("name", "World");

        let result = generator.generate_all(&vars)?;

        assert_eq!(result.success_count, 1);
        assert_eq!(result.error_count, 0);
        assert_eq!(result.generated_files.len(), 1);

        let output_path = output_dir.path().join("output.rs");
        assert!(output_path.exists());

        let content = std::fs::read_to_string(&output_path)?;
        assert!(content.contains("Hello, World!"));

        Ok(())
    }

    #[test]
    fn test_streaming_generator_multiple_files() {
        let temp_dir = TempDir::new()?;
        let output_dir = TempDir::new()?;

        // Create multiple templates
        for i in 0..5 {
            create_test_template(
                temp_dir.path(),
                &format!("test_{}", i),
                &format!(
                    r#"---
to: "output_{i}.rs"
---
fn main_{i}() {{ println!("File {i}"); }}
"#,
                ),
            )?;
        }

        let mut generator = StreamingGenerator::new(
            temp_dir.path().to_path_buf(),
            output_dir.path().to_path_buf(),
        )?;

        let vars = Context::new();
        let result = generator.generate_all(&vars)?;

        assert_eq!(result.success_count, 5);
        assert_eq!(result.error_count, 0);
        assert!(result.success_rate() > 99.0);
        assert!(result.throughput() > 0.0);

        Ok(())
    }

    #[test]
    fn test_streaming_generator_nested_output() {
        let temp_dir = TempDir::new()?;
        let output_dir = TempDir::new()?;

        create_test_template(
            temp_dir.path(),
            "nested",
            r#"---
to: "src/handlers/{{ module }}.rs"
---
// Handler module
"#,
        )?;

        let mut generator = StreamingGenerator::new(
            temp_dir.path().to_path_buf(),
            output_dir.path().to_path_buf(),
        )?;

        let mut vars = Context::new();
        vars.insert("module", "users");

        let result = generator.generate_all(&vars)?;

        assert_eq!(result.success_count, 1);

        let output_path = output_dir.path().join("src/handlers/users.rs");
        assert!(output_path.exists());

        Ok(())
    }

    #[test]
    fn test_streaming_generator_cache_reuse() {
        let temp_dir = TempDir::new()?;
        let output_dir = TempDir::new()?;

        create_test_template(
            temp_dir.path(),
            "cached",
            r#"---
to: "output.rs"
---
fn main() {}
"#,
        )?;

        let mut generator = StreamingGenerator::new(
            temp_dir.path().to_path_buf(),
            output_dir.path().to_path_buf(),
        )?;

        let vars = Context::new();

        // First generation
        generator.generate_all(&vars)?;
        let stats1 = generator.cache_stats()?;
        assert_eq!(stats1.size, 1);

        // Second generation should hit cache
        generator.generate_all(&vars)?;
        let stats2 = generator.cache_stats()?;
        assert_eq!(stats2.size, 1); // Still 1, reused from cache

        Ok(())
    }
}
