//! Tracing utilities for the ggen pipeline
//!
//! This module provides structured tracing capabilities using the `tracing` crate
//! for observability and debugging. It initializes tracing based on environment
//! variables and provides utilities for creating spans and logging pipeline operations.
//!
//! ## Features
//!
//! - **Environment-based configuration**: Controlled via `GGEN_TRACE` environment variable
//! - **Structured spans**: Create spans for template processing, RDF operations, etc.
//! - **Performance timing**: Built-in timer for measuring operation duration
//! - **Integration**: Works with OpenTelemetry and other tracing backends
//!
//! ## Configuration
//!
//! Set the `GGEN_TRACE` environment variable to control trace levels:
//!
//! - `error` - Only error messages
//! - `warn` - Warnings and errors
//! - `info` - Informational messages (default)
//! - `debug` - Debug information
//! - `trace` - Verbose trace information
//! - `1`, `true`, `yes` - Enable debug level
//! - `0`, `false`, `no` - Disable (error only)
//!
//! ## Examples
//!
//! ### Initializing Tracing
//!
//! ```rust,no_run
//! use ggen_core::tracing::init_tracing;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! init_tracing()?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Creating Spans
//!
//! ```rust,no_run
//! use ggen_core::tracing::PipelineTracer;
//! use std::path::Path;
//!
//! let span = PipelineTracer::template_span(Path::new("template.tmpl"));
//! let _guard = span.enter();
//! // ... template processing code ...
//! ```
//!
//! ### Performance Timing
//!
//! ```rust,no_run
//! use ggen_core::tracing::PerformanceTimer;
//!
//! let timer = PerformanceTimer::start("template_processing");
//! // ... do work ...
//! timer.finish(); // Automatically logs the duration
//! ```
//!
//! ### Using Macros
//!
//! ```rust,no_run
//! use ggen_core::{time_operation, trace_span};
//!
//! let result = time_operation!("expensive_operation", {
//!     // ... operation code ...
//!     42
//! });
//!
//! let span = trace_span!("my_span", operation = "test");
//! ```

use std::path::Path;
use tracing::{debug, error, info, warn};

/// Initialize tracing based on environment variables
///
/// Reads the `GGEN_TRACE` environment variable and configures the tracing
/// subscriber accordingly. If not set, defaults to INFO level.
///
/// # Errors
///
/// Returns an error if tracing subscriber initialization fails (e.g., if
/// already initialized).
pub fn init_tracing() -> ggen_utils::error::Result<()> {
    use tracing_subscriber::EnvFilter;

    let env_filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("ggen=info"));

    tracing_subscriber::fmt()
        .with_env_filter(env_filter)
        .try_init()
        .map_err(|e| ggen_utils::error::Error::with_source("Failed to initialize tracing", e))?;

    Ok(())
}

/// Pipeline tracer for structured logging of template operations
pub struct PipelineTracer;

impl PipelineTracer {
    /// Create a tracing span for template processing
    pub fn template_span(template_path: &Path) -> tracing::Span {
        tracing::info_span!(
            "template_processing",
            template = %template_path.display()
        )
    }

    /// Log template processing start
    pub fn template_start(template_path: &Path) {
        info!(
            template = %template_path.display(),
            "Starting template processing"
        );
    }

    /// Log template parsing completion
    pub fn template_parsing_complete(template_path: &Path, body_lines: usize) {
        info!(
            template = %template_path.display(),
            body_lines = body_lines,
            "Template parsing completed"
        );
    }

    /// Log frontmatter processing
    pub fn frontmatter_processed(frontmatter: &crate::template_types::Frontmatter) {
        debug!(
            to = ?frontmatter.to,
            inject = frontmatter.inject,
            rdf_inline_count = frontmatter.rdf_inline.len(),
            sparql_queries_count = frontmatter.sparql.len(),
            "Frontmatter processed"
        );
    }

    /// Log context blessing
    pub fn context_blessed(vars_count: usize) {
        debug!(
            vars_count = vars_count,
            "Context variables blessed (Name, locals added)"
        );
    }

    /// Log RDF loading start
    pub fn rdf_loading_start(files: &[String], inline_blocks: usize) {
        info!(
            files_count = files.len(),
            inline_blocks = inline_blocks,
            "Loading RDF data"
        );
    }

    /// Log RDF loading completion
    pub fn rdf_loading_complete(triples_count: usize) {
        info!(
            triples_count = triples_count,
            "RDF data loaded successfully"
        );
    }

    /// Log SPARQL query execution
    pub fn sparql_query(query: &str, result_count: Option<usize>) {
        debug!(
            query = query,
            result_count = ?result_count,
            "SPARQL query executed"
        );
    }

    /// Log template rendering start
    pub fn template_rendering_start(output_path: &Path) {
        info!(
            output_path = %output_path.display(),
            "Starting template rendering"
        );
    }

    /// Log template rendering completion
    pub fn template_rendering_complete(output_path: &Path, content_size: usize) {
        info!(
            output_path = %output_path.display(),
            content_size = content_size,
            "Template rendering completed"
        );
    }

    /// Log file injection start
    pub fn file_injection_start(target_path: &Path, mode: &str) {
        info!(
            target_path = %target_path.display(),
            mode = mode,
            "Starting file injection"
        );
    }

    /// Log file injection completion
    pub fn file_injection_complete(target_path: &Path, mode: &str) {
        info!(
            target_path = %target_path.display(),
            mode = mode,
            "File injection completed"
        );
    }

    /// Log shell hook execution start
    pub fn shell_hook_start(command: &str, timing: &str) {
        info!(command = command, timing = timing, "Executing shell hook");
    }

    /// Log shell hook completion
    pub fn shell_hook_complete(command: &str, timing: &str, exit_code: i32) {
        info!(
            command = command,
            timing = timing,
            exit_code = exit_code,
            "Shell hook completed"
        );
    }

    /// Log performance metrics
    pub fn performance_metric(operation: &str, duration_ms: u64) {
        debug!(
            operation = operation,
            duration_ms = duration_ms,
            "Performance metric"
        );
    }

    /// Log error with context
    pub fn error_with_context(error: &ggen_utils::error::Error, context: &str) {
        error!(
            error = %error,
            context = context,
            "Error occurred"
        );
    }

    /// Log warning
    pub fn warning(message: &str, context: Option<&str>) {
        if let Some(ctx) = context {
            warn!(message = message, context = ctx, "Warning");
        } else {
            warn!(message = message, "Warning");
        }
    }

    /// Log dry run
    pub fn dry_run(output_path: &Path, content_size: usize) {
        info!(
            output_path = %output_path.display(),
            content_size = content_size,
            "DRY RUN - File would be generated"
        );
    }

    /// Log backup creation
    pub fn backup_created(original_path: &Path, backup_path: &Path) {
        info!(
            original_path = %original_path.display(),
            backup_path = %backup_path.display(),
            "Backup created"
        );
    }

    /// Log skip conditions
    pub fn skip_condition(condition: &str, reason: &str) {
        info!(condition = condition, reason = reason, "Skipping operation");
    }
}

/// Performance timing utilities
pub struct PerformanceTimer {
    start: std::time::Instant,
    operation: String,
}

impl PerformanceTimer {
    /// Start timing an operation
    pub fn start(operation: &str) -> Self {
        Self {
            start: std::time::Instant::now(),
            operation: operation.to_string(),
        }
    }

    /// Finish timing and log the result
    pub fn finish(self) {
        let duration = self.start.elapsed();
        PipelineTracer::performance_metric(&self.operation, duration.as_millis() as u64);
    }
}

/// Macro for easy performance timing
#[macro_export]
macro_rules! time_operation {
    ($name:expr, $block:block) => {{
        let _timer = $crate::tracing::PerformanceTimer::start($name);
        let result = $block;
        _timer.finish();
        result
    }};
}

/// Macro for tracing spans
#[macro_export]
macro_rules! trace_span {
    ($name:expr, $($key:ident = $value:expr),*) => {
        tracing::span!(tracing::Level::DEBUG, $name, $($key = $value),*)
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tracing_initialization() {
        // Test that tracing can be initialized without errors
        std::env::set_var("GGEN_TRACE", "debug");
        let result = init_tracing();
        // May fail if already initialized, that's OK for this test
        assert!(result.is_ok() || result.unwrap_err().to_string().contains("already"));
    }

    #[test]
    fn test_performance_timer() {
        let timer = PerformanceTimer::start("test_operation");
        std::thread::sleep(std::time::Duration::from_millis(10));
        timer.finish(); // Should not panic
    }

    #[test]
    fn test_tracing_macros() {
        // Test that the macros compile and work
        let _span = trace_span!("test_span", operation = "test");

        let result = time_operation!("test_op", { 42 });
        assert_eq!(result, 42);
    }

    #[test]
    fn test_pipeline_tracer_methods() {
        // Test all public methods compile
        use std::fs;
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let test_path = temp_dir.path().join("test.tmpl");
        fs::write(&test_path, "test content").unwrap();

        PipelineTracer::template_start(&test_path);
        PipelineTracer::template_parsing_complete(&test_path, 100);

        let frontmatter = crate::template_types::Frontmatter::default();
        PipelineTracer::frontmatter_processed(&frontmatter);

        PipelineTracer::context_blessed(5);
        PipelineTracer::rdf_loading_start(&["file1.ttl".to_string()], 2);
        PipelineTracer::rdf_loading_complete(1000);
        PipelineTracer::sparql_query("SELECT * WHERE { ?s ?p ?o }", Some(10));

        PipelineTracer::template_rendering_start(&test_path);
        PipelineTracer::template_rendering_complete(&test_path, 500);

        PipelineTracer::file_injection_start(&test_path, "append");
        PipelineTracer::file_injection_complete(&test_path, "append");

        PipelineTracer::shell_hook_start("echo 'test'", "before");
        PipelineTracer::shell_hook_complete("echo 'test'", "before", 0);

        PipelineTracer::performance_metric("test_op", 50);

        let error = ggen_utils::error::Error::new("Test error");
        PipelineTracer::error_with_context(&error, "test context");

        PipelineTracer::warning("Test warning", Some("test context"));
        PipelineTracer::warning("Test warning", None);

        PipelineTracer::dry_run(&test_path, 500);
        PipelineTracer::backup_created(&test_path, &temp_dir.path().join("backup.tmpl"));
        PipelineTracer::skip_condition("skip_if", "pattern found");
    }

    #[test]
    fn test_template_span() {
        use std::fs;
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let test_path = temp_dir.path().join("test.tmpl");
        fs::write(&test_path, "test content").unwrap();

        let span = PipelineTracer::template_span(&test_path);
        let _guard = span.enter();
        // Span is active here
    }
}
