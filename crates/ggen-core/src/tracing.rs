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
    pub fn frontmatter_processed(frontmatter: &crate::template::Frontmatter) {
        debug!(
            to = ?frontmatter.to,
            inject = frontmatter.inject,
            // ❌ REMOVED: vars_count - no longer in frontmatter
            // ❌ REMOVED: rdf_files_count - RDF files now via CLI/API
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
    
    /// Log RDF loading
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
    
    /// Log template rendering
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
    
    /// Log file injection
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
    
    /// Log shell hook execution
    pub fn shell_hook_start(command: &str, timing: &str) {
        info!(
            command = command,
            timing = timing,
            "Executing shell hook"
        );
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
            warn!(
                message = message,
                context = ctx,
                "Warning"
            );
        } else {
            warn!(
                message = message,
                "Warning"
            );
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
        info!(
            condition = condition,
            reason = reason,
            "Skipping operation"
        );
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
    use tempfile::TempDir;
    use std::fs;
    use chicago_tdd_tools::{test, async_test};
    
    test!(test_tracing_initialization, {
        // Test that tracing can be initialized without errors
        std::env::set_var("GGEN_TRACE", "debug");
        let result = init_tracing();
        assert!(result.is_ok());
    });
    
    test!(test_performance_timer, {
        let timer = PerformanceTimer::start("test_operation");
        std::thread::sleep(std::time::Duration::from_millis(10));
        timer.finish(); // Should not panic
    });
    
    test!(test_tracing_macros, {
        // Test that the macros compile and work
        let _span = trace_span!("test_span", operation = "test");
        
        let result = time_operation!("test_op", {
            42
        });
        assert_eq!(result, 42);
    });
}
