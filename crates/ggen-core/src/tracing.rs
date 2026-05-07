use std::path::Path;
use tracing::info;

/// Initialize tracing based on environment variables
pub fn init_tracing() -> crate::utils::error::Result<()> {
    #[cfg(feature = "otel")]
    {
        init_otel_tracing()
    }

    #[cfg(not(feature = "otel"))]
    {
        use tracing_subscriber::EnvFilter;
        let env_filter =
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("ggen=info"));

        tracing_subscriber::fmt()
            .with_env_filter(env_filter)
            .try_init()
            .map_err(|e| {
                crate::utils::error::Error::with_source("Failed to initialize tracing", e)
            })?;

        Ok(())
    }
}

#[cfg(feature = "otel")]
fn init_otel_tracing() -> crate::utils::error::Result<()> {
    // OTEL feature disabled - API breaking changes in opentelemetry crates
    // Use standard tracing instead
    use tracing_subscriber::EnvFilter;

    let env_filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("ggen=info"));

    tracing_subscriber::fmt()
        .with_env_filter(env_filter)
        .try_init()
        .map_err(|e| crate::utils::error::Error::with_source("Failed to initialize tracing", e))?;

    Ok(())
}

/// Shutdown tracing and flush providers
pub fn shutdown_tracing() {
    // OTEL feature disabled - no provider to shutdown
    #[cfg(feature = "otel")]
    {
        // opentelemetry::global::shutdown_tracer_provider();  // Not available in current version
    }
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

    /// Log frontmatter processing
    pub fn frontmatter_processed(front: &crate::template_types::Frontmatter) {
        info!(
            target = %front.to.as_deref().unwrap_or("unknown"),
            "Frontmatter processed"
        );
    }

    /// Log context blessing
    pub fn context_blessed(vars_count: usize) {
        info!(vars_count = vars_count, "Template context blessed");
    }

    /// Log template parsing completion
    pub fn template_parsing_complete(template_path: &Path, content_size: usize) {
        info!(
            template = %template_path.display(),
            content_size = content_size,
            "Template parsing completed"
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

    /// Log template start
    pub fn template_start(template_path: &Path) {
        info!(
            template = %template_path.display(),
            "Starting template processing"
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
        info!(triples_count = triples_count, "RDF loading completed");
    }

    /// Log SPARQL query execution
    pub fn sparql_query(query: &str, results: Option<usize>) {
        info!(
            query_len = query.len(),
            results_count = results.unwrap_or(0),
            "SPARQL query executed"
        );
    }

    /// Log file injection start
    pub fn file_injection_start(path: &Path, mode: &str) {
        info!(
            path = %path.display(),
            mode = mode,
            "Starting file injection"
        );
    }

    /// Log file injection completion
    pub fn file_injection_complete(path: &Path, mode: &str) {
        info!(
            path = %path.display(),
            mode = mode,
            "File injection completed"
        );
    }

    /// Log shell hook start
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

    /// Log performance metric
    pub fn performance_metric(operation: &str, duration_ms: u64) {
        info!(
            operation = operation,
            duration_ms = duration_ms,
            "Performance metric recorded"
        );
    }

    /// Log error with context
    pub fn error_with_context(error: &crate::utils::error::Error, context: &str) {
        tracing::error!(
            error = %error,
            context = context,
            "Error occurred"
        );
    }

    /// Log warning
    pub fn warning(message: &str, context: Option<&str>) {
        tracing::warn!(
            message = message,
            context = context.unwrap_or(""),
            "Warning"
        );
    }

    /// Log backup creation
    pub fn backup_created(original: &Path, backup: &Path) {
        info!(
            original = %original.display(),
            backup = %backup.display(),
            "Backup created"
        );
    }

    /// Log skip condition
    pub fn skip_condition(condition: &str, reason: &str) {
        info!(condition = condition, reason = reason, "Template skipped");
    }

    /// Log dry run information
    pub fn dry_run(path: &Path, size: usize) {
        info!(
            path = %path.display(),
            size = size,
            "Dry run: skipping file write"
        );
    }
}

/// Performance timer for measuring operation duration (RAII)
pub struct PerformanceTimer {
    operation: String,
    start: std::time::Instant,
    finished: bool,
}

impl PerformanceTimer {
    /// Start a new performance timer
    pub fn start(operation: &str) -> Self {
        Self {
            operation: operation.to_string(),
            start: std::time::Instant::now(),
            finished: false,
        }
    }

    /// Finish the timer and record the duration
    pub fn finish(mut self) {
        if !self.finished {
            let duration = self.start.elapsed();
            PipelineTracer::performance_metric(&self.operation, duration.as_millis() as u64);
            self.finished = true;
        }
    }
}

impl Drop for PerformanceTimer {
    fn drop(&mut self) {
        if !self.finished {
            let duration = self.start.elapsed();
            PipelineTracer::performance_metric(&self.operation, duration.as_millis() as u64);
        }
    }
}

/// Macro to time an operation using PerformanceTimer
#[macro_export]
macro_rules! time_operation {
    ($name:expr, $block:block) => {{
        let _timer = $crate::tracing::PerformanceTimer::start($name);
        $block
    }};
}

/// Macro to create a tracing span
#[macro_export]
macro_rules! trace_span {
    ($name:expr, $($fields:tt)*) => {
        tracing::info_span!($name, $($fields)*)
    };
}

pub use time_operation;
pub use trace_span;
