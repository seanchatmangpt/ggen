use std::path::Path;
use tracing::{debug, info};

/// Initialize tracing based on environment variables
pub fn init_tracing() -> ggen_utils::error::Result<()> {
    #[cfg(feature = "otel")]
    {
        return init_otel_tracing();
    }

    #[cfg(not(feature = "otel"))]
    {
        use tracing_subscriber::EnvFilter;
        let env_filter =
            EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("ggen=info"));

        tracing_subscriber::fmt()
            .with_env_filter(env_filter)
            .try_init()
            .map_err(|e| ggen_utils::error::Error::with_source("Failed to initialize tracing", e))?;

        Ok(())
    }
}

#[cfg(feature = "otel")]
fn init_otel_tracing() -> ggen_utils::error::Result<()> {
    use opentelemetry::{global, KeyValue};
    use opentelemetry_sdk::{
        propagation::TraceContextPropagator,
        runtime,
        trace::{self, Sampler},
        Resource,
    };
    use opentelemetry_otlp::WithExportConfig;
    use tracing_opentelemetry::OpenTelemetryLayer;
    use tracing_subscriber::layer::SubscriberExt;
    use tracing_subscriber::util::SubscriberInitExt;
    use tracing_subscriber::EnvFilter;

    global::set_text_map_propagator(TraceContextPropagator::new());

    let otlp_exporter = opentelemetry_otlp::new_exporter()
        .tonic()
        .with_endpoint("http://localhost:4317");

    let tracer = opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(otlp_exporter)
        .with_trace_config(
            trace::config()
                .with_sampler(Sampler::AlwaysOn)
                .with_resource(Resource::new(vec![KeyValue::new(
                    "service.name",
                    "ggen",
                )])),
        )
        .install_batch(runtime::Tokio)
        .map_err(|e| ggen_utils::error::Error::with_source("Failed to initialize OTel pipeline", e))?;

    let otel_layer = OpenTelemetryLayer::new(tracer);
    let env_filter =
        EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("ggen=info"));

    tracing_subscriber::registry()
        .with(env_filter)
        .with(otel_layer)
        .with(tracing_subscriber::fmt::layer())
        .try_init()
        .map_err(|e| ggen_utils::error::Error::with_source("Failed to initialize tracing registry", e))?;

    Ok(())
}

/// Shutdown the global tracer provider to flush spans
pub fn shutdown_tracing() {
    #[cfg(feature = "otel")]
    opentelemetry::global::shutdown_tracer_provider();
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

    /// Log template rendering completion
    pub fn template_rendering_complete(output_path: &Path, content_size: usize) {
        info!(
            output_path = %output_path.display(),
            content_size = content_size,
            "Template rendering completed"
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

    /// Log performance metrics
    pub fn performance_metric(operation: &str, duration_ms: u64) {
        debug!(
            operation = operation,
            duration_ms = duration_ms,
            "Performance metric"
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

    /// Log context blessing
    pub fn context_blessed(vars_count: usize) {
        debug!(
            vars_count = vars_count,
            "Context variables blessed (Name, locals added)"
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
    
    pub fn dry_run(output_path: &Path, content_size: usize) {
        info!(
            output_path = %output_path.display(),
            content_size = content_size,
            "DRY RUN - File would be generated"
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