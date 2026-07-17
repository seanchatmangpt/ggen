#![allow(warnings)]
#[cfg(feature = "weaver")]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    use opentelemetry::trace::{Span, Tracer, TracerProvider as _};
    use opentelemetry::KeyValue;
    use opentelemetry_otlp::WithExportConfig;
    use opentelemetry_sdk::trace::{RandomIdGenerator, Sampler, SdkTracerProvider};
    use opentelemetry_sdk::Resource;
    use std::time::Duration;

    let rt = tokio::runtime::Runtime::new()?;
    rt.block_on(async {
        let endpoint = "http://127.0.0.1:4317";
        std::env::set_var("OTEL_EXPORTER_OTLP_ENDPOINT", endpoint);

        let exporter = opentelemetry_otlp::SpanExporter::builder()
            .with_tonic()
            .with_endpoint("http://127.0.0.1:4317")
            .build()?;

        let resource = Resource::builder_empty()
            .with_service_name("chicago-tdd-tools-negative-test")
            .build();

        let provider = SdkTracerProvider::builder()
            .with_batch_exporter(exporter)
            .with_sampler(Sampler::TraceIdRatioBased(1.0))
            .with_id_generator(RandomIdGenerator::default())
            .with_resource(resource)
            .build();

        let tracer = provider.tracer("chicago-tdd-tools");

        // Negative case: Missing process.object.type
        let mut span = tracer.span_builder("workflow").start(&tracer);
        span.add_event(
            "praxis.activity_executed",
            vec![
                KeyValue::new("process.workflow.id", 123i64),
                KeyValue::new("process.object.id", "obj-456"),
                KeyValue::new("process.activity.iri", "http://chatman-equation.org/core/Execute"),
                KeyValue::new("process.outcome", "completed"),
            ],
        );
        span.end();
        provider.force_flush()?;
        tokio::time::sleep(Duration::from_millis(500)).await;
        provider.shutdown()?;
        Ok(())
    })
}

#[cfg(not(feature = "weaver"))]
fn main() {}
