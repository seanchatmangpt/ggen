// Example: Using Weaver live-check for telemetry validation
// This demonstrates how to integrate OTEL Weaver live-check with KNHKS telemetry

use knhk_otel::{Tracer, SpanStatus, WeaverLiveCheck, MetricsHelper};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Start Weaver live-check server
    let weaver = WeaverLiveCheck::new()
        .with_registry("./schemas/my-registry".to_string()) // Optional: path to semantic convention registry
        .with_otlp_port(4317)  // Default OTLP gRPC port
        .with_admin_port(8080)  // HTTP admin endpoint for stopping
        .with_format("json".to_string())  // Output format: json or ansi
        .with_output("./weaver-reports".to_string())  // Optional: output directory for reports
        .with_inactivity_timeout(60);  // Stop after 60s of inactivity

    println!("Starting Weaver live-check...");
    let mut weaver_process = weaver.start()?;
    println!("Weaver live-check started on {}", weaver.otlp_endpoint());

    // 2. Configure tracer to export to Weaver's OTLP endpoint
    // Note: In production, use Weaver's gRPC endpoint (requires opentelemetry-otlp)
    // For this example, we'll use HTTP export which works with Weaver's HTTP adapter
    let mut tracer = Tracer::with_otlp_exporter(format!("http://{}", weaver.otlp_endpoint()));

    // 3. Generate some telemetry
    let span_ctx = tracer.start_span("knhk.operation.execute".to_string(), None);
    tracer.add_attribute(span_ctx, "operation".to_string(), "ASK_SP".to_string());
    tracer.add_attribute(span_ctx, "predicate".to_string(), "rdf:type".to_string());
    
    MetricsHelper::record_hook_latency(&mut tracer, 5, "ASK_SP");
    MetricsHelper::record_receipt(&mut tracer, "receipt-123");
    
    tracer.end_span(span_ctx, SpanStatus::Ok);

    // 4. Export telemetry to Weaver for validation
    println!("Exporting telemetry to Weaver...");
    tracer.export()?;

    // 5. Wait a bit for Weaver to process
    std::thread::sleep(std::time::Duration::from_secs(2));

    // 6. Stop Weaver live-check
    println!("Stopping Weaver live-check...");
    weaver.stop()?;
    
    // Wait for process to exit
    let _ = weaver_process.wait();
    
    println!("Weaver live-check completed. Check ./weaver-reports for validation results.");
    println!("Exit code 0 = compliant, non-zero = violations found");

    Ok(())
}

