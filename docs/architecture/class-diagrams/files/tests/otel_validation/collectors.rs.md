# `tests/otel_validation/collectors.rs`

Source SHA-256: `03fa63c7fae2f7ab6c7a974545af4df214d78e99deb044919d196df6cbf8bed4`

```mermaid
classDiagram
    class struct_InMemoryExporter {
      <<struct>>
      +"collector: Arc~TraceCollector~"
    }
    class struct_TestSpanBuilder {
      <<struct>>
      +"name: String"
      +"attributes: HashMap~String"
    }
    class struct_TraceAsserter {
      <<struct>>
      +"collector: &'a TraceCollector"
    }
    class struct_PerformanceMetrics {
      <<struct>>
      +"generation_time_ms: f64"
      +"memory_usage_mb: f64"
      +"cpu_usage_percent: f64"
    }
    class mod_tests {
      <<mod>>
    }
    note "InMemoryExporter"
    note "PerformanceMetrics"
    note "TestSpanBuilder"
    note "TraceAsserter~"
```

## Dependencies

- `std::sync::Arc`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
