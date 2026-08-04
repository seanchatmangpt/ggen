# `crates/chicago-tdd-tools/src/core/assertions.rs`

Source SHA-256: `a17c3bcee94444bea9c7dcad953c4a395624447cd68e78090ddf60b6213fdd61`

```mermaid
classDiagram
    class fn_assert_success {
      <<fn>>
    }
    class fn_assert_error {
      <<fn>>
    }
    class fn_assert_eq_with_msg {
      <<fn>>
    }
    class fn_assert_in_range {
      <<fn>>
    }
    class fn_assert_that {
      <<fn>>
    }
    class fn_assert_that_with_msg {
      <<fn>>
    }
    class struct_AssertionBuilder {
      <<struct>>
      +"value: T"
      +"span: Option~Span~"
    }
    class struct_ValidatedAssertion {
      <<struct>>
      +"value: T"
      +"span: Span"
      +"metric: Option~Metric~"
    }
    class mod_tests {
      <<mod>>
    }
    note "AssertionBuilder~T~"
    note "ValidatedAssertion~T~"
```

## Dependencies

- `crate::observability::otel::types::{ Metric, MetricValue, Span, SpanContext, SpanId, SpanStatus, TraceId, }`
- `crate::test`
- `std::time::{SystemTime, UNIX_EPOCH}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
