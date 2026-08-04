# `crates/praxis-graphlaw/src/rsp.rs`

Source SHA-256: `a56799d78f4a8ccd905df8b3376ecf5a580578faf69175718bf2b02846018a4f`

```mermaid
classDiagram
    class mod_r2r {
      <<mod>>
    }
    class mod_r2s {
      <<mod>>
    }
    class mod_s2r {
      <<mod>>
    }
    class enum_OperationMode {
      <<enum>>
    }
    class struct_RSPBuilder {
      <<struct>>
      +"width: usize"
      +"slide: usize"
      +"tick: Option~Tick~"
      +"report_strategy: Option~ReportStrategy~"
      +"triples: Option~&'a str~"
      +"syntax: Option~Syntax~"
      +"rules: Option~&'a str~"
      +"query_str: Option~&'a str~"
      +"result_consumer: Option~ResultConsumer~O~~"
      +"r2s: Option~StreamOperator~"
      +"r2r: Option~Box~dyn R2ROperator~I"
      +"operation_mode: OperationMode"
    }
    class struct_RSPEngine {
      <<struct>>
      +"s2r: CSPARQLWindow~I~"
      +"r2r: Arc~Mutex~Box~dyn R2ROperator~I"
      +"r2s_consumer: ResultConsumer~O~"
      +"r2s_operator: Arc~Mutex~Relation2StreamOperator~O~~~"
    }
    class struct_ResultConsumer {
      <<struct>>
      +"function: Arc~dyn Fn(I) + Send + Sync~"
    }
    class struct_SimpleR2R {
      <<struct>>
      +"item: TripleStore"
    }
    class mod_rsp_test {
      <<mod>>
    }
    note "R2ROperator~WindowTriple"
    note "RSPBuilder~"
    note "RSPEngine~I"
```

## Dependencies

- `crate::rsp::r2r::R2ROperator`
- `crate::rsp::r2s::{Relation2StreamOperator, StreamOperator}`
- `crate::rsp::s2r::{ CSPARQLWindow, ContentContainer, Report, ReportStrategy, Tick, WindowTriple, }`
- `crate::sparql::{evaluate_plan_and_debug, Binding}`
- `crate::{Encoder, Syntax, Triple, TripleStore}`
- `log::{debug, error}`
- `spargebra::Query`
- `std::fmt::Debug`
- `std::hash::Hash`
- `std::sync::mpsc::Receiver`
- `std::sync::{Arc, Mutex}`
- `std::thread`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
