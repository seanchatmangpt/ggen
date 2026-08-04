# `crates/praxis-graphlaw/src/rsp/r2r.rs`

Source SHA-256: `64f8e96415c9e8775ce2becc6e61a6c97cbf614d5c23715907a0199a878c3dde`

```mermaid
classDiagram
    class trait_R2ROperator {
      <<trait>>
      +"load_triples(&mut self, data: &str, syntax: Syntax) -~ Result~(), String~"
      +"load_rules(&mut self, data: &str) -~ Result~(), &'static str~"
      +"add(&mut self, data: I)"
      +"remove(&mut self, data: &I)"
      +"materialize(&mut self) -~ Vec~I~"
      +"execute_query(&self, query: &Query) -~ Vec~O~"
    }
```

## Dependencies

- `crate::Syntax`
- `spargebra::Query`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
