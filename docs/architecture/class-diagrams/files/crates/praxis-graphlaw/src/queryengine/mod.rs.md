# `crates/praxis-graphlaw/src/queryengine/mod.rs`

Source SHA-256: `f166cd1fdac8eca318b0317beac13f6ce87f369042a8cdfafdc16080a6a02fd0`

```mermaid
classDiagram
    class trait_QueryEngine {
      <<trait>>
      +"query(
        data: &TripleIndex, query_triples: &Vec~BodyLiteral~, triple_counter: Option~usize~,
    ) -~ Option~Binding~"
      +"query_semi_naive(
        data: &TripleIndex, query_triples: &Vec~BodyLiteral~, prev_limit: usize,
        current_limit: usize,
    ) -~ Option~Binding~"
    }
    class struct_SimpleQueryEngine {
      <<struct>>
    }
    class fn_collect_vars {
      <<fn>>
    }
    class fn_builtin_input_vars {
      <<fn>>
    }
    note "QueryEngine for SimpleQueryEngine"
```

## Dependencies

- `crate::builtins`
- `crate::builtins::BuiltinKind`
- `crate::{Binding, BodyLiteral, TripleIndex, VarOrTerm}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
