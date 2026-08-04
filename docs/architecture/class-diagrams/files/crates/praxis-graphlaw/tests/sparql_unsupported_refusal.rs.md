# `crates/praxis-graphlaw/tests/sparql_unsupported_refusal.rs`

Source SHA-256: `e1abf349b38b8e6ed08d511dd81df7aceb4a84f610017ca0abdfe6c3ae575b12`

```mermaid
classDiagram
    class fn_store {
      <<fn>>
    }
    class fn_probed_broken_constructs_are_refused_not_silently_wrong {
      <<fn>>
    }
    class fn_probed_working_constructs_still_answer_correctly {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::parser::Syntax`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
