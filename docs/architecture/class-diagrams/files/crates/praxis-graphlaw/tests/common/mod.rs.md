# `crates/praxis-graphlaw/tests/common/mod.rs`

Source SHA-256: `bf6bc9e0caca0cd8dd0a83e605dce7e2504772d89665bb3cf4d9910995104730`

```mermaid
classDiagram
    class struct_HookReceipt {
      <<struct>>
      +"hook_name: String"
      +"delta_hash: String"
      +"idempotency_key: String"
      +"delta_quads: String"
    }
    class fn_decode_or_panic {
      <<fn>>
    }
    class fn_assert_contains_triple {
      <<fn>>
    }
    class fn_assert_not_contains_triple {
      <<fn>>
    }
    class fn_decode_all {
      <<fn>>
    }
    class fn_pred {
      <<fn>>
    }
    class fn_build_data_index {
      <<fn>>
    }
    class fn_materialize {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::parser::{Parser, Syntax}`
- `praxis_graphlaw::tripleindex::TripleIndex`
- `praxis_graphlaw::triples::Triple`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
