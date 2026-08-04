# `crates/praxis-graphlaw/tests/datalog_conformance/safe_unsafe_rejection.rs`

Source SHA-256: `7c2ba27d7df113c9a93fd98b6271adf642cbdb5f3a24e446324e88cb36ed0927`

```mermaid
classDiagram
    class fn_test_safe_rule_accepted {
      <<fn>>
    }
    class fn_test_unsafe_unbound_head_var_rejected {
      <<fn>>
    }
    class fn_test_unsafe_unbound_negated_var_rejected {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::triples::{BodyLiteral, Rule, Triple}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
