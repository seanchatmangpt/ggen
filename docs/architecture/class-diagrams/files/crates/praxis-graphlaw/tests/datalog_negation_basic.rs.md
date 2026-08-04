# `crates/praxis-graphlaw/tests/datalog_negation_basic.rs`

Source SHA-256: `7f101e2be9c7662731b2cbe079aa6be9c052a6403aabea51f882f47f99a66ddc`

```mermaid
classDiagram
    class fn_test_stratified_negation_basic {
      <<fn>>
    }
    class fn_test_unstratifiable_rules_rejected {
      <<fn>>
    }
    class fn_test_rule_safety_check_rejects_unbound_negated_var {
      <<fn>>
    }
    class fn_test_negation_empty_relations {
      <<fn>>
    }
    class fn_test_empty_body_rule {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::triples::{BodyLiteral, Rule, Triple}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
