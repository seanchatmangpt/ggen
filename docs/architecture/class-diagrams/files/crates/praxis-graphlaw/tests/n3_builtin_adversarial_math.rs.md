# `crates/praxis-graphlaw/tests/n3_builtin_adversarial_math.rs`

Source SHA-256: `3377547aedca92f5e72306037d1e7dee81cc1a1fec1ea50b2c060a62819485c8`

```mermaid
classDiagram
    class fn_decode_all {
      <<fn>>
    }
    class fn_materialize {
      <<fn>>
    }
    class fn_math_logarithm_of_zero_yields_neg_infinity_literal_not_a_crash {
      <<fn>>
    }
    class fn_math_logarithm_of_negative_yields_nan_literal_not_a_crash {
      <<fn>>
    }
    class fn_math_greater_than_rejects_non_numeric_literal {
      <<fn>>
    }
    class fn_math_sum_rejects_non_numeric_list_member {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `proptest::prelude::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
