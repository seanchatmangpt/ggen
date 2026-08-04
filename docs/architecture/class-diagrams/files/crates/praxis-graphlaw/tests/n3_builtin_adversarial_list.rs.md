# `crates/praxis-graphlaw/tests/n3_builtin_adversarial_list.rs`

Source SHA-256: `502273c8f7daf0cd912a7e8732afcd2267c8406739a8b75ccbe304545c14ea59`

```mermaid
classDiagram
    class fn_decode_all {
      <<fn>>
    }
    class fn_materialize {
      <<fn>>
    }
    class fn_list_length_of_empty_list_is_zero {
      <<fn>>
    }
    class fn_list_first_and_last_of_empty_list_do_not_derive {
      <<fn>>
    }
    class fn_list_first_and_last_of_single_element_list_are_the_same_element {
      <<fn>>
    }
    class fn_list_rest_of_single_element_list_is_empty {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `proptest::prelude::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
