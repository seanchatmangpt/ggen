# `crates/praxis-graphlaw/tests/n3_unsupported_builtin_guard.rs`

Source SHA-256: `fdf5ba05d011899249f9837d32842ed0117b4e563309c0b4e5e3da2f7b2f378a`

```mermaid
classDiagram
    class fn_exotic_builtin_predicate_panics_instead_of_silently_failing {
      <<fn>>
    }
    class fn_ordinary_unmatched_predicate_does_not_panic {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `std::panic`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
