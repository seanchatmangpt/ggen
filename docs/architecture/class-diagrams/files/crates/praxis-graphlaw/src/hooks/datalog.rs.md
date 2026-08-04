# `crates/praxis-graphlaw/src/hooks/datalog.rs`

Source SHA-256: `54f7a4e17a6397db969a3ceb346f1b362d1e3f0e2c7cd08fdc38f53b7e792d6e`

```mermaid
classDiagram
    class struct_DatalogAtom {
      <<struct>>
      +"name: String"
      +"args: Vec~String~"
    }
    class fn_split_depth0 {
      <<fn>>
    }
    class fn_parse_datalog_atom {
      <<fn>>
    }
    class fn_format_term {
      <<fn>>
    }
    class fn_translate_datalog_to_n3 {
      <<fn>>
    }
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
