# `crates/praxis-graphlaw/src/parser/iri_resolve.rs`

Source SHA-256: `bf8f685760a09fe0f35e96ea382faa5912b7a783b270b491dd77a3290af166f1`

```mermaid
classDiagram
    class struct_PrefixMapper {
      <<struct>>
      +"prefixes: HashMap~String"
      +"base: Option~String~"
    }
    class fn_split_scheme {
      <<fn>>
    }
    class fn_split_fragment {
      <<fn>>
    }
    class fn_split_query {
      <<fn>>
    }
    class fn_split_authority {
      <<fn>>
    }
    class fn_remove_dot_segments {
      <<fn>>
    }
    class fn_merge_path {
      <<fn>>
    }
    class fn_build_iri {
      <<fn>>
    }
    class fn_resolve_reference {
      <<fn>>
    }
    note "Default for PrefixMapper"
    note "PrefixMapper"
```

## Dependencies

- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
