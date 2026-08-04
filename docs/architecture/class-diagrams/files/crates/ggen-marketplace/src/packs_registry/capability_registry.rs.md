# `crates/ggen-marketplace/src/packs_registry/capability_registry.rs`

Source SHA-256: `88410c4ffd1da174dde7ac15a26c2f0d07e83da2f65e920f9f54279217198978`

```mermaid
classDiagram
    class fn_resolve_capability_to_packs {
      <<fn>>
    }
    class struct_CapabilityDescriptor {
      <<struct>>
      +"id: String"
      +"name: String"
      +"description: String"
      +"category: String"
      +"atomic_packs: Vec~String~"
    }
    class fn_list_capabilities {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `crate::packs_registry::metadata::load_pack_metadata`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
