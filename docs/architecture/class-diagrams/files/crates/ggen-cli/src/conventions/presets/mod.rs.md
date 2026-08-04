# `crates/ggen-cli/src/conventions/presets/mod.rs`

Source SHA-256: `30baf219d61cd6541062c55ce4565d00841282154a3b5f97ab6767476ed617a2`

```mermaid
classDiagram
    class mod_clap_noun_verb {
      <<mod>>
    }
    class trait_ConventionPreset {
      <<trait>>
      +"name(&self) -~ &str"
      +"create_structure(&self, root: &Path) -~ Result~()~"
      +"rdf_files(&self) -~ Vec~(&str, &str)~"
      +"templates(&self) -~ Vec~(&str, &str)~"
      +"config_content(&self) -~ String"
    }
    class fn_get_preset {
      <<fn>>
    }
    class fn_list_presets {
      <<fn>>
    }
```

## Dependencies

- `crate::utils::error::Result`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
