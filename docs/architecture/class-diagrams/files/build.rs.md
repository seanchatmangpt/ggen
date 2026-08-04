# `build.rs`

Source SHA-256: `0369559c2d68be9f71e943a267f1bd3cf4b3eadcd4f4121e0936ab93c5ef0b58`

```mermaid
classDiagram
    class fn_main {
      <<fn>>
    }
    class struct_TemplateInfo {
      <<struct>>
      +"path: String"
      +"name: String"
      +"category: String"
    }
    class fn_discover_templates {
      <<fn>>
    }
    class fn_discover_templates_recursive {
      <<fn>>
    }
    class fn_generate_template_registry {
      <<fn>>
    }
    class fn_escape_string {
      <<fn>>
    }
    class struct_OntologyInfo {
      <<struct>>
      +"name: String"
      +"path: String"
      +"namespace: String"
      +"size: u64"
    }
    class fn_discover_core_ontologies {
      <<fn>>
    }
    class fn_generate_ontology_bundle {
      <<fn>>
    }
```

## Dependencies

- `std::env`
- `std::fs::{self, File}`
- `std::io::Write`
- `std::path::Path`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
