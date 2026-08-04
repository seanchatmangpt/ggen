# `tools/pack-gall/src/io.rs`

Source SHA-256: `71d4cb67c19d278d7e4f75e34b777575d1e855a9f32a53fdd4c6a0706d763f03`

```mermaid
classDiagram
    class fn_write_json {
      <<fn>>
    }
    class fn_read_observation {
      <<fn>>
    }
    class fn_parse_args {
      <<fn>>
    }
    class fn_digest_bytes {
      <<fn>>
    }
    class fn_digest_json {
      <<fn>>
    }
    class fn_digest_path {
      <<fn>>
    }
```

## Dependencies

- `crate::model::{Observation, OBSERVATION_SCHEMA}`
- `serde::Serialize`
- `std::collections::BTreeMap`
- `std::fs`
- `std::path::{Path, PathBuf}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
