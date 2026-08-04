# `crates/ggen-lsp/src/features/semantic_tokens.rs`

Source SHA-256: `e1a13dcdc294f1a81811cf1c08abad2539901f0baee316da5d0ca573b154f281`

```mermaid
classDiagram
    class struct_AbsToken {
      <<struct>>
      +"line: u32"
      +"start: u32"
      +"length: u32"
      +"token_type: u32"
    }
    class fn_semantic_tokens {
      <<fn>>
    }
    class fn_delta_encode {
      <<fn>>
    }
    class fn_push {
      <<fn>>
    }
    class fn_tokenize_rdf {
      <<fn>>
    }
    class fn_tokenize_sparql {
      <<fn>>
    }
    class fn_tokenize_toml {
      <<fn>>
    }
    class fn_tokenize_tera {
      <<fn>>
    }
    class struct_TeraRegion {
      <<struct>>
      +"start: usize"
      +"end: usize"
      +"delims: (&'static str"
    }
    class fn_tera_regions {
      <<fn>>
    }
```

## Dependencies

- `crate::state::FileType`
- `lsp_max::lsp_types::{SemanticToken, SemanticTokens}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
