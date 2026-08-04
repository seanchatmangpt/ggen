# `book/src/listings/208-208-fm-pack-012-and-fm-pack-013.rs`

Source SHA-256: `0ebe257cc59ef0e5ab39040a65be0dc9659faab50ddfccee0a51a9a13d5896d3`

```mermaid
classDiagram
    class enum_Standing {
      <<enum>>
    }
    class struct_ChapterArtifact {
      <<struct>>
      +"chapter: u16"
      +"title: &'static str"
      +"standing: Standing"
      +"evidence: &'static [&'static str]"
    }
    class fn_chapter_artifact_has_non_vacuous_evidence {
      <<fn>>
    }
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
