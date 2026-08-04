# `book/src/listings/appendix-R-appendix-r-tcps-corrective-divergence-ledger.rs`

Source SHA-256: `92414534745d01164ef92a63d06f0bede4567d7205c15ca951bb8b6b92e10648`

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
