# `book/src/listings/appendix-S-appendix-s-ggen-failure-code-reference.rs`

Source SHA-256: `90b9a7f3cbd81f8edcc611fc7c2d092b23a1df9d2873305088382f1d87cc74a2`

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
