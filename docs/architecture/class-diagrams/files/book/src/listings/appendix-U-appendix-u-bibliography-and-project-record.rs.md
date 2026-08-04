# `book/src/listings/appendix-U-appendix-u-bibliography-and-project-record.rs`

Source SHA-256: `0a4f5bca70a8e30032f5cc856fb2d4d7227d76ee6f66c121a01c1a42df1ff95c`

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
