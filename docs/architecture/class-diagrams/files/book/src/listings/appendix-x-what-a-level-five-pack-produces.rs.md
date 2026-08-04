# `book/src/listings/appendix-x-what-a-level-five-pack-produces.rs`

Source SHA-256: `513d142fbea88cfad7dbf282fea788daaf3e2c35d76286f7aaa7e4f259e6f2d3`

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
