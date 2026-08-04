# `book/src/listings/149-149-a-pack-without-a-consumer-is-not-finished.rs`

Source SHA-256: `59aceb1e5e751ad7c7e2302a84cbbfdcf2015a219efdd9de59c214550a39b5b2`

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
