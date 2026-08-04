# `book/src/listings/012-12-why-generated-files-are-not-authority.rs`

Source SHA-256: `f62d9dfc37191c6bfb987cb757176e18e88a7c78fbcf2b9d6096b0c9b0e56b60`

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
