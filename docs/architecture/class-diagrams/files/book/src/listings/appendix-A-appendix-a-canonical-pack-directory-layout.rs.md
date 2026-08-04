# `book/src/listings/appendix-A-appendix-a-canonical-pack-directory-layout.rs`

Source SHA-256: `d22be29f988898ea4d109beba8dc711e900d9cc45d7a5312f0af43c05080bfe2`

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
