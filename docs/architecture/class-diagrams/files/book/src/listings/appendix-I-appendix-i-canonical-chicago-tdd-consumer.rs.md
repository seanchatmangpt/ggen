# `book/src/listings/appendix-I-appendix-i-canonical-chicago-tdd-consumer.rs`

Source SHA-256: `5de48cdfc63a83d26c8e8e539f3241fab93a8ada008606fdda5bf28e47b8f991`

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
