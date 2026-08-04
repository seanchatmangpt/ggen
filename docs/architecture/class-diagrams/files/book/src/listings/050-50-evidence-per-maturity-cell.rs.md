# `book/src/listings/050-50-evidence-per-maturity-cell.rs`

Source SHA-256: `4005693d84a388ee859642c553ff969f7019d3316430f8cdb1031c45a65b54f6`

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
