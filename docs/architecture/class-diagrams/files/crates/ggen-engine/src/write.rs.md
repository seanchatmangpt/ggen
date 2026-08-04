# `crates/ggen-engine/src/write.rs`

Source SHA-256: `08e2fed4e315babffc2d4839b14412dce73aa898096ebe4826fd2153e2d2a644`

```mermaid
classDiagram
    class enum_WriteOutcome {
      <<enum>>
    }
    class fn_plan_write {
      <<fn>>
    }
    class fn_check_freeze {
      <<fn>>
    }
    class fn_record_freeze_checksum {
      <<fn>>
    }
    class fn_freeze_slots_dir {
      <<fn>>
    }
    class fn_freeze_checksum_path {
      <<fn>>
    }
    class fn_preflight_checksum_slot {
      <<fn>>
    }
    class fn_maybe_backup {
      <<fn>>
    }
    class fn_resolve_target {
      <<fn>>
    }
    class fn_ensure_parent {
      <<fn>>
    }
    class enum_MatchUse {
      <<enum>>
    }
    class struct_MatchSpan {
      <<struct>>
      +"start: usize"
      +"end: usize"
      +"start_line: usize"
      +"end_line: usize"
    }
    class struct_MatchObservation {
      <<struct>>
      +"matcher: MatchKind"
      +"scope: MatchScope"
      +"occurrence: MatchOccurrence"
      +"count: usize"
      +"selected: Option~MatchSpan~"
    }
    class enum_CompiledMatcher {
      <<enum>>
    }
    class struct_ResolvedMatch {
      <<struct>>
      +"pattern: &'a str"
      +"matcher: MatchKind"
      +"scope: MatchScope"
      +"occurrence: MatchOccurrence"
      +"index: usize"
      +"case_sensitive: bool"
      +"trim: bool"
    }
    class fn_resolve_match {
      <<fn>>
    }
    class fn_compile_matcher {
      <<fn>>
    }
    class fn_candidate_view {
      <<fn>>
    }
    class fn_line_for_offset {
      <<fn>>
    }
    class fn_select_span {
      <<fn>>
    }
    class fn_observe_match {
      <<fn>>
    }
    class fn_validate_match_specs {
      <<fn>>
    }
    class fn_preflight_structured_matchers {
      <<fn>>
    }
    class fn_inject_into {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
    note "CompiledMatcher~"
    note "MatchObservation"
    note "MatchUse"
```

## Dependencies

- `crate::{ error::{AppError, Result}, template::{ FreezePolicy, Frontmatter, MatchKind, MatchOccurrence, MatchRule, MatchScope, MatchSpec, }, }`
- `regex::{Regex, RegexBuilder}`
- `std::path::{Component, Path, PathBuf}`
- `super::*`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
