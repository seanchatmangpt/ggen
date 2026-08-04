# `crates/praxis-graphlaw/src/ruleindex.rs`

Source SHA-256: `b0773a82d5ed6f21f3ed7b1f089a867d0cc217a9a3238a365a5c7aeadc4e1cdc`

```mermaid
classDiagram
    class struct_RuleIndex {
      <<struct>>
      +"rules: Vec~Arc~Rule~~"
      +"spo: Vec~Arc~Rule~~"
      +"s: FxHashMap~usize"
      +"p: FxHashMap~usize"
      +"o: FxHashMap~usize"
      +"sp: FxHashMap~usize"
      +"po: FxHashMap~usize"
      +"so: FxHashMap~usize"
      +"spo_all: FxHashMap~usize"
      +"head_by_pred: FxHashMap~usize"
    }
    note "Default for RuleIndex"
    note "RuleIndex"
```

## Dependencies

- `crate::fastmap::FxHashMap`
- `crate::{Rule, Triple}`
- `std::sync::Arc`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
