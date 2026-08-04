# `examples/praxis-core-verify/src/praxis_core_refusal_table.rs`

Source SHA-256: `3dbffc7b3454de8f1705d29b5174f019b5ae355b9ef629a7845e927fe863cba5`

```mermaid
classDiagram
    class struct_RefusalTaxonomyRow {
      <<struct>>
      +"scenario: &'static str"
      +"category: &'static str"
      +"denial_lane: &'static str"
      +"variant_kind: &'static str"
      +"note: &'static str"
    }
    class fn_category_for_scenario {
      <<fn>>
    }
    class fn_denial_lane_for_scenario {
      <<fn>>
    }
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
