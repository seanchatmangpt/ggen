# `crates/bcinr-mfw-ir/src/event_set.rs`

Source SHA-256: `346b6738cbb48d504fd4e17a75f6b2cbdcab3073f375ce290b29feed7283cf47`

```mermaid
classDiagram
    class struct_EventSet {
      <<struct>>
      +"words: [u64; EVENT_WORDS]"
    }
    class struct_EventSetIter {
      <<struct>>
      +"words: [u64; EVENT_WORDS]"
      +"word_idx: usize"
    }
    class mod_tests {
      <<mod>>
    }
    note "EventSet"
    note "Iterator for EventSetIter"
    note "fmt::Debug for EventSet"
```

## Dependencies

- `std::fmt`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
