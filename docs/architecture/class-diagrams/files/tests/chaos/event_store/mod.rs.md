# `tests/chaos/event_store/mod.rs`

Source SHA-256: `17b3a8c37e7320a03705e47bf5fea3a1fa1aab5958c404f20a949907236a17f8`

```mermaid
classDiagram
    class enum_FailureEvent {
      <<enum>>
    }
    class trait_EventStore {
      <<trait>>
      +"record(&mut self, event: FailureEvent) -~ Result~(), String~"
      +"get_all(&self) -~ Vec~FailureEvent~"
      +"get_for_component(&self, component: &str) -~ Vec~FailureEvent~"
      +"get_between(&self, start: u64, end: u64) -~ Vec~FailureEvent~"
      +"clear(&mut self)"
      +"len(&self) -~ usize"
      +"is_empty(&self) -~ bool"
      +"export_json(&self) -~ Result~String, String~"
    }
    class struct_InMemoryEventStore {
      <<struct>>
      +"events: Arc~Mutex~Vec~FailureEvent~~~"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for InMemoryEventStore"
    note "EventStore for InMemoryEventStore"
    note "FailureEvent"
    note "InMemoryEventStore"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `std::sync::{Arc, Mutex}`
- `std::time::Duration`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
