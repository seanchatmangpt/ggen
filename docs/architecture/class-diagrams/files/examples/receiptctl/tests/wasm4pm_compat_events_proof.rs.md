# `examples/receiptctl/tests/wasm4pm_compat_events_proof.rs`

Source SHA-256: `3aac5d3560ffbce4c4d1b5280ecf95f5370e5537e112c6cee9bcc13e299e5ec8`

```mermaid
classDiagram
    class mod_wasm4pm_compat_events {
      <<generated>>
    }
    class fn_literal_time {
      <<generated>>
    }
    class fn_event_type_count_matches_independent_sparql_oracle {
      <<generated>>
    }
    class fn_emitted_event_type_has_exactly_the_three_ontology_variants {
      <<generated>>
    }
    class fn_emitted_event_type_name_matches_ontology_event_name_literals {
      <<generated>>
    }
    class fn_emit_graph_union_hashed_binds_ontology_graph_relationship_and_attribute {
      <<generated>>
    }
    class fn_emit_pack_lock_verified_binds_pack_relationship_and_attribute {
      <<generated>>
    }
    class fn_emit_receipt_chained_binds_sync_receipt_relationship_and_attribute {
      <<generated>>
    }
    class fn_emitted_pack_lock_verified_event_round_trips_through_json {
      <<generated>>
    }
    class fn_emit_graph_union_hashed_panics_on_empty_id {
      <<generated>>
    }
    class fn_emit_graph_union_hashed_panics_on_empty_attribute {
      <<generated>>
    }
    class fn_emit_pack_lock_verified_panics_on_empty_id {
      <<generated>>
    }
    class fn_emit_receipt_chained_panics_on_empty_id {
      <<generated>>
    }
    class fn_generated_events_are_admitted_by_the_reference_crates_own_linked_ocel_law {
      <<generated>>
    }
    class fn_generated_event_with_undeclared_object_is_refused_by_the_reference_crates_own_law {
      <<generated>>
    }
    class fn_generated_event_with_no_object_links_is_refused_by_the_reference_crates_own_law {
      <<generated>>
    }
    class fn_generated_pack_lock_verified_event_is_refused_for_missing_reference_object {
      <<generated>>
    }
    class fn_generated_graph_union_hashed_event_is_refused_for_multiple_reference_objects {
      <<generated>>
    }
    class fn_generated_graph_union_hashed_events_sharing_a_child_under_different_parents_violate_locality {
      <<generated>>
    }
```

## Dependencies

- `chrono::DateTime`
- `wasm4pm_compat::admission::Admit`
- `wasm4pm_compat::evidence::Evidence`
- `wasm4pm_compat::ocel::{EventObjectLink, LinkedOcel, Object, OcelLog}`
- `wasm4pm_compat::ocel::{EventObjectLink, LinkedOcel, OcelLog, OcelRefusal}`
- `wasm4pm_compat::ocel::{EventObjectLink, Object, OcelLog, OcelRefusal}`
- `wasm4pm_compat::ocel::{LinkedOcel, Object, OcelLog, OcelRefusal}`
- `wasm4pm_compat::ocel::{OCELAttributeValue, OCELEvent, OCELEventAttribute, OCELRelationship}`
- `wasm4pm_compat_events::{EmittedEventType, EVENT_TYPE_COUNT}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
