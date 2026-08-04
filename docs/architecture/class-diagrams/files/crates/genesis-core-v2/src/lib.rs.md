# `crates/genesis-core-v2/src/lib.rs`

Source SHA-256: `05ea3ba0de4a2b88a6916c2f6f9bfee7515ea0dce6f04343bb973aa2e1e3ad00`

```mermaid
classDiagram
    class mod_inventory {
      <<mod>>
    }
    class mod_primitives {
      <<mod>>
    }
    class mod_registry {
      <<mod>>
    }
    class mod_revelation {
      <<mod>>
    }
    class mod_split_laws {
      <<mod>>
    }
    class trait_Pattern {
      <<trait>>
      +"metadata(&self) -~ &PatternMetadata"
      +"execute(&self, context: &mut ExecutionContext) -~ Result~()~"
      +"validate(&self, context: &ExecutionContext) -~ Result~()~"
      +"get_next_steps(&self, context: &ExecutionContext) -~ Vec~PatternId~"
      +"write_next_steps(&self, _context: &ExecutionContext, _out: &mut [PatternId]) -~ usize"
    }
    class struct_SequencePattern {
      <<struct>>
      +"metadata: PatternMetadata"
    }
    class struct_ExclusiveChoicePattern {
      <<struct>>
      +"metadata: PatternMetadata"
    }
    class struct_ParallelSplitPattern {
      <<struct>>
      +"metadata: PatternMetadata"
    }
    class struct_CorePatternRegistry {
      <<struct>>
      +"registry: Arc~DashMap~u32"
    }
    class mod_tests {
      <<mod>>
    }
    note "CorePatternRegistry"
    note "Default for CorePatternRegistry"
    note "Default for ExclusiveChoicePattern"
    note "Default for ParallelSplitPattern"
    note "Default for SequencePattern"
    note "ExclusiveChoicePattern"
    note "ParallelSplitPattern"
    note "Pattern for ExclusiveChoicePattern"
    note "Pattern for ParallelSplitPattern"
    note "Pattern for SequencePattern"
    note "SequencePattern"
```

## Dependencies

- `async_trait::async_trait`
- `dashmap::DashMap`
- `genesis_types::schema::PatternMetadata`
- `genesis_types::{Error, ExecutionContext, PatternId, Result, WorkflowStep}`
- `inventory::{ArtifactStatus, ClassifiedArtifact, ConnectionMechanism, FinishStep}`
- `primitives::{ Construct8, Pair2, Receipt, Refusal, RefusalReason, RelationPage, ReplayCursor, }`
- `registry::PatternRegistry`
- `revelation::{ passes_all_gates, verify_lamb_authority, BabylonClaim, Church, ChurchJudgment, ChurchVerdict, JerusalemGate, Plague, PlagueRecord, Seal, SealState, }`
- `serde_json::json`
- `split_laws::{need257_split, need9_split, SplitResult}`
- `std::sync::Arc`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
