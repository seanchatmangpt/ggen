# `crates/bcinr-pddl/tests/brce_loop.rs`

Source SHA-256: `0c810732d96b735891dbb9f098de140af816ab29c54a84ae996e726edcee1570`

```mermaid
classDiagram
    class fn_blocksworld_brce_full_loop {
      <<fn>>
    }
    class fn_empty_tape_goal_not_reached {
      <<fn>>
    }
    class fn_prolog8_horn_denies_unadmitted_action {
      <<fn>>
    }
    class fn_receipt_differs_by_case_id {
      <<fn>>
    }
```

## Dependencies

- `bcinr_pddl::{domain_from_pddl, execute_tape, problem_from_pddl, GroundProblem, Pddl8Error}`
- `std::collections::BTreeSet`
- `wasm4pm_compat::pddl::Pddl8GroundAtom`
- `wasm4pm_compat::pddl::Pddl8Tape`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
