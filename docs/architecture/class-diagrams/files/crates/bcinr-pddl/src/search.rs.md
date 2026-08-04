# `crates/bcinr-pddl/src/search.rs`

Source SHA-256: `0085470f0779c860d10e58e119ccddb73114ac9c0aed9b27eb801c5f5029f825`

```mermaid
classDiagram
    class enum_ExactStepOutcome {
      <<enum>>
    }
    class trait_ExactSearchRail {
      <<trait>>
      +"step(&mut self) -~ ExactStepOutcome"
    }
    class struct_ExactBfsRail {
      <<struct>>
      +"problem: &'a GroundProblem"
      +"result: Option~ExactStepOutcome~"
    }
    class enum_ExploitStepOutcome {
      <<enum>>
    }
    class trait_ExploitSearchRail {
      <<trait>>
      +"step(&mut self) -~ ExploitStepOutcome"
    }
    class struct_QLensRail {
      <<struct>>
      +"problem: &'a GroundProblem"
      +"current_state: BTreeSet~Pddl8GroundAtom~"
      +"plan_so_far: Vec~usize~"
      +"visited: BTreeSet~Vec~Pddl8GroundAtom~~"
      +"q: QValue"
      +"done: bool"
    }
    class enum_RailSelection {
      <<enum>>
    }
    class struct_FairRailScheduler {
      <<struct>>
      +"max_gap: usize"
      +"ticks_since_exact: usize"
      +"num_exploit_rails: usize"
      +"next_exploit: usize"
    }
    class enum_PortfolioOutcome {
      <<enum>>
    }
    class struct_MfwPortfolio {
      <<struct>>
      +"exact: E"
      +"exploit: Vec~X~"
      +"scheduler: FairRailScheduler"
      +"max_ticks: usize"
    }
    class mod_tests {
      <<mod>>
    }
    note "ExactBfsRail~"
    note "ExactSearchRail for ExactBfsRail~"
    note "ExploitSearchRail for QLensRail~"
    note "FairRailScheduler"
    note "MfwPortfolio~E"
    note "QLensRail~"
```

## Dependencies

- `bcinr_mfw_ir::{BoundHit, ExhaustionWitness, PlannerOutcome}`
- `crate::ground::GroundProblem`
- `crate::mfw::{q_lens, PositiveDistribution, PositiveMass, QValue}`
- `crate::parse::{domain_from_pddl, problem_from_pddl}`
- `std::collections::BTreeSet`
- `super::*`
- `wasm4pm_compat::pddl::{Pddl8GroundAtom, Pddl8Tape}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
