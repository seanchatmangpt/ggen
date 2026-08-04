# `crates/chicago-tdd-tools/src/testing/effects.rs`

Source SHA-256: `772c246cbb5449cd2174198c3942d5a6f551f4c05747d65c62744dc69169b373`

```mermaid
classDiagram
    class struct_NetworkRead {
      <<struct>>
    }
    class struct_NetworkWrite {
      <<struct>>
    }
    class struct_StorageRead {
      <<struct>>
    }
    class struct_StorageWrite {
      <<struct>>
    }
    class struct_Privileged {
      <<struct>>
    }
    class struct_Pure {
      <<struct>>
    }
    class struct_Effects {
      <<struct>>
      +"_marker: PhantomData~E~"
    }
    class trait_HasEffect {
      <<trait>>
    }
    class struct_EffectTest {
      <<struct>>
      +"name: String"
      +"_effects: PhantomData~E~"
    }
    class trait_RequiresEffect {
      <<trait>>
      +"execute(&self, effect: &Self::Effect) -~ Result~(), String~"
    }
    class struct_HttpGet {
      <<struct>>
      +"url: String"
    }
    class struct_FileWrite {
      <<struct>>
      +"path: String"
      +"content: String"
    }
    class struct_EffectCoverage {
      <<struct>>
      +"effect_name: String"
      +"test_count: usize"
      +"invariants: Vec~String~"
    }
    class struct_EffectCoverageRegistry {
      <<struct>>
      +"effects: Vec~EffectCoverage~"
    }
    class mod_tests {
      <<mod>>
    }
    note "Clone for Effects~E~"
    note "Copy for Effects~E~"
    note "Default for Effects~E~"
    note "EffectCoverage"
    note "EffectCoverageRegistry"
    note "EffectTest~E~"
    note "Effects~E~"
    note "FileWrite"
    note "HasEffect~NetworkRead~ for Effects~NetworkRead~"
    note "HasEffect~NetworkRead~ for Effects~Pure~"
    note "HasEffect~NetworkWrite~ for Effects~NetworkWrite~"
    note "HasEffect~NetworkWrite~ for Effects~Pure~"
    note "HasEffect~Privileged~ for Effects~Privileged~"
    note "HasEffect~Privileged~ for Effects~Pure~"
    note "HasEffect~Pure~ for Effects~Pure~"
    note "HasEffect~StorageRead~ for Effects~Pure~"
    note "HasEffect~StorageRead~ for Effects~StorageRead~"
    note "HasEffect~StorageWrite~ for Effects~Pure~"
    note "HasEffect~StorageWrite~ for Effects~StorageWrite~"
    note "HttpGet"
    note "RequiresEffect~NetworkRead~ for HttpGet"
    note "RequiresEffect~StorageWrite~ for FileWrite"
```

## Dependencies

- `std::fmt::Write`
- `std::marker::PhantomData`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
