# `crates/chicago-tdd-tools/src/validation/coverage.rs`

Source SHA-256: `79d3fca272779993cbe2c3e8d4da70ad9c58d2b42fa9d6d95bfd31bfed1745ba`

```mermaid
classDiagram
    class struct_TotalCount {
      <<struct>>
    }
    class struct_CoveredCount {
      <<struct>>
    }
    class struct_CoveragePercentage {
      <<struct>>
    }
    class struct_CoverageReport {
      <<struct>>
      +"total: TotalCount"
      +"covered: CoveredCount"
      +"percentage: CoveragePercentage"
      +"details: HashMap~String"
    }
    class mod_tests {
      <<mod>>
    }
    note "CoveragePercentage"
    note "CoverageReport"
    note "CoveredCount"
    note "Default for CoverageReport"
    note "From~CoveragePercentage~ for f64"
    note "From~CoveredCount~ for usize"
    note "From~TotalCount~ for usize"
    note "TotalCount"
```

## Dependencies

- `std::collections::HashMap`
- `std::fmt::Write`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
