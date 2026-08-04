# `examples/_archive/jidoka_line.rs`

Source SHA-256: `0ca58861368607427d66d395a9b57c4a2c9b673ce13cb7faa974fb0cdcc11791`

```mermaid
classDiagram
    class struct_MockGate {
      <<struct>>
      +"name: String"
      +"signal: AndonSignal"
    }
    note "Gate for MockGate"
    note "MockGate"
    note "ggen_jidoka::Signal for MockGate"
```

## Dependencies

- `ggen_jidoka::{ gate::{CompilerGate, LintGate, TestGate}, AndonSignal, Gate, ProductionLine, Result, }`
- `std::sync::Arc`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
