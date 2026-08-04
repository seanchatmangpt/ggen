# `examples/affidavit-verify/src/affidavit_catalog.rs`

Source SHA-256: `08dd1664e6039ffaacab35c424f54b690ddb42e74e396955fda05324c15164e0`

```mermaid
classDiagram
    class struct_CertifyStage {
      <<struct>>
      +"order: u32"
      +"label: &'static str"
      +"doc: &'static str"
    }
    class struct_AffidavitCommand {
      <<struct>>
      +"verb: &'static str"
      +"doc: &'static str"
    }
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
