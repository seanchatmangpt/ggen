# `examples/receiptctl/src/sandbox_catalog.rs`

Source SHA-256: `75d0de60b06de2696211a22168f46037e867ec61df6671f80080ab2be6a99fa5`

```mermaid
classDiagram
    class enum_CapabilityId {
      <<enum>>
    }
    class enum_Operation {
      <<enum>>
    }
    class struct_CapabilityInfo {
      <<struct>>
      +"id: CapabilityId"
      +"language: &'static str"
      +"operation: Operation"
      +"doc: &'static str"
      +"timeout_ms: u64"
      +"authority_scope: &'static str"
      +"requires: &'static [CapabilityId]"
    }
    class fn_from_capability_id {
      <<fn>>
    }
    note "CapabilityId"
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
