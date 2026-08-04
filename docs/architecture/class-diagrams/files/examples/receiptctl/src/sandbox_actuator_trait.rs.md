# `examples/receiptctl/src/sandbox_actuator_trait.rs`

Source SHA-256: `8d338cd68616858629a7f06e39253f2906f00bd69911333c651236b6399fca7c`

```mermaid
classDiagram
    class struct_CapabilityRequest {
      <<struct>>
      +"capability: CapabilityId"
      +"files: Vec~(String"
      +"already_passed: Vec~CapabilityId~"
    }
    class struct_CapabilityReceipt {
      <<struct>>
      +"capability: CapabilityId"
      +"exit_code: i32"
      +"stdout: String"
      +"stderr: String"
      +"duration_ms: u64"
      +"digest: String"
    }
    class enum_CapabilityRefusal {
      <<enum>>
    }
    class trait_SandboxActuator {
      <<trait>>
      +"actuate(&self, request: CapabilityRequest) -~ Result~CapabilityReceipt, CapabilityRefusal~"
    }
    class fn_check_preconditions {
      <<fn>>
    }
```

## Dependencies

- `crate::sandbox_catalog::CapabilityId`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
