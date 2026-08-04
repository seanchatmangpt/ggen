# `examples/receiptctl/src/w4pm_cognition_catalog.rs`

Source SHA-256: `6e868d2c2652d319ef0cb31b388d9624d4afacc40c78e58a2bfce1146fe28dde`

```mermaid
classDiagram
    class enum_CognitionBreedId {
      <<enum>>
    }
    class struct_CognitionBreedInfo {
      <<struct>>
      +"id: CognitionBreedId"
      +"label: &'static str"
      +"citation: &'static str"
      +"family: &'static str"
    }
    class fn_breed_families {
      <<fn>>
    }
    class fn_from_breed_id {
      <<fn>>
    }
    note "CognitionBreedId"
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
