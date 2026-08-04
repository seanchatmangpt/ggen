# `marketplace/packages/telemedicine-platform/tests/chicago_tdd.rs`

Source SHA-256: `7805ae1ba9d7e27a03b3f7bdf134cd53faedf3f6a25129d31f0cbc02bbcdb628`

```mermaid
classDiagram
    class mod_telemedicine_tests {
      <<mod>>
    }
    class struct_TelemedicinePlatform {
      <<struct>>
      +"consultations: Vec~Consultation~"
      +"prescriptions: Vec~EPrescription~"
    }
    class struct_Consultation {
      <<struct>>
      +"id: String"
      +"consultation_type: ConsultationType"
      +"provider_id: String"
      +"patient_id: String"
      +"status: ConsultationStatus"
      +"encrypted: bool"
    }
    class struct_EPrescription {
      <<struct>>
      +"id: String"
      +"consultation_id: String"
    }
    class enum_ConsultationType {
      <<enum>>
    }
    class enum_ConsultationStatus {
      <<enum>>
    }
    note "TelemedicinePlatform"
```

## Dependencies

- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
