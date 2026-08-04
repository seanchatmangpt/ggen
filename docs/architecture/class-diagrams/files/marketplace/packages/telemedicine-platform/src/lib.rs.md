# `marketplace/packages/telemedicine-platform/src/lib.rs`

Source SHA-256: `090bf9ec3c8933c438ddb0b809b3b110f3a8edbe33be289b1d89d3eff01c95b1`

```mermaid
classDiagram
    class struct_Consultation {
      <<struct>>
      +"id: String"
      +"consultation_type: ConsultationType"
      +"provider_id: String"
      +"patient_id: String"
      +"start_time: SystemTime"
      +"duration_minutes: Option~u32~"
      +"status: ConsultationStatus"
      +"encrypted: bool"
    }
    class enum_ConsultationType {
      <<enum>>
    }
    class enum_ConsultationStatus {
      <<enum>>
    }
    class struct_EPrescription {
      <<struct>>
      +"id: String"
      +"consultation_id: String"
      +"medication_ndc: String"
      +"dosage: String"
      +"frequency: String"
      +"quantity: u32"
      +"pharmacy_id: String"
    }
    class struct_Appointment {
      <<struct>>
      +"id: String"
      +"provider_id: String"
      +"patient_id: String"
      +"scheduled_time: SystemTime"
      +"duration_minutes: u32"
      +"reminder_sent: bool"
    }
    class struct_TelemedicinePlatform {
      <<struct>>
      +"consultations: Vec~Consultation~"
      +"appointments: Vec~Appointment~"
      +"prescriptions: Vec~EPrescription~"
    }
    class mod_tests {
      <<mod>>
    }
    note "TelemedicinePlatform"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::time::SystemTime`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
