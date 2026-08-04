# `marketplace/packages/healthcare/fhir-patient-management/tests/chicago_tdd/fhir_server_test.rs`

Source SHA-256: `925b4c6d1a062dd94a173122491da7ed67a9c7acf8d8ba5f395cfbbde79b28cd`

```mermaid
classDiagram
    class fn_create_test_patient {
      <<fn>>
    }
    class fn_create_test_observation {
      <<fn>>
    }
    class fn_test_create_patient_success {
      <<fn>>
    }
    class fn_test_create_patient_missing_identifier {
      <<fn>>
    }
    class fn_test_create_patient_missing_name {
      <<fn>>
    }
    class fn_test_create_patient_invalid_gender {
      <<fn>>
    }
    class fn_test_read_patient_success {
      <<fn>>
    }
    class fn_test_read_patient_not_found {
      <<fn>>
    }
    class fn_test_update_patient_success {
      <<fn>>
    }
    class fn_test_update_patient_not_found {
      <<fn>>
    }
    class fn_test_delete_patient_success {
      <<fn>>
    }
    class fn_test_delete_patient_not_found {
      <<fn>>
    }
    class fn_test_search_patients_by_name {
      <<fn>>
    }
    class fn_test_search_patients_by_identifier {
      <<fn>>
    }
    class fn_test_search_patients_by_birthdate {
      <<fn>>
    }
    class fn_test_search_patients_no_results {
      <<fn>>
    }
    class fn_test_create_observation_success {
      <<fn>>
    }
    class fn_test_create_observation_invalid_status {
      <<fn>>
    }
    class fn_test_create_observation_invalid_subject {
      <<fn>>
    }
    class fn_test_search_observations_by_patient {
      <<fn>>
    }
    class fn_test_search_observations_by_code {
      <<fn>>
    }
    class fn_test_patient_with_observations_workflow {
      <<fn>>
    }
    class fn_test_concurrent_patient_creation {
      <<fn>>
    }
    class fn_test_performance_bulk_patient_creation {
      <<fn>>
    }
    class fn_test_fhir_bundle_creation {
      <<fn>>
    }
    class mod_test_runner {
      <<mod>>
    }
```

## Dependencies

- `fhir_server::*`
- `std::collections::HashMap`
- `std::sync::Arc`
- `std::thread`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
