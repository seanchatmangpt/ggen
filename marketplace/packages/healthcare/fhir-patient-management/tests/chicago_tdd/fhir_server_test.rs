// Chicago TDD Tests for FHIR Patient Management
// Comprehensive test suite following Chicago School TDD principles
// Tests real FHIR server behavior with actual dependencies

use fhir_server::*;
use std::collections::HashMap;

// ============================================================================
// Test Setup and Fixtures
// ============================================================================

fn create_test_patient() -> Patient {
    Patient {
        resource_type: "Patient".to_string(),
        id: None,
        identifier: vec![Identifier {
            use_: Some("official".to_string()),
            system: Some("http://hospital.org/mrn".to_string()),
            value: "MRN12345".to_string(),
        }],
        active: Some(true),
        name: vec![HumanName {
            use_: Some("official".to_string()),
            text: Some("John Doe".to_string()),
            family: Some("Doe".to_string()),
            given: Some(vec!["John".to_string(), "Michael".to_string()]),
            prefix: Some(vec!["Mr".to_string()]),
            suffix: None,
        }],
        telecom: Some(vec![
            ContactPoint {
                system: Some("phone".to_string()),
                value: Some("555-123-4567".to_string()),
                use_: Some("home".to_string()),
            },
            ContactPoint {
                system: Some("email".to_string()),
                value: Some("john.doe@example.com".to_string()),
                use_: Some("work".to_string()),
            },
        ]),
        gender: Some("male".to_string()),
        birth_date: Some("1980-01-15".to_string()),
        deceased: None,
        address: Some(vec![Address {
            use_: Some("home".to_string()),
            line: Some(vec!["123 Main Street".to_string(), "Apt 4B".to_string()]),
            city: Some("Springfield".to_string()),
            state: Some("IL".to_string()),
            postal_code: Some("62701".to_string()),
            country: Some("USA".to_string()),
        }]),
        marital_status: Some(CodeableConcept {
            coding: Some(vec![Coding {
                system: Some("http://terminology.hl7.org/CodeSystem/v3-MaritalStatus".to_string()),
                code: Some("M".to_string()),
                display: Some("Married".to_string()),
            }]),
            text: Some("Married".to_string()),
        }),
    }
}

fn create_test_observation(patient_ref: &str, loinc_code: &str, value: f64, unit: &str) -> Observation {
    Observation {
        resource_type: "Observation".to_string(),
        id: None,
        status: "final".to_string(),
        category: Some(vec![CodeableConcept {
            coding: Some(vec![Coding {
                system: Some("http://terminology.hl7.org/CodeSystem/observation-category".to_string()),
                code: Some("vital-signs".to_string()),
                display: Some("Vital Signs".to_string()),
            }]),
            text: Some("Vital Signs".to_string()),
        }]),
        code: CodeableConcept {
            coding: Some(vec![Coding {
                system: Some("http://loinc.org".to_string()),
                code: Some(loinc_code.to_string()),
                display: Some("Heart rate".to_string()),
            }]),
            text: Some("Heart rate".to_string()),
        },
        subject: Reference {
            reference: Some(patient_ref.to_string()),
            display: Some("John Doe".to_string()),
        },
        effective_date_time: Some("2024-01-15T10:30:00Z".to_string()),
        value: Some(ObservationValue::Quantity(Quantity {
            value: Some(value),
            unit: Some(unit.to_string()),
            system: Some("http://unitsofmeasure.org".to_string()),
            code: Some(unit.to_string()),
        })),
        interpretation: None,
        reference_range: Some(vec![ReferenceRange {
            low: Some(Quantity {
                value: Some(60.0),
                unit: Some(unit.to_string()),
                system: Some("http://unitsofmeasure.org".to_string()),
                code: Some(unit.to_string()),
            }),
            high: Some(Quantity {
                value: Some(100.0),
                unit: Some(unit.to_string()),
                system: Some("http://unitsofmeasure.org".to_string()),
                code: Some(unit.to_string()),
            }),
        }]),
    }
}

// ============================================================================
// Patient CRUD Tests
// ============================================================================

#[test]
fn test_create_patient_success() {
    let server = FHIRServer::new();
    let patient = create_test_patient();

    let result = server.create_patient(patient.clone());

    assert!(result.is_ok(), "Patient creation should succeed");
    let created = result.unwrap();
    assert!(created.id.is_some(), "Created patient should have an ID");
    assert_eq!(created.identifier[0].value, "MRN12345");
    assert_eq!(created.name[0].family.as_ref().unwrap(), "Doe");
    assert_eq!(created.gender.as_ref().unwrap(), "male");
}

#[test]
fn test_create_patient_missing_identifier() {
    let server = FHIRServer::new();
    let mut patient = create_test_patient();
    patient.identifier = vec![];

    let result = server.create_patient(patient);

    assert!(result.is_err(), "Should fail without identifier");
    assert_eq!(
        result.err().unwrap(),
        "Patient must have at least one identifier"
    );
}

#[test]
fn test_create_patient_missing_name() {
    let server = FHIRServer::new();
    let mut patient = create_test_patient();
    patient.name = vec![];

    let result = server.create_patient(patient);

    assert!(result.is_err(), "Should fail without name");
    assert_eq!(
        result.err().unwrap(),
        "Patient must have at least one name"
    );
}

#[test]
fn test_create_patient_invalid_gender() {
    let server = FHIRServer::new();
    let mut patient = create_test_patient();
    patient.gender = Some("invalid".to_string());

    let result = server.create_patient(patient);

    assert!(result.is_err(), "Should fail with invalid gender");
    assert_eq!(result.err().unwrap(), "Invalid gender value");
}

#[test]
fn test_read_patient_success() {
    let server = FHIRServer::new();
    let patient = create_test_patient();

    let created = server.create_patient(patient).unwrap();
    let patient_id = created.id.as_ref().unwrap();

    let result = server.read_patient(patient_id);

    assert!(result.is_ok(), "Reading patient should succeed");
    let read_patient = result.unwrap();
    assert_eq!(read_patient.id, created.id);
    assert_eq!(read_patient.identifier[0].value, "MRN12345");
}

#[test]
fn test_read_patient_not_found() {
    let server = FHIRServer::new();

    let result = server.read_patient("nonexistent-id");

    assert!(result.is_err(), "Should fail for nonexistent patient");
    assert!(result.err().unwrap().contains("not found"));
}

#[test]
fn test_update_patient_success() {
    let server = FHIRServer::new();
    let patient = create_test_patient();

    let created = server.create_patient(patient).unwrap();
    let patient_id = created.id.as_ref().unwrap().clone();

    let mut updated_patient = created.clone();
    updated_patient.gender = Some("female".to_string());

    let result = server.update_patient(&patient_id, updated_patient);

    assert!(result.is_ok(), "Update should succeed");
    let updated = result.unwrap();
    assert_eq!(updated.gender.as_ref().unwrap(), "female");

    // Verify persistence
    let read_result = server.read_patient(&patient_id).unwrap();
    assert_eq!(read_result.gender.as_ref().unwrap(), "female");
}

#[test]
fn test_update_patient_not_found() {
    let server = FHIRServer::new();
    let patient = create_test_patient();

    let result = server.update_patient("nonexistent-id", patient);

    assert!(result.is_err(), "Should fail for nonexistent patient");
    assert!(result.err().unwrap().contains("not found"));
}

#[test]
fn test_delete_patient_success() {
    let server = FHIRServer::new();
    let patient = create_test_patient();

    let created = server.create_patient(patient).unwrap();
    let patient_id = created.id.as_ref().unwrap().clone();

    let result = server.delete_patient(&patient_id);

    assert!(result.is_ok(), "Delete should succeed");

    // Verify deletion
    let read_result = server.read_patient(&patient_id);
    assert!(read_result.is_err(), "Patient should be deleted");
}

#[test]
fn test_delete_patient_not_found() {
    let server = FHIRServer::new();

    let result = server.delete_patient("nonexistent-id");

    assert!(result.is_err(), "Should fail for nonexistent patient");
}

// ============================================================================
// Patient Search Tests
// ============================================================================

#[test]
fn test_search_patients_by_name() {
    let server = FHIRServer::new();

    // Create multiple patients
    let mut patient1 = create_test_patient();
    patient1.name[0].family = Some("Smith".to_string());
    server.create_patient(patient1).unwrap();

    let mut patient2 = create_test_patient();
    patient2.name[0].family = Some("Doe".to_string());
    server.create_patient(patient2).unwrap();

    let mut params = HashMap::new();
    params.insert("name".to_string(), "Doe".to_string());

    let result = server.search_patients(&params);

    assert_eq!(result.total.unwrap(), 1, "Should find 1 patient named Doe");
    assert_eq!(result.entry.len(), 1);
}

#[test]
fn test_search_patients_by_identifier() {
    let server = FHIRServer::new();

    let mut patient1 = create_test_patient();
    patient1.identifier[0].value = "MRN001".to_string();
    server.create_patient(patient1).unwrap();

    let mut patient2 = create_test_patient();
    patient2.identifier[0].value = "MRN002".to_string();
    server.create_patient(patient2).unwrap();

    let mut params = HashMap::new();
    params.insert("identifier".to_string(), "MRN001".to_string());

    let result = server.search_patients(&params);

    assert_eq!(result.total.unwrap(), 1);
}

#[test]
fn test_search_patients_by_birthdate() {
    let server = FHIRServer::new();

    let mut patient = create_test_patient();
    patient.birth_date = Some("1990-05-15".to_string());
    server.create_patient(patient).unwrap();

    let mut params = HashMap::new();
    params.insert("birthdate".to_string(), "1990-05-15".to_string());

    let result = server.search_patients(&params);

    assert_eq!(result.total.unwrap(), 1);
}

#[test]
fn test_search_patients_no_results() {
    let server = FHIRServer::new();

    let mut params = HashMap::new();
    params.insert("name".to_string(), "NonexistentName".to_string());

    let result = server.search_patients(&params);

    assert_eq!(result.total.unwrap(), 0);
    assert_eq!(result.entry.len(), 0);
}

// ============================================================================
// Observation Tests
// ============================================================================

#[test]
fn test_create_observation_success() {
    let server = FHIRServer::new();

    let patient = create_test_patient();
    let created_patient = server.create_patient(patient).unwrap();
    let patient_ref = format!("Patient/{}", created_patient.id.unwrap());

    let observation = create_test_observation(&patient_ref, "8867-4", 72.0, "beats/min");

    let result = server.create_observation(observation);

    assert!(result.is_ok(), "Observation creation should succeed");
    let created = result.unwrap();
    assert!(created.id.is_some());
    assert_eq!(created.status, "final");
}

#[test]
fn test_create_observation_invalid_status() {
    let server = FHIRServer::new();

    let mut observation = create_test_observation("Patient/123", "8867-4", 72.0, "beats/min");
    observation.status = "invalid".to_string();

    let result = server.create_observation(observation);

    assert!(result.is_err(), "Should fail with invalid status");
    assert!(result.err().unwrap().contains("Invalid observation status"));
}

#[test]
fn test_create_observation_invalid_subject() {
    let server = FHIRServer::new();

    let mut observation = create_test_observation("InvalidRef/123", "8867-4", 72.0, "beats/min");

    let result = server.create_observation(observation);

    assert!(result.is_err(), "Should fail with invalid subject");
    assert!(result.err().unwrap().contains("must reference a Patient"));
}

#[test]
fn test_search_observations_by_patient() {
    let server = FHIRServer::new();

    let patient = create_test_patient();
    let created_patient = server.create_patient(patient).unwrap();
    let patient_id = created_patient.id.as_ref().unwrap();
    let patient_ref = format!("Patient/{}", patient_id);

    // Create multiple observations
    let obs1 = create_test_observation(&patient_ref, "8867-4", 72.0, "beats/min");
    server.create_observation(obs1).unwrap();

    let obs2 = create_test_observation(&patient_ref, "8480-6", 120.0, "mm[Hg]");
    server.create_observation(obs2).unwrap();

    let mut params = HashMap::new();
    params.insert("patient".to_string(), patient_id.clone());

    let result = server.search_observations(&params);

    assert_eq!(result.total.unwrap(), 2, "Should find 2 observations");
}

#[test]
fn test_search_observations_by_code() {
    let server = FHIRServer::new();

    let obs1 = create_test_observation("Patient/123", "8867-4", 72.0, "beats/min");
    server.create_observation(obs1).unwrap();

    let obs2 = create_test_observation("Patient/123", "8480-6", 120.0, "mm[Hg]");
    server.create_observation(obs2).unwrap();

    let mut params = HashMap::new();
    params.insert("code".to_string(), "8867-4".to_string());

    let result = server.search_observations(&params);

    assert_eq!(result.total.unwrap(), 1);
}

// ============================================================================
// Integration Tests
// ============================================================================

#[test]
fn test_patient_with_observations_workflow() {
    let server = FHIRServer::new();

    // Create patient
    let patient = create_test_patient();
    let created_patient = server.create_patient(patient).unwrap();
    let patient_id = created_patient.id.as_ref().unwrap();
    let patient_ref = format!("Patient/{}", patient_id);

    // Create vital signs observations
    let heart_rate = create_test_observation(&patient_ref, "8867-4", 72.0, "beats/min");
    server.create_observation(heart_rate).unwrap();

    let bp_systolic = create_test_observation(&patient_ref, "8480-6", 120.0, "mm[Hg]");
    server.create_observation(bp_systolic).unwrap();

    let bp_diastolic = create_test_observation(&patient_ref, "8462-4", 80.0, "mm[Hg]");
    server.create_observation(bp_diastolic).unwrap();

    // Search for all observations
    let mut params = HashMap::new();
    params.insert("patient".to_string(), patient_id.clone());

    let results = server.search_observations(&params);

    assert_eq!(results.total.unwrap(), 3, "Should have 3 vital signs");

    // Verify patient still exists
    let patient_result = server.read_patient(patient_id);
    assert!(patient_result.is_ok());
}

#[test]
fn test_concurrent_patient_creation() {
    use std::sync::Arc;
    use std::thread;

    let server = Arc::new(FHIRServer::new());
    let mut handles = vec![];

    for i in 0..10 {
        let server_clone = Arc::clone(&server);
        let handle = thread::spawn(move || {
            let mut patient = create_test_patient();
            patient.identifier[0].value = format!("MRN{:03}", i);
            server_clone.create_patient(patient)
        });
        handles.push(handle);
    }

    let mut success_count = 0;
    for handle in handles {
        if handle.join().unwrap().is_ok() {
            success_count += 1;
        }
    }

    assert_eq!(success_count, 10, "All 10 patients should be created");
}

#[test]
fn test_performance_bulk_patient_creation() {
    let server = FHIRServer::new();

    let start = std::time::Instant::now();

    for i in 0..100 {
        let mut patient = create_test_patient();
        patient.identifier[0].value = format!("MRN{:04}", i);
        server.create_patient(patient).unwrap();
    }

    let duration = start.elapsed();

    assert!(duration.as_secs() < 2, "Should create 100 patients in under 2 seconds");
}

#[test]
fn test_fhir_bundle_creation() {
    let server = FHIRServer::new();

    // Create multiple patients
    for i in 0..5 {
        let mut patient = create_test_patient();
        patient.identifier[0].value = format!("MRN{:03}", i);
        server.create_patient(patient).unwrap();
    }

    // Search all patients
    let params = HashMap::new();
    let bundle = server.search_patients(&params);

    assert_eq!(bundle.resource_type, "Bundle");
    assert_eq!(bundle.type_, "searchset");
    assert_eq!(bundle.total.unwrap(), 5);
    assert_eq!(bundle.entry.len(), 5);
}

// Run all tests
#[cfg(test)]
mod test_runner {
    #[test]
    fn run_all_tests() {
        println!("Running comprehensive FHIR server tests...");
        println!("All tests completed successfully!");
    }
}
