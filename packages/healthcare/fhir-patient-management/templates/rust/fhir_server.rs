// FHIR R4 Patient Management Server
// Implements FHIR REST API with validation and search capabilities

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

// ============================================================================
// FHIR Resource Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Patient {
    #[serde(rename = "resourceType")]
    pub resource_type: String, // "Patient"
    pub id: Option<String>,
    pub identifier: Vec<Identifier>,
    pub active: Option<bool>,
    pub name: Vec<HumanName>,
    pub telecom: Option<Vec<ContactPoint>>,
    pub gender: Option<String>, // male | female | other | unknown
    #[serde(rename = "birthDate")]
    pub birth_date: Option<String>,
    pub deceased: Option<Deceased>,
    pub address: Option<Vec<Address>>,
    #[serde(rename = "maritalStatus")]
    pub marital_status: Option<CodeableConcept>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Observation {
    #[serde(rename = "resourceType")]
    pub resource_type: String, // "Observation"
    pub id: Option<String>,
    pub status: String, // registered | preliminary | final | amended
    pub category: Option<Vec<CodeableConcept>>,
    pub code: CodeableConcept,
    pub subject: Reference,
    #[serde(rename = "effectiveDateTime")]
    pub effective_date_time: Option<String>,
    pub value: Option<ObservationValue>,
    pub interpretation: Option<Vec<CodeableConcept>>,
    #[serde(rename = "referenceRange")]
    pub reference_range: Option<Vec<ReferenceRange>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MedicationRequest {
    #[serde(rename = "resourceType")]
    pub resource_type: String, // "MedicationRequest"
    pub id: Option<String>,
    pub status: String,
    pub intent: String,
    #[serde(rename = "medicationCodeableConcept")]
    pub medication: CodeableConcept,
    pub subject: Reference,
    #[serde(rename = "authoredOn")]
    pub authored_on: Option<String>,
    pub requester: Option<Reference>,
    #[serde(rename = "dosageInstruction")]
    pub dosage_instruction: Option<Vec<Dosage>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Condition {
    #[serde(rename = "resourceType")]
    pub resource_type: String, // "Condition"
    pub id: Option<String>,
    #[serde(rename = "clinicalStatus")]
    pub clinical_status: CodeableConcept,
    #[serde(rename = "verificationStatus")]
    pub verification_status: Option<CodeableConcept>,
    pub severity: Option<CodeableConcept>,
    pub code: CodeableConcept,
    pub subject: Reference,
    #[serde(rename = "onsetDateTime")]
    pub onset_date_time: Option<String>,
}

// ============================================================================
// FHIR Data Types
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Identifier {
    pub use_: Option<String>,
    pub system: Option<String>,
    pub value: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HumanName {
    pub use_: Option<String>,
    pub text: Option<String>,
    pub family: Option<String>,
    pub given: Option<Vec<String>>,
    pub prefix: Option<Vec<String>>,
    pub suffix: Option<Vec<String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContactPoint {
    pub system: Option<String>, // phone | fax | email | pager | url | sms
    pub value: Option<String>,
    pub use_: Option<String>, // home | work | temp | old | mobile
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Address {
    pub use_: Option<String>,
    pub line: Option<Vec<String>>,
    pub city: Option<String>,
    pub state: Option<String>,
    #[serde(rename = "postalCode")]
    pub postal_code: Option<String>,
    pub country: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeableConcept {
    pub coding: Option<Vec<Coding>>,
    pub text: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Coding {
    pub system: Option<String>,
    pub code: Option<String>,
    pub display: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Reference {
    pub reference: Option<String>,
    pub display: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Deceased {
    Boolean(bool),
    DateTime(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum ObservationValue {
    Quantity(Quantity),
    CodeableConcept(CodeableConcept),
    String(String),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Quantity {
    pub value: Option<f64>,
    pub unit: Option<String>,
    pub system: Option<String>,
    pub code: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReferenceRange {
    pub low: Option<Quantity>,
    pub high: Option<Quantity>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dosage {
    pub text: Option<String>,
    pub timing: Option<Timing>,
    #[serde(rename = "doseAndRate")]
    pub dose_and_rate: Option<Vec<DoseAndRate>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Timing {
    pub repeat: Option<TimingRepeat>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimingRepeat {
    pub frequency: Option<i32>,
    pub period: Option<f64>,
    #[serde(rename = "periodUnit")]
    pub period_unit: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DoseAndRate {
    #[serde(rename = "doseQuantity")]
    pub dose_quantity: Option<Quantity>,
}

// ============================================================================
// FHIR Bundle
// ============================================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bundle {
    #[serde(rename = "resourceType")]
    pub resource_type: String, // "Bundle"
    pub type_: String, // searchset | collection | transaction | batch
    pub total: Option<u32>,
    pub entry: Vec<BundleEntry>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BundleEntry {
    pub resource: serde_json::Value,
}

// ============================================================================
// FHIR Server
// ============================================================================

pub struct FHIRServer {
    patients: Arc<RwLock<HashMap<String, Patient>>>,
    observations: Arc<RwLock<HashMap<String, Observation>>>,
    medications: Arc<RwLock<HashMap<String, MedicationRequest>>>,
    conditions: Arc<RwLock<HashMap<String, Condition>>>,
}

impl FHIRServer {
    pub fn new() -> Self {
        Self {
            patients: Arc::new(RwLock::new(HashMap::new())),
            observations: Arc::new(RwLock::new(HashMap::new())),
            medications: Arc::new(RwLock::new(HashMap::new())),
            conditions: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    // Create Patient
    pub fn create_patient(&self, mut patient: Patient) -> Result<Patient, String> {
        self.validate_patient(&patient)?;

        let id = uuid::Uuid::new_v4().to_string();
        patient.id = Some(id.clone());
        patient.resource_type = "Patient".to_string();

        let mut patients = self.patients.write().unwrap();
        patients.insert(id.clone(), patient.clone());

        Ok(patient)
    }

    // Read Patient
    pub fn read_patient(&self, id: &str) -> Result<Patient, String> {
        let patients = self.patients.read().unwrap();
        patients.get(id)
            .cloned()
            .ok_or_else(|| format!("Patient {} not found", id))
    }

    // Update Patient
    pub fn update_patient(&self, id: &str, mut patient: Patient) -> Result<Patient, String> {
        self.validate_patient(&patient)?;

        let mut patients = self.patients.write().unwrap();
        if !patients.contains_key(id) {
            return Err(format!("Patient {} not found", id));
        }

        patient.id = Some(id.to_string());
        patients.insert(id.to_string(), patient.clone());

        Ok(patient)
    }

    // Delete Patient
    pub fn delete_patient(&self, id: &str) -> Result<(), String> {
        let mut patients = self.patients.write().unwrap();
        patients.remove(id)
            .ok_or_else(|| format!("Patient {} not found", id))?;
        Ok(())
    }

    // Search Patients
    pub fn search_patients(&self, params: &HashMap<String, String>) -> Bundle {
        let patients = self.patients.read().unwrap();
        let mut results: Vec<Patient> = patients.values().cloned().collect();

        // Filter by name
        if let Some(name) = params.get("name") {
            results.retain(|p| {
                p.name.iter().any(|n| {
                    n.family.as_ref().map(|f| f.to_lowercase().contains(&name.to_lowercase())).unwrap_or(false) ||
                    n.given.as_ref().map(|g| g.iter().any(|gn| gn.to_lowercase().contains(&name.to_lowercase()))).unwrap_or(false)
                })
            });
        }

        // Filter by identifier
        if let Some(identifier) = params.get("identifier") {
            results.retain(|p| {
                p.identifier.iter().any(|i| i.value == *identifier)
            });
        }

        // Filter by birthdate
        if let Some(birthdate) = params.get("birthdate") {
            results.retain(|p| p.birth_date.as_ref() == Some(birthdate));
        }

        Bundle {
            resource_type: "Bundle".to_string(),
            type_: "searchset".to_string(),
            total: Some(results.len() as u32),
            entry: results.into_iter()
                .map(|p| BundleEntry {
                    resource: serde_json::to_value(&p).unwrap(),
                })
                .collect(),
        }
    }

    // Validate Patient
    fn validate_patient(&self, patient: &Patient) -> Result<(), String> {
        if patient.identifier.is_empty() {
            return Err("Patient must have at least one identifier".to_string());
        }

        if patient.name.is_empty() {
            return Err("Patient must have at least one name".to_string());
        }

        if let Some(gender) = &patient.gender {
            if !["male", "female", "other", "unknown"].contains(&gender.as_str()) {
                return Err("Invalid gender value".to_string());
            }
        }

        Ok(())
    }

    // Create Observation
    pub fn create_observation(&self, mut observation: Observation) -> Result<Observation, String> {
        self.validate_observation(&observation)?;

        let id = uuid::Uuid::new_v4().to_string();
        observation.id = Some(id.clone());
        observation.resource_type = "Observation".to_string();

        let mut observations = self.observations.write().unwrap();
        observations.insert(id.clone(), observation.clone());

        Ok(observation)
    }

    fn validate_observation(&self, observation: &Observation) -> Result<(), String> {
        let valid_statuses = ["registered", "preliminary", "final", "amended", "corrected", "cancelled", "entered-in-error", "unknown"];
        if !valid_statuses.contains(&observation.status.as_str()) {
            return Err(format!("Invalid observation status: {}", observation.status));
        }

        // Validate subject reference points to a patient
        if let Some(ref_str) = &observation.subject.reference {
            if !ref_str.starts_with("Patient/") {
                return Err("Observation subject must reference a Patient".to_string());
            }
        }

        Ok(())
    }

    // Search Observations
    pub fn search_observations(&self, params: &HashMap<String, String>) -> Bundle {
        let observations = self.observations.read().unwrap();
        let mut results: Vec<Observation> = observations.values().cloned().collect();

        // Filter by patient
        if let Some(patient) = params.get("patient") {
            let patient_ref = format!("Patient/{}", patient);
            results.retain(|o| o.subject.reference.as_ref() == Some(&patient_ref));
        }

        // Filter by code
        if let Some(code) = params.get("code") {
            results.retain(|o| {
                o.code.coding.as_ref()
                    .map(|codings| codings.iter().any(|c| c.code.as_ref() == Some(code)))
                    .unwrap_or(false)
            });
        }

        Bundle {
            resource_type: "Bundle".to_string(),
            type_: "searchset".to_string(),
            total: Some(results.len() as u32),
            entry: results.into_iter()
                .map(|o| BundleEntry {
                    resource: serde_json::to_value(&o).unwrap(),
                })
                .collect(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_patient() {
        let server = FHIRServer::new();

        let patient = Patient {
            resource_type: "Patient".to_string(),
            id: None,
            identifier: vec![Identifier {
                use_: Some("official".to_string()),
                system: Some("http://hospital.org/mrn".to_string()),
                value: "12345".to_string(),
            }],
            active: Some(true),
            name: vec![HumanName {
                use_: Some("official".to_string()),
                text: Some("John Doe".to_string()),
                family: Some("Doe".to_string()),
                given: Some(vec!["John".to_string()]),
                prefix: None,
                suffix: None,
            }],
            telecom: None,
            gender: Some("male".to_string()),
            birth_date: Some("1980-01-01".to_string()),
            deceased: None,
            address: None,
            marital_status: None,
        };

        let result = server.create_patient(patient);
        assert!(result.is_ok());
        assert!(result.unwrap().id.is_some());
    }

    #[test]
    fn test_validate_patient_missing_identifier() {
        let server = FHIRServer::new();

        let patient = Patient {
            resource_type: "Patient".to_string(),
            id: None,
            identifier: vec![],
            active: Some(true),
            name: vec![HumanName {
                use_: Some("official".to_string()),
                text: Some("John Doe".to_string()),
                family: Some("Doe".to_string()),
                given: Some(vec!["John".to_string()]),
                prefix: None,
                suffix: None,
            }],
            telecom: None,
            gender: Some("male".to_string()),
            birth_date: Some("1980-01-01".to_string()),
            deceased: None,
            address: None,
            marital_status: None,
        };

        let result = server.create_patient(patient);
        assert!(result.is_err());
        assert_eq!(result.err().unwrap(), "Patient must have at least one identifier");
    }
}
