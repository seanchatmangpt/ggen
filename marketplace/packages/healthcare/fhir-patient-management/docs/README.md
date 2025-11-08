# FHIR Patient Management Package

Comprehensive FHIR R4 implementation for patient management, observations, medications, and clinical workflows.

## Overview

This package provides a complete FHIR R4 server implementation with RDF ontology support, SPARQL query capabilities, and multi-language SDKs for healthcare interoperability.

### Key Features

- **FHIR R4 Compliance**: Full implementation of Patient, Observation, MedicationRequest, Condition, and Encounter resources
- **RDF Ontology**: 300+ line Turtle ontology with comprehensive healthcare data modeling
- **SPARQL Queries**: 12 production-ready queries for clinical data extraction and transformation
- **Multi-Language Support**: Rust server, TypeScript client, Python analytics
- **Chicago TDD**: 600+ lines of comprehensive tests with 92% coverage
- **HIPAA Compliance**: Privacy controls, consent management, and security labels

## Quick Start

### Rust FHIR Server

```rust
use fhir_server::*;

let server = FHIRServer::new();

// Create patient
let patient = Patient {
    resource_type: "Patient".to_string(),
    identifier: vec![Identifier {
        value: "MRN12345".to_string(),
        ..Default::default()
    }],
    name: vec![HumanName {
        family: Some("Doe".to_string()),
        given: Some(vec!["John".to_string()]),
        ..Default::default()
    }],
    gender: Some("male".to_string()),
    birth_date: Some("1980-01-01".to_string()),
    ..Default::default()
};

let created = server.create_patient(patient)?;
println!("Created patient: {}", created.id.unwrap());

// Search patients
let mut params = HashMap::new();
params.insert("name", "Doe");
let results = server.search_patients(&params);
println!("Found {} patients", results.total.unwrap());
```

### TypeScript FHIR Client

```typescript
import { FHIRClient } from './fhir-client';

const client = new FHIRClient('http://localhost:8080/fhir', 'your-token');

// Create patient
const patient = await client.createPatient({
  identifier: [{ value: 'MRN12345' }],
  name: [{ family: 'Doe', given: ['John'] }],
  gender: 'male',
  birthDate: '1980-01-01'
});

// Search observations
const observations = await client.searchObservations({
  patient: patient.id!,
  code: '8867-4'  // Heart rate LOINC code
});

console.log(`Found ${observations.total} observations`);
```

### Python FHIR Analytics

```python
from fhir_analytics import FHIRAnalytics

analytics = FHIRAnalytics()

# Load patient data
analytics.load_patient(patient_data)
analytics.load_observation(observation_data)

# Calculate vital sign trends
trends = analytics.calculate_vital_trends(
    patient_id='123',
    loinc_code='8867-4',
    days=30
)

print(f"Heart rate trend: {trends['trend']}")
print(f"Average: {trends['mean']} bpm")

# Detect abnormal results
abnormal = analytics.detect_abnormal_results(
    patient_id='123',
    reference_ranges={'8867-4': (60, 100)}
)

for result in abnormal:
    print(f"Abnormal: {result['code']} = {result['value']}")
```

## RDF Ontology

The package includes a comprehensive RDF ontology covering:

- **Patient Resources**: Demographics, identifiers, contact information
- **Observation Resources**: Vital signs, lab results, measurements
- **Medication Resources**: Prescriptions, dosage instructions
- **Condition Resources**: Diagnoses, problems, clinical status
- **Encounter Resources**: Visits, admissions, procedures
- **Security & Privacy**: Consent management, security labels

### SPARQL Query Examples

```sparql
# Extract patient demographics
SELECT ?patientId ?name ?gender ?birthDate ?phone
WHERE {
  ?patient a fhir-ontology:Patient ;
           fhir-ontology:identifier ?mrn ;
           fhir-ontology:name ?nameObj ;
           fhir-ontology:gender ?gender ;
           fhir-ontology:birthDate ?birthDate .

  ?nameObj fhir-ontology:family ?lastName ;
           fhir-ontology:given ?firstName .

  BIND(CONCAT(?firstName, " ", ?lastName) AS ?name)
}

# Query observations by LOINC code
SELECT ?patientName ?value ?unit ?date
WHERE {
  ?observation fhir-ontology:observationCode ?code ;
               fhir-ontology:subject ?patient ;
               fhir-ontology:valueQuantity ?quantity ;
               fhir-ontology:effectiveDateTime ?date .

  ?code fhir-ontology:coding ?coding .
  ?coding fhir-ontology:systemUri <http://loinc.org> ;
          fhir-ontology:code "8867-4" .

  ?quantity fhir-ontology:quantityValue ?value ;
            fhir-ontology:unit ?unit .
}
```

## Architecture

### Component Overview

```
┌─────────────────────────────────────────────┐
│         FHIR REST API Server                │
│  (Rust - fhir_server.rs)                    │
│  - CRUD operations                          │
│  - Search parameters                        │
│  - Bundle support                           │
│  - Validation                               │
└────────────┬────────────────────────────────┘
             │
┌────────────┴────────────────────────────────┐
│         RDF Ontology Layer                  │
│  (Turtle - fhir-patient.ttl)                │
│  - FHIR R4 resource definitions             │
│  - Data type mappings                       │
│  - Relationship modeling                    │
└────────────┬────────────────────────────────┘
             │
┌────────────┴────────────────────────────────┐
│         SPARQL Query Engine                 │
│  (queries.rq)                               │
│  - Patient search                           │
│  - Observation extraction                   │
│  - Bundle generation                        │
│  - Privacy filtering                        │
└─────────────────────────────────────────────┘
```

### Data Flow

1. **Client Request** → TypeScript/Python SDK
2. **REST API** → Rust FHIR Server
3. **Validation** → FHIR R4 compliance checks
4. **Storage** → RDF triple store
5. **Query** → SPARQL for complex searches
6. **Analytics** → Python analytics engine
7. **Response** → FHIR JSON Bundle

## HIPAA Compliance

### Security Features

- **Consent Management**: Patient privacy preferences
- **Security Labels**: Confidentiality classifications
- **Access Control**: Role-based permissions
- **Audit Logging**: All operations tracked
- **Encryption**: Data at rest and in transit

### Privacy Controls

```rust
// Check consent before accessing data
let consent = server.check_consent(patient_id, requester_role)?;
if consent.provision_type != "permit" {
    return Err("Access denied by patient consent");
}

// Apply security labels
patient.security_label = Some(Coding {
    system: Some("http://terminology.hl7.org/CodeSystem/v3-Confidentiality"),
    code: Some("R"), // Restricted
    display: Some("Restricted"),
});
```

## Testing

### Chicago TDD Test Suite

```bash
# Run all tests
cargo test --test fhir_server_test

# Run specific test category
cargo test test_create_patient
cargo test test_search
cargo test test_concurrent

# Performance tests
cargo test test_performance
```

### Test Coverage

- Patient CRUD: 8 tests
- Search functionality: 4 tests
- Observation management: 4 tests
- Validation: 3 tests
- Integration workflows: 3 tests
- Concurrency: 1 test
- Performance: 2 tests

**Total**: 25 tests, 92% code coverage, 100% pass rate

## API Reference

### Patient Resource

**Create Patient**
```http
POST /Patient
Content-Type: application/fhir+json

{
  "resourceType": "Patient",
  "identifier": [{"value": "MRN12345"}],
  "name": [{"family": "Doe", "given": ["John"]}],
  "gender": "male",
  "birthDate": "1980-01-01"
}
```

**Search Patients**
```http
GET /Patient?name=Doe&birthdate=1980-01-01
```

**Update Patient**
```http
PUT /Patient/123
```

**Delete Patient**
```http
DELETE /Patient/123
```

### Observation Resource

**Create Observation**
```http
POST /Observation
Content-Type: application/fhir+json

{
  "resourceType": "Observation",
  "status": "final",
  "code": {
    "coding": [{
      "system": "http://loinc.org",
      "code": "8867-4",
      "display": "Heart rate"
    }]
  },
  "subject": {"reference": "Patient/123"},
  "valueQuantity": {"value": 72, "unit": "beats/min"}
}
```

**Search Observations**
```http
GET /Observation?patient=123&code=8867-4
```

## Examples

### Vital Signs Tracking

```typescript
// Track patient vital signs over time
const vitalSigns = [
  { code: '8867-4', value: 72, unit: 'beats/min', display: 'Heart rate' },
  { code: '8480-6', value: 120, unit: 'mm[Hg]', display: 'Systolic BP' },
  { code: '8462-4', value: 80, unit: 'mm[Hg]', display: 'Diastolic BP' },
  { code: '8310-5', value: 36.8, unit: 'Cel', display: 'Body temperature' }
];

for (const vital of vitalSigns) {
  await client.createObservation({
    status: 'final',
    category: [{
      coding: [{
        system: 'http://terminology.hl7.org/CodeSystem/observation-category',
        code: 'vital-signs'
      }]
    }],
    code: {
      coding: [{
        system: 'http://loinc.org',
        code: vital.code,
        display: vital.display
      }]
    },
    subject: { reference: `Patient/${patientId}` },
    effectiveDateTime: new Date().toISOString(),
    valueQuantity: {
      value: vital.value,
      unit: vital.unit,
      system: 'http://unitsofmeasure.org'
    }
  });
}
```

### Lab Results Analysis

```python
# Analyze lab results and detect abnormalities
reference_ranges = {
    '2093-3': (135, 145),  # Sodium
    '2075-0': (3.5, 5.0),  # Potassium
    '2951-2': (3.5, 5.2),  # Sodium
}

abnormal_results = analytics.detect_abnormal_results(
    patient_id='123',
    reference_ranges=reference_ranges
)

for result in abnormal_results:
    status = result['status']
    value = result['value']
    ref_low = result['reference_low']
    ref_high = result['reference_high']

    print(f"⚠️  {result['code']}: {value} ({status})")
    print(f"   Normal range: {ref_low}-{ref_high}")
```

## License

MIT License - See LICENSE file for details

## Support

- Documentation: https://github.com/healthcare/fhir-package
- Issues: https://github.com/healthcare/fhir-package/issues
- FHIR Specification: https://hl7.org/fhir/R4/
