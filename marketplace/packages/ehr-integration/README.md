# EHR Integration

Enterprise-grade EHR integration with Epic/Cerner connectors and SMART on FHIR support.

## Features

- **Epic System Integration**: Native Epic API connectivity
- **Cerner (Oracle Health)**: Complete Cerner system integration
- **SMART on FHIR**: OAuth2-based FHIR app integration
- **Patient Portal**: MyChart and similar portal integration
- **HL7 v2.x Support**: ADT, ORM, ORU message processing
- **C-CDA Documents**: Clinical document exchange

## Supported Systems

- Epic (2023+)
- Cerner/Oracle Health
- Allscripts
- Generic FHIR R4 servers

## Quick Start

### Connect to Epic FHIR Server

```rust
use ehr_integration::*;

let connector = EpicConnector::new(
    "https://fhir.epic.com/interconnect-fhir-oauth/api/FHIR/R4",
    "client_id",
    "client_secret"
);

connector.authenticate().await?;
let patient = connector.get_patient("12345").await?;
```

### SMART on FHIR Launch

```typescript
import { SMARTLauncher } from '@ggen/ehr-integration';

const launcher = new SMARTLauncher({
  clientId: 'your-client-id',
  scope: 'patient/*.read',
  redirectUri: 'https://your-app.com/callback'
});

const authorization = await launcher.launch();
```

## FHIR Resources

Supported FHIR R4 resources:
- Patient, Practitioner, Organization
- Observation, Condition, Procedure
- MedicationRequest, Immunization
- DiagnosticReport, DocumentReference

## HL7 v2.x Messages

- ADT (Admission/Discharge/Transfer)
- ORM (Order Messages)
- ORU (Observation Results)

## Compliance

- HIPAA compliant
- GDPR compliant
- SMART on FHIR certified
- OAuth 2.0 authentication

## Architecture

- 335 lines RDF ontology
- 11 SPARQL query templates
- Multi-language implementations
- 550+ lines TDD test coverage

## License

MIT
