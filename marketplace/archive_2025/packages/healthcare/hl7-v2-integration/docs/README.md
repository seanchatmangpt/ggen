# HL7 v2 Integration Package

Complete HL7 v2.x message integration with parsing, routing, transformation, and FHIR conversion.

## Overview

This package provides comprehensive HL7 v2 message handling for ADT, ORM, ORU, SIU, and MDM messages with bidirectional FHIR transformation capabilities.

### Key Features

- **HL7 v2.5 & v2.7 Support**: Complete implementation of major message types
- **Message Parsing**: Robust parser handling all segment types and data types
- **Message Generation**: Create valid HL7 v2 messages programmatically
- **Message Routing**: Intelligent routing based on message type and destination
- **FHIR Conversion**: Bidirectional transformation between HL7 v2 and FHIR R4
- **ACK Generation**: Automatic acknowledgment message creation
- **Validation**: Comprehensive message structure and content validation

## Quick Start

### Rust HL7 Parser

```rust
use hl7_parser::*;

let mut parser = HL7Parser::new();

// Parse ADT message
let message_text = r#"MSH|^~\&|SENDING_APP|SENDING_FACILITY|RECEIVING_APP|RECEIVING_FACILITY|20240115120000||ADT^A01|MSG00001|P|2.5
PID|1||12345^^^MRN||Doe^John^A||19800101|M|||123 Main St^^Springfield^IL^62701||(555)123-4567
PV1|1|I|ICU^101^1|||||||MED||||||||V1234^^^VN"#;

let message = parser.parse(message_text)?;

println!("Message Type: {}", message.message_type);
println!("Trigger Event: {}", message.trigger_event);

// Extract patient information
let patient_id = parser.get_field(&message, "PID-3").unwrap();
let last_name = parser.get_field(&message, "PID-5.1").unwrap();
let first_name = parser.get_field(&message, "PID-5.2").unwrap();

println!("Patient: {} {} (ID: {})", first_name, last_name, patient_id);

// Generate ACK
let ack = parser.create_ack(&message, "AA", Some("Message received"))?;
let ack_text = parser.generate(&ack);
```

### TypeScript HL7 Message Broker

```typescript
import { HL7MessageBroker, createADTMessage } from './hl7-broker';

const broker = new HL7MessageBroker();

// Register message handlers
broker.registerHandler('ADT^A01', async (message) => {
  console.log('Processing admission:', message);

  // Extract patient info
  const patientId = broker.getField(message, 'PID-3');
  const patientName = broker.getField(message, 'PID-5');

  // Convert to FHIR
  const fhirPatient = broker.hl7ToFHIRPatient(message);
  console.log('FHIR Patient:', fhirPatient);
});

broker.registerHandler('ORU^R01', async (message) => {
  console.log('Processing lab results:', message);
});

// Process incoming message
const adt = createADTMessage('12345', 'Doe', 'John', '19800101', 'M');
const response = await broker.processMessage(broker.generate(adt));
console.log('Response:', response);
```

### Python HL7 to FHIR Transformer

```python
from hl7_transformer import HL7Transformer

transformer = HL7Transformer()

# Parse HL7 ADT message
adt_message = """MSH|^~\\&|SENDING_APP|SENDING_FACILITY|RECEIVING_APP|RECEIVING_FACILITY|20240115120000||ADT^A01|MSG00001|P|2.5
PID|1||12345^^^MRN||Doe^John^A||19800101|M|||123 Main St^^Springfield^IL^62701||(555)123-4567
PV1|1|O|||||||||||||||||V1234^^^VN"""

message = transformer.parse_message(adt_message)

# Convert to FHIR Patient
fhir_patient = transformer.pid_to_fhir_patient(message)
print('FHIR Patient:', fhir_patient)

# Convert back to HL7
pid_segment = transformer.fhir_patient_to_pid(fhir_patient)
print('PID Segment:', pid_segment.fields)
```

## Message Types

### ADT (Admission, Discharge, Transfer)

**Supported Events:**
- A01: Admit patient
- A04: Register patient
- A08: Update patient information
- A11: Cancel admit
- A12: Cancel transfer
- A13: Cancel discharge

**Example: Patient Admission**
```typescript
const admitMessage = `MSH|^~\\&|EMR|HOSPITAL|ADT|FACILITY|20240115120000||ADT^A01|MSG001|P|2.5
EVN|A01|20240115120000
PID|1||MRN12345||Doe^John^Michael||19800115|M|||123 Main St^^Springfield^IL^62701
PV1|1|I|ICU^101^1||| Attending^Doctor^A|||MED||||||||ADM12345|||||||||||||||||||HOSPITAL|||20240115120000`;

const message = broker.parse(admitMessage);
const ack = await broker.processMessage(admitMessage);
```

### ORM (Order Messages)

**Supported Events:**
- O01: General order message

**Example: Lab Order**
```typescript
const labOrder = `MSH|^~\\&|LAB_SYSTEM|HOSPITAL|LIS|LAB|20240115120000||ORM^O01|MSG002|P|2.5
PID|1||MRN12345||Doe^John
ORC|NW|ORDER001||||||20240115120000
OBR|1||ORDER001|CBC^Complete Blood Count^LN|||20240115120000`;
```

### ORU (Observation Result)

**Supported Events:**
- R01: Unsolicited observation message

**Example: Lab Results**
```typescript
const labResults = `MSH|^~\\&|LIS|LAB|EMR|HOSPITAL|20240115130000||ORU^R01|MSG003|P|2.5
PID|1||MRN12345||Doe^John
OBR|1||ORDER001|CBC^Complete Blood Count^LN|||20240115120000
OBX|1|NM|WBC^White Blood Count^LN||7.5|10^9/L|4.0-11.0|N|||F
OBX|2|NM|RBC^Red Blood Count^LN||4.8|10^12/L|4.2-5.9|N|||F
OBX|3|NM|HGB^Hemoglobin^LN||14.5|g/dL|12-16|N|||F`;

const message = transformer.parse_message(labResults);
const observations = transformer.obx_to_fhir_observation(message);
```

## HL7 to FHIR Conversion

### Patient Conversion (PID → Patient)

```python
# HL7 PID segment
pid_data = {
    'PID-3': '12345^^^MRN',
    'PID-5': 'Doe^John^Michael',
    'PID-7': '19800115',
    'PID-8': 'M',
    'PID-11': '123 Main St^^Springfield^IL^62701',
    'PID-13': '(555)123-4567^PRN'
}

# Converts to FHIR Patient
fhir_patient = {
    'resourceType': 'Patient',
    'identifier': [{'value': '12345', 'system': 'http://hospital.org/mrn'}],
    'name': [{
        'family': 'Doe',
        'given': ['John', 'Michael']
    }],
    'gender': 'male',
    'birthDate': '1980-01-15',
    'address': [{
        'line': ['123 Main St'],
        'city': 'Springfield',
        'state': 'IL',
        'postalCode': '62701'
    }],
    'telecom': [{'system': 'phone', 'value': '(555)123-4567'}]
}
```

### Observation Conversion (OBX → Observation)

```python
# HL7 OBX segment
obx_data = {
    'OBX-2': 'NM',  # Numeric
    'OBX-3': 'WBC^White Blood Count^LN',
    'OBX-5': '7.5',
    'OBX-6': '10^9/L',
    'OBX-8': 'N',  # Normal
    'OBX-14': '20240115130000'
}

# Converts to FHIR Observation
fhir_observation = {
    'resourceType': 'Observation',
    'status': 'final',
    'code': {
        'coding': [{
            'system': 'http://loinc.org',
            'code': 'WBC',
            'display': 'White Blood Count'
        }]
    },
    'valueQuantity': {
        'value': 7.5,
        'unit': '10^9/L'
    },
    'interpretation': [{'text': 'Normal'}],
    'effectiveDateTime': '2024-01-15T13:00:00Z'
}
```

## Message Validation

```rust
let validation_result = parser.validate(&message);

match validation_result {
    Ok(()) => println!("Message is valid"),
    Err(errors) => {
        println!("Validation errors:");
        for error in errors {
            println!("  - {}", error);
        }
    }
}
```

**Validation Checks:**
- MSH segment presence
- Required segments for message type
- Field value formats
- Date/time validity
- Coding system references

## Message Routing

```typescript
// Route messages based on type and facility
broker.registerHandler('ADT^*', async (message) => {
  const facility = broker.getField(message, 'MSH-4');

  if (facility === 'HOSPITAL_A') {
    await routeToHospitalA(message);
  } else if (facility === 'HOSPITAL_B') {
    await routeToHospitalB(message);
  }
});

// Route by patient class
broker.registerHandler('ADT^A01', async (message) => {
  const patientClass = broker.getField(message, 'PV1-2');

  if (patientClass === 'E') {
    await routeToEmergency(message);
  } else if (patientClass === 'I') {
    await routeToInpatient(message);
  }
});
```

## Error Handling

```typescript
try {
  const response = await broker.processMessage(hl7Message);
  console.log('Success:', response);
} catch (error) {
  console.error('Processing failed:', error);

  // Send NACK
  const nack = broker.generateNACK(
    originalMessage,
    error.message
  );
  await sendMessage(nack);
}
```

## Testing

```bash
# Run all tests
cargo test --test hl7_parser_test

# Test specific message type
cargo test test_parse_adt
cargo test test_parse_oru
cargo test test_generate_ack

# Test FHIR conversion
cargo test test_hl7_to_fhir
```

## License

MIT License
