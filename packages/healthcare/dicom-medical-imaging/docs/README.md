# DICOM Medical Imaging Package

Complete DICOM implementation with PACS integration, DICOMweb services, and medical imaging analytics.

## Overview

This package provides comprehensive DICOM support for medical imaging workflows, including metadata extraction, PACS connectivity, and DICOMweb REST services (WADO-RS, QIDO-RS, STOW-RS).

### Key Features

- **DICOM Standard Compliance**: Full support for DICOM PS3.1-PS3.20
- **Metadata Extraction**: Parse and extract all DICOM tags and attributes
- **PACS Integration**: Connect to Picture Archiving and Communication Systems
- **DICOMweb Services**: WADO-RS, QIDO-RS, STOW-RS implementations
- **Multi-Modality Support**: CT, MR, US, XA, CR, DX, MG, PT, NM, SC
- **Study/Series/Instance Hierarchy**: Complete information model support

## Quick Start

### Rust DICOM Parser

```rust
use dicom_parser::*;

let parser = DICOMParser::new();

// Parse DICOM file
let dicom = parser.parse_file("study/series/instance.dcm")?;

// Access patient information
println!("Patient: {}", dicom.patient.patient_name.unwrap());
println!("Patient ID: {}", dicom.patient.patient_id.unwrap());
println!("DOB: {}", dicom.patient.patient_birth_date.unwrap());

// Access study information
println!("Study UID: {}", dicom.study.study_instance_uid);
println!("Study Date: {}", dicom.study.study_date.unwrap());
println!("Modality: {}", dicom.series.modality.unwrap());

// Access image properties
println!("Dimensions: {}x{}", dicom.instance.rows.unwrap(), dicom.instance.columns.unwrap());
println!("Bits Allocated: {}", dicom.instance.bits_allocated.unwrap());

// Validate DICOM file
parser.validate(&dicom)?;

// Search studies
let results = parser.search_studies(
    &files,
    Some("12345"),  // Patient ID
    Some("20240115"),  // Study Date
    Some("CT")  // Modality
);
```

### TypeScript DICOM Web Client

```typescript
import { DICOMWebClient } from './dicom-web-client';

const client = new DICOMWebClient('http://localhost:8080/dicomweb');

// QIDO-RS: Search for studies
const studies = await client.searchStudies({
  PatientID: '12345',
  StudyDate: '20240115',
  Modality: 'CT'
});

for (const study of studies) {
  console.log(`Study: ${study.StudyInstanceUID}`);
  console.log(`Date: ${study.StudyDate}`);
  console.log(`Description: ${study.StudyDescription}`);

  // Search for series in study
  const series = await client.searchSeries(study.StudyInstanceUID, {
    Modality: 'CT'
  });

  for (const s of series) {
    console.log(`  Series: ${s.SeriesInstanceUID}`);
    console.log(`  Modality: ${s.Modality}`);
    console.log(`  Images: ${s.NumberOfSeriesRelatedInstances}`);
  }
}

// WADO-RS: Retrieve instance
const imageData = await client.retrieveInstance(
  studyUID,
  seriesUID,
  instanceUID
);

// WADO-RS: Retrieve metadata
const metadata = await client.retrieveMetadata(
  studyUID,
  seriesUID,
  instanceUID
);

console.log('Instance metadata:', metadata);

// STOW-RS: Store instance
await client.storeInstances(studyUID, [dicomFile]);
```

### Python DICOM Analytics

```python
from dicom_analytics import DICOMAnalytics

analytics = DICOMAnalytics()

# Load DICOM files from directory
studies = analytics.load_directory('/path/to/dicom/studies')

# Analyze study
study_analysis = analytics.analyze_study(study_uid)
print(f"Patient: {study_analysis['patient_name']}")
print(f"Modalities: {', '.join(study_analysis['modalities'])}")
print(f"Series Count: {study_analysis['series_count']}")
print(f"Image Count: {study_analysis['instance_count']}")

# Extract metadata by tag
patient_age = analytics.get_tag_value(dicom, '(0010,1010)')
slice_thickness = analytics.get_tag_value(dicom, '(0018,0050)')

# Generate statistics
stats = analytics.generate_statistics(studies)
print(f"Total Studies: {stats['total_studies']}")
print(f"Total Images: {stats['total_instances']}")
print(f"Modality Distribution:")
for modality, count in stats['modality_counts'].items():
    print(f"  {modality}: {count}")

# Quality checks
quality_report = analytics.check_quality(study_uid)
for issue in quality_report['issues']:
    print(f"⚠️  {issue['severity']}: {issue['description']}")
```

## DICOM Information Model

### Patient-Study-Series-Instance Hierarchy

```
Patient (0010,xxxx)
  ├── PatientName (0010,0010)
  ├── PatientID (0010,0020)
  ├── PatientBirthDate (0010,0030)
  └── PatientSex (0010,0040)
  │
  └── Study (0020,000D)
      ├── StudyInstanceUID (0020,000D)
      ├── StudyDate (0008,0020)
      ├── StudyDescription (0008,1030)
      └── AccessionNumber (0008,0050)
      │
      └── Series (0020,000E)
          ├── SeriesInstanceUID (0020,000E)
          ├── Modality (0008,0060)
          ├── SeriesNumber (0020,0011)
          └── SeriesDescription (0008,103E)
          │
          └── Instance (0008,0018)
              ├── SOPInstanceUID (0008,0018)
              ├── SOPClassUID (0008,0016)
              ├── InstanceNumber (0020,0013)
              ├── Rows (0028,0010)
              ├── Columns (0028,0011)
              └── BitsAllocated (0028,0100)
```

## DICOMweb Services

### QIDO-RS (Query)

**Search Studies:**
```http
GET /dicomweb/studies?PatientID=12345&StudyDate=20240115&Modality=CT
Accept: application/dicom+json
```

**Search Series:**
```http
GET /dicomweb/studies/{studyUID}/series?Modality=CT
```

**Search Instances:**
```http
GET /dicomweb/studies/{studyUID}/series/{seriesUID}/instances
```

### WADO-RS (Retrieve)

**Retrieve Instance:**
```http
GET /dicomweb/studies/{studyUID}/series/{seriesUID}/instances/{instanceUID}
Accept: application/dicom
```

**Retrieve Metadata:**
```http
GET /dicomweb/studies/{studyUID}/series/{seriesUID}/instances/{instanceUID}/metadata
Accept: application/dicom+json
```

**Retrieve Rendered Image:**
```http
GET /dicomweb/studies/{studyUID}/series/{seriesUID}/instances/{instanceUID}/rendered
Accept: image/jpeg
```

### STOW-RS (Store)

**Store Instances:**
```http
POST /dicomweb/studies/{studyUID}
Content-Type: multipart/related; type="application/dicom"

<multipart DICOM data>
```

## Modality-Specific Features

### CT (Computed Tomography)

```rust
// Extract CT-specific parameters
let kvp = parser.get_tag(&dicom, "(0018,0060)")?;  // KVP
let exposure_time = parser.get_tag(&dicom, "(0018,1150)")?;
let slice_thickness = parser.get_tag(&dicom, "(0018,0050)")?;

println!("CT Parameters:");
println!("  KVP: {} kV", kvp);
println!("  Exposure Time: {} ms", exposure_time);
println!("  Slice Thickness: {} mm", slice_thickness);
```

### MR (Magnetic Resonance)

```rust
// Extract MR-specific parameters
let field_strength = parser.get_tag(&dicom, "(0018,0087)")?;  // Tesla
let echo_time = parser.get_tag(&dicom, "(0018,0081)")?;  // TE
let repetition_time = parser.get_tag(&dicom, "(0018,0080)")?;  // TR
let sequence = parser.get_tag(&dicom, "(0018,0020)")?;

println!("MR Parameters:");
println!("  Field Strength: {} T", field_strength);
println!("  TE: {} ms", echo_time);
println!("  TR: {} ms", repetition_time);
println!("  Sequence: {}", sequence);
```

### US (Ultrasound)

```rust
// Extract US-specific parameters
let transducer_freq = parser.get_tag(&dicom, "(0018,6030)")?;

println!("US Parameters:");
println!("  Transducer Frequency: {} MHz", transducer_freq);
```

## PACS Integration

```typescript
// Configure PACS connection
const pacsClient = new PACSClient({
  aeTitle: 'WORKSTATION',
  hostname: 'pacs.hospital.org',
  port: 11112
});

// C-FIND: Query PACS
const studies = await pacsClient.findStudies({
  PatientID: '12345',
  StudyDate: '20240101-20240131',
  Modality: 'CT'
});

// C-MOVE: Retrieve studies
await pacsClient.moveStudy(
  studyUID,
  'DESTINATION_AE'
);

// C-STORE: Send images to PACS
await pacsClient.storeInstance(dicomFile);
```

## Viewer Integration

```typescript
// Generate viewer URL
const viewerUrl = client.generateViewerUrl(
  studyUID,
  seriesUID,
  instanceUID
);

// Launch OHIF Viewer
window.open(viewerUrl, '_blank');

// Weasis integration
const weasisUrl = client.generateWeasisUrl([studyUID]);
window.location.href = weasisUrl;
```

## Advanced Queries

### Multi-Modality Studies

```sql
SELECT ?studyUID ?patientID ?studyDate ?modalities
WHERE {
  ?study dicom-ontology:studyInstanceUID ?studyUID ;
         dicom-ontology:studyDate ?studyDate .

  ?patient dicom-ontology:hasStudy ?study ;
           dicom-ontology:patientID ?patientID .

  {
    SELECT ?study (GROUP_CONCAT(DISTINCT ?mod; separator=", ") AS ?modalities)
    WHERE {
      ?study dicom-ontology:hasSeries ?series .
      ?series dicom-ontology:modality ?mod .
    }
    GROUP BY ?study
    HAVING (COUNT(DISTINCT ?mod) > 1)
  }
}
ORDER BY DESC(?studyDate)
```

### Image Quality Analysis

```python
# Check for missing metadata
quality_issues = []

required_tags = [
    ('(0010,0010)', 'Patient Name'),
    ('(0010,0020)', 'Patient ID'),
    ('(0020,000D)', 'Study Instance UID'),
    ('(0020,000E)', 'Series Instance UID'),
    ('(0008,0018)', 'SOP Instance UID'),
]

for tag, name in required_tags:
    if not analytics.has_tag(dicom, tag):
        quality_issues.append(f"Missing {name} ({tag})")

# Check image dimensions
rows = analytics.get_tag_value(dicom, '(0028,0010)')
columns = analytics.get_tag_value(dicom, '(0028,0011)')

if rows < 256 or columns < 256:
    quality_issues.append(f"Low resolution: {rows}x{columns}")

# Check bits allocated
bits = analytics.get_tag_value(dicom, '(0028,0100)')
if bits < 8:
    quality_issues.append(f"Insufficient bit depth: {bits}")
```

## Testing

```bash
# Run all tests
cargo test --test dicom_parser_test

# Test specific functionality
cargo test test_parse_dicom
cargo test test_wado_url_generation
cargo test test_search_studies

# Test PACS integration
cargo test test_pacs_connection
```

## License

MIT License
