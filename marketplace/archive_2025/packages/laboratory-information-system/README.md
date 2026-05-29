# Laboratory Information System (LIS)

Comprehensive laboratory information system with test ordering, specimen tracking, and results reporting.

## Features

- **Test Ordering**: Comprehensive test catalog
- **Specimen Tracking**: Barcode-based tracking
- **Results Reporting**: Automated result delivery
- **Quality Control**: QC monitoring and calibration
- **Instrument Integration**: HL7/ASTM connectivity
- **Critical Value Alerts**: Automated notifications

## Lab Types Supported

- Clinical Laboratory (Chemistry, Hematology)
- Pathology (Histology, Cytology)
- Microbiology (Culture, Sensitivity)
- Molecular (PCR, NGS)

## Quick Start

### Create Test Order

```rust
use laboratory_lis::*;

let mut lab = Laboratory::new("LAB-001");

// Create order
let order = lab.create_order(
    "ORDER-001",
    "PAT-001",
    vec!["CBC", "CMP", "Lipid Panel"]
)?;

// Collect specimen
let specimen = lab.collect_specimen(
    "SPEC-001",
    SpecimenType::Blood,
    "PHB-001"
)?;

lab.link_order_specimen(&order, &specimen)?;
```

### Process Results

```python
from laboratory_lis import Laboratory, TestResult

lab = Laboratory("LAB-001")

# Receive results from analyzer
result = lab.create_result(
    order_id="ORDER-001",
    test_code="WBC",
    value=7.5,
    unit="K/uL",
    reference_range=(4.0, 11.0)
)

# Auto-verify if within range
if result.is_normal():
    result.verify(pathologist_id="PATH-001")
```

## Test Catalog

- Hematology (CBC, Diff, Coag)
- Chemistry (BMP, CMP, LFT)
- Lipid Panel
- Thyroid Function
- Microbiology cultures
- Pathology specimens

## Specimen Types

- Blood (whole, serum, plasma)
- Urine
- Tissue
- Swab
- Body fluids

## Instrument Integration

- HL7 v2.x (ORU, QRY)
- ASTM E1394
- LIS/Analyzer interface
- Automated result import

## Quality Control

- Daily QC checks
- Calibration tracking
- Proficiency testing
- Westgard rules

## Compliance

- CLIA certified
- CAP accreditation
- HIPAA compliant
- ISO 15189

## Architecture

- 325 lines RDF ontology
- 11 SPARQL queries
- Rust/TypeScript/Python
- 580+ lines TDD tests

## License

MIT
