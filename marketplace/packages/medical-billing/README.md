# Medical Billing

Enterprise medical billing system with CPT/ICD-10 coding, insurance claims processing, and revenue cycle management.

## Features

- **Claims Processing**: CMS-1500, UB-04 claim generation
- **Medical Coding**: ICD-10-CM, CPT, HCPCS automation
- **Denial Management**: Automated denial tracking and appeals
- **Payment Posting**: ERA/EOB processing (835 transactions)
- **Revenue Cycle**: Complete RCM from scheduling to payment
- **Compliance**: HIPAA 5010 transactions

## Supported Claim Types

- Professional (CMS-1500)
- Institutional (UB-04)
- Dental (ADA)
- Pharmacy (NCPDP)

## Quick Start

### Create and Submit Claim

```rust
use medical_billing::*;

let mut billing = BillingSystem::new();

// Create claim
let claim = billing.create_claim(
    "CLM-001",
    ClaimType::Professional,
    "PAT-001",
    "BCBS"
)?;

// Add diagnosis codes
claim.add_diagnosis("J06.9")?; // URI

// Add procedure codes
claim.add_procedure("99213", 1, 125.00)?; // Office visit

// Submit claim
billing.submit_claim(&claim).await?;
```

### Process ERA/EOB

```typescript
import { BillingSystem } from '@ggen/medical-billing';

const billing = new BillingSystem();

// Process 835 ERA
const era = billing.processERA(eraFile);

for (const payment of era.payments) {
  billing.postPayment({
    claimId: payment.claimNumber,
    amount: payment.paidAmount,
    adjustments: payment.adjustments
  });
}
```

### Denial Management

```python
from medical_billing import DenialManager

denial_mgr = DenialManager()

# Analyze denial
denial = denial_mgr.get_denial("CLM-001")
print(f"Denial reason: {denial.reason_code}")
print(f"Recommended action: {denial.recommended_action}")

# Submit appeal
if denial.is_appealable:
    appeal = denial_mgr.create_appeal(denial.id, "Supporting documentation attached")
    denial_mgr.submit_appeal(appeal)
```

## Medical Coding

### ICD-10-CM Diagnosis Codes
- Chapter-based organization
- Laterality and specificity
- Manifestation codes

### CPT Procedure Codes
- Evaluation & Management (99xxx)
- Surgery (10xxx-69xxx)
- Radiology (70xxx-79xxx)
- Laboratory (80xxx-89xxx)
- Medicine (90xxx-99xxx)

### Modifiers
- 25, 59 (Distinct services)
- 50 (Bilateral)
- 51 (Multiple procedures)
- 76, 77 (Repeat procedures)

## Revenue Cycle

1. **Patient Registration**: Insurance verification
2. **Charge Capture**: Service documentation
3. **Coding**: ICD-10/CPT assignment
4. **Claim Submission**: Electronic (837) or paper
5. **Payment Posting**: ERA/EOB processing
6. **Denial Management**: Appeal workflow
7. **Collections**: AR follow-up

## Insurance Payers

- Medicare
- Medicaid
- Commercial (BCBS, Aetna, UHC, etc.)
- Workers' Compensation
- Auto/Liability

## EDI Transactions

- 837P (Professional claims)
- 837I (Institutional claims)
- 835 (ERA - Electronic Remittance Advice)
- 270/271 (Eligibility inquiry/response)
- 276/277 (Claim status inquiry/response)

## Compliance

- HIPAA 5010 standards
- ICD-10-CM official guidelines
- CPT coding guidelines
- CMS billing rules
- Stark Law compliance

## Architecture

- 325 lines RDF ontology
- 12 SPARQL queries
- EDI transaction support
- 590+ lines TDD tests

## License

MIT
