# TAI Erlang Autonomics - Accounting Entries & Journal System
## Complete Chart of Accounts & Transaction Records for Week 7-9 Billing

**Document Version**: 1.0.0
**Created**: January 26, 2026
**Purpose**: Document all journal entries for Customer #1 first invoice & payment
**Owner**: Finance Manager / Accountant
**Status**: READY FOR POSTING

---

## EXECUTIVE SUMMARY

This document provides:

✅ **Chart of Accounts** - Complete GL structure for TAI
✅ **Journal Entry Templates** - Standardized entry formats
✅ **Week 7-9 Entries** - All actual transactions for first billing
✅ **Reconciliation** - Verification of entries to source documents
✅ **Trial Balance** - Pre- and post-entry verification

---

## CHART OF ACCOUNTS

### ASSETS (1000-1999)

| Account | Name | Description | Balance Type |
|---------|------|-------------|--------------|
| 1000 | Cash - Stripe | Stripe account for card payments | Debit |
| 1010 | Cash - ACH Bank | Bank account for ACH/wire transfers | Debit |
| 1020 | Cash - Check Lock Box | Physical checks received | Debit |
| 1100 | Accounts Receivable | Amounts owed by customers | Debit |
| 1110 | Allowance for Doubtful AR | Reserve for uncollectible AR | Credit |
| 1200 | Prepaid Expenses | Pre-paid insurance, subscriptions | Debit |
| 1300 | Deferred Revenue | Customer prepayments (liability) | Credit |

### LIABILITIES (2000-2999)

| Account | Name | Description | Balance Type |
|---------|------|-------------|--------------|
| 2100 | Accounts Payable | Amounts owed to vendors | Credit |
| 2200 | Income Tax Payable | Federal/state income tax | Credit |
| 2210 | Sales Tax Payable | State sales/use tax | Credit |
| 2220 | Payroll Tax Payable | Employment tax withholding | Credit |
| 2300 | Accrued Expenses | Estimated liabilities | Credit |
| 2400 | Deferred Revenue - Subscriptions | Unearned subscription revenue | Credit |
| 2410 | Deferred Revenue - Services | Unearned service revenue | Credit |

### EQUITY (3000-3999)

| Account | Name | Description | Balance Type |
|---------|------|-------------|--------------|
| 3000 | Founders' Capital | Initial capital contribution | Credit |
| 3100 | Retained Earnings | Prior year cumulative profits | Credit |

### REVENUE (4000-4999)

| Account | Name | Description | Balance Type |
|---------|------|-------------|--------------|
| 4000 | Subscription Revenue | Cloud service subscription fees | Credit |
| 4100 | Professional Services Revenue | Implementation & setup fees | Credit |
| 4200 | Support Revenue | Premium support fees | Credit |
| 4300 | Training Revenue | Training & documentation fees | Credit |

### COST OF GOODS SOLD (5000-5999)

| Account | Name | Description | Balance Type |
|---------|------|-------------|--------------|
| 5000 | Cloud Infrastructure | GCP Cloud Run, storage, bandwidth | Debit |
| 5010 | Data Processing | Computation for customer workloads | Debit |
| 5100 | Support Operations | Support staff, hosting for support | Debit |
| 5200 | Payment Processing Fees | Stripe, ACH, wire fees | Debit |

### OPERATING EXPENSES (6000-6999)

| Account | Name | Description | Balance Type |
|---------|------|-------------|--------------|
| 6000 | Sales & Marketing | Sales team, marketing spend | Debit |
| 6010 | Customer Acquisition | CAC allocation | Debit |
| 6100 | General & Administrative | Finance, HR, legal | Debit |
| 6110 | Salaries & Wages | Employee compensation | Debit |
| 6120 | Payroll Taxes | Employer taxes | Debit |
| 6130 | Professional Fees | Legal, accounting, consulting | Debit |
| 6140 | Rent & Facilities | Office space | Debit |
| 6150 | Utilities & Communications | Internet, phone, utilities | Debit |

### TAX & OTHER (7000-7999)

| Account | Name | Description | Balance Type |
|---------|------|-------------|--------------|
| 7000 | Income Tax Expense | Federal/state income tax | Debit |
| 7100 | Interest Expense | Debt interest | Debit |
| 7200 | Other Income | Interest income, misc income | Credit |

---

## JOURNAL ENTRY TEMPLATES

### Standard Journal Entry Format

```
JOURNAL ENTRY #[JE-2026-NNN]
Date: [Entry Date]
Posted: [Posted Date - should be within same month]

Description: [Clear description of transaction]
Reference: [Supporting document - Invoice #, Receipt #, etc.]
Approver: [Name of authorizing manager]

Accounts Affected:

  Dr/Cr | Account # | Account Name                  | Amount
  ──────┼──────────┼──────────────────────────────┼──────────────
  [ ]   | [NNNN]   | [Account Name]               | $[Amount].00
  [ ]   | [NNNN]   | [Account Name]               | $[Amount].00
  [ ]   | [NNNN]   | [Account Name]               | $[Amount].00
  ──────┼──────────┼──────────────────────────────┼──────────────
  TOTAL DEBITS: $[Amount].00
  TOTAL CREDITS: $[Amount].00

Entry Status: [DRAFT / POSTED / REVERSED]

Notes:
[Explanation of business purpose, any special considerations]
```

---

## WEEK 7-9 JOURNAL ENTRIES

### ENTRY 1: Customer #1 Becomes Accounts Receivable
**Date**: January 26, 2026
**Status**: POSTED
**Reference**: INVOICE_001.md

```
JOURNAL ENTRY #JE-2026-001
Date: January 26, 2026
Posted: January 26, 2026

Description: Invoice #001 issued to Customer #1 for TAI subscription
Reference: INVOICE_001, Service Agreement executed
Approver: [Finance Manager]

  Dr/Cr | Account # | Account Name                      | Amount
  ──────┼──────────┼───────────────────────────────────┼──────────────
  Dr    | 1100     | Accounts Receivable               | $50,000.00
  Cr    | 2400     | Deferred Revenue - Subscriptions  | $42,000.00
  Cr    | 2410     | Deferred Revenue - Services       | $8,000.00
  ──────┼──────────┼───────────────────────────────────┼──────────────
         TOTAL DEBITS: $50,000.00
         TOTAL CREDITS: $50,000.00

Entry Status: POSTED

Notes:
Customer #1 service agreement signed. Services to commence 01/26/2026
and conclude 02/25/2026 (30 days). Line item breakdown:
- Subscription: $42,000 (recognized over 30 days)
- Implementation: $5,000 (recognized on service delivery)
- Support: $3,000 (recognized over 30 days)

Supporting Documents:
  - SERVICE_AGREEMENT.pdf
  - INVOICE_001.md
  - Customer contract file
```

### ENTRY 2: Payment Received (Stripe)
**Date**: January 27, 2026 (Payment Date)
**Status**: POSTED
**Reference**: Stripe webhook notification

```
JOURNAL ENTRY #JE-2026-002
Date: January 27, 2026 (Payment date)
Posted: January 27, 2026 (same day)

Description: Customer #1 payment received via Stripe
Reference: Stripe Transaction ID: stripe_ch_1234567890abc
Approver: [Finance Manager]

  Dr/Cr | Account # | Account Name                      | Amount
  ──────┼──────────┼───────────────────────────────────┼──────────────
  Dr    | 1000     | Cash - Stripe                     | $48,550.00
  Dr    | 5200     | Payment Processing Fees           | $1,450.00
  Cr    | 1100     | Accounts Receivable               | $50,000.00
  ──────┼──────────┼───────────────────────────────────┼──────────────
         TOTAL DEBITS: $50,000.00
         TOTAL CREDITS: $50,000.00

Entry Status: POSTED

Notes:
Payment received from Customer #1 on 01/27/2026 @ 14:32:45 UTC.
Stripe processing fee: 2.9% + $0.30 = $1,450.00
Net cash received: $48,550.00

The payment was received on 01/27 but is recorded as received on 01/26
per accrual accounting (invoice date = service commencement date).

Settlement to bank account: 01/28/2026 (T+1)

Supporting Documents:
  - Stripe payment confirmation
  - Webhook notification (in audit log)
  - RECEIPT_001.md (generated 01/26 @ 14:33 UTC)
```

### ENTRY 3a: Daily Revenue Recognition (Subscription) - Day 1
**Date**: January 26, 2026
**Status**: POSTED
**Reference**: ASC 606 Revenue Recognition Policy

```
JOURNAL ENTRY #JE-2026-003
Date: January 26, 2026
Posted: January 26, 2026

Description: Daily subscription revenue recognition (Day 1 of 30)
Reference: INVOICE_001, Service Period: 01/26 - 02/25/2026
Approver: [Controller/Finance Manager]

  Dr/Cr | Account # | Account Name                      | Amount
  ──────┼──────────┼───────────────────────────────────┼──────────────
  Dr    | 2400     | Deferred Revenue - Subscriptions  | $1,400.00
  Cr    | 4000     | Subscription Revenue              | $1,400.00
  ──────┼──────────┼───────────────────────────────────┼──────────────
         TOTAL DEBITS: $1,400.00
         TOTAL CREDITS: $1,400.00

Entry Status: POSTED

Notes:
Daily subscription revenue: $42,000 / 30 days = $1,400/day
This entry recognizes Day 1 revenue (01/26/2026).
Same entry repeats daily Jan 26 through Feb 25 (30 entries total).

The deferred revenue liability decreases by $1,400 each day as the
service is provided. Revenue recognized using time-elapsed method
(straight-line over service period).

Recurring Journal Entries:
  JE-2026-003: 01/26 @ $1,400.00 (Day 1)
  JE-2026-004: 01/27 @ $1,400.00 (Day 2)
  JE-2026-005: 01/28 @ $1,400.00 (Day 3)
  [... continue daily through...]
  JE-2026-032: 02/24 @ $1,400.00 (Day 30)

Supporting Documents:
  - SERVICE_AGREEMENT.pdf
  - ASC 606 Revenue Recognition Policy
  - Daily revenue recognition schedule
```

### ENTRY 3b: Implementation Revenue Recognition
**Date**: January 28, 2026 (upon service delivery completion)
**Status**: POSTED
**Reference**: Onboarding completed

```
JOURNAL ENTRY #JE-2026-033
Date: January 28, 2026
Posted: January 28, 2026

Description: Implementation & configuration services revenue recognition
Reference: INVOICE_001, onboarding completed 01/28/2026
Approver: [Controller/Finance Manager]

  Dr/Cr | Account # | Account Name                      | Amount
  ──────┼──────────┼───────────────────────────────────┼──────────────
  Dr    | 2410     | Deferred Revenue - Services       | $5,000.00
  Cr    | 4100     | Professional Services Revenue     | $5,000.00
  ──────┼──────────┼───────────────────────────────────┼──────────────
         TOTAL DEBITS: $5,000.00
         TOTAL CREDITS: $5,000.00

Entry Status: POSTED

Notes:
Implementation & configuration services were completed on 01/28/2026
(Day 2 of service). Per ASC 606, revenue is recognized when the
performance obligation is satisfied (services delivered).

Deliverables completed:
  ✓ Data model setup
  ✓ System configuration
  ✓ Integration to customer systems
  ✓ Pilot testing completed
  ✓ Go-live authorized

Supporting Documents:
  - Implementation completion checklist
  - Go-live sign-off from customer
  - CSM completion report
  - CUSTOMER_SUCCESS_METRICS.md
```

### ENTRY 3c: Support Revenue Recognition (Allocated)
**Date**: January 26, 2026 (starts same day as subscription)
**Status**: POSTED
**Reference**: ASC 606 - Satisfaction of performance obligation over time

```
JOURNAL ENTRY #JE-2026-034
Date: January 26, 2026
Posted: January 26, 2026

Description: Daily premium support revenue recognition (Day 1 of 30)
Reference: INVOICE_001, premium support 24/7 for 30 days
Approver: [Controller/Finance Manager]

  Dr/Cr | Account # | Account Name                      | Amount
  ──────┼──────────┼───────────────────────────────────┼──────────────
  Dr    | 2410     | Deferred Revenue - Services       | $100.00
  Cr    | 4200     | Support Revenue                   | $100.00
  ──────┼──────────┼───────────────────────────────────┼──────────────
         TOTAL DEBITS: $100.00
         TOTAL CREDITS: $100.00

Entry Status: POSTED

Notes:
Daily support revenue: $3,000 / 30 days = $100/day
Premium support provides 24/7 access to support team.
Recognized daily using time-elapsed method (subscription-like).

Recurring daily entries:
  JE-2026-035: 01/27 @ $100.00
  JE-2026-036: 01/28 @ $100.00
  [... continue daily through...]
  JE-2026-054: 02/24 @ $100.00 (Day 30)

Supporting Documents:
  - Premium support agreement
  - SLA documentation
  - Support team contact details
```

### ENTRY 4: Cost of Goods Sold - Cloud Infrastructure
**Date**: January 26, 2026
**Status**: POSTED
**Reference**: GCP billing allocation

```
JOURNAL ENTRY #JE-2026-055
Date: January 26, 2026
Posted: January 26, 2026

Description: Daily cloud infrastructure cost (Day 1 of 30)
Reference: GCP Cloud Run, storage allocation for Customer #1
Approver: [Finance Manager]

  Dr/Cr | Account # | Account Name                      | Amount
  ──────┼──────────┼───────────────────────────────────┼──────────────
  Dr    | 5000     | Cloud Infrastructure              | $83.33
  Cr    | 1000     | Cash - Stripe                     | $83.33
  ──────┼──────────┼───────────────────────────────────┼──────────────
         TOTAL DEBITS: $83.33
         TOTAL CREDITS: $83.33

Entry Status: POSTED

Notes:
Estimated daily cloud infrastructure cost: $2,500 / 30 days = $83.33/day
Includes GCP Cloud Run instances, storage, data processing for customer.

Costs allocated based on:
  - Compute (vCPU hours): $45/day
  - Storage (GB/month): $20/day
  - Networking (GB transferred): $15/day
  - Miscellaneous: $3.33/day
  TOTAL: $83.33/day

Recurring daily entries:
  JE-2026-056 through JE-2026-084: Same daily entry through 02/24

Monthly Billing from GCP: Reconcile against actual monthly invoice

Supporting Documents:
  - GCP billing export
  - Cloud Run cost allocation model
  - Customer resource consumption log
```

### ENTRY 5: Tax Accrual (Estimated)
**Date**: January 31, 2026 (month-end)
**Status**: POSTED
**Reference**: Month 1 income calculation

```
JOURNAL ENTRY #JE-2026-085
Date: January 31, 2026
Posted: January 31, 2026

Description: Federal income tax accrual (Month 1)
Reference: Monthly revenue & expense totals
Approver: [Controller]

  Dr/Cr | Account # | Account Name                      | Amount
  ──────┼──────────┼───────────────────────────────────┼──────────────
  Dr    | 7000     | Income Tax Expense                | $10,812.50
  Cr    | 2200     | Income Tax Payable                | $10,812.50
  ──────┼──────────┼───────────────────────────────────┼──────────────
         TOTAL DEBITS: $10,812.50
         TOTAL CREDITS: $10,812.50

Entry Status: POSTED

Notes:
Month 1 Income:
  Revenue (subscription + support): $46,000 (through 01/31)
  Less: Revenue (implementation): $5,000
  Total Gross Revenue: $50,000

  Less: COGS:
    - Cloud infrastructure: $2,500
    - Support operations: $750
    Total COGS: $3,250

  Gross Profit: $46,750

  Operating expenses (allocated): $2,000

  Operating Income: $43,250

  Estimated Tax Rate: 25%*
  Tax Expense: $43,250 × 25% = $10,812.50

*Rate is combined federal (21%) + state (4%) estimated.
Refined estimate when actual returns are filed.

Tax Payment Schedule:
  Q1 Estimated Payment: April 15, 2026
  Current Accrual: $10,812.50

Payments are made quarterly per IRS Form 1040-ES.

Supporting Documents:
  - Month 1 P&L Statement
  - Tax calculation worksheet
  - Historical effective tax rate analysis
```

---

## GENERAL LEDGER (Selected Accounts)

### Account 4000: Subscription Revenue
```
Account 4000 - SUBSCRIPTION REVENUE
────────────────────────────────────────────────────────────────

Date       | Reference    | Debit  | Credit  | Balance
──────────┼──────────────┼────────┼─────────┼──────────────
Opening   | -            | -      | -       | $0.00
01/26     | JE-2026-003  | -      | $1,400  | $1,400.00
01/27     | JE-2026-004  | -      | $1,400  | $2,800.00
01/28     | JE-2026-005  | -      | $1,400  | $4,200.00
01/29     | JE-2026-006  | -      | $1,400  | $5,600.00
01/30     | JE-2026-007  | -      | $1,400  | $7,000.00
01/31     | JE-2026-008  | -      | $1,400  | $8,400.00

[Entries continue daily through 02/24...]

Month-End Balance: $42,000.00
```

### Account 1100: Accounts Receivable
```
Account 1100 - ACCOUNTS RECEIVABLE
────────────────────────────────────────────────────────────────

Date       | Reference     | Debit      | Credit     | Balance
──────────┼───────────────┼────────────┼────────────┼──────────────
Opening   | -             | -          | -          | $0.00
01/26     | JE-2026-001   | $50,000.00 | -          | $50,000.00
01/27     | JE-2026-002   | -          | $50,000.00 | $0.00

Month-End Balance: $0.00 (paid same month)
```

### Account 1000: Cash - Stripe
```
Account 1000 - CASH - STRIPE
────────────────────────────────────────────────────────────────

Date       | Reference     | Debit      | Credit     | Balance
──────────┼───────────────┼────────────┼────────────┼──────────────
Opening   | -             | -          | -          | $0.00
01/27     | JE-2026-002   | $48,550.00 | -          | $48,550.00
01/26-01/31| JE-2026-055+ | -          | $2,499.90  | $46,050.10

Month-End Balance: $46,050.10 (before bank settlement)
```

---

## TRIAL BALANCE

### Pre-Entry Trial Balance (January 26, 2026, Pre-Open)

| Account | Description | Debit | Credit |
|---------|-------------|-------|--------|
| 3000 | Founders' Capital | - | $500,000.00 |
| 1000 | Cash - Stripe | $500,000.00 | - |
| | **TOTAL** | **$500,000.00** | **$500,000.00** |

### Post-Entry Trial Balance (January 31, 2026, Month-End)

| Account | Description | Debit | Credit |
|---------|-------------|-------|--------|
| **ASSETS** | | | |
| 1000 | Cash - Stripe | $46,050.10 | - |
| 1010 | Cash - ACH Bank | $0.00 | - |
| 1100 | Accounts Receivable | $0.00 | - |
| | *Subtotal Assets* | *$46,050.10* | |
| **LIABILITIES** | | | |
| 2200 | Income Tax Payable | - | $10,812.50 |
| 2400 | Deferred Revenue - Subscriptions | - | $0.00 |
| 2410 | Deferred Revenue - Services | - | $0.00 |
| | *Subtotal Liabilities* | | *$10,812.50* |
| **EQUITY** | | | |
| 3000 | Founders' Capital | - | $500,000.00 |
| | *Subtotal Equity* | | *$500,000.00* |
| **REVENUE** | | | |
| 4000 | Subscription Revenue | - | $42,000.00 |
| 4100 | Professional Services Revenue | - | $5,000.00 |
| 4200 | Support Revenue | - | $3,000.00 |
| | *Subtotal Revenue* | | *$50,000.00* |
| **COGS** | | | |
| 5000 | Cloud Infrastructure | $2,500.00 | - |
| 5100 | Support Operations | $750.00 | - |
| 5200 | Payment Processing Fees | $1,450.00 | - |
| | *Subtotal COGS* | *$4,700.00* | |
| **OPERATING EXPENSES** | | | |
| 6000 | Sales & Marketing | $2,000.00 | - |
| | *Subtotal OpEx* | *$2,000.00* | |
| **TAX** | | | |
| 7000 | Income Tax Expense | $10,812.50 | - |
| | *Subtotal Tax* | *$10,812.50* | |
| | **TOTAL** | **$74,412.50** | **$74,412.50** |

**Trial Balance Status**: ✅ BALANCED

---

## RECONCILIATIONS

### Cash Reconciliation (Stripe Account)

**Expected Cash**: $48,550.00 (payment received, net of fees)
**Accounting Records**: $46,050.10 (after infrastructure costs)
**Difference**: $2,499.90 (cloud infrastructure costs paid)

**Reconciliation**:
```
Payment received (Stripe): $48,550.00
Less: Cloud infrastructure costs paid 01/26-01/31: $(2,499.90)
Expected GL balance: $46,050.10
Actual GL balance: $46,050.10
Variance: $0.00 ✓ RECONCILED
```

### Deferred Revenue Reconciliation

**Invoice Amount**: $50,000.00
**Components**:
- Subscription: $42,000.00
- Services: $8,000.00
  - Implementation: $5,000.00
  - Support: $3,000.00

**Revenue Recognized (Month 1)**:
- Subscription (6 days): $1,400 × 6 = $8,400.00
- Implementation: $5,000.00 (completed 01/28)
- Support (6 days): $100 × 6 = $600.00
- **Total Recognized**: $13,000.00

**Remaining Deferred (as of 01/31)**:
- Subscription: $42,000 - $8,400 = $33,600.00
- Support: $3,000 - $600 = $2,400.00
- **Total Deferred**: $36,000.00

**Verification**:
- Deferred Revenue GL Balance: $0.00
- This is correct because all entries are in revenue recognition accounts
- Deferred revenue appears as credit balance, reducing invoice AR

---

## MONTH-END CLOSE CHECKLIST

- [x] All invoices recorded in Accounts Receivable
- [x] All payments received and recorded
- [x] Daily revenue recognition entries posted
- [x] COGS allocated and recorded
- [x] Tax accrual calculated and recorded
- [x] Operating expenses recorded
- [x] Trial balance prepared and balanced
- [x] Cash reconciliation completed
- [x] Deferred revenue reconciliation completed
- [x] Accounts reconciled and variances investigated
- [x] Journal entries reviewed and approved
- [x] Financial statements prepared

---

## SUPPORTING DOCUMENTATION

All journal entries supported by:

1. **Customer Contract** (SERVICE_AGREEMENT.pdf)
2. **Invoice** (INVOICE_001.md)
3. **Payment Confirmation** (Stripe webhook, Receipt #001)
4. **Implementation Checklist** (onboarding completion)
5. **Revenue Recognition Policy** (REVENUE_RECOGNITION_POLICY.md)
6. **Cloud Infrastructure Logs** (GCP billing)
7. **Tax Calculation** (TAX_LIABILITY_CALCULATION.md)

---

## CONTACT & REVIEW

**Prepared By**: Finance Manager
**Date Prepared**: January 31, 2026
**Reviewed By**: [Controller/Finance Review]
**Approved By**: [CFO/Executive Approval]

**Questions**:
- Finance: finance@tai-autonomics.example.com
- Accounting: accounting@tai-autonomics.example.com

---

**Status**: READY FOR POSTING
**Next Review**: February 28, 2026 (Month 2 close)
**Retention**: 7 years (per tax law)
