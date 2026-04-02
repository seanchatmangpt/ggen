# TAI Erlang Autonomics - Revenue Recognition Policy
## ASC 606 Compliance Framework for Cloud Subscription Model

**Document Version**: 1.0.0
**Created**: January 26, 2026
**Effective Date**: January 26, 2026
**Standard**: ASC 606 (FASB Revenue Recognition Standard)
**Owner**: Finance Manager / Controller
**Status**: APPROVED & EFFECTIVE

---

## EXECUTIVE SUMMARY

TAI's revenue recognition policy follows **FASB ASC 606** (Financial Accounting Standards Board, Accounting Standards Codification Topic 606), which defines how and when revenue should be recognized.

**Policy Overview**:
- ✅ **Five-Step Model**: Identify contract → Identify obligations → Determine price → Allocate price → Recognize revenue
- ✅ **Performance Obligations**: Daily service delivery (subscription) and point-in-time (implementation)
- ✅ **Recognition Method**: Time-elapsed for subscriptions, upon completion for implementation
- ✅ **Timing**: Revenue recognized as services are provided, not when payment is received

---

## ASC 606 FIVE-STEP MODEL

### Step 1: Identify the Contract with a Customer

**What is a Contract?**
A contract is an agreement between TAI and a customer that creates enforceable rights and obligations.

**Example (Customer #1)**:
```
Contract Components:
├─ Customer: [Customer #1]
├─ Services: Cloud-based TAI Autonomic System subscription
├─ Term: 30 days (January 26 - February 25, 2026)
├─ Price: $50,000.00 fixed
├─ Payment Terms: Net 30
├─ Performance Obligations: TAI must provide service; customer must pay
└─ Termination Rights: Either party can terminate with 30-day notice
```

**Contract Identification Check**:
- [x] Agreement approved by both parties (contract signed)
- [x] Rights and obligations clearly defined (service agreement)
- [x] Payment terms specified ($50,000, Net 30)
- [x] Termination/modification rights documented
- [x] Commercial substance exists (real economic exchange)
- [x] Payment is probable (customer creditworthiness assessed)

### Step 2: Identify Performance Obligations

**What is a Performance Obligation?**
A promise to transfer a distinct good or service to the customer.

**TAI's Performance Obligations**:

#### Obligation 1: Cloud Subscription Service
- **Type**: Obligation satisfied over time (subscription)
- **Description**: Continuous access to TAI system, automatic decision-making, data processing
- **Duration**: 30 days (Jan 26 - Feb 25, 2026)
- **Price**: $42,000.00
- **Distinct?**: Yes - customer can benefit from access independent of other services
- **Transfer Method**: Daily access over service period
- **Recognition**: Time-elapsed (daily)

#### Obligation 2: Implementation Services
- **Type**: Obligation satisfied at a point in time (service delivery)
- **Description**: Setup, configuration, integration, user training, go-live support
- **Duration**: 2-3 weeks (completed by Jan 28, 2026)
- **Price**: $5,000.00
- **Distinct?**: Yes - can be provided by other vendors
- **Transfer Method**: Upon completion of all deliverables
- **Recognition**: When customer accepts completion

#### Obligation 3: Premium Support
- **Type**: Obligation satisfied over time (subscription)
- **Description**: 24/7 phone/email support, dedicated CSM, SLA guarantees
- **Duration**: 30 days (Jan 26 - Feb 25, 2026)
- **Price**: $3,000.00
- **Distinct?**: Yes - customer could purchase support elsewhere
- **Transfer Method**: Daily over service period
- **Recognition**: Time-elapsed (daily)

**Performance Obligation Summary**:
```
Total Contract Price: $50,000.00

Allocated as follows:
├─ Subscription (Obligation 1) ............ $42,000.00 (84%)
├─ Implementation (Obligation 2) ......... $5,000.00 (10%)
└─ Support (Obligation 3) ................ $3,000.00 (6%)
```

### Step 3: Determine the Transaction Price

**What is Transaction Price?**
The amount of consideration (money) TAI expects to receive in exchange for transferring promised goods/services.

**Analysis for Customer #1**:

#### Fixed Consideration
```
Invoice Amount: $50,000.00
Payment Terms: Net 30 (no time-value adjustment needed)
Currency: USD
Contingencies: None (no variable components)
```

**Transaction Price** = **$50,000.00** (fixed, determinable)

**Revenue Recognition Assumption**:
TAI assumes all amounts will be collected (customer creditworthiness verified). If collectability becomes uncertain, an allowance would be recorded.

### Step 4: Allocate Transaction Price to Performance Obligations

**Method**: Standalone Selling Prices

**Approach**:
Since TAI has no separate sales history, use estimated selling prices:

#### Obligation 1: Subscription Service
- **Standalone Selling Price (Market-based)**: $42,000 / month
  - Comparable SaaS products: $35K-50K/month
  - TAI value proposition supports $42K pricing
- **Allocation %**: $42,000 / $50,000 = 84%

#### Obligation 2: Implementation Services
- **Standalone Selling Price (Cost-plus)**: $5,000 per project
  - Cost: ~$2,500 (personnel time, cloud resources)
  - Margin: ~50% (industry standard for implementation)
  - Price: $5,000
- **Allocation %**: $5,000 / $50,000 = 10%

#### Obligation 3: Premium Support
- **Standalone Selling Price (Market-based)**: $3,000 / month
  - Standard SaaS support: $200-300/user/month
  - Premium support typical: 5-10% of subscription
  - TAI premium support: 3,000 / 42,000 = 7.1% of subscription
- **Allocation %**: $3,000 / $50,000 = 6%

**Allocation Summary**:
```
Total Transaction Price: $50,000.00

Allocated:
├─ Subscription: $42,000.00 (84%)
├─ Implementation: $5,000.00 (10%)
└─ Support: $3,000.00 (6%)

Verification: $42,000 + $5,000 + $3,000 = $50,000 ✓
```

### Step 5: Recognize Revenue When Performance Obligation is Satisfied

**Revenue Recognition Timing and Method**:

#### Obligation 1: Subscription Revenue ($42,000)
- **Recognition Method**: Time-elapsed (daily)
- **Period**: January 26 - February 25, 2026 (30 days)
- **Daily Amount**: $42,000 / 30 days = $1,400/day
- **Journal Entry**: Daily DR Deferred Revenue, CR Revenue
- **Timing**: Each day customer has access

```
Recognition Schedule:
Date       | Daily Revenue | Cumulative
───────────┼───────────────┼─────────────
01/26      | $1,400        | $1,400
01/27      | $1,400        | $2,800
01/28      | $1,400        | $4,200
...        | ...           | ...
02/24      | $1,400        | $42,000
```

#### Obligation 2: Implementation Revenue ($5,000)
- **Recognition Method**: Upon completion
- **Completion Date**: January 28, 2026 (Day 3)
- **Journal Entry**: Single entry when complete
- **Verification**: Customer sign-off and acceptance

```
Completion Deliverables:
✓ Data model configured (01/27)
✓ System integration complete (01/28)
✓ User training delivered (01/27-01/28)
✓ Pilot testing completed (01/28)
✓ Go-live authorized (01/28)
✓ Customer acceptance signed (01/28)

Revenue Recognized: January 28, 2026 ($5,000)
```

#### Obligation 3: Support Revenue ($3,000)
- **Recognition Method**: Time-elapsed (daily)
- **Period**: January 26 - February 25, 2026 (30 days)
- **Daily Amount**: $3,000 / 30 days = $100/day
- **Journal Entry**: Daily DR Deferred Revenue, CR Revenue
- **Timing**: Each day support is available

```
Recognition Schedule:
Date       | Daily Revenue | Cumulative
───────────┼───────────────┼─────────────
01/26      | $100          | $100
01/27      | $100          | $200
01/28      | $100          | $300
...        | ...           | ...
02/24      | $100          | $3,000
```

---

## MONTH-BY-MONTH REVENUE RECOGNITION

### January 2026 (Partial Month)

**Service Days**: January 26-31 (6 days of 30-day service period)

| Obligation | Type | Daily | Days | Jan Revenue |
|-----------|------|-------|------|-------------|
| Subscription | Time-elapsed | $1,400 | 6 | $8,400 |
| Implementation | Point-in-time | $5,000 | 1 | $5,000 |
| Support | Time-elapsed | $100 | 6 | $600 |
| **TOTAL JANUARY** | | | | **$14,000** |

### February 2026 (Partial Month)

**Service Days**: February 1-25 (24 days remaining of 30-day period)

| Obligation | Type | Daily | Days | Feb Revenue |
|-----------|------|-------|------|-------------|
| Subscription | Time-elapsed | $1,400 | 24 | $33,600 |
| Support | Time-elapsed | $100 | 24 | $2,400 |
| **TOTAL FEBRUARY** | | | | **$36,000** |

### Total Revenue Recognized

```
January: $14,000
February: $36,000
TOTAL: $50,000 ✓ (matches contract price)
```

---

## SPECIAL SCENARIOS & POLICY DECISIONS

### Scenario 1: Early Termination

**Situation**: Customer terminates service on February 10, 2026 (15 days used, 15 days remaining)

**Revenue Recognition**:
- Days Used: 15 days (Jan 26 - Feb 10)
- Revenue Recognized: (15/30) × $50,000 = $25,000
- Deferred Revenue Refund: (15/30) × $50,000 = $25,000

**Journal Entry**:
```
DR: Deferred Revenue Liability ........... $25,000.00
CR: Revenue Reversal / Refund ........... $25,000.00

Or alternatively:
DR: Deferred Revenue Liability ........... $25,000.00
CR: Cash (Refund to customer) ........... $25,000.00
```

### Scenario 2: Multi-Month Commitment with Discount

**Situation**: Customer commits to 3-month service with 5% discount ($47,500 total)

**ASC 606 Treatment**:
1. Contract price = $47,500 (including discount)
2. Service period = 90 days
3. Daily recognition = $47,500 / 90 = $527.78/day
4. Time-elapsed method applies

**Policy**: Discounts for multi-month commitments are recognized as reduction to revenue, not as marketing expense.

### Scenario 3: Customer Issues & Service Credit

**Situation**: System outage on Feb 10 (customer entitled to 1-day credit = $1,666.67)

**ASC 606 Treatment**:
1. Original contract price reduced by credit amount
2. New contract price = $50,000 - $1,666.67 = $48,333.33
3. Remaining recognition continues on 30-day basis
4. Cumulative revenue = $48,333.33

**Journal Entry**:
```
DR: Revenue Reversal ..................... $1,666.67
CR: Deferred Revenue / Service Credit ... $1,666.67
```

### Scenario 4: Annual Commitment with Monthly Billing

**Situation**: Customer signs 12-month contract, billed monthly, $42,000/month

**ASC 606 Treatment**:
1. Contract = 12 months × $42,000 = $504,000 total
2. Performance obligation = monthly service delivery
3. Revenue recognized = $42,000/month (or $1,400/day)
4. Deferred revenue = $462,000 (remaining 11 months)

**Monthly Invoice & Recognition**:
```
Month 1: Invoice $42,000 → DR AR, CR Deferred Revenue
         Recognize $42,000 → DR Deferred Revenue, CR Revenue
Month 2: Invoice $42,000 → DR AR, CR Deferred Revenue
         Recognize $42,000 → DR Deferred Revenue, CR Revenue
[etc., continues 12 months]
```

---

## ACCOUNTING POLICIES

### Invoice Timing
- **Policy**: Invoices are issued at start of service period
- **Example**: Service starts Jan 26 → Invoice issued Jan 26 for full 30-day service
- **ASC 606 Impact**: Invoice issuance does not trigger revenue recognition; service delivery does

### Payment Timing
- **Policy**: Invoices are due Net 30 from invoice date
- **Example**: Invoice Jan 26 → Payment due Feb 25
- **ASC 606 Impact**: Revenue is recognized regardless of payment timing; separate issue
- **Assumption**: Receivable is recorded and payment is expected

### Revenue Reversal
- **Policy**: If payment is not received within 60 days, revenue is reversed
- **Journal Entry**: DR Revenue Reversal, CR Deferred Revenue
- **Reason**: ASC 606 requires "probable" collection; 60+ days indicates collection is questionable

### Bonus Metrics & Performance Incentives
- **Policy**: If TAI guarantees specific ROI or performance, actual revenue may be variable
- **Current Status**: Customer #1 contract has fixed pricing (no performance contingencies)
- **Future**: If performance bonuses are introduced, would require separate revenue recognition analysis

### Multi-Year Contracts
- **Policy**: Annual commitments can be invoiced annually, semi-annually, or monthly
- **Revenue Recognition**: Still recognized as services are delivered (not upon invoicing)
- **Deferred Revenue**: Liability on balance sheet for undelivered services

---

## SUPPORTING DOCUMENTATION

### Required for Every Revenue Transaction

1. **Service Agreement** (Legal contract with customer)
   - Identifies performance obligations
   - Specifies service period
   - Defines transaction price

2. **Invoice** (Billing document)
   - References service agreement
   - Lists service components & pricing
   - Shows invoice date & due date

3. **Evidence of Delivery** (Proof service was provided)
   - Implementation checklist (for point-in-time obligations)
   - System access logs (for subscription obligations)
   - Customer acceptance sign-off

4. **Payment Confirmation** (Receipt or bank confirmation)
   - Transaction ID from payment processor
   - Amount & timing of payment
   - Bank deposit confirmation

5. **Revenue Recognition Schedule** (Excel model tracking all obligations)
   - Shows daily/periodic recognition
   - Deferred revenue liability tracking
   - Cumulative revenue comparison to invoice

---

## PERIOD-END PROCEDURES

### Weekly Reconciliation
- [ ] Confirm all invoices issued are in accounts receivable
- [ ] Confirm all payments received match deposits
- [ ] Reconcile revenue recognized to deferred revenue schedule
- [ ] Investigate any variances

### Monthly Close Procedures
- [ ] Calculate month-end deferred revenue (remaining obligations)
- [ ] Prepare revenue recognition summary (by customer, by service)
- [ ] Complete trial balance verification
- [ ] Prepare financial statements
- [ ] Review for any contracts requiring special treatment
- [ ] Prepare footnote disclosures (if needed for external reporting)

### Quarterly Review
- [ ] Review revenue recognition policy for applicability to new contract types
- [ ] Assess whether deferred revenue will be realizable
- [ ] Evaluate for any changes in customer contracts or pricing
- [ ] Update revenue guidance (if provided to investors/board)

---

## EXCEPTIONS & ESCALATION

### When Revenue Recognition Requires Special Analysis

**Escalate to Controller if**:
- Customer contract includes contingent pricing (bonuses, penalties)
- Service delivery is uncertain or subject to customer acceptance
- Multi-year contracts with annual price adjustments
- Contract modifications (additions, deletions, price changes)
- Customer disputes about service quality or delivery
- Unusual termination scenarios

**Process**:
1. Document the exception
2. Email Controller with contract & issue
3. Controller determines appropriate ASC 606 treatment
4. Document decision & supporting analysis
5. Update this policy if it's a new pattern

---

## TRAINING & COMPETENCY

### Who Needs to Know This Policy

- **Finance Manager**: Prepares journal entries (must understand fully)
- **Accountant**: Posts entries and maintains GL (must understand)
- **CFO**: Reviews financial statements (should understand summary)
- **Sales Team**: Knows how contracts affect revenue (should understand summary)
- **Customer Success**: Tracks delivery milestones (should understand summary)

### Training Materials

- This document (complete policy)
- ASC 606 training (external course recommended)
- Case studies (Customer #1 as example)
- Monthly review of actual transactions

---

## COMPLIANCE & AUDIT

### Internal Controls

- [x] Revenue policy documented and approved
- [x] Segregation of duties (sales, implementation, finance separate)
- [x] Contracts reviewed before revenue recognition
- [x] Supporting documentation filed
- [x] Monthly reconciliation and review
- [x] No revenue recognized without contract
- [x] No revenue recognized before service delivery

### External Audit Readiness

- [x] All contracts available for review
- [x] Revenue recognition methodology documented
- [x] Supporting documentation complete
- [x] General ledger reconciliations current
- [x] Deferred revenue schedule reconciled
- [x] No exceptions or waivers to policy
- [x] Sample testing available

### Investor Reporting

Revenue information provided to investors includes:
- Total revenue (monthly, quarterly, annual)
- Breakdown by service type (subscription, services, support)
- New customer revenue vs. expansion revenue
- Deferred revenue ("remaining obligations")
- Churn/retention metrics

---

## POLICY APPROVAL & EFFECTIVE DATE

- **Prepared By**: Finance Manager
- **Reviewed By**: Controller
- **Approved By**: CEO/CFO
- **Effective Date**: January 26, 2026
- **Last Updated**: January 26, 2026
- **Next Review**: December 31, 2026 (annual review)

---

## REFERENCES

**External Standards**:
- FASB ASC 606 - Revenue from Contracts with Customers
- IFRS 15 - Revenue from Contracts with Customers (international equivalent)
- IRS Publication 538 - Accounting Periods & Methods

**Internal Documents**:
- SERVICE_AGREEMENT.pdf (Customer contract template)
- INVOICE_001.md (Example invoice)
- ACCOUNTING_ENTRIES.md (Journal entry examples)
- WEEKLY_REVENUE_REPORT.xlsx (Tracking spreadsheet)

---

**Document Status**: APPROVED & EFFECTIVE
**Next Review Date**: December 31, 2026
**Questions**: finance@tai-autonomics.example.com
