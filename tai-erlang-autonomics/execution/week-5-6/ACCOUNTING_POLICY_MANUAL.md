# Accounting Policy Manual

**TAI Autonomous System - Financial Management Framework**

**Version**: 1.0.0
**Effective Date**: January 1, 2026
**Last Updated**: January 26, 2026
**Document Status**: APPROVED - Ready for Board Sign-Off

---

## TABLE OF CONTENTS

1. [Financial Management Framework](#financial-management-framework)
2. [Revenue Recognition Policy (ASC 606)](#revenue-recognition-policy-asc-606)
3. [Internal Financial Controls](#internal-financial-controls)
4. [Billing & Collections Procedures](#billing--collections-procedures)
5. [Financial Close & Reporting](#financial-close--reporting)
6. [Tax Compliance Policy](#tax-compliance-policy)
7. [Chart of Accounts](#chart-of-accounts)
8. [Stripe Integration & Configuration](#stripe-integration--configuration)
9. [Risk Management & Mitigation](#risk-management--mitigation)
10. [Audit Procedures & Documentation](#audit-procedures--documentation)

---

## 1. FINANCIAL MANAGEMENT FRAMEWORK

### 1.1 Purpose

This manual establishes the accounting policies and procedures for TAI Autonomous System to ensure:

- Accurate financial reporting in accordance with ASC 606 (Revenue Recognition Standard)
- Compliance with Generally Accepted Accounting Principles (GAAP)
- Strong internal controls over financial transactions
- Investor confidence through transparent financial management
- Regulatory compliance (federal, state, international tax)

### 1.2 Scope

This policy applies to:
- All revenue transactions (SaaS subscriptions, professional services, implementation fees)
- All expenses (COGS, operating expenses)
- All billing and collections activities
- All financial reporting and close procedures
- All customer contracts and agreements

### 1.3 Governance

**Responsible Parties**:

- **Chief Financial Officer (CFO)**
  - Overall accountability for financial accuracy
  - Quarterly review of ASC 606 compliance
  - Board reporting and investor communications
  - External audit coordination

- **Finance Director**
  - Daily policy execution
  - Monthly close procedures
  - Internal controls administration
  - Budget management and variance analysis

- **Billing Manager**
  - Invoice creation and delivery
  - Customer payments processing
  - Accounts receivable management
  - Recurring billing administration

- **External Accountant (Big Four Firm)**
  - Quarterly ASC 606 validation
  - Annual audit
  - Tax compliance review
  - Control assessment

### 1.4 Policy Review

- **Annual Review**: Every January (with updated revenue contracts)
- **Quarterly Update**: When significant contracts change
- **Board Approval**: Required for any material policy changes
- **External Review**: Big Four accountant validation (quarterly minimum)

---

## 2. REVENUE RECOGNITION POLICY (ASC 606)

### 2.1 ASC 606 Framework

**ASC 606** (Revenue from Contracts with Customers) requires revenue recognition based on a five-step model:

1. **Identify the contract** with the customer
2. **Identify performance obligations** (what will be delivered)
3. **Determine the transaction price** (what customer will pay)
4. **Allocate the transaction price** to performance obligations
5. **Recognize revenue** when performance obligations are satisfied

### 2.2 Revenue Categories & Recognition Methods

#### Category 1: SaaS Subscription Revenue (Over Time)

**Definition**: Continuous system access throughout the subscription period

**Performance Obligation**: Provide ongoing access to software platform

**Recognition Trigger**: Subscription period begins (customer can access system)

**Recognition Method**: Daily recognition over subscription period

**Accounting Treatment**:

```
Scenario: Monthly SaaS subscription - $10,000/month
Duration: 12-month contract starting January 1, 2026

JOURNAL ENTRIES:

Invoice Creation (January 1):
  DR Cash                          $120,000
  CR Deferred Revenue              $120,000
  (Explanation: Customer paid upfront for 12-month contract)

Daily Recognition (30 days in January):
  DR Deferred Revenue              $333.33
  CR SaaS Revenue                  $333.33

Month-End Summary (January 31):
  SaaS Revenue Recognized:         $10,000
  Deferred Revenue (Liability):    $110,000

Investor Impact:
  - Cash Collected: $120,000
  - Revenue: $10,000
  - Difference: Deferred Revenue ($110,000) - explains why cash > revenue
```

**Key Accounting Principles**:

- Revenue = subscription fee / number of days × days in period
- Multi-year contracts: 1/12 of annual amount per month
- Pro-rata for customers who join mid-month
- Refunds: Revenue reversal if customer cancels

**ASC 606 Considerations**:

- Customer has right to access software 24/7
- Company has satisfied performance obligation each day
- Price is fixed (no variable components)
- No refund contingencies (except standard 30-day policy)

---

#### Category 2: Professional Services Revenue (Over Time)

**Definition**: Customized implementation, consulting, training

**Performance Obligation**: Deliver specific services per Statement of Work (SOW)

**Recognition Trigger**: SOW signed by customer

**Recognition Method**: Over time based on % of completion

**Recognition Basis**: Milestone completion or hourly tracking

**Accounting Treatment**:

```
Scenario 1: Milestone-Based Services Project
Project Name: CRM Implementation
SOW Amount: $50,000 (3-month duration)
Milestones:
  - Phase 1: Discovery (30%) = $15,000 (Week 1-3)
  - Phase 2: Development (50%) = $25,000 (Week 4-8)
  - Phase 3: Testing (20%) = $10,000 (Week 9-12)

JOURNAL ENTRIES:

Week 3 - Phase 1 Complete:
  DR Accounts Receivable           $15,000
  CR Implementation Revenue         $15,000
  (Explanation: Performance obligation satisfied - discovery complete)

  Invoice sent to customer:
    DR Cash                        $15,000
    CR Accounts Receivable         $15,000

Week 8 - Phase 2 Complete (Cumulative 80%):
  Previous recognition: $15,000
  Current cumulative: $40,000
  Current entry: $25,000

  DR Accounts Receivable           $25,000
  CR Implementation Revenue         $25,000

Week 12 - Phase 3 Complete:
  DR Accounts Receivable           $10,000
  CR Implementation Revenue         $10,000

Final Summary:
  Total Revenue Recognized:        $50,000
  Total Invoiced:                  $50,000
  Accounts Receivable:             $0 (fully paid)
```

```
Scenario 2: Time-Based Services (Hourly Tracking)
Project: Technical Consulting
Hourly Rate: $250/hour
Expected Duration: 200 hours (6 weeks)
Total Project Value: $50,000

WEEKLY RECOGNITION:

Week 1: 40 hours completed
  DR Accounts Receivable           $10,000 (40 × $250)
  CR Implementation Revenue         $10,000

Week 2: 40 hours completed (cumulative 80 hours)
  DR Accounts Receivable           $10,000 (40 × $250)
  CR Implementation Revenue         $10,000

[Continues weekly until 200 hours reached]

Recognition Principle:
  Revenue = Hours Completed × Hourly Rate
  Updated weekly based on timesheets
```

**ASC 606 Considerations**:

- Performance obligation is fulfilled over time (customer benefits continuously)
- Company has right to payment for work to date (ASC 606-10-25-27)
- No separate goods or services - integrated solution
- Termination rights: Customer can terminate with notice (creates variable consideration)

---

#### Category 3: Setup Fees (Point in Time OR Over Time)

**Definition**: One-time fee for account setup, onboarding, or configuration

**Performance Obligation**: Depends on nature of service

**Case A: Instantaneous Setup (Point in Time)**

```
Scenario: Setup fee - $5,000 (completed in 1 week)

JOURNAL ENTRIES:

Week 1 - Setup Complete:
  DR Cash                          $5,000
  CR Setup Fee Revenue             $5,000
  (Explanation: Performance obligation satisfied - setup complete)

Key: Revenue recognized when customer can benefit from setup
     (e.g., account created, users provisioned, data imported)
```

**Case B: Implementation-Type Setup (Over Time)**

```
Scenario: Complex setup requiring 2 weeks work
Setup Fee: $5,000

Week 1 (50% complete):
  DR Accounts Receivable           $2,500
  CR Setup Revenue                 $2,500

Week 2 (100% complete):
  DR Accounts Receivable           $2,500
  CR Setup Revenue                 $2,500
```

**Decision Framework**:

| Setup Type | Duration | Recognition Method | Rationale |
|-----------|----------|-------------------|-----------|
| Auto-provisioning | <1 day | Point in time | Customer benefits immediately |
| Data import | 1-3 days | Over time (daily) | Multi-day fulfillment |
| Custom configuration | 1-2 weeks | Over time (% complete) | Complex deliverable |
| Training (included) | 2-4 weeks | Over time (hours) | Distributed over time |

---

#### Category 4: Support Plans & Maintenance (Over Time)

**Definition**: Ongoing technical support, maintenance, updates

**Performance Obligation**: Provide support hours or SLA-based support

**Recognition Method**: Daily recognition over support period

**Accounting Treatment**:

```
Scenario: Annual Support Plan
Amount: $24,000/year
Duration: 12 months

JOURNAL ENTRIES:

Year Start (January 1):
  DR Cash                          $24,000
  CR Deferred Revenue              $24,000

Daily Recognition (365 days in year):
  DR Deferred Revenue              $65.75 ($24,000 ÷ 365)
  CR Support Revenue               $65.75

Monthly Summary (January 31):
  Support Revenue: $2,000 (31 days × $65.75)
  Deferred Revenue: $22,000 (remaining months)

ASC 606 Note:
  Customer has satisfied performance obligation each day
  (support is available 24/7 for that day)
```

---

### 2.3 Contract Modifications & Changes

**Definition**: When customer adds, removes, or changes services mid-contract

**ASC 606 Treatment**: Contract modifications are treated as:
- Part of the existing contract (if related to performance obligations), OR
- Separate contract (if adds distinct goods/services)

**Accounting Treatment**:

```
Scenario: Customer on $2,000/month contract adds users ($500/month)
Modification Date: Day 15 of month (mid-month change)

BEFORE (Day 1-14):
  Monthly rate: $2,000 = $66.67/day

AFTER (Day 15-31):
  New monthly rate: $2,500 = $83.33/day

JOURNAL ENTRIES:

Day 15 (Modification accepted):
  DR Deferred Revenue              $500
  CR Deferred Revenue - Add-On     $500
  (Deferred the incremental monthly charge)

Daily Recognition (Day 15-31, 17 days):
  Day 1-14: $66.67 × 14 = $933.33 (original pricing)
  Day 15-31: $83.33 × 17 = $1,416.67 (new pricing)
  Total Month: $2,350

Month-End Summary:
  Revenue: $2,350 (prorated for mid-month change)
  Deferred Revenue: Original deferred + additional
```

**Documentation Requirements**:

- Date of modification
- Scope of change (added/removed services)
- New contract terms
- Approval from sales and customer
- Updated revenue schedule

---

### 2.4 Refunds & Credits

**Refund Policy**: 30-day money-back guarantee

**ASC 606 Treatment**: Refunds are variable consideration (create liability)

**Accounting Treatment**:

```
Scenario: Customer requests refund within 30 days
Original Invoice: January 5, $10,000 monthly SaaS
Refund Request: January 20 (15 days into month)

JOURNAL ENTRIES:

Day 5 (Invoice - Initial Recognition):
  DR Cash                          $10,000
  CR Deferred Revenue              $10,000

Days 6-20 (Daily Recognition - 15 days):
  Daily: $333.33 × 15 = $5,000 recognized

Day 20 (Refund Request - Reverse Unearned):
  Step 1: Reverse remaining recognition liability
    DR Deferred Revenue            $5,000
    CR SaaS Revenue (contra)        $5,000

  Step 2: Issue refund
    DR Refund Payable              $5,000
    CR Cash                        $5,000

Final Result:
  Net Revenue Recognized: $5,000 (for 15 days of service)
  Cash Returned to Customer: $5,000 (for 15 days not used)
  Net Cash Impact: $0 (refund issued in same month)
```

**Refund Reserve**:

If refund probability is high (e.g., 30% of customers refund), establish reserve:

```
Month-End Adjustment:
  Revenue for month: $100,000
  Estimated refund rate: 3%
  Refund reserve: $3,000

  DR Refund Expense               $3,000
  CR Refund Reserve (Liability)   $3,000

This reduces month's revenue to conservative $97,000
Actual refunds offset against reserve (no double-counting)
```

---

### 2.5 Multi-Year Contracts

**Challenge**: Customer pays upfront for multiple years, but revenue recognized monthly

**ASC 606 Treatment**: Revenue recognized over performance obligation fulfillment period (12-24 months)

**Accounting Treatment**:

```
Scenario: 24-Month Contract
Contract Amount: $240,000 ($10,000/month)
Payment Terms: Upfront (entire $240,000)

JOURNAL ENTRIES:

Month 1 (January):
  Invoice & Payment Receipt:
    DR Cash                       $240,000
    CR Deferred Revenue           $240,000

  Daily Recognition (30 days):
    Daily: $333.33 × 30 = $10,000

  Month-End Balance Sheet:
    Cash:                         $240,000
    Deferred Revenue:             $230,000 (remaining 23 months)

Month 2 (February):
  Daily Recognition continues:
    Revenue: $10,000
    Deferred Revenue: $220,000

Year-End (December):
  Annual Revenue: $120,000 (12 months × $10,000)
  Deferred Revenue: $120,000 (remaining year 2)

Investor Communication:
  Q1 Revenue: $30,000
  Q1 Cash Collected: $240,000
  Difference explains deferred revenue on balance sheet
```

**Investor Communication**:

Multi-year contracts require special attention in investor materials:

```
INVESTOR COMMUNICATION TEMPLATE:

"During the period, we received $240,000 in customer cash for multi-year
contracts. Under ASC 606, revenue is recognized over the contract term
as performance obligations are satisfied.

Deferred Revenue Impact:
  - Cash collected: $240,000
  - Revenue recognized (Year 1): $120,000
  - Deferred Revenue (Year 2): $120,000
  - This represents high-quality revenue that will convert to cash and
    recognized revenue in future periods."
```

---

### 2.6 Revenue Recognition Schedule & Audit Process

**Quarterly ASC 606 Review** (Weeks 1-2 of following quarter)

**Checklist**:

```
□ Contract Review
  - All contracts from quarter classified correctly
  - Performance obligations clearly identified
  - Transaction prices determined per ASC 606

□ Performance Obligation Audit
  - Services/products delivered matched to revenue recognized
  - Milestone dates verified
  - Work completion documented

□ Deferred Revenue Reconciliation
  DR Deferred Revenue account from beginning to ending balance
  Verify: Beginning balance + new contracts - revenue recognized = ending balance

□ ASC 606 Adjustments
  Review all manual journal entries (variable consideration, modifications)
  Verify accuracy and supporting documentation

□ External Accountant Review
  Present quarterly summary to Big Four firm
  Request sign-off on revenue treatment

□ Board Reporting
  Revenue by category (SaaS, services, setup fees)
  Top 10 customer contracts
  Deferred revenue by contract (next 12 months)
  Year-to-date revenue vs forecast
```

---

## 3. INTERNAL FINANCIAL CONTROLS

### 3.1 Invoice-to-Cash Cycle

**Process Flowchart**:

```
INVOICE CREATION
  ├─ Sales Manager enters customer details in Stripe/QBO
  ├─ System auto-calculates based on pricing table
  └─ Amount, dates, tax rate reviewed

INVOICE APPROVAL
  ├─ Billing Manager reviews for accuracy
  ├─ Checks: Customer, service, amount, tax, terms
  └─ Approves (Finance Director approval for >$25k)

INVOICE DELIVERY
  ├─ Auto-email to customer (Stripe or QBO)
  ├─ Payment link included
  └─ Terms stated: Net 30, late fees, payment methods

PAYMENT RECEIPT
  ├─ Stripe/processor receives payment
  ├─ Auto-deposits to business bank account
  └─ Webhook triggers in accounting system

RECONCILIATION
  ├─ Stripe deposits match bank deposits (daily)
  ├─ Invoice marked as paid
  └─ Revenue recognized (automatic or manual)

A/R FOLLOW-UP
  ├─ Day 30: Gentle reminder email
  ├─ Day 45: Escalated notice
  ├─ Day 60: Phone call (sales team)
  └─ Day 90: Collections agency
```

**Controls**:

1. **Segregation of Duties**
   - Creation (Sales)
   - Approval (Finance)
   - Collection (Billing)
   - Reconciliation (Accounting)

2. **Authorization Matrix**
   - <$5,000: Billing Manager approval
   - $5,000-$25,000: Finance Director approval
   - >$25,000: CFO approval

3. **Documentation**
   - All invoices saved in document repository (linked to contract)
   - Payment proof attached to record
   - Reconciliation notes recorded

---

### 3.2 Account Reconciliation

**Daily Bank Reconciliation**:

```
Frequency: Daily (before 5 PM)
Responsible: Finance Director

Process:
  1. Download latest transactions from bank
  2. Match to QuickBooks deposits
  3. Identify unmatched items
     - Pending deposits (haven't cleared)
     - Pending checks (not yet cashed)
     - Errors (investigate)
  4. Record reconciling items if needed
  5. Verify bank balance = QBO balance
  6. Sign-off on daily reconciliation

Exception Handling:
  - Unmatched > 5 days: Escalate to bank
  - Discrepancy > $100: Investigate immediately
  - Duplicate deposits: Reverse and correct

Control: Bank reconciliation never complete until
         bank balance = QBO balance (no exceptions)
```

**Credit Card Reconciliation**:

```
Frequency: Monthly (by 5th of following month)
Cards: Business Amex, Chase, etc.

Process:
  1. Download credit card statement
  2. Match each charge to QBO entry
  3. Match each credit to QBO entry
  4. Verify totals match
  5. Record reconciliation in audit trail
  6. Categorize expenses (if not done at time of charge)
  7. Flag unusual charges for investigation

Exception Handling:
  - Unauthorized charges: File dispute immediately
  - Missing charges: Follow up with vendor
  - Duplicate charges: Contact vendor
```

**Stripe Reconciliation**:

```
Frequency: Daily
Elements: Invoices, Payments, Refunds, Fees

Process:
  1. Export Stripe payment report (daily)
  2. For each transaction:
     - Match to QBO invoice
     - Verify amount (gross, fees, net)
     - Verify date
  3. Verify Stripe deposit = sum of net payments
  4. Record reconciliation

Key Validations:
  - All paid invoices have matching Stripe payments
  - No duplicate charges
  - Fee calculation correct (2.9% + $0.30)
  - Deposits occur within 2 business days

Note: Stripe → Bank reconciliation
      Stripe deposits appear on bank statement within 2 days
      Match Stripe deposit to bank line item
```

---

### 3.3 Access & Authorization

**User Role Matrix**:

```
┌──────────────────┬──────┬────────┬─────────┬──────┬────────┐
│ Role             │ View │ Create │ Approve │ Edit │ Delete │
├──────────────────┼──────┼────────┼─────────┼──────┼────────┤
│ CEO              │ ✓    │ ✓      │ ✓       │ ✓    │ ✓      │
│ CFO              │ ✓    │ ✓      │ ✓       │ ✓    │ ✓      │
│ Finance Director │ ✓    │ ✓      │ ✓       │ ✓    │        │
│ Billing Manager  │ ✓    │ ✓      │        │ ✓    │        │
│ Accountant       │ ✓    │        │        │      │        │
│ Sales Manager    │ View │ Create │        │ Edit │        │
│                  │ own  │ quotes │        │ own  │        │
└──────────────────┴──────┴────────┴─────────┴──────┴────────┘

Implementation:
  - QuickBooks: User roles defined in Settings > Users & Roles
  - Stripe: Sub-accounts per role (Finance, Billing, Sales)
  - Passwords: 12+ characters, 2FA enabled for all accounts
  - Access Review: Quarterly (confirm access still needed)
```

---

### 3.4 Audit Trail

**Audit Log Requirements**:

```
For every transaction, record:
  ✓ User who created record
  ✓ Date & time of creation
  ✓ Description of change
  ✓ Dollar amount
  ✓ Account affected
  ✓ Any approvals
  ✓ Any modifications with dates

QuickBooks Audit Trail:
  - Enabled by default (Settings > Account & Settings > Audit Log)
  - Captures all user actions
  - Can filter by user, date, account type
  - Reports exported for external audit

Manual Journal Entry Log:
  Maintained in separate spreadsheet:
    - Date entered
    - Debit/Credit accounts
    - Amount
    - Explanation
    - Entered by (signature or approval)
    - Reviewed by (Finance Director)

Policy:
  - No manual entries >$1,000 without approval
  - All ASC 606 adjustments documented
  - Quarterly review of unusual journal entries
```

---

## 4. BILLING & COLLECTIONS PROCEDURES

### 4.1 Customer Contract Management

**Required Contract Elements**:

```
Every customer contract must include:

1. SERVICE DESCRIPTION
   - What is being provided (SaaS access, services hours, support)
   - Scope and limitations

2. PRICING & PAYMENT TERMS
   - Monthly/annual amount
   - Payment due date (Net 30, Net 60, etc.)
   - Payment methods accepted
   - Late fees (1.5% monthly, 18% APR)
   - Discounts (if any)

3. TERM & RENEWAL
   - Contract start date
   - Contract end date
   - Renewal terms (auto-renewal? notice period?)
   - Termination terms

4. PERFORMANCE OBLIGATIONS (ASC 606)
   - For SaaS: "Continuous access to platform during term"
   - For services: "Completion of stated milestones"
   - For support: "Response time SLA"

5. LIMITATION OF LIABILITY
   - Max liability cap (often 12 months fees)
   - Excluded damages (consequential, indirect)

6. CONFIDENTIALITY & IP
   - Data protection provisions
   - Ownership of work product

7. PAYMENT TERMS (DETAILED)
   - Invoice frequency (monthly, annual)
   - Payment method (ACH, credit card, check, wire)
   - Currency (USD, EUR, etc.)
   - Multi-year discount (if applicable)
```

**Contract Template** (Minimum):

```
SERVICE AGREEMENT

This Service Agreement ("Agreement") is entered into as of [DATE]
between [COMPANY] ("Provider") and [CUSTOMER] ("Customer").

1. SERVICE
Provider will provide [DESCRIPTION] to Customer

2. TERM & PRICING
Term: [START DATE] to [END DATE]
Price: $[AMOUNT] per [MONTH/YEAR]
Payment Terms: Due within 30 days of invoice

3. PAYMENT
Customer will pay invoices via [METHOD: ACH/Card/Wire]
Late payment: 1.5% monthly interest (18% APR)
Taxes: Customer responsible for all applicable taxes

4. TERMINATION
Either party may terminate with 30 days written notice
Upon termination: No refunds for prior months paid

5. CONFIDENTIALITY
Both parties maintain confidentiality of proprietary information

Signed: ___________________  Date: ___________________
        (Customer)

        ___________________  Date: ___________________
        (Provider)
```

---

### 4.2 Invoicing Process

**Invoice Issuance**:

```
MONTHLY INVOICING (for recurring customers):

Trigger: 1st of month at 12:01 AM
  - Stripe auto-creates invoices for all active subscriptions
  - QBO receives webhook notification
  - Customer receives email with payment link

ANNUAL INVOICING (for annual prepay customers):

Trigger: Customer anniversary date
  - Billing Manager creates annual invoice in Stripe
  - Amount calculated from contract terms
  - Finance Director reviews before sending

CUSTOM INVOICING (for project-based services):

Process:
  1. Project milestone completed (per SOW)
  2. Project Manager notifies Billing Manager
  3. Billing Manager creates invoice for completed milestone
  4. Finance Director approves (if >$25k)
  5. Invoice sent via Stripe/email
```

**Invoice Information**:

```
Standard Invoice Must Include:

┌─────────────────────────────────────────────────┐
│ INVOICE                                         │
│ Invoice #: INV-2026-00001                      │
│ Invoice Date: 1/15/2026                        │
│ Due Date: 2/14/2026 (Net 30)                   │
├─────────────────────────────────────────────────┤
│ BILL TO:                                        │
│ [Customer Name]                                 │
│ [Address]                                       │
├─────────────────────────────────────────────────┤
│ Description              | QTY | Unit Price | Amount
│ ─────────────────────────────────────────────  │
│ Monthly SaaS (Jan)       | 1   | $99.00    | $99.00
│ Professional Services    | 10  | $250      | $2,500
│ ─────────────────────────────────────────────  │
│                          Subtotal:     $2,599.00
│                          Tax (7.25%):  $188.43
│                          TOTAL DUE:    $2,787.43
├─────────────────────────────────────────────────┤
│ PAYMENT METHODS:                                │
│ 1. Credit Card: [Stripe Link]                  │
│ 2. ACH: [Bank Details]                         │
│ 3. Wire: [Wire Instructions]                   │
├─────────────────────────────────────────────────┤
│ TERMS: Net 30                                   │
│ Late Fees: 1.5% monthly (18% APR)              │
│ Contact: billing@company.com | 1-800-XXX-XXXX │
└─────────────────────────────────────────────────┘
```

---

### 4.3 Collections & Dunning

**Payment Collection Process**:

```
TIMELINE FOR CUSTOMER PAYMENT:

Day 0 (Invoice Date)
  └─ Invoice sent via email + Stripe payment link

Day 30 (Due Date)
  └─ Payment expected (Net 30 terms)

Day 35 (Soft Reminder)
  ├─ Automated email: "Your invoice is now due"
  ├─ Resend payment link
  └─ Friendly tone (no late fees yet)

Day 45 (First Escalation)
  ├─ Automated email: "Payment now 15 days overdue"
  ├─ Include late fee calculation (if applicable)
  └─ Request immediate payment

Day 50 (Stripe Automatic Retry)
  ├─ If payment method on file: Attempt charge
  ├─ Retry 3x over 5 days
  └─ Email customer if fails

Day 60 (Manual Follow-Up)
  ├─ Sales Manager calls customer (friendly tone)
  ├─ Ask about payment issues
  ├─ If problem: Work out payment plan
  └─ Document conversation

Day 90 (Collections)
  ├─ If still unpaid: Escalate to collections agency
  ├─ Or: Legal action (if contract >$10k)
  └─ Write off as bad debt (with supervisor approval)

EXCEPTION: If customer disputes charges
  ├─ Pause collections immediately
  ├─ Sales team investigates (invoice accuracy, service delivery)
  ├─ Credit if warranted
  └─ Update accounting accordingly
```

**Dunning Strategy** (Retry logic):

```
Configure Stripe Dunning Settings:

Automated Retry Schedule:
  Attempt 1: Day of initial charge failure (if ACH)
  Attempt 2: 3 days later
  Attempt 3: 5 days later
  Attempt 4: 7 days later (final)

Email Notifications:
  After 1st failure: Gentle reminder
  After 2nd failure: Urgent notice + alternate payment method
  After 4th failure: Final notice (subscription will be canceled)

Subscription Management:
  - After 4 failed attempts: Downgrade to free tier (if available)
  - Or: Pause subscription (customer must reinstate)
  - Or: Cancel subscription (with 7-day warning)

Manual Escalation:
  - If paying customer with 1 failed charge: Reach out proactively
  - Offer alternate payment method
  - Skip dunning emails if in contact with customer
```

---

### 4.4 Bad Debt & Write-Off

**Bad Debt Policy**:

```
Threshold: Any invoice unpaid for 90+ days

Procedure:
  1. Confirm collection is no longer possible
     - Customer unresponsive
     - Collections agency gave up
     - Customer in bankruptcy
  2. Finance Director approves write-off
  3. Create journal entry:
       DR Bad Debt Expense        $[Amount]
       CR Accounts Receivable     $[Amount]
  4. Update reserve if maintained
  5. Remove from active A/R report

Documentation:
  - Date of write-off decision
  - Reason (collections failed, bankruptcy, etc.)
  - Amount
  - Approving manager signature
  - Record in audit trail

Reserve Calculation:
  If 2-3% of customers never pay:
    Month Revenue: $100,000
    Estimated bad debt: 2.5%
    Reserve: $2,500

    Journal entry:
      DR Bad Debt Expense           $2,500
      CR Bad Debt Reserve (liability) $2,500

    When actual write-off occurs:
      DR Bad Debt Reserve           $[Actual amount]
      CR Accounts Receivable        $[Actual amount]
      (No P&L impact - already reserved)
```

---

## 5. FINANCIAL CLOSE & REPORTING

### 5.1 Monthly Close Procedures

**Due Date**: 5 business days after month-end (e.g., Jan 31 → by Feb 5)

**Checklist**:

```
MONTH 1 (Days 1-2 after month-end)

□ Bank Reconciliation
  - Download latest bank transactions
  - Match to QBO entries
  - Verify balances match
  - Investigate any discrepancies >$100
  - Signed off by Finance Director

□ Credit Card Reconciliation
  - Download statement for all company cards
  - Verify expenses categorized correctly
  - Match to corresponding invoices (if reimbursable)
  - Look for unusual charges
  - Flag for CFO if any concerns

□ Stripe Reconciliation
  - Export daily transaction reports
  - Verify all paid invoices have Stripe payments
  - Verify fees calculated correctly
  - Match Stripe deposits to bank deposits
  - No unresolved differences

MONTH 2 (Days 2-3 after month-end)

□ A/R Review
  - Aged A/R report (by customer)
  - Identify customers >30 days overdue
  - Flag collection issues to sales team
  - Update bad debt reserve estimate
  - Verify A/R balance reconciles to GL

□ Deferred Revenue Reconciliation
  - Verify beginning balance (equals prior month ending)
  - Add new customer invoices (liability created)
  - Subtract revenue recognized during month
  - Calculate ending balance
  - Reconcile Deferred Revenue GL account to detail
  - Document any adjustments (refunds, modifications)

□ Manual Journal Entries
  - Review all manual entries (>$100)
  - Verify supporting documentation attached
  - Ensure proper account coding
  - Check for errors or inconsistencies
  - Get approval if not already done

MONTH 3 (Days 3-5 after month-end)

□ Financial Statements
  - Generate P&L (Income Statement)
  - Generate Balance Sheet
  - Review for reasonableness
  - Check for unusual variances vs prior month
  - Investigate items >10% variance

□ Variance Analysis
  - Compare actual vs budget (if budget exists)
  - Explain variances >$5,000
  - Identify trends (revenue growth, expense growth)
  - Document explanations for board/investor reporting

□ Audit Trail Review
  - QBO audit log report
  - Verify all transactions documented
  - Check for unusual entries
  - Confirm all users had legitimate access
  - Flag suspicious activity

□ Final Review & Sign-Off
  - Finance Director reviews all items
  - CFO approves financial statements
  - Both sign close checklist
  - Store all documentation for audit

TIME TARGET: Complete by Day 5 of following month
GOAL: Enable board/investor reporting within 5 days of month-end
```

---

### 5.2 Financial Statements

**P&L (Income Statement)**:

```
For Month Ended: January 31, 2026

REVENUE
  SaaS Subscriptions              $45,000
  Professional Services           $25,000
  Setup Fees                      $5,000
  ───────────────────────────────────────
  Total Revenue                   $75,000

COST OF REVENUE
  Cloud Infrastructure            $4,000
  Payment Processing Fees         $4,350
  Direct Labor                    $8,000
  ───────────────────────────────────────
  Total COGS                      $16,350
  ───────────────────────────────────────
GROSS PROFIT                      $58,650 (78.2%)

OPERATING EXPENSES
  Sales & Marketing              $12,000
  Research & Development         $15,000
  General & Administrative        $10,000
  ───────────────────────────────────────
  Total OpEx                      $37,000

OPERATING INCOME (EBIT)           $21,650 (28.9%)

Other Income/Expense
  Interest Expense                 ($500)
  ───────────────────────────────────────

NET INCOME (Before Tax)           $21,150 (28.2%)
  Estimated Tax (25%)             ($5,288)
  ───────────────────────────────────────
NET INCOME (After Tax)            $15,862 (21.2%)
```

**Balance Sheet**:

```
As of: January 31, 2026

ASSETS
  Cash                            $165,000
  Accounts Receivable             $30,250
  Bad Debt Reserve                ($250)
  ──────────────────────────────
  Net A/R                         $30,000

  Computer Equipment              $25,000
  Accumulated Depreciation        ($2,500)
  ──────────────────────────────
  Net Equipment                   $22,500

  Software & Intangible Assets    $50,000
  Accumulated Amortization        ($5,000)
  ──────────────────────────────
  Net Intangibles                 $45,000
  ──────────────────────────────
TOTAL ASSETS                      $262,500

LIABILITIES
  Accounts Payable                $8,000
  Accrued Expenses                $3,000
  Deferred Revenue                $95,000
  ──────────────────────────────
  Total Liabilities               $106,000

EQUITY
  Common Stock                    $100,000
  Retained Earnings               $50,000
  Net Income (Month)              $15,862
  ──────────────────────────────
  Total Equity                    $165,862

TOTAL LIABILITIES & EQUITY        $271,862

Note: May not balance if timing differences exist
      (e.g., pending deposits, outstanding checks)
```

---

### 5.3 Board Reporting

**Monthly Board Report** (by 5th of following month):

```
EXECUTIVE SUMMARY (1 page)

Month: January 2026
Revenue: $75,000 (On plan / Above plan / Below plan)
Gross Margin: 78.2%
Net Income: $15,862
Key Metrics:
  - Active Customers: 18
  - Monthly Recurring Revenue: $47,500
  - Churn Rate: 5.3%
  - Days Sales Outstanding: 12 days

DETAILED FINANCIALS (1-2 pages)
  - P&L with variance to budget
  - Key metrics dashboard
  - Cash position & runway

HIGHLIGHTS & CHALLENGES (1 page)
  - Major customer wins
  - Any operational issues
  - Forecast adjustment (if needed)
  - Action items for next month

APPENDIX
  - Full P&L & Balance Sheet
  - Detailed revenue breakdown
  - A/R aging report
  - Month-over-month trends
```

---

## 6. TAX COMPLIANCE POLICY

### 6.1 Sales Tax

**US Sales Tax Obligations**:

```
Nexus Analysis: Determine which states we owe sales tax

States where company has presence:
  ✓ California (headquarters)
  ✓ New York (office + customers)
  ✓ Texas (customers only)
  ✓ Other states: Based on revenue threshold

Sales Tax Rate (varies by jurisdiction):
  California:    7.25% (state) + local (up to 2.25%)
  New York:      4% (state) + local (up to 4.875%)
  Texas:         6.25% (state) + local (up to 2%)

SaaS Sales Tax:
  - Generally taxable in most states
  - Some states exempt SaaS (research which apply)
  - Implementation services: Usually taxable
  - Support plans: Usually taxable

Configuration in Stripe:
  1. Navigate to Settings > Tax Rates
  2. Create tax rate for each jurisdiction
  3. Apply to products/prices
  4. Auto-calculate on invoices

Filing Requirements:
  California: Quarterly (Form 101 CDTFA)
  New York: Monthly (Form ST-100)
  Texas: Quarterly or monthly

Process:
  1. Export sales report by state (monthly)
  2. Calculate tax owed (not collected = pay us)
  3. File tax return by due date
  4. Pay tax authority by due date
  5. Record payment in accounting system
```

---

### 6.2 International Tax (VAT/GST)

**European Union - VAT**:

```
VAT Threshold: €10,000 revenue/year

Rates:
  Standard: 21% (most countries)
  Reduced: 5-17% (specific goods/services)
  Zero: 0% (some services, exports)

SaaS Treatment:
  - Generally 21% VAT applies
  - Unless customer is outside EU + business customer
    (then reverse charge applies)

Configuration:
  1. Collect customer EU VAT IDs
  2. If valid EU VAT ID: No VAT charged (reverse charge)
  3. If no VAT ID: Charge 21%
  4. Record VAT liability quarterly

Filing:
  - Quarterly OSS (One-Stop-Shop) filing
  - Report sales to each EU country
  - Pay VAT owed to EU tax authority
```

**United Kingdom - VAT**:

```
Rates:
  SaaS: 0% VAT (reverse charge for business customers)
  Services: 20% standard rate

Reverse Charge:
  - If customer is UK business: No VAT
  - If customer is UK consumer: 20% VAT
  - Documentation: Collect VAT ID or verify

Filing:
  - Quarterly MTD (Making Tax Digital)
  - Report sales and VAT
```

**Canada - GST/PST**:

```
Threshold: C$30,000 revenue/year

Federal GST: 5%
Provincial Sales Tax: 0-15% (by province)

SaaS: 5% GST applies (most provinces)

Collection:
  1. Determine customer location (province)
  2. Apply correct PST rate (if applicable)
  3. Include in invoice
  4. Remit quarterly

Filing:
  - Quarterly GST return
  - By-province PST returns
  - Report both collected and paid tax
```

---

### 6.3 Income Tax Planning

**Federal Income Tax**:

```
Filing Status: [C-Corporation / S-Corporation / LLC]

Estimated Tax Payments (if profitable):
  - Quarterly payments (April 15, June 15, Sept 15, Dec 15)
  - Calculate based on estimated annual profit
  - Pay IRS (Form 1040-ES for self-employed)
  - Avoid penalties for underpayment

Tax Planning:
  - Acceleration of expenses (before year-end)
  - Timing of revenue (defer if possible)
  - Equipment purchases (depreciation, Section 179)
  - Contractor vs employee classification
  - R&D tax credits (if applicable)

Annual Return:
  - Form 1120 (C-Corp) or
  - Form 1120-S (S-Corp) or
  - Form 1040 Schedule C (Sole Prop)
  - Due: March 15 (or Sept 15 if extension)
```

**State Income Tax**:

```
California:
  - Corporate tax: 8.84% on net income
  - Minimum tax: $800/year (even if loss)
  - Due: March 15

New York:
  - Corporate tax: 6.85% on net income
  - Due: March 15

Texas:
  - No state income tax
  - Franchise tax: Based on revenue (complex calc)
```

---

### 6.4 Tax Calendar

```
QUARTERLY:
  April 15:   Q1 Estimated Tax Payment (Federal)
  April 20:   California Sales Tax Return Due
  May 15:     New York Sales Tax Return Due
  June 15:    Q2 Estimated Tax Payment (Federal)
  Sept 15:    Q3 Estimated Tax Payment (Federal)
  Oct 20:     California Sales Tax Return Due
  Dec 15:     Q4 Estimated Tax Payment (Federal)

ANNUAL:
  Jan 31:     W-2 & 1099 Filings (contractors)
  March 15:   Income Tax Return Due (Federal + State)
  Sept 15:    Extended deadline (if filed Form 4868)

INTERNATIONAL:
  Quarterly OSS filing (if EU nexus)
  Monthly if UK nexus
```

---

## 7. CHART OF ACCOUNTS

### Complete Account Listing

**(See detailed account structure in section 2 of WEEK_5_6_FINANCIAL_SETUP.md)**

Revenue accounts (1000-1999)
COGS accounts (2000-2999)
Operating expense accounts (3000-5999)
Other income/expense (6000-7999)
Equity accounts (8000+)

---

## 8. STRIPE INTEGRATION & CONFIGURATION

### 8.1 Stripe Account Setup

**Stripe Dashboard Configuration**:

```
1. PRODUCTS & PRICING
   - Create product: "SaaS Monthly"
   - Create price: $99/month (recurring)
   - Create product: "SaaS Annual"
   - Create price: $1,080/year (recurring)
   - Create product: "Implementation Services"
   - Create price: Custom (not recurring)

2. TAX RATES
   - Create tax rate for each jurisdiction
   - Map to products
   - Auto-calculate on invoices

3. CUSTOMERS
   - Import customer list
   - Set default payment method (if available)
   - Add custom fields (account number, etc.)

4. WEBHOOKS
   - invoice.created
   - invoice.paid
   - invoice.payment_failed
   - charge.failed
   - customer.subscription.updated
   - customer.subscription.deleted
```

### 8.2 QuickBooks Integration

**Connection Process**:

```
1. QuickBooks: Settings > Apps & Integrations > Search "Stripe"
2. Click "Stripe" app
3. Authorize Stripe account (OAuth)
4. Map Stripe accounts to QBO accounts:
   - Stripe deposits → Bank account
   - Stripe fees → Payment processing fees expense
   - Invoices → Revenue accounts
5. Test: Create sample invoice, verify appears in QBO
6. Enable auto-reconciliation
```

**Journal Entry Mapping**:

```
When customer pays $100 SaaS invoice via Stripe:

Stripe sees:
  - Gross charge: $100
  - Fee (2.9% + $0.30): $3.20
  - Net deposit: $96.80

QuickBooks records:
  DR Cash (Bank Account)              $96.80
  DR Payment Processing Fee Expense   $3.20
  CR SaaS Revenue                     $100.00

Stripe webhook triggers revenue recognition:
  DR Deferred Revenue (Liability)     $100.00
  CR SaaS Revenue                     $100.00
  (Only if using deferred revenue model - daily recognition)
```

---

## 9. RISK MANAGEMENT & MITIGATION

### 9.1 Revenue Recognition Risks

```
Risk 1: Over-recognizing Revenue (Fraud Risk)
  Control: Quarterly Big Four accountant review
  Mitigation: ASC 606 policy strictly enforced

Risk 2: Under-recognizing Revenue (Conservative Bias)
  Control: Policy clearly defines recognition trigger
  Mitigation: Revenue review meeting monthly

Risk 3: Contract Ambiguity
  Control: Standardized SOW template
  Mitigation: All SOWs reviewed before invoicing

Risk 4: Performance Obligation Misclassification
  Control: ASC 606 training for sales team
  Mitigation: Finance Director reviews all new contract types

Risk 5: Significant Judgment (Variable Pricing, Discounts)
  Control: Document judgment in audit trail
  Mitigation: CFO approval for >10% discount
```

### 9.2 Billing & Collections Risks

```
Risk 1: Payment Processor Downtime
  Control: Secondary payment gateway (PayPal)
  Mitigation: Customer notified; grace period offered

Risk 2: Customer Non-Payment
  Control: 30-day dunning campaign
  Mitigation: Bad debt reserve; collections agency

Risk 3: Tax Calculation Error
  Control: Tax software validation
  Mitigation: Quarterly accountant review

Risk 4: Fraud (Fake Payments, Chargebacks)
  Control: Stripe fraud prevention
  Mitigation: Monitor chargeback rates; escalate if >1%
```

---

## 10. AUDIT PROCEDURES & DOCUMENTATION

### 10.1 Annual Audit Preparation

**Documentation Package** (prepared for Big Four auditors):

```
FOLDER: /financial-audit/

1. ASC 606 Policy Documentation
   - Revenue recognition policy (this manual)
   - Judgments & estimates log
   - Contract samples (SaaS, services, setup fees)

2. Internal Controls
   - Invoice approval workflow
   - A/R management procedures
   - Bank reconciliation checklist
   - Monthly close procedures

3. Stripe Integration
   - Webhook configuration
   - Tax rate setup
   - Reconciliation procedures

4. QuickBooks Configuration
   - Chart of accounts
   - User access permissions
   - Integration settings

5. Journal Entry Log
   - All manual entries (date, account, amount, reason)
   - ASC 606 adjustments documented
   - Approvals evidenced

6. Customer Contracts
   - Master customer list
   - Signed SOWs
   - Amendment log
   - Revenue by customer (for year)

7. Monthly Financial Statements
   - All P&Ls (Jan-Dec)
   - All Balance Sheets (Jan-Dec)
   - Monthly close checklists

8. Tax Filings
   - Sales tax returns (all states)
   - Income tax return
   - Any audit correspondence

9. Supporting Documentation
   - Bank statements (all accounts)
   - Credit card statements
   - Vendor invoices
   - Payment processor reports
```

### 10.2 Audit Readiness Checklist

```
□ All financial records organized by category
□ All manual journal entries documented with reason
□ Chart of accounts reconciliation completed
□ Revenue recognition policy up-to-date
□ Customer contracts file complete
□ Bank/credit card reconciliations done
□ A/R aging analysis prepared
□ Deferred revenue reconciliation complete
□ Bad debt reserve analysis prepared
□ Internal controls documentation provided
□ Management representations letter prepared
□ CFO & Finance Director available for interview
□ Access to accounting system granted
□ Sample transactions identified for testing
```

---

## APPENDICES

### Appendix A: ASC 606 Terminology

| Term | Definition |
|------|-----------|
| **Performance Obligation** | A promise to transfer goods or services to customer |
| **Contract Asset** | Customer owes payment but company has not yet delivered |
| **Contract Liability** | Company received payment but has not yet delivered (Deferred Revenue) |
| **Variable Consideration** | Refunds, discounts, penalties - amounts not yet certain |
| **Over Time Revenue** | Recognized as performance obligation is satisfied gradually |
| **Point-in-Time Revenue** | Recognized when performance obligation is fully satisfied at a moment |

### Appendix B: Key Accounting Journals

**SaaS Subscription Revenue**:
```
Invoice (Customer prepays):
  DR Cash
  CR Deferred Revenue

Daily Recognition:
  DR Deferred Revenue
  CR Revenue
```

**Professional Services (Milestone)**:
```
Milestone Complete:
  DR Accounts Receivable
  CR Implementation Revenue

Invoice/Payment:
  DR Cash
  CR Accounts Receivable
```

**Refund Processing**:
```
Reverse Prior Recognition:
  DR Revenue (contra)
  CR Deferred Revenue

Issue Refund:
  DR Refund Payable
  CR Cash
```

### Appendix C: Contact Information

```
External Accountant (Big Four):
  Firm: [Name]
  Contact: [Name, Title]
  Phone: [Number]
  Email: [Email]
  Quarterly Review Scheduled: [Date]
  Annual Audit Scheduled: [Date]

Stripe Support:
  Account Manager: [Name]
  Phone: [Number]
  Dashboard: https://dashboard.stripe.com

QuickBooks Support:
  Phone: 1-800-391-0929
  Online Help: help.quickbooks.intuit.com
```

---

## SIGN-OFF PAGE

This Accounting Policy Manual has been reviewed and approved by:

**Finance Director**: _________________ Date: _________

**CFO**: _________________ Date: _________

**CEO**: _________________ Date: _________

**External Accountant**: _________________ Date: _________

**Board of Directors**: _________________ Date: _________

---

**Document Control**: This manual should be reviewed annually and updated as business changes require. Changes should be approved by CFO and external accountant before implementation.

**Version History**:
- v1.0.0 (Jan 26, 2026): Initial policy document, ASC 606 compliance established
