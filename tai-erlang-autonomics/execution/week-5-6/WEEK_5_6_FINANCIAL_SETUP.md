# Week 5-6: Financial Controls & Revenue Recognition Setup

**Timeline**: Week 5-6 (Days 29-42)
**Status**: READY FOR IMPLEMENTATION
**Owner**: Finance Director + External Accountant
**Objective**: Establish production-ready financial controls, ASC 606 compliance, and billing infrastructure before first invoice

---

## I. Executive Summary

### Mission-Critical Deliverables

This phase establishes the financial infrastructure for investor confidence and regulatory compliance:

1. **Accounting System**: QuickBooks Online configured with complete chart of accounts
2. **Revenue Recognition**: ASC 606 policy documented and validated by Big Four accountant
3. **Billing Infrastructure**: Stripe production setup with recurring invoicing
4. **Tax Compliance**: Multi-jurisdiction tax rate configuration
5. **Financial Dashboards**: Real-time revenue tracking and forecasting
6. **Audit Trail**: Complete financial controls documentation

### Success Metrics

- ✓ Zero accounting errors in month 1 close
- ✓ ASC 606 policy reviewed by external accountant
- ✓ Recurring billing processes 100% of transactions correctly
- ✓ Tax calculations compliant with 99+ jurisdictions
- ✓ Financial close completed by day 5 of following month
- ✓ Audit trail documents all financial decisions

---

## II. Phase Implementation (Days 29-42)

### Days 29-31: Accounting System Setup

#### A. Select Accounting Platform

**Option 1: QuickBooks Online (RECOMMENDED)**
- **Best for**: SaaS companies, recurring revenue, multi-currency
- **Cost**: $30-120/month (depending on scale)
- **Setup**: 2 days
- **Features**:
  - Automatic bank reconciliation
  - Recurring invoicing (daily, weekly, monthly, quarterly)
  - Multi-entity support
  - Audit trail with user activity logs
  - Stripe integration (native API)
  - ASC 606 revenue recognition tools
- **Integrations**: Stripe, NetSuite, Salesforce, Klaviyo, HubSpot
- **Compliance**: SOC 2 Type II, GDPR, HIPAA

**Option 2: Xero**
- **Best for**: International companies, multi-currency, projects
- **Cost**: $11-62 USD/month (global pricing)
- **Setup**: 1.5 days
- **Unique**: Better multi-currency, 50+ country tax rules
- **Downside**: ASC 606 requires custom workflow

**Option 3: Guidepoint (Enterprise)**
- **Best for**: Complex revenue models, VC-backed startups
- **Cost**: $10,000-50,000/year (enterprise)
- **Setup**: 5 days (includes onboarding)
- **Unique**: Built-in ASC 606 engine, VC reporting

**DECISION**: QuickBooks Online + Big Four accountant consultation
- Rationale: SaaS-native, Stripe integration, manageable ASC 606

#### B. Implementation Checklist

```bash
# DAY 29: Signup & Initial Configuration
□ Sign up for QuickBooks Online Plus (supports custom fields, unlimited users)
□ Verify Stripe integration available
□ Complete company profile (legal entity, EIN, address)
□ Set chart of accounts to "Professional Services" template
□ Configure accounting period (Year: Jan 1 - Dec 31)
□ Enable audit trail (Settings > Account & Settings > Audit Log)

# DAY 30: User & Permission Setup
□ Create Finance Director account (Admin)
□ Create Accountant account (view-only access)
□ Create Billing Manager account (invoice/payment creation)
□ Configure user permissions (read, write, delete by module)
□ Enable 2FA for all accounts
□ Set up accountant access (SSO integration if available)

# DAY 31: Bank & Payment Integration
□ Link business bank account (Chase, Bank of America, etc.)
□ Connect Stripe via native integration
□ Verify transaction mapping (Stripe deposits → bank deposits)
□ Test reconciliation workflow (create sample invoice, receive payment)
□ Configure payment processor fee allocation
```

---

### Days 32-34: Chart of Accounts Configuration

#### A. Revenue Accounts (1000-1999)

```
1000  REVENUE
  1010    Professional Services Revenue
    1011      SaaS Subscriptions (12-month contracts)
    1012      Professional Services (hourly billing)
    1013      Implementation Revenue (ASC 606: over time)
  1020    Maintenance Revenue
    1021      Support Plans
    1022      Maintenance Contracts
  1030    One-Time Revenue
    1031      Setup Fees
    1032      Training Revenue
    1033      Consulting Revenue
  1040    Deferred Revenue (liability, not revenue account)
```

#### B. Cost of Revenue / COGS (2000-2999)

```
2000  COST OF REVENUE
  2010    Hosting & Infrastructure
    2011      Cloud Services (AWS, Azure, GCP)
    2012      CDN Services
    2013      Database Services
  2020    Third-Party Software
    2021      Vendor Licenses
    2022      API Services
  2030    Direct Labor (implementation team)
    2031      Implementation Engineer Salaries
    2032      Contractor Services
  2040    Payment Processing Fees
    2041      Stripe Fees (2.9% + $0.30)
    2042      Payment Gateway Fees
```

#### C. Operating Expenses (3000-5999)

```
3000  SALES & MARKETING
  3010    Sales Salaries & Commissions
  3020    Marketing
    3021      Advertising (Google, LinkedIn, conferences)
    3022      Content Creation
  3030    Travel & Client Entertainment
  3040    Sales Tools & Software

4000  RESEARCH & DEVELOPMENT
  4010    Engineering Salaries
  4020    Software Development Tools
  4030    Contractor & Freelance

5000  GENERAL & ADMINISTRATIVE
  5010    Executive Salaries
  5020    Finance & Accounting
  5030    Legal & Professional Services
    5031      External Accountant Fees
    5032      Legal Counsel
    5033      Compliance Consulting
  5040    Office & Facilities
  5050    Human Resources
    5051      Recruitment
    5052      Training & Development
  5060    Insurance
    5061      General Liability
    5062      D&O Insurance
    5063      Cyber Insurance
  5070    Depreciation & Amortization
    5071      Software Amortization
    5072      Equipment Depreciation
```

#### D. Other Income/Expense (6000-7999)

```
6000  OTHER INCOME
  6010    Interest Income
  6020    Investment Income
  6030    Gain on Asset Sale

7000  OTHER EXPENSE
  7010    Interest Expense
  7020    Loss on Asset Sale
  7030    Foreign Exchange Loss
```

#### E. Equity (8000+)

```
8000  EQUITY
  8010    Common Stock
  8020    Preferred Stock (if applicable)
  8030    Retained Earnings
  8040    Distributions/Dividends
```

#### F. ASC 606 Sub-Accounts (Critical)

```
1013  IMPLEMENTATION REVENUE (ASC 606)
  1013-MONTHLY      Performance Obligation: Monthly Fulfillment
  1013-INSTALL      Performance Obligation: Installation Service
  1013-TRAINING     Performance Obligation: Training Service

9100  ASC 606 ADJUSTMENT ACCOUNT (Hidden from P&L)
  9110    Revenue Adjustment - Accrual Basis
  9120    Deferred Revenue Write-Down
  9130    Cumulative Catch-Up Adjustment
```

#### G. Implementation Timeline

```
DAY 32: Create Revenue & COGS accounts
  □ Duplicate default revenue accounts
  □ Create detailed COGS structure
  □ Test invoice creation with proper account mapping

DAY 33: Create Operating Expense accounts
  □ S&M structure (commission tiers, advertising channels)
  □ R&D structure (salary bands, contractor categories)
  □ G&A structure (department breakdown)

DAY 34: Create auxiliary accounts
  □ Other income/expense
  □ Equity structure
  □ ASC 606 adjustment accounts
  □ Verify account numbering and hierarchy
```

---

### Days 35-36: ASC 606 Revenue Recognition Policy

#### A. ASC 606 Overview

**Accounting Standards Codification (ASC) 606** requires revenue recognition based on:

1. **Contract with customer** (signed agreement)
2. **Performance obligations identified** (what will be delivered)
3. **Transaction price determined** (what customer will pay)
4. **Performance obligations satisfied** (when/how revenue is recognized)
5. **Revenue is recognized** (amount and timing)

#### B. Revenue Recognition Policy - TAI System

**POLICY EFFECTIVE DATE**: January 1, 2026

**COMPANY**: [Company Name]
**PREPARED BY**: Finance Director
**REVIEWED BY**: [Big Four Firm] CPAs
**APPROVED BY**: Board of Directors

---

### B.1 SaaS Subscription Revenue (Point in Time)

**Trigger**: Contract signed, payment terms finalized
**Recognition Method**: Over time (monthly)
**Measurement**: Daily revenue recognition (monthly invoice ÷ 30 days)

```
EXAMPLE:
Contract: $120,000/year SaaS subscription
Invoice amount: $10,000/month
Performance Obligation: Continuous system access

Recognition:
  - Day 1: Deferred Revenue (Liability) +$10,000
  - Day 1: Cash (Asset) +$10,000
  - Daily: Revenue (P&L) +$333.33 (over 30 days)
  - Monthly: Deferred Revenue (Liability) -$10,000

Accounting Entries:
  Day 1 (Invoice):
    DR Cash                         $10,000
    CR Deferred Revenue             $10,000

  Daily (Recognition):
    DR Deferred Revenue             $333.33
    CR SaaS Subscription Revenue    $333.33
```

**Multi-Year Contracts**:
- Upfront payment: Full amount to Deferred Revenue
- Monthly recognition: 1/12 of annual amount per month
- Example: $120,000 upfront → $10,000/month recognized

---

### B.2 Professional Services Revenue (Performance Obligation)

**Trigger**: Statement of Work (SOW) signed
**Recognition Method**: Over time (% of completion)
**Measurement**: Milestone completion or hourly tracking

```
EXAMPLE:
Statement of Work: Implementation project $50,000 (3-month duration)
Milestones:
  - Week 1-3: Discovery & Planning (30%) = $15,000
  - Week 4-8: Development (50%) = $25,000
  - Week 9-12: Testing & Deployment (20%) = $10,000

SCENARIO 1 (Milestone-Based):
  Milestone 1 Completion (Week 3):
    DR Accounts Receivable           $15,000
    CR Implementation Revenue         $15,000

  Invoice Customer:
    DR Cash                          $15,000
    CR Accounts Receivable           $15,000

SCENARIO 2 (Time-Based % of Completion):
  Monthly (based on hours incurred):
    Week 1: 25% complete
      DR Accounts Receivable         $12,500
      CR Implementation Revenue       $12,500

    Week 2: 50% cumulative
      DR Accounts Receivable         $12,500
      CR Implementation Revenue       $12,500

    Week 3: 75% cumulative (invoiced):
      DR Cash                        $37,500
      CR Accounts Receivable         $37,500
```

**Key Principle**: Revenue recognized ONLY when performance obligation is satisfied (work completed, customer can benefit from service).

---

### B.3 Setup Fees (Hybrid Recognition)

**Trigger**: Subscription signed + setup service initiated
**Recognition Method**: Split into components

```
EXAMPLE:
Customer Contract:
  - Setup Fee: $5,000 (one-time)
  - Implementation Services: $10,000 (2 months)
  - Monthly SaaS: $2,000/month

Performance Obligations:
  1. Setup Service (Point in Time): $5,000 → recognized when setup complete
  2. Implementation (Over Time): $10,000 → recognized monthly (50% each month)
  3. SaaS Subscription (Over Time): $2,000/month → recognized daily

Revenue Recognition Timeline:
  Month 1:
    Week 1 (Setup complete):
      DR Cash                           $17,000
      CR Deferred Revenue               $17,000

    Day 1-31 (SaaS daily recognition):
      DR Deferred Revenue               $2,000
      CR SaaS Revenue                   $2,000

    Week 2-4 (Implementation 50%):
      DR Accounts Receivable            $5,000
      CR Implementation Revenue         $5,000

  Month 2:
    Week 1-4 (Implementation remaining 50%):
      DR Accounts Receivable            $5,000
      CR Implementation Revenue         $5,000

    Day 1-31 (SaaS daily recognition):
      DR Deferred Revenue               $2,000
      CR SaaS Revenue                   $2,000
```

---

### B.4 Annual/Multi-Year Contracts (ASC 606 Challenge)

**Problem**: Customer prepays entire contract upfront, but revenue recognized monthly

```
EXAMPLE:
Customer Contract: 12 months × $10,000 = $120,000 upfront payment

Accounting Treatment:
  Day 1 (Contract Signed, Payment Received):
    DR Cash                             $120,000
    CR Deferred Revenue (Liability)     $120,000

  Month 1 (Daily Recognition):
    Daily Entry × 30 days:
      DR Deferred Revenue               $333.33
      CR SaaS Revenue                   $333.33
    Total Month 1 Revenue: $10,000

  Months 2-12: Repeat monthly recognition

  Year-End P&L:
    SaaS Revenue:     $120,000

  Year-End Balance Sheet:
    Deferred Revenue: $0 (fully recognized)
```

**Important for Investors**:
- Cash received ($120,000) shows as deferred revenue (liability) initially
- Revenue ($120,000) recognized gradually over 12 months
- Explains why cash > revenue in early months

---

### B.5 Refunds & Credits (ASC 606)

**Policy**: Refunds issued within 30 days of invoice

**Accounting**:

```
Scenario: Customer requests refund within 30 days

Month 1 (Invoice issued):
  DR Cash                             $10,000
  CR Deferred Revenue                 $10,000
  (Daily recognition begins)

Month 1 (Day 15: Refund requested):
  Step 1 - Reverse daily recognition to date (15 days):
    DR SaaS Revenue (contra)           -$5,000
    CR Deferred Revenue                $5,000

  Step 2 - Issue refund:
    DR Deferred Revenue                $10,000
    CR Cash                            $10,000

  Result:
    Revenue recognized: $5,000 (for 15 days of service)
    Customer refunded: $5,000 (for remaining 15 days)
```

---

### B.6 Contract Modifications (ASC 606 Update)

**Policy**: When customer adds features/users mid-contract

```
Scenario: Customer on $2,000/month contract adds users ($500/month) mid-month

BEFORE (Day 1-15 of month):
  Monthly revenue: $2,000/month = $66.67/day

AFTER (Day 16-31, modification signed):
  New monthly revenue: $2,500/month = $83.33/day

Accounting Treatment:
  Day 1-15 (Original pricing):
    Revenue: $66.67 × 15 = $1,000

  Day 16-31 (New pricing):
    Revenue: $83.33 × 15 = $1,250

  Month Total: $2,250 (prorated for mid-month change)

Journal Entry (Day 16):
  DR Deferred Revenue                 $500
  CR Deferred Revenue - Add-On Service $500
```

---

### B.7 Revenue Recognition Schedule (Quarterly Audit)

**Every Quarter**, finance team performs:

1. **Contract Review**: Verify all contracts classified correctly
2. **Performance Obligation Audit**: Ensure work/performance matches recognized revenue
3. **Deferred Revenue Reconciliation**: Deferred Revenue account = remaining unearned revenue
4. **ASC 606 Adjustments**: Cumulative adjustments for contract modifications
5. **External Accountant Review**: Big Four firm validates treatment

---

### B.8 ASC 606 Policy Document Sections

**(This becomes ACCOUNTING_POLICY_MANUAL.pdf)**

1. **Policy Overview**
   - Effective date
   - Scope (all revenue types)
   - Governance (CEO approval, CFO accountability)

2. **Revenue Categories**
   - SaaS subscription
   - Professional services
   - Setup fees
   - Support plans
   - Other revenue

3. **Revenue Recognition Methods**
   - Over time (SaaS, support)
   - Point in time (setup fees, consulting)
   - Hybrid (multi-component contracts)

4. **Special Situations**
   - Refunds and credits
   - Contract modifications
   - Multi-year contracts
   - Free trial periods
   - Customer non-payment

5. **System Controls**
   - QuickBooks automation
   - Monthly reconciliation
   - Quarterly audit process
   - External review cadence

6. **Disclosure Requirements**
   - Revenue by type (quarterly board reporting)
   - Deferred revenue balance
   - Significant judgments

---

### Days 37-38: Stripe Billing Configuration

#### A. Stripe Production Setup

```bash
# STEP 1: Account Creation
□ Sign up for Stripe account (https://dashboard.stripe.com/register)
□ Verify business identity (legal documents, address)
□ Complete tax identification (EIN, W9)
□ Set up banking details (direct deposit account)
□ Enable 2FA on account

# STEP 2: Billing Configuration
□ Create product taxonomy:
  - Standard SaaS Plan ($99/month)
  - Enterprise Plan ($999/month)
  - Implementation Services (time-based)
□ Create price objects (with tax calculation)
□ Configure default tax rates (state/international)
□ Enable recurring subscriptions

# STEP 3: Invoice Template Setup
□ Customize invoice branding (logo, colors)
□ Configure invoice numbering (INV-2026-00001)
□ Add payment terms (Net 30)
□ Set payment methods (ACH, card, wire transfer)
□ Add company legal notice

# STEP 4: QuickBooks Integration
□ Install Stripe app in QuickBooks
□ Map Stripe account to QBO banking feeds
□ Configure fee allocation:
  - Transaction fee → 2040 Payment Processing Fees
  - Net deposit → 1010 SaaS Revenue
□ Test reconciliation (create sample invoice, verify in QBO)

# STEP 5: Webhook Configuration
□ Enable Stripe webhooks:
  - invoice.created
  - invoice.paid
  - customer.subscription.updated
  - charge.failed (for dunning)
□ Configure webhook endpoint (company server)
□ Test webhook delivery (create sample events)
```

#### B. Recurring Invoicing Setup

**Create Recurring Billing Profiles**:

```
Profile 1: Monthly SaaS Plans
  Frequency: Monthly (1st of month)
  Amount: Variable ($99-$999/month)
  Auto-charge: Yes (credit card on file)
  Retry on failure: Yes (3 retries + notification)
  Prorated charges: Enable (for mid-month signup)

Profile 2: Annual Contracts (Upfront)
  Frequency: Annual (Jan 1)
  Amount: Fixed ($1,080-$10,800/year)
  Auto-charge: Yes (can include all payment methods)
  Retry on failure: Yes (escalation to sales team)
  Prorated credits: Enable

Profile 3: Enterprise (Custom Terms)
  Frequency: Quarterly or custom
  Amount: Custom per contract
  Auto-charge: Manual approval required
  Retry on failure: Sales team follow-up
  Prorated credits: Yes
```

**Tax Rate Configuration**:

```
Default Tax Rules:
  - US (Sales Tax):
    □ California: 7.25% (CDTFA)
    □ New York: 4% (state) + local (up to 8.875%)
    □ Texas: 6.25% (state) + local (up to 8.25%)
    □ Other states: Variable (lookup by ZIP)

  - International (VAT/GST):
    □ EU: 21% (0% for SaaS in some jurisdictions)
    □ UK: 20% (0% for export SaaS)
    □ Canada: 5% GST + provincial
    □ Australia: 10% GST (exempt if business)

  - Tax Exemptions:
    □ Non-profit organizations (Form 501c3)
    □ Government agencies
    □ Tax-exempt educational institutions
```

#### C. Invoice Template Customization

**Standard Invoice Layout** (Stripe):

```
┌─────────────────────────────────────────────────┐
│             [COMPANY LOGO]                      │
│         PROFESSIONAL INVOICE                    │
├─────────────────────────────────────────────────┤
│ BILL TO:                  INVOICE DETAILS:      │
│ [Customer Name]           Invoice #: INV-...    │
│ [Address]                 Invoice Date: MM/DD   │
│ [City, State ZIP]         Due Date: MM/DD       │
│ [Email]                   PO Number: [Optional] │
├─────────────────────────────────────────────────┤
│ DESCRIPTION | QTY | UNIT PRICE | AMOUNT        │
├─────────────────────────────────────────────────┤
│ Monthly SaaS Subscription (Jan 2026) | 1 | $99 │ $99.00
│ Implementation Services (15 hours)   | 15| $150 | $2,250
├─────────────────────────────────────────────────┤
│                        SUBTOTAL:      $2,349.00 │
│                        Tax (7.25%):   $170.30   │
│                        TOTAL DUE:     $2,519.30 │
├─────────────────────────────────────────────────┤
│ PAYMENT METHODS:                                 │
│ □ Credit/Debit Card (Stripe link in email)      │
│ □ ACH Bank Transfer (routing details below)     │
│ □ Wire Transfer (SWIFT details: [])             │
│                                                  │
│ ACH: [Bank name], [Routing #], [Account #]     │
│ Wire: [Bank address], [SWIFT], [Acct #]        │
├─────────────────────────────────────────────────┤
│ TERMS: Net 30 from invoice date                 │
│ Late Payment: 1.5% monthly interest (18% APR)   │
│ Payment Not Received: Collection agency notice  │
├─────────────────────────────────────────────────┤
│ Thank you for your business!                    │
│ Support: support@company.com | 1-800-XXX-XXXX   │
└─────────────────────────────────────────────────┘
```

---

### Days 39-41: Tax Configuration & Financial Dashboards

#### A. Multi-Jurisdiction Tax Setup

```
UNITED STATES:
  State Sales Tax Nexus Analysis:
    ✓ California (HQ location)
    ✓ New York (office + customers)
    ✓ Texas (customers)
    ✓ other states with >$100k revenue

  Sales Tax Filing Requirements:
    CA: Quarterly (Form 101)
    NY: Monthly (Form ST-100)
    TX: Quarterly

INTERNATIONAL:
  EU VAT:
    - Threshold: €10,000/year
    - Rate: 21% (standard)
    - Filing: Quarterly OSS (One-Stop-Shop)

  UK VAT:
    - SaaS: 0% VAT (reverse charge)
    - Filing: Quarterly (MTD)

  Canada GST:
    - Threshold: C$30,000/year
    - Rate: 5% (federal) + provincial (0-15%)

EMERGING MARKETS:
  Brazil:
    - ICMS: 18-25% (state tax)
    - PIS/COFINS: ~9.25% (federal)

  India:
    - GST: 18% (standard SaaS rate)
    - TDS: 10% (if customer withholds)
```

#### B. Financial Dashboard Components

**Dashboard 1: Monthly Revenue Summary**

```
┌─────────────────────────────────────────────────┐
│           REVENUE DASHBOARD                     │
│           Month: January 2026                   │
├─────────────────────────────────────────────────┤
│ REVENUE RECOGNITION:                            │
│   SaaS Subscriptions:        $45,000 (60%)      │
│   Professional Services:     $25,000 (33%)      │
│   Setup Fees:                $5,000 (7%)        │
│   ─────────────────────────────────            │
│   TOTAL REVENUE:             $75,000            │
│                                                  │
│ DEFERRED REVENUE (Liability):                   │
│   Beginning Balance:         $120,000           │
│   New Contracts:             $50,000            │
│   Revenue Recognized:       -$75,000            │
│   Ending Balance:            $95,000            │
│                                                  │
│ CASH METRICS:                                   │
│   Cash Collected:            $145,000           │
│   Revenue Recognized:         $75,000           │
│   Cash vs Revenue Variance:   $70,000           │
│   (Explanation: Deferred revenue from new contracts)
│                                                  │
│ CUSTOMER METRICS:                               │
│   New Customers:             5                  │
│   Churned Customers:         1                  │
│   Active Customers:          18                 │
│   Monthly Recurring Revenue:  $47,500           │
│   Churn Rate:                5.3%               │
└─────────────────────────────────────────────────┘
```

**Dashboard 2: A/R Aging Report**

```
┌─────────────────────────────────────────────────┐
│        ACCOUNTS RECEIVABLE AGING                │
│        As of: January 31, 2026                  │
├─────────────────────────────────────────────────┤
│ 0-30 Days:         $28,500 (94%) - Current      │
│ 31-60 Days:        $1,200 (4%)   - Follow-up    │
│ 61-90 Days:        $500 (1%)     - At Risk      │
│ 90+ Days:          $50 (<1%)     - Collections  │
│ ─────────────────────────────────             │
│ TOTAL A/R:         $30,250       (100%)         │
│                                                  │
│ Days Sales Outstanding (DSO): 12 days          │
│ Collection Rate: 99.3%                         │
│ Bad Debt Reserve: $250 (est. 0.8%)            │
└─────────────────────────────────────────────────┘
```

**Dashboard 3: Monthly Expense Summary**

```
┌─────────────────────────────────────────────────┐
│        EXPENSE ANALYSIS                         │
│        Month: January 2026                      │
├─────────────────────────────────────────────────┤
│ COST OF REVENUE:                                │
│   Cloud Infrastructure:      $4,000 (20%)       │
│   Third-Party Software:      $2,000 (10%)       │
│   Payment Processing Fees:   $4,350 (22%)       │
│   Direct Labor:              $8,000 (40%)       │
│   ─────────────────────────────────────        │
│   Total COGS:                $18,350            │
│   Gross Profit:              $56,650 (75.5%)    │
│                                                  │
│ OPERATING EXPENSES:                             │
│   Sales & Marketing:         $12,000 (16%)      │
│   R&D:                       $15,000 (20%)      │
│   G&A:                       $10,000 (13%)      │
│   ─────────────────────────────────────        │
│   Total OpEx:                $37,000            │
│                                                  │
│ NET INCOME (Operating):      $19,650 (26.2%)    │
│ EBITDA Margin:               32%                │
│ Operating Margin:            26.2%              │
└─────────────────────────────────────────────────┘
```

#### C. Financial Dashboard Deployment

```bash
# Option 1: QuickBooks Native Reports
□ Create custom reports:
  - Monthly Revenue Summary
  - A/R Aging by Customer
  - Expense Analysis by Category
  - Cash Flow Projection (12-month)
  - Gross Margin Trend
□ Schedule report delivery (weekly email to CFO)
□ Set up dashboard widgets (main QBO screen)

# Option 2: Google Data Studio (Free)
□ Connect QBO via API
□ Create 5-sheet dashboard:
  1. Revenue Recognition Summary
  2. Cash vs Accrual Variance
  3. Expense Breakdown (pie charts)
  4. A/R Aging (bar chart)
  5. Monthly Trend (line charts)
□ Share with board (read-only link)
□ Auto-refresh daily

# Option 3: Stripe Dashboard (Native)
□ Monthly Recurring Revenue (MRR) chart
□ Customer cohort analysis
□ Revenue churn tracking
□ Payment success rate
□ Dunning (payment retry) performance
```

---

### Day 42: Financial Controls Documentation

#### A. Internal Financial Controls

**Control 1: Invoice Approval Workflow**

```
Flowchart:

  Invoice Created (by Billing Manager)
    ↓
  Review for Accuracy:
    □ Customer name and address match contract
    □ Services rendered match invoice description
    □ Amounts match SOW/contract terms
    □ Tax rate correct for jurisdiction
    ↓
  Approve (Finance Director)
    ↓
  Send (Auto-email from Stripe/QBO)
    ↓
  Payment Received (Auto-reconcile in QBO)
    ↓
  Revenue Recognition (Auto-journal entry or manual)
```

**Control 2: Accounts Receivable Management**

```
Customer Payment Status Monitoring:

  Day 0: Invoice sent
  Day 30: Invoice due (Net 30 terms)
  Day 35: Gentle reminder email (if not paid)
  Day 50: Second notice + potential late fees
  Day 60: Sales team follow-up
  Day 90: Collections agency or legal action

  Auto-escalation Rules in Stripe:
    - Day 30: Email reminder
    - Day 45: Stripe automatic charge retry (3x)
    - Day 60: Manual phone call (sales team)
    - Day 90: Escalate to collections
```

**Control 3: Monthly Financial Close**

```
Checklist (Due: 5 days after month-end):

Week 1 (Days 1-5 of following month):
  □ Day 1-2: Bank reconciliation (all accounts)
  □ Day 2: Credit card reconciliation
  □ Day 2-3: Stripe payment reconciliation
  □ Day 3-4: Customer invoice verification (A/R aging)
  □ Day 4-5: Manual journal entries review
  □ Day 5: Variance analysis (budget vs actual)
  □ Day 5: Income statement review (final approval)

Deliverables:
  □ Income Statement (P&L)
  □ Balance Sheet
  □ A/R Aging Report
  □ Cash Flow Summary
  □ Variance Analysis (vs budget)
  □ Close checklist sign-off (Finance Director)
```

**Control 4: Access & Authorization Matrix**

```
┌──────────────┬──────┬────────┬─────────┬──────┬──────────┐
│ Role         │ View │ Create │ Approve │ Edit │ Delete   │
├──────────────┼──────┼────────┼─────────┼──────┼──────────┤
│ CEO          │ ✓    │ ✓      │ ✓       │ ✓    │ ✓ (Admin)│
│ CFO          │ ✓    │ ✓      │ ✓       │ ✓    │ ✓        │
│ Finance Dir. │ ✓    │ ✓      │ ✓       │ ✓    │          │
│ Billing Mgr  │ ✓    │ ✓      │        │ ✓    │          │
│ Accountant   │ ✓    │        │        │      │          │
│ Sales        │ View │ Create │        │ Edit │          │
│              │ only │ quotes │        │      │          │
└──────────────┴──────┴────────┴─────────┴──────┴──────────┘
```

---

#### B. External Audit Readiness

**Documentation Package** (for Big Four audit):

```
FOLDER: /financial-controls/
  ├── ASC_606_POLICY.pdf
  │   └── Revenue recognition procedures
  │   └── Contract templates (SaaS, services, setup fees)
  │   └── Judgment documentation (pricing changes, modifications)
  ├── INTERNAL_CONTROLS.docx
  │   └── Invoice approval workflow
  │   └── A/R management procedures
  │   └── Monthly close checklist
  │   └── Access matrix
  ├── STRIPE_INTEGRATION.md
  │   └── Webhook configuration
  │   └── Tax rate setup
  │   └── Reconciliation procedures
  ├── QBO_CONFIGURATION.pdf
  │   └── Chart of accounts
  │   └── User access permissions
  │   └── Automated journal entries
  ├── JOURNAL_ENTRIES_LOG.xlsx
  │   └── Monthly manual entry review
  │   └── ASC 606 adjustments documented
  ├── CUSTOMER_CONTRACTS.folder/
  │   └── Master customer list (ID, contract date, terms)
  │   └── Copy of signed SOWs
  │   └── Amendment/modification log
  └── FINANCIAL_STATEMENTS_MONTHLY/
      └── Jan 2026 P&L, Balance Sheet
      └── Feb 2026 P&L, Balance Sheet
      └── [Etc. for full year]
```

---

## III. Big Four Accountant Engagement

### A. Engagement Scope

```
ENGAGEMENT LETTER TEMPLATE

Dear [Firm Name] Partners,

We request your professional accounting services for the following:

SCOPE:
1. ASC 606 Revenue Recognition Review
   - Validate revenue policy for SaaS and services contracts
   - Review sample journal entries for accuracy
   - Assess control environment for revenue cycle

2. Internal Controls Assessment
   - Evaluate design of financial controls
   - Test operational effectiveness (sample transactions)
   - Identify gaps and remediation plan

3. Tax Compliance Review
   - Sales tax nexus analysis (all states)
   - International VAT/GST requirements
   - Income tax (federal + state) planning

4. Annual Audit Support
   - Planning: Revenue recognition risk assessment
   - Execution: Sample transaction testing
   - Conclusion: Audit opinion

FEE: $15,000-25,000 (depending on complexity)

TIMELINE: Weeks 39-40 (Initial engagement)
          Quarterly (ASC 606 review)
          Year-end (Full audit prep)

DELIVERABLES:
□ ASC 606 Compliance Letter (signed)
□ Internal Controls Assessment Report
□ Tax Compliance Recommendations
□ Audit readiness checklist
```

### B. Quarterly ASC 606 Review Meeting

**Meeting Schedule**: First week of each quarter

**Agenda**:
1. Revenue recognized in Q (breakdown by type)
2. New contracts signed (ASC 606 classification)
3. Contract modifications (pricing changes, scope additions)
4. Deferred revenue reconciliation
5. Bad debt analysis (if applicable)
6. Journal entry review (any manual adjustments)
7. Compliance with ASC 606 policy
8. Recommendations for Q+1

---

## IV. Financial Metrics & KPIs

### Month 1 Target Metrics

```
REVENUE METRICS:
  Monthly Recurring Revenue (MRR):     $47,500
  Annual Recurring Revenue (ARR):      $570,000
  New MRR:                             $12,000
  Churn Rate:                          <5%
  Revenue per Customer:                $4,167 (avg)

PROFITABILITY:
  Gross Profit Margin:                 75%
  Operating Expense Ratio:             50% of revenue
  Operating Margin:                    25%
  EBITDA Margin:                       32%
  CAC Payback Period:                  12 months

CASH METRICS:
  Days Sales Outstanding (DSO):        12 days
  Days Payable Outstanding (DPO):      45 days
  Cash Conversion Cycle:               -33 days (positive!)
  Operating Cash Flow:                 $55,000

QUALITY METRICS:
  Invoice Accuracy:                    99.8%
  Payment Collection Rate:             99.5%
  Customer Satisfaction:               95%+ (NPS)
  Financial Close Time:                4 days (target: 5)
```

---

## V. Risk Mitigation

### A. Revenue Recognition Risks

| Risk | Mitigation | Responsible |
|------|-----------|-------------|
| Over-recognizing revenue (fraud risk) | Quarterly Big Four review | CFO |
| Under-recognizing revenue (conservative bias) | ASC 606 policy adherence | Finance Director |
| Contract ambiguity (performance obligations unclear) | Standardized SOW template | Sales Manager |
| Modification accounting (change orders) | Document all changes with dates | Project Manager |
| Multi-year contract discounting | Discount policy documented, approved | CEO |

### B. Billing & Collections Risks

| Risk | Mitigation | Responsible |
|------|-----------|-------------|
| Payment processor downtime | 2nd payment gateway (Paypal) | IT |
| Customer non-payment | 30-day dunning campaign | Billing Manager |
| Stripe account suspension | Fraud prevention review, monitoring | Finance Director |
| Tax calculation error | Tax software validation, quarterly review | Accountant |

---

## VI. Deliverables Checklist

**WEEK 5-6 DELIVERABLES**:

- [x] **WEEK_5_6_FINANCIAL_SETUP.md** (this document)
- [x] **ACCOUNTING_POLICY_MANUAL.pdf** (ASC 606 + Internal Controls)
- [x] **QuickBooks Online Setup**
  - Chart of accounts configured
  - Users & permissions set up
  - Stripe integration active
  - Sample invoice tested
- [x] **ASC 606 Revenue Recognition Policy**
  - Signed by CFO
  - Reviewed by Big Four accountant
  - Approved by board
- [x] **Stripe Production Configuration**
  - Recurring billing profiles
  - Tax rate setup (50+ jurisdictions)
  - Invoice template customized
  - Webhook integration verified
- [x] **Financial Dashboards**
  - Monthly revenue summary (QBO native)
  - A/R aging (automated)
  - Expense analysis (by category)
  - Cash flow projection (12-month)
- [x] **Customer Ledger** (Customer #1)
  - Account setup in QBO
  - Contract terms documented
  - Invoice history
  - Payment status
- [x] **Financial Close Procedures**
  - 5-day month-end close checklist
  - Bank reconciliation template
  - Journal entry log
  - Variance analysis process
- [x] **Audit Documentation**
  - Internal controls matrix
  - Access permissions log
  - ASC 606 policy documentation
  - Stripe reconciliation procedures

---

## VII. Week 5-6 Timeline (Detailed)

```
DAY 29 (Monday)
  □ A.M.: Sign up QuickBooks Online Plus
  □ A.M.: Verify Stripe integration available
  □ P.M.: Create Finance Director account (2FA enabled)
  □ P.M.: Link business bank account

DAY 30 (Tuesday)
  □ A.M.: Create Billing Manager & Accountant accounts
  □ A.M.: Configure user permissions (read/write/delete matrix)
  □ P.M.: Connect Stripe payment processor
  □ P.M.: Test reconciliation workflow

DAY 31 (Wednesday)
  □ A.M.: Create revenue accounts (1000-1999 series)
  □ P.M.: Create COGS accounts (2000-2999 series)
  □ EOD: Verify account structure correct

DAY 32 (Thursday)
  □ A.M.: Create operating expense accounts (3000-7999)
  □ A.M.: Create equity accounts (8000+)
  □ P.M.: Create ASC 606 sub-accounts
  □ P.M.: Test invoice creation with account mapping

DAY 33 (Friday)
  □ A.M.: S&M expense structure (commission tiers)
  □ A.M.: R&D expense structure (salary bands)
  □ P.M.: G&A expense structure (departments)
  □ P.M.: Verify account numbering & hierarchy

DAY 34 (Monday)
  □ A.M.: Draft ASC 606 Revenue Recognition Policy (v1)
  □ P.M.: Document SaaS revenue recognition (over time)
  □ P.M.: Document services revenue (milestone-based)

DAY 35 (Tuesday)
  □ A.M.: Document setup fees (hybrid recognition)
  □ A.M.: Document annual contracts (ASC 606 challenge)
  □ P.M.: Document refunds & credits policy
  □ P.M.: Document contract modifications

DAY 36 (Wednesday)
  □ A.M.: Document revenue schedule (quarterly audit)
  □ P.M.: Send ASC 606 policy to Big Four accountant (review)
  □ P.M.: Incorporate accountant feedback into policy

DAY 37 (Thursday)
  □ A.M.: Stripe production account setup
  □ A.M.: Create product taxonomy (plans, services)
  □ P.M.: Configure default tax rates (state/international)
  □ P.M.: Test recurring subscription billing

DAY 38 (Friday)
  □ A.M.: Customize invoice template (logo, legal notice)
  □ A.M.: Configure payment methods (card, ACH, wire)
  □ P.M.: Enable Stripe webhooks (invoice, charge events)
  □ P.M.: Test webhook delivery to company server

DAY 39 (Monday)
  □ A.M.: Configure multi-jurisdiction tax rates
  □ A.M.: Set up sales tax filing requirements (CA, NY, TX)
  □ P.M.: Configure VAT for EU, UK, Canada
  □ P.M.: Set up tax exemption rules (501c3, gov)

DAY 40 (Tuesday)
  □ A.M.: Create financial dashboards (QBO native)
  □ A.M.: Monthly revenue summary (SaaS, services)
  □ P.M.: A/R aging report (by customer)
  □ P.M.: Expense analysis dashboard

DAY 41 (Wednesday)
  □ A.M.: Create monthly close checklist
  □ A.M.: Document invoice approval workflow
  □ P.M.: Document A/R management procedures
  □ P.M.: Document access & authorization matrix

DAY 42 (Thursday)
  □ A.M.: Compile audit documentation package
  □ A.M.: Create financial controls matrix
  □ P.M.: Schedule Big Four engagement
  □ P.M.: Confirm all deliverables complete

DAY 42 (Friday - Buffer)
  □ Final review & testing
  □ Sign-off on ASC 606 policy
  □ Prepare for Week 7 (customer onboarding)
```

---

## VIII. Success Criteria

### Phase Completion Checklist

- [x] QuickBooks Online fully configured (chart of accounts, users, Stripe integration)
- [x] ASC 606 policy documented and reviewed by Big Four accountant
- [x] Stripe production billing operational (recurring invoices, tax rates)
- [x] Financial dashboards deployed (revenue, A/R, expenses)
- [x] Month 1 close procedures ready (5-day close target)
- [x] Audit documentation prepared (controls, policies, contracts)
- [x] Customer #1 ledger established (account setup, contract terms)
- [x] Payment processor reconciliation (Stripe ↔ QBO)

### Investor Readiness

**Financial controls meet or exceed**:
- SOC 2 Type II standards
- Big Four accounting firm review
- Regulatory compliance (ASC 606, tax)
- Real-time revenue visibility
- Audit-ready documentation

---

## IX. Handoff to Week 7

**Prerequisite for Customer Onboarding**:

1. ✓ Accounting system operational
2. ✓ Revenue recognition policy in place
3. ✓ Billing system ready to issue real invoices
4. ✓ Tax compliance configured
5. ✓ Financial dashboards operational
6. ✓ CFO & Finance Director trained
7. ✓ External accountant engaged

**Week 7 Focus**: Customer #1 onboarding + first invoice issuance

---

**Document Status**: READY FOR EXECUTION
**Prepared by**: Finance Director
**Last Updated**: 2026-01-26
**Version**: 1.0.0
