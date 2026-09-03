# Financial Metrics & Customer Ledger Template

**Status**: Ready for Week 7 Customer #1 Onboarding
**Updated**: January 26, 2026

---

## SECTION 1: MONTHLY FINANCIAL METRICS

### Financial Dashboard - Month 1 Targets

```
MONTH: January 2026

REVENUE METRICS
═════════════════════════════════════════════════════════

Monthly Recurring Revenue (MRR)
  Definition: Predictable revenue from active subscriptions
  Target: $47,500
  Formula: Sum of all active monthly subscriptions (normalized)
  Calculation:
    - SaaS Basic (10 customers × $99): $990
    - SaaS Professional (15 customers × $299): $4,485
    - SaaS Enterprise (1 customer × $1,000): $1,000
    - Support Plans (5 customers × $199): $995
    - Professional Services (ongoing, amortized): $40,030
    ──────────────────────────────────
    Total MRR: $47,500

Annual Recurring Revenue (ARR)
  Definition: MRR annualized
  Target: $570,000
  Formula: MRR × 12
  Calculation: $47,500 × 12 = $570,000

Reported Revenue (Month 1)
  Definition: Revenue recognized in January per ASC 606
  Target: $75,000
  Components:
    - SaaS subscriptions recognized: $45,000
    - Professional services recognized: $25,000
    - Setup fees: $5,000
    ──────────────────────────────────
    Total Revenue: $75,000

  Note: Reported revenue > MRR because professional services
        are over-time recognition, can spike in a month.

New MRR (from new customers)
  Definition: MRR added from new customer contracts
  Target: $12,000
  Customers added: 5
  Average: $2,400 per customer

Monthly Net Retention
  Definition: % of prior month MRR retained (after churn)
  Target: 95%+ (5% churn acceptable for SaaS)
  Formula: (MRR_end - churn_MRR + expansion_MRR) / MRR_begin
  Example:
    MRR Beginning Month: $45,000
    New MRR added: +$3,000
    Churn: -$2,000 (2 customers @ $1,000 each)
    ──────────────────────────────────
    MRR End of Month: $46,000
    Retention Rate: (46,000 - 2,000 + 0) / 45,000 = 97.8% ✓

Revenue per Customer (Average)
  Definition: Total revenue / active customers
  Target: $4,167/customer (on average)
  Calculation: $75,000 / 18 active customers = $4,167

Customer Acquisition Cost (CAC)
  Definition: Sales + marketing spend / new customers
  Target: $2,500 per customer
  Calculation: $12,500 sales/marketing / 5 new = $2,500
  Target CAC Payback: 12 months (standard for B2B SaaS)

Lifetime Value (LTV)
  Definition: Average revenue per customer × average lifetime
  Target: $60,000+ (12+ months at $4,167/month)
  LTV/CAC Ratio: Should be 3:1 or higher
  Example: $60,000 / $2,500 = 24:1 ✓ Excellent


PROFITABILITY METRICS
═════════════════════════════════════════════════════════

Gross Profit
  Total Revenue: $75,000
  COGS (cloud, payment fees, labor): -$16,350
  ──────────────────────────────────
  Gross Profit: $58,650

Gross Profit Margin
  Definition: Gross Profit / Revenue
  Target: 75%+
  Calculation: $58,650 / $75,000 = 78.2% ✓

Operating Expenses
  Sales & Marketing: $12,000
  Research & Development: $15,000
  General & Administrative: $10,000
  ──────────────────────────────────
  Total OpEx: $37,000

Operating Income (EBIT)
  Gross Profit: $58,650
  OpEx: -$37,000
  ──────────────────────────────────
  Operating Income: $21,650

Operating Margin
  Definition: Operating Income / Revenue
  Target: 20%+ (sustainable SaaS target)
  Calculation: $21,650 / $75,000 = 28.9% ✓

EBITDA
  Operating Income: $21,650
  Add back depreciation/amortization: $1,000
  ──────────────────────────────────
  EBITDA: $22,650

EBITDA Margin
  Definition: EBITDA / Revenue
  Target: 30%+
  Calculation: $22,650 / $75,000 = 30.2% ✓

Net Income (After Tax)
  Operating Income: $21,650
  Tax (25% est.): -$5,413
  ──────────────────────────────────
  Net Income: $16,237

Net Profit Margin
  Definition: Net Income / Revenue
  Target: 15%+
  Calculation: $16,237 / $75,000 = 21.6% ✓


CASH METRICS
═════════════════════════════════════════════════════════

Cash Collected (Actual)
  Definition: Cash received from customers
  Target: $145,000
  Components:
    - New customer prepayments: $100,000
    - Existing customer renewals: $45,000
    ──────────────────────────────────
    Total cash: $145,000

Revenue Recognized (Accrual)
  Definition: Revenue recognized per ASC 606
  Calculated Above: $75,000

Cash vs Revenue Variance
  Cash Collected: $145,000
  Revenue Recognized: -$75,000
  ──────────────────────────────────
  Variance: $70,000 (positive variance)

  Explanation: Deferred Revenue Liability
  When customers prepay (especially annual contracts),
  cash comes in before revenue is recognized.
  This shows up on balance sheet as "Deferred Revenue"
  (liability) and decreases as revenue is recognized monthly.

Days Sales Outstanding (DSO)
  Definition: How many days to collect payment
  Target: <30 days (faster is better)
  Calculation: (A/R / Revenue) × 30

  Assuming $30,000 A/R outstanding:
    DSO = ($30,000 / $75,000) × 30 = 12 days ✓

  Interpretation: On average, customers pay within 12 days.
  Industry standard for SaaS: 15-30 days

Days Payable Outstanding (DPO)
  Definition: How many days we take to pay vendors
  Target: 45+ days (longer is better, if vendors allow)
  Calculation: (Accounts Payable / COGS) × 30

  Assuming $8,000 A/P outstanding:
    DPO = ($8,000 / $16,350) × 30 = 14.7 days

  Interpretation: We pay vendors faster than we collect.
  This creates cash flow pressure (negative working capital).
  Action item: Negotiate longer payment terms with vendors.

Cash Conversion Cycle (CCC)
  Definition: Days to convert cash outlay to cash collection
  Formula: DSO + Inventory - DPO (no inventory for SaaS)
  Calculation: 12 + 0 - 14.7 = -2.7 days

  Interpretation: Negative CCC is GREAT for SaaS!
  Means we collect cash before we pay vendors.
  Creates natural working capital float.

Operating Cash Flow
  Revenue (accrual): $75,000
  COGS (paid): -$16,350
  OpEx (paid): -$35,000 (less one-time expenses)
  Changes in working capital: +$70,000 (deferred revenue)
  ──────────────────────────────────
  Operating Cash Flow: $93,650 (very strong!)


CUSTOMER METRICS
═════════════════════════════════════════════════════════

Total Active Customers
  Definition: Customers with active subscriptions
  Target: 18
  Breakdown:
    - SaaS Basic: 10
    - SaaS Professional: 15
    - SaaS Enterprise: 1
    - Support Only: 5
    Note: Some overlap (customers on multiple plans)

New Customers (Month 1)
  Definition: New contracts signed
  Target: 5
  Average contract value: $2,400 MRR

Churned Customers
  Definition: Customers who canceled
  Target: 1 (5.3% churn rate)
  Impact: Lost $2,000 MRR

Gross Churn Rate
  Definition: % of revenue lost (without considering expansion)
  Target: <5% (healthy SaaS benchmark)
  Calculation: $2,000 churn / $45,000 prior MRR = 4.4% ✓

Net Churn Rate (Including Expansion)
  Definition: Net change in MRR (after expansion from existing customers)
  Target: Negative (growing existing customers)
  Calculation: -$2,000 churn / $45,000 prior = -4.4%
  Plus: Expansion from existing customers = +1.5%
  Net: -2.9% (positive sign: growing)


UNIT ECONOMICS
═════════════════════════════════════════════════════════

Customer Acquisition Cost (CAC)
  Sales & Marketing Spend: $12,000
  New Customers: 5
  ──────────────────────────────────
  CAC per Customer: $2,400

CAC Payback Period
  Definition: How many months to recover CAC from customer revenue
  Target: <12 months
  Calculation:
    MRR per Customer: $2,400
    Monthly Gross Profit per Customer: $2,400 × 78.2% = $1,875
    Payback: CAC / Gross Profit = $2,400 / $1,875 = 1.28 months ✓

  Interpretation: Recover CAC in just 5 weeks! Very healthy.

Lifetime Value (LTV)
  Definition: Total profit extracted from average customer lifetime
  Target: LTV/CAC Ratio > 3:1

  Calculation:
    Average monthly revenue: $2,400
    Average gross profit margin: 78.2%
    Monthly gross profit: $1,875
    Average customer lifetime: 24 months
    Total LTV: $1,875 × 24 = $45,000

    LTV/CAC Ratio: $45,000 / $2,400 = 18.75:1 ✓

  Interpretation: Exceptional unit economics!
  For every dollar spent on acquisition, we get $18.75 in lifetime value.

Payback Multiple
  Definition: Total LTV / CAC
  Calculated Above: 18.75x
  Benchmark: 3x is good, 5x is excellent


KEY PERFORMANCE INDICATORS (KPIs) SUMMARY
═════════════════════════════════════════════════════════

Metric                          Target      Actual    Status
─────────────────────────────────────────────────────────────
MRR                           $47,500     $47,500     ✓ On Plan
ARR                          $570,000    $570,000     ✓ On Plan
Monthly Revenue               $75,000     $75,000     ✓ On Plan
Gross Margin                    75%+        78.2%     ✓ Exceeds
Operating Margin              20%+         28.9%     ✓ Exceeds
Net Margin                     15%+         21.6%     ✓ Exceeds
DSO                            30 days     12 days    ✓ Exceeds
Customer Churn                  <5%        4.4%       ✓ Good
CAC Payback                    12 months  1.28 months ✓ Excellent
LTV/CAC Ratio                  3:1        18.75:1     ✓ Exceptional
```

---

## SECTION 2: CUSTOMER LEDGER TEMPLATE

### Customer #1: First Production Customer

```
═════════════════════════════════════════════════════════════════
                    CUSTOMER MASTER RECORD
═════════════════════════════════════════════════════════════════

Customer Details
──────────────────────────────────────────────────────────────────
Customer ID:                CUST-000001
Customer Name:              [Company Name]
Industry:                   [Industry]
Employees:                  [Size]
Location:                   [City, State, Country]
Time Zone:                  [Timezone]
Contact Person:             [Name]
Title:                      [Title]
Email:                      [email@company.com]
Phone:                      [Phone]
Billing Email:              [billing@company.com]

Contract Information
──────────────────────────────────────────────────────────────────
Contract Start Date:        [Date]
Contract End Date:          [Date]
Contract Duration:          [# months]
Contract Type:              SaaS + Implementation Services
Contract Status:            ACTIVE

Pricing & Terms
──────────────────────────────────────────────────────────────────
Base SaaS Plan:             Professional Plan ($299/month)
Setup Fees:                 $5,000 (one-time)
Implementation Services:    $10,000 (2-month engagement)
Support Level:              Premium ($199/month)

Monthly Recurring Revenue (MRR)
  - SaaS Professional:      $299
  - Premium Support:        $199
  ─────────────────────────────────
  Total Monthly:            $498

Annual Revenue (Annualized)
  - Base annual MRR:        $498 × 12 = $5,976
  - Plus one-time (Y1):     $5,000 setup + $10,000 services = $15,000
  - Year 1 Total:           $20,976

Payment Terms:              Net 30
Payment Method:             Credit Card (Stripe)
Auto-Renewal:               Yes (annual)
Renewal Terms:              Same as current (subject to price change)

Sales Information
──────────────────────────────────────────────────────────────────
Sales Rep:                  [Name]
Close Date:                 [Date]
Deal Size:                  $20,976 (Year 1)
Proposal Date:              [Date]
Discount Applied:           5% volume discount = -$1,048
Net Deal Value:             $19,928

ASC 606 Revenue Recognition
──────────────────────────────────────────────────────────────────
Performance Obligation 1: Setup Fees
  Amount: $5,000
  Recognition: Point in time (when setup complete)
  Trigger: Customer account created, users provisioned
  Date: [Setup completion date]

Performance Obligation 2: SaaS Professional Plan
  Amount: $299/month
  Recognition: Over time (daily recognition)
  Trigger: Begins [Start date]
  Monthly recognition: $299 / 30 days = $9.97/day

Performance Obligation 3: Implementation Services
  Amount: $10,000
  Recognition: Over time (2 months, 50% per month)
  Month 1: $5,000
  Month 2: $5,000
  Trigger: Based on project completion milestones

Performance Obligation 4: Premium Support
  Amount: $199/month
  Recognition: Over time (daily recognition)
  Trigger: Begins [Start date]
  Monthly recognition: $199 / 30 days = $6.63/day

Initial Revenue Forecast (Year 1)
  Month 1 (Setup): $5,000 setup + $309 SaaS + $5,000 impl = $10,309
  Month 2: $498 MRR + $5,000 impl = $5,498
  Months 3-12: $498 × 10 = $4,980
  ─────────────────────────────────────────
  Year 1 Total Revenue: $20,787

Invoicing Schedule
──────────────────────────────────────────────────────────────────
Invoice 1: Setup + Impl Month 1
  Date: [Start date]
  Amount: $5,000 + $5,000 = $10,000
  Due: [Net 30 from invoice]
  Paid: [Status]

Invoice 2: Impl Month 2 + First Monthly Subscription
  Date: [Start date + 30 days]
  Amount: $5,000 + $498 = $5,498
  Due: [Net 30]
  Paid: [Status]

Recurring Monthly Invoices:
  Date: 1st of each month
  Amount: $498 (SaaS $299 + Support $199)
  Due: [Net 30]
  Frequency: Monthly, recurring

Payment History & Collections
──────────────────────────────────────────────────────────────────

Invoice #   Date        Amount    Due Date   Paid Date   Status
────────────────────────────────────────────────────────────────
INV-000001  [Date]      $10,000   [Date]     [Date]      PAID
INV-000002  [Date]      $5,498    [Date]     [Date]      PAID
INV-000003  [Date]      $498      [Date]     [Date]      PAID (if monthly)

Days Sales Outstanding (DSO)
  Invoice 1: [# days to pay]
  Invoice 2: [# days to pay]
  Average: [# days]

Total Invoiced (YTD):     $[Total]
Total Paid (YTD):         $[Total]
Accounts Receivable:      $[Outstanding]
Collection Issues:        None / [Issue description]

Financial Performance
──────────────────────────────────────────────────────────────────
Revenue Recognized (YTD)
  Setup fees:             $5,000
  SaaS recognition:       $[Monthly × months active]
  Implementation:         $[Milestones completed]
  Support recognition:    $[Monthly × months active]
  ─────────────────────────────────────────
  Total Revenue YTD:      $[Total]

Deferred Revenue (Liability)
  Prepayments received:   $[Amount]
  Revenue recognized:    -$[Amount]
  ─────────────────────────────────────────
  Remaining deferred:     $[Amount]

Cost of Revenue (Allocated)
  Hosting/Infrastructure: $[Amount]
  Support labor:          $[Amount]
  Payment processing fees: $[Amount]
  ─────────────────────────────────────────
  Total COGS:             $[Amount]

Gross Profit
  Revenue:                $[Amount]
  COGS:                  -$[Amount]
  ─────────────────────────────────────────
  Gross Profit:           $[Amount]
  Gross Margin:           [%]

Customer Health & Retention
──────────────────────────────────────────────────────────────────
Customer Health Score:      [Metric based on usage, support tickets]
Implementation Status:      [On track / At risk / Complete]
Adoption Rate:              [% of features in use]
Support Tickets (30d):      [#]
NPS (Net Promoter Score):   [Score 0-100]
Churn Risk:                 [Low / Medium / High]
Expansion Opportunity:      [Y/N] [Description]
Expected Renewal:           [Date, probability %]

Account Management
──────────────────────────────────────────────────────────────────
QBR Date (Quarterly):       [Scheduled]
Last QBR Notes:             [Summary]
Implementation Manager:     [Name]
Support Contact:            [Name]
Executive Sponsor (at TAI): [Name]
Next Review Date:           [Date]

Compliance & Documentation
──────────────────────────────────────────────────────────────────
Signed Contract Filed:      Yes □ / No □ Path: [Location]
SOW (if services):          Yes □ / No □ Path: [Location]
Data Processing Agreement:  Yes □ / No □ Path: [Location]
Security Questionnaire:     Completed □ In Progress □
SOC 2 Copy Requested:       Yes □ No □
Compliance Notes:           [Any security/compliance requirements]

Billing & Account Issues
──────────────────────────────────────────────────────────────────
Outstanding Issues:         [None / Issue list]
Billing Disputes:           [None / Dispute details]
Payment Method:
  □ Credit Card (on file)
  □ ACH Bank Transfer
  □ Wire Transfer
  □ Invoice (manual approval)
Payment Method Valid Until: [Expiration date]

=================================================================
                    LEDGER BALANCES
=================================================================

Account Summary (As of Month-End)

Account: 1011 SaaS Revenue
  Beginning Balance:        $0.00
  + Revenue recognized:     $[Amount]
  - Adjustments:            $[Amount]
  ─────────────────────────────────
  Ending Balance:           $[Amount]

Account: 1013 Implementation Revenue
  Beginning Balance:        $0.00
  + Revenue recognized:     $[Amount]
  - Adjustments:            $[Amount]
  ─────────────────────────────────
  Ending Balance:           $[Amount]

Account: 1031 Setup Fee Revenue
  Beginning Balance:        $0.00
  + Revenue recognized:     $5,000.00
  - Refunds:                $0.00
  ─────────────────────────────────
  Ending Balance:           $5,000.00

Account: 1031 Support Revenue
  Beginning Balance:        $0.00
  + Revenue recognized:     $[Amount]
  ─────────────────────────────────
  Ending Balance:           $[Amount]

Account: 1200 Accounts Receivable
  Beginning Balance:        $0.00
  + Invoices issued:        $[Amount]
  - Payments received:      $[Amount]
  - Write-offs:             $0.00
  ─────────────────────────────────
  Ending Balance:           $[Amount]

Account: 2100 Deferred Revenue (Liability)
  Beginning Balance:        $0.00
  + Customer prepayments:   $[Amount]
  - Revenue recognized:    -$[Amount]
  - Refunds issued:         $0.00
  ─────────────────────────────────
  Ending Balance:           $[Amount]

=================================================================
NOTES & ADDITIONAL CONTEXT
=================================================================

[Free text area for account notes, sales strategy, risks, etc.]

Last Updated:               [Date]
Updated By:                 [Name]
Next Review Date:           [Date]
Customer Status:            ACTIVE / AT RISK / INACTIVE
```

---

## SECTION 3: REVENUE RECOGNITION SCHEDULE

### Year 1 Revenue Schedule - Customer #1

```
═════════════════════════════════════════════════════════════════
        CUSTOMER #1 - YEAR 1 REVENUE RECOGNITION SCHEDULE
═════════════════════════════════════════════════════════════════

Contract Period: [Start Date] through [End Date]
Prepared: January 26, 2026

MONTHLY BREAKDOWN

Month 1: [Month/Year]
─────────────────────────────────────────────────────────────────
Setup Fees (Point in Time - Complete)
  Deferred Revenue at month start:          $5,000.00
  Revenue recognized (setup complete):     $5,000.00
  Deferred Revenue at month end:            $0.00

SaaS Professional Plan (Daily Recognition)
  Monthly subscription:                      $299.00
  Days in month:                             30
  Daily rate:                                $9.97
  Days of service provided:                  30
  Revenue recognized:                        $299.00
  Deferred Revenue at end:                   $0.00 (monthly billing)

Implementation Services (50% - Month 1 of 2)
  Total contract value:                      $10,000.00
  Month 1 (50% complete):                   $5,000.00
  Deferred Revenue at start:                 $10,000.00
  Revenue recognized (milestone 1):         $5,000.00
  Deferred Revenue at end:                   $5,000.00

Premium Support (Daily Recognition)
  Monthly subscription:                      $199.00
  Days in month:                             30
  Daily rate:                                $6.63
  Days of service provided:                  30
  Revenue recognized:                        $199.00
  Deferred Revenue at end:                   $0.00 (monthly billing)

─────────────────────────────────────────────────────────────────
Month 1 Summary:
  Total Revenue Recognized:                  $10,498.00
  Cash Received (upfront payment):           $15,000.00
  Deferred Revenue at month-end:            $5,000.00 (impl phase 2)

Explanation:
  - Cash ($15,000) > Revenue ($10,498) because customer prepaid
    for implementation and first month. Implementation revenue
    deferred until services delivered.
  - Deferred Revenue ($5,000) = remaining implementation phase.
    Will be recognized next month as phase 2 completes.


Month 2: [Month/Year]
─────────────────────────────────────────────────────────────────
Implementation Services (50% - Month 2 of 2)
  Month 2 (50% complete):                   $5,000.00
  Deferred Revenue at start:                 $5,000.00
  Revenue recognized (milestone 2):         $5,000.00
  Deferred Revenue at end:                   $0.00

SaaS Professional Plan
  Revenue recognized:                        $299.00

Premium Support
  Revenue recognized:                        $199.00

─────────────────────────────────────────────────────────────────
Month 2 Summary:
  Total Revenue Recognized:                  $5,498.00
  Cash Received:                             $498.00 (regular billing)
  Deferred Revenue at month-end:            $0.00

Explanation:
  - Implementation complete, all remaining revenue recognized
  - Customer now on recurring monthly billing
  - Regular invoicing cycle begins


Months 3-12: [Repeating]
─────────────────────────────────────────────────────────────────
Each month:

SaaS Professional Plan
  Revenue recognized:                        $299.00

Premium Support
  Revenue recognized:                        $199.00

─────────────────────────────────────────────────────────────────
Monthly Summary (Months 3-12):
  Total Revenue Recognized (per month):      $498.00
  Cash Received (per month):                 $498.00
  Deferred Revenue (month-end):             $0.00 (current month billing)


═════════════════════════════════════════════════════════════════
                  YEAR 1 ANNUAL SUMMARY
═════════════════════════════════════════════════════════════════

Revenue Category          Month 1   Months 2-12   Year 1 Total
─────────────────────────────────────────────────────────────────
Setup Fees                $5,000         $0         $5,000
Implementation Services   $5,000     $5,000        $10,000
SaaS Subscriptions          $299    $2,988 (11mo)  $3,289
Premium Support             $199    $1,988 (11mo)  $2,187
─────────────────────────────────────────────────────────────────
TOTAL YEAR 1 REVENUE    $10,498     $9,976       $20,474

Cash vs Accrual Variance
─────────────────────────────────────────────────────────────────
Year 1 Cash Received:                        $15,498
  - Upfront payment (M1): $10,000 + $5,000  $15,000
  - Recurring (Months 2-12): $498 × 11      $5,478
  Total:                                     $20,478

Year 1 Revenue Recognized:                   $20,474

Variance (Cash > Revenue):                   $4
  Note: Minimal variance in year 1. Variance increases in year 2
  if customer renews with upfront annual payment (creates deferred
  revenue that decreases throughout year 2).


Revenue Forecast - Year 2
─────────────────────────────────────────────────────────────────
Assuming renewal at same terms:

Setup/Implementation: $0 (already done)
SaaS + Support (12 months):                  $5,976 (annualized)

Expected Year 2 Revenue: $5,976

Note: Year 2 is lower because one-time setup fees and
implementation services (recognized in Year 1) don't recur.
This is normal for SaaS businesses. "New customer" revenue
is higher in Year 1 due to one-time services.

CAGR (assuming no churn):
  Year 1: $20,474
  Year 2: $5,976
  CAGR: Would decrease unless customer expands or company adds
        professional services component to support plan.

Retention & Expansion Strategy:
  - Quarterly Business Reviews (QBR) to increase adoption
  - Identify expansion opportunities (additional users, modules)
  - Target: $100+ MRR by end of Year 2
═════════════════════════════════════════════════════════════════
```

---

## SECTION 4: MONTHLY REPORTING TEMPLATE

### Board-Ready Financial Report - Month 1

```
═════════════════════════════════════════════════════════════════
                  TAI AUTONOMOUS SYSTEM
                    FINANCIAL REPORT
                    Month of January 2026
═════════════════════════════════════════════════════════════════

Prepared by: Finance Director
Date: February 5, 2026 (prepared by Day 5 of following month)
For: Board of Directors

────────────────────────────────────────────────────────────────
                    EXECUTIVE SUMMARY
────────────────────────────────────────────────────────────────

January Financial Results:

Revenue Recognized:         $75,000    (on plan)
Gross Profit:              $58,650    (78.2% margin)
Operating Income:          $21,650    (28.9% margin)
Net Income:                $16,237    (21.6% margin)

Key Metrics:
  Monthly Recurring Revenue (MRR):    $47,500
  Annual Recurring Revenue (ARR):     $570,000
  Active Customers:                   18
  New Customers Added:                5
  Churn Rate:                         4.4% (target: <5%)
  Days Sales Outstanding:             12 days (healthy)

Cash & Liquidity:
  Cash Collected:                     $145,000
  Operating Cash Flow:                $93,650
  Cash on Hand:                       [Amount]
  Runway (if no new revenue):         [Months]

────────────────────────────────────────────────────────────────
              INCOME STATEMENT (P&L)
              For Month Ended January 31, 2026
────────────────────────────────────────────────────────────────

REVENUE
  SaaS Subscriptions        $45,000    60% of total
  Professional Services     $25,000    33% of total
  Setup Fees                 $5,000     7% of total
  ──────────────────────────────────
  Total Revenue             $75,000   100%

COST OF REVENUE
  Cloud Infrastructure       $4,000
  Payment Processing Fees    $4,350
  Direct Labor               $8,000
  ──────────────────────────────────
  Total COGS                $16,350    22% of revenue

GROSS PROFIT               $58,650    78% of revenue

OPERATING EXPENSES
  Sales & Marketing         $12,000
  Research & Development    $15,000
  General & Admin            $10,000
  ──────────────────────────────────
  Total OpEx                $37,000    49% of revenue

OPERATING INCOME           $21,650    29% of revenue

Other Income / (Expense)       ($500)  Interest expense

INCOME BEFORE TAX          $21,150    28% of revenue

Income Tax Expense         ($5,288)   25% effective rate

NET INCOME                 $15,862    21% of revenue


────────────────────────────────────────────────────────────────
              BALANCE SHEET
              As of January 31, 2026
────────────────────────────────────────────────────────────────

ASSETS
  Cash                     $165,000
  Accounts Receivable       $30,000
  Equipment (net)           $22,500
  Software/Intangibles      $45,000
  ──────────────────────────────────
  Total Assets             $262,500

LIABILITIES
  Accounts Payable           $8,000
  Accrued Expenses           $3,000
  Deferred Revenue          $95,000   (prepayments, liability)
  ──────────────────────────────────
  Total Liabilities        $106,000

EQUITY
  Common Stock             $100,000
  Retained Earnings         $50,000
  Current Month Income      $15,862
  Distributions            ($9,362)  (if taken)
  ──────────────────────────────────
  Total Equity             $156,500

TOTAL LIABILITIES & EQUITY $262,500  (balances to $262.5k assets)


────────────────────────────────────────────────────────────────
              CASH FLOW STATEMENT
              For Month Ended January 31, 2026
────────────────────────────────────────────────────────────────

OPERATING ACTIVITIES
  Net Income                $15,862
  Add: Depreciation          $1,000
  Changes in Working Capital:
    Increase in Deferred Revenue  $95,000  (positive - cash received)
    Increase in A/R          ($30,000)    (negative - uncollected)
    Increase in A/P           $8,000      (positive - unpaid)
  ──────────────────────────────────
  Net Operating Cash Flow   $89,862

INVESTING ACTIVITIES
  Equipment purchases        ($3,500)
  ──────────────────────────────────
  Net Investing Cash Flow    ($3,500)

FINANCING ACTIVITIES
  Distributions paid         ($9,362)  (if taken)
  ──────────────────────────────────
  Net Financing Cash Flow    ($9,362)

TOTAL CASH CHANGE          $77,000
Beginning Cash Balance       $88,000
Ending Cash Balance        $165,000


────────────────────────────────────────────────────────────────
              KEY METRICS & ANALYSIS
────────────────────────────────────────────────────────────────

PROFITABILITY
  Gross Profit Margin         78.2%  (Target: 75%+) ✓ Exceeds
  Operating Margin            28.9%  (Target: 20%+) ✓ Exceeds
  Net Profit Margin           21.6%  (Target: 15%+) ✓ Exceeds
  EBITDA Margin               30.2%  (Target: 30%+) ✓ Meets

LIQUIDITY
  Current Ratio               2.47x  (Liabilities/Assets)
  Quick Ratio                 1.85x  (excludes inventory)
  Cash Position              $165k   (sufficient for 3+ months OpEx)

CUSTOMER METRICS
  Active Customers               18   (growing)
  New Customers (Month)           5   (strong acquisition)
  Churn Rate                    4.4%  (Target: <5%) ✓
  Monthly Recurring Revenue   $47.5k  (annualizes to $570k)
  Average Revenue/Customer   $4,167   (healthy for B2B SaaS)

EFFICIENCY METRICS
  Days Sales Outstanding        12 days  (Target: <30) ✓ Excellent
  CAC Payback Period            1.28 months  (Target: <12) ✓ Excellent
  LTV/CAC Ratio                18.75:1  (Target: 3:1+) ✓ Exceptional

CASH METRICS
  Operating Cash Flow        $89,862  (strong positive)
  Cash Conversion Cycle      -2.7 days  (negative is good)
  Cash Runway (months)          15+    (if revenue stops)


────────────────────────────────────────────────────────────────
              NOTES & ANALYSIS
────────────────────────────────────────────────────────────────

ASC 606 Compliance:
  All revenue recognized per ASC 606 policy (reviewed by [Firm]).
  Deferred Revenue ($95k) represents customer prepayments to be
  recognized over 12+ months. This is normal for SaaS with
  upfront annual payments.

Variance to Plan:
  Revenue: $75k actual vs $75k plan = 0% variance (on plan)
  Gross Margin: 78.2% actual vs 75% plan = $2.4k benefit
  OpEx: $37k actual vs $40k plan = $3k under budget (timing)
  Net Income: $15.9k actual vs $12.5k plan = $3.4k ahead

Notable Items:
  1. Deferred Revenue increased to $95k as new customer prepaid
     for 12-month contract upfront. This creates positive working
     capital flow (cash now, revenue recognized over 12 months).

  2. Implementation services (one-time $10k) recognized partially
     in Month 1 ($5k) and will be completed Month 2 ($5k).

  3. Days Sales Outstanding of 12 days is excellent. Most customers
     pay within 2 weeks. A/R of $30k reflects invoices sent in
     final days of January.

  4. Cash position ($165k) is strong. Includes upfront payments
     from new customers. Sufficient for 4-5 months of operating
     expenses without new revenue.

Risks & Opportunities:
  RISKS:
    - Customer #1 is 26% of January MRR ($47.5k). High concentration
      risk. Need to diversify customer base (target: top 3 customers
      <40% of MRR).
    - Implementation work (direct labor) is resource-intensive.
      Scaling will require hiring. Current 2 engineers can handle
      2-3 concurrent projects.

  OPPORTUNITIES:
    - Strong unit economics (18.75x LTV/CAC) support aggressive
      customer acquisition spending. Budget could increase 50%
      (to $18k/month S&M) while maintaining healthy economics.
    - Professional services revenue is 33% of total. Opportunity
      to bundle into subscription (recurring instead of one-time).
    - Support revenue ($199/month) has high margins. Could upsell
      higher tier ($399/month) to Customer #1.

Actions for Next Month:
  1. Focus on customer #2 onboarding (sales pipeline shows 2-3
     opportunities closing in Feb)
  2. Complete implementation Phase 2 for Customer #1 (on track)
  3. Negotiate longer payment terms with infrastructure vendors
     (currently 15-day payment, target 45-day for working capital)
  4. Hire 1 additional engineer to support Q1 implementations


────────────────────────────────────────────────────────────────
              APPENDICES
────────────────────────────────────────────────────────────────

A. Detailed Revenue Breakdown (by contract type)
B. Customer Ledger Summary (Customer #1 financial activity)
C. A/R Aging Report (invoice collection status)
D. Monthly Close Checklist (sign-offs and evidence)
E. Variance Analysis (plan vs actual by line item)
F. Cash Flow Forecast (next 12 months)


────────────────────────────────────────────────────────────────
              SIGN-OFFS
────────────────────────────────────────────────────────────────

Prepared by:    Finance Director
                Name: ____________________  Date: ________
                Signature: ________________

Reviewed by:    Chief Financial Officer
                Name: ____________________  Date: ________
                Signature: ________________

Approved by:    Chief Executive Officer
                Name: ____________________  Date: ________
                Signature: ________________

External Review: [Big Four Firm] (if performing audit)
                Partner: _________________  Date: ________
```

---

**Document Status**: READY FOR IMPLEMENTATION
**Last Updated**: January 26, 2026
**Version**: 1.0.0
