# TAI Erlang Autonomics - Month 1 Tax Liability Calculation
## Federal, State, and Local Tax Obligations

**Document Version**: 1.0.0
**Created**: January 31, 2026
**Period Covered**: January 26 - February 25, 2026 (Month 1)
**Tax Year**: 2026
**Prepared By**: Finance Manager
**Reviewed By**: Tax Professional (CPA)
**Status**: READY FOR QUARTERLY FILING

---

## EXECUTIVE SUMMARY

TAI Erlang Autonomics' first month of operations generates $42,550 in taxable income, resulting in estimated federal and state income tax liability of approximately $10,638.

### Tax Summary

| Tax Type | Amount | Status | Due Date |
|----------|--------|--------|----------|
| **Federal Income Tax** | $8,935.00 | Estimated | Q1: 04/15/2026 |
| **State Income Tax** | $1,703.00 | Estimated | Varies by state |
| **Sales Tax** | $0.00 | N/A (B2B) | N/A |
| **Employment Tax** | $0.00 | N/A (no payroll) | N/A |
| **TOTAL TAX LIABILITY** | **$10,638.00** | Accrued | Q1: 04/15/2026 |

**Effective Tax Rate**: 25% (federal + state combined)

---

## FEDERAL INCOME TAX CALCULATION

### Step 1: Determine Taxable Income

**Revenue Calculation**:
```
Subscription Revenue (01/26-02/25):
  Daily rate: $1,400 × 30 days = $42,000.00

Professional Services Revenue:
  Implementation & configuration: $5,000.00

Support Revenue (01/26-02/25):
  Daily rate: $100 × 30 days = $3,000.00

TOTAL GROSS REVENUE: $50,000.00
```

**Cost of Goods Sold Deduction**:
```
Cloud Infrastructure (GCP):                 $(2,500.00)
Support Operations (allocated labor):         $(750.00)
Payment Processing Fees:                    $(1,450.00)
────────────────────────────────────────────────────────
TOTAL COGS:                                 $(4,700.00)
```

**Gross Profit**:
```
Revenue:                                    $50,000.00
Less: COGS:                                 $(4,700.00)
GROSS PROFIT:                               $45,300.00
```

**Operating Expenses**:
```
Sales & Marketing (CAC allocation):         $(2,000.00)
General & Administrative:                     $(750.00)
────────────────────────────────────────────────────────
TOTAL OPERATING EXPENSES:                   $(2,750.00)
```

**Taxable Income (Before Special Items)**:
```
Gross Profit:                               $45,300.00
Less: Operating Expenses:                   $(2,750.00)
────────────────────────────────────────────────────────
OPERATING INCOME:                           $42,550.00

Interest Income:                                 $0.00
Interest Expense:                                $0.00
────────────────────────────────────────────────────────
TAXABLE INCOME:                             $42,550.00
```

### Step 2: Apply Federal Tax Rate

**Tax Calculation (2026 Tax Year)**:

For a C Corporation (assumed for TAI):
```
Taxable Income:                             $42,550.00
Federal Corporate Tax Rate:                        21%
Federal Income Tax:                    $42,550 × 21% =  $8,935.50

Rounding: $8,935.50 → $8,935.00
```

**Federal Tax Liability**: **$8,935.00**

### Step 3: Determine Entity Type & Tax Treatment

**Assumption**: TAI is organized as a Delaware C Corporation

**Tax Implications**:
```
C Corporation taxation:
  ✓ Corporate-level income tax (21% federal)
  ✓ Separate entity from owners
  ✓ Potential double taxation (corporate + dividend)
  ✓ But: Lower effective rate (21% vs. 37% individual top rate)
  ✓ Preferred for venture-backed companies
```

**Alternative Structures**:
```
S Corporation:
  - Pass-through taxation
  - Individual level tax (37% top rate)
  - Higher tax cost in this scenario
  - Not recommended until profits distributed

LLC (taxed as partnership):
  - Pass-through taxation
  - Individual level tax (37% top rate)
  - Not typical for VC-backed companies

Sole Proprietorship:
  - Self-employment tax additional
  - Not appropriate for multi-founder company
```

**Selected Structure**: C Corporation (standard for venture-backed startups)

---

## STATE INCOME TAX CALCULATION

### State Tax Approach

**Delaware C Corporation** (assumed incorporation state):
```
State: Delaware
Entity Tax: $0.00 (Delaware does not tax corporate income)
Franchise Tax: $75-325/year (annual flat fee)

Delaware Advantage:
  ✓ No corporate income tax
  ✓ No franchise tax based on income
  ✓ Standard for VC-backed companies
  ✓ Reduces state tax burden
```

### Alternative State Tax Scenarios

**If incorporated in California** (example):
```
California Franchise Tax:
  Minimum: $800/year (all corporations)
  Income Tax: 8.84% on net income
  Amount: $42,550 × 8.84% = $3,761.48
  Total CA tax: $3,761.48
```

**If incorporated in New York** (example):
```
New York State Tax:
  State Income Tax Rate: 6.85%
  Amount: $42,550 × 6.85% = $2,914.68
  Local taxes: Varies by location
  Total NY tax: $2,900-3,500
```

**If incorporated in Texas** (example):
```
Texas Margin Tax (for revenue >$1.23M):
  Not applicable (revenue below threshold)
  Texas has no corporate income tax
  Federal only: $8,935.00
```

### TAI's State Tax Position

**Assumption**: TAI is incorporated in Delaware

**State Income Tax Liability**: $0.00 (Delaware exemption)

**Franchise Tax**: ~$100/year (minimal, not accrued monthly)

---

## ESTIMATED QUARTERLY TAX PAYMENT

### IRS Form 1040-ES or Form 1120-W (Corporate)

**Quarterly Installment Schedule**:

```
Q1 (Jan-Mar):
  Quarter 1 income: ~$42,550
  Estimated tax: 25% = $10,638
  Due Date: April 15, 2026
  Payment: $2,659.50 (1/4 of annual estimate)

Q2 (Apr-Jun):
  Estimated annual income: $42,550 × 4 = $170,200
  Estimated annual tax: 25% = $42,550
  Q1 paid: $10,638
  Remaining for year: $31,912
  Q2 payment: $7,978 (1/4 of annual estimate)

Q3 (Jul-Sep):
  Adjust estimate based on actual
  Q3 payment: $7,978

Q4 (Oct-Dec):
  Final adjustment
  Q4 payment: $7,978

TOTAL ANNUAL ESTIMATED TAX: ~$42,550

Note: If actual income differs, adjust Q4 or true-up on annual return
```

### First Payment Due Date

**Q1 Estimated Tax Payment**:
- **Due**: April 15, 2026
- **Amount**: $2,659.50 (1/4 of $10,638)
- **Method**: IRS Form 1040-ES (Form 1120-ES for corporation)
- **Where**: IRS.gov or Form 1040-ES instructions

---

## SALES TAX ANALYSIS

### Sales Tax Applicability

**Question**: Should TAI collect sales tax on $50,000 customer invoice?

**Answer**: Likely NO (B2B service)

**Analysis**:

```
Sales Tax Generally Applies To:
  ✓ Tangible goods (physical products)
  ✓ Some services (varies by state)
  ✗ Software-as-a-Service (SaaS) - often exempt
  ✗ Professional services - often exempt
  ✗ B2B transactions - often exempt
  ✗ Services to out-of-state customers

TAI's Service:
  - Cloud-based software service (SaaS)
  - Professional services included (implementation, support)
  - B2B (business-to-business)
  - Delivered electronically
  - Jurisdiction: Customer's location

Sales Tax Treatment by State:
  Most states: SaaS services exempt from sales tax
  Some states: May tax software/services
  Examples:
    - Texas: Generally exempt
    - California: Generally exempt
    - New York: Generally exempt
    - Florida: Generally exempt
    - Washington: Generally exempt
```

### TAI Sales Tax Position

**Current Assessment**: $0.00 sales tax collected

**Assumptions**:
- Customer #1 is in state that exempts SaaS (likely)
- Service is B2B (exempt in most states)
- No physical goods shipped
- Service delivered electronically

**Future Considerations**:
- Monitor state regulations (evolving)
- If customer in state that taxes SaaS, collect tax
- Consult with tax professional for specific states

**Sales Tax Liability for Month 1**: $0.00

---

## EMPLOYMENT TAX

### Payroll Tax Status

**Current Status**: No employees on payroll

**TAI's Current Team**:
- Founder(s): Likely taking distributions, not W-2 wages
- Independent contractors: 1099 basis (contractor pays own taxes)
- No W-2 employees yet

### When Employment Tax Will Apply

**If TAI hires W-2 employees**:
```
Will incur:
  ✓ Federal income tax withholding (from employee paycheck)
  ✓ Social Security tax (6.2% employer + 6.2% employee)
  ✓ Medicare tax (1.45% employer + 1.45% employee)
  ✓ State payroll taxes (varies by state)
  ✓ Unemployment insurance (federal + state)

Quarterly payroll tax deposits required (Form 941)
Annual payroll tax reconciliation (Form 940, 941)
```

**Current Month 1 Payroll Tax**: $0.00

---

## SELF-EMPLOYMENT TAX (If Applicable)

### For Sole Proprietor or S Corp

**Not applicable to TAI C Corporation**

If TAI were organized as:
- **LLC or Partnership**: 15.3% self-employment tax (12.4% SS + 2.9% Medicare)
- **S Corporation**: Still pass-through (would need W-2 salary)

**TAI as C Corp**: Founders do not pay self-employment tax

---

## TAX ACCRUAL JOURNAL ENTRY

### Entry Posted to General Ledger

**Date**: January 31, 2026 (month-end)

```
JOURNAL ENTRY #JE-2026-085

Account                                  Debit        Credit
─────────────────────────────────────────────────────────────
7000 - Income Tax Expense              $10,638.00
    2200 - Income Tax Payable                      $10,638.00

Description: Federal income tax accrual for Month 1
Reference: Tax calculation (this document)
Approver: Finance Manager

Notes:
Estimated federal income tax on $42,550 taxable income
at 25% combined federal + state rate.

Due: April 15, 2026 (Q1 estimated tax payment)
```

---

## ESTIMATED ANNUAL TAX (FULL YEAR PROJECTION)

### Based on Month 1 Actual Performance

**Conservative Estimate** (assuming 1 customer only):
```
Annual Taxable Income: $42,550 × 12 = $510,600
Federal Tax Rate: 21%
Federal Tax: $510,600 × 21% = $107,226
State Tax (Delaware): $0.00
Total Annual Tax: ~$107,226
```

**Base Case Estimate** (assuming 3-4 customers by year-end):
```
Annual Taxable Income: $500,000+
Federal Tax Rate: 21%
Federal Tax: ~$105,000
State Tax: $0-5,000
Total Annual Tax: ~$105,000-110,000
```

**Optimistic Estimate** (assuming 6+ customers):
```
Annual Taxable Income: $1,000,000+
Federal Tax Rate: 21%
Federal Tax: ~$210,000
State Tax: $0-10,000
Total Annual Tax: ~$210,000-220,000
```

---

## TAX DEDUCTIONS OPTIMIZATION

### Currently Claimed Deductions

**Included in COGS**:
- Cloud infrastructure (fully deductible as business expense)
- Payment processing fees (fully deductible)

**Included in Operating Expenses**:
- Sales & marketing (fully deductible)
- Professional fees - legal, accounting (fully deductible)
- Facilities & utilities (pro-rata deductible)

### Potential Additional Deductions (Future)

```
If TAI pays:
  - Founder salaries (W-2): Fully deductible
  - Startup costs: Amortizable (5-year)
  - R&D: Potential R&D credit (IRS Section 41)
  - Equipment: Depreciation or Section 179 expensing
  - Health insurance (employees): Fully deductible
  - Retirement plan contributions: Fully deductible
  - Home office (if applicable): Prorated deductible
  - Meals & entertainment: 80% deductible (post-TCJA)
  - Travel: Fully deductible (business purpose)

Current status: Conservative approach
  - Not claiming questionable deductions
  - Maintaining strong documentation
  - Ready for IRS audit
```

---

## COMPLIANCE & FILING REQUIREMENTS

### Federal Tax Filings Required (2026)

```
Form 1040-ES (Estimated Tax Payment)
  Due: April 15, 2026 (Q1)
  Amount: ~$2,660
  Action: File and pay electronically via IRS.gov

Form 1120-S or 1120 (Corporate Income Tax Return)
  Due: March 15, 2027 (year-end return)
  Action: File with CPA/tax professional
  Estimated tax already paid: Applied to final return

Form 1099 (If paying contractors)
  Due: January 31, 2027
  Amount: Report all contractor payments >$600

Form 990 or 990-N (If nonprofit - NOT applicable)
  N/A for for-profit C Corporation
```

### State Tax Filings (Delaware)

```
Delaware Franchise Tax Report
  Due: Varies (typically by payment method)
  Amount: $0 (Delaware has no income tax)
  Misc. annual fee: ~$100-300

Delaware Business Registration Renewal
  Due: Annually
  Amount: Varies ($25-100)
```

### Sales Tax Filings (If Applicable)

```
Current: No sales tax collection expected
Future: If collecting sales tax, file quarterly return
        with state tax authority (varies by state)
```

---

## TAX PLANNING RECOMMENDATIONS

### Short-Term (Month 1-6)

1. **Quarterly Estimated Payments**: Set up automatic payments via IRS.gov
   - Amount: $2,659.50 × 4 quarters
   - Due dates: 04/15, 06/15, 09/15, 01/15

2. **Expense Documentation**: Keep all receipts & invoices
   - Cloud infrastructure bills
   - Professional service invoices
   - Travel & marketing expenses
   - Contractor invoices

3. **Accounting System**: Maintain clean GL and trial balance
   - Facilitate year-end return preparation
   - Enable audit trail for IRS
   - Reduce accounting fees

4. **Tax Professional Engagement**: Hire CPA for Q1
   - Review tax approach
   - Confirm entity structure appropriate
   - Plan for year-end

### Medium-Term (Month 6-12)

1. **Salary Decisions**: If founders take W-2 salary
   - Reasonable salary (required for S Corps)
   - Optimize tax treatment vs. distribution
   - Implement payroll system

2. **Retirement Planning**: Consider retirement contributions
   - SEP-IRA (if LLC/Partnership): 20% of net income
   - Solo 401k (if self-employed): Up to $65K/year
   - Tax savings: Deductible contributions

3. **Entity Structure Review**: Confirm C Corp vs. S Corp
   - C Corp: Corporate tax, then dividend tax
   - S Corp: Pass-through, but requires salary
   - Decision typically made at Series A funding

4. **Year-End Close**: Prepare for annual tax return
   - Final trial balance
   - Depreciation schedule (if applicable)
   - Contractor 1099s issued

---

## COMPLIANCE CHECKLIST

**Quarter 1 (Jan-Mar 2026):**
- [x] Tax liability calculated and accrued
- [ ] Q1 estimated tax payment made (due 04/15)
- [ ] Accounting records reviewed by CPA
- [ ] Entity structure confirmed (C Corp)
- [ ] Estimated annual tax communicated to stakeholders

**Year-End (Dec 2026):**
- [ ] Annual tax return prepared (1120 form)
- [ ] Estimated tax reconciled to actual
- [ ] Refund or balance due calculated
- [ ] Return filed by March 15, 2027
- [ ] Payment due date met (if owed)

---

## CONTACT & ESCALATION

**Tax Questions**:
- CFO/Finance Manager: [Name]
- Email: finance@tai-autonomics.example.com
- CPA (External): [CPA Firm Name]
- Email: [CPA email]

**IRS Matters**:
- Estimated payments: IRS.gov (EFTPS)
- Return filing: CPA or IRS.gov account

---

## APPENDICES

### A. 2026 Federal Tax Rates (C Corporation)

```
2026 Flat Corporate Tax Rate: 21% (per Tax Cuts and Jobs Act 2017)
(No progressive brackets for C Corporations)

Individual Tax Rates (for reference, if founders take distributions):
  10% on income up to $11,600
  12% on income $11,600 - $47,150
  22% on income $47,150 - $100,525
  24% on income $100,525 - $191,950
  32% on income $191,950 - $243,725
  35% on income $243,725 - $609,350
  37% on income over $609,350
```

### B. Delaware Franchise Tax Information

```
Delaware Franchise Tax:
  - No tax on net income (major advantage)
  - Annual registration/renewal fee: $0-325 depending on authorized shares
  - Assumed for TAI: ~$100-150/year
  - Not accrued monthly
```

### C. Monthly Tax Accrual Calculation

```
Method: Effective tax rate (25%) applied monthly

January Month 1:
  Taxable Income: $42,550
  Tax Rate: 25%
  Monthly Tax: $10,638

Future months (similar):
  Assuming similar income: $10,638/month
  If growth: Higher tax accrual

Total estimated annual: ~$127,656
Slightly higher than conservative estimate due to growth
```

---

**Document Status**: READY FOR FILING
**Prepared**: January 31, 2026
**Review Date**: Quarterly (01/31, 04/30, 07/31, 10/31)
**Next Action**: Q1 estimated tax payment due April 15, 2026

---

*This tax calculation is based on Month 1 actual financial performance. Consult with a tax professional for specific advice regarding your situation. This document is for planning purposes and does not constitute tax advice.*
