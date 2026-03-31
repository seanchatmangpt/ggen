# INCORPORATION PACKAGE: Entity Setup & Governance

**Status**: Production-ready incorporation and governance framework
**Date**: January 2026
**Scope**: C-Corporation with venture financing path

---

## EXECUTIVE SUMMARY

This document provides complete incorporation guidance to establish a tax-efficient, VC-ready legal entity capable of charging customers on day one.

**Recommendation**: **Delaware C-Corporation**
- **Why**: Only entity structure compatible with venture capital financing
- **Timeline**: 2-3 weeks from filing to operational readiness
- **Cost**: $5K-15K (incorporation, bylaws, cap table, legal review)
- **Compliance readiness**: FULL (all fiduciary requirements met)

---

## PART I: ENTITY SELECTION FRAMEWORK

### Option 1: Delaware C-Corporation (RECOMMENDED FOR VC)

**Structure**:
```
Delaware C-Corp (DE 8-K filing)
├── Board of Directors (CEO, 1-2 independent directors)
├── Officers (CEO, CFO, Secretary)
├── Shareholders (Founder + future investors)
└── Stock classes (Common + Series A/B/C Preferred)
```

**Advantages**:
- **VC-compatible**: Only structure venture investors will fund
- **Favorable tax law**: Delaware has developed body of case law favorable to corporations
- **Dual incorporation option**: Can operate in DE and register in home state
- **Tax efficiency**: Possible C/S-Corp election if exit timing favorable
- **Governance maturity**: Clear director duties, shareholder protections
- **Option pool**: Can create ESOP (incentive stock options) for employees

**Disadvantages**:
- **Double taxation**: Corporations taxed at entity + shareholder level (35-55% combined)
- **Annual compliance**: Registered agent required ($150-300/year), annual reports
- **Complexity**: Requires corporate formalities (annual meetings, minutes, resolutions)

**Tax Impact Example** (assuming 40% federal/state corporate tax + 20% shareholder tax on dividends):
```
Company revenue: $1,000,000
Corporate tax (35% all-in): $350,000
Net available for distribution: $650,000
Shareholder tax on dividend (20%): $130,000
Total founder keeps: $520,000
Effective tax rate: 48%
```

**When to Use**:
- [ ] Raising venture capital (ONLY option)
- [ ] Building company for acquisition
- [ ] Creating employee equity program
- [ ] Exit timeline: 5-10 years

---

### Option 2: S-Corporation (HYBRID APPROACH)

**Structure**:
```
Delaware C-Corp (election to be taxed as S-Corp)
├── S-Corporation status (IRS election)
├── Pass-through taxation (1120-S filing)
└── Tax at shareholder level only
```

**How it Works**:
1. Form Delaware C-Corp (same structure as Option 1)
2. File Form 2553 with IRS (S-Corp election)
3. Company income flows through to founders' personal tax returns
4. Founders pay individual income tax (37% max federal + state)
5. Can also take W-2 salary (self-employment tax: 15.3%)

**Advantages**:
- **Hybrid taxation**: C-Corp structure + pass-through taxation
- **Tax savings**: 20-40% better than straight C-Corp (avoids double taxation)
- **Still VC-compatible**: Can convert back to C-Corp for Series A
- **Self-employment tax savings**: Deduct reasonable salary, remainder as dividend

**Disadvantages**:
- **VC conversion complexity**: Series A investors usually require C-Corp conversion
- **Strict ownership rules**: Must have <100 US shareholders
- **Single class constraint**: Cannot have preferred stock (breaks investor model)
- **Timing risk**: If converting to C for Series A, triggers phantom income

**Tax Impact Example** (same $1M revenue):
```
Company revenue: $1,000,000
Reasonable salary (W-2): $150,000
Dividend income (non-W-2): $850,000
Founder personal income tax:
  - Salary (37% + 15.3% FICA): $76,195
  - Dividend (20% qualified): $170,000
Total founder keeps: $753,805
Effective tax rate: 24.6%
```

**When to Use**:
- [ ] No current venture capital plans
- [ ] Building lifestyle business (keep more cash flow)
- [ ] Timeline to exit: 5+ years
- [ ] Manual S-Corp accounting acceptable

**Conversion to C-Corp**:
- File Form 1120-C and revoke S-election
- Converts PFIC income to C-Corp double taxation
- Process: 2-4 weeks, minimal tax impact if done pre-Series A

---

### Option 3: LLC (NOT RECOMMENDED FOR VC)

**Why NOT**: Venture investors will NOT fund LLCs (no preferred stock, no option pool, pass-through taxation creates cap table complexity).

If you choose LLC:
- Only acceptable if bootstrapping or seeking strategic investor
- Can convert to C-Corp later (Section 351 exchange)
- Costs 1-2 months of transition work

---

## PART II: INCORPORATION CHECKLIST (2-3 WEEKS)

### Week 1: Preparation & Filing

**Task 1: Incorporate in Delaware** (1 day, $300-500)
```
[ ] Choose registered agent (Delaware resident/agent)
    - Use service: Corporation Service Company, LegalZoom, Stripe Atlas
    - Cost: $100-300/year
    - Function: Receive official documents on behalf of corporation

[ ] File Certificate of Incorporation with Delaware Secretary of State
    - Form: DGCL § 102 certificate (one-page form)
    - Filing fee: $89-185 (depending on time)
    - Timeline: Same-day to 24 hours
    - Deliverable: Corporate charter received

[ ] Create corporate bylaws
    - Template: Use Delaware Model Bylaws (DGCL § 109)
    - Key sections:
      - Board meetings (annual minimum)
      - Share issuance process
      - Director indemnification
      - Amendment procedures
    - Must adopt at first board meeting

[ ] Issue founder stock certificates
    - Number of shares: 10,000,000 authorized (standard)
    - Founder allocation: 100% initially (e.g., 5,000,000 common shares)
    - 409A valuation: $0.0001/share (stub value, almost zero)
    - Form: Stock certificate + ledger entry
```

**Task 2: Create Corporate Records** (1 day, no cost)
```
[ ] Minute books (official corporate records)
    - Incorporator action (initial setup)
    - Bylaws adoption (first meeting)
    - Board resolutions (future use)
    - Shareholder resolutions (future use)
    - Keep in physical or electronic binder

[ ] Corporate seal (optional but recommended)
    - Embossed stamp showing company name + date
    - Cost: $20-50
    - Use: Seal stock certificates, official documents

[ ] Stock ledger (cap table tracking)
    - Record: All stock issuances, transfers, options
    - Entries: Date, recipient, shares, price, certificate #
    - Keep updated continuously (required by law)
```

**Task 3: Obtain EIN from IRS** (1 day, free)
```
[ ] Apply for Employer Identification Number (EIN)
    - Form: SS-4 (online application at irs.gov)
    - Timeline: Immediate (same-day EIN)
    - Required: Company name, principal location, incorporation date
    - Deliverable: EIN letter from IRS
    - Use for: Bank account, tax filings, contractor 1099s

[ ] Create bank account
    - Required documents: Certificate of Incorporation, EIN letter, founder ID
    - Account type: Business checking + savings
    - Cost: $0-15/month (most banks free for small business)
    - Timeline: Same day
```

---

### Week 2: Governance Setup & Shareholder Agreements

**Task 4: Board Governance** (2 days, $1K-2K)
```
[ ] Board of Directors structure
    - Initial: Founder CEO + 0 independent directors (allowed)
    - Recommended: CEO + 2 independent directors (if raising capital)
    - Path: Start with founder only, add independent directors at Series A

[ ] Board meeting schedule
    - Frequency: Quarterly (4 meetings/year minimum)
    - Attendees: Board + optional observers (CFO, legal counsel)
    - Agenda: Finance, strategy, hiring, legal/compliance
    - Minutes: Must record all major decisions
    - First meeting: Adopt bylaws + authorize stock issuance

[ ] Board committees (optional initially)
    - Audit committee: Monitor internal controls, external auditor
    - Compensation committee: Manage stock options, salary decisions
    - Nominating committee: Recruit new directors
    - Activated at Series A (not needed pre-seed)

[ ] Director liability insurance
    - Coverage: D&O (Directors & Officers) liability
    - Amount: $1-2M initially (increases with Series A)
    - Cost: $2K-5K/year
    - Requirement: Almost mandatory by Series A (investors require it)
    - Covers: Director/officer legal defense, settlements
```

**Task 5: Shareholder Agreements** (3 days, $2K-5K)
```
[ ] Founder stock agreement
    - Terms: Vesting schedule (4-year vesting, 1-year cliff)
    - Acceleration triggers: IPO, change of control (optional)
    - Repurchase rights: Company buyback if founder leaves
    - Why: Protects company if founder departs early
    - Required if: Taking outside funding

[ ] Stockholders' agreement
    - Parties: All shareholders (founder + investors + key employees)
    - Restrictions: Right of first refusal, co-sale rights, drag-along
    - Voting: Specify major decision requirements
    - Board seat rights: How investors get board seats
    - Adoption: Required for every financing round

[ ] Right of First Refusal / Co-Sale Agreement
    - ROFR: Company has first right to buy shares founder wants to sell
    - Co-sale: If founder sells, investors can sell alongside
    - Purpose: Prevents founder from diluting investor position
    - Standard: Included in stockholders' agreement
```

**Task 6: Option Pool & Employee Equity** (2 days, $1K-2K)
```
[ ] Design equity incentive plan
    - Type: ISO (Incentive Stock Options) for tax efficiency
    - Size: 10-20% of fully-diluted shares reserved for employees
    - Calculation: If 5M founder shares, reserve ~625K-1.25M options
    - Adoption: Board resolution at incorporation

[ ] Create stock option documentation
    - Plan document: Master ESOP (Equity Stock Option Plan)
    - Option agreement template: Terms for each optionee
    - Admin: Track grants, vesting, exercise dates
    - IRS compliance: 409A valuation must support option pricing

[ ] Cap table management
    - Tool: Use Carta, Pulley, or Runwayapp for tracking
    - Entries: Stock, options, warrants, convertible notes
    - Updates: Update after every financing event
    - Reconcile: Verify quarterly against cap table tool
```

---

### Week 3: Tax Setup & Compliance

**Task 7: Sales Tax Registration** (1 day, free)
```
[ ] Nexus analysis (where you need to collect sales tax)
    - Rule 1: State where company incorporated (always)
    - Rule 2: States where you have "economic nexus" (>$100K-1M sales)
    - Rule 3: States where you have employees
    - Research: Use TaxJar, Avalara nexus tool

[ ] Register for sales tax ID (per state)
    - Application: Online via state revenue department
    - Frequency: Only once per state
    - Information required: Company name, address, business type
    - Timeline: 1-7 days per state
    - Cost: Free

[ ] Collect & remit sales tax
    - Frequency: Monthly or quarterly (per state)
    - Calculation: Sales tax = taxable sales × tax rate
    - Payment: Online portal per state
    - Software: Avalara, Stripe Tax, Wave

[ ] Track exemptions
    - B2B software: Usually NOT taxable in most states
    - B2C SaaS: May be taxable (varies by state)
    - Document: Keep customer resale certificates on file
```

**Task 8: Payroll & Employment Tax Setup** (2 days, $500-1K)
```
[ ] Payroll system selection
    - Options: Guidepoint, ADP, Rippling, Paychex
    - Cost: $50-500/month depending on employees
    - Timeline: Set up before first employee payment

[ ] I-9 & W-4 documentation
    - Requirement: EVERY employee must complete I-9 + W-4
    - Timing: On or before day 1 of employment
    - Retention: I-9 kept for 3 years
    - Penalty: $100-1,100 per missing form

[ ] Federal employment tax ID (Form EIN)
    - Already obtained in Week 1
    - Use for: Payroll tax filings

[ ] Register for state employment taxes
    - Application: State labor department (similar to sales tax)
    - Information: EIN, number of employees
    - Frequency: Before first employee hired
    - Timeline: 1-2 weeks

[ ] Quarterly tax payments
    - Federal: Form 941 (payroll tax return, quarterly)
    - State: Varies (SC: Form SCTAX1, CA: DE-9, etc.)
    - Deadline: 15th of month after quarter ends
    - Software: Automate via payroll system
```

**Task 9: International Tax IDs (If Applicable)** (3 days, varies)

If operating internationally or have international customers:

```
[ ] US Export/Import Registration (if selling globally)
    - DUNS number: Automatic with EIN, no action needed
    - Foreign sales: May require ITIN (Individual Tax ID)

[ ] UK Tax Registration (if UK customers)
    - VAT number: Required if >£85K turnover from UK
    - Registration: HMRC online (2-3 weeks)
    - Threshold: Depends on UK operations

[ ] EU VAT (if EU customers)
    - VAT number: Required if any EU sales (threshold varies)
    - Registration: Each country has own registration portal
    - Complexity: May need EU tax representative
    - Cost: €500-5K/year for compliance

[ ] Canada GST/HST (if Canadian customers)
    - Registration: Required if >$30K CAD annual sales
    - Application: CRA (Canada Revenue Agency)
    - Timeline: 2-3 weeks
```

**Task 10: 409A Valuation** (3 days, $2K-5K)

```
[ ] Hire 409A valuation specialist
    - Why: Required for option pricing (IRS requirement)
    - When: Before issuing any stock options or restricted stock
    - Cost: $2K-5K (startup firms, less than $20M valuation)
    - Timeline: 2-3 weeks
    - Providers: Gust Prime Valuation, Carta, 409A firms

[ ] What they deliver
    - Valuation report: Fair market value per share at date of valuation
    - Conclusion: "$0.50/share FMV as of January 2026"
    - Use: Price options at FMV (required for ISO tax treatment)
    - Update: Needed only if major event (financing, acquisition rumor)

[ ] How founders use it
    - Option grants: "Grant options at $0.50 strike price"
    - Safe harbor: Following 409A guidance prevents IRS penalties
    - ISO treatment: Allows favorable tax treatment (no AMT)
    - Documentation: Keep valuation report in minute book
```

---

## PART III: BUSINESS LICENSING

### Federal Licenses (By Industry)

#### A. Universal Federal Requirements

**Every company needs**:
```
[ ] Employer Identification Number (EIN)
    - Obtained Week 1
    - Free from IRS

[ ] Business name registration (DBA)
    - If operating under different name than corporation
    - Cost: $50-200 (state/county)
    - Frequency: Every 5 years
```

#### B. Industry-Specific Federal Licenses

**If offering software/SaaS services**:
```
[ ] None required (software is unregulated federally)
    - Exception: If handling payment processing
    - Exception: If storing healthcare data (HIPAA)
    - Exception: If handling financial data (SEC/FINRA)
```

**If handling financial data (investment advice, trading)**:
```
[ ] SEC registration (if investment advice)
    - Form ADV: Register as Investment Advisor
    - Timeline: 2-4 weeks
    - Cost: Free
    - Requirement: If giving specific investment recommendations

[ ] FINRA registration (if securities broker-dealer)
    - Form BD: Register as broker-dealer
    - Timeline: 4-8 weeks + exam
    - Cost: $1,500-5,000
    - Requirement: If buying/selling securities for clients
```

**If handling healthcare data (HIPAA)**:
```
[ ] HHS Notification: Not a license, but required notification
    - Form: No specific form (notification via letter)
    - Timeline: Same day
    - Cost: Free
    - Requirement: If handling PHI (Protected Health Information)
    - Impact: Triggers HIPAA compliance, business associate agreements
```

**If handling payment cards (PCI DSS)**:
```
[ ] No specific license required
    - Instead: PCI DSS compliance (self-assessed or audited)
    - Cost: $0-50K/year (depends on volume)
    - Requirement: If processing <$50M cards/year
```

### State-Level Licenses (By Jurisdiction)

#### Delaware (Recommended)

```
[ ] Certificate of Incorporation
    - Obtained Week 1
    - Annual report: Due 6/30 each year
    - Fee: $25/year

[ ] Business license (local)
    - If physical office in Delaware
    - Cost: $50-150
    - Frequency: Annual renewal
```

#### California (If operating there)

```
[ ] California Business License (local)
    - Form: Online via county clerk
    - Cost: $100-500 depending on annual income
    - Timeline: 2-5 days
    - Renewal: Annually

[ ] California Seller's Permit (for sales tax)
    - If selling software to CA customers
    - Form: online via California Department of Tax & Fee Administration
    - Cost: Free
    - Timeline: Same-day

[ ] California Professional Corporation Certificate (if applicable)
    - Not needed for software/SaaS
    - Only needed for licensed professions (law, medicine, etc.)
```

#### New York (If operating there)

```
[ ] New York Business License (local)
    - If physical office in NY
    - Cost: $50-250
    - Timeline: 1-2 weeks
    - Renewal: Annually

[ ] Resale Certificate (for sales tax exemption)
    - If buying equipment/supplies for resale
    - Cost: Free
    - Timeline: Same-day online
```

---

## PART IV: REGULATORY CERTIFICATIONS & COMPLIANCE

### SOC 2 Type II (Most Important for B2B SaaS)

**What is it?**
- System and Organization Controls audit
- Assesses controls over: security, availability, processing integrity, confidentiality, privacy
- Type II proves controls over time (minimum 6 months of monitoring)

**When needed**:
- [ ] Enterprise customers require SOC 2 (80%+ of enterprise deals)
- [ ] Healthcare/finserv customers mandate it
- [ ] Timeline: Don't need immediately, needed for first enterprise sale

**Timeline & Cost**:
```
Preparation: 2-3 months
  - Document controls
  - Implement monitoring systems
  - Remediate gaps

Audit: 1-2 months
  - Choose auditor: Deloitte, PwC, EY, or specialized firms
  - Cost: $25K-75K (depending on complexity)

Report: 30 days post-audit
  - NDA required: Can only share with customer under NDA
  - Validity: 12 months from issue date
  - Renewal: Annual audit required
```

**Compliance items needed before SOC 2 audit**:
- [ ] Information security policy (documented)
- [ ] Access controls (passwords, MFA, identity management)
- [ ] Encryption (data at rest, data in transit)
- [ ] Monitoring & logging (audit trails of access)
- [ ] Incident response plan (process for security breaches)
- [ ] Disaster recovery plan (backup/restoration procedures)
- [ ] Vulnerability management (regular security scanning)
- [ ] Change management (process for code/config changes)
- [ ] Risk assessment (annual review of risks)

**Path to SOC 2**:
```
Months 0-2: Document and implement controls
Months 2-3: Internal audit/remediation
Months 3-4: Third-party audit
Month 5: SOC 2 report issued
```

**Cost-benefit**:
- Cost: $30K-75K
- Benefit: $50K-500K in incremental enterprise sales (typical)
- ROI: Positive within first 1-2 enterprise deals

---

### ISO 27001 (Information Security)

**When needed**: Large enterprises, especially in regulated industries (finance, healthcare, government)

**Timeline**: 6-12 months from start to certification

**Cost**: $50K-150K (including audit)

**Certification body**: Bureau Veritas, TÜV Rheinland, DNV, Apptis

**Path**:
1. Gap analysis (where you are vs. ISO 27001) - 2 weeks
2. Implement controls - 4-6 months
3. Internal audit - 2-4 weeks
4. Third-party audit - 4-6 weeks
5. Certification issued - 2-4 weeks

**Timeline priority**: AFTER SOC 2 (can usually get it done 3-6 months after SOC 2)

---

### GDPR & CCPA Compliance (See Data Privacy Section)

---

## PART V: INSURANCE REQUIREMENTS FOR DAY-ONE BILLING

### 1. Professional Liability Insurance (E&O)

**Critical for outcome-based contracts** (See legal/LEGAL_FRAMEWORK.md)

**What it covers**:
- Errors and omissions in service delivery
- Professional negligence claims
- Contractual liability for outcome-based arrangements (WITH endorsement)

**Minimum coverage**:
- Amount: $2M per occurrence / $5M aggregate
- Deductible: $25K-50K
- Cost: $5K-15K/year (base rate)
- Additional cost for contractual liability endorsement: $3K-8K/year

**Carriers offering contractual liability for SaaS**:
- ACE (now Chubb)
- Zurich
- XL Catlin (now AXA XL)
- Hiscox
- Heidrick & Struggles

**Timeline to obtain**:
- Application: 1-2 weeks
- Underwriting: 1-2 weeks
- Policy issuance: 1-2 weeks
- Total: 4-6 weeks

**Critical**: DO NOT sign customer contracts without this endorsement in place.

---

### 2. Directors & Officers Liability (D&O)

**What it covers**:
- Personal liability of directors/officers
- Shareholder lawsuits
- Regulatory investigations
- Defense costs

**Why you need it**:
- Venture investors require it (condition of funding)
- Protects founders from personal liability
- Covers SEC/regulatory investigations

**Minimum coverage**:
- Amount: $1M per claim / $2M aggregate (initially)
- Deductible: $50K-100K
- Cost: $2K-5K/year (startup rates)

**Timeline**: 2-4 weeks after incorporation

**Carriers**: Chubb, Zurich, Hartford, Hiscox, Travelers

---

### 3. Cyber Liability Insurance

**What it covers**:
- Data breach notification costs
- Regulatory fines & penalties
- Privacy liability claims
- Business interruption from cyber attack
- Forensics & credit monitoring

**Why you need it**:
- Customer data breach (notification, credit monitoring = $1M+)
- GDPR/CCPA fines (up to 4% of global revenue)
- Ransomware attack (recovery, downtime = $100K-5M)

**Minimum coverage**:
- Cyber liability: $1-5M
- Data breach notification: $500K-2M
- Privacy liability: $1-2M
- Business interruption: $100K-500K
- Deductible: $25K-50K
- Cost: $3K-10K/year

**Carriers**: Chubb, Zurich, Coalition, Beazley, AIG

---

### 4. General Commercial Liability (Standard)

**What it covers**:
- Bodily injury
- Property damage
- Advertising injury
- Products liability

**Why you need it**:
- Often required by office landlord (if physical office)
- Standard business requirement
- Protects from premises liability

**Minimum coverage**:
- Amount: $1M per occurrence / $2M aggregate (standard)
- Cost: $500-2K/year
- Deductible: $0-1K

**Carriers**: State Farm, Hartford, Travelers, Hiscox

---

### 5. Employment Practices Liability (EPLI)

**What it covers**:
- Wrongful termination claims
- Discrimination lawsuits
- Sexual harassment claims
- Wage & hour violations

**Why you need it**:
- If hiring employees (as opposed to contractors)
- If operating in California (highest employment litigation state)
- Protection for founder/CEO against personal liability

**Minimum coverage**:
- Amount: $1M per claim
- Cost: $1K-3K/year
- Deductible: $10K-25K

**Carriers**: Chubb, Hartford, Zurich, AIG

---

### Insurance Quote Summary (Total Estimated Cost)

```
Insurance Type              Amount      Cost/Year       Critical?
─────────────────────────────────────────────────────────────
Professional Liability      $2-5M       $8K-23K         YES*
  (E&O w/ contractual)
Directors & Officers        $1-2M       $2K-5K          YES
Cyber Liability             $1-5M       $3K-10K         YES
General Commercial          $1M         $500-2K         Optional
Employment Practices        $1M         $1K-3K          Optional
─────────────────────────────────────────────────────────────
TOTAL FIRST YEAR            -           $14.5K-43K      -
ANNUAL RENEWAL              -           $14.5K-43K      -

* Must have contractual liability endorsement on E&O before signing
  outcome-based contracts. Non-negotiable.
```

**Negotiation strategy**:
- Get quotes from 3+ carriers
- Bundle policies (5-15% discount)
- Pre-pay annual (2-5% discount)
- Demonstrate security controls (5-10% discount)
- Claims history (no claims = better rates)

**Timeline for all insurance**:
- Professional liability (with contractual endorsement): 4-6 weeks
- D&O: 2-4 weeks
- Cyber: 1-3 weeks
- General commercial: 1-2 weeks
- **Total: 4-6 weeks, start ASAP**

---

## PART VI: SECURITIES COMPLIANCE

### Equity Issuance Framework

#### Founder Stock (Day 1)

**How it works**:
1. Create stock with 4-year vesting, 1-year cliff
2. Example: Founder receives 5,000,000 shares
   - Year 1: 1,000,000 vests (25%) [cliff]
   - Years 2-4: 1,000,000 vests annually
3. If founder leaves month 6: Gets 250,000 shares (25% of year 1)
4. Company can repurchase unvested shares at $0.0001/share

**Documentation**:
- Stock purchase agreement (2-3 pages)
- Restricted stock certificate
- Repurchase agreement (boilerplate)
- 409A valuation memo

**Tax treatment**:
- If > $0.0001 valuation: Founder pays tax on difference (AMT risk)
- If = $0.0001 valuation: No tax (stub value, common practice)
- Consult: 409A specialist before issuing

---

#### Stock Options (Employee Equity)

**When to grant**:
- First employees (engineers, product managers)
- Board members (at their request)
- Key contractors/consultants (rarely)

**How they work**:
1. Company reserves 10-20% of shares for option pool
2. Grant individual options (e.g., 50,000 options to engineer)
3. Vesting: 4 years, 1-year cliff (same as founder stock)
4. Exercise: Employee can buy shares at strike price
5. Tax: If exercised at <FMV, difference taxable as income

**Example**:
```
Grant: 100,000 options at $0.50/share strike (FMV at grant date)
Vesting: 4 years, 1 year cliff
Employee exercises at: Year 4 (fully vested)
  Cost: 100,000 × $0.50 = $50,000
  Benefit: Options now worth $5/share = $500,000
  Gain: $450,000 (taxable at long-term capital gains rates)
```

**Documentation**:
- Stock Option Plan (master document)
- Individual option agreements
- 409A valuation

---

#### Convertible Notes (Seed Funding)

**Structure** (used instead of common stock for seed rounds):
```
Investor gives: $500,000 cash
Company issues: Convertible Note (not stock)
  - Interest rate: 4-8% per year
  - Maturity: 24-36 months
  - Conversion: Into next round at discount (e.g., 20% discount)
  - Exception: Can convert to common if no Series A by maturity

Benefits for investor:
  - Discount to Series A valuation (20-30% typical)
  - Seniority in cap table
  - Less founder dilution upfront

Benefits for founder:
  - No valuation negotiation (no arguing on Series A price)
  - Simpler documentation
  - Keeps option pool intact
```

**Requirements**:
- Note agreement (2-3 pages, standard form from Y Combinator)
- Board resolution authorizing issuance
- 409A valuation update before Series A

---

#### Series A Preferred Stock (VC Funding)

**When companies raise $2-5M+ from VC investors**

**Terms typical of Series A**:
- Liquidation preference: 1x (company liquidates, investors get capital back first)
- Anti-dilution: Broad-based weighted average (protects investor from down rounds)
- Board seat: Lead investor gets 1 board seat
- Information rights: Quarterly financials + annual audit
- Pro-rata rights: Can invest in future rounds to maintain %)
- Conversion: Can convert to common at IPO

**Documentation**:
- Series A Preferred Stock agreement (40-60 pages)
- Certificate of Incorporation (amended)
- Investors' rights agreement
- Stockholders' agreement
- Registration rights agreement

**Founder impact**:
- Valuation set: $5M Series A on $5M post-money → founder owns 50%
- Dilution: Option pool reserved (10-20% new pool created)
- Board seat: Investor joins board
- Governance: New voter approval requirements

---

### Cap Table Management

**Keep a master cap table**:

| Shareholder | Common | Options | %-Fully Diluted | Notes |
|---|---|---|---|---|
| Founder A | 5,000,000 | - | 50.0% | 4yr vest, 1yr cliff |
| Founder B | 5,000,000 | - | 50.0% | 4yr vest, 1yr cliff |
| Option pool | - | 1,250,000 | - | 10% pool for employees |
| **Total** | **10,000,000** | **1,250,000** | **100.0%** | |

**Updates needed**:
- After each stock grant (option to employee)
- After each investment (convertible note or stock)
- Quarterly review (reconcile to cap table software)
- Annual update (true-up after audits/valuations)

**Tools**:
- Carta (most popular, $500-2K/month)
- Pulley (simpler, $300-1K/month)
- Google Sheets (free, for <10 shareholders)

---

## PART VII: BOARD GOVERNANCE & FIDUCIARY DUTIES

### Board Structure

**For pre-Series A (bootstrapping)**:
```
Board of Directors:
├── Founder/CEO (required)
└── Optional: 1-2 advisors (no formal voting rights)

Meetings: Quarterly (at minimum)
  - Jan: Annual meeting (elect directors, approve budget)
  - Apr, Jul, Oct: Quarterly meetings (financials, strategy)

Documentation:
  - Agenda sent 1 week before
  - Minutes taken (recorded decisions)
  - Resolutions for major decisions
  - Stored in minute book
```

**For Series A+ (after outside investment)**:
```
Board of Directors:
├── Founder/CEO (required)
├── Lead investor director (required)
├── Independent director #2 (required for governance)
└── Optional: Founder/advisor (non-voting observer)

Meetings: Quarterly + ad-hoc emergency meetings

Documentation: (same as above)
```

---

### Major Board Decisions & Approvals Required

```
Decision Type                    Required Approval
────────────────────────────────────────────────────
1. Annual budget & plan          Board vote
2. Material contract (>$100K)    Board vote
3. Hire/fire CFO or CTO          Board vote
4. Bank line of credit           Board vote
5. Leases (>$50K/month)          Board vote
6. New product line              Board vote
7. M&A activity                  Board vote
8. Related-party transactions    Board vote
9. Equity compensation plan      Board vote
10. Dividend payment             Board vote
11. Stock split                  Board vote
12. New class of stock           Board vote (+ shareholder)
13. Acquisition of company       Board + shareholder vote
14. IPO/exit planning            Board + shareholder vote
```

---

### Director Fiduciary Duties

**Three core duties (Delaware law)**:

#### 1. Duty of Care
- Directors must be informed (review materials, attend meetings)
- Must make reasoned business judgment
- Protected by "business judgment rule" if process sound

**What this means**:
- [ ] Review financials before each board meeting
- [ ] Ask tough questions about strategy
- [ ] Get expert opinion if unsure (accountant, lawyer)
- [ ] Document your reasoning

#### 2. Duty of Loyalty
- Directors must avoid conflicts of interest
- Cannot compete with company
- Must put company interests above own

**What this means**:
- [ ] Disclose conflicts before voting
- [ ] Recuse yourself from interested decisions
- [ ] Cannot start competing business
- [ ] Cannot take company opportunities

#### 3. Duty of Good Faith
- Directors must act honestly
- Cannot commit fraud
- Must report material problems

**What this means**:
- [ ] Report financial irregularities immediately
- [ ] Cannot cover up mistakes
- [ ] Must escalate material risks

---

### Board Meetings & Documentation

**Quarterly board meeting agenda template**:

```
BOARD MEETING AGENDA
Q1 2026 | January 31, 2026 | 9:00 AM PT | [Virtual/Conference Room]

Attendees: Founder/CEO, Investor director, Independent director
Observers: CFO (optional), Legal counsel (optional)

1. OPENING (5 minutes)
   - Agenda review
   - Conflicts of interest disclosure

2. FINANCIAL REPORT (15 minutes)
   - Revenue YTD: $[X]
   - Runway (months): [X] months
   - Key metrics: CAC, LTV, churn
   - Major expenses coming

3. STRATEGIC UPDATE (20 minutes)
   - Product roadmap update
   - Customer wins/losses
   - Team changes
   - Market opportunities/threats

4. RISK & COMPLIANCE (10 minutes)
   - Material legal issues
   - Insurance coverage gaps
   - Regulatory changes

5. DECISIONS & APPROVALS (10 minutes)
   - Vote on: [Material decision]
   - Resolution: [Formal board action]

6. EXECUTIVE SESSION (10 minutes)
   - [Private discussion without management]

ACTION ITEMS & NEXT MEETING
   - Document all decisions + assignments
   - Next meeting: April 30, 2026
```

**Board minutes template**:

```
BOARD MEETING MINUTES
Date: January 31, 2026
Attendees: [List with titles]
Absent: [None]

ACTIONS APPROVED:
1. Approved Q1 2026 budget ($150K monthly burn)
2. Authorized $500K bridge financing (convertible note)
3. Approved hiring plan (3 engineers, 1 sales person)
4. Approved new Data Processing Agreement (GDPR compliant)

DISCUSSIONS:
1. Customer churn at 5% annually (normal for B2B)
2. Market opportunity in healthcare (TAM $10B+)
3. Competitive pressure from Datadog

RISKS NOTED:
1. Team expansion from 10 to 15 people doubles burn
2. Enterprise customers requiring SOC 2 audit (3-4 months)

NEXT STEPS:
1. CFO: Obtain SOC 2 audit quotes by March 15
2. CEO: Close first healthcare customer by April 30
3. Board: Review cash runway at next meeting

Minutes recorded by: [Secretary name]
Next meeting: April 30, 2026
```

---

## PART VIII: COMPLIANCE CHECKLIST (BEFORE FIRST CUSTOMER)

### Tier 1: Day-One Requirements (Must have before charging)

```
Entity & Tax
─────────────────────────────────────────────────
[ ] Delaware C-Corporation incorporated
[ ] Certificate of Incorporation obtained
[ ] EIN obtained from IRS
[ ] Business bank account opened
[ ] Sales tax ID registered (home state)
[ ] Payroll system set up (if employees)

Governance & Legal
─────────────────────────────────────────────────
[ ] Bylaws adopted by board
[ ] Founder stock issued & vested
[ ] 409A valuation completed
[ ] Stock ledger (cap table) created
[ ] Board minute book established
[ ] Registered agent assigned

Insurance (CRITICAL)
─────────────────────────────────────────────────
[ ] Professional liability ($2-5M) with contractual
    liability endorsement
[ ] Directors & Officers liability ($1-2M)
[ ] Certificate of insurance obtained & kept on file

Contracts
─────────────────────────────────────────────────
[ ] Master Services Agreement (MSA) finalized
    (See legal/MSA_TEMPLATE.md)
[ ] Data Processing Agreement (DPA) for GDPR
[ ] Terms of Service finalized
[ ] Privacy Policy finalized
```

### Tier 2: Before First Customer Signature (1-2 weeks)

```
Customer-Specific Requirements
─────────────────────────────────────────────────
[ ] Customer MSA customized & executed
[ ] All exhibits completed (A, B, C, D, E)
[ ] Outcome metrics defined & objective
[ ] Baseline data collected (12+ months historical)
[ ] Customer signs data processing agreement (if EU)
[ ] Insurance certificate attached to customer file

Billing & Payment
─────────────────────────────────────────────────
[ ] Stripe/payment processor account set up
[ ] PCI DSS compliance self-assessment completed
[ ] Billing address captured in customer file
[ ] Tax ID captured in customer file
[ ] Sales tax exemption documented (if applicable)
[ ] Invoice template created (with payment terms)

Operational Readiness
─────────────────────────────────────────────────
[ ] Measurement infrastructure ready (data feeds)
[ ] Calculation formula tested (Exhibit A scenario)
[ ] Monthly measurement process documented
[ ] Customer communication plan ready (how to explain outcomes)
[ ] Dispute resolution process documented
[ ] Revenue recognition policy (ASC 606) applied
```

### Tier 3: Before First Dollar Invoiced

```
Validation Checklist
─────────────────────────────────────────────────
[ ] All documents signed & countersigned
[ ] Insurance broker confirmed contractual liability in place
[ ] Finance team trained on outcome metric tracking
[ ] External auditor (if applicable) briefed on outcome model
[ ] First month's baseline data validated
[ ] Outcome calculation performed & validated
[ ] Revenue recognized per ASC 606 (apply constraint)
[ ] Invoice created with transparent outcome breakdown
[ ] Payment terms confirmed with customer

Risk Mitigation
─────────────────────────────────────────────────
[ ] 4-level escalation process documented
[ ] Dispute tracking system created
[ ] Customer communication schedule established
[ ] Monthly reporting template prepared
[ ] Quarterly review meeting scheduled
```

---

## PART IX: TIMELINE & CRITICAL PATH (3 WEEKS)

### Week 1: Incorporation & Tax

```
DAY 1-2: Incorporation
  [ ] File Certificate of Incorporation (Delaware)
      → Deliverable: Certificate from DE Secretary of State

  [ ] Create bylaws & adopt at founder meeting
      → Deliverable: Bylaws signed + minute book entry

  [ ] Issue founder stock with vesting
      → Deliverable: Stock certificate + repurchase agreement

DAY 3: Tax Setup
  [ ] Apply for EIN (same-day online)
      → Deliverable: EIN letter from IRS

  [ ] Open business bank account
      → Deliverable: Checks, debit card, online access

  [ ] Register for sales tax ID
      → Deliverable: Sales tax ID, quarterly payment schedule

DAY 4-5: 409A Valuation
  [ ] Engage 409A valuation specialist
      → Deliverable: Valuation report ($0.0001 FMV)

DAY 6-7: Governance
  [ ] Create cap table & stock ledger
      → Deliverable: Master cap table spreadsheet

  [ ] Create option pool (10-20% reserved)
      → Deliverable: Board resolution authorizing ESOP
```

### Week 2: Insurance & Contracts

```
DAY 1-3: Insurance Quote & Apply
  [ ] Solicit 3+ insurance quotes
      → Coverage: E&O $2-5M, D&O $1-2M, Cyber $1-5M
      → Deliverable: Quote spreadsheet + comparison

  [ ] Select insurer & apply
      → Timeline: 1-2 weeks to policy issuance
      → Deliverable: Application submitted

DAY 4-7: Contract Templates
  [ ] Review MSA template from legal/MSA_TEMPLATE.md
      → Customize for your service type

  [ ] Prepare DPA (Data Processing Agreement)
      → Required for any EU customers

  [ ] Prepare Terms of Service & Privacy Policy
      → Deliverable: Reviewed by attorney
```

### Week 3: Customer Preparation

```
DAY 1-3: Outcome Definition
  [ ] Define 3-5 candidate outcome metrics
      → Metric must be objective, measurable, verifiable
      → Deliverable: Outcome metrics library

  [ ] Calculate baseline for pilot customer
      → 12+ months historical data
      → Deliverable: Baseline spreadsheet with formulas

DAY 4-5: Measurement Infrastructure
  [ ] Set up data feeds (customer logs → your system)
      → Automated or manual depending on customer

  [ ] Test calculation formula
      → Run scenarios with historical data
      → Deliverable: Validated calculation model

DAY 6-7: Customer Readiness
  [ ] Identify & approach first customer
      → Customer willing to be pilot/reference

  [ ] Get insurance policy in hand (if not already)
      → CRITICAL: DO NOT sign contract without this
```

---

## PART X: SUMMARY & NEXT STEPS

### What you've established (by end of 3 weeks):

```
✓ Venture-ready legal entity (Delaware C-Corp)
✓ Tax ID & payroll infrastructure
✓ Governance framework (board, bylaws, minutes)
✓ Equity structure (founder stock, option pool, cap table)
✓ Comprehensive insurance (E&O, D&O, Cyber)
✓ Production-ready contracts (MSA, DPA, TOS)
✓ Measurement infrastructure for outcomes
✓ Revenue recognition policy (ASC 606)
```

### Ready to charge customers on Day 1?

**YES** - if all Tier 1 requirements complete.

**Verification**:
```
Legal entity: Delaware C-Corp, EIN, bank account ✓
Insurance: Professional liability with contractual
          liability endorsement IN PLACE ✓
Contracts: Signed MSA, DPA, executed by both parties ✓
Outcomes: Objective metrics defined, baseline data ✓
Payment: PCI DSS compliant, invoice ready ✓
```

### Recommended reading:

1. **legal/LEGAL_FRAMEWORK.md** (outcome-based contract enforceability)
2. **legal/MSA_TEMPLATE.md** (customize for your service)
3. **legal/REGULATORY_ROADMAP.md** (14-week implementation)
4. **This document** (incorporation & governance)

---

**Document prepared**: January 2026
**Status**: Production-ready
**Confidence**: HIGH (with legal counsel review)

**Next step**: Engage corporate attorney for state-specific guidance + contract review

---

## Glossary

**Certificate of Incorporation**: Official document creating the corporation (filed with state)

**Bylaws**: Internal governance rules (board meetings, voting, shareholder rights)

**409A Valuation**: IRS-compliant fair market value of stock at grant date (required for options)

**Vesting schedule**: How much stock/options employee earns over time (e.g., 4 years, 1 year cliff)

**Repurchase right**: Company right to buy back unvested shares if employee leaves

**Option pool**: Percentage of total shares reserved for future employee grants (typically 10-20%)

**Cap table**: Master list of all shareholders, stock, options (who owns what % of company)

**Liquidation preference**: In a sale, investor's payment priority (1x = get capital back first)

**Anti-dilution**: Investor protection from down rounds (% ownership protected if next round is lower price)

**Material contract**: Agreement > some threshold (typically $100K) requiring board approval

**Nexus**: Physical or economic connection triggering tax/legal obligation in a jurisdiction

---

**Document completed**: January 2026
**Author**: Production Validation Agent
**Review status**: Ready for attorney review before first customer
