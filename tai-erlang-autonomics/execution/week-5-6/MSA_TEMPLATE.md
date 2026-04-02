# Master Service Agreement (MSA) - TAI Erlang Autonomics

**THIS AGREEMENT IS READY TO CUSTOMIZE FOR EACH CUSTOMER**

---

## MASTER SERVICE AGREEMENT

**Effective Date**: [CUSTOMER COMPLETES: Today's date]

**BETWEEN:**

**SERVICE PROVIDER**:
- Legal Name: TAI Erlang Autonomics, Inc.
- Address: [COMPANY ADDRESS - fill in your registered office]
- Phone: [PHONE]
- Email: [EMAIL]
- Tax ID/EIN: [YOUR EIN - from Week 1]

**CUSTOMER**:
- Legal Name: [CUSTOMER COMPLETES: Exact legal entity name]
- Address: [CUSTOMER COMPLETES: Registered business address]
- Phone: [CUSTOMER COMPLETES: Contact number]
- Email: [CUSTOMER COMPLETES: Primary contact email]
- Tax ID/EIN: [CUSTOMER COMPLETES: Customer's tax ID]

---

## 1. SERVICES

**1.1 Service Description**

TAI agrees to provide the Customer with TAI Erlang Autonomics services as specified in the Statement of Work (SOW) attached as Exhibit A. The services include:

- Autonomic SKU management and entitlement governance on Google Cloud Platform (GCP) Cloud Run
- Cryptographically verified receipt ledger for all entitlement changes
- Real-time entitlement enforcement with <50ms latency SLA
- Audit trail and compliance reporting
- 12 months of implementation support and onboarding

**1.2 Service Level**

TAI commits to the following Service Level Agreements (SLAs):

| SLA Metric | Target | Excluded |
|-----------|--------|----------|
| **Availability** | 99.5% uptime | Scheduled maintenance (max 4 hrs/month, off-peak windows) |
| **Response Time** | <50ms p99 for entitlement checks | Network latency outside GCP |
| **Support Response** | <4 hours for critical issues (M-F 9-5 PT) | Customer-caused outages |
| **Data Retention** | 90-day audit trail | Archived data available at $[X]/request |

If TAI fails to meet Availability SLA, Customer receives proportional service credit:
- 99.0-99.49% uptime: 5% monthly fee credit
- 98.0-98.99% uptime: 10% monthly fee credit
- <98% uptime: 25% monthly fee credit

**1.3 Scope of Work**

The detailed scope of work is defined in the SOW (Exhibit A), including:
- Discovery and configuration (Weeks 1-2)
- Testing and UAT (Weeks 3-4)
- Production cutover (Weeks 5-6)
- Training and optimization (Weeks 7-8)

---

## 2. FEES & PAYMENT TERMS

**2.1 Pricing**

Annual Service Fee: **$[CUSTOMER COMPLETES: Negotiated price]** per year

This fee includes:
- [ ] License for TAI Erlang Autonomics on Customer's GCP project
- [ ] 12 months of implementation and onboarding support
- [ ] Cryptographic receipt ledger generation
- [ ] Monthly performance reporting
- [ ] Support escalation (4-hour response SLA)

**2.2 Payment Schedule**

| Milestone | Amount | Due Date |
|-----------|--------|----------|
| Contract Signature | 50% of annual fee = $[X] | Within 5 business days of signature |
| Production Go-Live | 50% of annual fee = $[X] | Within 5 business days of go-live |

**2.3 Payment Method**

Customer shall pay via:
- [ ] ACH transfer to: [BANK ACCOUNT - your treasury details]
- [ ] Credit card via Stripe: [STRIPE PAYMENT LINK]
- [ ] Wire transfer to: [WIRE INSTRUCTIONS]

**2.4 Renewal & Price Adjustment**

After 12-month initial term:
- Renewal price: $[CUSTOMER COMPLETES: Year 2 price, typically 20% discount from Year 1]
- Annual price increase thereafter: Up to [CPI + 5%] per year
- Renewal term: 12 months auto-renewal unless either party provides 60-day notice of non-renewal

**2.5 Late Payment**

Payments not received within 30 days of invoice will accrue late fees:
- Late payment fee: 1.5% per month (18% annually) on outstanding balance
- If payment is not received within 60 days, TAI may suspend services without liability

---

## 3. TERM & TERMINATION

**3.1 Initial Term**

This Agreement begins on the Effective Date and continues for **12 months** ("Initial Term").

**3.2 Renewal**

Unless either party provides written notice of non-renewal at least 60 days before the end of the then-current term, this Agreement will automatically renew for successive 12-month periods.

**3.3 Termination for Cause**

Either party may terminate this Agreement immediately upon written notice if:

**(a) Material Breach**: The other party materially breaches this Agreement and fails to cure within 30 days of written notice.

**(b) Insolvency**: The other party becomes insolvent, bankrupt, or makes an assignment for the benefit of creditors.

**(c) Uncured Payment Default**: Customer fails to pay invoices within 60 days despite written demand.

**3.4 Termination for Convenience**

Either party may terminate this Agreement without cause by providing 90 days' written notice. Upon termination:

- [ ] Customer remains responsible for fees through the notice period
- [ ] TAI will provide 30-day data migration window (no charge)
- [ ] After 30 days, archived data may be deleted without further notice

**3.5 Effect of Termination**

Upon termination:
1. Customer must pay all outstanding invoices within 10 days
2. TAI ceases providing Services on the termination date
3. Customer may export all data within 30 days at no charge
4. After 30 days, data is deleted; TAI has no obligation to retain
5. Confidentiality and liability provisions survive termination

---

## 4. SECURITY & DATA PROTECTION

**4.1 Data Encryption**

TAI maintains:
- **At-rest encryption**: AES-256 on Google Cloud Storage
- **In-transit encryption**: TLS 1.2 or higher for all API communications
- **Key management**: Google Cloud KMS with rotation every 90 days

Customer may request customer-managed keys (CMK) for an additional fee of $[X]/month.

**4.2 Access Control & Authentication**

TAI implements:
- **Role-Based Access Control (RBAC)** with 4 tiers: Admin / Operator / Viewer / Auditor
- **Multi-Factor Authentication (MFA)** required for all admin access
- **Service accounts** with least-privilege IAM roles
- **Audit logging** of all admin actions (90-day retention)

Customer can audit access logs upon request.

**4.3 Data Location & Residency**

Customer data is stored in **[GCP Region - e.g., us-central1]** unless otherwise requested.

Options:
- [ ] Default: us-central1 (USA, multi-zone redundancy)
- [ ] GDPR-compliant: eu-west1 (Ireland, EU data residency)
- [ ] APAC: asia-southeast1 (Singapore)

Data is automatically backed up to a separate region (geo-redundancy).

**4.4 Compliance Certifications**

TAI maintains:
- [ ] SOC 2 Type II audit (annual, available upon request under NDA)
- [ ] ISO 27001 certification (target: achieve by [date])
- [ ] GDPR Data Processing Agreement (Exhibit B)
- [ ] CCPA compliance documentation available for CA-based customers

**4.5 Sub-Processors**

Customer acknowledges TAI uses third-party services:
- **Google Cloud Platform**: Compute, storage, networking
- **Stripe**: Payment processing
- [Add any other critical sub-processors]

TAI maintains Data Processing Agreements with all sub-processors.

---

## 5. INTELLECTUAL PROPERTY

**5.1 TAI Intellectual Property**

TAI retains all rights, title, and interest in:
- TAI Erlang Autonomics software and source code
- Algorithms, templates, and documentation
- All improvements, modifications, and derivative works created by TAI

Customer receives a limited, non-exclusive, non-transferable license to use TAI Services for Customer's own business purposes during the Agreement term.

**5.2 Customer Data & Intellectual Property**

Customer retains all rights to:
- Customer data (SKU definitions, entitlement rules, customer records)
- Customer's business processes and proprietary information
- Any derivative works created by Customer using TAI Services

TAI may:
- [ ] Use anonymized, aggregated data for product improvement
- [ ] Use performance metrics for benchmarking (with permission)
- [ ] NOT share Customer data with competitors or third parties

**5.3 Feedback & Suggestions**

Customer agrees that any feedback or suggestions provided about TAI Services may be used by TAI without restriction or compensation.

---

## 6. INDEMNIFICATION & LIABILITY

**6.1 TAI Indemnification**

TAI indemnifies Customer against third-party claims that TAI Services infringe upon intellectual property rights. TAI's obligations are conditioned on Customer:
- [ ] Providing prompt written notice of claim
- [ ] Allowing TAI sole control of defense and settlement
- [ ] Cooperating fully with TAI's defense

Remedies if claim is upheld: TAI may (at its option):
- [ ] Obtain rights for Customer to continue use
- [ ] Replace or modify Services to make non-infringing
- [ ] If neither is commercially reasonable, terminate Agreement and refund unused fees

**6.2 Customer Indemnification**

Customer indemnifies TAI against third-party claims arising from:
- Customer's use of TAI Services in violation of applicable law
- Customer's modification of TAI Services (beyond Customer Data)
- Customer's combination of TAI Services with non-TAI products
- Customer's breach of confidentiality or intellectual property obligations

---

## 7. LIMITATION OF LIABILITY

**7.1 Limitation of Liability**

**EXCEPT FOR EITHER PARTY'S INDEMNIFICATION OBLIGATIONS OR BREACH OF CONFIDENTIALITY**, IN NO EVENT SHALL EITHER PARTY BE LIABLE TO THE OTHER FOR:

- Indirect damages (lost profits, lost revenue, lost savings)
- Incidental damages (business interruption, delay)
- Consequential damages (data loss, reputational harm)
- Punitive or exemplary damages

**7.2 Liability Cap**

EACH PARTY'S TOTAL LIABILITY UNDER THIS AGREEMENT SHALL NOT EXCEED THE LESSER OF:
- **(a)** Fees paid or payable by Customer in the 12 months preceding the claim, OR
- **(b)** $[PRICING CAP - typically $250K-$500K]

This cap applies to all claims, regardless of legal theory.

**7.3 Essential Terms**

This limitation is an essential term; without it, neither party would enter into this Agreement.

---

## 8. CONFIDENTIALITY

**8.1 Confidential Information**

"Confidential Information" means all non-public information disclosed by one party ("Discloser") to the other ("Recipient"), including:
- Source code, algorithms, technical specifications
- Customer data and business information
- Financial information and pricing terms
- Business plans and strategies

**8.2 Obligations**

Recipient agrees to:
- [ ] Treat Confidential Information as confidential
- [ ] Disclose only to employees/contractors with a need-to-know
- [ ] Implement reasonable security measures to protect
- [ ] Not use for any purpose other than this Agreement

**8.3 Exceptions**

Confidential Information does not include information that:
- Is publicly available (not through breach of this Agreement)
- Was independently developed without reference to Confidential Information
- Is required to be disclosed by law (with prior notice if permitted)

**8.4 Survival**

Confidentiality obligations survive termination of this Agreement for 3 years.

---

## 9. INSURANCE

**9.1 TAI Insurance**

TAI maintains:
- **General Liability**: $1,000,000 per occurrence / $2,000,000 aggregate
- **Professional Liability (E&O)**: $1,000,000 per claim / $2,000,000 aggregate
- **Cyber Liability**: $1,000,000 per claim (if available)

Certificates available upon request. Customer may request to be named as "Additional Insured" (subject to insurer approval; typically 2-4 week endorsement).

**9.2 Insurance Changes**

TAI will provide 30 days' notice of any material reduction in coverage.

---

## 10. COMPLIANCE & LEGAL

**10.1 Regulatory Compliance**

TAI commits to comply with applicable laws, including:
- GDPR (for EU personal data)
- CCPA and CPRA (for California resident data)
- SOX, if Customer is a publicly traded company
- Industry-specific regulations (HIPAA if health data, PCI-DSS if payment card data)

**10.2 Data Processing Agreement (GDPR)**

If Customer is an EU entity or processes EU resident data, the Data Processing Agreement (Exhibit B) is incorporated by reference and governs all data processing.

**10.3 Subpoenas & Government Requests**

If TAI receives a government request for Customer data:
- TAI will provide Customer with prompt notice (unless legally prohibited)
- TAI will request confidentiality/minimization from authorities
- TAI will disclose only the minimum information legally required

**10.4 Compliance Audits**

Customer may audit TAI's compliance upon reasonable notice:
- Frequency: Up to 1 audit per calendar year
- Notice: 15 business days advance notice required
- Location: On-site at TAI facilities or remote
- Cost: Customer covers audit expenses (TAI provides reasonable cooperation at no cost)

---

## 11. REPRESENTATIONS & WARRANTIES

**11.1 TAI's Representations**

TAI represents:
- [ ] It is duly organized, validly existing, and in good standing
- [ ] It has full authority to execute and perform this Agreement
- [ ] Services will be performed in a professional and workmanlike manner
- [ ] Services do not infringe third-party intellectual property rights
- [ ] It has no knowledge of any pending claims against it

**11.2 Customer's Representations**

Customer represents:
- [ ] It is duly organized, validly existing, and in good standing
- [ ] It has full authority to execute and perform this Agreement
- [ ] Customer Data does not infringe third-party rights
- [ ] Use of Services complies with all applicable laws
- [ ] It is not on any sanctions list (OFAC, UN, EU)

**11.3 Disclaimer**

EXCEPT AS EXPRESSLY STATED IN THIS AGREEMENT, TAI PROVIDES SERVICES "AS IS" WITHOUT WARRANTIES, EXPRESS OR IMPLIED, INCLUDING:
- **NO WARRANTY OF MERCHANTABILITY** (fitness for a particular purpose)
- **NO WARRANTY OF NON-INFRINGEMENT** (except as stated in Section 6.1)
- **NO WARRANTY OF PERFORMANCE** (beyond SLAs stated in Section 1.2)

---

## 12. GENERAL PROVISIONS

**12.1 Entire Agreement**

This Agreement, including SOW (Exhibit A) and DPA (Exhibit B), constitutes the entire understanding between the parties. It supersedes all prior agreements, negotiations, and understandings.

**12.2 Amendments**

Amendments to this Agreement must be in writing and signed by both parties. No oral agreements are binding.

**12.3 Severability**

If any provision is found invalid or unenforceable:
- That provision is severed
- All other provisions remain in full force and effect
- If severance would materially alter the Agreement, either party may terminate

**12.4 Waiver**

Failure to enforce any provision does not constitute waiver of that provision or any other provision.

**12.5 Assignment**

Neither party may assign this Agreement without the other party's written consent, except:
- TAI may assign to an affiliate or as part of a business acquisition (with notice)
- Customer may assign to a successor in a merger or acquisition (with notice)

Any unauthorized assignment is void.

**12.6 Notices**

All notices must be in writing and delivered:
- Email to the primary contact listed above, OR
- Mail to the address listed above, certified mail return receipt

Notices are effective upon receipt.

**12.7 Relationship of Parties**

Nothing in this Agreement creates a partnership, joint venture, or agency relationship. Neither party is authorized to bind the other without written consent.

**12.8 Counterparts**

This Agreement may be executed in counterparts (electronic signatures via DocuSign, email, or fax count as originals).

**12.9 Governing Law & Jurisdiction**

This Agreement is governed by the laws of **[State, typically Delaware or where company is incorporated]**, without regard to conflicts of law principles.

Exclusive jurisdiction for disputes: **[Specify: U.S. Federal Courts in [district] OR [State] State Courts]**

---

## 13. SIGNATURES

**IN WITNESS WHEREOF**, the parties have executed this Agreement as of the Effective Date.

**FOR TAI ERLANG AUTONOMICS, INC.**:

Signature: ___________________________

Name (print): ___________________________

Title: ___________________________

Date: ___________________________


**FOR CUSTOMER**:

Company Name (print): ___________________________

Signature: ___________________________

Name (print): ___________________________

Title: ___________________________

Date: ___________________________

Email: ___________________________

Phone: ___________________________

---

## EXHIBIT A: STATEMENT OF WORK

[See separate SOW document - SOW_TEMPLATE.md]

---

## EXHIBIT B: DATA PROCESSING AGREEMENT (GDPR)

[See separate DPA document - DPA_TEMPLATE.md]

---

**Document Version**: 1.0
**Template Status**: READY TO CUSTOMIZE
**Last Updated**: 2026-01-26

### How to Use This Template:

1. **Copy this document** for each new customer
2. **Find & Replace**:
   - `[COMPANY ADDRESS]` → Your registered office
   - `[CUSTOMER COMPLETES: ...]` → Sections for customer to fill in
   - `[X]` → Placeholder prices/numbers to be negotiated
   - `[State, typically Delaware]` → Your incorporation state
3. **Legal review**: Have your attorney review the customized version before sending
4. **Send to customer**: Upload to DocuSign or print for wet signature
5. **Track signature progress**: Monitor in DocuSign until both parties have signed

