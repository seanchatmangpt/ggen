# Data Processing Agreement (DPA) - TAI Erlang Autonomics

**THIS DOCUMENT IS ATTACHED TO THE MSA AS EXHIBIT B**

**Applicable To**: Customers processing EU resident personal data (GDPR compliance)

---

## DATA PROCESSING AGREEMENT

**Effective Date**: [Same as MSA]

**Between:**

**DATA CONTROLLER**: [Customer Legal Name] ("Customer")

**DATA PROCESSOR**: TAI Erlang Autonomics, Inc. ("TAI" or "Processor")

---

## 1. SUBJECT MATTER & DURATION

**1.1 Scope of Processing**

This DPA governs TAI's processing of personal data on behalf of Customer in connection with providing TAI Erlang Autonomics services as described in the MSA.

**Personal Data Processed**:
- Customer user information (names, email addresses, roles)
- Customer entitlement data (SKU assignments tied to customer identifiers)
- System logs and audit trails (containing user identifiers and timestamps)

**Categories of Data Subjects**: Customer's end-users (employees, customers, contractors)

**1.2 Duration**

This DPA applies for the term of the MSA and survives termination for 90 days (data retention period).

---

## 2. ROLES & RESPONSIBILITIES

**2.1 Customer as Controller**

Customer is the **Data Controller** and is responsible for:
- Determining purposes and means of personal data processing
- Ensuring lawful basis for collecting personal data (consent, contract, legal obligation, etc.)
- Providing privacy notices to data subjects
- Responding to data subject requests (access, deletion, portability)
- Complying with GDPR Chapters 1-3 (overall GDPR obligations)

**2.2 TAI as Processor**

TAI is the **Data Processor** and is responsible for:
- Processing personal data only on documented instructions from Customer
- Ensuring persons authorized to process personal data are under confidentiality obligations
- Implementing technical and organizational measures (security)
- Assisting Customer with data subject rights requests
- Assisting Customer with compliance obligations
- Deleting or returning personal data upon termination

**2.3 Sub-Processors**

TAI engages the following sub-processors:

| Sub-Processor | Service | Data Processed | Location |
|---------------|---------|-----------------|----------|
| Google Cloud Platform | Compute, Storage, Logging | All personal data | US (default) / EU (if customer requests) |
| Stripe | Payment processing | Customer company name, tax ID | US |
| [Add others as applicable] | [Service] | [Data categories] | [Location] |

Customer is notified of sub-processors at signature. TAI will provide 30 days' notice before adding/removing sub-processors, allowing Customer to object on reasonable grounds.

---

## 3. PROCESSING INSTRUCTIONS

**3.1 Authorized Processing**

TAI will process personal data only in accordance with:
- Customer's written instructions (via MSA/SOW or email request)
- Applicable EU Data Protection Laws
- This DPA

**3.2 Processing Activities**

TAI will perform the following processing activities:
- [ ] Data entry/storage: Ingesting Customer's user/entitlement data into TAI systems
- [ ] Querying: Executing entitlement checks (API calls querying personal data)
- [ ] Logging: Recording audit trail data (user IDs, timestamps, actions)
- [ ] Analysis: Performance analytics (aggregated, anonymized metrics)
- [ ] Deletion: Removing data upon Customer's request

**3.3 Personal Data Categories**

| Category | Examples | Retention |
|----------|----------|-----------|
| **Identification** | User name, email, employee ID | For contract duration + 90 days |
| **Location** | IP address, GCP region | For audit trail only (90 days) |
| **System** | Log entries, API call records | For audit trail only (90 days) |
| **Behavioral** | Feature usage, entitlement checks | Aggregated only (no personal identifiers) |

**3.4 Unauthorized Processing**

TAI will **NOT**:
- [ ] Use personal data for its own purposes (except as required by law)
- [ ] Combine personal data with other data sources
- [ ] Process personal data beyond the scope of services
- [ ] Share personal data with competitors or non-affiliated third parties

---

## 4. SECURITY & DATA PROTECTION MEASURES

**4.1 Technical Measures**

TAI implements:
- **Encryption at rest**: AES-256 on Google Cloud Storage
- **Encryption in transit**: TLS 1.2+ (minimum) for all data flows
- **Access controls**: Role-based access control (RBAC) with authentication
- **Multi-factor authentication (MFA)**: Required for all admin access
- **Network isolation**: Customer data segregated by project/tenant
- **Intrusion detection**: Cloud Security Command Center monitoring
- **Backup & recovery**: Automated daily backups, 90-day retention, tested recovery

**4.2 Organizational Measures**

TAI implements:
- **Data protection by design**: Minimizing personal data collection
- **Staff training**: Annual data protection & security training for all personnel
- **Confidentiality obligations**: All employees sign confidentiality agreements
- **Audit trails**: 90-day retention of system access logs
- **Incident response**: 4-hour notification SLA for data breaches
- **Vendor management**: Data Processing Agreements with all sub-processors

**4.3 Security Certifications**

TAI maintains:
- [ ] SOC 2 Type II audit (annual, available upon request)
- [ ] ISO 27001 certification (target: [date])
- [ ] Annual penetration testing (results available under NDA)

---

## 5. DATA SUBJECT RIGHTS

**5.1 Rights Support**

Customer may request that TAI assist with the following data subject rights under GDPR Articles 15-22:

| Right | Customer Request | TAI Response | Timeline |
|------|-----------------|-------------|----------|
| **Access (Art. 15)** | "Export data for user X" | TAI exports personal data in CSV/JSON format | 10 business days |
| **Rectification (Art. 16)** | "Update email for user X" | TAI updates field in system | 2 business days |
| **Erasure (Art. 17)** | "Delete data for user X" | TAI deletes from production; archived after 90 days | 2 business days (+ 90 day archive) |
| **Restriction (Art. 18)** | "Block processing for user X" | TAI restricts access (marks as "restricted") | 2 business days |
| **Portability (Art. 20)** | "Export all data" | TAI exports all data in standard format | 10 business days |
| **Objection (Art. 21)** | "Stop marketing use" | TAI stops processing for that purpose | Immediate |

**5.2 Request Process**

To request data subject right support:
1. Customer submits written request to [email]
2. TAI verifies Customer identity (email match)
3. TAI performs action or explains if unable
4. TAI confirms completion within specified timeline

**5.3 TAI Limitations**

TAI will assist with data subject requests **only if Customer is the lawful controller**. Customer is responsible for:
- Verifying data subject identity (TAI will not authenticate end-users)
- Providing TAI with documented instructions
- Ensuring lawful basis for the request

---

## 6. DATA TRANSFER & LOCATION

**6.1 Data Location (Default)**

Unless Customer requests otherwise, personal data is stored in:
- **Primary**: Google Cloud US region (us-central1)
- **Backup**: Geo-redundant backup (separate region)

**6.2 Alternative Locations**

Customer may request EU data residency:
- [ ] **EU Default**: eu-west1 (Ireland)
- [ ] **EU GDPR-Compliant**: All processing within EU territory
- [ ] **Additional Cost**: $[X]/month for EU-only infrastructure

**6.3 International Data Transfers (Standard Contractual Clauses)**

If data is transferred outside the EU:
- TAI has executed **Standard Contractual Clauses** (SCCs) with sub-processors
- SCCs are available upon request (include in DPA Attachment)
- Transfers comply with GDPR Chapter 5 (Article 46, standard contractual clauses)

**6.4 Adequacy Decisions**

For transfers to/from countries WITH adequacy decisions (e.g., US data transfers):
- TAI complies with applicable data transfer mechanisms
- Customer remains responsible for assessing transfer legality

---

## 7. DATA RETENTION & DELETION

**7.1 Retention Schedule**

TAI retains personal data as follows:

| Data Type | Retention Period | Reason |
|-----------|-----------------|--------|
| **Active user data** | For contract duration | Required for services |
| **Audit logs** | 90 days | Compliance & troubleshooting |
| **Backups** | 30 days (incremental), 365 days (annual full) | Disaster recovery |
| **Archives** | [Customer specifies] | Compliance (may be required to retain) |
| **Deleted data** | 90 days post-deletion (audit trail only) | Data recovery if needed |

**7.2 Deletion Upon Termination**

Upon MSA termination or data subject request:
- [ ] TAI deletes personal data from production systems within 2 business days
- [ ] Backups deleted within 90 days
- [ ] Archive data (if retained per Customer request) moved to cold storage
- [ ] Deletion certificate provided to Customer

**7.3 Deletion Exception**

TAI may retain personal data if **required by law** (e.g., tax records, regulatory holds). TAI will:
- Notify Customer of legal hold
- Continue to protect data with same security measures
- Delete when hold expires

---

## 8. AUDITS & COMPLIANCE VERIFICATION

**8.1 Audit Rights**

Customer may audit TAI's compliance with this DPA:
- **Frequency**: Up to 1 audit per calendar year
- **Notice**: 15 business days advance notice required
- **Scope**: TAI's data protection & security measures
- **Cost**: TAI provides reasonable cooperation; Customer pays audit expenses
- **Confidentiality**: Audit results kept confidential under NDA

**8.2 Audit Methods**

TAI supports:
- [ ] **Self-assessment questionnaire** (TAI provides responses)
- [ ] **On-site audit** (at TAI facilities, by Customer or auditor)
- [ ] **SOC 2 Type II report** (annual audit by 3rd party, available under NDA)
- [ ] **Compliance certifications** (SOC 2, ISO 27001, provided when available)

**8.3 Compliance Reporting**

TAI provides monthly/quarterly compliance reports including:
- Data access logs (who accessed what, when)
- Security incidents (if any)
- Sub-processor changes
- Compliance certifications status

---

## 9. INCIDENT RESPONSE & BREACH NOTIFICATION

**9.1 Data Breach Definition**

A "breach" includes:
- Unauthorized access to personal data
- Accidental loss of personal data
- Unauthorized alteration of personal data
- Encryption key loss or compromise
- Infrastructure outage exceeding 4 hours

**9.2 TAI Notification Obligation**

Upon discovering a breach, TAI will:
1. **Notify Customer within 4 hours** (or immediately if critical)
   - Notification method: Email + phone to emergency contact
   - Content: Incident description, data categories affected, likely consequences
2. **Provide incident details within 24 hours**:
   - Root cause analysis
   - Scope (how many data subjects affected)
   - Remediation steps taken
   - Timeline to resolution
3. **Provide final incident report within 7 days**:
   - Complete analysis
   - Preventive measures implemented
   - Customer notification recommendations

**9.3 Customer Notification Authority**

- Customer is responsible for notifying data subjects & regulators per GDPR Article 33
- TAI provides recommended notification language + data needed
- TAI cooperates with Customer's breach response (forensics, communications)

**9.4 Regulatory Cooperation**

If regulators (e.g., national data protection authority) investigate:
- TAI cooperates fully with investigation
- TAI provides requested documentation within 10 business days
- TAI contacts Customer immediately of regulatory inquiry

---

## 10. PROCESSOR INSTRUCTIONS & CHANGE MANAGEMENT

**10.1 Standing Instructions**

TAI processes personal data only in accordance with:
- This DPA
- The MSA & SOW
- Documented written instructions from Customer

**10.2 Requesting Processing Changes**

Customer may request processing changes (e.g., "delete data older than 1 year") by:
1. **Email**: Sending detailed written request to [email]
2. **Approval**: TAI reviews feasibility (responds within 2 business days)
3. **Implementation**: TAI implements within agreed timeline
4. **DPA Amendment**: If material change, parties sign DPA amendment

**10.3 Emergency Instructions**

In emergency situations (e.g., security incident, legal hold):
- Customer may provide verbal instructions (email confirmation required within 24 hours)
- TAI implements immediately
- Parties document change in writing

---

## 11. SUB-PROCESSORS & VENDOR MANAGEMENT

**11.1 Approved Sub-Processors**

TAI has disclosed the following sub-processors (as of signature date):

| Sub-Processor | Service | Location | DPA in Place |
|---------------|---------|----------|--------------|
| Google Cloud Platform | Compute, storage, logging | US/EU | Yes (Google Customer DPA) |
| Stripe | Payment processing | US | Yes (Stripe DPA) |
| [Add others] | [Service] | [Location] | [Yes/No] |

**11.2 Sub-Processor Changes**

Before engaging a new sub-processor:
- TAI provides 30 days' written notice to Customer
- Notice includes: sub-processor name, location, processing activities
- Customer may object on reasonable grounds (e.g., security concerns)
- If objection, parties negotiate resolution or Customer may terminate

**11.3 Sub-Processor DPAs**

TAI ensures all sub-processors have:
- Data Processing Agreements in place
- Equivalent data protection obligations
- Same security standards as TAI

TAI is liable to Customer for any sub-processor's failure to meet DPA obligations.

---

## 12. RETURN OR DELETION OF DATA

**12.1 Upon Termination**

Within 30 days of MSA termination, TAI will:
- [ ] **Return** all personal data to Customer (in portable format: CSV/JSON)
- **OR**
- [ ] **Delete** all personal data (if requested)

**12.2 Return Format**

Returned data includes:
- All user records (names, emails, identifiers)
- All entitlement records (tied to user)
- All audit logs (if Customer requests)

Format: CSV or JSON (Customer specifies), with data dictionary.

**12.3 Verification**

TAI provides deletion certificate:
- Date of deletion
- Data categories deleted
- Confirmation of no backups remaining
- Signed by TAI officer

---

## 13. GDPR COMPLIANCE OBLIGATIONS

**13.1 GDPR Principles**

TAI commits to GDPR Article 5 principles:
- **Lawfulness**: Processing only per Customer's lawful instructions
- **Purpose limitation**: Data used only for services
- **Data minimization**: Collecting only necessary data
- **Accuracy**: Assisting Customer with data accuracy
- **Storage limitation**: Retaining only as long as needed
- **Integrity & confidentiality**: Protecting from unauthorized access
- **Accountability**: Documenting compliance measures

**13.2 Data Protection Impact Assessment (DPIA)**

Customer may request TAI's assistance with DPIA:
- TAI provides documentation of processing activities
- TAI identifies security & privacy risks
- TAI recommends mitigation measures
- TAI provides 20 days for Customer to conduct DPIA

**13.3 Data Protection Officer (DPO) Coordination**

If Customer has a DPO:
- TAI designates point of contact for DPO inquiries
- TAI responds to DPO questions within 10 business days
- TAI copies Customer's DPO on relevant communications

---

## 14. LIMITATION OF LIABILITY

**14.1 Data Protection Liability**

TAI's liability for data protection breaches is limited to:
- Direct damages (not indirect/consequential damages)
- Documented losses (not speculative)
- Lesser of: (a) fees paid by Customer or (b) $[250K per Article 82 GDPR]

**14.2 Customer's Responsibility**

Customer is responsible for losses arising from:
- Customer's failure to provide lawful processing instructions
- Customer's failure to ensure lawful basis for collection
- Customer's configuration of TAI systems (e.g., access controls)
- Customer's data subjects' actions

---

## 15. TERMINATION & SURVIVAL

**15.1 Term**

This DPA applies for the duration of the MSA.

**15.2 Effect of Termination**

Upon MSA termination:
- TAI's processing obligations end
- Data retention/deletion obligations continue for 90 days
- Confidentiality & indemnification obligations survive indefinitely

**15.3 Survival Clause**

The following sections survive termination:
- Section 5: Data Subject Rights (assist with pending requests)
- Section 7: Data Retention & Deletion
- Section 14: Limitation of Liability

---

## 16. AMENDMENTS & UPDATES

**16.1 DPA Updates**

This DPA may be amended if:
- Regulatory requirements change (e.g., GDPR guidance)
- TAI's security measures are enhanced
- Sub-processors are added/removed

Amendments require written agreement from both parties.

**16.2 GDPR Updates**

If GDPR regulations change, parties agree to:
- Review DPA for compliance
- Negotiate any necessary amendments
- Implement changes within 30 days

---

## 17. SIGNATURE

**THIS DPA IS BINDING UPON BOTH PARTIES WHEN SIGNED**

**FOR TAI ERLANG AUTONOMICS, INC.**:

Signature: ___________________________

Name (print): ___________________________

Title: ___________________________

Date: ___________________________


**FOR CUSTOMER**:

Signature: ___________________________

Name (print): ___________________________

Title: ___________________________

Date: ___________________________

---

## ATTACHMENT A: DATA PROCESSING DETAILS

**To be completed at signature:**

| Detail | Value |
|--------|-------|
| **Subject matter of processing** | [Customer specifies] |
| **Duration of processing** | [Contract duration + termination period] |
| **Nature and purpose** | Autonomic SKU management & entitlement enforcement |
| **Type of personal data** | User identifiers, emails, entitlement assignments |
| **Categories of data subjects** | Customer's employees, customers, contractors |
| **Personal data locations** | [GCP region - default: us-central1] |
| **Data retention period** | Contract term + 90 days |
| **Deletion trigger** | MSA termination or Customer request |
| **Customer contact** | [Email/phone for data requests] |
| **TAI DPO** | [Name, email] |

---

## ATTACHMENT B: STANDARD CONTRACTUAL CLAUSES (SCCs)

**[To be attached separately - SCCs are 20+ pages, standard boilerplate]**

This DPA incorporates the Standard Contractual Clauses (Module 2: Controller to Processor) as approved by the European Commission for transfer of personal data outside the EEA.

---

**Document Version**: 1.0
**Status**: READY TO CUSTOMIZE
**Last Updated**: 2026-01-26

### How to Use This Template:

1. **For Non-GDPR Customers**: Don't include this DPA in contract (US-only companies don't need it)
2. **For EU Customers**: Customize with Customer's specifics:
   - [ ] Data location (default: US or EU option?)
   - [ ] Data retention schedule (align with Customer requirements)
   - [ ] Authorized processing activities (list specific services)
   - [ ] Sub-processor list (update if using different vendors)
3. **Legal Review**: Have your attorney review before sending to customer
4. **Include in MSA**: DPA is "Exhibit B" to MSA (mandatory for GDPR compliance)

