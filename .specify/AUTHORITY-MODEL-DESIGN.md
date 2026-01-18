# Authority Model Schema: Gap 2 Design Document

## Executive Summary

The Authority Model RDF schema encodes policy governance for legal defensibility under SOX, HIPAA, GDPR, and similar compliance frameworks. The design is built on **immutable event sourcing** and **cryptographic accountability** — not mutation.

**Key Equation**: `Legal Defensibility = Non-Repudiation + Immutability + Auditability + Traceability`

---

## Core Design Principle: Immutability as Law

Unlike traditional databases where rows are updated, the Authority Model treats every fact as immutable:

- **Policies** are never modified; instead, new `PolicyVersion` instances are created.
- **Audit events** are never deleted; they form an append-only ledger with chain pointers.
- **Signatures** are never revoked inline; instead, a new event marks them "REVOKED" with timestamp.

**Why?** SOX, HIPAA, and GDPR all require **proof that changes happened and who made them**. If you delete a record, you lose proof. Instead:

1. Create the new state.
2. Record the change in an `AuditEvent`.
3. Link events chronologically (previousEvent chain).
4. Sign everything with timestamps.

---

## Five Pillars: What Makes a Policy Legally Defensible?

### 1. **Non-Repudiation: The WHO, WHAT, WHEN, HOW**

A `PolicySignature` captures:

```turtle
auth:signature-123 a auth:PolicySignature ;
    auth:signedBy auth:alice ;              # WHO (non-repudiation: Alice signed)
    auth:signedVersion auth:pol-v2 ;        # WHAT (exact version she approved)
    auth:signatureTimestamp "2024-01-15T14:32:00Z"^^xsd:dateTime ;  # WHEN
    auth:signatureValue "AABBCC...=="@base64 ;  # HOW (cryptographic proof)
    auth:signatureAlgorithm "RSA-SHA256" ;
    auth:certificateReference <https://pki.company.com/certs/alice.pem> ;  # Verify trust
    auth:signingAuthority auth:auth-cfo ;   # Authority level that permitted signature
    auth:signatureStatus "VALID" .          # Lifecycle (can be revoked)
```

**Why this design?**

- **Hash binding**: Signature includes `contentHash` (not stored in signature, but validated during verification). If policy text changes, the hash no longer matches → fraud detected.
- **Timestamp**: Proves *when* approval happened, not just *that* it happened.
- **Algorithm specification**: Auditor can verify signature used acceptable cryptography.
- **Certificate reference**: Links to external PKI infrastructure (company's Vault, AWS Secrets Manager, etc.). Signature is worthless without verifiable public key.
- **Authority metadata**: Proves the signer had legitimate authority to approve policies in the relevant domain.

---

### 2. **Immutability: The Append-Only Ledger**

Each `PolicyVersion` is immutable:

```turtle
auth:policy-payment-v2 a auth:PolicyVersion ;
    auth:versionNumber "2.0.0" ;
    auth:policyContent "Only CFO can approve payments >$100K" ;
    auth:contentHash "sha256base64:..." ;  # Hash, never changes
    auth:createdAt "2024-01-15T10:00:00Z"^^xsd:dateTime ;  # Immutable timestamp
    auth:createdBy auth:alice ;            # Immutable authorship
    auth:effectiveFrom "2024-01-16T00:00:00Z"^^xsd:dateTime ;
    auth:hasSignature auth:signature-123, auth:signature-456 .  # Multiple signers
```

**Key properties:**

- **versionNumber**: Semantic versioning (1.0.0, 2.3.1). Enables tracking.
- **contentHash**: SHA-256 of policy content. Auditors verify integrity offline.
- **createdAt vs. signatureTimestamp**: Creation time (when policy drafted) ≠ approval time. Both matter.
- **effectiveFrom / effectiveUntil**: Policy lifecycle. Enables temporal queries ("what policy was active on 2024-01-15?").

**SHACL constraint ensures temporal consistency:**

```
effectiveUntil >= effectiveFrom (if both present)
createdAt <= effectiveFrom (policy drafted before it takes effect)
signatureTimestamp >= createdAt (approval after creation)
```

---

### 3. **Auditability: The Immutable Event Log**

Every action creates an `AuditEvent`:

```turtle
auth:event-uuid-001 a auth:AuditEvent ;
    auth:eventId "a1b2c3d4-..." ;         # UUID (deduplication)
    auth:eventTimestamp "2024-01-15T14:32:05Z"^^xsd:dateTime ;
    auth:eventActor auth:alice ;           # WHO acted
    auth:eventType "POLICY_MODIFIED" ;     # WHAT action
    auth:targetResource <http://.../policy/payment> ;  # Resource affected
    auth:targetResourceBefore "version 1.0.0" ;  # Before state (for diffing)
    auth:targetResourceAfter "version 2.0.0" ;   # After state
    auth:eventReason "GDPR Article 6 requirement" ;  # WHY (business driver)
    auth:eventStatus "SUCCESS" ;           # RESULT (action completed)
    auth:previousEvent auth:event-uuid-000 ;  # Chain pointer (immutable ledger)
    auth:sourceIpAddress "192.168.1.100" ;
    auth:userAgent "ggen-cli/1.0" .
```

**Why each field?**

- **eventId**: UUID prevents duplicate audit entries in distributed systems.
- **eventTimestamp**: When action occurred (must be before/after policy times for sanity).
- **eventActor**: WHO did it (for forensics and accountability).
- **eventType**: Standard set (POLICY_CREATED, POLICY_MODIFIED, SIGNATURE_ADDED, EXECUTION_DENIED, etc.) enables automated queries.
- **targetResourceBefore/After**: Enables "show me what changed" without storing full diffs.
- **eventReason**: Links action to business driver (regulatory requirement, ticket, etc.).
- **eventStatus**: SUCCESS | DENIED | FAILED (enables "show me all failed approval attempts").
- **previousEvent**: Immutable chain. If Event B points to Event A, and Event A points to Event C, the ledger is Event C → A → B. Auditors can verify chain integrity.
- **sourceIpAddress**: For forensics (detect suspicious locations).
- **userAgent**: Which tool/UI was used (ggen-cli vs. web UI vs. API).

**Canonical Audit Query Example (SPARQL):**

```sparql
# "Who changed what policy when and why?"
SELECT ?actor ?policy ?versionBefore ?versionAfter ?timestamp ?reason WHERE {
    ?event a auth:AuditEvent ;
        auth:eventType "POLICY_MODIFIED" ;
        auth:eventActor ?actor ;
        auth:targetResource ?policy ;
        auth:targetResourceBefore ?versionBefore ;
        auth:targetResourceAfter ?versionAfter ;
        auth:eventTimestamp ?timestamp ;
        auth:eventReason ?reason .
    FILTER (?timestamp >= "2024-01-01T00:00:00Z"^^xsd:dateTime &&
            ?timestamp <= "2024-01-31T23:59:59Z"^^xsd:dateTime)
}
ORDER BY ?timestamp
```

---

### 4. **Authority Hierarchy: Scoped Governance**

An `AuthorityLevel` defines WHO can do WHAT in WHICH domain:

```turtle
auth:role-cfo a auth:AuthorityLevel ;
    auth:authorityId "AUTH-CFO" ;
    auth:authorityName "Chief Financial Officer" ;
    auth:canApproveDomains auth:domain-payments, auth:domain-contracts ;  # Scope
    auth:canExecuteInDomains auth:domain-payments ;  # CFO approves but doesn't execute
    auth:requiresMultipleSignatures true ;
    auth:requiredSignatureCount 2 ;  # CFO + Board Chair
    auth:approvalRiskLevel "CRITICAL" ;  # Can approve CRITICAL/HIGH/MEDIUM/LOW policies
    auth:canDelegate true ;              # Can temporarily delegate authority
    auth:authorityValidFrom "2024-01-01T00:00:00Z"^^xsd:dateTime ;
    auth:authorityValidUntil "2024-12-31T23:59:59Z"^^xsd:dateTime .  # Expires end of year
```

**SHACL validates:**

- If `requiresMultipleSignatures=true`, then `requiredSignatureCount >= 2`.
- Authority is only valid if current time is between `validFrom` and `validUntil`.
- Actor cannot approve policies in domains not in `canApproveDomains`.

**Multi-Approver Workflows:**

For CRITICAL policies:

```turtle
auth:policy-critical-v1 a auth:PolicyVersion ;
    auth:appliesTo auth:domain-payments ;
    auth:hasSignature auth:sig-cfo, auth:sig-board-chair ;
    # Both signatures required, in any order (unless order specified in workflow)
```

Each signature records the approval round:

```turtle
auth:sig-cfo a auth:PolicySignature ;
    auth:signedBy auth:alice ;
    auth:multiApprovalRound 1 ;
    auth:signingAuthority auth:role-cfo .

auth:sig-board-chair a auth:PolicySignature ;
    auth:signedBy auth:bob ;
    auth:multiApprovalRound 2 ;
    auth:signingAuthority auth:role-board-chair .
```

---

### 5. **Traceability: Why Did This Happen?**

Every policy change linked to business driver:

```turtle
auth:policy-payment-v2 a auth:PolicyVersion ;
    auth:rationale "GDPR Article 6 requires explicit consent before processing payments" ;
    auth:linkedChangeRequest <https://jira.company.com/browse/GDPR-1523> .
```

**Why?**

1. **Compliance audits**: "Prove that this policy change was driven by a real regulatory requirement." Link to JIRA ticket.
2. **Change management**: "Who requested this? What's the impact?" Link visible in ticket.
3. **Reversion planning**: "Why did we make this change? What would revert it?" Captured in rationale.

---

## Domain-Scoped Policies

`PolicyDomain` enables governance per business area:

```turtle
auth:domain-payments a auth:PolicyDomain ;
    auth:domainId "payment-processing" ;
    auth:domainName "Payment Processing" ;
    auth:riskLevel "CRITICAL" ;  # High risk → requires more signatures
    auth:complianceFrameworks "PCI-DSS", "SOX", "GDPR" .

auth:domain-gdpr-export a auth:PolicyDomain ;
    auth:domainId "gdpr-data-export" ;
    auth:domainName "GDPR Subject Access Requests" ;
    auth:riskLevel "HIGH" ;
    auth:complianceFrameworks "GDPR" ;
    auth:owningTeam "Data Privacy Office" .
```

**Why domain-scoped?**

- Different teams own different policies (Finance owns payment policies, Data Privacy owns GDPR policies).
- Different compliance frameworks apply (PCI-DSS for payments, GDPR for data).
- Different risk levels (CRITICAL vs. LOW).
- Authority levels are scoped to domains (`canApproveDomains`, `canExecuteInDomains`).

---

## Temporal Reasoning: The Effective Timeline

Policies have a lifecycle:

```turtle
auth:policy-payment-v2 a auth:PolicyVersion ;
    auth:createdAt "2024-01-10T10:00:00Z"^^xsd:dateTime ;      # Drafted on Jan 10
    auth:effectiveFrom "2024-01-15T00:00:00Z"^^xsd:dateTime ;  # Active from Jan 15
    auth:effectiveUntil "2024-12-31T23:59:59Z"^^xsd:dateTime . # Expires end of year
```

**SHACL ensures:** `createdAt <= effectiveFrom <= effectiveUntil`

**Audit queries enabled:**

```sparql
# "What policy was active on 2024-06-15?"
SELECT ?policy WHERE {
    ?policy a auth:PolicyVersion ;
        auth:effectiveFrom ?from ;
        auth:effectiveUntil ?until .
    FILTER ("2024-06-15T00:00:00Z"^^xsd:dateTime >= ?from &&
            "2024-06-15T00:00:00Z"^^xsd:dateTime < ?until)
}
```

---

## Cryptographic Trust Model

**External PKI (required):**

Signatures reference external certificates:

```turtle
auth:sig-alice a auth:PolicySignature ;
    auth:signatureAlgorithm "RSA-SHA256" ;
    auth:signatureValue "AABBCC...==" ;
    auth:certificateReference <https://pki.company.com/alice.pem> ;
    # Verification: Fetch alice.pem, verify signature against contentHash
```

**Why external PKI?**

- RDF can't store private keys (security risk).
- Certificates can be revoked independently (CA revokes, audit event records it).
- Multiple certificate providers supported (corporate PKI, AWS KMS, Azure Key Vault, Vault).

**Status Lifecycle:**

```
PENDING_VERIFICATION  → (validate signature)
         ↓
      VALID  → (CA revokes cert)
         ↓
      REVOKED (immutable, recorded with AuditEvent)
```

---

## Actor Identity Model

```turtle
auth:alice a auth:Actor ;
    auth:actorId "alice@company.com" ;  # LDAP/OAuth subject
    auth:actorType "HUMAN" ;
    auth:actorName "Alice Johnson" ;
    auth:actorEmail "alice@company.com" ;
    auth:hasAuthority auth:role-cfo ;
    auth:isActive true ;  # Soft deactivation (no deletion)
    auth:publicKeyFingerprint "ABC123DEF456..." . # SHA-256 of public key
```

**Service accounts:**

```turtle
auth:ggen-sync a auth:Actor ;
    auth:actorId "ggen-sync-service" ;
    auth:actorType "SERVICE" ;
    auth:actorName "ggen Sync Service" ;
    auth:hasAuthority auth:role-automation ;
    auth:isActive true ;
    auth:publicKeyFingerprint "XYZ789..." .
```

**Why separate identity and authority?**

- Actors can have multiple roles (Alice is both CFO and Board member).
- Roles can be granted/revoked without deleting the actor.
- Soft deactivation: set `isActive=false` instead of deleting (preserves audit trail).

---

## SHACL Validation Strategy

**Validation happens at write time:**

1. **Datatype constraints**: `policyContent` must be `xsd:string`, timestamps must be `xsd:dateTime`, signatures must be base64.
2. **Required fields**: All 5 pillars require core metadata (WHO, WHAT, WHEN, HOW, WHY).
3. **Referential integrity**: `Policy.currentVersion` must exist in `Policy.hasVersionHistory`.
4. **Temporal consistency**: `createdAt <= effectiveFrom`, `effectiveUntil >= effectiveFrom`, etc.
5. **Cardinality**: `signatureStatus` can only be "VALID", "REVOKED", "EXPIRED", "PENDING_VERIFICATION".

**Example constraint:**

```shacl
sh:sparql [
    a sh:SPARQLConstraint ;
    sh:message "If requiresMultipleSignatures=true, requiredSignatureCount must be >= 2" ;
    sh:select """
        PREFIX auth: <http://ggen.io/authority/>
        SELECT $this WHERE {
            $this auth:requiresMultipleSignatures true .
            FILTER NOT EXISTS {
                $this auth:requiredSignatureCount ?count .
                FILTER (?count >= 2)
            }
        }
    """ ;
] .
```

---

## Files Delivered

1. **authority-model-schema.ttl**
   - Core ontology (Policy, PolicyVersion, PolicySignature, AuditEvent, AuthorityLevel, PolicyDomain, Actor)
   - 80+ properties with rdfs:comment explaining legal defensibility

2. **authority-model-shapes.ttl**
   - SHACL validation shapes
   - Temporal constraints
   - Referential integrity checks
   - Enum constraints (eventType, eventStatus, etc.)

3. **AUTHORITY-MODEL-DESIGN.md** (this file)
   - Design rationale
   - Audit query examples
   - Cryptographic trust model
   - Temporal reasoning

---

## Compliance Mapping

| Compliance Framework | Schema Component | Purpose |
|---|---|---|
| **SOX 404** | AuditEvent chain, PolicyVersion history | Prove internal controls (policies) were followed |
| **HIPAA 164.308** | AuthorityLevel + Signature + AuditEvent | Workforce security, access controls, accountability |
| **GDPR Article 5** | PolicyVersion rationale + AuditEvent + effectiveFrom | Lawfulness, fairness, transparency, accountability |
| **PCI-DSS 10.1** | AuditEvent ledger | User and administrator activity audit trail |
| **ISO 27001 A.12.4** | Signature + Certificate + eventStatus | Event logging and monitoring |

---

## Implementation Notes

### For the RDF Store (Oxigraph/similar)

- Use indexed queries on `eventTimestamp` (high cardinality).
- Create composite index on `(eventType, eventTimestamp)` for fast forensics.
- Archive AuditEvents to cold storage after N days, maintain hot index for last 90 days.

### For Signature Verification

- Fetch certificate from `certificateReference` (external PKI).
- Verify signature over `(contentHash || signatureTimestamp || signingKey)`.
- Check certificate validity (not revoked, not expired).
- Validate authority (signer's `hasAuthority` includes domain of policy).

### For Audit Queries

- Build UI filters on `eventType`, `eventStatus`, `eventActor`, `targetResource`, `eventTimestamp`.
- Export audit log to SIEM (Splunk, ELK) with `eventId` as unique key.
- Generate reports: "Policy changes by domain", "Signatures by actor", "Failed approval attempts", etc.

---

## Success Criteria (This Design Achieves)

✅ **Non-repudiation**: Cryptographic signatures with timestamps, linked to authorities
✅ **Immutability**: Append-only ledger, no deletions
✅ **Auditability**: Every action logged with WHO/WHAT/WHEN/WHY/HOW
✅ **Traceability**: Linked to business drivers (JIRA, RFCs)
✅ **Temporal reasoning**: Effective dates, expiration, version histories
✅ **Domain scoping**: Governance per business area
✅ **Multi-approver workflows**: Support complex approval chains
✅ **Authority delegation**: Support succession planning
✅ **SHACL validation**: Enforce constraints at write time

---

## References

- [W3C SHACL (Shapes Constraint Language)](https://www.w3.org/TR/shacl/)
- [ISO 27001:2022 Information security management](https://www.iso.org/standard/27001)
- [GDPR: Article 5 (Principles of personal data processing)](https://gdpr-info.eu/art-5-gdpr/)
- [SOX Section 404 (Internal control compliance)](https://en.wikipedia.org/wiki/Sarbanes%E2%80%93Oxley_Act)
- [Event Sourcing Pattern](https://martinfowler.com/eaaDev/EventSourcing.html)

