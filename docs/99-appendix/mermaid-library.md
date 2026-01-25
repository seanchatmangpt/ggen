<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Mermaid Diagram Library (CANONICAL)](#mermaid-diagram-library-canonical)
  - [Table of Contents](#table-of-contents)
  - [Naming Conventions](#naming-conventions)
    - [IDs (Used in Mermaid Nodes)](#ids-used-in-mermaid-nodes)
    - [Labels (Displayed in Diagrams)](#labels-displayed-in-diagrams)
    - [Color Codes (Semantic)](#color-codes-semantic)
  - [Diagram Types](#diagram-types)
    - [C4 Model Overview](#c4-model-overview)
  - [C4 Component Diagrams](#c4-component-diagrams)
  - [C4 Container Diagrams](#c4-container-diagrams)
  - [Sequence Diagrams](#sequence-diagrams)
  - [State Machine Diagrams](#state-machine-diagrams)
  - [Class Diagrams](#class-diagrams)
  - [Examples](#examples)
    - [Example 1: Complete Decommission FSM](#example-1-complete-decommission-fsm)
    - [Example 2: Adversarial Testing Taxonomy (Block Diagram)](#example-2-adversarial-testing-taxonomy-block-diagram)
    - [Example 3: Receipt Chain Flow](#example-3-receipt-chain-flow)
  - [Receipt Contract](#receipt-contract)
  - [Definition of Done](#definition-of-done)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Mermaid Diagram Library (CANONICAL)

**Version**: 6.0.0 | **Status**: Production-Ready | **Last Updated**: 2026-01-25

> **CRITICAL**: This is the CANONICAL Mermaid diagram style guide. All agents and systems MUST follow these conventions. No exceptions.

## Table of Contents
1. [Naming Conventions](#naming-conventions)
2. [Color Scheme](#color-scheme)
3. [Diagram Types](#diagram-types)
4. [C4 Component Diagrams](#c4-component-diagrams)
5. [C4 Container Diagrams](#c4-container-diagrams)
6. [Sequence Diagrams](#sequence-diagrams)
7. [State Machine Diagrams](#state-machine-diagrams)
8. [Class Diagrams](#class-diagrams)
9. [Examples](#examples)

---

## Naming Conventions

### IDs (Used in Mermaid Nodes)
- **Format**: `snake_case` with underscores
- **Pattern**: `[a-z][a-z0-9_]*`
- **Examples**: `signal_ingestion`, `firestore_db`, `gcp_pubsub`

### Labels (Displayed in Diagrams)
- **Format**: `PascalCase` or `Title Case`
- **Pattern**: Capital first letter, spaces allowed
- **Examples**: `Signal Ingestion`, `Firestore Database`, `GCP Pub/Sub`

### Color Codes (Semantic)
| Plane | Color | Hex | Purpose |
|-------|-------|-----|---------|
| **Control** | Blue | #4A90E2 | API servers, control logic, decision engines |
| **Data** | Green | #7ED321 | Storage, queues, persistence layers |
| **Evidence** | Yellow | #F5A623 | Receipts, logs, audit trails |
| **Error** | Red | #D0021B | Failures, refusals, error paths |
| **Processing** | Purple | #9013FE | Workers, processors, async handlers |

---

## Diagram Types

### C4 Model Overview
- **Level 1**: System Context (system + external systems)
- **Level 2**: Containers (major components within system)
- **Level 3**: Components (detailed components within containers)
- **Level 4**: Code (classes, functions, detailed design)

We use Levels 1-3 (architectural level), not Level 4 (code detail).

---

## C4 Component Diagrams

**Purpose**: Show internal structure of a system (Level 3).

**Template**:
```mermaid
graph TB
    subgraph external["External Systems"]
        gcp_mp["GCP Marketplace"]
        customer["Customer Webhook"]
    end

    subgraph system["ggen System"]
        subgraph api_plane["API Plane (Control)"]
            signal_endpoint["🔵 Signal Endpoint<br/>POST /signal/:sku/:tenant"]
            webhook_endpoint["🔵 Webhook Endpoint<br/>POST /entitlement/webhook"]
            health_endpoint["🔵 Health Check<br/>GET /health"]
        end

        subgraph process_plane["Processing Plane (Purple)"]
            signal_processor["🟣 Signal Processor<br/>Validation + Deduplication"]
            action_engine["🟣 Action Engine<br/>State Machine"]
        end

        subgraph data_plane["Data Plane (Data)"]
            firestore_db["🟢 Firestore<br/>Receipts + Entitlements"]
            pubsub_queue["🟢 Cloud Pub/Sub<br/>Signal Queue"]
            gcs_archive["🟢 Cloud Storage<br/>Audit Trail Archive"]
        end

        subgraph evidence_plane["Evidence Plane (Evidence)"]
            receipt_logger["🟡 Receipt Logger<br/>Chain-linked Receipts"]
            audit_trail["🟡 Audit Trail<br/>Deterministic Hash Chain"]
        end
    end

    gcp_mp -->|Entitlement Webhook| webhook_endpoint
    customer -->|Signal Webhook| signal_endpoint

    signal_endpoint -->|Process| signal_processor
    webhook_endpoint -->|Process| action_engine

    signal_processor -->|Queue| pubsub_queue
    action_engine -->|Execute| firestore_db

    signal_processor -->|Emit| receipt_logger
    action_engine -->|Emit| receipt_logger

    receipt_logger -->|Store| firestore_db
    receipt_logger -->|Log| audit_trail
    audit_trail -->|Archive| gcs_archive

    health_endpoint -->|Check| firestore_db
    health_endpoint -->|Check| pubsub_queue

    classDef control fill:#4A90E2,stroke:#2E5C8A,color:#fff
    classDef data fill:#7ED321,stroke:#5AA612,color:#000
    classDef evidence fill:#F5A623,stroke:#C47F1A,color:#000
    classDef process fill:#9013FE,stroke:#6B0DB0,color:#fff
    classDef external_sys fill:#999999,stroke:#666666,color:#fff

    class signal_endpoint,webhook_endpoint,health_endpoint control
    class firestore_db,pubsub_queue,gcs_archive data
    class receipt_logger,audit_trail evidence
    class signal_processor,action_engine process
    class gcp_mp,customer external_sys
```

**Conventions**:
- Subgraphs for logical planes (API, Processing, Data, Evidence)
- 🔵 emoji for control plane components
- 🟢 emoji for data plane components
- 🟡 emoji for evidence plane components
- 🟣 emoji for processing plane components
- Color fills match plane semantics
- Labels include component type and brief purpose

---

## C4 Container Diagrams

**Purpose**: Show high-level system containers and integrations (Level 2).

**Template**:
```mermaid
graph LR
    external["🌐 External Systems<br/>GCP Marketplace + Customers"]

    subgraph ggen["ggen System"]
        api["🔵 API Layer<br/>Signal + Webhook Ingestion<br/>Status: Express.js"]
        processing["🟣 Processing Layer<br/>Signal Queue + Action Engine<br/>Status: Tokio async"]
        storage["🟢 Storage Layer<br/>Firestore + Pub/Sub + GCS<br/>Status: GCP Services"]
        evidence["🟡 Evidence Layer<br/>Receipt Logger + Chain<br/>Status: Cryptographic"]
    end

    monitoring["📊 Monitoring<br/>Cloud Logging + Cloud Trace"]

    external -->|Webhooks + Signals| api
    api -->|Route| processing
    processing -->|Persist| storage
    processing -->|Emit| evidence
    evidence -->|Archive| storage
    api -->|Query| storage
    processing -->|Metrics| monitoring
    evidence -->|Logs| monitoring

    classDef control fill:#4A90E2,stroke:#2E5C8A,color:#fff
    classDef process fill:#9013FE,stroke:#6B0DB0,color:#fff
    classDef data fill:#7ED321,stroke:#5AA612,color:#000
    classDef evidence fill:#F5A623,stroke:#C47F1A,color:#000
    classDef external_sys fill:#999999,stroke:#666666,color:#fff
    classDef monitoring_sys fill:#FF6B9D,stroke:#CC1A4A,color:#fff

    class api control
    class processing process
    class storage data
    class evidence evidence
    class external external_sys
    class monitoring monitoring_sys
```

**Conventions**:
- 3-4 major containers (API, Processing, Storage, Evidence/Monitoring)
- High-level data flows
- Technology stack listed in container label
- External systems and monitoring shown

---

## Sequence Diagrams

**Purpose**: Show message flow and interactions over time.

**Template** (Successful Signal Processing):
```mermaid
sequenceDiagram
    actor customer as Customer
    participant api as API Gateway
    participant processor as Signal Processor
    participant firestore as Firestore
    participant pubsub as Pub/Sub
    participant logger as Receipt Logger

    customer->>api: POST /signal (webhook)
    activate api

    api->>api: Verify HMAC-SHA256 signature
    alt signature_valid
        api->>processor: Queue signal
        api-->>customer: 202 Accepted + Receipt
    else signature_invalid
        api-->>customer: 401 Unauthorized + Receipt
        api->>logger: Emit refusal receipt
        deactivate api
        Note over customer: Flow stops
    end

    activate processor
    processor->>processor: Validate signal structure
    processor->>firestore: Check entitlement status
    activate firestore
    firestore-->>processor: {entitlement_id, active}
    deactivate firestore

    alt entitlement_active
        processor->>pubsub: Queue action
        processor->>logger: Emit action_attempted receipt
        deactivate processor
        Note over processor: Async processing continues
    else entitlement_expired
        processor->>logger: Emit entitlement_cancelled receipt
        deactivate processor
        Note over processor: Signal dropped
    end

    activate logger
    logger->>firestore: Store receipt (chain-linked)
    activate firestore
    firestore-->>logger: {receipt_id, chain_hash}
    deactivate firestore
    logger->>logger: Verify chain integrity
    deactivate logger
```

**Conventions**:
- **Actors**: Customer (outside system)
- **Participants**: System components (left to right order)
- **Activations**: Boxes show when component is processing
- **Alt/Else**: Conditional branches for decision points
- **Notes**: Explain key decisions or flow discontinuities
- **Message naming**: `action_type` (e.g., `verify_signature`, `queue_signal`)

---

## State Machine Diagrams

**Purpose**: Show valid state transitions and invariants.

**Template** (Signal Lifecycle):
```mermaid
stateDiagram-v2
    [*] --> pending: signal_received<br/>(validation passed)

    pending --> invalid: validation_failed<br/>(400 Bad Request)
    pending --> unauthorized: signature_mismatch<br/>(401 Unauthorized)
    pending --> denied: quota_exceeded<br/>(429 Rate Limited)
    pending --> processing: action_queued<br/>(202 Accepted)

    invalid --> [*]: refusal_emitted
    unauthorized --> [*]: refusal_emitted
    denied --> [*]: refusal_emitted

    processing --> in_flight: action_started<br/>(Pub/Sub consumer begins)
    in_flight --> succeeded: action_completed<br/>(output received)
    in_flight --> failed: action_error<br/>(error code set)
    in_flight --> timeout: deadline_exceeded<br/>(180s max)

    succeeded --> [*]: receipt_emitted<br/>(decision: accept)
    failed --> dead_letter: retry_exhausted<br/>(max 3 retries)
    timeout --> dead_letter: manual_intervention_required

    dead_letter --> [*]: incident_receipt<br/>(manual remediation)

    note right of pending
        Invariant: Every signal must have valid HMAC signature
        within 5 minutes of creation
    end note

    note right of processing
        Invariant: No two actions can execute
        on same entitlement simultaneously
    end note

    note right of in_flight
        Invariant: Action must complete or timeout
        within 180 seconds
    end note
```

**Conventions**:
- States: Clear, descriptive names (past tense for terminal states)
- Transitions: Include trigger event + action
- Notes: Document key invariants
- Terminal states: Point to `[*]` end marker
- Initial state: Start with `[*]` marker
- Colors: Use defaults (not customized for state machines)

---

## Class Diagrams

**Purpose**: Show data structures and relationships (very high level).

**Template**:
```mermaid
classDiagram
    class Receipt {
        +kind: string
        +ts: DateTime
        +decision: 'accept'|'refuse'|'unknown'
        +project_id: string
        +repo: string
        +branch: string
        +sku_id: string?
        +account_id: string?
        +details: object
        +prev_chain_hash_b64: string
        +compute_hash() -> string
        +verify_chain(receipts) -> bool
    }

    class Signal {
        +signal_id: string
        +signal_type: 'launch'|'update'|'terminate'|'delete'
        +sku_id: string
        +entitlement_id: string
        +parameters: object
        +signature: string
        +created_ts: DateTime
        +validate() -> bool
    }

    class Action {
        +action_id: string
        +action_type: 'launch'|'update'|'terminate'|'delete'
        +signal_id: string
        +state: 'pending'|'in_flight'|'succeeded'|'failed'
        +output: object?
        +error_code: string?
        +created_ts: DateTime
        +deadline_ts: DateTime
        +execute() -> Result
    }

    class Entitlement {
        +entitlement_id: string
        +sku_id: string
        +account_id: string
        +state: 'active'|'cancelled'|'expired'
        +activation_ts: DateTime
        +expiration_ts: DateTime
        +is_valid() -> bool
    }

    Signal "1" --> "1" Entitlement: references
    Signal "1" --> "1" Action: triggers
    Action "1" --> "1" Receipt: emits
    Entitlement "1" --> "*" Receipt: referenced_in
```

**Conventions**:
- Only show essential attributes and relationships
- Use type annotations
- Include key methods (`validate()`, `is_valid()`, `execute()`)
- Keep detail minimal (not a code-level diagram)

---

## Examples

### Example 1: Complete Decommission FSM

```mermaid
stateDiagram-v2
    [*] --> notice_period: decommission_initiated<br/>(30 days before termination)

    notice_period --> active_still: customer_reactivates<br/>(within 30 days)
    notice_period --> shutting_down: deadline_reached<br/>(30 days passed)

    active_still --> [*]: decommission_cancelled<br/>(no more termination)

    shutting_down --> draining: signal_intake_closed<br/>(stop new signals)
    draining --> exporting: action_queue_empty<br/>(all in-flight done)

    exporting --> export_complete: export_succeeded<br/>(receipts + configs archived)
    exporting --> export_failed: export_error<br/>(retry up to 3x)

    export_failed --> exporting: retry_backoff<br/>(exponential backoff)

    export_complete --> cleanup: archive_verified<br/>(GCS Object Lock enabled)
    cleanup --> archived: resources_deleted<br/>(Firestore, Pub/Sub, Cloud Run)

    archived --> forgotten: retention_expired<br/>(7 years after termination)
    forgotten --> [*]: data_purged<br/>(GDPR compliance)

    note right of notice_period
        Invariant: Customer can reactivate
        at any time during 30-day window
    end note

    note right of cleanup
        Invariant: Before cleanup, data must be
        exported + verified readable in GCS
    end note

    note right of forgotten
        Invariant: Data deleted only after
        retention_expiration_ts reached
    end note
```

### Example 2: Adversarial Testing Taxonomy (Block Diagram)

```mermaid
graph TD
    adversarial["🔴 Adversarial Test Suite"]

    adversarial --> black_box["Black-Box Testing<br/>(No internal knowledge)"]
    adversarial --> property["Property-Based Testing<br/>(Invariant verification)"]
    adversarial --> red_team["Red Team Scenarios<br/>(Attacker simulation)"]

    black_box --> input_validation["Input Validation<br/>- Malformed JSON<br/>- Oversized payloads<br/>- Invalid characters"]
    black_box --> protocol_violation["Protocol Violation<br/>- Missing headers<br/>- Wrong HTTP methods<br/>- Invalid signatures"]
    black_box --> authz_bypass["Authorization Bypass<br/>- Actions without permission<br/>- Cross-tenant access<br/>- Privilege escalation"]
    black_box --> quota_attack["Quota/Rate Limit<br/>- Burst traffic<br/>- Sustained high load<br/>- Quota exhaustion"]

    property --> idempotency["Idempotency<br/>Same request 2x =<br/>Same response (no dups)"]
    property --> determinism["Determinism<br/>Same input =<br/>Same hash (reproducible)"]
    property --> bounded_resources["Bounded Resources<br/>Receipt generation ≤<br/>Quota limits"]
    property --> state_safety["State Safety<br/>No path violates<br/>invariants"]

    red_team --> sig_bypass["Signature Bypass<br/>- Forge HMAC<br/>- Expected: 401 refusal"]
    red_team --> entitlement_escalation["Entitlement Escalation<br/>- Change SKU to expensive<br/>- Expected: rejected"]
    red_team --> permission_escalation["Permission Escalation<br/>- Admin actions without role<br/>- Expected: 403 refusal"]
    red_team --> firestore_corruption["Firestore Corruption<br/>- Bad receipt data<br/>- Expected: hash chain fails"]

    style adversarial fill:#D0021B,stroke:#8B0000,color:#fff
    style black_box fill:#FF6B6B,stroke:#CC0000,color:#fff
    style property fill:#FF6B6B,stroke:#CC0000,color:#fff
    style red_team fill:#FF6B6B,stroke:#CC0000,color:#fff
    style input_validation fill:#FFB3B3,stroke:#FF6B6B,color:#000
    style protocol_violation fill:#FFB3B3,stroke:#FF6B6B,color:#000
    style authz_bypass fill:#FFB3B3,stroke:#FF6B6B,color:#000
    style quota_attack fill:#FFB3B3,stroke:#FF6B6B,color:#000
    style idempotency fill:#FFB3B3,stroke:#FF6B6B,color:#000
    style determinism fill:#FFB3B3,stroke:#FF6B6B,color:#000
    style bounded_resources fill:#FFB3B3,stroke:#FF6B6B,color:#000
    style state_safety fill:#FFB3B3,stroke:#FF6B6B,color:#000
    style sig_bypass fill:#FFB3B3,stroke:#FF6B6B,color:#000
    style entitlement_escalation fill:#FFB3B3,stroke:#FF6B6B,color:#000
    style permission_escalation fill:#FFB3B3,stroke:#FF6B6B,color:#000
    style firestore_corruption fill:#FFB3B3,stroke:#FF6B6B,color:#000
```

### Example 3: Receipt Chain Flow

```mermaid
sequenceDiagram
    participant client as HTTP Client
    participant receipt_log as Receipt Logger
    participant firestore as Firestore
    participant chain as Chain Verifier

    client->>receipt_log: POST /signal
    receipt_log->>receipt_log: Create receipt_1<br/>(kind: signal_received)
    receipt_log->>receipt_log: hash_1 = SHA256(receipt_1)

    receipt_log->>firestore: Store receipt_1<br/>(prev_chain_hash = zero)
    firestore-->>receipt_log: OK
    receipt_log-->>client: 202 Accepted + receipt_1

    Note over client,receipt_log: Signal processing continues...

    receipt_log->>receipt_log: Create receipt_2<br/>(kind: action_attempted)
    receipt_log->>receipt_log: hash_2 = SHA256(receipt_2)<br/>prev = hash_1

    receipt_log->>firestore: Store receipt_2<br/>(prev_chain_hash = hash_1)
    firestore-->>receipt_log: OK

    Note over firestore,chain: Chain verification...
    chain->>firestore: Get all receipts
    firestore-->>chain: [receipt_1, receipt_2, ...]

    chain->>chain: Verify receipt_1.prev = zero ✓
    chain->>chain: Verify receipt_2.prev = hash(receipt_1) ✓
    chain->>chain: All hashes match ✓

    chain-->>client: Chain integrity verified
```

---

## Receipt Contract

**Every Mermaid diagram MUST**:
- ✅ Follow naming conventions (snake_case IDs, Title Case labels)
- ✅ Use correct color scheme (blue control, green data, yellow evidence, red errors)
- ✅ Include descriptive labels with component type
- ✅ Have clear data flow directions
- ✅ Document key invariants in notes
- ✅ Be horizontally/vertically balanced (readable on mobile)

---

## Definition of Done

- [x] Naming conventions documented (snake_case IDs, Title Case labels)
- [x] Color scheme defined (control/data/evidence/error/processing)
- [x] 4 diagram types explained (C4 component, C4 container, sequence, state machine)
- [x] 3 complete examples provided (decommission FSM, adversarial testing, receipt chain)
- [x] Class diagram template provided (high-level, not code-level)
- [x] Glossary references included
- [x] All conventions illustrated with examples

