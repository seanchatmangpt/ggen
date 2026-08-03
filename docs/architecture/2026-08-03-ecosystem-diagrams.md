# Ecosystem Architecture Diagrams

**Scope:** documentation-only views of the target architecture described in `2026-08-03-ecosystem-architecture-reconstitution.md`.

## 1. Context and authority boundaries

```mermaid
flowchart TB
    O[Observed work and exact source identity] --> A[Admission: O to O*]
    A --> CG[Canonical ecosystem capability graph]
    CG --> P[Ferroplan construction and planning]
    P --> M[ggen deterministic manufacture]
    M --> I[BRCE intent]
    I --> G[BRCE grant]
    G --> DO[Consequential actuation]
    DO --> R[Receipt DAG]
    R --> PI[wasm4pm process intelligence and OCEL]
    PI --> V[ggen-legacy and Truthforge independent verification]
    V --> S[Scoped standing]

    D[Doctor] -->|findings only| A
    W[Wizard] -->|reversible plans only| P
    T[Telco] -->|typed envelopes| CG
    T --> P
    T --> R

    H[Hooks] -->|manufacture intents| I
    H -. no direct actuation .-> DO
    D -. no actuation authority .-> DO
    W -. no actuation authority .-> DO
    V -. no actuation authority .-> DO
```

## 2. Layered target architecture

```mermaid
flowchart TB
    L0[0. Constitutional authority<br/>A=μ(O*), standing, ownership, BRCE, claim ceilings]
    L1[1. Ecosystem capability graph<br/>canonical capabilities, ABB/SBB, realization and substitution]
    L2[2. Observation and admission<br/>exact source, RDF, SHACL, provenance, contradiction]
    L3[3. Construction and planning<br/>CMD, Ferroplan, linear, temporal, FOND, probabilistic]
    L4[4. Manufacture<br/>graph, query, template, ggen, deterministic projection]
    L5[5. Brokered runtime<br/>BRCE grants and consequential boundaries]
    L6[6. Evidence and process intelligence<br/>receipt DAG, OCEL, wasm4pm, replay, conformance]
    L7[7. Independent verification<br/>ggen-legacy, Truthforge, mutation and clean-room replay]
    L8[8. Product projections<br/>CLI, MCP, A2A, LSP, web, browser, television, AtomVM]

    L0 --> L1 --> L2 --> L3 --> L4 --> L5 --> L6 --> L7 --> L8
    L8 -. observed consequence and telemetry .-> L6
    L7 -. verdict and claim ceiling .-> L0
```

## 3. Capability passport model

```mermaid
classDiagram
    class CanonicalCapability {
      +capabilityId
      +label
      +claimCeiling
      +authorityRef
    }
    class ABBRequirement {
      +requirementId
      +acceptanceLaw
    }
    class RepositoryRealization {
      +repository
      +ref
      +exactSha
      +implementationPath
    }
    class CapabilityPassport {
      +subjectIdentity
      +toolchainIdentity
      +executionMode
      +standing
    }
    class Witness {
      +command
      +expectedConsequence
    }
    class Falsifier {
      +mutation
      +expectedRefusal
    }
    class ReceiptEdge {
      +authority
      +arguments
      +result
      +consequence
      +parentDigest
    }
    class ReplayReport {
      +replayIdentity
      +matchResult
    }

    CanonicalCapability "1" --> "1..*" ABBRequirement : satisfies
    CanonicalCapability "1" --> "1..*" RepositoryRealization : realizedBy
    RepositoryRealization "1" --> "1" CapabilityPassport : governedBy
    CapabilityPassport "1" --> "1..*" Witness : provesWith
    CapabilityPassport "1" --> "1..*" Falsifier : boundedBy
    Witness "1" --> "1..*" ReceiptEdge : emits
    ReceiptEdge "1..*" --> "1" ReplayReport : replayedBy
```

## 4. Doctor, Wizard, Telco, Truthforge protocol

```mermaid
sequenceDiagram
    participant Operator
    participant Doctor
    participant Wizard
    participant Telco
    participant BRCE
    participant Runtime
    participant Truthforge

    Operator->>Doctor: observe exact subject
    Doctor-->>Operator: bounded findings
    Operator->>Wizard: construct repair or realization request
    Wizard-->>Telco: WizardPlan envelope
    Telco->>BRCE: typed intent with authority and correlation
    BRCE->>BRCE: admit or typed refusal
    alt grant admitted
        BRCE->>Runtime: actuate bounded consequence
        Runtime-->>BRCE: result and observed consequence
        BRCE-->>Telco: receipt edge
        Telco->>Truthforge: evidence and replay request
        Truthforge-->>Operator: independent verdict and standing
    else refused
        BRCE-->>Operator: REFUSED_TYPE with evidence
    end
```

## 5. Receipt DAG

```mermaid
flowchart LR
    S[ExactSubject] --> E1[Receipt edge 1<br/>admission]
    AU[AuthorityAssertion] --> E1
    P[Resolved plan and arguments] --> E2[Receipt edge 2<br/>construction]
    E1 --> E2
    G[BRCE grant] --> E3[Receipt edge 3<br/>actuation]
    E2 --> E3
    C[Observed consequence] --> E3
    E3 --> E4[Receipt edge 4<br/>process evidence]
    O[OCEL events] --> E4
    E4 --> E5[Receipt edge 5<br/>independent replay]
    M[Mutation outcomes] --> E5
    E5 --> ST[Scoped standing]
```

## 6. Repository role map

```mermaid
flowchart LR
    GG[ggen<br/>semantic authority and manufacturing kernel]
    GL[ggen-legacy<br/>executable corpus and independent verifier]
    FP[Ferroplan<br/>planning, persistent mind, operator experience]
    W4[wasm4pm<br/>process evidence and resumable execution]
    WC[wasm4pm-compat<br/>compatibility court]
    BR[BRCE<br/>exclusive consequential DO path]
    DP[Domain products<br/>CNS, DTeam, ByteStar, KNHK, GitVan, MMDIO, others]

    GG -->|manufactures projections| DP
    FP -->|constructs plans| GG
    FP -->|manufactures intents| BR
    DP -->|requests bounded actuation| BR
    BR -->|results and consequences| W4
    W4 -->|receipt and process evidence| GL
    GG -->|authority bundle| GL
    GL -->|independent verdict| GG
    WC -->|compatibility disposition| GG
    WC -->|compatibility disposition| DP
```

## 7. Standing composition

```mermaid
stateDiagram-v2
    [*] --> UNKNOWN
    UNKNOWN --> PARTIAL_ALIVE: bounded checkpoint executed
    UNKNOWN --> BLOCKED: admitted dependency prevents execution
    UNKNOWN --> UNSUPPORTED: outside admitted capability boundary
    PARTIAL_ALIVE --> ALIVE: exact admitted subject executes claimed consequence
    PARTIAL_ALIVE --> BLOCKED: remaining crown dependency unavailable
    PARTIAL_ALIVE --> BUILD_BROKEN: verifier path cannot be reached
    BLOCKED --> PARTIAL_ALIVE: dependency restored and checkpoint executes
    BUILD_BROKEN --> PARTIAL_ALIVE: build path repaired and checkpoint executes
    ALIVE --> UNKNOWN: subject identity changes without replay
    ALIVE --> PARTIAL_ALIVE: broader claim exceeds evidence ceiling
```

## 8. First reference-product crown

```mermaid
flowchart LR
    C[CNS bounded request] --> F[Ferroplan construction]
    F --> G[ggen manufacture]
    G --> B[BRCE local consequence]
    B --> W[wasm4pm OCEL and conformance]
    W --> L[ggen-legacy independent replay]
    L --> X[mutation and refusal portfolio]
    X --> A[scoped ecosystem product ALIVE]
```

## 9. Toolchain capsule closure

```mermaid
flowchart TB
    SRC[Pinned source capsule]
    TC[Pinned Rust toolchain]
    DC[Dependency cache]
    TS[Target set]
    VP[Validation pack]
    EM[Execution mode]

    SRC --> CAP[Portable toolchain capsule]
    TC --> CAP
    DC --> CAP
    TS --> CAP
    VP --> CAP
    EM --> CAP

    CAP --> D[DTeam]
    CAP --> C[wasm4pm-compat]
    CAP --> K[KNHK]
    CAP --> M[MCPP]
    CAP --> N[NASA Rust core]
    CAP --> U[Unibit]
```

## 10. Generated-surface ownership

```mermaid
flowchart LR
    O[Canonical ontology] --> Q[Query]
    Q --> T[Template]
    T --> G[ggen manufacture]
    G --> P[Generated projection]
    P --> R[Runtime consumer]
    G --> RC[Projection receipt]
    G --> SM[Second manufacture]
    SM --> EQ{byte or semantic identity}
    EQ -->|match| AL[projection standing]
    EQ -->|mismatch| RF[REFUSED_GENERATED_DRIFT]
```
