# Completed ggen-legacy v27.8.1 Transformation and Sunset

`ggen-legacy v27.8.1` is modeled here as the final preservation and migration release: every historical fence is explained, every capability receives a disposition, and the repository becomes a replayable research corpus.

```mermaid
flowchart TB
  SRC[ggen-legacy complete Git history] --> FREEZE[Immutable preservation baseline]
  FREEZE --> OBS[Repository and history observers]
  OBS --> FENCE[Chesterton fence inventory]
  FENCE --> WHY[Original purpose reconstruction]
  WHY --> MAP[Legacy capability graph]

  MAP --> DISP{Disposition decision}
  DISP --> PRES[Preserve as canonical history]
  DISP --> EXTRACT[Extract into new authority]
  DISP --> MIGRATE[Migrate with equivalence proof]
  DISP --> MERGE[Merge duplicate authority]
  DISP --> REPLACE[Replace with proved successor]
  DISP --> RELEASE[Release active obligation]
  DISP --> UNKNOWN[UNKNOWN blocks sunset]

  PRES & EXTRACT & MIGRATE & MERGE & REPLACE & RELEASE --> EQ[Capability-equivalence matrix]
  EQ --> COMMAND[Command and diagnostic parity]
  COMMAND --> DATA[Data, schema, and receipt compatibility]
  DATA --> REPLAY[Historical replay and recovery drill]
  REPLAY --> LOSS{Zero information loss?}
  LOSS -->|yes| SUNSET[Sunset admitted]
  LOSS -->|no| BLOCK[LEGACY_INFORMATION_LOSS refusal]
  UNKNOWN --> BLOCK
```

## Legacy-to-manufacturing lineage

```mermaid
flowchart LR
  subgraph LegacyResearch[ggen-legacy v27.8.1 preserved discoveries]
    LG[Graph generation]
    LT[Templates]
    LC[CLI experiments]
    LP[Packs and marketplace]
    LV[Verification experiments]
    LE[Evidence and receipts]
    LPM[Process mining]
    LI[Infrastructure experiments]
  end

  subgraph Successor[ggen v26.8.1 manufactured authorities]
    UO[Unified ontology]
    PP[Planner portfolio]
    MK[Manufacturing kernel]
    GC[Generated CNV]
    PB[Pack and bblock kernel]
    VS[Distinct verifier suites]
    ER[Evidence and replay plane]
    EA[Enterprise architecture as software]
  end

  LG --> UO
  LT --> MK
  LC --> GC
  LP --> PB
  LV --> VS
  LE --> ER
  LPM --> ER
  LI --> EA
  UO --> PP --> MK
```
