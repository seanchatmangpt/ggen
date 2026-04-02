# Ontology-Native Enterprise Construction

## A Graduate Syllabus — 14 Weeks

**Course Number:** CS/SE 6XX
**Prerequisites:** Formal languages, databases, software engineering, enterprise architecture
**Core Text:** *Ontology-Native Enterprise Construction* (14 Parts, 54 Chapters)
**Lab Stack:** ggen v6.0.1 (Rust), Oxigraph, SPARQL, Tera templates
**Philosophy:** A = μ(O) — every artifact is a deterministic projection of an ontology graph

---

## Calendar Overview

| Week | Part | Topic | Lab |
|------|------|-------|-----|
| 1 | I | Foundations: Artifact, Ontology, Transformation | Load and query an RDF graph |
| 2 | I–II | Paradigms, Deployment, Governance | Design a deployment question set |
| 3 | III | Generative Analysis and Knowledge Capture | LLM-CONSTRUCT exercise |
| 4 | IV | Process Mining Foundations | Event log → Petri net discovery |
| 5 | IV–V | Object-Centric Mining, Workflow Theory | Alpha++ on real event data |
| 6 | V | YAWL and the Workflow Pattern Canon | Build a YAWL workflow in ggen |
| 7 | VI | RDF, SPARQL, CONSTRUCT as Compiler | Write a CONSTRUCT pipeline |
| 8 | VI | Full-Text Compilation and Template Systems | Build a μ₁-μ₅ compiler |
| 9 | VII | Capability Surfaces and Interface Systems | Generate a CLI from ontology |
| 10 | VIII | Packs, Profiles, and Registries | Build and publish a pack |
| 11 | IX | Receipts, Quality, and Proof | Ed25519 receipt chain |
| 12 | X | Time, Versioning, Temporal Navigation | Event-sourced knowledge graph |
| 13 | XI–XII | Enterprise Text, Multi-Agent Systems | Multi-agent coordination exercise |
| 14 | XIII–XIV | Practicum, Conclusion, Event Horizon | Final project presentations |

---

## Week 1 — Foundations: Artifact, Ontology, and Transformation

**Readings:** Ch 1 (Artifact, Ontology, and Transformation), Ch 2 (Construct and Select/Do)

**Lecture:**
- The artifact as transformed ontology — A = μ(O)
- Public ontology vs private model — why the graph is the source of truth
- Select/do paradigm (current enterprise tools) vs construct paradigm (ontology-native)
- Coordination ceilings — why pattern selection hits a ceiling that graph transformation does not

**Lab 1.1 — Load and Query an RDF Graph**
```bash
# Install ggen
cargo install ggen-cli

# Load a sample ontology
ggen load .specify/specs/demo-ontology.ttl

# Query it
ggen query "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
```

**Deliverable:** Written reflection (500 words): "Describe an artifact in your current work that is the result of a transformation. What is the source ontology? Where is the loss?"

**Key Concepts:** Artifact, ontology, transformation pipeline, loss localization, select/do vs construct

---

## Week 2 — Organizational Work as Formal Domain, Deployment, and Governance

**Readings:** Ch 3 (Organizational Work as a Formal Domain), Ch 4 (Information, Meaning, and Abstraction), Ch 5 (Deployment Architecture), Ch 6 (Leadership and Institutional Support)

**Lecture:**
- Why organizations are compiler targets — work as executable structure
- Levels of abstraction — from informal knowledge to formal graph
- Deployment question set — scope, sponsorship, operating model
- Governance before execution — why governance is a compile-time concern
- Sponsorship structures and escalation — leadership as runtime dependency

**Lab 2.1 — Design a Deployment Question Set**
- Given a fictional enterprise scenario (provided), design the full deployment question set
- Map organizational capabilities to ontology classes
- Identify governance constraints that must be encoded as type-level invariants

**Deliverable:** Deployment question set document with governance constraints mapped to OWL restrictions

**Key Concepts:** Compiler target, abstraction levels, governance-as-types, sponsorship, escalation

---

## Week 3 — Generative Analysis and Knowledge Capture

**Readings:** Ch 10 (Capturing the Preformal Domain), Ch 11 (Generative Analysis), Ch 12 (Communication Models), Ch 13 (Literate Modeling)

**Lecture:**
- Knowledge capture as observatory construction — from informal to formal
- Information typing, semantic highlighting, key-statement analysis
- Prompting as semantic boundary engineering — the LLM is a projection surface
- Literate modeling — prose-model alignment, narrative as executable semantic scaffolding
- LLM-CONSTRUCT methodology — closed-graph reconstruction

**Lab 3.1 — LLM-CONSTRUCT Exercise**
- Given an unstructured domain description (2000 words about a fictional supply chain)
- Use LLM-CONSTRUCT to extract: entities, relationships, constraints, capabilities
- Produce a Turtle ontology that represents the domain
- Validate: can you answer 10 questions about the domain purely via SPARQL?

**Deliverable:** Turtle file + SPARQL query set + reflection on what was lost in the extraction

**Key Concepts:** LLM-CONSTRUCT, semantic highlighting, literate modeling, prompting as boundary engineering

---

## Week 4 — Process Mining Foundations

**Readings:** Ch 14 (Process Mining Foundations)

**Lecture:**
- Event data and process models — the alphabet of organizational execution
- Discovery — Alpha algorithm, heuristics, inductive mining
- Conformance — checking reality against the model
- Performance analysis — bottlenecks, variants, deviations

**Lab 4.1 — Event Log to Petri Net**
```rust
// Using ggen-process-mining
use ggen_process_mining::{EventLog, ProcessMiner, AlphaPlusPlus};

// Parse an XES event log
let log = EventLog::from_xes_file("supply_chain.xes")?;

// Discover a Petri net
let miner = AlphaPlusPlus::new();
let petri_net = miner.discover(&log)?;

// Check soundness
assert!(petri_net.is_sound());
```

**Deliverable:** Discovered Petri net + soundness proof + conformance report against original log

**Key Concepts:** Event logs (XES), discovery, conformance checking, soundness, Alpha++

---

## Week 5 — Object-Centric Process Mining and Workflow Theory

**Readings:** Ch 15 (Object-Centric Process Mining), Ch 16 (Process Intelligence), Ch 17 (AI Grounded in Process), Ch 18 (Workflow Theory)

**Lecture:**
- Limits of case-centric mining — why one case ID is not enough
- Object-centric event data (OCEL) — multi-object reality
- PI as the grounding layer for enterprise AI — why there is no enterprise AI without PI
- Workflow semantics — soundness, deadlock, completion
- Formal execution models — tokens, enabled transitions, firing rules

**Lab 5.1 — Alpha++ on Real Event Data**
- Use `ggen-process-mining` to parse an OCEL log
- Run multi-object discovery
- Identify the top 3 process variants
- Generate conformance report

**Deliverable:** Multi-object process model + variant analysis + "What would AI need from this process model?" (500 words)

**Key Concepts:** OCEL, object-centric discovery, PI grounding, workflow soundness, deadlock

---

## Week 6 — YAWL and the Workflow Pattern Canon

**Readings:** Ch 19 (YAWL and the Workflow Pattern Canon)

**Lecture:**
- The 43 workflow patterns (van der Aalst) — the complete coordination catalog
- Why YAWL covers all 43 — soundness as a design principle
- YAWL as formal coordination IR — intermediate representation for organizational work
- Resource, data, and exception patterns
- Mapping workflow patterns to ontology constructs

**Lab 6.1 — Build a YAWL Workflow in ggen**
```bash
# Generate YAWL from ontology
ggen yawl generate --spec .specify/specs/order-process.ttl --output order-workflow.yaml
```

**Deliverable:** YAWL specification covering at least 10 of the 43 patterns + pattern coverage matrix

**Key Concepts:** 43 workflow patterns, YAWL, coordination IR, resource patterns, exception handling

---

## Week 7 — RDF, SPARQL, CONSTRUCT as Business Logic

**Readings:** Ch 22 (RDF, OWL, and SPARQL as Construction Substrate), Ch 23 (CONSTRUCT as Business Logic)

**Lecture:**
- RDF as universal representation — triples as the assembly language of meaning
- OWL for semantic constraints — classes, properties, restrictions
- SPARQL query forms — SELECT, ASK, DESCRIBE, CONSTRUCT
- CONSTRUCT as transformation primitive — graph rewriting without imperative code
- Business logic without branching — declarative transformation chains

**Lab 7.1 — Write a CONSTRUCT Pipeline**
- Given an input graph (TTL), write 3 CONSTRUCT queries that:
  1. Extract entity relationships
  2. Transform to a target schema
  3. Generate binding set for template rendering
- Chain them into a pipeline

**Deliverable:** 3 CONSTRUCT queries + pipeline configuration + before/after graph diffs

**Key Concepts:** RDF, OWL, SPARQL CONSTRUCT, graph rewriting, declarative transformation

---

## Week 8 — Full-Text Compilation and Template Systems

**Readings:** Ch 24 (Full-Text Compilation), Ch 25 (Template Systems and Projection Layers), Ch 26 (Compiler Pipelines)

**Lecture:**
- Text as the enterprise output medium — code, contracts, spreadsheets, decks, policy
- Projection surfaces and artifact classes — one graph, many outputs
- Templates as pure projection — no semantics in rendering
- The five-stage compiler pipeline: Load → Extract → Render → Validate → Emit
- Receipts as proof objects — cryptographic verification of transformation provenance

**Lab 8.1 — Build a μ₁-μ₅ Compiler**
```bash
# Full pipeline with receipt
ggen sync --spec .specify/specs/ -o generated/ --audit true
```

**Deliverable:** Working compiler pipeline + generated artifacts + receipt verification output

**Key Concepts:** μ₁-μ₅ pipeline, projection, template discipline, receipt emission

---

## Week 9 — Capability Surfaces and Interface Systems

**Readings:** Ch 27 (Capability Modeling), Ch 28 (Machine Action Surfaces), Ch 29 (Inter-Agent Capability Contracts), Ch 30 (CLI and Human Control Surfaces)

**Lecture:**
- Capabilities as first-class graph objects — visibility, exposure, constraints
- Tool surfaces, prompt surfaces, semantic boundaries for machine action
- Inter-agent capability contracts — declaration, alignment, interoperability
- CLI as governed interface — noun-verb grammar from ontology
- Neutral CLI ontology and application overlays

**Lab 9.1 — Generate a CLI from Ontology**
- Define a capability surface in TTL
- Use ggen's clap-noun-verb template system to generate a CLI
- Verify: every noun maps to an ontology class, every verb maps to an operation
- Test generated CLI with `--help`

**Deliverable:** TTL capability surface + generated CLI + coverage matrix (ontology → CLI surface)

**Key Concepts:** Capability surfaces, noun-verb grammar, governed interfaces, MCP tools

---

## Week 10 — Packs, Profiles, and Registries

**Readings:** Ch 31 (Pack Algebra), Ch 32 (Pack Composition), Ch 33 (Registries and Distribution), Ch 34 (Trust and Policy Enforcement)

**Lecture:**
- Atomic packs, bundles, profiles, foundation packs, consequence packs
- Dependency resolution, ownership classes, merge/overlay/exclusivity
- Public and private registries — transport vs trust
- Trust tiers, signature verification, lockfile authority
- Enterprise profiles and regulated execution modes

**Lab 10.1 — Build and Publish a Pack**
```bash
# Create a pack
ggen pack create --name my-domain --foundation core

# Add capabilities
ggen pack add --capability ontology:SupplyChain
ggen pack add --capability workflow:OrderFulfillment

# Publish to registry
ggen pack publish --registry local --sign
```

**Deliverable:** Pack manifest + dependency tree + installation receipt + trust verification

**Key Concepts:** Pack algebra, composition, registries, trust tiers, Ed25519 signatures

---

## Week 11 — Receipts, Quality, and Proof

**Readings:** Ch 35 (Receipts as Proof Objects), Ch 36 (Determinism and Reproducibility), Ch 37 (Quality Gates), Ch 38 (Validation and Observability)

**Lecture:**
- Composition receipts, artifact receipts, chain verification
- Canonical inputs, lock-resolved execution, deterministic emission
- Quality gates — evidence-based validation, type-enforced quality
- Andon-style escalation — stop the line when quality fails
- Runtime observability — OTEL spans as proof of execution

**Lab 11.1 — Ed25519 Receipt Chain**
- Generate 5 artifacts from the same ontology
- Verify each receipt
- Chain them: receipt 1 → receipt 2 → ... → receipt 5
- Verify the chain — prove no artifact was tampered with

**Deliverable:** Receipt chain + verification output + reflection on determinism

**Key Concepts:** Ed25519 receipts, chain verification, determinism, quality gates, OTEL

---

## Week 12 — Time, Versioning, and Temporal Navigation

**Readings:** Ch 39 (Event-Sourced Knowledge Graphs), Ch 40 (Temporal Versioning), Ch 41 (Time-Travel Query Systems), Ch 42 (Wormhole-Based Navigation)

**Lecture:**
- Events as state transitions — event-sourced knowledge graphs
- Temporal graph construction, snapshotting, causal consistency
- Time-travel queries — reconstruct historical state
- Wormhole navigation — temporal anchors, nonlinear access
- Temporal calculus for knowledge systems

**Lab 12.1 — Event-Sourced Knowledge Graph**
- Build an event-sourced knowledge graph over 10 revisions of an ontology
- Implement 3 time-travel queries:
  1. "What did the graph look like at revision 5?"
  2. "What changed between revision 3 and revision 7?"
  3. "Show the causal chain that led to the current state"
- Implement a wormhole jump to a specific historical state

**Deliverable:** Event-sourced graph + 3 temporal queries + wormhole demonstration

**Key Concepts:** Event sourcing, temporal versioning, time-travel queries, wormhole navigation

---

## Week 13 — Enterprise Text, Multi-Agent Systems

**Readings:** Ch 43 (The Enterprise as a Text-Generating System), Ch 44 (Architecture Frameworks), Ch 45 (Organizational Knowledge Graphs), Ch 46 (Multi-Agent Organizational Systems), Ch 47 (Competitive Moats), Ch 48 (Safety and Reliability)

**Lecture:**
- Why organizations run on text — textual drift, semantic synchronization
- Enterprise architecture as compiler input — doctrine as source
- Multi-agent coordination without message passing — shared graph state
- Capability marketplaces — agent discovery, contract alignment
- Coordination cost asymmetry as competitive moat
- Hard block semantics, safe extension, trust-preserving generation

**Lab 13.1 — Multi-Agent Coordination**
- Deploy 3 agents on a shared ontology graph
- Agent 1: Process miner (reads event logs, updates graph)
- Agent 2: Code generator (reads graph, generates artifacts)
- Agent 3: Quality gate (reads artifacts, validates, updates graph)
- Observe: no message passing required — coordination through shared graph state

**Deliverable:** Multi-agent system + coordination trace + reflection on message-free coordination

**Key Concepts:** Shared graph coordination, capability marketplace, hard blocks, trust preservation

---

## Week 14 — Practicum, Conclusion, Event Horizon

**Readings:** Ch 49 (Research Methods), Ch 50–53 (Practicum chapters), Ch 54 (The Event Horizon of Construction)

**Lecture:**
- Research methods — frontier observatories, LLM-CONSTRUCT, closed-graph reconstruction
- The event horizon — human-coordinated vs machine-governed production
- The closed graph as institutional substrate
- When processes work, everything works

**Final Project Presentations (3 hours)**

**Final Project:** Students choose one:
1. **Design an Ontology-Native Program** (Ch 50) — full curriculum graph for a domain
2. **Build a Governed Compiler** (Ch 51) — source graph → query set → templates → receipts
3. **Build a Process-Intelligent Enterprise System** (Ch 52) — event data → PI layer → artifacts
4. **Evaluate a Machine-Governed Organization** (Ch 53) — maturity model, coordination speed, cost

---

## Assessment

| Component | Weight | Description |
|-----------|--------|-------------|
| Weekly Labs (13) | 40% | Each lab requires a deliverable (code + reflection) |
| Reading Reflections (6) | 15% | 500-word reflections on key concepts (Weeks 1, 3, 5, 8, 11, 13) |
| Midterm | 15% | Written exam covering Parts I–VI |
| Final Project | 30% | Choose from Ch 50–53 practicum options |

## Grading Criteria

- **Correctness:** Does the code compile? Do the receipts verify?
- **Evidence:** Are claims supported by OTEL spans, receipt chains, or test output?
- **Design:** Is the ontology well-structured? Are invariants encoded in types?
- **Reflection:** Does the student demonstrate understanding of WHY, not just HOW?

---

## Required Software

| Tool | Version | Purpose |
|------|---------|---------|
| ggen-cli | 6.0.1+ | Code generation, YAWL, packs |
| Rust toolchain | 1.91.1+ | Lab development |
| Oxigraph | latest | RDF triplestore |
| ggen-process-mining | latest | Process mining labs |
| ggen-yawl | latest | Workflow generation |

## Supplementary Reading

| Source | Relevant Chapters |
|--------|-------------------|
| van der Aalst, *Process Mining* (2nd ed.) | Ch 14–17 |
| van der Aalst & ter Hofstede, *YAWL* | Ch 18–19 |
| Berners-Lee et al., *Semantic Web* | Ch 22–23 |
| Womack & Jones, *Lean Thinking* | Ch 8 |
| Liker, *The Toyota Way* | Ch 37 |
| Cousot & Cousot, *Abstract Interpretation* | Ch 4 |

---

*This syllabus assumes the field already exists and teaches its settled canon.*
