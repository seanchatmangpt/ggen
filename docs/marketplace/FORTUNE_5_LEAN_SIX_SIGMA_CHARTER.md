# Lean Six Sigma Project Charter — ggen (Fortune 5 Enterprise Text Compiler)

**Document version:** 1.0  
**Last updated:** 2026-03-31  
**Scope:** Strategic DfLSS charter + engineering alignment (this repo). Branch/commit may vary; use `git log -1` for exact revision.

**Related technical docs:** [PIPELINE_INTEGRATION.md](PIPELINE_INTEGRATION.md) (μ-stage model), [ATOMIC_PACKS.md](ATOMIC_PACKS.md), [BUNDLES_AND_PROFILES.md](BUNDLES_AND_PROFILES.md), [SECURITY_MODEL.md](SECURITY_MODEL.md). **Build/test gate truth:** [FORTUNE_5_WIP_STATUS.md](FORTUNE_5_WIP_STATUS.md). **Inventory snapshot (may lag):** [DELTA.md](DELTA.md).

**Pipeline nuance:** [PIPELINE_INTEGRATION.md](PIPELINE_INTEGRATION.md) describes μ₂/μ₃ consuming project *and* pack queries/templates. In code, μ₀ (`PackResolver`) resolves packs and loads contributed queries/templates into `ResolvedPacks`, but [crates/ggen-core/src/v6/pipeline.rs](../../crates/ggen-core/src/v6/pipeline.rs) still carries explicit TODOs to feed pack queries into μ₂ and register pack templates in μ₃—treat “full pack-aware extraction/emission” as **not complete** until those paths are implemented.

**Organizational roles:** Titles below are canonical for the initiative narrative. **Named individuals and multi-title sponsor lines are illustrative** until recorded on the signoff grid with formal enterprise approval.

---

## Project Title

**ggen: Ontology-Governed Enterprise Text Compiler for Machine-Governed Production**

## Project Type

**Design for Lean Six Sigma (DfLSS)** / Enterprise Transformation Initiative

## Business Unit

Enterprise Architecture / Platform Engineering / AI Transformation / Knowledge Systems

## Project Sponsor (roles — formal names on signoff)

Chief Information Security Officer  
Chief Technology Officer  
Chief Enterprise Architect  

## Project Champion (role — formal name on signoff)

VP, Platform Transformation / VP, Enterprise Architecture  

## Project Leader / Black Belt / Lead Architect

*Named on signoff; charter narrative owner: Sean Chatman (pending sponsor confirmation).*

## Core Team

- Enterprise Architecture  
- Platform Engineering  
- Security Architecture  
- AI/Agent Platform  
- Workflow / BPM Engineering  
- DevEx / Tooling  
- Compliance / Audit  
- Quality Engineering  
- Business Transformation Office  

---

## 1. Business Case

Fortune 5 enterprises operate through fragmented text systems: code, APIs, workflows, spreadsheets, architecture documents, executive decks, policies, compliance artifacts, and agent capability declarations. These artifacts are currently produced and reconciled through human coordination, creating drift, delay, inconsistency, and hidden operational risk.

ggen addresses this by serving as an **ontology-governed full-text compiler** that can generate and maintain the enterprise’s operational text surfaces from authoritative truth. When integrated with YAWL, MCP, A2A, receipts, and governed pack composition, ggen enables a shift from **human-coordinated production** to **machine-governed construction**.

This initiative is strategically important because it reduces the human coordination bottleneck, improves consistency across artifact classes, strengthens auditability, and creates a new operating model for enterprise production.

---

## 2. Problem Statement

Current enterprise production systems rely on manual synchronization across multiple artifact classes and organizational boundaries. As a result:

- architecture and implementation drift apart  
- protocol and capability surfaces are manually maintained  
- workflow, API, agent, and documentation systems are disconnected  
- upgrades and migrations require human choreography  
- validation and audit proof are reconstructed after the fact  
- textual operating surfaces across the enterprise become stale, duplicative, and contradictory  

Within ggen itself, major enabling infrastructure exists, but critical integration and governance questions remain around pack algebra, capability composition, trust, ownership, runtime explicitness, and enterprise compiler authority.

---

## 3. Goal Statement

Design, validate, and operationalize ggen as a **governed enterprise text compiler** capable of generating coherent, deterministic, and auditable enterprise artifacts across technical, operational, architectural, and executive text surfaces.

The project will establish:

- a canonical pack algebra  
- compiler-stage integration of packs into `ggen sync`  
- signed, locked, and receipted pack composition  
- typed capability/surface/projection/runtime governance  
- deterministic generation across artifact families  
- enterprise policy controls suitable for Fortune 5 deployment  

---

## 4. Project Scope

### In Scope

- ontology-governed artifact generation  
- pack taxonomy and composition model  
- bundle/profile/foundation pack architecture  
- `ggen sync` integration for governed packs  
- MCP, A2A, OpenAPI, Rust generation surfaces  
- YAWL as coordination IR  
- receipt generation and proof model  
- Cargo/private registry transport model  
- enterprise governance model for trust, policy, and conflict resolution  
- extension to non-code artifact classes such as Slidev, XLSX, architecture and quality artifacts  

### Out of Scope

- generalized public community marketplace as phase 1  
- unconstrained plugin ecosystem  
- manual/template-only codegen paths outside `μ`  
- runtime autonomy without governed receipts and validation  
- purely cosmetic CLI/UX work disconnected from pack truth  
- enterprise-wide rollout before reference pack proof  

---

## 5. Customers / Stakeholders

### Primary Customers

- Enterprise Architecture  
- Security / CISO Office  
- Platform Engineering  
- AI Agent Platform teams  
- Workflow / BPM teams  
- Compliance / Audit  

### Secondary Customers

- Application engineering teams  
- PMO / Transformation office  
- Operations / SRE  
- Strategy / executive communications teams  

---

## 6. CTQs (Critical to Quality)

The CISO and enterprise architecture office would likely define these as the non-negotiable CTQs:

1. **Determinism** — Same governed inputs produce same outputs.  
2. **Traceability** — Every output is traceable to ontology, queries, templates, validators, and packs.  
3. **Compiler Authority** — `ggen sync` is the authoritative path; no bypass generation.  
4. **No Hidden Semantics** — Templates contain no business logic, defaults, or inferred capability.  
5. **Signed and Governed Composition** — All packs are versioned, trusted, policy-evaluated, and lockfile-bound.  
6. **Conflict Safety** — Overlap, ambiguity, and contradiction fail closed.  
7. **Runtime Explicitness** — No silent runtime selection for security-significant surfaces.  
8. **Proofability** — Receipts are replayable, auditable, and composition-aware.  
9. **Scalability Across Artifact Classes** — Supports code, contracts, workflows, decks, spreadsheets, policy, and architecture outputs.  
10. **Reduction of Human Coordination Load** — Demonstrable shift from manual synchronization to machine-governed generation.  

---

## 7. Project Objective

Create a production-ready design and reference implementation proving that ggen can compile enterprise truth into multiple governed output classes while preserving security, policy, determinism, and receipts.

---

## 8. Target Outcomes

### Strategic Outcomes

- Establish a new category: **machine-governed enterprise text production**  
- Cross the event horizon from human-coordinated delivery to machine-governed construction  
- Reduce architecture/implementation drift  
- Reduce upgrade choreography labor  
- Enable enterprise capability composition at scale  

### Operational Outcomes

- installable governed packs  
- pack-aware `ggen sync`  
- reference `mcp-rust` pack  
- typed atomic pack taxonomy  
- bundle and profile model  
- lockfile/provenance model  
- multi-pack receipts  
- policy-driven conflict engine  

---

## 9. Baseline / Current State (evidence-aligned)

**Strategic:** The conceptual model (μ pipeline, packs, receipts, marketplace) is advanced; institutional load-bearing requires proven end-to-end paths and green CI.

**Implementation (this repository):**

- **Library layer:** `ggen-marketplace` includes atomic taxonomy, bundles, profiles, ownership, multi-dimensional compatibility, policy enforcement, composition receipts, and a substantial `install.rs` (dependency resolution, cache, trust hooks). `ggen-core` implements μ₀ via `PackResolver` and merges pack ontologies into the pipeline graph when `.ggen/packs.lock` is present.  
- **Gaps vs charter CTQs:** (1) `ggen packs` CLI verbs largely return mock or TODO wiring ([crates/ggen-cli/src/cmds/packs.rs](../../crates/ggen-cli/src/cmds/packs.rs)). (2) v6 pipeline still TODOs feeding pack SPARQL into μ₂ and registering pack Tera templates in μ₃ ([crates/ggen-core/src/v6/pipeline.rs](../../crates/ggen-core/src/v6/pipeline.rs)). (3) Receipt `PackProvenance` fields for signature, digest, and contributed templates/queries/files include placeholders until wired from pack metadata and execution.  
- **Documentation:** [DELTA.md](DELTA.md) describes a disconnected CLI/sync picture as of its snapshot date; [FORTUNE_5_WIP_STATUS.md](FORTUNE_5_WIP_STATUS.md) tracks compile/test gates and newer modules—**prefer WIP status + this charter appendix** when they disagree.

**Bottom line:** Engine and governance modules exist; **operator-facing install/sync/receipt completeness** remains the main institutional risk.

---

## 10. Future State Vision

In the future state:

- enterprise capabilities are enabled through typed pack composition  
- `ggen sync` loads project + pack ontology into one authoritative pipeline  
- YAWL coordination, MCP surfaces, A2A contracts, OpenAPI, Rust, receipts, Slidev, XLSX, and other artifacts are generated from common truth  
- public/private registry transport is separated from governance and trust  
- profiles enforce enterprise operating posture  
- receipts prove every emitted artifact and composition decision  
- the platform no longer depends on one human expert to prevent drift  

---

## 11. Measures of Success

### Primary Metrics

- % of generated artifacts produced through authoritative `μ` pipeline  
- % of outputs with complete composition receipts  
- % of pack installs with verified signature, provenance, and lockfile entry  
- number of artifact classes emitted from shared ontology  
- reduction in manual reconciliation steps across surfaces  
- reduction in drift defects between architecture, interface, and implementation  
- percentage of pack conflicts caught before generation  
- percentage of runtime decisions made explicitly rather than implicitly  

### Secondary Metrics

- time to enable a new governed capability  
- time to emit migration artifacts after ontology change  
- number of reusable atomic packs certified for enterprise use  
- number of business/technical text surfaces compiled from same truth source  

---

## 12. Financial / Strategic Benefit Hypothesis

ggen is expected to create value by reducing:

- manual coordination cost  
- architecture drift rework  
- interface mismatch remediation  
- document/code divergence  
- migration planning overhead  
- audit reconstruction effort  
- platform duplication across teams  

It is also expected to increase:

- speed of governed delivery  
- reuse of enterprise capability patterns  
- audit readiness  
- strategic coherence  
- enterprise knowledge leverage  
- quality of machine-consumable interfaces  

---

## 13. Risks

### Key Risks

- concept collapse into “just a plugin marketplace”  
- template/business-logic leakage  
- ambiguous ownership and overlap  
- silent runtime or capability defaults  
- public package trust contamination  
- failure to unify competing architectures  
- underinvestment in receipts and lockfile truth  
- institutional resistance due to unfamiliarity  
- overexpansion before reference implementation is proven  

### Mitigations

- typed atomic pack model  
- capability-first explicit UX  
- Tera as canonical rendering engine  
- strict ownership classes  
- trust tiers and enterprise profiles  
- foundation packs for shared ontology  
- consequence packs for upgrade law  
- CISO-driven policy gates  
- MCP reference pack as first proof point  

---

## 14. Constraints

- limited team support / founder-led architecture  
- need for Fortune 5-grade trust and proof  
- requirement for deterministic generation  
- existing codebase contains disconnected infrastructure and stubs in some CLI paths  
- organizational understanding currently lags technical significance  

---

## 15. Assumptions

- ontology can serve as authoritative truth source  
- YAWL can act as coordination IR  
- MCP and A2A can be projected as governed surfaces  
- Cargo/private registries can serve as transport substrate  
- packs can be made signed, attestable, and policy-governed  
- multiple enterprise text classes can be compiled from one truth model  

---

## 16. Methodology

### DfLSS / DMADV

**Define** — Define the problem as enterprise text drift and coordination burden.  

**Measure** — Measure current generation paths, manual handoffs, drift points, and proof gaps.  

**Analyze** — Analyze pack algebra, ownership conflict, runtime ambiguity, trust model, and compiler authority requirements.  

**Design** — Design atomic pack taxonomy; bundle/profile model; sync integration; install/lockfile/provenance path; receipts; CISO policy architecture.  

**Verify** — Verify using reference implementations: `mcp-rust`, `a2a-rust`, `openapi-rust`, and one non-code text target such as Slidev or XLSX.  

---

## 17. Deliverables

### Phase 1 Deliverables

- PRD/ARD for governed pack platform  
- canonical pack taxonomy  
- `ggen-pack.toml` schema  
- lockfile/provenance schema  
- ownership/conflict model  
- enterprise profile model  

### Phase 2 Deliverables

- functioning installer  
- pack-aware `ggen sync`  
- Tera-only canonical rendering path  
- reference `mcp-rust` pack  
- composition receipts  

### Phase 3 Deliverables

- `a2a-rust` pack  
- `openapi-rust` pack  
- policy foundation packs  
- runtime packs  
- one enterprise text projection beyond code  

### Phase 4 Deliverables

- private registry integration  
- trust tiers / certification pipeline  
- enterprise rollout model  
- measurement dashboard  

---

## 18. Milestones

| # | Milestone | Target |
|---|-----------|--------|
| 1 | Canonical architecture approved (pack classes, bundle/profile distinction, CISO signoff) | **TBD (quarter)** |
| 2 | Real install path + lockfile + trust record | **TBD** |
| 3 | Pack-aware `ggen sync` | **TBD** |
| 4 | Reference `mcp-rust` pack proves compiler-stage generation | **TBD** |
| 5 | Multi-pack receipt proof | **TBD** |
| 6 | Non-code enterprise text output proves broader compiler target | **TBD** |
| 7 | Enterprise pilot under controlled profile | **TBD** |

---

## 19. Tollgate Questions

A Lean Six Sigma sponsor would ask:

1. Is the problem defined in measurable business terms?  
2. Is the compiler authority model unambiguous?  
3. Have all meaningful conflict dimensions been identified?  
4. Is trust separated from package transport?  
5. Are atomic packs clearly distinct from bundles and profiles?  
6. Can the system fail closed on ambiguity?  
7. Are receipts sufficient for audit and replay?  
8. Has at least one capability family been proven end-to-end?  
9. Can this reduce coordination labor at enterprise scale?  
10. Does this cross the event horizon from manual synchronization to machine-governed production?  

---

## 20. Charter Statement

**This project exists to design and prove ggen as a governed enterprise text compiler that can replace human-maintained synchronization across architecture, interfaces, workflows, policies, and technical artifacts with machine-governed generation, validation, and proof.**

---

## 21. One-Page Executive Version

### Project

ggen — Ontology-Governed Enterprise Text Compiler  

### Why Now

Fortune 5 enterprises are constrained less by coding speed than by text drift and coordination overhead across code, APIs, workflows, decks, spreadsheets, policy, and architecture artifacts.  

### Goal

Create a machine-governed system that compiles enterprise truth into coherent, deterministic, and auditable artifact surfaces.  

### Success Looks Like

- packs install and govern generation  
- `ggen sync` is authoritative  
- MCP/A2A/OpenAPI/Rust/Slidev/XLSX can be emitted from shared truth  
- receipts prove all outputs  
- profiles enforce CISO-grade policy  
- human coordination bottleneck materially reduced  

### Critical Risks

- hidden defaults  
- ambiguous overlap  
- public dependency trust  
- split architecture  
- false-green CLI behavior  

### Strategic Payoff

Cross the event horizon from human-coordinated enterprise production to machine-governed construction.  

---

## Appendix A — Corporate charter summary table (Six Sigma style)

| Field | Content |
|-------|---------|
| **Project title** | ggen: Ontology-Governed Enterprise Text Compiler for Machine-Governed Production |
| **Project type** | DfLSS / Enterprise transformation |
| **Business unit** | EA / Platform / AI / Knowledge systems |
| **Sponsor (roles)** | CISO; CTO; Chief EA — *names: signoff pending* |
| **Champion** | VP Platform Transformation / VP EA — *name: signoff pending* |
| **Project leader / BB** | *Signoff pending* (narrative owner: Sean Chatman, pending confirmation) |
| **Problem** | Manual sync across artifact classes → drift, delay, weak audit, operational risk; ggen must close pack/sync/receipt gaps. |
| **Goal** | Governed, deterministic, auditable multi-surface compilation; compiler authority; fail-closed conflicts. |
| **Scope — In** | Ontology-first generation; packs/bundles/profiles; sync integration; MCP/A2A/OpenAPI/Rust; receipts; policy/trust; non-code targets (Slidev/XLSX) in later phases. |
| **Scope — Out** | Phase-1 public marketplace; unconstrained plugins; non-μ template-only paths; rollout before reference proof. |
| **Primary KPIs** | % artifacts via μ; % outputs with full receipts; % installs with verified sig + lockfile; artifact-class count; ↓ reconciliation steps; ↓ drift defects; % conflicts caught pre-gen; % explicit runtime decisions. |
| **Milestones** | M1–M7 as §18; dates **TBD**. |
| **Tollgates** | §19 checklist — all must be “yes” or waived with recorded risk. |

### CTQ → KPI mapping (measurable acceptance)

| CTQ | KPI (from §11) |
|-----|----------------|
| 1 Determinism | Same inputs → identical outputs (hash/receipt verification in CI) |
| 2 Traceability | % outputs with receipt linking ontology, queries, templates, packs |
| 3 Compiler authority | % generated artifacts via authoritative `μ` pipeline only |
| 4 No hidden semantics | Template audit gates; no inferred capability in templates |
| 5 Signed governed composition | % installs with verified signature, provenance, lockfile entry |
| 6 Conflict safety | % pack conflicts caught before generation |
| 7 Runtime explicitness | % runtime decisions explicit (profile/config/receipt) |
| 8 Proofability | % outputs with complete composition receipts; replayable receipts |
| 9 Scalability across classes | Count of artifact classes from shared ontology |
| 10 ↓ coordination load | ↓ manual reconciliation steps; time to enable capability (secondary) |

### Formal signoff (print / record in PLM)

| Role | Name | Signature | Date |
|------|------|-----------|------|
| Executive sponsor (CISO) | | | |
| Executive sponsor (CTO) | | | |
| Executive sponsor (Chief EA) | | | |
| Project champion | | | |
| Project leader / Black Belt | | | |
| Security architecture delegate | | | |
| Enterprise architecture delegate | | | |

---

## Appendix B — Implementation alignment and tollgate status

### B.1 Charter phase → repository anchors

| Phase | Intent | Code / doc anchors |
|-------|--------|-------------------|
| **1** | Taxonomy, policy, conflict, profiles | [crates/ggen-marketplace/src/atomic.rs](../../crates/ggen-marketplace/src/atomic.rs), [bundle.rs](../../crates/ggen-marketplace/src/bundle.rs), [profile.rs](../../crates/ggen-marketplace/src/profile.rs), [ownership.rs](../../crates/ggen-marketplace/src/ownership.rs), [compatibility.rs](../../crates/ggen-marketplace/src/compatibility.rs), [policy.rs](../../crates/ggen-marketplace/src/policy.rs); docs: [ATOMIC_PACKS.md](ATOMIC_PACKS.md), [BUNDLES_AND_PROFILES.md](BUNDLES_AND_PROFILES.md) |
| **2** | Install, lockfile, pack-aware sync | [crates/ggen-marketplace/src/install.rs](../../crates/ggen-marketplace/src/install.rs), [crates/ggen-core/src/pack_resolver.rs](../../crates/ggen-core/src/pack_resolver.rs), [crates/ggen-core/src/v6/pipeline.rs](../../crates/ggen-core/src/v6/pipeline.rs); CLI: [crates/ggen-cli/src/cmds/packs.rs](../../crates/ggen-cli/src/cmds/packs.rs), [capability.rs](../../crates/ggen-cli/src/cmds/capability.rs) |
| **3+** | Reference packs, non-code surfaces | Proof hooks: e.g. [crates/ggen-core/tests/mcp_generation_e2e_test.rs](../../crates/ggen-core/tests/mcp_generation_e2e_test.rs). **Slidev / XLSX** are charter targets—not claimed proven in-tree unless covered by dedicated tests and docs. |

### B.2 Milestone tollgate vs current evidence (R/Y/G)

| Milestone | Status | Rationale |
|-----------|--------|-----------|
| **M1** Canonical architecture | **Yellow** | Taxonomy/modules exist; formal enterprise “approved” is organizational, not code-only. |
| **M2** Install + lockfile + trust | **Yellow** | Crate installer advanced; CLI install and E2E lockfile population still TODO/mocked in `ggen packs`. |
| **M3** Pack-aware `ggen sync` | **Yellow** | μ₀ merges ontology; μ₂/μ₃ pack query/template wiring and full receipt fields incomplete (see §9). |
| **M4** Reference `mcp-rust` | **Yellow** | Use E2E tests + pack assets as evidence; treat as green only when sponsor-defined acceptance criteria met. |
| **M5** Multi-pack receipt proof | **Yellow** | Bundle expansion + pack list in receipt; real digests/signatures/template-query lists still placeholders in v6 pipeline. |
| **M6** Non-code enterprise text | **Red** | Charter target; not baseline-proven without named tests/artifacts. |
| **M7** Enterprise pilot | **Red** | Deployment posture outside this repo’s evidence. |

---

*End of charter document.*
