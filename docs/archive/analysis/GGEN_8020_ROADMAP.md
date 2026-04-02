<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Dark Matter/Energy 80/20 Roadmap](#ggen-dark-matterenergy-8020-roadmap)
  - [Executive Summary](#executive-summary)
  - [Part 1: Current State (What ggen Has Built)](#part-1-current-state-what-ggen-has-built)
    - [1.1 ✅ O-First Ontologies (COMPLETE)](#11--o-first-ontologies-complete)
    - [1.2 ✅ Projection Families (Π) (MOSTLY COMPLETE)](#12--projection-families-%CE%A0-mostly-complete)
    - [1.3 ✅ Marketplace + Validation (COMPLETE)](#13--marketplace--validation-complete)
    - [1.4 ✅ Graph-Anchored AI (COMPLETE)](#14--graph-anchored-ai-complete)
    - [1.5 ❌ Sector Bundles (MISSING - CRITICAL GAP)](#15--sector-bundles-missing---critical-gap)
    - [1.6 ⚠️ Validation Receipts (PARTIALLY MISSING)](#16--validation-receipts-partially-missing)
    - [1.7 ⚠️ 8020 Guard + Badge (PARTIALLY MISSING)](#17--8020-guard--badge-partially-missing)
    - [1.8 ⚠️ Dark Matter Reduction Targets (MISSING)](#18--dark-matter-reduction-targets-missing)
  - [Part 2: The 80/20 Strategy (What to Build)](#part-2-the-8020-strategy-what-to-build)
    - [2.1 Design Principle: Sector Bundles as First-Class Artifacts](#21-design-principle-sector-bundles-as-first-class-artifacts)
    - [2.2 Five Core 8020 Packages to Build](#22-five-core-8020-packages-to-build)
      - [**8020 Package #1: `sector-observability-8020`**](#8020-package-1-sector-observability-8020)
      - [**8020 Package #2: `sector-rust-microservice-8020`**](#8020-package-2-sector-rust-microservice-8020)
      - [**8020 Package #3: `sector-paper-lifecycle-8020`**](#8020-package-3-sector-paper-lifecycle-8020)
      - [**8020 Package #4: `sector-support-hooks-8020`**](#8020-package-4-sector-support-hooks-8020)
      - [**8020 Package #5: `sector-api-gateway-8020`**](#8020-package-5-sector-api-gateway-8020)
    - [2.3 Guard Definitions (8020 Validation Criteria)](#23-guard-definitions-8020-validation-criteria)
      - [Guard: `Guard8020Coverage`](#guard-guard8020coverage)
      - [Guard: `GuardChatmanCompliant`](#guard-guardchatmancompliant)
      - [Guard: `GuardTelemetryComplete`](#guard-guardtelemetrycomplete)
    - [2.4 Implementation Steps (Near-Term: 2-4 Weeks)](#24-implementation-steps-near-term-2-4-weeks)
      - [**Phase 1: Infrastructure (Week 1)**](#phase-1-infrastructure-week-1)
      - [**Phase 2: Package Composition (Week 2-3)**](#phase-2-package-composition-week-2-3)
      - [**Phase 3: Validation & Documentation (Week 4)**](#phase-3-validation--documentation-week-4)
  - [Part 3: Dark Matter Reduction Targets (Measurable Claims)](#part-3-dark-matter-reduction-targets-measurable-claims)
    - [3.1 Proposed Targets Per Sector](#31-proposed-targets-per-sector)
      - [**Healthcare Sector**:](#healthcare-sector)
      - [**Microservice Sector**:](#microservice-sector)
      - [**Academic Sector**:](#academic-sector)
      - [**Support Sector**:](#support-sector)
    - [3.2 Measurement Approach](#32-measurement-approach)
  - [Part 4: Roadmap Timeline & Ownership](#part-4-roadmap-timeline--ownership)
    - [Week 1: Infrastructure](#week-1-infrastructure)
    - [Week 2-3: Package Composition](#week-2-3-package-composition)
    - [Week 4: Validation & Launch](#week-4-validation--launch)
  - [Part 5: Strategic Outcomes](#part-5-strategic-outcomes)
    - [What This Achieves](#what-this-achieves)
  - [Part 6: Appendix](#part-6-appendix)
    - [6.1 Package Structure Reference](#61-package-structure-reference)
    - [6.2 Dark Matter/Energy Glossary](#62-dark-matterenergy-glossary)
    - [6.3 Existing Proof Points](#63-existing-proof-points)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Dark Matter/Energy 80/20 Roadmap

**Status**: DRAFT - Strategic Roadmap for 8020 Innovation Line
**Branch**: `claude/ggen-8020-roadmap-011tUBxZ8opuu5rNgzCMBvyP`
**Vision**: Which 20% of ggen's capabilities eliminate 80% of dark matter/energy from knowledge work?

---

## Executive Summary

This roadmap reframes ggen's innovation in terms of **dark matter/energy elimination**:
- **Dark Matter** = invisible scaffolding work (copying structures, reformatting, syncing definitions)
- **Dark Energy** = continuous drag (keeping docs/schemas in sync, re-onboarding projects)

ggen's job is to make the domain O explicit (RDF) and let μ (ggen's projection system) handle everything else, pushing dark work down to the platform instead of humans.

**Core Insight**: A = μ(O) — when domain O is centralized as an ontology, ggen's projections (μ) can eliminate most manual scaffolding work at scale.

---

## Part 1: Current State (What ggen Has Built)

### 1.1 ✅ O-First Ontologies (COMPLETE)

**What Works**:
- RDF/SPARQL core with Oxigraph triple store (real SPARQL 1.1)
- 2 example ontologies:
  - `academic-paper_v1.0.0.ttl` (21KB, academic domain model)
  - `clap-noun-verb_v3.3.0.ttl` (14KB, CLI argument parsing)
- Graph comparison and delta detection (`delta.rs`)
- RDF metadata in template frontmatter (YAML/TOML + SPARQL queries)

**Dark Matter Eliminated**:
- Users define domain O once; all downstream artifacts derive from it
- Schema drift prevented by single source of truth

**Evidence**:
- `ggen-core/src/graph/` handles all RDF operations
- Delta-driven projection detects ontology changes and regenerates only affected templates
- Three-way merge supports mixed generated/manual content

---

### 1.2 ✅ Projection Families (Π) (MOSTLY COMPLETE)

**What Works**:
- **Π_models**: Rust → TypeScript → Python (type mapping tested & working)
- **Π_apis**: REST endpoints from SPARQL specifications
- **Π_cli**: Noun-verb command structures from ontologies
- **Π_papers**: Academic paper LaTeX generation from RDF
- **Π_data**: Struct/interface/class generation from rdfs:Class

**Type Mapping Tested**:
```
XSD Type      | Rust      | TypeScript | Python
xsd:string    | String    | string     | str
xsd:decimal   | f64       | number     | float
xsd:integer   | i64       | number     | int
xsd:boolean   | bool      | boolean    | bool
xsd:dateTime  | DateTime  | Date       | datetime
```

**Dark Matter Eliminated**:
- No need to hand-write language-specific models
- No manual synchronization of type definitions across languages
- Templates are SPARQL-driven; queries extract variables from O

**Evidence**:
- 77 template files (.tmpl) with SPARQL frontmatter
- Type mapping tested in `tests/`
- SPARQL query execution with caching (`ggen-core/src/graph/sparql.rs`)

---

### 1.3 ✅ Marketplace + Validation (COMPLETE)

**What Works**:
- 76 active marketplace packages organized by domain
- Package manifest structure (`package.toml` + `ggen.toml`)
- 6-dimensional production readiness scoring:
  1. Documentation (0-20 pts)
  2. Testing (0-20 pts)
  3. Security (0-20 pts)
  4. Performance (0-15 pts)
  5. Adoption (0-15 pts)
  6. Maintenance (0-10 pts)
  - **Total: 100 points max** → MaturityLevel (Experimental / Beta / Production / Enterprise)

**Validation Guards** (Currently Active):
- Package metadata completeness
- Documentation standards (README, API docs, examples, changelog)
- Configuration validation (make.toml, Cargo.toml)
- File existence checks
- License file detection

**Dark Matter Eliminated**:
- Users don't hand-craft scaffolding; they install production-ready templates
- Quality criteria deterministic, not tribal knowledge
- Broken packages blocked before publication

**Evidence**:
- `ggen-marketplace` crate implements full validation framework
- 85% production readiness possible for well-maintained packages (e.g., `advanced-rust-api-8020`)
- Cleanroom E2E tests confirm packages work standalone

---

### 1.4 ✅ Graph-Anchored AI (COMPLETE)

**What Works**:
- Multi-provider LLM client (OpenAI, Anthropic, Ollama, Gemini, Groq, etc.)
- Schema-anchored generation (not direct code generation):
  - Input: Natural language + RDF schema
  - Output: Validated structures (templates parsed, Turtle validated before return)
- Generators:
  - Template Generator: NL → ggen templates
  - Ontology Generator: Domain descriptions → RDF/Turtle
  - SPARQL Generator: Intent → SPARQL queries
  - Refactor Assistant: Code improvement suggestions

**Dark Matter Eliminated**:
- AI doesn't generate code in isolation; it generates ontology edits
- ggen regenerates all artifacts → consistency by design
- No prompt sprawl; all calls anchored to the same O

**Evidence**:
- `ggen-ai` crate with streaming support
- Template validation framework (constraints module defined)
- LLM response parsing ensures valid Turtle/templates before returning

---

### 1.5 ❌ Sector Bundles (MISSING - CRITICAL GAP)

**What's Missing**:
- No pre-composed vertical stacks (e.g., "Healthcare Bundle", "Microservice Bundle")
- 76 packages are standalone; users compose manually
- No package dependency resolution (dependencies field in package.toml unused)
- No explicit "sector bundle" artifact type

**Dark Matter Not Eliminated**:
- Users must understand which packages work together
- Each org re-invents end-to-end pipelines for well-known problems
- Fragmented setup: "we use X for API, Y for observability, Z for docs"

**Impact**: 🔴 **CRITICAL** — Users lose 80/20 gains because they must manually assemble the 20%

---

### 1.6 ⚠️ Validation Receipts (PARTIALLY MISSING)

**What's Missing**:
- No signed proof that template→code validation passed
- No "validation receipt" JSON artifact
- Tests pass/fail silently; no auditable proof

**Dark Matter Not Eliminated**:
- "Is this template safe to use?" remains a judgment call
- Can't audit why a package is/isn't production-ready
- No signed certificates proving quality standards met

**Impact**: 🟡 **MEDIUM** — Package trust remains implicit instead of explicit

---

### 1.7 ⚠️ 8020 Guard + Badge (PARTIALLY MISSING)

**What Exists**:
- `advanced-rust-api-8020` example package (proves concept)
- Production readiness scoring exists

**What's Missing**:
- No explicit `is_8020` flag in package metadata
- No marketplace filter for "show only 8020 packages"
- No guard definition that checks 8020 coverage criteria
- No CLI command: `ggen market search --only-8020`

**Dark Matter Not Eliminated**:
- Users can't distinguish "critical 20%" packages from the rest
- No deterministic way to find pre-composed stacks

**Impact**: 🟡 **MEDIUM** — 8020 concept exists but isn't surfaced in tooling

---

### 1.8 ⚠️ Dark Matter Reduction Targets (MISSING)

**What's Missing**:
- No per-package claim about % of work eliminated
- No sector-specific targets (e.g., "healthcare bundle reduces manual setup by 70%")
- No way to measure success

**Dark Matter Not Eliminated**:
- Value proposition implicit, not measurable
- Can't demonstrate ROI of adopting 8020 packages

**Impact**: 🟡 **MEDIUM** — Can't measure if 80/20 strategy is working

---

## Part 2: The 80/20 Strategy (What to Build)

### 2.1 Design Principle: Sector Bundles as First-Class Artifacts

**Vision**: Install an entire vertical stack, not isolated templates.

**Example**: Installing `sector-healthcare-8020` gives you:
```
✅ ontology/ (FHIR + compliance models)
✅ templates/ (structured data templates)
✅ guards/ (healthcare-specific validation rules)
✅ examples/ (HIPAA-compliant patterns)
✅ CI/ (healthcare testing pipeline)
```

**Result**: Users don't start from "Hello world," they start from "80% of a working healthcare system."

---

### 2.2 Five Core 8020 Packages to Build

#### **8020 Package #1: `sector-observability-8020`**

**Composition**:
- O: Observability ontology (metrics, spans, logs, SLOs)
- Templates: OTEL configuration, Weaver registry, SLI/SLO dashboards
- Guards: "Telemetry-complete" (spans present, metrics named, SLOs defined)
- Examples: Microservice instrumentation, database monitoring, trace-to-metrics

**Dark Matter Eliminated**:
- Stop hand-wiring observability for each service (70% reduction in setup)
- Span naming standardized; no "what should we call this span?" debates
- SLO definitions enforceable; no drift between team goals and dashboards

**Target Package**: Exists partially (`observability` category has 5 packages)
**Work Needed**: Compose into unified bundle + add 8020 guard + dark matter targets

---

#### **8020 Package #2: `sector-rust-microservice-8020`**

**Composition**:
- O: Microservice domain ontology (services, endpoints, dependencies)
- Templates: Service scaffold, error handling (chicago-tdd-tools), logging, health checks
- Guards: "Chatman-compliant" (no unwrap, AAA enforced, error handling correct)
- Examples: API server, gRPC service, async runtime patterns

**Dark Matter Eliminated**:
- Stop rewriting error handling for every new service (50% reduction in scaffolding)
- Force functions comply with Chatman's AAA (All-Agree-Always) principle
- New services inherit production patterns by default

**Target Package**: `advanced-rust-api-8020` (exists, proven at 85% readiness)
**Work Needed**: Define guards + document dark matter targets + link to observability bundle

---

#### **8020 Package #3: `sector-paper-lifecycle-8020`**

**Composition**:
- O: Academic paper ontology (authors, submissions, reviews, versions)
- Templates: LaTeX submission pack, reviewer response, camera-ready format
- Guards: "Paper-complete" (all sections present, citations valid, submission requirements met)
- Examples: ICML/NeurIPS/ICLR submission templates, cover letters

**Dark Matter Eliminated**:
- Stop reformatting papers for different venues (80% reduction in formatting work)
- Submission checklists automated; no more "did we include this?" emails
- Version control for papers; audit trail of changes

**Target Package**: `academic-paper-v1.0.0` examples exist
**Work Needed**: Build unified bundle + guards + submission automation

---

#### **8020 Package #4: `sector-support-hooks-8020`**

**Composition**:
- O: Support case domain ontology (issues, routing, escalation, SLAs)
- Templates: Case intake form, routing rules, SLA tracker, escalation matrix
- Guards: "Support-complete" (all routing paths covered, SLAs defined, escalation clear)
- Examples: Zendesk/Jira integration patterns, Fortune-500 support flows

**Dark Matter Eliminated**:
- Stop manually routing support cases (90% reduction in triage work)
- SLA tracking automated; no more "are we meeting SLAs?" manual checks
- Escalation paths enforceable; no more stuck cases falling through cracks

**Target Package**: New; needs design
**Work Needed**: Build ontology + guards + hooks + examples

---

#### **8020 Package #5: `sector-api-gateway-8020`**

**Composition**:
- O: API gateway ontology (routes, auth, rate limiting, versioning)
- Templates: Kubernetes ingress, rate limiter config, OAuth2 setup
- Guards: "API-complete" (all endpoints documented, auth enforced, versioning clear)
- Examples: Multi-tenant SaaS gateway, versioned API patterns

**Target Package**: New; needs design
**Work Needed**: Build ontology + guards + templates

---

### 2.3 Guard Definitions (8020 Validation Criteria)

#### Guard: `Guard8020Coverage`

**Checks**:
```rust
pub struct Guard8020Coverage {
    ontology_present: bool,           // O exists and is valid RDF
    projections_complete: bool,       // Π_models, Π_apis, Π_docs all present
    templates_present: bool,          // At least 3 templates in templates/
    tests_present: bool,              // Unit + integration tests present
    docs_present: bool,               // README, examples, architecture docs
    guards_defined: bool,             // At least 1 validation guard
    bundle_integration: bool,         // Works with at least 1 other 8020 package
    dark_matter_target: Option<String>, // "Reduces X work by Y%"
}
```

**Scoring**:
- 6/7 checks pass → `is_8020 = true`
- Full pass (7/7) + dark matter target → `is_8020_certified = true`

**Result in Marketplace**:
```toml
[package]
name = "sector-observability-8020"
is_8020 = true
is_8020_certified = true
dark_matter_reduction_target = "Eliminates ~70% of manual observability setup"
```

---

#### Guard: `GuardChatmanCompliant`

**For Rust packages**:
```rust
pub struct GuardChatmanCompliant {
    no_unwrap: bool,              // Zero .unwrap() in production code
    no_panic: bool,               // No panic!() except in tests
    aaa_enforced: bool,           // Functions follow Arrange-Act-Assert
    error_handling_complete: bool, // All errors properly handled
    trait_bounds_clear: bool,     // Generic constraints well-documented
}
```

---

#### Guard: `GuardTelemetryComplete`

**For observability packages**:
```rust
pub struct GuardTelemetryComplete {
    otel_spans_present: bool,      // All async boundaries instrumented
    metrics_registered: bool,      // Weaver registry used
    slo_defined: bool,            // SLI/SLO targets documented
    dashboards_provided: bool,    // Reference dashboards (Grafana/Datadog)
}
```

---

### 2.4 Implementation Steps (Near-Term: 2-4 Weeks)

#### **Phase 1: Infrastructure (Week 1)**

1. **Add 8020 metadata to package.toml**:
   ```toml
   [package]
   is_8020 = false                    # Default false
   is_8020_certified = false
   dark_matter_reduction_target = ""  # Claim, e.g., "70% reduction in setup"
   sector = ""                        # healthcare, observability, api, support, etc.
   ```

2. **Implement Guard8020Coverage**:
   - Add to `ggen-marketplace/src/guards/`
   - Check: ontology, projections, templates, tests, docs, guards, bundle integration
   - Emit validation receipt (JSON proof)

3. **Update validation framework**:
   ```rust
   pub struct ValidationReceipt {
       package_name: String,
       version: String,
       timestamp: DateTime,
       checks_passed: Vec<String>,
       checks_failed: Vec<String>,
       guards_applied: Vec<String>,
       final_score: i32,
       is_8020: bool,
       signature: String,  // Signed proof
   }
   ```

4. **Update CLI to filter by 8020**:
   ```bash
   ggen market search --8020
   ggen market search --sector observability --8020
   ggen market install sector-observability-8020
   ```

---

#### **Phase 2: Package Composition (Week 2-3)**

5. **Compose `sector-observability-8020`**:
   - Define observability ontology (OTEL + Weaver + SLO models)
   - Link existing observability packages as dependencies
   - Add guards (GuardTelemetryComplete)
   - Document dark matter target

6. **Compose `sector-rust-microservice-8020`**:
   - Use existing `advanced-rust-api-8020` as foundation
   - Add guards (GuardChatmanCompliant, GuardTelemetryComplete)
   - Link to observability bundle via package dependencies
   - Document dark matter target

7. **Compose `sector-paper-lifecycle-8020`**:
   - Build paper submission ontology
   - Create venue-specific templates (ICML, NeurIPS, ICLR)
   - Add guards (GuardPaperComplete)
   - Document dark matter target

---

#### **Phase 3: Validation & Documentation (Week 4)**

8. **Add validation receipts**:
   - Emit signed JSON proof for each package validation
   - Store in marketplace registry
   - Surface in CLI: `ggen market info <package> --show-receipt`

9. **Document dark matter targets**:
   - Per-package claims with measurement approach
   - Examples: "Observability bundle reduces setup from 8 hours to 2 hours"
   - Collect telemetry to validate claims

10. **Update marketplace UI/docs**:
    - Badge "8020 Package" in marketplace
    - Filter/search by 8020 status
    - Show sector groupings
    - Link bundles to their composition

---

## Part 3: Dark Matter Reduction Targets (Measurable Claims)

### 3.1 Proposed Targets Per Sector

#### **Healthcare Sector**:
- **FHIR Compliance**: "Reduces schema translation work by 80% (from 16 hours to 3 hours per resource)"
- **Privacy/Compliance**: "Automated HIPAA checklist eliminates ~90% of compliance reviews (from 4 hours to 20 min)"
- **Interoperability**: "FHIR ontology eliminates custom integration mapping (from 2 days to 2 hours per integration)"

#### **Microservice Sector**:
- **Error Handling**: "Chatman-compliant templates eliminate hand-written error handling (from 6 hours to 0 per service)"
- **Observability**: "OTEL bundle setup reduces instrumentation from 8 hours to 1 hour"
- **Deployment**: "CI/CD scaffold eliminates Dockerfile/K8s config work (from 4 hours to 0.5 hours)"

#### **Academic Sector**:
- **Paper Formatting**: "Venue-specific templates eliminate reformatting work (from 4 hours to 0 per submission)"
- **Submission Prep**: "Automated checklist eliminates manual verification (from 1 hour to 5 min)"
- **Version Control**: "Paper ontology enables automatic versioning (from manual to fully automatic)"

#### **Support Sector**:
- **Case Routing**: "Automated routing eliminates manual triage (from 10 min/case to 0)"
- **SLA Tracking**: "Automated SLA monitoring eliminates manual tracking (from 2 hours/week to 0)"
- **Escalation**: "Clear escalation paths eliminate stuck cases (from 5% of cases to 0%)"

---

### 3.2 Measurement Approach

For each package, collect:
1. **Time estimates** (hours/task before and after)
2. **Error rates** (manual mistakes → automated checks)
3. **Consistency metrics** (% drift between systems)
4. **Adoption data** (# of users, # of packages installed)

**Example Dashboard**:
```
Package: sector-observability-8020
Dark Matter Reduction Target: "70% reduction in observability setup"

Measurement: Average setup time
- Before ggen: 8 hours (span naming, metric registration, SLO definition)
- After ggen: 2.4 hours (review + customize)
- Reduction: 70% ✅

Status: On track (500+ adopters, avg 7.2 hours saved per project)
```

---

## Part 4: Roadmap Timeline & Ownership

### Week 1: Infrastructure
- [ ] Add 8020 metadata to package.toml schema
- [ ] Implement Guard8020Coverage
- [ ] Build validation receipt system
- [ ] Update CLI for --8020 flag

**Ownership**: @coder + @analyst
**Success Criteria**: `ggen market search --8020` works, shows 0 packages (expected)

---

### Week 2-3: Package Composition
- [ ] Define sector-observability-8020 (ontology + guards + composition)
- [ ] Enhance sector-rust-microservice-8020 (guards + linking)
- [ ] Sketch sector-paper-lifecycle-8020
- [ ] Create sector-support-hooks-8020 (ontology + guards)

**Ownership**: @planner + @coder + domain experts
**Success Criteria**: Each bundle passes Guard8020Coverage, shows dark matter targets

---

### Week 4: Validation & Launch
- [ ] Emit and validate signed receipts
- [ ] Document all dark matter targets with measurement plans
- [ ] Update marketplace UI to surface 8020 packages
- [ ] Create sector-specific examples and getting-started guides

**Ownership**: @reviewer + @documentation
**Success Criteria**: Users can install sector bundles, see validation receipts, understand dark matter reduction

---

## Part 5: Strategic Outcomes

### What This Achieves

**For Small Teams**:
- Install 8020 bundles, get "80% of working system" out of the box
- No need to understand ggen's internals; just follow the bundle's getting-started guide
- Trust signals (validation receipt, dark matter target) make choice deterministic

**For Large Orgs**:
- ggen becomes the single source of truth for domain O
- All code, docs, telemetry, policies derived from O
- Dark work measured and optimized at platform layer
- New services inherit all patterns automatically

**For the Platform**:
- 8020 packages become the "hero" content (featured in marketplace)
- Sector bundles increase time-to-productivity from days to hours
- Validation receipts make quality objectively measurable
- Dark matter reduction targets become KPIs (measure platform success)

---

## Part 6: Appendix

### 6.1 Package Structure Reference

```
sector-observability-8020/
├── package.toml                 # Metadata (name, version, is_8020, dark_matter_target)
├── ggen.toml                    # Lifecycle & AI config
├── make.toml                    # Build phases
├── README.md                    # User guide
├── ontologies/
│   └── observability_v1.0.0.ttl # OTEL + SLO models
├── templates/
│   ├── otel-config.tmpl         # OTEL configuration
│   ├── weaver-registry.tmpl     # Metric registration
│   └── slo-dashboard.tmpl       # SLO dashboard (Grafana)
├── guards/
│   └── telemetry_complete.rs    # GuardTelemetryComplete
├── examples/
│   ├── microservice-instrumentation/
│   ├── database-monitoring/
│   └── trace-to-metrics-mapping/
├── tests/
│   ├── unit/
│   └── integration/
└── src/ (if applicable)
```

---

### 6.2 Dark Matter/Energy Glossary

| Term | Definition | ggen's Answer |
|------|-----------|---|
| **Dark Matter** | Invisible scaffolding work (copying structures, reformatting, syncing) | O-first + projections eliminate it |
| **Dark Energy** | Continuous drag (keeping docs in sync, re-onboarding projects) | Delta-driven projection + bundles eliminate it |
| **O** | Domain ontology (RDF/OWL) | Explicit, version-controlled, queryable |
| **μ** | Projection system (ggen's generators) | SPARQL-driven, deterministic, auditable |
| **Π** | Projection family (e.g., Π_models, Π_apis, Π_docs) | Templates with SPARQL frontmatter |
| **Sector Bundle** | Vertical stack (O + Π + guards for one domain) | sector-observability-8020, etc. |
| **8020 Package** | A bundle that covers 80% of real-world usage with minimal hand-editing | Marked with is_8020 flag, passes Guard8020Coverage |
| **Validation Receipt** | Signed proof that template→code meets quality standards | JSON artifact, cryptographically signed |
| **Dark Matter Reduction** | Measurable claim about % of manual work eliminated | e.g., "observability bundle = 70% reduction" |

---

### 6.3 Existing Proof Points

- ✅ `advanced-rust-api-8020`: 85% production readiness, proven marketplace package
- ✅ `academic-paper_v1.0.0.ttl`: Ontology-driven paper generation (LaTeX templates work)
- ✅ RDF/SPARQL core: 610 files, real triple store, working projections
- ✅ 76 marketplace packages: Real packages, real users, scoring system in place

**What's been proven**: The pieces work individually. The roadmap integrates them into a coherent "dark matter elimination" system.

---

## Conclusion

The 80/20 roadmap is **not about adding new technology**; it's about **composing and surfacing** what ggen already has:

1. **Define sector bundles** as first-class marketplace artifacts
2. **Add 8020 guards** to make quality signals explicit and measurable
3. **Attach dark matter reduction targets** to every package (claims + measurement approach)
4. **Surface in CLI/marketplace** so users can find and install the critical 20% that delivers 80% value

**Result**: ggen goes from "powerful but hard to start with" → "install a sector bundle, get 80% of a working system."

That's the dark matter/energy 80/20 move.

---

**Document Version**: 1.0
**Created**: 2025-11-16
**Branch**: claude/ggen-8020-roadmap-011tUBxZ8opuu5rNgzCMBvyP
**Next Step**: Phase 1 (Infrastructure) — Start week 1 implementation
