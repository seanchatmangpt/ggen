# CodeManufactory Skills & Manufacturing Capabilities

**Version:** 26.5.5  
**Last Updated:** 2026-05-05  
**Product Doctrine:** The product is CodeManufactory; RevOps is merely proof that CodeManufactory works.

---

## Executive Summary

CodeManufactory manufactures artifacts (code, specifications, proofs) from RDF ontologies through a deterministic five-stage pipeline (μ₁–μ₅). This document catalogs all manufacturing skills available in the system.

**Key Formula:** `A = μ(O)` — Artifacts precipitate from RDF via five-stage pipeline.

---

## Manufacturing Pipeline Skills

### Core Manufacturing: `ggen sync`

**Skill:** Full specification-driven code generation from RDF ontologies.

**Stages:**
1. **μ₁ (Load)** — Read RDF ontology from `.specify/` directory
2. **μ₂ (Extract)** — Extract patterns, rules, and templates
3. **μ₃ (Generate)** — Render code via Tera templates with SPARQL bindings
4. **μ₄ (Validate)** — Run quality gates (SHACL, compiler, tests)
5. **μ₅ (Emit)** — Write artifacts and cryptographic receipts

**Usage:**
```bash
ggen sync                    # Full manufacturing run with dry-run preview
ggen sync --dry-run false    # Real artifact emission
ggen sync --audit true       # Generate cryptographic receipts and audit chain
ggen sync --locked           # Verify pack digest chain before manufacturing
```

**Output:** Generated source files, receipts (`.ggen/receipts/*.json`), audit logs

**Test Cases:** RevOps (B2B revenue operations pipeline)

---

## Initialization & Onboarding Skills

### Project Initialization: `ggen init`

**Skill:** Bootstrap a new CodeManufactory project with RDF structure, ggen.toml, and templates.

**Features:**
- Interactive project setup
- Create `.specify/` directory structure
- Generate baseline `ggen.toml` with defaults
- Initialize RDF ontology seed files
- Set up CI/CD hooks

**Usage:**
```bash
ggen init                    # Interactive guided setup
ggen init --name myproject   # Non-interactive with project name
```

**Output:** `.specify/`, `ggen.toml`, baseline ontology files

---

### Guided Onboarding: `ggen wizard`

**Skill:** Step-by-step manufacturing walkthrough for first-time users.

**Features:**
- RDF ontology design guidance
- Template variable binding
- Quality gate configuration
- Receipt chain verification

**Usage:**
```bash
ggen wizard                  # Start interactive wizard
```

---

## Capability & Specification Skills

### Capability Introspection: `ggen capability`

**Skill:** Query and verify manufacturing capabilities against RDF ontologies.

**Verbs:**
- `ggen capability list` — Show all registered capabilities
- `ggen capability show <name>` — Display capability definition
- `ggen capability validate <spec>` — Verify capability conformance
- `ggen capability invoke <name>` — Call a capability operation
- `ggen capability trace <name>` — Show capability execution trace

**Features:**
- Real-time capability verification
- OpenTelemetry tracing
- Proof object generation

---

### SPARQL Construct Queries: `ggen construct`

**Skill:** Execute SPARQL CONSTRUCT queries against loaded RDF ontologies.

**Verbs:**
- `ggen construct run <query>` — Execute a CONSTRUCT query
- `ggen construct compile <file>` — Compile query from file
- `ggen construct validate <query>` — Validate SPARQL syntax

**Features:**
- Derive new RDF triples from ontology patterns
- Power template variable binding in μ₃
- Enable declarative code generation rules

**Example:**
```bash
ggen construct run 'CONSTRUCT { ?s rdfs:label ?label } WHERE { ?s a gv6:Entity ; rdfs:label ?label }'
```

---

## Quality & Validation Skills

### Health Diagnostics: `ggen doctor`

**Skill:** Comprehensive workspace health assessment and recovery guidance.

**Verbs:**
- `ggen doctor run` — Full health check (ggen.toml, binary, workspace, toolchain)
- `ggen doctor check` — Quick validation (ggen.toml presence)
- `ggen doctor config` — Scan ggen.toml for issues
- `ggen doctor ontology` — Validate RDF ontology structure
- `ggen doctor telemetry` — Verify OpenTelemetry collector connectivity
- `ggen doctor registry` — Check marketplace registry health
- `ggen doctor security` — Scan for exposed secrets, dangerous permissions
- `ggen doctor publish` — Pre-release validation gate

**Output:** JSON with per-check details, recovery guidance, exit codes

**Gateway:** Blocks release if critical checks fail

---

### Receipt Chain Verification: `ggen packs receipt`

**Skill:** Cryptographic proof-of-manufacturing via Ed25519-signed receipts.

**Verbs:**
- `ggen packs receipt verify` — Validate receipt signature and chain
- `ggen packs receipt show <id>` — Display receipt details
- `ggen packs receipt list` — List all manufacturing receipts

**Features:**
- Ed25519 signature verification
- BLAKE3 hash chain validation
- Pack digest integrity checks
- Tamper detection

**Output:** `is_valid: true/false`, tampering evidence if invalid

---

## Marketplace & Package Management Skills

### Pack Management: `ggen packs` / `ggen pack`

**Skill:** Manage reusable, versioned, cryptographically-signed code packages.

**Verbs:**
- `ggen pack add <pack>` — Install a pack from marketplace
- `ggen packs list` — Show installed packs
- `ggen packs remove <pack>` — Uninstall a pack
- `ggen packs update <pack>` — Upgrade pack version
- `ggen packs lock` — Generate `.ggen/packs.lock` with digest chain
- `ggen packs verify` — Validate pack integrity and signatures

**Features:**
- Locked dependency chain (`.ggen/packs.lock`)
- Package digest verification
- Version pinning for reproducibility
- Offline-first pack resolution

**Output:** `packs.lock` (lockfile with SHA-256 digests per pack)

---

### Marketplace Registry: `ggen marketplace` / `ggen registry`

**Skill:** Query and manage the package marketplace with RDF store backend.

**Marketplace Verbs:**
- `ggen marketplace search <query>` — Find packages by name/keyword
- `ggen marketplace info <pkg>` — Show package metadata
- `ggen marketplace install <pkg>` — Install from marketplace
- `ggen marketplace list` — List installed packages
- `ggen marketplace publish <pkg>` — Publish a package (requires auth)
- `ggen marketplace quality <pkg>` — Show quality score

**Registry Verbs:**
- `ggen registry validate` — Check local marketplace registry
- `ggen registry sync` — Update registry from remote
- `ggen registry search <query>` — Query RDF-backed registry

**Features:**
- SPARQL-based package search
- Quality scoring (fitness, precision, simplicity)
- RDF metadata store
- Cryptographic package signing

---

## Specification & Ontology Skills

### Ontology Management: `ggen ontology`

**Skill:** Create, validate, and manage RDF ontologies that drive manufacturing.

**Verbs:**
- `ggen ontology create <name>` — Bootstrap new ontology
- `ggen ontology validate <file>` — Check TTL syntax and SHACL conformance
- `ggen ontology import <url>` — Import external ontology
- `ggen ontology show <class>` — Display entity or class definition
- `ggen ontology query <sparql>` — Execute SPARQL query

**Features:**
- TTL (Turtle) format support
- SHACL shape validation
- Automatic consistency checking
- Ontology imports and merging

---

### Graph Operations: `ggen graph`

**Skill:** Visualize and analyze RDF graphs and entity relationships.

**Verbs:**
- `ggen graph show <entity>` — Visualize entity connections
- `ggen graph analyze` — Compute graph metrics (density, centrality)
- `ggen graph diff <old> <new>` — Compare graph versions
- `ggen graph export <format>` — Export to GraphML, DOT, or JSON

**Features:**
- Mermaid diagram generation
- Graph statistics
- Entity relationship mapping
- Circular dependency detection

---

## Template & Code Generation Skills

### Template Management: `ggen template`

**Skill:** Manage Tera templates that render artifacts in μ₃ stage.

**Verbs:**
- `ggen template list` — Show all registered templates
- `ggen template show <name>` — Display template content
- `ggen template validate <file>` — Check Tera syntax
- `ggen template test <name> <context>` — Dry-run template with sample context
- `ggen template compile <file>` — Pre-compile template for performance

**Features:**
- Tera template engine integration
- Variable binding via SPARQL
- Conditional rendering
- Loop support for list generation

---

## Workflow Orchestration Skills

### Workflow Management: `ggen workflow`

**Skill:** Define and execute multi-step manufacturing processes.

**Verbs:**
- `ggen workflow create <name>` — Define new workflow
- `ggen workflow list` — Show registered workflows
- `ggen workflow run <name>` — Execute workflow with inputs
- `ggen workflow validate <file>` — Check workflow YAML syntax
- `ggen workflow trace <id>` — Show workflow execution trace

**Features:**
- Declarative workflow definitions
- Step dependencies
- Error recovery and retry logic
- OpenTelemetry tracing per step

---

## Documentation & Paper Generation Skills

### Paper Generation: `ggen paper`

**Skill:** Generate human-readable documentation and analysis reports from ontologies.

**Verbs:**
- `ggen paper generate <type>` — Create documentation artifact
- `ggen paper template <type>` — Show template for doc type
- `ggen paper validate <file>` — Check documentation structure

**Supported Types:**
- `design` — Architecture and design rationale
- `api` — API reference documentation
- `ontology` — Ontology specification document
- `manufacturing` — Manufacturing process description
- `audit` — Compliance and audit trail

---

## Policy & Governance Skills

### Policy Management: `ggen policy`

**Skill:** Define and enforce manufacturing policies and validation constraints.

**Verbs:**
- `ggen policy create <name>` — Define new policy
- `ggen policy list` — Show all policies
- `ggen policy validate <artifact>` — Check artifact against policies
- `ggen policy enforce <name>` — Enable strict policy enforcement
- `ggen policy audit <artifact>` — Generate compliance report

**Features:**
- SHACL-based shape constraints
- Custom validation rules
- Compliance scoring
- Policy inheritance

---

## Semantic OS & Advanced Skills

### Semantic OS Operations: `ggen semantic-os`

**Skill:** Manage SemanticOS integration and distributed semantic computing.

**Verbs:**
- `ggen semantic-os deploy <config>` — Deploy semantic OS instance
- `ggen semantic-os health` — Check SemanticOS health
- `ggen semantic-os query <sparql>` — Execute distributed SPARQL
- `ggen semantic-os sync` — Synchronize local ontology with SemanticOS

**Features:**
- Distributed RDF store
- Federated SPARQL queries
- Semantic caching
- Cross-system knowledge graphs

---

### Self-Play & AI Learning: `ggen self-play`

**Skill:** Train manufacturing optimization models via self-play and bootstrapping.

**Verbs:**
- `ggen self-play train <domain>` — Start self-play training loop
- `ggen self-play evaluate <model>` — Benchmark trained model
- `ggen self-play import <model>` — Load external model
- `ggen self-play metrics <run>` — Show training metrics

**Features:**
- LLM-powered code generation optimization
- Bootstrapping from weak signals
- Continuous model improvement
- A/B testing support

---

## Infrastructure & CI/CD Skills

### Git Hooks Management: `ggen git-hooks`

**Skill:** Install and manage pre-commit and pre-push validation hooks.

**Verbs:**
- `ggen git-hooks install` — Install quality gates as git hooks
- `ggen git-hooks uninstall` — Remove hooks
- `ggen git-hooks validate` — Test hooks locally

**Hooks Installed:**
- `pre-commit` — Run: cargo check, cargo test, cargo lint
- `pre-push` — Run: full test suite with 300s timeout

---

### CI Integration: `ggen ci`

**Skill:** Generate CI/CD pipeline configurations for GitHub Actions, GitLab CI, etc.

**Verbs:**
- `ggen ci generate <platform>` — Create CI config
- `ggen ci validate <file>` — Check CI syntax
- `ggen ci show <platform>` — Display platform-specific config

**Supported Platforms:**
- GitHub Actions
- GitLab CI
- Jenkins
- CircleCI

---

### Telco (Telemetry) Operations: `ggen telco`

**Skill:** Configure and manage OpenTelemetry observability pipeline.

**Verbs:**
- `ggen telco setup <backend>` — Configure tracing backend (Jaeger, Tempo)
- `ggen telco export <format>` — Export traces (JSON, protobuf)
- `ggen telco validate` — Verify tracing connectivity
- `ggen telco show-config` — Display telemetry configuration

---

## Envelope & Encoding Skills

### Envelope Operations: `ggen envelope`

**Skill:** Manage cryptographic envelopes for secure artifact distribution.

**Verbs:**
- `ggen envelope create <artifact>` — Wrap artifact in envelope
- `ggen envelope seal <artifact>` — Sign and encrypt
- `ggen envelope open <envelope>` — Verify and decrypt
- `ggen envelope validate <envelope>` — Check envelope integrity

**Features:**
- Ed25519 signatures
- HMAC-based integrity
- Encryption support
- Tamper detection

---

## Utility Skills

### Miscellaneous Utilities: `ggen utils`

**Skill:** General-purpose utility operations.

**Verbs:**
- `ggen utils version` — Show ggen version
- `ggen utils config get <key>` — Read config value
- `ggen utils config set <key> <value>` — Update config
- `ggen utils health` — Quick health check
- `ggen utils describe <artifact>` — Analyze artifact structure

---

## Quality Metrics & Proof Framework

All manufacturing skills are validated against **14 proof gates**:

| Gate | Skill | Validates |
|------|-------|-----------|
| 1 | MANIFEST_PRESENT | ggen.toml exists and parses |
| 2 | OBSERVABILITY_PRESENT | OTEL spans emitted for all ops |
| 3 | RDF_LOADABLE | Ontology loads without errors |
| 4 | SPARQL_QUERYABLE | SPARQL queries execute on ontology |
| 5 | OWL_CONSISTENCY | Ontology passes OWL consistency checks |
| 6 | SHACL_CONFORMANCE | Artifacts conform to SHACL shapes |
| 7 | DETERMINISTIC_OUTPUT | Identical inputs → identical outputs |
| 8 | RECEIPT_VERIFIED | Cryptographic receipts valid |
| 9 | CROSS_SYSTEM_CAUSALITY | Causal links proven via OTEL |
| 10 | PROCESS_CONFORMANCE | Actual process matches declared model |
| 11 | VARIANT_EXPLOSION | Execution variants within bounds |
| 12 | TEMPORAL_LAWFULNESS | Events in lawful causal order |
| 13 | ARTIFACT_IMMUTABILITY | Receipts prove artifacts unchanged |
| 14 | GATEWAY_PASS | All gates pass before release |

---

## Test Cases (RevOps)

CodeManufactory proves itself via the **RevOps test case** — a complete B2B revenue operations manufacturing pipeline demonstrating:

- **μ₁ (Load):** Load B2B revenue ontology from `.specify/revenue/`
- **μ₂ (Extract):** Extract deal stages, forecast models, quota allocation rules
- **μ₃ (Generate):** Render Elixir/Go microservices for sales, forecasting, compensation
- **μ₄ (Validate):** Run SHACL gates on generated code; verify no circular logic in quota
- **μ₅ (Emit):** Write services, generate receipts, publish to marketplace

---

## Performance SLOs

All skills must meet these service-level objectives:

| SLO | Target | Validation |
|-----|--------|-----------|
| First build | ≤15s | `cargo make slo-check` |
| Incremental build | ≤2s | `cargo make slo-check` |
| RDF processing | ≤5s/1k triples | Benchmark suite |
| CLI startup | ≤500ms | Integration tests |
| Generation memory | ≤100MB | Profiling |
| Reproducibility | 100% | Hash verification |

---

## Definition of Done (DoD)

Every skill manufacturing run must satisfy:

```bash
cargo make check          # ✅ No compiler errors
cargo make test           # ✅ All tests pass (Chicago TDD)
cargo make lint           # ✅ No warnings
cargo make slo-check      # ✅ All SLOs met
RUST_LOG=trace cargo test # ✅ OTEL spans verified
```

All 14 proof gates must pass before artifact release.

---

## Getting Started

1. **Initialize a project:**
   ```bash
   ggen init --name myproject
   ```

2. **Create RDF ontology:**
   ```bash
   mkdir -p .specify/specs/001-myfeature
   vim .specify/specs/001-myfeature/feature.ttl
   ggen ontology validate .specify/specs/001-myfeature/feature.ttl
   ```

3. **Manufacture artifacts:**
   ```bash
   ggen sync --dry-run true      # Preview
   ggen sync --audit true        # Real manufacturing with receipts
   ```

4. **Verify proof gates:**
   ```bash
   ggen doctor run                # Full health check
   ggen packs receipt verify      # Check receipt chain
   ```

5. **Publish to marketplace:**
   ```bash
   ggen marketplace publish mypackage@1.0.0
   ```

---

## References

- **Core Formula:** `A = μ(O)` — Artifacts from RDF ontologies
- **Pipeline:** Five-stage deterministic transformation (μ₁–μ₅)
- **Doctrine:** CodeManufactory is the product; RevOps proves it works
- **Proof:** 14-gate framework validates every manufacturing run
- **Testing:** Chicago TDD (real collaborators, no mocks)
- **Standard:** OpenTelemetry observability throughout

**Repository:** https://github.com/seanchatmangpt/ggen

---

**Version 26.5.5 — Last Updated 2026-05-05**
