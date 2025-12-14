<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Diataxis User Documentation Plan](#ggen-diataxis-user-documentation-plan)
  - [📊 Current State Analysis](#-current-state-analysis)
    - [✅ What Exists (Internal Development Focus)](#-what-exists-internal-development-focus)
    - [⚠️ What's Missing (End-User Adoption Blockers)](#-whats-missing-end-user-adoption-blockers)
  - [🎯 Diataxis User Documentation Strategy](#-diataxis-user-documentation-strategy)
    - [User Journey Map](#user-journey-map)
  - [📖 1. TUTORIALS (Learning-Oriented)](#-1-tutorials-learning-oriented)
    - [Priority 1: Essential Learning Paths (MUST HAVE)](#priority-1-essential-learning-paths-must-have)
      - [T1: Quick Start - Your First Code Generation ⭐⭐⭐](#t1-quick-start---your-first-code-generation-)
      - [T2: RDF to Rust API - Complete Project ⭐⭐⭐](#t2-rdf-to-rust-api---complete-project-)
      - [T3: Custom Template Creation ⭐⭐](#t3-custom-template-creation-)
    - [Priority 2: Advanced Workflows (NICE TO HAVE)](#priority-2-advanced-workflows-nice-to-have)
      - [T4: SPARQL-Powered Code Generation ⭐](#t4-sparql-powered-code-generation-)
      - [T5: Multi-Language Project Scaffolding ⭐](#t5-multi-language-project-scaffolding-)
      - [T6: Marketplace Package Integration ⭐](#t6-marketplace-package-integration-)
  - [🛠️ 2. HOW-TO GUIDES (Task-Oriented)](#-2-how-to-guides-task-oriented)
    - [Priority 1: Core Tasks (80% of use cases)](#priority-1-core-tasks-80-of-use-cases)
      - [H1: Generate TypeScript Types from Schema.org ⭐⭐⭐](#h1-generate-typescript-types-from-schemaorg-)
      - [H2: Create a CLI Tool with Clap-Noun-Verb ⭐⭐⭐](#h2-create-a-cli-tool-with-clap-noun-verb-)
      - [H3: Query RDF Data with SPARQL ⭐⭐⭐](#h3-query-rdf-data-with-sparql-)
      - [H4: Add New Template to Existing Project ⭐⭐](#h4-add-new-template-to-existing-project-)
    - [Priority 2: Advanced Tasks](#priority-2-advanced-tasks)
      - [H5: Integrate AI Code Generation ⭐](#h5-integrate-ai-code-generation-)
      - [H6: Set Up Lifecycle Hooks ⭐](#h6-set-up-lifecycle-hooks-)
      - [H7: Deploy Generated Code to Production ⭐](#h7-deploy-generated-code-to-production-)
      - [H8: Troubleshoot Generation Errors ⭐](#h8-troubleshoot-generation-errors-)
  - [💡 3. EXPLANATIONS (Understanding-Oriented)](#-3-explanations-understanding-oriented)
    - [Priority 1: Core Concepts (Essential Understanding)](#priority-1-core-concepts-essential-understanding)
      - [E1: What is Ontology-Driven Development? ⭐⭐⭐](#e1-what-is-ontology-driven-development-)
      - [E2: How Templates Work ⭐⭐⭐](#e2-how-templates-work-)
      - [E3: RDF for Programmers (No Prior Knowledge Required) ⭐⭐⭐](#e3-rdf-for-programmers-no-prior-knowledge-required-)
      - [E4: Deterministic Generation Philosophy ⭐⭐](#e4-deterministic-generation-philosophy-)
    - [Priority 2: Advanced Concepts](#priority-2-advanced-concepts)
      - [E5: SPARQL Query Patterns ⭐](#e5-sparql-query-patterns-)
      - [E6: Type Mapping Strategies ⭐](#e6-type-mapping-strategies-)
      - [E7: Marketplace Architecture ⭐](#e7-marketplace-architecture-)
  - [📚 4. REFERENCE (Information-Oriented)](#-4-reference-information-oriented)
    - [Priority 1: Command Reference (Complete Documentation)](#priority-1-command-reference-complete-documentation)
      - [R1: ggen Command Reference ⭐⭐⭐](#r1-ggen-command-reference-)
      - [R3: SPARQL Cookbook ⭐⭐⭐](#r3-sparql-cookbook-)
    - [Priority 2: Additional Reference](#priority-2-additional-reference)
      - [R5: RDF Vocabulary Reference ⭐](#r5-rdf-vocabulary-reference-)
      - [R6: Template Directory Reference ⭐](#r6-template-directory-reference-)
      - [R7: Configuration File Reference ⭐](#r7-configuration-file-reference-)
  - [🚀 Implementation Plan (80/20 Priority)](#-implementation-plan-8020-priority)
    - [Phase 1: Minimum Viable Documentation (Week 1) ⭐⭐⭐](#phase-1-minimum-viable-documentation-week-1-)
    - [Phase 2: Complete Core Workflows (Week 2) ⭐⭐](#phase-2-complete-core-workflows-week-2-)
    - [Phase 3: Advanced Features (Week 3) ⭐](#phase-3-advanced-features-week-3-)
  - [📁 Documentation File Structure](#-documentation-file-structure)
  - [📊 Success Metrics](#-success-metrics)
    - [User Adoption Metrics](#user-adoption-metrics)
    - [Documentation Quality Metrics](#documentation-quality-metrics)
  - [🎯 Maintenance Plan](#-maintenance-plan)
    - [Continuous Improvement](#continuous-improvement)
    - [Documentation CI/CD](#documentation-cicd)
  - [🤝 Contribution Guidelines](#-contribution-guidelines)
    - [How to Add Documentation](#how-to-add-documentation)
  - [📝 Next Actions](#-next-actions)
    - [Immediate (This Week)](#immediate-this-week)
    - [Short-Term (Next 2 Weeks)](#short-term-next-2-weeks)
    - [Long-Term (Next 3 Months)](#long-term-next-3-months)
  - [✅ Definition of Done](#-definition-of-done)
  - [🔗 Related Resources](#-related-resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Diataxis User Documentation Plan
**Making ggen Usable: End-User Focused Documentation**

**Status:** Planning Phase
**Target Audience:** External users adopting ggen
**Complementary To:** Existing internal dev documentation (testing, lean, swarm)

---

## 📊 Current State Analysis

### ✅ What Exists (Internal Development Focus)

**Diataxis Structure Present:**
- `/docs/tutorials/` - 10 docs (marketplace, packs, ontology)
- `/docs/how-to-guides/` - 10 docs (installation, config, deployment)
- `/docs/reference/` - 8 docs (CLI, hooks, templates, SPARQL)
- `/docs/explanations/` - 7 docs (architecture, determinism, poka-yoke)

**Existing Internal Dev Docs:** (68% complete for internal use)
- Test refactoring tutorials
- Lean manufacturing guides
- Hive Mind swarm coordination
- Clap-noun-verb upgrade paths

### ⚠️ What's Missing (End-User Adoption Blockers)

**Critical Gaps:**
1. **No "First 5 Minutes" tutorial** - New users can't get started quickly
2. **Limited real-world examples** - Tutorials focus on internal concepts, not user workflows
3. **Incomplete command reference** - README has commands, but missing comprehensive flag documentation
4. **Conceptual knowledge scattered** - Users don't understand "ontology-driven development"
5. **No success path** - Users can install but don't know what to build

**User Pain Points (from README):**
- ✅ Can install via Homebrew/Cargo
- ❌ Don't know what to do after `ggen template list`
- ❌ Don't understand RDF/SPARQL (requires prior knowledge)
- ❌ Can't transition from example to custom project
- ❌ No troubleshooting guides for common errors

---

## 🎯 Diataxis User Documentation Strategy

**Goal:** Enable a complete beginner to generate production-ready code in 30 minutes

**Approach:** 80/20 Rule - 20% of docs cover 80% of use cases

### User Journey Map

```
NEW USER
  ↓
[Quick Start Tutorial] ← 5-10 min: First generation
  ↓
[Core Workflow How-Tos] ← 3-5 specific tasks
  ↓
[Concept Explanations] ← Understand RDF, ontologies, templates
  ↓
[Complete Reference] ← Look up specific commands/flags
  ↓
PRODUCTIVE USER
```

---

## 📖 1. TUTORIALS (Learning-Oriented)

**Philosophy:** "I want to learn by doing"
**Goal:** Confident competence in core workflows
**Metric:** User can generate their first code in 10 minutes

### Priority 1: Essential Learning Paths (MUST HAVE)

#### T1: Quick Start - Your First Code Generation ⭐⭐⭐
**Audience:** Complete beginners
**Time:** 10 minutes
**Outcome:** User generates TypeScript types from an ontology

**Steps:**
1. Install ggen via Homebrew
2. List available templates (`ggen template list`)
3. Create simple RDF ontology (3 classes, 5 properties)
4. Generate TypeScript types with `ggen ontology generate`
5. See working code in `output/` directory
6. Understand what happened (template + RDF → code)

**Success Criteria:** Runnable TypeScript file generated

#### T2: RDF to Rust API - Complete Project ⭐⭐⭐
**Audience:** Users who completed T1
**Time:** 25 minutes
**Outcome:** Working REST API from RDF ontology

**Steps:**
1. Design product catalog ontology (Product, Category, Inventory)
2. Write SPARQL queries for CRUD operations
3. Generate Rust API with `ggen project gen --template rust-api`
4. Run generated tests (`cargo test`)
5. Start API server and test endpoints
6. Understand template variables and customization

**Success Criteria:** API responds to HTTP requests with data from ontology

#### T3: Custom Template Creation ⭐⭐
**Audience:** Users who want to customize output
**Time:** 20 minutes
**Outcome:** Custom template that generates Python Pydantic models

**Steps:**
1. Clone existing `python.tmpl` template
2. Understand Tera template syntax
3. Add frontmatter variables (namespace, author)
4. Test template with `ggen template lint`
5. Generate code with custom template
6. Iterate on template (add type hints, docstrings)

**Success Criteria:** Custom template generates valid Python code

### Priority 2: Advanced Workflows (NICE TO HAVE)

#### T4: SPARQL-Powered Code Generation ⭐
**Time:** 30 minutes
**Outcome:** Use SPARQL queries to drive complex code generation

#### T5: Multi-Language Project Scaffolding ⭐
**Time:** 35 minutes
**Outcome:** Generate full-stack project (React + Rust + PostgreSQL)

#### T6: Marketplace Package Integration ⭐
**Time:** 15 minutes
**Outcome:** Install and use community templates from marketplace

---

## 🛠️ 2. HOW-TO GUIDES (Task-Oriented)

**Philosophy:** "I want to solve this specific problem NOW"
**Goal:** Quick solutions for common tasks
**Metric:** User solves their problem in 5 minutes

### Priority 1: Core Tasks (80% of use cases)

#### H1: Generate TypeScript Types from Schema.org ⭐⭐⭐
**Problem:** "I want type-safe TypeScript from Schema.org vocabulary"

**Steps:**
1. Download Schema.org Turtle file
2. Load into graph: `ggen graph load --file schema.ttl`
3. Query for specific types (Person, Product, etc.)
4. Extract ontology: `ggen ontology extract --ontology_file schema.ttl`
5. Generate types: `ggen ontology generate --language typescript`

**Output:** `types/schema-org.ts` with interfaces for Schema.org types

#### H2: Create a CLI Tool with Clap-Noun-Verb ⭐⭐⭐
**Problem:** "I want to scaffold a Rust CLI with auto-discovered commands"

**Steps:**
1. Initialize project: `ggen project init --name my-cli --preset clap-noun-verb`
2. Add commands in `cmds/` directory (auto-discovered)
3. Generate command boilerplate: `ggen template generate_tree`
4. Build and test: `cargo build && cargo test`

**Output:** Working Rust CLI with `my-cli noun verb` pattern

#### H3: Query RDF Data with SPARQL ⭐⭐⭐
**Problem:** "I need to extract specific data from my ontology"

**Steps:**
1. Load RDF: `ggen graph load --file ontology.ttl`
2. Write SPARQL query (examples provided)
3. Execute: `ggen graph query --sparql_query "SELECT..."`
4. Export results: `ggen graph export --format json`

**Output:** JSON data matching SPARQL query

#### H4: Add New Template to Existing Project ⭐⭐
**Problem:** "I want to add database migrations to my generated project"

**Steps:**
1. Create `migrations.tmpl` with SQL templates
2. Add frontmatter (db_type, migration_tool)
3. Run: `ggen project gen --template migrations.tmpl`
4. Customize output with `--vars` flags

**Output:** `migrations/` directory with SQL files

### Priority 2: Advanced Tasks

#### H5: Integrate AI Code Generation ⭐
**Problem:** "Use AI to generate templates/ontologies"

#### H6: Set Up Lifecycle Hooks ⭐
**Problem:** "Auto-format generated code with prettier/rustfmt"

#### H7: Deploy Generated Code to Production ⭐
**Problem:** "CI/CD pipeline for generated code"

#### H8: Troubleshoot Generation Errors ⭐
**Problem:** "My template fails to render"

---

## 💡 3. EXPLANATIONS (Understanding-Oriented)

**Philosophy:** "I want to understand the concepts and philosophy"
**Goal:** Mental models for effective use
**Metric:** User understands *why* ggen works this way

### Priority 1: Core Concepts (Essential Understanding)

#### E1: What is Ontology-Driven Development? ⭐⭐⭐
**Concept:** Define domain once in RDF, generate code multiple times

**Content:**
- Traditional approach: Write code in each language manually
- Ontology approach: Model domain in RDF/OWL, generate polyglot code
- Benefits: Single source of truth, consistency across languages
- Trade-offs: Learning curve for RDF, upfront modeling effort
- When to use: Multi-language projects, complex domains, API consistency

**Mental Model:** RDF = Domain Schema, Template = Code Recipe, Generation = Transformation

#### E2: How Templates Work ⭐⭐⭐
**Concept:** Tera template system with RDF variable injection

**Content:**
- Template anatomy: Frontmatter (metadata) + Body (Tera syntax)
- Variable resolution: RDF queries → template variables
- Control flow: `{% for %}`, `{% if %}`, filters
- Directory trees: Single template → multiple files
- Composition: Templates can include other templates

**Mental Model:** Template = Parameterized Code Mold, RDF = Parameter Values

#### E3: RDF for Programmers (No Prior Knowledge Required) ⭐⭐⭐
**Concept:** RDF as a graph database for domain modeling

**Content:**
- Triple pattern: Subject-Predicate-Object (like "User hasName 'Alice'")
- Why RDF: Language-agnostic, standardized, queryable with SPARQL
- Common vocabularies: Schema.org, Dublin Core, FOAF
- Tools: Protégé (editor), GraphDB (store), Oxigraph (ggen uses this)
- Practical example: Product catalog as RDF graph

**Mental Model:** RDF = Labeled Graph, Classes = Node Types, Properties = Edge Types

#### E4: Deterministic Generation Philosophy ⭐⭐
**Concept:** Same input → same output, always

**Content:**
- Why deterministic: Reproducibility, version control, testing
- How ggen achieves this: No randomness, sorted output, fixed algorithms
- Comparison: ggen vs. Yeoman/Plop (non-deterministic)
- Benefits: Git-friendly diffs, reliable CI/CD, cacheable builds

**Mental Model:** ggen = Pure Function (Input:RDF+Template → Output:Code)

### Priority 2: Advanced Concepts

#### E5: SPARQL Query Patterns ⭐
**Concept:** Common SPARQL queries for code generation

#### E6: Type Mapping Strategies ⭐
**Concept:** RDF types → TypeScript/Rust/Python types

#### E7: Marketplace Architecture ⭐
**Concept:** Package distribution and versioning

---

## 📚 4. REFERENCE (Information-Oriented)

**Philosophy:** "I need to look up this specific fact"
**Goal:** Scannable, comprehensive, indexed
**Metric:** User finds answer in 30 seconds

### Priority 1: Command Reference (Complete Documentation)

#### R1: ggen Command Reference ⭐⭐⭐
**Format:** Command → Subcommand → Flags

```markdown
# ggen template list

**Description:** List all available templates

**Usage:**
```bash
ggen template list [OPTIONS]
```

**Options:**
- `--format <json|table>` - Output format (default: json)
- `--source <local|marketplace>` - Filter by source

**Output:**
```json
{
  "templates": [...],
  "total": 22
}
```

**Examples:**
```bash
# List as table
ggen template list --format table

# Only local templates
ggen template list --source local
```

**See Also:** `ggen template show`, `ggen template new`
```

**Coverage:** ALL 50+ commands with full flag documentation

#### R2: Template Variable Reference ⭐⭐⭐
**Format:** Variable → Type → Example → Usage

```markdown
# Template Variables

## Frontmatter Variables

### `name` (String, required)
- **Description:** Project name
- **Example:** `name: "my-api"`
- **Used In:** File paths, package names

### `namespace` (String, optional)
- **Description:** RDF namespace prefix
- **Example:** `namespace: "http://example.org/"`
- **Used In:** RDF triple generation
```

**Coverage:** All 30+ template variables

#### R3: SPARQL Cookbook ⭐⭐⭐
**Format:** Use Case → Query → Explanation

```markdown
# SPARQL Queries for Code Generation

## Get All Classes

```sparql
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>

SELECT ?class ?label
WHERE {
  ?class rdf:type owl:Class .
  OPTIONAL { ?class rdfs:label ?label }
}
```

**Use Case:** Generate type definitions for each class
**Explanation:** Finds all OWL classes, optionally with human-readable labels
```

**Coverage:** 20+ common SPARQL patterns

#### R4: Error Code Reference ⭐⭐
**Format:** Error Code → Cause → Solution

```markdown
# Error Codes

## E001: Template Rendering Failed

**Cause:** Variable undefined in template
**Solution:** Add variable to frontmatter or RDF data
**Example:**
```
ERROR: Undefined variable 'namespace' in template
FIX: Add to frontmatter: `namespace: "http://example.org/"`
```
```

**Coverage:** All 15+ error codes

### Priority 2: Additional Reference

#### R5: RDF Vocabulary Reference ⭐
**Format:** Vocabulary → Classes → Properties → Examples

#### R6: Template Directory Reference ⭐
**Format:** Template → Description → Variables → Example Output

#### R7: Configuration File Reference ⭐
**Format:** Config Key → Type → Default → Description

---

## 🚀 Implementation Plan (80/20 Priority)

### Phase 1: Minimum Viable Documentation (Week 1) ⭐⭐⭐

**Goal:** User can go from install to first generated code in 10 minutes

**Deliverables:**
1. **T1: Quick Start Tutorial** (10 min read, 5 min do)
2. **H1: Generate TypeScript Types** (most common use case)
3. **H3: Query RDF with SPARQL** (essential skill)
4. **E1: Ontology-Driven Development** (conceptual foundation)
5. **E3: RDF for Programmers** (prerequisite knowledge)
6. **R1: Command Reference** (lookup for all commands)

**Success Metric:** New user completes first generation without external help

**Estimated Effort:** 16-20 hours
- T1: 4 hours (write + test + iterate)
- H1: 2 hours
- H3: 2 hours
- E1: 3 hours
- E3: 4 hours
- R1: 5 hours (comprehensive)

### Phase 2: Complete Core Workflows (Week 2) ⭐⭐

**Goal:** User can build real projects with ggen

**Deliverables:**
1. **T2: RDF to Rust API Tutorial**
2. **T3: Custom Template Creation Tutorial**
3. **H2: Create CLI Tool**
4. **H4: Add New Template**
5. **E2: How Templates Work**
6. **E4: Deterministic Generation**
7. **R2: Template Variable Reference**
8. **R3: SPARQL Cookbook**

**Success Metric:** User builds custom project end-to-end

**Estimated Effort:** 18-22 hours

### Phase 3: Advanced Features (Week 3) ⭐

**Goal:** User can leverage advanced ggen capabilities

**Deliverables:**
1. **T4-T6: Advanced tutorials**
2. **H5-H8: Advanced how-tos**
3. **E5-E7: Advanced explanations**
4. **R4-R7: Complete reference**

**Success Metric:** User customizes workflow for production use

**Estimated Effort:** 12-16 hours

---

## 📁 Documentation File Structure

**New directory organization:**

```
/docs
  /getting-started/              # Entry point for new users
    README.md                    # Navigation hub
    quick-start.md               # T1: 10-minute tutorial
    installation.md              # Platform-specific install
    first-generation.md          # Guided first-time experience

  /tutorials/                    # Learning paths (existing, expand)
    /core/                       # Essential workflows
      01-first-code-generation.md         # T1
      02-rdf-to-rust-api.md               # T2
      03-custom-template-creation.md      # T3
    /advanced/                   # Power user features
      04-sparql-powered-generation.md     # T4
      05-multi-language-scaffolding.md    # T5
      06-marketplace-integration.md       # T6
    /existing/                   # Keep existing internal dev tutorials
      (move current tutorials here)

  /how-to/                       # Task solutions (existing, expand)
    /generation/
      generate-typescript-types.md        # H1
      create-cli-tool.md                  # H2
      query-rdf-sparql.md                 # H3
      add-new-template.md                 # H4
    /integration/
      ai-code-generation.md               # H5
      lifecycle-hooks.md                  # H6
    /troubleshooting/
      fix-generation-errors.md            # H8
      common-issues.md
    /existing/
      (move current how-tos here)

  /explanations/                 # Concepts (existing, expand)
    /fundamentals/
      ontology-driven-development.md      # E1
      how-templates-work.md               # E2
      rdf-for-programmers.md              # E3
      deterministic-generation.md         # E4
    /advanced/
      sparql-patterns.md                  # E5
      type-mapping.md                     # E6
      marketplace-architecture.md         # E7
    /existing/
      (keep current explanations)

  /reference/                    # Lookup docs (existing, expand)
    /commands/
      template.md                         # ggen template commands
      graph.md                            # ggen graph commands
      ontology.md                         # ggen ontology commands
      project.md                          # ggen project commands
      ai.md                               # ggen ai commands
      complete-cli-reference.md           # R1: All commands
    /templates/
      variable-reference.md               # R2
      directory-tree-reference.md
      tera-syntax-reference.md
    /sparql/
      query-cookbook.md                   # R3
      patterns-by-use-case.md
    /errors/
      error-codes.md                      # R4
      troubleshooting-matrix.md
    /rdf/
      vocabulary-reference.md             # R5
    /existing/
      (keep current reference docs)

  /examples/                     # Keep existing 48 examples
    (no changes)
```

---

## 📊 Success Metrics

### User Adoption Metrics

**Installation to First Generation:**
- **Current:** Unknown (no tracking)
- **Target:** 80% of installers generate code within 24 hours
- **Measurement:** Telemetry (opt-in) or survey

**Documentation Usefulness:**
- **Current:** Unknown
- **Target:** 90% of users find answer in docs (not GitHub issues)
- **Measurement:** Issue labels ("answered-by-docs" vs "doc-gap")

**Time to Productivity:**
- **Current:** Unknown
- **Target:** 30 minutes from install to custom project
- **Measurement:** User study with 5-10 participants

### Documentation Quality Metrics

**Completeness:**
- Tutorial coverage: 3/3 core workflows documented
- How-to coverage: 4/4 essential tasks documented
- Reference coverage: 100% of commands documented

**Clarity:**
- External review: 3+ technical writers review content
- User testing: 5+ beginners test tutorials
- Feedback loop: Documentation issues → fixes within 1 week

**Discoverability:**
- Navigation: 3 clicks from README to any doc
- Search: Full-text search via mdBook or Docusaurus
- Cross-links: Every doc links to 3+ related docs

---

## 🎯 Maintenance Plan

### Continuous Improvement

**Weekly:**
- Monitor GitHub issues for documentation gaps
- Update docs when new features ship
- Fix broken links/examples

**Monthly:**
- User survey: "What docs are missing?"
- Metrics review: Track most-visited docs
- Update priority list based on usage data

**Quarterly:**
- Full audit: Test all tutorials end-to-end
- Refresh examples: Ensure compatibility with latest version
- Community contributions: Review and merge PRs

### Documentation CI/CD

**Automated Checks:**
```bash
# Run on every commit
npm run docs:lint          # Check markdown syntax
npm run docs:links         # Verify no broken links
npm run docs:examples      # Test code examples compile
npm run docs:screenshots   # Update outdated screenshots
```

**Manual Review:**
- Technical accuracy: Core team reviews
- Clarity: Non-expert reviewer tests
- Completeness: Coverage checklist verified

---

## 🤝 Contribution Guidelines

### How to Add Documentation

**1. Identify Diataxis Category:**
- **Tutorial:** Learning journey, hand-holds user
- **How-to:** Solve specific problem, assumes knowledge
- **Explanation:** Build mental model, theory + context
- **Reference:** Fact lookup, scannable lists

**2. Follow Template:**
```markdown
# [Title that matches user intent]

**Audience:** [Beginner | Intermediate | Advanced]
**Time:** [X minutes]
**Prerequisites:** [Links to required knowledge]

## What You'll Learn
- Learning objective 1
- Learning objective 2

## Steps
1. ...
2. ...

## Verify Success
[How user knows it worked]

## Next Steps
- [Link to related tutorial]
- [Link to deeper how-to]

## Troubleshooting
[Common errors and solutions]
```

**3. Cross-Link:**
- Link to 3+ related docs
- Add to navigation hub
- Update README if necessary

---

## 📝 Next Actions

### Immediate (This Week)

1. **Stakeholder Review:** Get feedback on this plan from core team
2. **Prioritize Phase 1:** Confirm T1, H1, H3, E1, E3, R1 are correct priorities
3. **Assign Ownership:** Who writes each doc? (may be AI-assisted with human review)
4. **Set Timeline:** Commit to Phase 1 completion date

### Short-Term (Next 2 Weeks)

1. **Create Phase 1 Docs:** Write + test + iterate on 6 core documents
2. **User Test:** Have 3-5 external users try Quick Start tutorial
3. **Iterate:** Fix gaps discovered during user testing
4. **Publish:** Make Phase 1 docs visible in main README

### Long-Term (Next 3 Months)

1. **Complete Phases 2-3:** Build out comprehensive documentation
2. **Integrate Tooling:** Set up mdBook or Docusaurus for better navigation
3. **Community Engagement:** Accept documentation PRs from community
4. **Metrics Dashboard:** Track documentation usage and effectiveness

---

## ✅ Definition of Done

**Phase 1 is complete when:**
- [ ] New user completes Quick Start in 10 minutes
- [ ] All 6 Phase 1 docs published and linked from README
- [ ] 5 external users test docs successfully
- [ ] No P0/P1 documentation issues open
- [ ] All code examples tested and working
- [ ] Cross-links verified (no broken links)
- [ ] Navigation hub created and accessible

**Full plan is complete when:**
- [ ] All 21+ tutorials/how-tos/explanations/references written
- [ ] 100% command coverage in reference docs
- [ ] User adoption metrics show 80%+ success rate
- [ ] Documentation CI/CD automated
- [ ] Community contributing documentation PRs
- [ ] GitHub issues show declining "documentation needed" labels

---

## 🔗 Related Resources

**Existing Documentation:**
- [README.md](../README.md) - Installation and quick start
- [Diataxis Internal Dev Docs](./diataxis-index.md) - Testing, lean, swarm
- [Examples Directory](../examples/) - 48 working code examples

**Diataxis Framework:**
- [Diataxis Website](https://diataxis.fr/) - Documentation methodology
- [Divio Blog](https://www.divio.com/blog/documentation/) - Framework rationale

**Similar Projects:**
- [Rust Book](https://doc.rust-lang.org/book/) - Tutorial + reference style
- [Next.js Docs](https://nextjs.org/docs) - Excellent how-to organization
- [Stripe Docs](https://stripe.com/docs) - API reference done right

---

**Last Updated:** 2025-12-11
**Plan Status:** DRAFT - Awaiting core team review
**Owner:** ggen Documentation Team
**Feedback:** Open GitHub issue with "documentation" label
