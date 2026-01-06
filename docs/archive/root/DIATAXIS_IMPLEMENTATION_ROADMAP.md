<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Diataxis Implementation Roadmap](#diataxis-implementation-roadmap)
  - [Phase 1: Critical Path Unblocking (Weeks 1-2)](#phase-1-critical-path-unblocking-weeks-1-2)
    - [1.1 Create `how-to-guides/configure-llm.md`](#11-create-how-to-guidesconfigure-llmmd)
    - [1.2 Create `tutorials/marketplace-quick-start.md`](#12-create-tutorialsmarketplace-quick-startmd)
    - [1.3 Create `explanations/rdf-for-beginners.md`](#13-create-explanationsrdf-for-beginnersmd)
    - [1.4 Reorganize Root-Level Documentation](#14-reorganize-root-level-documentation)
    - [1.5 Update `docs/README.md` with Decision Tree](#15-update-docsreadmemd-with-decision-tree)
  - [Phase 2: High-Priority Enhancements (Weeks 2-4)](#phase-2-high-priority-enhancements-weeks-2-4)
    - [2.1 Create `tutorials/path-selector.md`](#21-create-tutorialspath-selectormd)
    - [2.2 Update All Tutorials with First Success Checkpoints](#22-update-all-tutorials-with-first-success-checkpoints)
    - [2.3 Create `tutorials/rest-api-quickstart.md`](#23-create-tutorialsrest-api-quickstartmd)
    - [2.4 Create `reference/marketplace-metadata.md`](#24-create-referencemarketplace-metadatamd)
    - [2.5 Resolve `/docs/src/` Documentation Duplication](#25-resolve-docssrc-documentation-duplication)
  - [Phase 3: Medium-Priority Improvements (Weeks 4-8)](#phase-3-medium-priority-improvements-weeks-4-8)
    - [3.1 Create `explanations/ggen-vs-alternatives.md`](#31-create-explanationsggen-vs-alternativesmd)
    - [3.2 Rewrite Explanations with Multi-Level Depth](#32-rewrite-explanations-with-multi-level-depth)
    - [3.3 Update `how-to-guides/troubleshoot.md` with Decision Trees](#33-update-how-to-guidestroubleshootmd-with-decision-trees)
    - [3.4 Add Real-World Examples Throughout Documentation](#34-add-real-world-examples-throughout-documentation)
    - [3.5 Create `explanations/architecture-decisions.md`](#35-create-explanationsarchitecture-decisionsmd)
  - [Phase 4: Nice-to-Have Enhancements (Weeks 8-12)](#phase-4-nice-to-have-enhancements-weeks-8-12)
    - [4.1 Create Video Content](#41-create-video-content)
    - [4.2 Create Interactive Tutorial Mode](#42-create-interactive-tutorial-mode)
    - [4.3 Create Official Template Repository Examples](#43-create-official-template-repository-examples)
    - [4.4 Documentation Search Optimization](#44-documentation-search-optimization)
  - [Success Metrics](#success-metrics)
    - [Phase 1 (Weeks 1-2)](#phase-1-weeks-1-2)
    - [Phase 2 (Weeks 2-4)](#phase-2-weeks-2-4)
    - [Phase 3 (Weeks 4-8)](#phase-3-weeks-4-8)
    - [Phase 4 (Weeks 8-12)](#phase-4-weeks-8-12)
  - [Risk Mitigation](#risk-mitigation)
    - [Risk: Documentation becomes stale](#risk-documentation-becomes-stale)
    - [Risk: New docs conflict with existing ones](#risk-new-docs-conflict-with-existing-ones)
    - [Risk: Implementation team gets sidetracked](#risk-implementation-team-gets-sidetracked)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Diataxis Implementation Roadmap

**Objective:** Execute recommendations from DIATAXIS_EVALUATION_EXTERNAL_PERSPECTIVE.md to improve new user onboarding success rate from 31% → 75%+

**Timeline:** 8-12 weeks (phased approach)

---

## Phase 1: Critical Path Unblocking (Weeks 1-2)

These four documents unblock 40% of new user failures and take minimal time to create.

### 1.1 Create `how-to-guides/configure-llm.md`

**Purpose:** Unblock AI-powered generation feature

**Sections:**
1. Supported LLMs (GPT-4o, Claude, Ollama, local models)
2. API Key Setup (with provider links)
   - OpenAI (GPT-4o)
   - Anthropic (Claude)
   - Ollama (local)
3. Configuration methods
   - Environment variables
   - Config file (~/.ggen/config.toml)
   - CLI flags
4. Cost comparison
5. Testing your setup
6. Troubleshooting common errors

**Success Criteria:** User can run `ggen generate --ai "describe a todo app"` without errors

**Time Estimate:** 4 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 1.2 Create `tutorials/marketplace-quick-start.md`

**Purpose:** Make marketplace feature actually usable

**Content:**
1. What is the ggen marketplace?
2. Finding templates
   - Browse marketplace (URL: ...)
   - Search for specific domains
   - Filter by language/license
3. Installing a template
   - `ggen add template <name>`
   - Verification after install
4. Using an installed template
   - In your project
   - Customizing for your domain
5. Publishing your first template
   - Preparing your template
   - Creating marketplace metadata
   - Publishing process
   - Marketplace review/approval
6. Template updates

**Success Criteria:** User can find, install, and use a template in < 15 minutes

**Time Estimate:** 6 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 1.3 Create `explanations/rdf-for-beginners.md`

**Purpose:** Remove RDF knowledge barrier for 60% of users

**Content:**
1. "Why RDF?" (problem it solves)
2. Analogies to familiar formats (JSON, YAML, databases)
3. RDF core concepts
   - Subject-Predicate-Object triples
   - Resources, literals, properties
   - Visual examples
4. Simple RDF example (build step-by-step)
   - Person with name, email, friends
   - Show raw RDF/XML, Turtle, JSON-LD
5. Why ggen uses RDF
   - Semantic clarity
   - Language-agnostic
   - Tool ecosystem
6. SPARQL basics
   - SELECT WHERE pattern
   - Filter, aggregate, limit
   - Common patterns
7. When NOT to use RDF (expectations setting)
8. Next steps (point to tutorials/ontology-to-code.md)

**Success Criteria:** Non-semantic developer understands "I can create RDF ontologies for code generation"

**Time Estimate:** 8 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 1.4 Reorganize Root-Level Documentation

**Purpose:** Reduce clutter; improve first impression

**Actions:**
1. Create `/docs/archive/` directory
2. Move 45+ analysis files to `/docs/archive/analysis/`
   - GGEN_8020_ROADMAP.md
   - AUTONOMOUS_ONTOLOGY_SYSTEM.md
   - EVIDENCE_GRAPH_SCHEMA.md
   - SECTOR_BUNDLES_INTEGRATION_GUIDE.md
   - DARK_MATTER_MEASUREMENT.md
   - (all others)
3. Create `/docs/archive/README.md` explaining archive contents
4. Keep only these at root:
   - README.md (main entry point)
   - CHANGELOG.md
   - CONTRIBUTING.md
   - SECURITY.md
   - PERFORMANCE.md
5. Update repository root links to point to `/docs/`

**Files to keep (8 root files):**
- docs/README.md
- docs/CHANGELOG.md
- docs/CONTRIBUTING.md
- docs/SECURITY.md
- docs/PERFORMANCE.md
- docs/DOCUMENTATION_INDEX.md
- docs/DOCUMENTATION_STANDARDS.md
- docs/CODING_STANDARDS.md

**Files to archive (40+ files):**
- GGEN_8020_ROADMAP.md
- COMPLETE_8020_SYSTEM_SUMMARY.md
- AUTONOMOUS_ONTOLOGY_SYSTEM.md
- EVIDENCE_GRAPH_SCHEMA.md
- (and 36 more)

**Success Criteria:** GitHub root shows only essential docs; analysis files discoverable in archive

**Time Estimate:** 3 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 1.5 Update `docs/README.md` with Decision Tree

**Purpose:** Help external users self-route to appropriate documentation

**Add to README.md:**
1. "Choose Your Learning Path" section before detailed TOC
2. Decision tree (in text format, can expand to interactive later)
3. Links to path-specific entry points

**Example Structure:**
```
## 🛣️ Choose Your Learning Path

Answer these questions to find the right starting point:

**Question 1: Are you familiar with RDF/Semantic Web?**
- [Yes, I know RDF](path-rdf-first) → Jump straight to ontology-to-code
- [No, new to this](path-rdf-beginner) → Start with RDF primer

**Question 2: Do you want to use AI to generate ontologies?**
- [Yes](llm-config) → Configure your LLM first
- [No, I'll write RDF manually](ontology-to-code) → Traditional workflow

**Question 3: What's your main use case?**
- [REST/GraphQL APIs](rest-api-quickstart) → Backend developer guide
- [Data models/types](python-dataclasses) → Data scientist guide
- [CI/CD automation](deploy-production) → DevOps engineer guide
- [Build team platform](marketplace-workflow) → Platform team guide
```

**Success Criteria:** First-time visitor can find appropriate starting point in < 1 minute

**Time Estimate:** 2 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

## Phase 2: High-Priority Enhancements (Weeks 2-4)

These documents improve the core learning and documentation paths.

### 2.1 Create `tutorials/path-selector.md`

**Purpose:** Interactive/detailed path selection guide

**Content:**
- Detailed "choose your path" questionnaire
- Path descriptions (what you'll learn, time, prerequisites)
- Links to entry points
- Success criteria for each path
- "Getting stuck?" troubleshooting by path

**Success Criteria:** Users confident they're on the right path

**Time Estimate:** 3 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 2.2 Update All Tutorials with First Success Checkpoints

**Files to update:**
- `tutorials/getting-started.md`
- `tutorials/ontology-to-code.md`
- `tutorials/ai-powered-generation.md`
- `tutorials/marketplace-workflow.md`
- `tutorials/zero-to-generated-code.md`

**For each tutorial, add:**

```markdown
## Prerequisites
- [ ] Item 1 (e.g., "ggen installed")
- [ ] Item 2 (e.g., "OpenAI API key configured")
- [ ] Item 3

## What You'll Learn
[Learning objectives]

## Time Required
[15 minutes / 30 minutes / 1 hour]

## Success Looks Like
At the end of this tutorial, you will have:
1. [Concrete deliverable 1]
2. [Concrete deliverable 2]
3. [Concrete deliverable 3]

## Step-by-step Guide
[Current content]

## ✅ Success Checkpoint
You've succeeded if you can:
- [ ] Verify condition 1
- [ ] Show output 1
- [ ] Run command X and get result Y

## Common Failures
If you see error X:
→ Solution: [Fix]

If you see error Y:
→ Solution: [Fix]

## Next Steps
After completing this tutorial:
- [Next tutorial 1]
- [How-to guide 1]
- [Reference docs to explore]
```

**Success Criteria:** Each tutorial has clear entrance/exit criteria

**Time Estimate:** 12 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 2.3 Create `tutorials/rest-api-quickstart.md`

**Purpose:** Backend developer quick-start path

**Content:**
1. "Generate a REST API in 10 minutes"
2. Prerequisites (just ggen installed)
3. Sample domain (User/Post model)
4. Step-by-step:
   - Create minimal ontology (RDF)
   - Generate TypeScript code
   - Generate Python code
   - Generate Rust code
5. Run generated server (example)
6. Test endpoints (curl examples)
7. Verify output matches expectations
8. Next steps (customize, deploy, etc.)

**Success Criteria:** Backend developer sees working API in 2 languages in < 15 minutes

**Time Estimate:** 6 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 2.4 Create `reference/marketplace-metadata.md`

**Purpose:** Enable marketplace ecosystem; standard for template metadata

**Content:**
1. manifest.json schema
   - Required fields (name, author, version, description)
   - Optional fields (tags, license, dependencies)
   - Full JSON example
2. Template structure conventions
3. Metadata best practices
4. Publishing checklist
5. Version management
6. License considerations
7. Marketplace discovery optimization
   - Good tags
   - Good descriptions
   - Examples to include

**Success Criteria:** Template authors can publish with confidence

**Time Estimate:** 4 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 2.5 Resolve `/docs/src/` Documentation Duplication

**Analysis needed:**
- Are /src/ docs authoritative or legacy?
- Is there a build system that generates src/ from docs/?
- Is there tooling that syncs between them?

**Options:**
1. **Consolidate:** Move /src/ content to /docs/, delete /src/, update docs tooling
2. **Automate:** Create sync script to prevent divergence
3. **Archive:** Move /src/ to archive if truly legacy
4. **Document:** Clear README explaining relationship

**Success Criteria:** Clear ownership; no redundant maintenance

**Time Estimate:** 8 hours (depends on decision)

**Owner:** [Assign]

**Status:** [ ] Not Started

---

## Phase 3: Medium-Priority Improvements (Weeks 4-8)

These enhance documentation depth and reduce support burden.

### 3.1 Create `explanations/ggen-vs-alternatives.md`

**Purpose:** Help prospects understand competitive positioning

**Content:**
1. Code generation landscape overview
2. ggen vs. OpenAPI/Swagger generators
   - When to use each
   - Feature comparison
3. ggen vs. Terraform
   - Infrastructure vs. application code
   - When each makes sense
4. ggen vs. Helm
   - Application generation vs. Kubernetes deployment
5. ggen vs. Custom generators
   - Why ggen's RDF approach is better
   - Examples of issues custom generators have
6. ggen vs. AI-only code generation (GitHub Copilot, etc.)
   - Determinism vs. variability
   - Team standardization
   - When to combine

**Success Criteria:** Prospect understands why ggen is different and better

**Time Estimate:** 8 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 3.2 Rewrite Explanations with Multi-Level Depth

**Files to update:**
- `explanations/projections.md`
- `explanations/poke-yoke.md`
- `explanations/determinism.md`

**For each, restructure as:**

```markdown
# [Concept Name]

## Quick Version (2 minutes)
[One paragraph explaining core idea]

## Visual Explanation
[Diagram or ASCII art]

## Intermediate Version (10 minutes)
[More detailed explanation with examples]

## Deep Dive (30 minutes)
[Theory, mathematics, formal definitions]

## Real-World Implications
[How does this affect you as a user?]

## Further Reading
[References to related concepts]
```

**Success Criteria:** All knowledge levels can understand the concept

**Time Estimate:** 20 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 3.3 Update `how-to-guides/troubleshoot.md` with Decision Trees

**Purpose:** Help users self-diagnose common issues

**Add decision trees for:**
1. "I can't find my templates"
   - Is ggen installed? → Check PATH
   - Did you run `ggen add template`? → Run it now
   - Is marketplace accessible? → Check network
2. "My hook isn't running"
   - Is hook configured? → Run `ggen config show hooks`
   - Is file path correct? → Verify path
   - Is syntax valid? → Validate hook file
3. "Generated code won't compile"
   - Are you using latest version? → Update ggen
   - Is ontology valid? → Validate RDF
   - Are templates compatible? → Check template docs
4. "LLM generation failed"
   - Is API key valid? → Test API key
   - Is model available? → Check rate limits
   - Is prompt too long? → Simplify prompt
5. "Marketplace template broken"
   - Is template version compatible? → Check ggen version
   - Are dependencies installed? → Install dependencies
   - Is metadata valid? → Report issue

**Format:** flowchart-style decision trees (text or ASCII)

**Success Criteria:** Users can solve 90% of common issues themselves

**Time Estimate:** 8 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 3.4 Add Real-World Examples Throughout Documentation

**Files to enhance:**
- `how-to-guides/create-templates.md`
- `reference/rdf-sparql.md`
- `reference/templates.md`
- `reference/type-mapping.md`

**For each file, add:**
1. Before/after comparisons
2. Real example output (not just snippets)
3. Common use cases
4. Anti-patterns to avoid
5. Variations of the solution

**Example (for template creation):**
```markdown
### Example: Todo App Template

#### 1. Define Your Ontology
[RDF/Turtle example]

#### 2. Create Jinja Template
[Template code]

#### 3. Generated Output
TypeScript:
[Real TypeScript code output]

Python:
[Real Python code output]

#### 4. What You Should See
[Expected output structure]

#### 5. Common Mistakes
❌ [Anti-pattern 1 explanation]
✅ [Correct pattern 1]

❌ [Anti-pattern 2 explanation]
✅ [Correct pattern 2]
```

**Success Criteria:** All reference docs include real examples

**Time Estimate:** 15 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 3.5 Create `explanations/architecture-decisions.md`

**Purpose:** Build confidence in technical choices; help contributors understand rationale

**Content:**
1. Why Rust for ggen CLI?
   - Memory safety
   - Performance
   - Compiled binaries
   - Trade-offs (learning curve, compilation time)

2. Why Oxigraph for RDF backend?
   - SPARQL 1.1 compliance
   - Performance
   - Linked Data support
   - Trade-offs (vs. custom JSON schema)

3. Why SPARQL for ontology queries?
   - W3C standard
   - Powerful query language
   - Ecosystem tools
   - Trade-offs (learning curve)

4. Why marketplace pattern?
   - Composability
   - Community-driven
   - Version management
   - Trade-offs (vs. monolithic templates)

5. Why deterministic generation?
   - Reproducibility
   - Testability
   - Predictability
   - Trade-offs (flexibility constraints)

6. Why ontology-driven?
   - Single source of truth
   - Multiple outputs
   - Reduces drift
   - Trade-offs (upfront learning)

7. Architecture decisions matrix (table)
   - Decision → Options considered → Choice → Rationale → Consequences

**Success Criteria:** Contributors understand the "why" behind technical choices

**Time Estimate:** 6 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

## Phase 4: Nice-to-Have Enhancements (Weeks 8-12)

These are value-adds but not critical for core user success.

### 4.1 Create Video Content

**Videos to create:**
1. "ggen in 5 minutes" (5 min)
   - Overview of concept
   - Install
   - Generate first code

2. "Create your first REST API" (10 min)
   - Write simple ontology
   - Generate REST server
   - Test endpoints

3. "AI-powered ontology generation" (10 min)
   - Setup LLM
   - Describe domain in English
   - See generated ontology

4. "Building the marketplace" (15 min)
   - Create template
   - Publish to marketplace
   - Use your template

**Platform:** YouTube (embedded in docs)

**Time Estimate:** 20 hours (scripting, recording, editing)

**Owner:** [Video team]

**Status:** [ ] Not Started

---

### 4.2 Create Interactive Tutorial Mode

**Feature:** `ggen tutorial start`

**Functionality:**
- Guided step-by-step walkthrough
- Validates user inputs in real-time
- Explains what each step does
- Troubleshoots common errors
- Provides hints when stuck

**Time Estimate:** 16 hours (implementation)

**Owner:** [Backend team]

**Status:** [ ] Not Started

---

### 4.3 Create Official Template Repository Examples

**Create git repositories:**
1. `ggen-template-rest-api`
   - Complete REST API template
   - Generated code examples
   - Deployment examples
   - Tests

2. `ggen-template-graphql`
   - GraphQL API template
   - Generated code
   - Subscription examples

3. `ggen-template-microservices`
   - Multi-service ontology
   - Service separation
   - Integration patterns

**Time Estimate:** 24 hours

**Owner:** [Assign]

**Status:** [ ] Not Started

---

### 4.4 Documentation Search Optimization

**Implement full-text search:**
- Tool: Meilisearch or Algolia
- Index all documentation
- Smart query understanding
- Result ranking by relevance
- "Related docs" suggestions

**Time Estimate:** 12 hours (integration + testing)

**Owner:** [Assign]

**Status:** [ ] Not Started

---

## Success Metrics

### Phase 1 (Weeks 1-2)
**Goal:** Unblock critical features

**Metrics:**
- [ ] All 4 critical documents created
- [ ] Root documentation cleaned up
- [ ] No build/link errors
- [ ] Review feedback incorporated

---

### Phase 2 (Weeks 2-4)
**Goal:** Improve core learning paths

**Metrics:**
- [ ] All 5 high-priority documents created
- [ ] All tutorials updated with checkpoints
- [ ] New user success rate: 50% (from 31%)
- [ ] Tutorial bounce rate < 30%
- [ ] Documentation duplication resolved

---

### Phase 3 (Weeks 4-8)
**Goal:** Deepen understanding and reduce support burden

**Metrics:**
- [ ] All 5 medium-priority documents created
- [ ] Support questions reduced by 40%
- [ ] New user success rate: 70% (from 50%)
- [ ] Explanation documents accessible to all levels

---

### Phase 4 (Weeks 8-12)
**Goal:** Premium user experience

**Metrics:**
- [ ] Video content available
- [ ] Search implemented
- [ ] 3+ official template examples created
- [ ] New user success rate: 75%+ (from 31%)
- [ ] NPS (Net Promoter Score) improvement

---

## Risk Mitigation

### Risk: Documentation becomes stale

**Mitigation:**
- [ ] Create MUDA inventory (prevent duplicates)
- [ ] Document maintenance schedule
- [ ] Flag outdated content with dates
- [ ] Automated link checking

### Risk: New docs conflict with existing ones

**Mitigation:**
- [ ] Review for duplication before creation
- [ ] Link between related documents
- [ ] Define "source of truth" for each topic
- [ ] Cross-reference instead of repeat

### Risk: Implementation team gets sidetracked

**Mitigation:**
- [ ] Assign specific owners per task
- [ ] Weekly progress meetings
- [ ] Use issues/PRs for tracking
- [ ] Clear definition of done

---

## Summary

| Phase | Duration | Documents | Impact |
|-------|----------|-----------|--------|
| Phase 1 | 2 weeks | 5 | 40% new user success |
| Phase 2 | 2 weeks | 5 updated | 70% new user success |
| Phase 3 | 4 weeks | 5 | 70% support reduction |
| Phase 4 | 4 weeks | Premium | 75%+ new user success |

**Total Timeline:** 8-12 weeks
**Total Effort:** ~113 hours (~3 person-weeks)
**Expected ROI:** 3-5x increase in new user adoption

---

**Status:** Ready for execution
**Review Date:** TBD
**Last Updated:** November 17, 2025
