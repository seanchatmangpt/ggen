<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Crossing the Event Horizon: Frequently Asked Questions (FAQ)](#crossing-the-event-horizon-frequently-asked-questions-faq)
  - [Quick Navigation](#quick-navigation)
  - [Table of Contents](#table-of-contents)
  - [Conceptual Questions](#conceptual-questions)
    - [What is the "event horizon" in RDF-first development? {&#035;what-is-the-event-horizon}](#what-is-the-event-horizon-in-rdf-first-development-what-is-the-event-horizon)
    - [What is the A = μ(O) equation? {&#035;what-is-the-equation}](#what-is-the-a--%CE%BCo-equation-what-is-the-equation)
    - [Is RDF-first right for my project? {&#035;is-rdf-first-right-for-my-project}](#is-rdf-first-right-for-my-project-is-rdf-first-right-for-my-project)
    - [How long does it take to learn? {&#035;how-long-to-learn}](#how-long-does-it-take-to-learn-how-long-to-learn)
  - [Getting Started](#getting-started)
    - [Where do I start? {&#035;where-to-start}](#where-do-i-start-where-to-start)
    - [Do I need to know RDF/SPARQL already? {&#035;need-to-know-rdf}](#do-i-need-to-know-rdfsparql-already-need-to-know-rdf)
    - [What tools do I need? {&#035;what-tools-needed}](#what-tools-do-i-need-what-tools-needed)
  - [Technical Details](#technical-details)
    - [What's the difference between RDF, Turtle, and SPARQL? {&#035;rdf-turtle-sparql}](#whats-the-difference-between-rdf-turtle-and-sparql-rdf-turtle-sparql)
    - [What are the five stages of μ? {&#035;five-stages}](#what-are-the-five-stages-of-%CE%BC-five-stages)
    - [How does SHACL validation work? {&#035;how-shacl-works}](#how-does-shacl-validation-work-how-shacl-works)
  - [Workflow and Process](#workflow-and-process)
    - [How do I edit generated code? {&#035;edit-generated-code}](#how-do-i-edit-generated-code-edit-generated-code)
    - [What if I need custom code that can't be generated? {&#035;custom-code}](#what-if-i-need-custom-code-that-cant-be-generated-custom-code)
    - [Can I use RDF-first with existing projects? {&#035;existing-projects}](#can-i-use-rdf-first-with-existing-projects-existing-projects)
  - [Troubleshooting](#troubleshooting)
    - [Why is my SHACL validation failing? {&#035;shacl-failing}](#why-is-my-shacl-validation-failing-shacl-failing)
    - [Why doesn't my code compile after generation? {&#035;code-not-compiling}](#why-doesnt-my-code-compile-after-generation-code-not-compiling)
    - [How do I debug ontology issues? {&#035;debug-ontology}](#how-do-i-debug-ontology-issues-debug-ontology)
  - [Comparison with Alternatives](#comparison-with-alternatives)
    - [How does this compare to GraphQL/Protobuf/OpenAPI? {&#035;compare-to-alternatives}](#how-does-this-compare-to-graphqlprotobufopenapi-compare-to-alternatives)
  - [Team and Organizational](#team-and-organizational)
    - [How do I convince my team to try RDF-first? {&#035;convince-team}](#how-do-i-convince-my-team-to-try-rdf-first-convince-team)
  - [Advanced Topics](#advanced-topics)
    - [How do I optimize generation performance? {&#035;optimize-performance}](#how-do-i-optimize-generation-performance-optimize-performance)
    - [Can I generate code for multiple languages? {&#035;multiple-languages}](#can-i-generate-code-for-multiple-languages-multiple-languages)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Crossing the Event Horizon: Frequently Asked Questions (FAQ)

> **Template Instructions**: Replace all `[bracketed sections]` with actual content. Organize questions by category. Delete this instruction block when creating a real FAQ.

## Quick Navigation

**Getting Started**:
- [What is the "event horizon" in RDF-first development?](#what-is-the-event-horizon)
- [Is RDF-first right for my project?](#is-rdf-first-right-for-my-project)
- [How long does it take to learn?](#how-long-to-learn)

**Technical Questions**:
- [What is the A = μ(O) equation?](#what-is-the-equation)
- [What are the five stages of μ?](#five-stages)
- [What's the difference between RDF, Turtle, and SPARQL?](#rdf-turtle-sparql)

**Practical Questions**:
- [How do I edit generated code?](#edit-generated-code)
- [What if I need custom code that can't be generated?](#custom-code)
- [Can I use RDF-first with existing projects?](#existing-projects)

**Troubleshooting**:
- [Why is my SHACL validation failing?](#shacl-failing)
- [Why doesn't my code compile after generation?](#code-not-compiling)
- [How do I debug ontology issues?](#debug-ontology)

**Advanced Topics**:
- [How do I optimize generation performance?](#optimize-performance)
- [Can I generate code for multiple languages?](#multiple-languages)
- [How does this compare to GraphQL/Protobuf/OpenAPI?](#compare-to-alternatives)

---

## Table of Contents

1. [Conceptual Questions](#conceptual-questions)
2. [Getting Started](#getting-started)
3. [Technical Details](#technical-details)
4. [Workflow and Process](#workflow-and-process)
5. [Troubleshooting](#troubleshooting)
6. [Comparison with Alternatives](#comparison-with-alternatives)
7. [Team and Organizational](#team-and-organizational)
8. [Advanced Topics](#advanced-topics)

---

## Conceptual Questions

### What is the "event horizon" in RDF-first development? {#what-is-the-event-horizon}

**Short Answer**: The point of no return when you commit to ontology-driven development.

**Detailed Answer**:

In physics, an event horizon is the boundary around a black hole beyond which nothing can escape. In software development, the "event horizon" is the conceptual boundary between:

- **Before**: Traditional code-first thinking (code is truth)
- **After**: RDF-first thinking (ontology is truth, code is a projection)

Once you cross this boundary and experience:
- Zero spec/code drift
- Deterministic outputs
- Cryptographic receipts
- Machine-readable requirements

...you cannot go back to code-first thinking. The ontology becomes your reality, and code becomes merely a projection of that reality.

**Example**:
```
Before Event Horizon: "The spec says X, but the code does Y. I guess the code is right."
After Event Horizon:  "The ontology says X, but the code does Y. Fix the ontology and regenerate."
```

**See Also**:
- [EVENT_HORIZON.md - Introduction](EVENT_HORIZON_GUIDE_TEMPLATE.md#executive-summary)
- [Mental Model Shifts](EVENT_HORIZON_GUIDE_TEMPLATE.md#mental-model-shifts)

---

### What is the A = μ(O) equation? {#what-is-the-equation}

**Short Answer**: Code (A) is generated from Ontology (O) via transformation pipeline (μ).

**Detailed Answer**:

The equation **A = μ(O)** is the fundamental principle of RDF-first development:

- **A** (Artifacts): Generated code, documentation, tests, configs
- **μ** (Mu): Five-stage deterministic transformation pipeline
- **O** (Ontology): RDF/Turtle specifications (source of truth)

This means:
1. Ontology is the **only** source of truth
2. Code is **derived** from ontology (not hand-written)
3. Transformation is **deterministic** (same input → same output)
4. Change the ontology, regenerate artifacts (not the other way around)

**Five Stages of μ**:
```
μ = μ₅ ∘ μ₄ ∘ μ₃ ∘ μ₂ ∘ μ₁

μ₁: Normalize   (validate RDF with SHACL)
μ₂: Extract     (SPARQL queries)
μ₃: Emit        (template rendering)
μ₄: Canonicalize (formatting)
μ₅: Receipt     (cryptographic proof)
```

**Example**:
```turtle
# Ontology (O)
:us-001 a sk:UserStory ;
    sk:title "User can log in" .

# ↓ μ (transformation pipeline)

# Artifacts (A)
pub struct Login { ... }  // Rust code
#[test] fn test_login() { ... }  // Test
## Login Feature  // Documentation
```

**See Also**:
- [Five-Stage Pipeline Details](EVENT_HORIZON_GUIDE_TEMPLATE.md#five-stage-pipeline-μ)

---

### Is RDF-first right for my project? {#is-rdf-first-right-for-my-project}

**Decision Tree**:

```
Is your project trivial (< 1000 LOC)?
├─ YES → Code-first is fine (RDF overhead not worth it)
└─ NO  → Continue...

Do you have formal requirements/specs?
├─ NO  → Maybe code-first (but consider future)
└─ YES → Continue...

Do specs and code often drift?
├─ YES → RDF-first STRONGLY RECOMMENDED
└─ NO  → Continue...

Multiple implementations (Rust + TS + Python)?
├─ YES → RDF-first STRONGLY RECOMMENDED
└─ NO  → Continue...

Compliance requirements (SOC2, HIPAA)?
├─ YES → RDF-first RECOMMENDED (cryptographic receipts)
└─ NO  → RDF-first is OPTIONAL but beneficial
```

**When RDF-First Excels**:
- ✅ Projects with formal specifications
- ✅ Multi-language codebases (one ontology, many projections)
- ✅ Compliance requirements (audit trails needed)
- ✅ Complex domain models (50+ entities)
- ✅ Teams struggling with spec/code drift
- ✅ Long-lived projects (maintainability critical)

**When Code-First May Be Better**:
- ❌ Prototypes and throwaway code
- ❌ Very small projects (< 1000 LOC)
- ❌ No formal requirements
- ❌ Single developer, single language

**Break-Even Point**: After ~5-10 features (2-3 weeks), RDF-first becomes faster and safer than code-first.

**See Also**:
- [Decision Framework](EVENT_HORIZON_GUIDE_TEMPLATE.md#decision-framework)
- [Cost-Benefit Analysis](EVENT_HORIZON_GUIDE_TEMPLATE.md#cost-benefit-analysis)

---

### How long does it take to learn? {#how-long-to-learn}

**Short Answer**: 1-2 weeks to productivity, 3-6 months to mastery.

**Learning Curve by Phase**:

| Phase | Duration | Productivity | Key Milestone |
|-------|----------|--------------|---------------|
| **1. Recognition** | 1-2 days | 0% | Understand concepts |
| **2. Resistance** | 3-5 days | 20% | First ontology created |
| **3. Experimentation** | 2-3 weeks | 50% | Prefer RDF for new features |
| **4. Commitment** | 1-2 months | 80% | All new work is RDF-first |
| **5. Mastery** | 3-6 months | 150% | 2-3x faster than code-first |

**What You Need to Learn**:

**Week 1**:
- [ ] Turtle syntax (RDF serialization)
- [ ] Basic SPARQL queries (SELECT, WHERE)
- [ ] ggen CLI commands (validate, sync)
- [ ] SHACL validation concepts

**Week 2**:
- [ ] Writing user stories in RDF
- [ ] Acceptance scenarios (Given/When/Then)
- [ ] Template basics (Tera syntax)
- [ ] Reading generated code

**Week 3-4**:
- [ ] Custom SPARQL queries
- [ ] Custom templates
- [ ] Ontology design patterns
- [ ] Receipt verification

**Months 2-3**:
- [ ] Advanced SPARQL (CONSTRUCT, FILTER, OPTIONAL)
- [ ] SHACL shape design
- [ ] Multi-language projection
- [ ] Performance optimization

**Comparison to Other Technologies**:
- Similar learning curve to GraphQL (2-3 weeks to productivity)
- Easier than learning a new programming language
- Harder than learning a new library

**See Also**:
- [Transformation Journey](EVENT_HORIZON_GUIDE_TEMPLATE.md#transformation-journey)
- [Getting Started Tutorial](../tutorials/01_getting_started.md)

---

## Getting Started

### Where do I start? {#where-to-start}

**Recommended Path**:

**Day 1**: Understand Concepts
1. Read [EVENT_HORIZON.md Executive Summary](EVENT_HORIZON_GUIDE_TEMPLATE.md#executive-summary)
2. Review [Before vs After comparison](EVENT_HORIZON_GUIDE_TEMPLATE.md#before-vs-after-the-event-horizon)
3. Watch [Video Tutorial] (if available)

**Day 2**: Setup Environment
1. Install ggen: `cargo install ggen`
2. Verify installation: `ggen --version`
3. Complete [Setup Checklist](../tutorials/01_setup.md)

**Day 3-5**: First Ontology
1. Complete [Exercise 1: Your First Ontology](../exercises/01_first_ontology.md)
2. Create a user story in Turtle
3. Generate code with `ggen sync`
4. Commit your receipt

**Week 2**: Real Feature
1. Choose a small feature from your backlog
2. Model it as RDF ontology
3. Create custom templates for your codebase
4. Compare velocity to code-first

**Week 3-4**: Team Training
1. Share your experience with team
2. Pair program with teammates
3. Convert 3-5 features to RDF-first
4. Gather metrics

**Resources**:
- [Quick Start Guide](../getting-started/quick_start.md)
- [Video Tutorials](../tutorials/index.md)
- [Example Projects](../examples/index.md)

---

### Do I need to know RDF/SPARQL already? {#need-to-know-rdf}

**Short Answer**: No, but basic understanding helps.

**What You Actually Need**:

**Minimal (Day 1)**:
- Understand that RDF is "triples" (subject-predicate-object)
- Know Turtle is a human-friendly RDF format
- Recognize basic pattern: `:subject :predicate "object" .`

**Practical (Week 1)**:
- Write user stories in Turtle syntax
- Copy-paste-modify examples
- Read basic SPARQL queries

**Proficient (Month 1)**:
- Design ontology schemas
- Write custom SPARQL queries
- Create SHACL shapes

**Expert (Month 3+)**:
- Optimize SPARQL performance
- Design domain-specific vocabularies
- Contribute to ggen core

**Learning Resources**:

**Official W3C Specs** (reference, not for learning):
- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [Turtle Specification](https://www.w3.org/TR/turtle/)
- [SPARQL Query Language](https://www.w3.org/TR/sparql11-query/)

**Practical Guides** (start here):
- [ggen Turtle Quick Reference](../reference/turtle_quick_reference.md)
- [SPARQL Query Examples](../examples/sparql_queries.md)
- [Interactive SPARQL Tutorial](https://www.w3.org/2009/Talks/0615-qbe/)

**You Don't Need**:
- ❌ PhD in Semantic Web
- ❌ Deep understanding of RDF theory
- ❌ Knowledge of OWL reasoning
- ❌ Experience with triple stores

**Just Start**: Copy the [user story template](../../.specify/templates/rdf-helpers/user-story.ttl.template) and modify it. You'll learn by doing.

---

### What tools do I need? {#what-tools-needed}

**Required**:
- [x] ggen CLI: `cargo install ggen`
- [x] Rust toolchain: `rustup` (for cargo make)
- [x] Git (version control)
- [x] Text editor (any will work)

**Recommended**:
- [x] VS Code with RDF extension (syntax highlighting)
- [x] `jq` command-line JSON processor (for receipts)
- [x] `timeout` command (SLO enforcement)

**Optional but Helpful**:
- [ ] SPARQL query editor (e.g., Yasgui)
- [ ] Graph visualization tool (e.g., WebVOWL)
- [ ] Docker (for testcontainers)

**Installation**:

```bash
# Install ggen
cargo install ggen

# Verify installation
ggen --version

# Install VS Code RDF extension
code --install-extension stardog-union.vscode-langserver-sparql

# Install jq (macOS)
brew install jq

# Install jq (Ubuntu)
sudo apt-get install jq

# Verify timeout exists
cargo make timeout-check
```

**IDE Setup**:

**VS Code** (recommended):
```json
// .vscode/settings.json
{
  "files.associations": {
    "*.ttl": "turtle",
    "*.tera": "jinja"
  },
  "editor.formatOnSave": true
}
```

**Vim**:
```vim
" .vimrc
au BufRead,BufNewFile *.ttl set filetype=turtle
au BufRead,BufNewFile *.tera set filetype=jinja
```

**See Also**:
- [Development Environment Setup](../how-to/setup_environment.md)

---

## Technical Details

### What's the difference between RDF, Turtle, and SPARQL? {#rdf-turtle-sparql}

**Short Answer**:
- **RDF**: Data model (triples)
- **Turtle**: Syntax for writing RDF
- **SPARQL**: Query language for RDF

**Detailed Comparison**:

| Technology | Role | Analogy |
|------------|------|---------|
| **RDF** | Data model | Like "relational model" (tables, rows) |
| **Turtle** | Serialization format | Like "SQL script" (CREATE TABLE...) |
| **SPARQL** | Query language | Like "SQL SELECT" (SELECT * FROM...) |

**RDF (Resource Description Framework)**:
- Core concept: Everything is a "triple" (subject-predicate-object)
- Example: `(:john :likes :pizza)` means "John likes pizza"
- Think of it as a graph database model

**Turtle (Terse RDF Triple Language)**:
- Human-friendly way to write RDF
- Example:
  ```turtle
  :john :likes :pizza ;
        :age 30 .
  ```
- Alternatives: RDF/XML (verbose), N-Triples (explicit), JSON-LD (for JSON lovers)

**SPARQL (SPARQL Protocol and RDF Query Language)**:
- SQL for RDF graphs
- Example:
  ```sparql
  SELECT ?person ?food
  WHERE {
      ?person :likes ?food .
      ?person :age ?age .
      FILTER(?age > 25)
  }
  ```

**In ggen Workflow**:

1. **You write**: Turtle files (`.ttl`)
   ```turtle
   :us-001 a sk:UserStory ;
       sk:title "User can log in" .
   ```

2. **ggen executes**: SPARQL queries on your Turtle data
   ```sparql
   SELECT ?title WHERE { ?story sk:title ?title }
   ```

3. **ggen generates**: Code from query results

**See Also**:
- [Turtle Syntax Reference](../reference/turtle_syntax.md)
- [SPARQL Query Examples](../examples/sparql_queries.md)

---

### What are the five stages of μ? {#five-stages}

**Quick Reference**:

```
μ₁: Normalize    → Validate RDF, resolve dependencies
μ₂: Extract      → Execute SPARQL queries
μ₃: Emit         → Render Tera templates
μ₄: Canonicalize → Format and hash outputs
μ₅: Receipt      → Generate cryptographic proof
```

**Detailed Breakdown**:

**Stage 1: Normalize (μ₁)**
- **Input**: Raw `.ttl` files from `.specify/`
- **Process**:
  1. Parse Turtle syntax
  2. Validate against SHACL shapes
  3. Resolve imports and dependencies
  4. Materialize OWL inferences
- **Output**: Validated, normalized RDF graph
- **Time**: < 1 second (for typical projects)
- **Can Fail**: Yes (SHACL validation errors)

**Stage 2: Extract (μ₂)**
- **Input**: Normalized RDF graph
- **Process**:
  1. Execute SPARQL SELECT queries
  2. Execute SPARQL CONSTRUCT queries
  3. Apply inference rules (RDFS, OWL2-RL)
  4. Build template context (JSON/YAML)
- **Output**: Structured data bindings
- **Time**: < 100ms (for typical projects)
- **Can Fail**: Yes (SPARQL syntax errors)

**Stage 3: Emit (μ₃)**
- **Input**: Template bindings from μ₂
- **Process**:
  1. Load Tera templates
  2. Render templates with bindings
  3. Generate multiple files
  4. Create directory structures
- **Output**: Raw generated artifacts
- **Time**: < 500ms (for typical projects)
- **Can Fail**: Yes (template syntax errors)

**Stage 4: Canonicalize (μ₄)**
- **Input**: Raw generated artifacts
- **Process**:
  1. Apply language formatters (rustfmt, prettier)
  2. Validate syntax (compiler checks)
  3. Compute SHA-256 hashes
- **Output**: Formatted, hashed artifacts
- **Time**: < 2 seconds (for typical projects)
- **Can Fail**: Yes (syntax errors)

**Stage 5: Receipt (μ₅)**
- **Input**: Canonicalized artifacts
- **Process**:
  1. Generate execution ID (UUID + timestamp)
  2. Hash manifest (ggen.toml)
  3. Hash ontology (all .ttl files)
  4. Collect timings (μs precision)
  5. Write receipt JSON
  6. Write audit log
- **Output**: Cryptographic receipt, audit trail
- **Time**: < 100ms
- **Can Fail**: No (always succeeds)

**Total Pipeline Time**: < 5 seconds (SLO target)

**Command**:
```bash
ggen sync --audit true
# Executes μ₁ through μ₅
```

**See Also**:
- [Five-Stage Pipeline Details](EVENT_HORIZON_GUIDE_TEMPLATE.md#five-stage-pipeline-μ)
- [Performance SLOs](../reference/performance_slos.md)

---

### How does SHACL validation work? {#how-shacl-works}

**Short Answer**: SHACL defines "shapes" that RDF data must conform to (like JSON Schema for RDF).

**Example**:

**Ontology Data** (what you write):
```turtle
:us-001 a sk:UserStory ;
    sk:priority "HIGH" ;  # ❌ Will fail validation
    sk:title "User can log in" .
```

**SHACL Shape** (validation rules):
```turtle
:UserStoryShape a sh:NodeShape ;
    sh:targetClass sk:UserStory ;
    sh:property [
        sh:path sk:priority ;
        sh:in ("P1" "P2" "P3") ;  # Only these values allowed
        sh:message "Priority must be exactly P1, P2, or P3" ;
    ] .
```

**Validation Result**:
```
❌ SHACL Validation Failed
  - Priority must be exactly P1, P2, or P3 (found: "HIGH")
```

**Common SHACL Constraints**:

**Required Property**:
```turtle
sh:property [
    sh:path sk:title ;
    sh:minCount 1 ;
    sh:message "User story must have a title" ;
] .
```

**Allowed Values** (enum):
```turtle
sh:property [
    sh:path sk:status ;
    sh:in ("planning" "in-progress" "completed") ;
] .
```

**Data Type**:
```turtle
sh:property [
    sh:path sk:storyIndex ;
    sh:datatype xsd:integer ;
    sh:minInclusive 1 ;
] .
```

**Minimum Count** (e.g., at least one acceptance scenario):
```turtle
sh:property [
    sh:path sk:hasAcceptanceScenario ;
    sh:minCount 1 ;
    sh:message "User story must have at least one acceptance scenario" ;
] .
```

**Why SHACL Matters**:
1. **Catch errors early** (before generation)
2. **Enforce consistency** (all user stories have same structure)
3. **Self-documenting** (shapes define expected structure)
4. **Poka-Yoke** (error prevention, not detection)

**See Also**:
- [SHACL Specification](https://www.w3.org/TR/shacl/)
- [ggen SHACL Shapes](../../.specify/ontology/spec-kit-schema.ttl)

---

## Workflow and Process

### How do I edit generated code? {#edit-generated-code}

**Short Answer**: You don't. Edit the ontology, then regenerate.

**WRONG Workflow** ❌:
```bash
# 1. ggen sync generates src/auth/login.rs
# 2. You edit src/auth/login.rs directly
vim src/auth/login.rs  # ❌ DON'T DO THIS

# 3. Next ggen sync overwrites your changes
ggen sync  # 💥 Your edits are LOST
```

**CORRECT Workflow** ✅:
```bash
# 1. ggen sync generates src/auth/login.rs

# 2. You need to make a change
vim .specify/specs/013-auth/feature.ttl  # ✅ Edit ontology

# 3. Regenerate
ggen sync --audit true  # Generated code updated
```

**"But I just need to change one line!"**

Even for a one-line change, follow the correct workflow:

**Example**: Change session timeout from 30 minutes to 60 minutes

**WRONG** ❌:
```rust
// In generated file src/auth/session.rs
pub const SESSION_TIMEOUT: u32 = 1800;  // Change 1800 to 3600
```

**CORRECT** ✅:
```turtle
# In .specify/specs/013-auth/feature.ttl
:us-001-as-003 a sk:AcceptanceScenario ;
    sk:given "User has been logged in for 60 minutes" ;  # Change 30 to 60
    sk:when "User attempts to access protected resource" ;
    sk:then "System returns 401 Unauthorized" .
```

Then regenerate:
```bash
ggen sync --audit true
```

**Why This Matters**:
1. **Traceability**: Change is in git history of ontology
2. **Consistency**: Docs, tests, and code all update together
3. **Receipt**: Cryptographic proof of what changed
4. **Zero Drift**: Spec and code cannot diverge

**Exception**: See [custom code](#custom-code) for code that genuinely can't be generated.

---

### What if I need custom code that can't be generated? {#custom-code}

**Short Answer**: Use "extension points" - generated code calls your custom code.

**Strategy 1: Implement Interfaces**

**Generated Code** (from ontology):
```rust
// Generated file: src/domain/auth.rs
// DO NOT EDIT

pub trait AuthProvider {
    fn verify_credentials(&self, email: &str, password: &str) -> Result<User, AuthError>;
}

pub struct AuthService<P: AuthProvider> {
    provider: P,
}

impl<P: AuthProvider> AuthService<P> {
    pub fn login(&self, email: &str, password: &str) -> Result<Session, AuthError> {
        let user = self.provider.verify_credentials(email, password)?;
        // Generated session creation logic...
    }
}
```

**Your Custom Code** (hand-written):
```rust
// Your file: src/infrastructure/auth_provider.rs
// CUSTOM CODE - safe to edit

use crate::domain::auth::{AuthProvider, User, AuthError};

pub struct DatabaseAuthProvider {
    db: Database,
}

impl AuthProvider for DatabaseAuthProvider {
    fn verify_credentials(&self, email: &str, password: &str) -> Result<User, AuthError> {
        // Your custom database logic
        let user = self.db.find_user_by_email(email)?;
        if user.verify_password(password) {
            Ok(user)
        } else {
            Err(AuthError::InvalidCredentials)
        }
    }
}
```

**Strategy 2: Separate Directories**

```
src/
├── domain/          # Generated (from ontology)
│   ├── auth.rs      # DO NOT EDIT
│   └── user.rs      # DO NOT EDIT
│
└── infrastructure/  # Hand-written (your custom code)
    ├── auth_provider.rs   # SAFE TO EDIT
    └── database.rs        # SAFE TO EDIT
```

**Mark Custom Directories** in `.gitattributes`:
```gitattributes
# .gitattributes
src/domain/**/*.rs generated
src/infrastructure/**/*.rs -generated
```

**Strategy 3: Partial Generation**

**Template** (`.specify/templates/auth.tera`):
```tera
// Generated from ontology
// DO NOT EDIT above this line

{% for story in user_stories %}
pub struct {{ story.name | pascal_case }} {
    // Generated fields...
}
{% endfor %}

// CUSTOM CODE BELOW - safe to edit
// Add your custom implementations here
```

**Generated Output**:
```rust
// Generated from ontology
// DO NOT EDIT above this line

pub struct Login {
    pub email: String,
    pub password: String,
}

// CUSTOM CODE BELOW - safe to edit
// Add your custom implementations here

impl Login {
    // Your custom validation logic
    pub fn validate(&self) -> Result<(), ValidationError> {
        // ...
    }
}
```

**Best Practice**: Keep generated and custom code in separate files/modules. Generated code defines interfaces/data structures, custom code provides implementations.

---

### Can I use RDF-first with existing projects? {#existing-projects}

**Short Answer**: Yes, incrementally. Start with new features, gradually migrate old code.

**Incremental Adoption Strategy**:

**Phase 1: New Features Only (Week 1-4)**
- All new features written as RDF ontologies
- Existing code remains unchanged
- Low risk, high learning value

**Phase 2: Critical Features (Month 2-3)**
- Identify 3-5 critical features with high drift risk
- Model in RDF, generate code
- Compare generated vs hand-written
- Replace hand-written if generated is equivalent

**Phase 3: Domain Model (Month 3-6)**
- Model core domain entities in RDF
- Generate domain layer code
- Keep infrastructure/implementation layers hand-written
- Focus on business logic, not technical details

**Phase 4: Full Migration (Month 6+)**
- Convert remaining features incrementally
- Deprecate hand-written code as ontology coverage grows
- Eventually: 80%+ generated, 20% custom infrastructure

**Coexistence Pattern**:

```
project/
├── .specify/              # RDF ontologies (new)
│   └── specs/
│       └── 013-feature/
│           └── feature.ttl
│
├── src/
│   ├── legacy/           # Old hand-written code
│   │   ├── auth.rs       # To be replaced
│   │   └── user.rs       # To be replaced
│   │
│   └── generated/        # New RDF-generated code
│       ├── auth.rs       # From ontology
│       └── user.rs       # From ontology
│
└── Cargo.toml
```

**Gradual Integration**:
```rust
// In main.rs
use crate::generated::auth::AuthService;  // New RDF-first code
use crate::legacy::payment::PaymentService;  // Old hand-written code

// They can coexist
let auth = AuthService::new();
let payment = PaymentService::new();
```

**Metrics to Track**:
- % of code generated vs hand-written
- Drift incidents (generated vs hand-written)
- Development velocity (RDF-first vs code-first)
- Bug rates (generated vs hand-written)

**Case Study**: [Incremental Migration Example](../examples/case-studies/incremental_migration.md)

---

## Troubleshooting

### Why is my SHACL validation failing? {#shacl-failing}

**Common Causes**:

**1. Priority Value Mismatch**

**Error**:
```
Priority must be exactly P1, P2, or P3 (found: "HIGH")
```

**Cause**:
```turtle
:us-001 sk:priority "HIGH" ;  # ❌ WRONG
```

**Fix**:
```turtle
:us-001 sk:priority "P1" ;  # ✅ CORRECT
```

**2. Missing Required Field**

**Error**:
```
User story must have a title
```

**Cause**:
```turtle
:us-001 a sk:UserStory ;
    sk:priority "P1" .
# Missing sk:title
```

**Fix**:
```turtle
:us-001 a sk:UserStory ;
    sk:priority "P1" ;
    sk:title "User can log in" .  # ✅ Added
```

**3. Minimum Count Not Met**

**Error**:
```
User story must have at least one acceptance scenario
```

**Cause**:
```turtle
:us-001 a sk:UserStory ;
    sk:title "User can log in" .
# Missing sk:hasAcceptanceScenario
```

**Fix**:
```turtle
:us-001 a sk:UserStory ;
    sk:title "User can log in" ;
    sk:hasAcceptanceScenario :us-001-as-001 .  # ✅ Added

:us-001-as-001 a sk:AcceptanceScenario ;
    sk:given "..." ;
    sk:when "..." ;
    sk:then "..." .
```

**4. Wrong Data Type**

**Error**:
```
storyIndex must be an integer
```

**Cause**:
```turtle
:us-001 sk:storyIndex "1" .  # ❌ String, not integer
```

**Fix**:
```turtle
:us-001 sk:storyIndex 1 .  # ✅ Integer (no quotes)
```

**Debugging SHACL Errors**:

```bash
# Validate with verbose output
ggen validate --verbose .specify/specs/013-feature/feature.ttl

# Check SHACL shapes
cat .specify/ontology/spec-kit-schema.ttl

# Verify your ontology syntax
rapper --input turtle --count .specify/specs/013-feature/feature.ttl
```

---

### Why doesn't my code compile after generation? {#code-not-compiling}

**Common Causes**:

**1. Template Syntax Error**

**Symptom**:
```rust
error[E0425]: cannot find value `x` in this scope
```

**Cause**: Template has undefined variable

**Debug**:
```bash
# Check template for errors
vim .specify/templates/auth.tera

# Look for undefined variables
{{ undefined_variable }}  # ❌
```

**Fix**: Use correct variable from SPARQL query result

**2. SPARQL Query Returns Empty Result**

**Symptom**: Generated code missing expected structs/functions

**Cause**: SPARQL query didn't match any data

**Debug**:
```bash
# Test SPARQL query manually
ggen query .specify/specs/013-feature/feature.ttl <<EOF
PREFIX sk: <http://github.com/github/spec-kit#>
SELECT ?title WHERE { ?story sk:title ?title }
EOF
```

**Fix**: Verify ontology data matches query pattern

**3. Missing Imports**

**Symptom**:
```rust
error[E0433]: failed to resolve: use of undeclared type `User`
```

**Cause**: Template didn't generate necessary imports

**Fix**: Update template to include imports
```tera
use crate::domain::user::User;  // Add this
```

**4. Type Mismatch**

**Symptom**:
```rust
error[E0308]: mismatched types
  expected `String`, found `&str`
```

**Cause**: Template generated wrong type

**Fix**: Update template to use correct type conversion
```tera
{{ field }}.to_string()  // Convert &str to String
```

**Debugging Strategy**:

```bash
# 1. Verify ontology is valid
ggen validate .specify/specs/013-feature/feature.ttl

# 2. Check SPARQL query results
ggen query .specify/specs/013-feature/feature.ttl < query.sparql

# 3. Regenerate with force flag
ggen sync --force true --audit true

# 4. Check compiler errors
cargo make check

# 5. Review generated code
cat src/domain/auth.rs
```

---

### How do I debug ontology issues? {#debug-ontology}

**Strategy 1: SPARQL Queries**

**Find all user stories**:
```sparql
PREFIX sk: <http://github.com/github/spec-kit#>

SELECT ?story ?title ?priority
WHERE {
    ?story a sk:UserStory ;
           sk:title ?title ;
           sk:priority ?priority .
}
```

**Find user stories missing acceptance scenarios**:
```sparql
PREFIX sk: <http://github.com/github/spec-kit#>

SELECT ?story ?title
WHERE {
    ?story a sk:UserStory ;
           sk:title ?title .
    FILTER NOT EXISTS { ?story sk:hasAcceptanceScenario ?scenario }
}
```

**Find high-priority user stories**:
```sparql
PREFIX sk: <http://github.com/github/spec-kit#>

SELECT ?title ?priority
WHERE {
    ?story sk:title ?title ;
           sk:priority ?priority .
    FILTER(?priority = "P1")
}
```

**Strategy 2: Visualization**

**Generate graph visualization**:
```bash
# Install rapper (RDF parser)
brew install raptor

# Convert Turtle to DOT format
rapper -i turtle -o dot .specify/specs/013-feature/feature.ttl > graph.dot

# Render with Graphviz
dot -Tpng graph.dot -o graph.png
open graph.png
```

**Strategy 3: Incremental Validation**

**Start minimal**:
```turtle
# Start with absolutely minimal ontology
:us-001 a sk:UserStory ;
    sk:title "Test" ;
    sk:priority "P1" .

# Validate
ggen validate feature.ttl
```

**Add incrementally**:
```turtle
# Add more fields one at a time
:us-001 sk:description "Test description" .

# Validate after each addition
ggen validate feature.ttl
```

**Strategy 4: Diff Against Working Example**

```bash
# Compare your ontology to a known-good example
diff .specify/specs/013-feature/feature.ttl \
     .specify/specs/001-example/feature.ttl
```

**Tools**:
- [SPARQL Query Editor (Yasgui)](https://yasgui.triply.cc/)
- [RDF Validator](http://www.w3.org/RDF/Validator/)
- [Turtle Syntax Checker](https://www.w3.org/2015/03/ShExValidata/)

---

## Comparison with Alternatives

### How does this compare to GraphQL/Protobuf/OpenAPI? {#compare-to-alternatives}

**Comparison Matrix**:

| Feature | RDF-First (ggen) | GraphQL | Protobuf | OpenAPI |
|---------|------------------|---------|----------|---------|
| **Scope** | Full stack (requirements → code) | API schema only | Data serialization | API documentation |
| **Source of Truth** | RDF ontology | GraphQL schema | .proto files | openapi.yaml |
| **Code Generation** | Yes (Rust, TS, Python, etc.) | Yes (resolvers, types) | Yes (serialization) | Yes (client SDK) |
| **Requirements Tracing** | Yes (SPARQL queries) | No | No | No |
| **Test Generation** | Yes (from acceptance scenarios) | No | No | No |
| **Documentation Generation** | Yes (from ontology) | Yes (from schema) | Yes (from comments) | Yes (built-in) |
| **Validation** | SHACL | Schema validation | Type checking | JSON Schema |
| **Query Language** | SPARQL | GraphQL | N/A | N/A |
| **Deterministic Output** | Yes (SHA-256 verified) | No | Yes | No |
| **Cryptographic Receipts** | Yes | No | No | No |
| **Multi-Language** | Yes | Yes | Yes | Yes |
| **Learning Curve** | Medium | Medium | Low | Low |
| **Maturity** | New | Mature | Mature | Mature |

**When to Use Each**:

**RDF-First (ggen)**:
- ✅ You have formal requirements
- ✅ Spec/code drift is a problem
- ✅ Compliance requires audit trails
- ✅ Complex domain model
- ✅ Multi-language codebase

**GraphQL**:
- ✅ Building an API
- ✅ Flexible client queries needed
- ✅ Frontend-driven development
- ❌ Not for full-stack generation

**Protobuf**:
- ✅ High-performance serialization
- ✅ Cross-language data exchange
- ✅ gRPC services
- ❌ Not for requirements/docs

**OpenAPI**:
- ✅ Documenting REST APIs
- ✅ Generating client SDKs
- ✅ API-first development
- ❌ Not for business logic

**Can They Coexist?**

Yes! RDF-first can generate GraphQL/Protobuf/OpenAPI schemas:

```turtle
# Ontology defines domain model
:User a sk:Entity ;
    sk:hasField :username, :email .

# ↓ μ (transformation)

# Generate GraphQL schema
type User {
  username: String!
  email: String!
}

# Generate Protobuf
message User {
  string username = 1;
  string email = 2;
}

# Generate OpenAPI
User:
  type: object
  properties:
    username:
      type: string
    email:
      type: string
```

**Comparison Summary**: RDF-first is broader in scope (requirements through deployment), while GraphQL/Protobuf/OpenAPI focus on specific technical concerns.

---

## Team and Organizational

### How do I convince my team to try RDF-first? {#convince-team}

**Strategy 1: Start Small**

- **Don't**: Propose rewriting entire codebase
- **Do**: Propose pilot for one small feature

**Pitch**:
> "Let's try RDF-first for [specific feature]. If it doesn't work, we'll revert. If it works, we'll consider expanding."

**Strategy 2: Show, Don't Tell**

- **Don't**: Send long emails explaining RDF theory
- **Do**: Build working example and demo

**Approach**:
1. Pick a feature that has spec/code drift
2. Model it in RDF
3. Generate code
4. Show side-by-side comparison (before/after)
5. Emphasize time saved and consistency gained

**Strategy 3: Focus on Pain Points**

**If team struggles with**:
- Spec/code drift → Show how RDF-first prevents this
- Slow onboarding → Show how SPARQL queries aid understanding
- Multi-language duplication → Show one ontology, multiple projections
- Compliance overhead → Show cryptographic receipts

**Strategy 4: Metrics-Driven**

Track and share:
- Time to implement feature (RDF-first vs code-first)
- Drift incidents (before vs after)
- Onboarding time (before vs after)
- Test coverage (generated vs hand-written)

**Sample Metrics Table**:

| Metric | Before (Code-First) | After (RDF-First) | Improvement |
|--------|---------------------|-------------------|-------------|
| Feature time | 8 hours | 3 hours | 62% faster |
| Drift incidents | 3/month | 0/month | 100% reduction |
| Test coverage | 65% | 87% | +22% |

**Strategy 5: Address Concerns**

**Concern**: "Learning curve too steep"
**Response**: "Similar to learning GraphQL. 2-3 weeks to productivity. I'll pair with you."

**Concern**: "What if ggen is abandoned?"
**Response**: "Ontology is standard RDF. We can switch tools. Code-first gives us no such portability."

**Concern**: "Too much overhead for simple projects"
**Response**: "Agreed. Let's use it for complex features, keep code-first for simple CRUD."

**Resources to Share**:
- [5-minute video demo]
- [Case study with metrics](../examples/case-studies/)
- [Quick start guide](../getting-started/quick_start.md)

---

## Advanced Topics

### How do I optimize generation performance? {#optimize-performance}

**Performance SLOs** (ggen v6.0.0):
- First build: ≤ 15s
- Incremental: ≤ 2s
- RDF processing: ≤ 5s (1k+ triples)
- Generation memory: ≤ 100MB
- CLI scaffolding: ≤ 3s end-to-end

**Optimization Strategies**:

**1. Split Large Ontologies**

**Slow** ❌:
```
.specify/specs/auth/feature.ttl  (10,000 triples)
```

**Fast** ✅:
```
.specify/specs/auth/
├── feature.ttl      (100 triples - user stories only)
├── entities.ttl     (500 triples - domain model)
├── plan.ttl         (200 triples - architecture)
└── tasks.ttl        (300 triples - implementation)
```

**2. Optimize SPARQL Queries**

**Slow** ❌:
```sparql
# Cartesian product
SELECT ?story ?scenario
WHERE {
    ?story a sk:UserStory .
    ?scenario a sk:AcceptanceScenario .
}
```

**Fast** ✅:
```sparql
# Direct relationship
SELECT ?story ?scenario
WHERE {
    ?story sk:hasAcceptanceScenario ?scenario .
}
```

**3. Cache Template Results**

```bash
# First run: 5 seconds
ggen sync --audit true

# Subsequent runs: < 2 seconds (cached)
ggen sync --audit true
```

**4. Use Incremental Generation**

```bash
# Only regenerate changed files
ggen sync --incremental true
```

**5. Parallelize Generation**

```bash
# Generate multiple modules in parallel
ggen sync --parallel true --jobs 4
```

**Profiling**:

```bash
# Profile generation performance
ggen sync --profile true --audit true

# View timing breakdown
cat .ggen/receipts/latest.json | jq '.timings'
```

**Example Output**:
```json
{
  "normalize": "342μs",
  "extract": "128μs",
  "emit": "456μs",
  "canonicalize": "234μs",
  "receipt": "89μs",
  "total": "1249μs"
}
```

**Bottleneck Analysis**:
- If `normalize` is slow → Split ontology or optimize SHACL shapes
- If `extract` is slow → Optimize SPARQL queries
- If `emit` is slow → Simplify templates or reduce file count
- If `canonicalize` is slow → Reduce generated file size

**See Also**:
- [Performance Guide](../how-to/performance_optimization.md)
- [Benchmarking Guide](../how-to/benchmarking.md)

---

### Can I generate code for multiple languages? {#multiple-languages}

**Short Answer**: Yes, using language-specific templates.

**One Ontology, Many Projections**:

```turtle
# Single ontology (.specify/specs/auth/feature.ttl)
:us-001 a sk:UserStory ;
    sk:title "User can log in" ;
    sk:hasAcceptanceScenario :us-001-as-001 .

:us-001-as-001 a sk:AcceptanceScenario ;
    sk:given "User has valid credentials" ;
    sk:when "User submits login form" ;
    sk:then "System returns JWT token" .
```

**Multiple Templates**:

**.specify/templates/rust.tera**:
```tera
pub struct Login {
    pub email: String,
    pub password: String,
}

#[test]
fn test_login() { ... }
```

**.specify/templates/typescript.tera**:
```tera
export interface Login {
  email: string;
  password: string;
}

test('login', () => { ... });
```

**.specify/templates/python.tera**:
```tera
class Login:
    def __init__(self, email: str, password: str):
        self.email = email
        self.password = password

def test_login():
    ...
```

**Configuration** (ggen.toml):
```toml
[[generators]]
template = ".specify/templates/rust.tera"
output = "rust/src/domain/auth.rs"

[[generators]]
template = ".specify/templates/typescript.tera"
output = "typescript/src/domain/auth.ts"

[[generators]]
template = ".specify/templates/python.tera"
output = "python/src/domain/auth.py"
```

**Generate All**:
```bash
ggen sync --audit true
```

**Output**:
```
rust/src/domain/auth.rs         (Rust code)
typescript/src/domain/auth.ts   (TypeScript code)
python/src/domain/auth.py       (Python code)
```

**Benefits**:
- ✅ Single source of truth (ontology)
- ✅ Consistent logic across languages
- ✅ Synchronized updates (change ontology, regenerate all)
- ✅ Language-specific idioms (templates handle this)

**Example Project**: [Multi-Language API](../examples/multi_language_api/)

---

**Document Status**: Production Template (v1.0.0)
**Last Updated**: 2026-01-24
**Maintainer**: ggen Core Team
**License**: MIT
