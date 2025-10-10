# GGen Glossary

## Overview

This glossary defines key terms, concepts, and jargon used throughout the GGen ecosystem. Terms are organized alphabetically within categories for easy reference.

---

## Core Concepts

### AST (Abstract Syntax Tree)
A tree representation of the syntactic structure of source code. GGen can generate and manipulate ASTs for precise code generation.

### Autonomic Generation
Self-managing code generation that adapts, self-heals, and optimizes without manual intervention. See patterns 101-150.

### Context
Data provided to templates during generation, typically in JSON, YAML, or TOML format. Includes variables, configuration, and metadata.

### Contract
An explicit agreement defined by a template about what inputs it requires and what outputs it produces. See [Pattern 1: Template as Contract](../patterns/001_template_as_contract.md).

### Delta
An incremental change to code or data, tracked separately from full snapshots for efficiency. See [Pattern 4: Delta Over Snapshot](../patterns/004_delta_over_snapshot.md).

### Determinism
The property that a template always produces the same output given the same inputs. Critical for reproducible builds.

---

## Template Concepts

### Fan-Out
The process of generating multiple files or code blocks from a single template invocation, typically by iterating over collections.

**Example:**
```handlebars
{{#each entities}}
  {{> entity-file this}}
{{/each}}
```

### Freeze Block
A section of generated code marked as immutable. Once generated, freeze blocks are never regenerated, preserving hand-written customizations.

**Syntax:**
```rust
// FREEZE_START
fn custom_business_logic() {
    // This code will never be regenerated
}
// FREEZE_END
```

**See:** [Pattern 2: Freeze Block Determinism](../patterns/002_freeze_block_determinism.md)

### Frontmatter
YAML, TOML, or JSON metadata at the beginning of a template file that declares configuration, dependencies, and contract information.

**Example:**
```yaml
---
name: rust-api
version: 1.2.0
dependencies:
  - "@ggen/base"
parameters:
  required:
    - entity_name
  optional:
    - db_type: "postgres"
---
```

**See:** [Pattern 3: Frontmatter as Configuration](../patterns/003_frontmatter_as_configuration.md)

### Partial
A reusable template fragment that can be included in other templates. Promotes modularity and DRY principles.

**Example:**
```handlebars
{{> header}}
fn main() {
    // ...
}
{{> footer}}
```

### Seal Block
Similar to a freeze block, but allows regeneration only with explicit user consent. Balances immutability with flexibility.

**Syntax:**
```rust
// SEAL_START
fn generated_but_protected() {
    // Regenerated only with --force-seal flag
}
// SEAL_END
```

### Template
A reusable code generation blueprint written in a templating language (e.g., Handlebars, Tera, Jinja). Accepts context data and produces code.

---

## Package Management

### GGen Package (gpack)
A distributable unit containing templates, tests, documentation, and metadata. Published to marketplaces for reuse.

**Structure:**
```
my-template.gpack
├── ggen.toml          # Package metadata
├── template.tmpl      # Main template
├── partials/          # Reusable fragments
├── tests/             # Template tests
└── README.md          # Documentation
```

### Marketplace
A centralized repository for discovering, installing, and publishing GGen templates. Similar to npm, PyPI, or crates.io.

**Commands:**
```bash
ggen market search "rust api"
ggen market install @ggen/rust-api
ggen market publish ./my-template
```

### Namespace
A hierarchical organization scheme for templates, typically using the format `@owner/template`. Prevents naming conflicts.

**Examples:**
- `@ggen/base` - Official GGen base template
- `@myorg/internal-api` - Organization-specific template
- `@username/experiment` - Personal template

### Registry
The backend service that hosts marketplace data, handles authentication, and serves template packages.

**Default:** `https://registry.ggen.io`

### Semantic Versioning (semver)
A versioning scheme (MAJOR.MINOR.PATCH) used for templates to communicate backward compatibility and breaking changes.

**Example:**
- `1.0.0` - Initial stable release
- `1.1.0` - Added features, backward compatible
- `2.0.0` - Breaking changes

---

## Knowledge Graphs

### Entity
A distinct object or concept in the knowledge graph, typically represented by a URI and associated properties.

**Example (Turtle):**
```turtle
:User123 a :User ;
    :name "Alice" ;
    :email "alice@example.com" .
```

### Knowledge Graph
A structured representation of entities and their relationships, stored as RDF triples. Used as a data source for template generation.

### Named Graph
A subset of the knowledge graph identified by a URI, allowing isolation and versioning of graph data.

**Example:**
```turtle
GRAPH <http://my-project.org/users> {
    :User123 a :User ;
        :name "Alice" .
}
```

### Ontology
A formal specification of concepts, relationships, and constraints within a domain. Guides knowledge graph structure.

### RDF (Resource Description Framework)
A W3C standard for representing information as subject-predicate-object triples.

**Example:**
```turtle
:User123 :hasEmail "alice@example.com" .
# Subject: :User123
# Predicate: :hasEmail
# Object: "alice@example.com"
```

### SHACL (Shapes Constraint Language)
A W3C standard for validating RDF graphs against shape definitions, ensuring data integrity.

### SPARQL
A query language for RDF graphs, similar to SQL for relational databases.

**Example:**
```sparql
SELECT ?name ?email
WHERE {
    ?user a :User ;
          :name ?name ;
          :email ?email .
}
```

### Triple
The fundamental unit of RDF: a subject-predicate-object statement.

**Example:**
```turtle
:User123 :name "Alice" .
```

### Turtle
A human-readable RDF serialization format.

**See Also:** N-Triples, RDF/XML, JSON-LD

---

## Testing

### Coverage
The percentage of template code executed during tests. Higher coverage indicates more thorough testing.

### Integration Test
A test that validates complete template workflows, including file I/O, external dependencies, and multi-step generation.

### Property-Based Test
A test that generates random inputs to verify template properties and invariants hold across many scenarios.

### Snapshot Test
A test that compares generated output against a saved "known-good" snapshot. Detects unintended changes.

**Example:**
```bash
ggen template test my-template --snapshot
```

**See:** [Pattern 6: Test-First Template Development](../patterns/006_test_first_templates.md)

### Unit Test
A test that validates individual template functions or partials in isolation.

---

## Workflows

### Apply
The phase where a generation plan is executed, writing files to disk.

**Command:**
```bash
ggen project apply plan.json
```

### Generate
The process of executing a template with context data to produce code.

**Command:**
```bash
ggen project gen my-template -c context.yaml
```

### Plan
A preview of generation actions (files to create, update, or delete) before execution.

**Command:**
```bash
ggen project plan my-template -o plan.json
```

### Plan-Generate-Apply
The three-phase workflow: (1) create an execution plan, (2) generate code, (3) apply changes to the project.

---

## Advanced

### Adaptive Generation
Templates that adjust behavior based on runtime conditions, performance metrics, or user feedback. See patterns 111-125.

### Chain-of-Thought Prompting
An AI technique where the model is guided through step-by-step reasoning to improve code generation quality.

### Consensus-Based Decisions
Multi-agent systems where agents vote or negotiate to make generation decisions collectively. See pattern 144.

### Control Flow Analysis
Analysis of code execution paths to optimize generated code. See pattern 212.

### DAA (Decentralized Autonomous Agents)
Self-organizing agents that coordinate code generation without central control. See patterns 141-150.

### Data Flow Analysis
Tracking how data moves through code to improve generation accuracy. See pattern 213.

### Differential Privacy
Techniques to generate code from private data without revealing sensitive information. See pattern 251.

### Federated Learning
Training generation models across multiple organizations without sharing raw data. See pattern 248.

### Few-Shot Learning
AI generation guided by a small number of examples. See pattern 233.

### Homomorphic Generation
Generating code on encrypted data. See pattern 249.

### JIT (Just-In-Time) Generation
Generating code on-demand at runtime rather than ahead-of-time. See pattern 204.

### LLM (Large Language Model)
AI models (e.g., GPT, Claude) used to generate or assist with code generation. See patterns 231-245.

### Meta-Learning
Systems that learn how to improve their own learning strategies. See pattern 138.

### Metaprogramming
Code that generates or manipulates other code. See patterns 201-215.

### Ontology-Driven Generation
Generating code based on formal ontologies. See pattern 216.

### Partial Evaluation
Evaluating parts of templates at compile-time for performance. See pattern 207.

### Provenance
The origin and history of generated code or data. See pattern 64.

### RAG (Retrieval-Augmented Generation)
Combining information retrieval with generation for better results. See pattern 237.

### Reflection
Analyzing existing code structure to guide generation. See pattern 209.

### Reinforcement Learning
Improving templates through trial-and-error feedback. See pattern 134.

### Self-Critique
AI that evaluates and improves its own generated code. See pattern 235.

### Staging
Multi-stage template evaluation for complex scenarios. See pattern 208.

### Swarm Intelligence
Coordinating multiple agents to solve complex generation tasks. See pattern 146.

### Transfer Learning
Applying knowledge from one domain to improve generation in another. See pattern 133.

---

## Infrastructure

### CI/CD Integration
Running template generation automatically in continuous integration/deployment pipelines.

**Commands:**
```bash
ggen ci check
ggen ci plan -o plan.json
ggen ci apply plan.json
```

### Docker Integration
Executing template generation in containerized environments for reproducibility.

### IDE Integration
Editor plugins that provide template generation, autocomplete, and validation in development environments.

### LSP (Language Server Protocol)
A protocol for providing IDE features (autocomplete, diagnostics, etc.) for template languages. See pattern 187.

### Webhook Trigger
Automated template generation triggered by external events (e.g., GitHub push, database change). See pattern 184.

---

## Miscellaneous

### Audit
Security and compliance analysis of templates and generated code.

**Command:**
```bash
ggen audit --severity error
```

### Backward Compatibility
The property that newer template versions can generate code compatible with older versions.

### Breaking Change
A modification to a template that changes its contract or output in incompatible ways, requiring a major version bump.

### Dry Run
Previewing generation results without writing files to disk.

**Flag:**
```bash
ggen project gen my-template --dry-run
```

### REPL (Read-Eval-Print Loop)
An interactive shell for developing and testing templates.

**Command:**
```bash
ggen shell -t my-template
```

### Scaffolding
Generating boilerplate project structure and files to kickstart development.

**Example:**
```bash
ggen template new my-api --scaffold
```

### Validation
Checking templates for syntax errors, missing dependencies, or contract violations.

**Command:**
```bash
ggen template validate my-template --strict
```

### Watch Mode
Automatically re-running generation or tests when files change.

**Flag:**
```bash
ggen project test --watch
```

---

## Acronyms

- **AI**: Artificial Intelligence
- **API**: Application Programming Interface
- **AST**: Abstract Syntax Tree
- **CI/CD**: Continuous Integration / Continuous Deployment
- **CLI**: Command-Line Interface
- **CRDT**: Conflict-Free Replicated Data Type
- **DAA**: Decentralized Autonomous Agents
- **DRY**: Don't Repeat Yourself
- **GNN**: Graph Neural Network
- **IDE**: Integrated Development Environment
- **JIT**: Just-In-Time
- **JSON**: JavaScript Object Notation
- **JSON-LD**: JSON for Linking Data
- **LSP**: Language Server Protocol
- **LLM**: Large Language Model
- **OWL**: Web Ontology Language
- **RAG**: Retrieval-Augmented Generation
- **RDF**: Resource Description Framework
- **REPL**: Read-Eval-Print Loop
- **REST**: Representational State Transfer
- **SARIF**: Static Analysis Results Interchange Format
- **SHACL**: Shapes Constraint Language
- **SPARQL**: SPARQL Protocol and RDF Query Language
- **TDD**: Test-Driven Development
- **TOML**: Tom's Obvious, Minimal Language
- **TTL**: Turtle (Terse RDF Triple Language)
- **URI**: Uniform Resource Identifier
- **YAML**: YAML Ain't Markup Language

---

## Related Resources

- [Full Pattern Language](full_pattern_language.md) - Index of all 253 patterns
- [CLI Reference](cli_reference.md) - Complete command reference
- [Pattern Documentation](../patterns/) - Detailed pattern descriptions

---

**Contributions Welcome:**
If you encounter undefined terms or think a term is missing, please submit an issue or pull request to the GGen repository.
