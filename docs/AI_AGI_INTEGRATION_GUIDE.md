# AI/AGI Integration with ggen: Building Intelligent Code Generators

> **This guide shows how to use ggen as the foundation for AI-powered and agentic code generation systems.** Learn to build systems where agents generate code, learn from output, and continuously improve.

## The Vision: Code Generation as a Feedback Loop

Traditional code generation is **one-shot**: Write spec → Generate code → Done.

Intelligent code generation is **continuous**: Write spec → Generate → Learn → Improve → Repeat.

```
┌─────────────────────────────────────────────────────────────┐
│                   AGENT-DRIVEN WORKFLOW                      │
├─────────────────────────────────────────────────────────────┤
│                                                               │
│  1. SPECIFICATION          2. GENERATION       3. VALIDATION │
│     (Natural Language)        (ggen Sync)        (Testing)   │
│                                                               │
│  "Build a REST API"     →  Ontology + Code  →  Run tests    │
│                                                               │
│  ↑────────────────────────────────────────────────────────↓  │
│  │                                                             │
│  │           4. FEEDBACK (Agent Learning)                    │
│  │              Parse test output → Refine spec             │
│  │                                                             │
│  └─────────────────────────────────────────────────────────→ │
│                                                               │
│  Loop N times until: Tests pass + Coverage >80% + No debt   │
│                                                               │
└─────────────────────────────────────────────────────────────┘
```

---

## Core Principle: Ontology-Driven Agents

An **ontology-driven agent** understands the semantic structure of your system:

```
Traditional Agent:           Ontology-Driven Agent:
─────────────────────────    ──────────────────────
Read documentation     →     Query RDF ontology
Guess patterns         →     Execute SPARQL rules
Hope code compiles     →     Validate with SHACL
Cross fingers          →     Generate with reasoning
```

**Ontology-driven agents have:**
1. Machine-readable specs (RDF)
2. Logical inference rules (SPARQL CONSTRUCT)
3. Formal validation (SHACL)
4. Deterministic generation (Tera templates)
5. Feedback mechanisms (test results → spec refinement)

---

## Architecture: 4-Layer Agentic System

### Layer 1: Understanding (Semantic Analysis)

**Agent task:** Parse natural language requirements into RDF ontology.

```
Input:  "Create a user management system with JWT auth and role-based access"

Process:
  1. LLM extracts concepts: User, JWT, Auth, Role, Access
  2. Creates RDF ontology with entities and relationships
  3. Validates against SHACL schema
  4. Returns structured spec

Output: user-auth.ttl (RDF ontology file)
```

**How ggen helps:**
- RDF graph is the single source of truth
- SHACL validation ensures ontology quality
- Agents can reason about relationships

**Example:**

```python
# Agent: Natural Language → RDF
from anthropic import Anthropic
import rdflib

client = Anthropic()

def spec_to_ontology(requirement: str) -> str:
    """Convert user requirement to RDF ontology using Claude."""

    prompt = f"""
    Convert this requirement to an RDF/Turtle ontology:

    Requirement: {requirement}

    Use this template:
    - Classes: Entity types mentioned
    - Properties: Attributes and relationships
    - Constraints: Business rules (if mentioned)

    Return valid Turtle syntax.
    """

    message = client.messages.create(
        model="claude-opus-4-5",
        max_tokens=2048,
        messages=[{"role": "user", "content": prompt}]
    )

    ontology_ttl = message.content[0].text

    # Validate with SHACL
    graph = rdflib.Graph()
    graph.parse(data=ontology_ttl, format="turtle")

    # In real usage, run SHACL validation here
    # shacl_graph = rdflib.Graph()
    # shacl_graph.parse("schema.ttl")
    # conforms, report = validate(graph, shacl_graph=shacl_graph)

    return ontology_ttl

# Usage
requirement = "User system with JWT auth and admin roles"
ontology = spec_to_ontology(requirement)
print(ontology)
```

### Layer 2: Generation (Code Synthesis)

**Agent task:** Call `ggen sync` with ontology, get generated code.

```
Input:  user-auth.ttl (from Layer 1)
        ggen.toml (configuration)
        templates/

Process:
  1. ggen loads ontology
  2. Runs inference rules (CONSTRUCT)
  3. Renders templates
  4. Validates with poka-yoke

Output: src/generated/ (code files)
```

**How ggen helps:**
- Deterministic generation (same spec → same code)
- Inference rules enable intelligent generation
- Poka-yoke prevents accidental overwrites

**Example:**

```python
# Agent: Trigger code generation
import subprocess
import json

def generate_code(ontology_path: str, config_path: str) -> dict:
    """Run ggen sync and capture results."""

    result = subprocess.run(
        [
            "ggen", "sync",
            "--config", config_path,
            "--format", "json",  # Machine-readable output
            "--audit"  # Generate audit trail
        ],
        capture_output=True,
        text=True,
        cwd="/path/to/project"
    )

    if result.returncode != 0:
        return {
            "success": False,
            "error": result.stderr,
            "generated": []
        }

    # Parse JSON output
    output = json.loads(result.stdout)

    return {
        "success": True,
        "generated_files": output["files"],
        "audit_trail": output["audit"],
        "timestamp": output["timestamp"]
    }
```

### Layer 3: Validation (Testing & Learning)

**Agent task:** Run tests, parse results, extract feedback.

```
Input:  Generated code (from Layer 2)

Process:
  1. Run test suite
  2. Parse test failures
  3. Extract error messages
  4. Map errors to spec issues
  5. Generate improvements

Output: Feedback list (what to fix)
```

**How ggen helps:**
- Audit trails show what was generated
- Deterministic output makes testing predictable
- SPARQL queries can analyze spec completeness

**Example:**

```python
# Agent: Validate and extract feedback
import subprocess
import re

def validate_generated_code(project_path: str) -> dict:
    """Run tests and extract actionable feedback."""

    # Run tests
    result = subprocess.run(
        ["cargo", "test"],
        capture_output=True,
        text=True,
        cwd=project_path
    )

    feedback = {
        "tests_passed": False,
        "errors": [],
        "coverage": 0.0,
        "suggestions": []
    }

    if result.returncode == 0:
        feedback["tests_passed"] = True
    else:
        # Parse test failures
        for line in result.stderr.split("\n"):
            if "error[" in line:
                feedback["errors"].append(line)

    # Extract coverage
    coverage_match = re.search(r"coverage: (\d+\.\d+)%", result.stdout)
    if coverage_match:
        feedback["coverage"] = float(coverage_match.group(1))

    # Generate improvement suggestions
    if not feedback["tests_passed"]:
        if "trait" in "\n".join(feedback["errors"]):
            feedback["suggestions"].append(
                "Missing trait implementation - add to domain/"
            )
        if "lifetime" in "\n".join(feedback["errors"]):
            feedback["suggestions"].append(
                "Lifetime issues - review property definitions"
            )

    if feedback["coverage"] < 80:
        feedback["suggestions"].append(
            f"Coverage {feedback['coverage']}% < 80% target"
        )

    return feedback
```

### Layer 4: Refinement (Spec Improvement)

**Agent task:** Update ontology based on test feedback.

```
Input:  Feedback from Layer 3

Process:
  1. Parse feedback messages
  2. Identify spec deficiencies
  3. Update RDF ontology
  4. Re-run generation (go to Layer 2)

Output: Improved ontology → loop back to Layer 2
```

**How ggen helps:**
- RDF is machine-readable (agents can modify it)
- SPARQL allows agents to query and update graphs
- Inference rules can be adjusted based on feedback

**Example:**

```python
# Agent: Refine spec based on feedback
from rdflib import Graph, Namespace, RDF, RDFS, Literal

def refine_ontology(
    ontology_path: str,
    feedback: dict,
    agent_client
) -> str:
    """Update ontology based on test feedback."""

    # Load current ontology
    graph = Graph()
    graph.parse(ontology_path, format="turtle")

    # Ask Claude to suggest improvements
    prompt = f"""
    Based on these test failures:
    {feedback['errors']}

    And coverage gap:
    Coverage {feedback['coverage']}%

    Suggest SPARQL CONSTRUCT or RDF additions to fix:
    1. Add missing constraints
    2. Fix type definitions
    3. Add validation rules

    Return updated Turtle.
    """

    message = agent_client.messages.create(
        model="claude-opus-4-5",
        max_tokens=2048,
        messages=[{"role": "user", "content": prompt}]
    )

    improved_ttl = message.content[0].text

    # Write back
    with open(ontology_path, "w") as f:
        f.write(improved_ttl)

    return improved_ttl

# Main loop
def agentic_generation_loop(
    requirement: str,
    max_iterations: int = 5
) -> dict:
    """Run full 4-layer loop until tests pass."""

    results = {
        "iterations": 0,
        "final_ontology": None,
        "final_code": None,
        "success": False
    }

    # Layer 1: Generate ontology
    ontology = spec_to_ontology(requirement)

    for iteration in range(max_iterations):
        results["iterations"] += 1

        # Layer 2: Generate code
        gen_result = generate_code(ontology, "ggen.toml")
        if not gen_result["success"]:
            print(f"Generation failed: {gen_result['error']}")
            break

        # Layer 3: Validate
        feedback = validate_generated_code(".")

        if feedback["tests_passed"] and feedback["coverage"] >= 80:
            results["success"] = True
            results["final_code"] = gen_result
            break

        print(f"Iteration {iteration}: Tests {'✓' if feedback['tests_passed'] else '✗'}, "
              f"Coverage {feedback['coverage']}%")

        # Layer 4: Refine
        if iteration < max_iterations - 1:
            ontology = refine_ontology(ontology, feedback, client)

    results["final_ontology"] = ontology

    return results

# Usage
result = agentic_generation_loop(
    "Build a REST API with User and Post entities, JWT auth, admin roles"
)

print(f"Success: {result['success']}")
print(f"Iterations: {result['iterations']}")
```

---

## Advanced Pattern: Multi-Agent Orchestration

For complex systems, use **multiple specialized agents**:

```
┌──────────────────────────────────────────────────────────────┐
│                 MULTI-AGENT SYSTEM                            │
├──────────────────────────────────────────────────────────────┤
│                                                                │
│  Requirements Agent      Schema Agent      Code Agent        │
│  ──────────────────      ────────────      ──────────        │
│  - Parse user stories    - Design DB       - Generate code   │
│  - Extract entities        schema          - Write tests     │
│  - Extract behaviors     - Validate        - Optimize perf   │
│                            against rules                     │
│         ↓                    ↓                  ↓             │
│  ┌──────────────────────────────────────────────────────┐   │
│  │           Shared RDF Graph (Source of Truth)         │   │
│  │  - Entity definitions                                 │   │
│  │  - Relationships                                      │   │
│  │  - Constraints                                        │   │
│  │  - Inference rules                                    │   │
│  └──────────────────────────────────────────────────────┘   │
│         ↑                    ↑                  ↑             │
│  - Read & update   - Read & validate   - Read & generate   │
│  - SPARQL queries  - SHACL validation  - Render templates  │
│                                                                │
└──────────────────────────────────────────────────────────────┘
```

**Benefits:**
1. **Separation of concerns** - Each agent focuses on one task
2. **Parallel execution** - Agents can work simultaneously
3. **Shared source of truth** - RDF graph is the contract
4. **Composable** - Add/remove agents without breaking others

---

## Integration Patterns

### Pattern A: CI/CD Integration (Automated Code Generation)

```yaml
# .github/workflows/generate.yml
name: Auto-Generate Code

on: [push]

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Check for spec changes
        run: |
          git diff HEAD~1 -- 'ontology/*.ttl' | grep -q '@@' && \
          echo "REGEN=true" >> $GITHUB_ENV || \
          echo "REGEN=false" >> $GITHUB_ENV

      - name: Generate code (if specs changed)
        if: env.REGEN == 'true'
        run: |
          ggen sync \
            --validate-only \
            --format json \
            --audit > /tmp/audit.json

      - name: Verify tests pass
        run: cargo test

      - name: Commit generated code
        if: env.REGEN == 'true'
        run: |
          git add src/generated/
          git commit -m "chore: regenerate code from ontology changes"
          git push
```

### Pattern B: Development Loop (Watch Mode)

```bash
# In development:
ggen sync --watch

# Output:
# ✓ Loaded ontology: 5 files
# ✓ Loaded templates: 8 files
# Watching for changes...
#
# [09:15:23] Modified: ontology/domain.ttl
# [09:15:23] → Regenerating...
# [09:15:24] ✓ Updated src/generated/domain.rs
# [09:15:24] ✓ Tests pass (142 tests in 0.8s)
# [09:15:24] Ready for next change...
```

### Pattern C: Agentic Refinement (Continuous Improvement)

```python
# agent_loop.py - Runs continuously
while not_converged():
    # Analyze current spec
    metrics = analyze_ontology()

    if metrics.coverage < 80:
        # Ask Claude to expand spec
        improvements = claude.suggest_improvements(metrics)
        apply_improvements(improvements)

        # Regenerate
        subprocess.run(["ggen", "sync"])

        # Re-test
        test_results = run_tests()

        print(f"Iteration {i}: Coverage {test_results.coverage}%")
```

---

## Example: Building a Blog System

### Step 1: Natural Language Requirement

```
Build a blog system with:
- Users (registration, login, profiles)
- Posts (create, edit, delete, publish)
- Comments (nested, moderation)
- Search (full-text over posts)
- Authentication (JWT)
- Authorization (authors can edit own posts, admins can moderate)
```

### Step 2: Agent Generates Ontology

Claude (or your agent) converts to RDF:

```turtle
@prefix ex: <https://blog.example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Entities
ex:User a rdfs:Class ;
    rdfs:label "User" ;
    rdfs:comment "Blog user with authentication" .

ex:Post a rdfs:Class ;
    rdfs:label "Post" ;
    rdfs:comment "Blog post by a user" .

ex:Comment a rdfs:Class ;
    rdfs:label "Comment" ;
    rdfs:comment "Comment on a post" .

# Relationships
ex:author a rdf:Property ;
    rdfs:domain ex:Post ;
    rdfs:range ex:User ;
    rdfs:label "author" .

ex:replies_to a rdf:Property ;
    rdfs:domain ex:Comment ;
    rdfs:range ex:Comment ;
    rdfs:label "replies to" .

# Capabilities
ex:User :canCreate ex:Post .
ex:User :canRead ex:Post .
ex:User :canUpdate ex:Post .  # own posts only
ex:User :canDelete ex:Comment .  # own comments only

ex:Admin a rdfs:Class ;
    rdfs:subClassOf ex:User .
ex:Admin :canDelete ex:Post .
ex:Admin :canDelete ex:User .

# Constraints
ex:PostShape a sh:NodeShape ;
    sh:targetClass ex:Post ;
    sh:property [
        sh:path ex:title ;
        sh:datatype xsd:string ;
        sh:minLength 5 ;
        sh:maxLength 200 ;
    ] ;
    sh:property [
        sh:path ex:content ;
        sh:datatype xsd:string ;
        sh:minLength 10 ;
    ] ;
    sh:property [
        sh:path ex:status ;
        sh:in ( "draft" "published" "archived" ) ;
    ] .
```

### Step 3: Agent Runs ggen

```bash
ggen sync
```

Generates:
- `src/generated/models.rs` - User, Post, Comment structs
- `src/generated/api.rs` - REST endpoints
- `src/generated/db.rs` - Database queries
- `src/generated/auth.rs` - JWT validation
- `src/generated/permissions.rs` - Authorization checks

### Step 4: Tests Run

```
test result: FAILED. 85 passed; 8 failed

Failures:
  - test_user_cannot_update_others_post (permission check missing)
  - test_comment_nesting (recursive delete not implemented)
  - test_search_performance (index missing)
```

### Step 5: Agent Analyzes Failures

- **Missing permission check**: Add to ontology → SPARQL rule → regenerate
- **Recursive delete**: Add property to Comment → regenerate
- **Search**: Add full-text search property → regenerate

### Step 6: Loop Back to Step 3

```
Iteration 1: 85/93 tests pass (91%)
Iteration 2: 91/93 tests pass (98%) - Added permission checks
Iteration 3: 93/93 tests pass (100%) - Fixed recursive delete
Success! Generated blog system is ready.
```

---

## Key Benefits of Agentic Code Generation

| Benefit | How ggen Enables It |
|---------|-------------------|
| **Determinism** | Same spec always generates identical code |
| **Testability** | Code gen is testable; inference rules are testable |
| **Feedback loops** | Audit trails show what was generated and why |
| **Reasoning** | RDF + SPARQL enable semantic reasoning |
| **Composability** | Inference rules can be chained and reordered |
| **Transparency** | Generated code is readable, not magic |
| **Debuggability** | SPARQL queries explain why code was generated a certain way |
| **Optimization** | Agents can profile and improve templates over time |

---

## Getting Started

1. **Define your domain ontology** (RDF/Turtle)
2. **Write 2-3 inference rules** (SPARQL CONSTRUCT)
3. **Create code generation templates** (Tera)
4. **Build a simple agent loop** (Python + Claude API)
5. **Iterate**: Generate → Test → Refine → Repeat

See [SPARQL_INFERENCE_GUIDE.md](./SPARQL_INFERENCE_GUIDE.md) for detailed inference rule patterns.

---

## Advanced Topics

- **Multi-stage inference** - Chain rules for complex generation
- **Performance optimization** - Profile and optimize SPARQL queries
- **Package marketplace** - Share reusable ontologies and templates
- **Knowledge graphs** - Build larger knowledge bases over time
- **Continuous learning** - Agents improve templates based on historical success

The future of code generation is **agentic, semantic, and continuous.**

