# How to Use This Pattern Language

> *"The patterns are not isolated. They form a language. And the language is generative: it allows you to create new, coherent configurations by combining patterns in meaningful ways."*

---

## Reading the Patterns

Each pattern in this book represents a recurring solution to a recurring problem in code synchronization. The patterns are not rules to follow blindly—they are **relationships** between context, forces, and resolution.

### The Pattern Structure

Every pattern follows this form:

```
PATTERN NAME [confidence stars]

[An evocative image or opening that sets the scene]

Context:
    The situation where this pattern applies

❖ ❖ ❖

Problem:
    The forces that create tension

Therefore:
    The action that resolves the forces

❖ ❖ ❖

Connections:
    Related patterns that support or extend this one

Implementation:
    How ggen sync realizes this pattern
```

---

## The Confidence Stars

Each pattern name is followed by one to three stars:

| Stars | Meaning |
|-------|---------|
| `*` | **Emerging** — This pattern is promising but still evolving. Use it, but expect refinement. |
| `**` | **Established** — This pattern has proven itself in practice. You can rely on it. |
| `***` | **Fundamental** — This pattern is essential to the language. Without it, the system loses coherence. |

The confidence level reflects how deeply the pattern is woven into the fabric of ggen sync, not how "important" it is. An emerging pattern may solve a critical problem; a fundamental pattern may seem obvious.

---

## Navigating the Language

### By Need

| If you need to... | Start with... |
|-------------------|---------------|
| Understand what ggen sync does | [THE SINGLE COMMAND](patterns/01-single-command.md) |
| Configure a project | [MANIFEST AS TRUTH](patterns/02-manifest-as-truth.md) |
| Understand the architecture | [THREE-LAYER ARCHITECTURE](patterns/03-three-layer-architecture.md) |
| Work with RDF ontologies | [ONTOLOGY LOADING](patterns/04-ontology-loading.md) |
| Add derived knowledge | [INFERENCE ENRICHMENT](patterns/05-inference-enrichment.md) |
| Define what gets generated | [GENERATION RULES](patterns/06-generation-rules.md) |
| Preview changes safely | [DRY RUN](patterns/08-dry-run.md) |
| Ensure reproducibility | [DETERMINISTIC OUTPUT](patterns/13-deterministic-output.md) |
| Debug failures | [ERROR SIGNALS](patterns/12-error-signals.md) |

### By Layer

The patterns are organized in concentric layers, from the outermost context to the innermost detail:

```
┌─────────────────────────────────────────────────┐
│  Foundation Patterns (1-3)                       │
│  ┌─────────────────────────────────────────┐    │
│  │  Knowledge Patterns (4-7)                │    │
│  │  ┌─────────────────────────────────┐    │    │
│  │  │  Safety Patterns (8-12)          │    │    │
│  │  │  ┌─────────────────────────┐    │    │    │
│  │  │  │  Integrity Patterns     │    │    │    │
│  │  │  │  (13-15)                │    │    │    │
│  │  │  └─────────────────────────┘    │    │    │
│  │  └─────────────────────────────────┘    │    │
│  └─────────────────────────────────────────┘    │
└─────────────────────────────────────────────────┘
```

Start at the outer layer to understand the whole, then move inward for details.

---

## Applying the Patterns

### Pattern as Conversation

A pattern is not a command—it is a **conversation** between you and the system. When you read:

> *Therefore: Execute the pipeline as a single atomic operation.*

This is not saying "you must." It is saying "when you find yourself in this context, with these forces, this is what brings resolution."

You may find yourself in a different context. The forces may be different. The pattern may not apply, or may apply differently.

### Patterns Compose

The patterns in this language are designed to work together. A single ggen sync invocation may activate many patterns simultaneously:

- **THE SINGLE COMMAND** provides the entry point
- **MANIFEST AS TRUTH** declares configuration
- **ONTOLOGY LOADING** brings in domain knowledge
- **INFERENCE ENRICHMENT** derives new facts
- **GENERATION RULES** transforms knowledge to code
- **DETERMINISTIC OUTPUT** ensures reproducibility
- **AUDIT TRAIL** records what happened

Each pattern does its work. Together, they create wholeness.

### When Patterns Conflict

Occasionally, two patterns may seem to pull in different directions. For example:

- **FORCE OVERWRITE** says "write anyway"
- **DRY RUN** says "don't write at all"

These are not conflicts—they are **choices**. The patterns exist to give you options. The `--force` flag invokes one pattern; the `--dry-run` flag invokes another. You choose based on your context.

---

## A Path Through the Language

For a first reading, follow this sequence:

1. **[THE SINGLE COMMAND](patterns/01-single-command.md)** — The unified entry point
2. **[MANIFEST AS TRUTH](patterns/02-manifest-as-truth.md)** — Configuration as declaration
3. **[ONTOLOGY LOADING](patterns/04-ontology-loading.md)** — Bringing knowledge in
4. **[GENERATION RULES](patterns/06-generation-rules.md)** — Transforming knowledge to code
5. **[DETERMINISTIC OUTPUT](patterns/13-deterministic-output.md)** — Reproducibility as foundation
6. **[DRY RUN](patterns/08-dry-run.md)** — Safe exploration
7. **[ERROR SIGNALS](patterns/12-error-signals.md)** — Understanding failures

This path takes you from "what is sync?" through "how does it work?" to "how do I use it safely?"

---

## The Living Language

This pattern language is not complete. As ggen sync evolves, new patterns will emerge. Some patterns may gain confidence; others may fade. This is natural.

The language lives in the practice of using it. When you use ggen sync, you are speaking this language. When you find a new pattern, you extend it.

Begin with what is written. End with what you discover.
