# Complete Diataxis 2030 Example

This example shows the **complete pipeline** from RDF ontology to generated documentation.

## The Flow

```
documentation-instance.ttl (RDF)
         ↓
SPARQL Queries (Extract each quadrant)
         ↓
Query Results (Structured data)
         ↓
Tera Templates (Transform)
         ↓
Generated Documentation (All 4 quadrants)
```

## Input: RDF Ontology

**File**: `examples/documentation-instance.ttl`

Contains instances of all four Diataxis types:
- `:GettingStartedTutorial` (Learning-oriented)
- `:ParseRdfHowTo` (Problem-oriented)
- `:ParseRdfReference` (Information-oriented)
- `:RdfParsingExplanation` (Understanding-oriented)

## Step 1: Extract with SPARQL

### Tutorial Query

**File**: `queries/diataxis-tutorial.sparql`

Extracts:
- Title, description, difficulty, time
- Prerequisites and learning objectives
- Steps with code examples

### How-To Query

**File**: `queries/diataxis-howto.sparql`

Extracts:
- Problem statement and context
- Solution steps with code

### Reference Query

**File**: `queries/diataxis-reference.sparql`

Extracts:
- Function signature and parameters
- Return values and examples
- Links to code ontology (ffi:Function)

### Explanation Query

**File**: `queries/diataxis-explanation.sparql`

Extracts:
- Concepts and key ideas
- Analogies for understanding
- Design decisions with rationale

## Step 2: Transform with Tera

### Tutorial Template

**File**: `templates/tutorial.md.tera`

Generates:
```markdown
# Getting Started with ggen-node

**Difficulty**: Beginner
**Estimated Time**: 10 minutes

## Prerequisites
- Node.js >= 16.0.0
- Basic JavaScript knowledge

## What You'll Learn
- Install and set up ggen-node
- Parse your first RDF file

## Tutorial Steps

### Step 1: Install ggen-node package
...
```

### How-To Template

**File**: `templates/howto.md.tera`

Generates:
```markdown
# How to Parse RDF Files with Error Handling

## Problem
I need to parse RDF files but handle errors when files are invalid

## Solution

### 1. Wrap parseRdf in try-catch
```javascript
try {
  const triples = await parseRdf('./data.ttl');
} catch (error) {
  console.error('Parsing failed:', error.message);
}
```
...
```

### Reference Template

**File**: `templates/reference.md.tera`

Generates:
```markdown
# parseRdf() API Reference

## Signature
```typescript
parseRdf(filePath: string, options?: ParseOptions): Promise<Triple[]>
```

## Parameters

### `filePath` (required)
- **Type**: `string`
- **Description**: Path to RDF file

## Returns
Promise that resolves to array of Triple objects
...
```

### Explanation Template

**File**: `templates/explanation.md.tera`

Generates:
```markdown
# Understanding RDF Parsing in ggen-node

## Understanding RDF Graph Model

RDF represents data as a directed, labeled graph

## Key Concepts
- RDF is a graph-based data model, not a tree
- Triples are the atomic unit of RDF data

## Design Decisions

### All parsing functions are async
**Rationale:**
- File I/O blocks the Node.js event loop
- Native Rust operations may be expensive
...
```

## Output: Complete Documentation Set

```
docs/
├── tutorials/
│   ├── getting-started.md
│   └── code-generation.md
├── how-to/
│   ├── parse-rdf-with-error-handling.md
│   ├── validate-ontology.md
│   └── generate-typescript-types.md
├── reference/
│   ├── parseRdf.md
│   ├── generateCode.md
│   └── validateOntology.md
└── explanation/
    ├── rdf-parsing-architecture.md
    ├── async-design.md
    └── native-implementation.md
```

## The Magic: Same Source, Multiple Views

The **same RDF ontology** generates:

1. **Code** (`index.mjs`) - via `templates/index.mjs.tera`
2. **Tutorials** (`getting-started.md`) - via `templates/tutorial.md.tera`
3. **How-Tos** (`parse-rdf.md`) - via `templates/howto.md.tera`
4. **Reference** (`parseRdf.md`) - via `templates/reference.md.tera`
5. **Explanations** (`architecture.md`) - via `templates/explanation.md.tera`

## Key Insight

> **Code and documentation share the same source of truth (RDF ontology)**

When you:
- Add a new function → Gets auto-documented in all 4 quadrants
- Change a parameter → Reference updates automatically
- Add a use case → How-To guide auto-generates
- Clarify a concept → Explanation expands

## 2030 Enhancements

### AI-Generated Examples (Planned)

```turtle
:ParseRdfFunction
    doc:aiGenerated true ;
    doc:aiPrompt "Generate 3 real-world examples for parsing RDF" .
```

AI would generate:
- Basic usage example
- Error handling example
- Production-ready example

### Personalized Documentation

User profile: `{ experience: "beginner", goal: "web-development" }`

Diataxis 2030 personalizes:
- **Tutorials**: More detailed, web-focused examples
- **How-Tos**: Common web development problems
- **Reference**: Simplified explanations
- **Explanations**: Analogies from web dev

### Semantic Search

Users can query:
```sparql
# Find tutorials about parsing for beginners
SELECT ?tutorial ?title
WHERE {
  ?tutorial a doc:Tutorial ;
            doc:title ?title ;
            doc:difficultyLevel "beginner" .
  FILTER(CONTAINS(?title, "parsing"))
}
```

## Benefits Summary

| Aspect | Traditional | Diataxis 2030 |
|--------|-------------|---------------|
| **Source** | Manual writing | RDF ontology |
| **Maintenance** | Update each doc separately | Update ontology once |
| **Consistency** | Drift over time | Always consistent |
| **Currency** | Becomes outdated | Always current |
| **Coverage** | Gaps in docs | Complete by design |
| **Examples** | Hand-written | AI-generated |
| **Search** | Text search | Semantic queries |

## Try It Yourself

1. **Add a new function** to `examples/concrete-ontology.ttl`
2. **Run SPARQL queries** on it
3. **Apply Tera templates**
4. **Get documentation** for all 4 quadrants automatically!

---

**The future is here**: Documentation as data, not prose.
