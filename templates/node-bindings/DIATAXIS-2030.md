# Diataxis 2030: Ontology-Driven Documentation

## Vision: Documentation as Data

By 2030, documentation is not **written** - it's **generated from semantic ontologies**.

### The Four Quadrants (Evolved)

```
         LEARNING ←─────────────→ INFORMATION
            │                         │
            │                         │
      ┌─────┴─────┐           ┌───────┴───────┐
      │ TUTORIALS │           │   REFERENCE   │
      │  (AI-Gen) │           │ (Auto-Derived)│
      └───────────┘           └───────────────┘
            │                         │
            │                         │
      PRACTICAL                   THEORETICAL
            │                         │
            │                         │
      ┌─────┴─────┐           ┌───────┴───────┐
      │  HOW-TO   │           │ EXPLANATION   │
      │ (Pattern) │           │  (Semantic)   │
      └───────────┘           └───────────────┘
            │                         │
            │                         │
         PROBLEM ←─────────────→ UNDERSTANDING
```

## The 2030 Difference

### Traditional Diataxis (2020s)
- Manually written by humans
- Static markdown files
- One-size-fits-all
- Siloed from code
- Becomes outdated

### Diataxis 2030
- Generated from RDF ontology
- Dynamic, queryable knowledge graph
- Personalized by AI
- Single source of truth
- Always current

## The Stack

```
┌────────────────────────────────────────────────┐
│ RDF Ontology (Source of Truth)                │
│ - Code structure (functions, types)           │
│ - Documentation metadata (examples, guides)   │
│ - Usage patterns (common tasks)               │
│ - Conceptual models (explanations)            │
└──────────────┬─────────────────────────────────┘
               │
               ▼
┌────────────────────────────────────────────────┐
│ SPARQL Queries (Data Extraction)              │
│ - Tutorial queries: "Getting started" tasks   │
│ - How-to queries: "Solve problem X" patterns  │
│ - Reference queries: "What parameters?"       │
│ - Explain queries: "Why designed this way?"   │
└──────────────┬─────────────────────────────────┘
               │
               ▼
┌────────────────────────────────────────────────┐
│ AI Enhancement (2030 Layer)                   │
│ - Generate code examples from patterns        │
│ - Expand explanations with context            │
│ - Personalize to user's background            │
│ - Translate to any language                   │
└──────────────┬─────────────────────────────────┘
               │
               ▼
┌────────────────────────────────────────────────┐
│ Tera Templates (Rendering)                    │
│ - Tutorials: Step-by-step guides              │
│ - How-to: Problem-solution templates          │
│ - Reference: API documentation                │
│ - Explanation: Concept deep-dives             │
└──────────────┬─────────────────────────────────┘
               │
               ▼
┌────────────────────────────────────────────────┐
│ Multi-Format Output                           │
│ - Markdown (for GitHub)                       │
│ - HTML (for web)                              │
│ - JSON-LD (for semantic search)               │
│ - Interactive (for learning platforms)        │
└────────────────────────────────────────────────┘
```

## Ontology Structure

### Documentation Metadata Ontology

```turtle
@prefix doc: <https://ggen.dev/ontology/documentation#> .
@prefix ffi: <https://ggen.dev/ontology/node-ffi#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Tutorial metadata
doc:Tutorial a rdfs:Class ;
    rdfs:label "Tutorial" ;
    rdfs:comment "Learning-oriented step-by-step guide" .

doc:hasPrerequisite a rdf:Property ;
    rdfs:domain doc:Tutorial ;
    rdfs:range xsd:string .

doc:hasLearningObjective a rdf:Property ;
    rdfs:domain doc:Tutorial ;
    rdfs:range xsd:string .

# How-to metadata
doc:HowToGuide a rdfs:Class ;
    rdfs:label "How-To Guide" ;
    rdfs:comment "Problem-oriented solution guide" .

doc:solvesProblem a rdf:Property ;
    rdfs:domain doc:HowToGuide ;
    rdfs:range xsd:string .

doc:hasStep a rdf:Property ;
    rdfs:domain doc:HowToGuide ;
    rdfs:range doc:Step .

# Explanation metadata
doc:Explanation a rdfs:Class ;
    rdfs:label "Explanation" ;
    rdfs:comment "Understanding-oriented concept clarification" .

doc:explainsConcept a rdf:Property ;
    rdfs:domain doc:Explanation ;
    rdfs:range xsd:string .

doc:relatedTo a rdf:Property ;
    rdfs:domain doc:Explanation ;
    rdfs:range ffi:Function .
```

## The Four Quadrants (Generated)

### 1. Tutorials (Learning-Oriented)

**Ontology → SPARQL → Template → Tutorial**

```turtle
# In ontology
:GettingStartedTutorial a doc:Tutorial ;
    doc:title "Getting Started with ggen-node" ;
    doc:hasPrerequisite "Node.js >= 16.0.0" ;
    doc:hasPrerequisite "Basic JavaScript knowledge" ;
    doc:hasLearningObjective "Parse your first RDF file" ;
    doc:hasLearningObjective "Generate code from ontology" ;
    doc:includesFunction :ParseRdfFunction ;
    doc:estimatedTime "10 minutes" .
```

**SPARQL extracts:**
```sparql
SELECT ?title ?prerequisite ?objective ?function
WHERE {
  ?tutorial a doc:Tutorial ;
            doc:title ?title ;
            doc:hasPrerequisite ?prerequisite ;
            doc:hasLearningObjective ?objective ;
            doc:includesFunction ?function .
}
```

**Template generates:**
```markdown
# Getting Started with ggen-node

**Time**: 10 minutes
**Prerequisites**: Node.js >= 16.0.0, Basic JavaScript knowledge

## What You'll Learn
- Parse your first RDF file
- Generate code from ontology

## Step 1: Install
...
```

### 2. How-To Guides (Problem-Oriented)

**Pattern-based solutions from ontology:**

```turtle
:ParseRdfFileHowTo a doc:HowToGuide ;
    doc:title "How to Parse an RDF File" ;
    doc:solvesProblem "I need to read RDF triples from a file" ;
    doc:hasStep :InstallPackageStep ;
    doc:hasStep :ImportModuleStep ;
    doc:hasStep :CallParseStep ;
    doc:relatedTo :ParseRdfFunction .

:InstallPackageStep a doc:Step ;
    doc:order 1 ;
    doc:action "Install the package" ;
    doc:codeExample "npm install ggen-node" .
```

**Generated How-To:**
```markdown
# How to Parse an RDF File

**Problem**: I need to read RDF triples from a file

## Solution

### Step 1: Install the package
```bash
npm install ggen-node
```

### Step 2: Import the module
...
```

### 3. Reference (Information-Oriented)

**Auto-derived from code ontology:**

```turtle
# Already have this!
:ParseRdfFunction a ffi:Function ;
    ffi:functionName "parseRdf" ;
    ffi:returnType "Triple[]" ;
    ffi:async true ;
    ffi:hasParameter :FilePathParam .
```

**SPARQL extracts for API reference:**
```sparql
SELECT ?funcName ?description ?param ?paramType ?returns
WHERE {
  ?func a ffi:Function ;
        ffi:functionName ?funcName ;
        rdfs:comment ?description ;
        ffi:returnType ?returns ;
        ffi:hasParameter ?param .
  ?param ffi:paramName ?paramName ;
         ffi:type ?paramType .
}
```

**Generated Reference:**
```markdown
# API Reference

## parseRdf(filePath, options?)

Parse RDF file and return triples.

**Parameters:**
- `filePath` (string, required): Path to RDF file
- `options` (ParseOptions, optional): Parse options

**Returns:** `Promise<Triple[]>`

**Throws:** Error if parsing fails
```

### 4. Explanations (Understanding-Oriented)

**Conceptual knowledge in ontology:**

```turtle
:RdfParsingConcept a doc:Explanation ;
    doc:explainsConcept "RDF Parsing" ;
    doc:title "Understanding RDF Parsing" ;
    doc:keyIdea "RDF is a graph-based data model" ;
    doc:keyIdea "Triples are the fundamental unit" ;
    doc:relatedTo :ParseRdfFunction ;
    doc:hasAnalogy "Think of RDF like a database where relationships are first-class" .

:WhyAsyncConcept a doc:Explanation ;
    doc:explainsConcept "Async Design" ;
    doc:title "Why Functions Are Async" ;
    doc:rationale "File I/O blocks the event loop" ;
    doc:rationale "Native calls may be expensive" ;
    doc:designDecision "All I/O operations return Promises" .
```

**Generated Explanation:**
```markdown
# Understanding RDF Parsing

RDF is a graph-based data model where data is represented as triples...

**Key Concepts:**
- Triples are the fundamental unit
- Think of RDF like a database where relationships are first-class

**Design Decisions:**
- All I/O operations return Promises
- Why? File I/O blocks the event loop
```

## AI-Enhanced Generation (2030)

### Example Synthesis

**Ontology says:** "Function `parseRdf` takes `filePath` and returns `Triple[]`"

**AI generates real-world examples:**

```javascript
// Example 1: Basic usage
import { parseRdf } from 'ggen-node';

const triples = await parseRdf('./schema.ttl');
console.log(`Found ${triples.length} triples`);

// Example 2: Error handling
try {
  const triples = await parseRdf('./data.ttl', { strict: true });
  processTriples(triples);
} catch (error) {
  console.error('Parsing failed:', error.message);
}

// Example 3: Filter results
const triples = await parseRdf('./ontology.ttl');
const classes = triples.filter(t =>
  t.predicate === 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type'
);
```

**AI provides context:**
- Common use cases
- Best practices
- Gotchas to avoid
- Performance tips

### Personalization

User profile: `{ experience: "beginner", language: "en", background: "web-dev" }`

**AI adjusts:**
- Tutorials: More detailed, assumes less prior knowledge
- Examples: Web-focused scenarios
- Explanations: Relates to familiar web concepts
- Language: English with simple terminology

## Query-Driven Documentation

Users can query documentation semantically:

```sparql
# Find all functions that work with RDF files
SELECT ?func ?description
WHERE {
  ?func a ffi:Function ;
        rdfs:comment ?description ;
        ffi:hasParameter ?param .
  ?param ffi:type "string" .
  FILTER(CONTAINS(?description, "RDF"))
}
```

```sparql
# Find tutorials for beginners about parsing
SELECT ?tutorial ?title
WHERE {
  ?tutorial a doc:Tutorial ;
            doc:title ?title ;
            doc:difficultyLevel "beginner" ;
            doc:includesFunction ?func .
  ?func ffi:functionName ?name .
  FILTER(CONTAINS(?name, "parse"))
}
```

## Benefits of Diataxis 2030

| Aspect | Traditional | 2030 |
|--------|-------------|------|
| **Source** | Manual writing | Generated from ontology |
| **Currency** | Often outdated | Always current |
| **Consistency** | Variable | Perfect |
| **Personalization** | One-size-fits-all | AI-tailored |
| **Discovery** | Search text | Query semantics |
| **Maintenance** | High effort | Zero effort |
| **Languages** | Manual translation | AI translation |
| **Examples** | Hand-crafted | AI-synthesized |

## Implementation

```
templates/node-bindings/
├── ontology/
│   ├── node-ffi-ontology.ttl     # Code structure
│   └── documentation.ttl          # Doc metadata
├── queries/
│   ├── tutorials.sparql           # Extract learning paths
│   ├── howto.sparql               # Extract solution patterns
│   ├── reference.sparql           # Extract API info
│   └── explanations.sparql        # Extract concepts
├── templates/
│   ├── tutorial.md.tera           # Tutorial format
│   ├── howto.md.tera              # How-to format
│   ├── reference.md.tera          # API reference
│   └── explanation.md.tera        # Concept docs
└── generated/
    ├── docs/
    │   ├── tutorials/
    │   ├── how-to/
    │   ├── reference/
    │   └── explanation/
    └── docs.jsonld                # Semantic docs
```

## The Future Is Data

> "In 2030, nobody writes documentation.
> We describe intent in ontologies,
> AI generates examples,
> Users query for answers."

**Documentation is no longer prose - it's structured data rendered as needed.**
