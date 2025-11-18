# Hyper-Thesis Framework (HTF) CLI

A sophisticated RDF-backed thesis planning tool implementing the unified μ-architecture framework that blends IMRaD, Papers, Argument, Contribution, Monograph, DSR, and Narrative modes.

## Overview

The HTF framework organizes thesis research into **Δ-Shards** (atomic units of work) across 26 canonical families, enforced through:

- **Λ-Scheduler**: Maps shards into chapters respecting a total-order chain
- **Π-Profiler**: Shows coverage across all research categories
- **Γ-Checker**: Validates consistency against Q-invariants

## Features

### 1. **Λ-Scheduling (Chapter Planning)**
```bash
htf schedule
# or with custom chapter size:
htf schedule --chapter-size 2000
```

Plans chapters by:
- Applying canonical Λ-total order to research shards
- Grouping shards into chapters of target word count
- Preserving ordering dependencies

**Example Output:**
```json
{
  "thesis_id": "97c40fdc-d2fa-4ace-8adc-02c0d804cc58",
  "chapters": 2,
  "total_shards": 6,
  "total_words": 5870,
  "chapters_detail": [
    {
      "number": 1,
      "title": "Chapter 1: Intro",
      "shards": 3,
      "words": 2940,
      "families": ["Intro", "Gap", "Problem"]
    },
    {
      "number": 2,
      "title": "Chapter 2: Method",
      "shards": 3,
      "words": 2930,
      "families": ["Method", "Evaluation", "Artifact"]
    }
  ]
}
```

### 2. **Π-Profiling (Coverage Analysis)**
```bash
htf profile
```

Shows your thesis coverage across all 26 Δ-families:
- Word count per family
- Coverage percentages
- Highlights missing families

**Example Output:**
```
=== HTF Coverage Report ===

Total Words: 2800

Coverage by Family:
  Intro            | ████████████    | 17.9%
  Method           | ███████████     | 17.9%
  Artifact         | ███████████     | 16.1%
  ...

Uncovered Families:
  - Paper
  - Synthesis
  - Canon
```

### 3. **Γ-Checking (Invariant Validation)**
```bash
htf check
```

Validates your thesis against Q-invariants:
- **AllFamiliesCovered**: All 26 families represented
- **NoCyclicDependencies**: Shards form DAG (no cycles)
- **TotalOrderPreserved**: Λ-ordering respected
- **ContentNotEmpty**: All shards have text
- **StatusConsistent**: Valid status transitions

**Example Output:**
```json
{
  "is_valid": true,
  "passed": [
    "AllFamiliesCovered",
    "NoCyclicDependencies",
    "TotalOrderPreserved",
    "ContentNotEmpty",
    "StatusConsistent"
  ],
  "failed": [],
  "drift": [],
  "recommendations": []
}
```

## The 26 Δ-Families

### IMRaD (4)
- `Intro` - Introduction
- `Method` - Research methodology
- `Result` - Empirical results
- `Discussion` - Analysis & interpretation

### Thesis-by-Papers (2)
- `Paper` - Individual research papers
- `Synthesis` - Integration across papers

### Argument (5)
- `Claim` - Primary thesis claim
- `Ground` - Foundational support
- `Proof` - Formal/empirical evidence
- `Objection` - Counter-arguments
- `Reply` - Responses to objections

### Contribution (4)
- `Gap` - Research gap identified
- `Design` - Solution design
- `Evaluation` - Results/validation
- `Impact` - Significance & broader implications

### Monograph (5)
- `Context` - Historical/disciplinary context
- `Canon` - Relevant literature
- `Analysis` - Deep analysis
- `Conclusion` - Synthesis
- `Problem` - Problem statement

### DSR (Design Science Research) (3)
- `Problem` - Problem definition
- `Artifact` - Designed solution
- `Theory` - Theoretical contributions

### Narrative (3)
- `Field` - Field/discipline framing
- `Voice` - Authorial voice/positioning
- `Pattern` - Pattern recognition
- `Insight` - Emergent insights

## Λ-Total Order (Canonical Ordering)

The framework defines a strict ordering:

```
Problem → Gap → Claim → Intro → Method → Context → Voice → Canon → Field
→ Artifact → Proof → Paper → Result → Evaluation → Objection → Discussion
→ Reply → Pattern → Theory → Analysis → Synthesis → Insight → Impact
→ Design → Ground → Conclusion
```

This ordering ensures:
1. **Logical flow**: Problem leads to solution
2. **Evidence hierarchy**: Claims precede proofs
3. **Interpretation**: Results before discussion
4. **Synthesis**: Individual contributions before impact

## Usage Examples

### List all shards in your thesis
```bash
htf list
```

### Add a new shard
```bash
htf add "Introduction section" Intro
```

### Export thesis
```bash
htf export --format json
```

## Architecture

### Core Modules

- **`models.rs`** - Data structures (DeltaShard, Invariant, ChapterPlan)
- **`ontology.rs`** - RDF schema and Λ-ordering definition
- **`scheduler.rs`** - Λ-scheduler (chapter planning)
- **`profiler.rs`** - Π-profiler (coverage analysis)
- **`checker.rs`** - Γ-checker (invariant validation)

### Key Algorithms

**Λ-Scheduler**:
- Topological sort of shards by family order
- Greedy bin-packing into chapters
- Word-count aware chapter sizing

**Π-Profiler**:
- Category coverage calculation
- Missing family detection
- Statistical summaries

**Γ-Checker**:
- Cycle detection (DFS-based)
- Status consistency validation
- Order preservation verification

## Q-Invariants

The framework enforces 5 core invariants:

1. **AllFamiliesCovered** - All 26 families must be represented
2. **NoCyclicDependencies** - Dependencies form a DAG
3. **TotalOrderPreserved** - Λ-ordering is respected
4. **ContentNotEmpty** - All shards have non-empty content
5. **StatusConsistent** - Valid status transitions (Draft → Progress → Review → Final)

## τ-Evolution (Draft to Final)

Shards progress through statuses:
```
Draft → InProgress → Review → Final
```

The Γ-checker ensures dependencies respect status:
- Final shards can't depend on Draft shards
- Status transitions follow valid paths

## RDF Backing

The framework is backed by RDF (via oxigraph):

```turtle
:DeltaShard a owl:Class ;
    rdfs:label "Δ-Shard"@en ;
    rdfs:comment "Atomic unit of thesis work"@en .

:lambdaOrder a rdf:Property ;
    rdfs:label "Λ-Order"@en ;
    rdfs:domain :DeltaShard ;
    rdfs:range :DeltaShard .
```

## Testing

Run unit tests:
```bash
cargo test -p htf-cli
```

Tests cover:
- Λ-scheduling with various shard counts
- Π-profiling coverage calculations
- Γ-checker invariant validation
- Cycle detection in dependencies
- Status consistency rules

## Future Enhancements

- **RDF Store Persistence**: Save/load theses to oxigraph backend
- **Merge Strategy**: Π-merge operations for combining research
- **Globalization**: Γ-globalization creating unified thesis view
- **SPARQL Queries**: Query thesis structure via semantic web
- **Visualization**: Generate thesis dependency graphs
- **Collaboration**: Multi-user editing with conflict resolution

## References

The HTF framework unifies:
- **IMRaD**: Standard scientific paper structure
- **Papers Pattern**: Thesis as collection of papers
- **Argument Mapping**: Toulmin's argument model
- **Contribution-First**: Design Science Research patterns
- **Monograph**: Traditional thesis structure
- **Narrative**: Qualitative/interpretive thesis modes
