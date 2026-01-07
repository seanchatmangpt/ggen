# ggen Evidence & Research Data Manifest

**Location**: `.ggen/` directory
**Total Files**: 15+ JSON files
**Total Size**: ~600+ KB of structured research data
**Last Updated**: 2026-01-07

---

## Evidence Catalog

Complete inventory of all research findings, corpus materials, and knowledge structures.

### Primary Evidence Files

#### 1. evidence_catalog.json (306 KB)
**Purpose**: Master catalog of all research evidence
**Structure**: Hierarchical evidence database
**Contains**:
- Complete research findings
- Evidence classification
- Cross-references to documents
- Evidence hierarchy and relationships
- Source attribution
- Temporal metadata

**Access Pattern**: Use this as the primary lookup for any specific research finding

---

#### 2. evidence_graph_v2.json (141 KB)
**Purpose**: Knowledge graph (Version 2 - Latest)
**Structure**: RDF-like graph representation
**Contains**:
- Node definitions (research concepts)
- Edge relationships (concept connections)
- Node properties and metadata
- Relationship types and cardinality
- Semantic associations

**Use Case**: Understand relationships between research areas

---

#### 3. evidence_graph.json (41 KB)
**Purpose**: Knowledge graph (Version 1 - Legacy)
**Structure**: Earlier version of evidence graph
**Contains**: Original knowledge structure
**Status**: Maintained for version history

---

#### 4. relationships_graph.json (35 KB)
**Purpose**: Relationship mappings between concepts
**Structure**: Relationship-centric graph
**Contains**:
- Cross-domain relationships
- Concept connection patterns
- Dependency graphs
- Association strengths

**Use Case**: Find related research areas

---

#### 5. breadth_expansion_report.json (34 KB)
**Purpose**: Analysis of research breadth
**Contains**:
- Coverage across different domains
- Topic expansion analysis
- New areas discovered
- Coverage gaps
- Breadth metrics

**Analysis Type**: Horizontal knowledge coverage

---

#### 6. depth_expansion_report.json (50 KB)
**Purpose**: Analysis of research depth
**Contains**:
- Deep-dive findings per topic
- Recursive research layers
- Detailed investigations
- Complexity analysis
- Depth metrics

**Analysis Type**: Vertical knowledge depth

---

#### 7. evidence_mining_interim.json (27 KB)
**Purpose**: Work-in-progress research mining
**Contains**:
- Partially processed evidence
- Mining intermediate results
- Pending analysis
- Extraction artifacts

**Status**: Interim results

---

#### 8. evidence_nodes.json (23 KB)
**Purpose**: All research concept nodes
**Structure**: Node definitions
**Contains**:
- Concept definitions
- Node properties
- Node classifications
- Metadata for each concept

**Use Case**: Look up specific research concepts

---

#### 9. concept_coverage.json (6.7 KB)
**Purpose**: Coverage metrics per concept
**Contains**:
- Coverage percentages
- Completeness scores
- Documentation coverage
- Evidence availability per concept

**Analysis Type**: Quality metrics

---

#### 10. concept_gaps.json (1.4 KB)
**Purpose**: Identified research gaps
**Contains**:
- Missing research areas
- Incomplete coverage
- Topics requiring more evidence
- Priority gaps

**Use Case**: Identify areas for future research

---

#### 11. graph_metrics.json (1.2 KB)
**Purpose**: Structural metrics of knowledge graph
**Contains**:
- Node count
- Edge count
- Graph density
- Centrality metrics
- Connectivity measures

**Analysis Type**: Graph health metrics

---

## Graph Processing Tools

### 1. graph_merger.py
**Purpose**: Merge multiple evidence graphs
**Usage**: Combine evidence from multiple sources
**Output**: Unified evidence graph

### 2. synthesize_graph.py
**Purpose**: Synthesize new knowledge from graph
**Usage**: Generate higher-level insights
**Output**: Derived knowledge structures

### 3. validate_graph_v2.py
**Purpose**: Validate graph consistency and integrity
**Usage**: Quality assurance for evidence graph
**Output**: Validation report

---

## Evidence Organization Structure

### By Category

```
Evidence Data
├── Research Findings (evidence_catalog.json)
├── Knowledge Graphs
│   ├── Version 2 (evidence_graph_v2.json) [CURRENT]
│   └── Version 1 (evidence_graph.json)
├── Relationships (relationships_graph.json)
├── Analyses
│   ├── Breadth Analysis (breadth_expansion_report.json)
│   └── Depth Analysis (depth_expansion_report.json)
├── Concepts
│   ├── Node Definitions (evidence_nodes.json)
│   ├── Coverage Metrics (concept_coverage.json)
│   └── Gaps (concept_gaps.json)
├── Processing
│   ├── Interim Results (evidence_mining_interim.json)
│   └── Graph Metrics (graph_metrics.json)
└── Tools
    ├── graph_merger.py
    ├── synthesize_graph.py
    └── validate_graph_v2.py
```

---

## Research Domains in Evidence

### Financial Domain
- FIBO (Financial Industry Business Ontology) patterns
- RegTech inference chains
- Systemic risk propagation
- Financial instruments analysis
- Compliance workflows

### Ontology & Specifications
- RDF/Turtle specifications
- SHACL validation rules
- Temporal/bitemporal constructs
- Type mappings
- Schema standardization

### Testing & Quality
- Chicago TDD patterns
- Mutation testing approaches
- Test concurrency strategies
- FMEA methodologies
- Poka-Yoke patterns

### Code Generation
- AI-assisted generation patterns
- Deterministic output strategies
- Language-specific patterns (Rust, TypeScript, Python)
- Template systems
- Configuration generation

### Performance
- Benchmark methodologies
- Optimization strategies
- Concurrency patterns
- Resource management
- SLO enforcement

### Architecture
- Pack system design
- Migration strategies
- RDF control planes
- API design patterns
- Marketplace architecture

---

## Accessing the Evidence

### Method 1: Direct JSON Query

Load any JSON file directly:
```
cat .ggen/evidence_catalog.json | jq '.findings[] | .title'
```

### Method 2: Graph Analysis

Use graph processing tools:
```bash
python .ggen/synthesize_graph.py
```

### Method 3: Research Documents

Link from evidence to source documents:
- Check `evidence_catalog.json` for document references
- Jump to `.specify/`, `docs/`, or root-level summaries
- Consult archive materials for historical context

### Method 4: Concept Lookup

Search concept definitions:
```bash
cat .ggen/evidence_nodes.json | jq '.nodes[] | select(.title | contains("TERM"))'
```

---

## Evidence Quality Metrics

### Coverage Analysis

| Metric | File | Key Finding |
|--------|------|------------|
| Breadth | `breadth_expansion_report.json` | Domain coverage distribution |
| Depth | `depth_expansion_report.json` | Investigation layers |
| Completeness | `concept_coverage.json` | Documentation coverage % |
| Gaps | `concept_gaps.json` | Missing research areas |
| Health | `graph_metrics.json` | Structural integrity |

### Graph Statistics

Access `graph_metrics.json` for:
- Total nodes (research concepts)
- Total edges (relationships)
- Graph density
- Connected components
- Average degree
- Centrality rankings

---

## Evidence Integrity Validation

### Validation Checklist

1. **Structural Validity**
   - File: `validate_graph_v2.py`
   - Ensures graph consistency
   - Checks cardinality constraints

2. **Reference Integrity**
   - Links between evidence_catalog and graphs
   - Document cross-references
   - Citation accuracy

3. **Temporal Consistency**
   - Evidence timestamps align
   - No temporal contradictions
   - Version history maintained

4. **Semantic Validity**
   - Concept definitions consistent
   - Relationship types valid
   - Ontology adherence

### Running Validation

```bash
python .ggen/validate_graph_v2.py
# Outputs validation report
```

---

## Synthesis & Analysis

### Available Syntheses

1. **Graph Synthesis** (`synthesize_graph.py`)
   - Generates derived knowledge
   - Identifies patterns
   - Creates meta-concepts

2. **Graph Merging** (`graph_merger.py`)
   - Combines evidence sources
   - Resolves duplicates
   - Creates unified view

---

## Evidence-Driven Documentation

### Link Chain

```
Evidence Data (.ggen/JSON)
    ↓
Evidence Catalog + Graphs
    ↓
Document References
    ↓
Research Summaries (.md files)
    ↓
Thesis & Reports (docs/thesis/)
    ↓
RDF Specifications (.specify/)
    ↓
Generated Code (crates/*/src/)
```

---

## Usage Examples

### Find All Evidence on a Topic

```bash
# Example: Find all evidence about RDF generation
cat .ggen/evidence_catalog.json | jq '.findings[] | select(.tags[] | contains("RDF"))'
```

### Explore Concept Relationships

```bash
# Find concepts related to "code-generation"
cat .ggen/evidence_graph_v2.json | jq '.edges[] | select(.source == "code-generation")'
```

### Check Coverage Gaps

```bash
# Identify low-coverage areas
cat .ggen/concept_coverage.json | jq '.concepts[] | select(.coverage < 0.5)'
```

### View Graph Health

```bash
# Check overall graph metrics
cat .ggen/graph_metrics.json
```

---

## Evidence Lifecycle

### Collection Phase
- Raw evidence gathered from agents and research
- Stored in `evidence_mining_interim.json`
- Processed and classified

### Processing Phase
- Evidence validated and structured
- Relationships mapped in graphs
- Concepts extracted and cataloged

### Analysis Phase
- Breadth analysis (horizontal coverage)
- Depth analysis (vertical investigation)
- Gap identification

### Synthesis Phase
- Relationships derived from evidence
- Patterns identified
- Meta-concepts created

### Documentation Phase
- Evidence linked to documents
- Thesis updated from evidence
- RDF specs generated from findings

---

## Recent Evidence Expansions

### Breadth Expansion Areas
- Multi-domain financial patterns
- Cross-language code generation
- Marketplace ecosystem
- Agent-based systems
- Performance optimization strategies

### Depth Expansion Areas
- Financial compliance details
- Type system sophistication
- Testing methodology layers
- RDF semantic depth
- Control plane architecture

---

## Evidence Access Patterns

### Quick Lookup
→ Use `evidence_nodes.json` for concept definitions

### Deep Dive
→ Use `evidence_catalog.json` for comprehensive findings

### Relationship Analysis
→ Use `relationships_graph.json` or `evidence_graph_v2.json`

### Coverage Assessment
→ Use `concept_coverage.json` and `concept_gaps.json`

### System Health
→ Use `graph_metrics.json`

---

## Maintenance & Updates

### Regular Tasks
1. Run `validate_graph_v2.py` weekly
2. Update `evidence_catalog.json` with new findings
3. Synthesize new relationships with `synthesize_graph.py`
4. Review gaps in `concept_gaps.json`

### Quality Assurance
- All evidence must have source attribution
- Relationships must be validated
- Temporal metadata must be consistent
- Cross-references must be accurate

---

## Integration Points

### With Specifications
- Evidence informs RDF specifications (.specify/)
- Specs generated from evidence patterns
- Bidirectional validation

### With Thesis
- Thesis chapters supported by evidence
- Citations link to evidence catalog
- Academic rigor maintained

### With Code Generation
- Evidence drives pattern selection
- Code generated from spec -> evidence chain
- Deterministic output based on evidence

### With Testing
- Test cases derived from evidence
- Coverage tied to evidence breadth/depth
- Mutation testing guided by evidence

---

## Data Migration & Versioning

### Current Version
- **evidence_graph_v2.json** - Version 2 (current)
- **evidence_graph.json** - Version 1 (archived)

### Version History
All versions maintained in `.ggen/` directory
Previous versions available for historical analysis

### Migration Path
v1 → v2 migration completed
Future versions will maintain backward compatibility

---

## Research Corpus Size

| Metric | Value |
|--------|-------|
| Total Evidence Files | 11 main + tools |
| Combined Size | ~600 KB |
| Largest File | evidence_catalog.json (306 KB) |
| Node Count | See graph_metrics.json |
| Relationship Count | See graph_metrics.json |
| Archive History | Full version history maintained |

---

## Future Evidence Directions

### Planned Expansions
- Agent execution evidence graphs
- Test coverage correlations
- Performance benchmark evidence
- Marketplace usage patterns
- User research evidence

### Data Enrichment
- Add evidence confidence scores
- Track evidence provenance
- Link to specific commits
- Add evidence review history

---

## Contact & Questions

For evidence-related questions:
1. Check `evidence_catalog.json` for similar findings
2. Review `concept_gaps.json` for related work
3. Consult original research documents via links
4. Use graph analysis tools for relationships

---

**Last Updated**: 2026-01-07
**Version**: 2.0
**Status**: Active research corpus
**Maintenance**: Ongoing
