# Concept Selection - Multi-Step Workflow

## Purpose

This command guides agents through systematic concept selection methods (Pugh Matrix and AHP) to evaluate and select the best design concepts for ggen features. Concept selection ensures objective, data-driven decisions when choosing between multiple design alternatives. Experts use systematic methods to avoid bias and select concepts that best meet requirements.

## Workflow Overview

```
Step 1: Define Selection Criteria → Step 2: Generate Concepts → Step 3: Pugh Matrix Evaluation → Step 4: AHP Evaluation → Step 5: Select and Verify
```

## Step-by-Step Instructions

### Step 1: Define Selection Criteria

**Action**: Define criteria for evaluating concepts based on ggen requirements.

#### 1.1: Identify Criteria from Requirements

**Action**: Extract criteria from ggen requirements and SLOs.

**Criteria sources**:
- **SLOs**: Performance targets (first build ≤ 15s, incremental ≤ 2s, RDF processing ≤ 5s)
- **Determinism**: 100% reproducible outputs
- **Type Safety**: Zero-cost abstractions, type-level guarantees
- **Maintainability**: Code quality, test coverage
- **Integration**: IDE and tooling integration

**Example criteria identification**:
```markdown
## Selection Criteria

**From SLOs**:
- C1: RDF processing speed (≤ 5s for 1k+ triples)
- C2: Generation memory (≤ 100MB)
- C3: CLI scaffolding speed (≤ 3s end-to-end)

**From Determinism Requirements**:
- C4: Reproducible outputs (100% deterministic)
- C5: Template validation (deterministic transformation)

**From Type Safety Requirements**:
- C6: Compile-time guarantees (type-level encodings)
- C7: Zero-cost abstractions (no runtime overhead)

**From Maintainability**:
- C8: Test coverage (80%+ coverage)
- C9: Code complexity (maintainable patterns)
```

#### 1.2: Prioritize Criteria

**Action**: Determine relative importance of criteria.

**Example prioritization**:
```markdown
## Criteria Prioritization

**Must-Have** (Critical):
- C1: RDF processing speed (10/10)
- C4: Reproducible outputs (10/10)
- C6: Compile-time guarantees (10/10)

**Important** (High priority):
- C2: Generation memory (8/10)
- C7: Zero-cost abstractions (8/10)
- C8: Test coverage (8/10)

**Nice-to-Have** (Lower priority):
- C3: CLI scaffolding speed (6/10)
- C5: Template validation (7/10)
- C9: Code complexity (7/10)
```

---

### Step 2: Generate Concepts

**Action**: Generate multiple design concept alternatives for ggen features.

#### 2.1: Concept Generation Methods

**Action**: Use multiple methods to generate diverse concepts.

**Generation methods**:
- **Brainstorming**: Generate many ideas
- **TRIZ**: Use TRIZ principles for innovative concepts
- **Benchmarking**: Learn from existing solutions
- **Prototyping**: Build quick prototypes to explore

**Example concepts for RDF processing**:
```markdown
## Design Concepts

**Concept A**: Streaming RDF Parser
- **Description**: Process RDF triples in streaming fashion
- **Key Features**: Iterator-based processing, memory-efficient
- **Pros**: Low memory usage, handles large graphs
- **Cons**: More complex implementation

**Concept B**: In-Memory Graph Store
- **Description**: Load entire RDF graph into memory
- **Key Features**: Fast random access, simple queries
- **Pros**: Simple implementation, fast queries
- **Cons**: High memory usage for large graphs

**Concept C**: Hybrid Approach (Streaming + Caching)
- **Description**: Stream processing with selective caching
- **Key Features**: Streaming for large graphs, cache hot paths
- **Pros**: Best of both worlds, balanced performance
- **Cons**: More complex implementation
```

---

### Step 3: Pugh Matrix Evaluation

**Action**: Use Pugh Matrix to compare concepts against baseline.

#### 3.1: Select Baseline

**Action**: Choose baseline concept for comparison.

**Example baseline**:
```markdown
## Baseline Selection

**Baseline**: Current In-Memory Processing
- **Description**: Current approach - load entire graph into memory
- **Rationale**: Known performance, serves as reference point
- **Performance**: 8s for 1k triples, 200MB memory
```

#### 3.2: Create Pugh Matrix

**Action**: Create matrix comparing concepts to baseline.

**Example Pugh Matrix**:
```markdown
## Pugh Matrix

| Criterion | Baseline | Concept A (Streaming) | Concept B (In-Memory) | Concept C (Hybrid) |
|-----------|----------|----------------------|----------------------|-------------------|
| C1: Speed | 0 | + | + | S |
| C2: Memory | 0 | S | - | + |
| C4: Determinism | 0 | 0 | 0 | 0 |
| C6: Type Safety | 0 | 0 | 0 | 0 |
| C8: Coverage | 0 | - | 0 | - |
| **Net Score** | 0 | +1 | 0 | +2 |
```

---

### Step 4: AHP Evaluation

**Action**: Use Analytic Hierarchy Process (AHP) for detailed evaluation.

#### 4.1: Create Pairwise Comparison Matrix

**Action**: Compare criteria pairwise to determine weights.

**Example pairwise comparison**:
```markdown
## Pairwise Comparison Matrix (Criteria)

| Criterion | C1 | C2 | C4 | C6 | C8 |
|-----------|----|----|----|----|----|
| C1: Speed | 1 | 3 | 1 | 1 | 3 |
| C2: Memory | 1/3 | 1 | 1/3 | 1/3 | 1 |
| C4: Determinism | 1 | 3 | 1 | 1 | 3 |
| C6: Type Safety | 1 | 3 | 1 | 1 | 3 |
| C8: Coverage | 1/3 | 1 | 1/3 | 1/3 | 1 |

**Weights** (normalized):
- C1: Speed: 0.25
- C2: Memory: 0.10
- C4: Determinism: 0.25
- C6: Type Safety: 0.25
- C8: Coverage: 0.10
```

---

### Step 5: Select and Verify

**Action**: Select best concept(s) and verify selection.

#### 5.1: Compare Methods

**Action**: Compare Pugh Matrix and AHP results.

**Example comparison**:
```markdown
## Method Comparison

**Pugh Matrix Ranking**:
1. Concept C (Hybrid): Net +2
2. Concept A (Streaming): Net +1
3. Concept B (In-Memory): Net 0

**AHP Ranking**:
1. Concept C (Hybrid): 0.85
2. Concept A (Streaming): 0.72
3. Concept B (In-Memory): 0.65

**Consensus**: Both methods favor Concept C (Hybrid)
**Selected**: Concept C (Hybrid) - highest AHP score, good Pugh score
```

#### 5.2: Verify Selection Criteria Met

**Action**: Verify selected concept meets all criteria.

**Example verification**:
```markdown
## Selection Verification

**Selected**: Concept C (Hybrid - Streaming + Caching)

**Must-Have Criteria**:
- ✅ C1: Speed - Estimated 3s for 1k triples (meets ≤ 5s SLO)
- ✅ C4: Determinism - Streaming maintains determinism
- ✅ C6: Type Safety - Type-level encodings supported

**Important Criteria**:
- ✅ C2: Memory - Estimated 80MB (meets ≤ 100MB SLO)
- ✅ C7: Zero-cost - Iterator-based abstractions are zero-cost
- ✅ C8: Coverage - Testable architecture

**Conclusion**: Selected concept meets all must-have criteria ✅
```

#### 5.3: Create Todo List for Concept Implementation

**CRITICAL**: Do NOT write documents or reports. Create todos and execute them.

**Action**: Create 10+ item todo list for implementing selected concept.

**Example todo list**:
```markdown
## Concept Implementation Todos (10+ items)

**Concept C (Hybrid) Implementation**:
- [ ] Design streaming RDF parser architecture
- [ ] Implement iterator-based RDF processing
- [ ] Design selective caching mechanism
- [ ] Implement cache for hot paths
- [ ] Add type-level encodings for RDF types
- [ ] Integrate streaming with caching
- [ ] Add error handling and recovery
- [ ] Write comprehensive tests
- [ ] Verify compilation: `cargo make check`
- [ ] Run tests: `cargo make test`
- [ ] Measure performance: `cargo make slo-check`
- [ ] Verify SLOs met: ≤ 5s processing, ≤ 100MB memory
- [ ] Document implementation
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement concept)
3. Mark todos as completed as work is done
4. Verify each step works before moving to next
5. Continue until all todos complete

**Principle**: Execute concept implementation, don't document it. Todos track progress, implementation delivers value.

---

## Integration with Other Commands

- **[Voice of Customer (QFD)](./voice-of-customer-qfd.md)** - Use to identify selection criteria from customer needs
- **[TRIZ Problem Solving](./triz-problem-solving.md)** - Use to generate innovative concepts for evaluation
- **[DMEDI Design Process](./dmedi-design-process.md)** - Use concept selection in Explore phase
- **[Robust Design](./robust-design.md)** - Use to evaluate concepts for robustness
- **[FMEA](./fmea.md)** - Use to evaluate concepts for failure modes

## Expert Insights

**Why this matters**: Systematic concept selection ensures objective, data-driven decisions. Avoids bias and selects concepts that best meet ggen requirements.

**Key principle**: "Data over opinion" - Use systematic methods (Pugh Matrix, AHP) to make objective decisions, not subjective preferences.

**Remember**: 
- **Multiple concepts**: Generate many concepts before selecting
- **Systematic evaluation**: Use structured methods, not gut feel
- **Multiple methods**: Use both Pugh Matrix and AHP for validation
- **Verify selection**: Ensure selected concept meets all criteria

**DfLSS alignment**: Concept selection supports DfLSS (Design for Lean Six Sigma) by ensuring selected concepts address both efficiency (waste elimination) AND quality (defect prevention) - evaluating concepts against both efficiency and quality criteria. Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

End Command ---

