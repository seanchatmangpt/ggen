# Jobs To Be Done: Analyzing Dependency Graph Across Project Files

**User Persona:** Software Architect / Tech Lead
**MCP Tool:** `validate_dependency_graph`
**Project Context:** ggen - Specification-driven Rust code generation CLI
**Date:** 2026-03-31

---

## 5 Whys Analysis

### Why #1: Why does the user need to analyze dependencies?

**Answer:** To understand the architectural structure and identify hidden coupling between project components.

**Context:**
- The ggen project has 30+ crates with complex interdependencies
- RDF ontologies, SPARQL queries, Tera templates, and generated code form a dependency chain
- Manual tracking of dependencies becomes impossible as project scales
- Architectural decisions require data-driven insight, not intuition

**Architectural Problem:**
- Hidden dependencies between files can create surprising coupling
- Changes to core files (like `ggen.toml` or `templates.ttl`) may have ripple effects
- Need to verify that the documented architecture matches actual implementation
- Onboarding new developers requires clear dependency documentation

---

### Why #2: Why detect circular dependencies?

**Answer:** Circular dependencies create build failures, infinite loops, and unmaintainable code.

**Risk of Cycles in the Project:**

**Build System Failures:**
- Rust's module system prohibits circular dependencies at compilation
- Template inheritance cycles cause rendering errors (Tera template engine)
- SPARQL query import cycles create infinite recursion during extraction

**Runtime Failures:**
- Code generation pipeline (μ₁-μ₅) can enter infinite loops
- Dependency resolution algorithms fail on cycles (topological sort requires DAG)
- Watch mode (`ggen sync --watch`) may trigger continuous rebuilds

**Maintenance Burden:**
- Cyclic code is harder to test (Chicago TDD requires independent unit tests)
- Refactoring becomes risky (changing one file breaks others in unpredictable ways)
- Mutation testing produces false positives (cycles inflate complexity metrics)

**Real Example from ggen:**
```
templates/hello.tmpl → queries/extract-skills.rq → templates.ttl → templates/hello.tmpl
```
This cycle would break the μ₂ extraction stage and prevent code generation.

---

### Why #3: Why find the critical path?

**Answer:** The critical path identifies the longest dependency chain, which determines build time and optimization targets.

**Value of Knowing the Critical Path:**

**Performance Optimization:**
- Critical path = worst-case build time (sequential dependency chain)
- Optimizing files on the critical path reduces overall build duration
- Parallelization opportunities exist only off the critical path
- Incremental build efficiency depends on minimizing critical path depth

**Architectural Insight:**
- Longest chain = most coupled part of the system
- Files on critical path are "architecture pillars" (change them carefully)
- Depth of critical path indicates architectural complexity
- Shallow critical paths enable faster iteration

**Example Critical Path:**
```
ggen.toml (1s) → templates.ttl (2s) → extract-skills.rq (1s) → hello.tmpl (3s) → generated.rs (5s)
Total: 12s (worst-case build time)
```
Optimizing `hello.tmpl` rendering from 3s to 1s reduces total build time by 17%.

**Project Management:**
- Critical path files require senior architect review for changes
- Testing priority: critical path files need highest coverage (Chicago TDD)
- Documentation: critical path should be visually highlighted in architecture diagrams

---

### Why #4: Why identify orphan nodes?

**Answer:** Orphan nodes represent unused files, dead code, or broken documentation.

**Problem with Unused Files:**

**Technical Debt:**
- Orphan files confuse new developers ("Why does this file exist?")
- Dead code increases cognitive load without value
- Unused templates/queries waste maintenance time
- CI/CD runs tests on irrelevant files (slowing feedback)

**Broken Documentation:**
- Orphan `.md` files may document non-existent features
- Stale examples create confusion (code doesn't match docs)
- "Zombie" documentation lowers trust in documentation quality

**Missed Architecture Opportunities:**
- Orphan nodes may indicate planned-but-unimplemented features
- Could reveal architectural gaps (forgot to wire up new component)
- Might be leftovers from refactoring (cleanup incomplete)

**Example Orphan Node:**
```
queries/legacy-extract.rq (not referenced by any template or manifest)
```
This file should either be deleted or integrated into the dependency graph.

---

### Why #5: Why use MCP tool instead of manual graph analysis?

**Answer:** Manual graph analysis is error-prone, time-consuming, and doesn't scale with project growth.

**Complexity of Manual Analysis:**

**Scale Challenge:**
- ggen has 30+ crates, 100+ templates, 50+ SPARQL queries
- Manual tracking requires reading every file and extracting imports
- Dependency changes require re-analyzing entire project
- Human error inevitable (missed imports, forgotten files)

**Dynamic Dependencies:**
- Template inheritance (`{% extends "base.tmpl" %}`)
- SPARQL query imports (custom `IMPORT` statements)
- Manifest file references (dynamic paths, glob patterns)
- Generated code dependencies (not visible in source files)

**Tool Requirements:**
- **Graph construction:** Parse all file formats (TTL, RQ, TMPL, TOML, RS)
- **Cycle detection:** Tarjan's algorithm or DFS-based detection
- **Critical path:** Topological sort + longest path calculation
- **Orphan detection:** Graph traversal to find disconnected nodes
- **Visualization:** Generate Mermaid/DOT diagrams for documentation

**Manual Analysis Effort (for ggen-scale project):**
- Read 200+ files: ~2 hours
- Extract dependencies manually: ~4 hours
- Detect cycles (mental or whiteboard): ~2 hours
- Verify accuracy: ~2 hours
- **Total: ~10 hours per analysis**

**MCP Tool Effort:**
```bash
mcp.call("validate_dependency_graph", {project_root: "./ggen"})
```
- Execution time: <5 seconds
- Accuracy: 100% (automated parsing)
- Repeatability: Run on every commit (CI/CD integration)
- **Total: ~5 seconds per analysis**

**ROI:** 7,200x faster than manual analysis with higher accuracy.

---

## JTBD Story Format

### When [situation]

**Context 1: Greenfield Project (Early Stage)**
- Just starting a ggen project with `ggen init my-project`
- Adding templates, queries, and ontology files rapidly
- Want to establish clean architecture from the start
- Need to verify that dependencies match intended design

**Context 2: Legacy Codebase (Refactoring)**
- Inherited a ggen project with unclear architecture
- Experiencing build failures or slow builds
- Planning major refactoring (e.g., splitting monolith into modules)
- Need to understand current dependencies before making changes

**Context 3: Team Onboarding**
- New developers joining the team
- Need to explain architecture visually (dependency graphs)
- Want to identify which files are safe to modify vs. critical path
- Reducing bus factor (knowledge silos)

**Context 4: Pre-Release Validation**
- Preparing for v1.0 release
- Need to ensure no circular dependencies (build blocker)
- Verify that all files are used (no dead code in release)
- Document architecture for users/contributors

**Context 5: Continuous Integration**
- Every commit should be validated for dependency integrity
- Prevent accidental cycle introduction
- Catch orphan files early (cleanup immediately)
- Monitor critical path growth (architecture complexity creep)

---

### I want to [motivation]

**Primary Motivation:** Gain confidence in architectural decisions through data-driven dependency analysis.

**Specific Motivations:**

1. **Validate Architecture:**
   - Confirm that actual dependencies match documented design
   - Identify accidental coupling (e.g., template importing unexpected query)
   - Verify layering (e.g., no templates depending on generated code)

2. **Prevent Build Failures:**
   - Detect circular dependencies before they break CI/CD
   - Identify files that will cause compilation errors
   - Ensure incremental build correctness (dependency chains)

3. **Optimize Performance:**
   - Find critical path (worst-case build time)
   - Identify parallelization opportunities (independent subgraphs)
   - Target optimization efforts (files on critical path)

4. **Reduce Technical Debt:**
   - Remove orphan files (dead code)
   - Eliminate unnecessary dependencies (reduce coupling)
   - Simplify architecture (shorten critical path)

5. **Communicate Architecture:**
   - Generate visual diagrams (Mermaid/DOT) for documentation
   - Onboard new developers with clear dependency maps
   - Facilitate code reviews (show impact of changes)

---

### So that [expected outcome]

**Expected Outcome 1: Architectural Clarity**
- Clear visualization of project dependencies
- Understanding of which files are "leaves" (safe to modify) vs. "core" (risky)
- Confidence that architecture matches intended design

**Expected Outcome 2: Risk Mitigation**
- Zero circular dependencies (no build failures)
- Identified critical path (know what to optimize)
- Documented orphans (cleanup plan)

**Expected Outcome 3: Performance Optimization**
- Reduced build time (optimizing critical path)
- Parallel build strategy (independent subgraphs)
- Incremental build efficiency (minimal recompilation)

**Expected Outcome 4: Improved Maintainability**
- Cleaner codebase (orphans removed)
- Lower coupling (unnecessary dependencies eliminated)
- Easier onboarding (visual dependency documentation)

**Expected Outcome 5: Continuous Validation**
- CI/CD integration (every commit validated)
- Automated dependency updates (graph regenerated on change)
- Architecture evolution tracking (critical path growth over time)

---

## Forces at Play

### Force 1: Complexity

**Challenge:** Dependency graphs become exponentially harder to understand as project grows.

**Force Impact:**
- Small project (10 files): Mental tracking possible
- Medium project (100 files): Whiteboard or spreadsheet required
- Large project (1000+ files): Automated tool mandatory

**MCP Tool Addresses:**
- Automated parsing of all file formats
- Graph algorithms handle complexity (cycles, paths, orphans)
- Visualization scales to large graphs (filtering, clustering)

---

### Force 2: Risk Mitigation

**Challenge:** Circular dependencies can break builds and halt development.

**Force Impact:**
- High risk: Cycle introduced in critical path = build failure
- Medium risk: Cycle in non-critical path = runtime failure
- Low risk: Orphan files = technical debt

**MCP Tool Addresses:**
- Cycle detection algorithms (Tarjan's, DFS)
- CI/CD integration (prevent merge if cycles detected)
- Early warning (detect cycles during development, not after merge)

---

### Force 3: Communication

**Challenge:** Architecture knowledge is often tacit (in architect's head) and not shared.

**Force Impact:**
- New developers struggle to understand dependencies
- Code reviews miss architectural violations
- Documentation drifts from implementation

**MCP Tool Addresses:**
- Visual diagrams (Mermaid for documentation)
- Machine-readable output (JSON for tooling)
- Automated documentation generation (keep docs in sync)

---

### Force 4: Evolution

**Challenge:** Projects evolve, and dependencies change over time.

**Force Impact:**
- Dependencies added accidentally (copy-paste code)
- Dependencies removed but not documented (stale docs)
- Architecture degrades gradually (entropy)

**MCP Tool Addresses:**
- Continuous validation (run on every commit)
- Historical tracking (compare graphs over time)
- Automated cleanup (detect drift from intended architecture)

---

## MCP Tool Usage

### Basic Usage

```bash
# Analyze entire project dependency graph
mcp.call("validate_dependency_graph", {
  project_root: "./my-ggen-project",
  manifest_path: "./ggen.toml"
})
```

### Advanced Usage

```bash
# Analyze specific subdirectory
mcp.call("validate_dependency_graph", {
  project_root: "./ggen",
  subdirectory: "./templates/elixir-a2a",
  include_patterns: ["**/*.tera", "**/*.rq"],
  exclude_patterns: ["**/node_modules/**", "**/target/**"]
})

# Generate visualization
mcp.call("validate_dependency_graph", {
  project_root: "./ggen",
  output_format: "mermaid",
  output_path: "./docs/architecture/dependency-graph.mmd"
})

# CI/CD integration (fail on cycles)
mcp.call("validate_dependency_graph", {
  project_root: "./ggen",
  fail_on_circular_dependencies: true,
  fail_on_orphans: false  # Warning only
})
```

---

## Tool Output Examples

### Success Case (Valid Graph)

```json
{
  "is_valid": true,
  "summary": {
    "total_files": 47,
    "total_edges": 63,
    "max_depth": 5,
    "orphan_count": 0
  },
  "dependency_graph": {
    "ggen.toml": ["ontology/templates.ttl", "queries/*.rq"],
    "ontology/templates.ttl": [],
    "queries/extract-skills.rq": ["ontology/templates.ttl"],
    "queries/extract-templates.rq": ["ontology/templates.ttl"],
    "templates/hello.tmpl": ["queries/extract-skills.rq"],
    "templates/goodbye.tmpl": ["queries/extract-templates.rq", "templates/hello.tmpl"]
  },
  "circular_dependencies": [],
  "orphan_nodes": [],
  "critical_path": [
    "ggen.toml",
    "ontology/templates.ttl",
    "queries/extract-skills.rq",
    "templates/hello.tmpl"
  ],
  "visualization": {
    "mermaid": "graph TD\n  ggen.toml --> templates.ttl\n  templates.ttl --> extract-skills.rq\n  ..."
  }
}
```

### Failure Case (Circular Dependency Detected)

```json
{
  "is_valid": false,
  "error": "Circular dependency detected",
  "circular_dependencies": [
    {
      "cycle": [
        "templates/hello.tmpl",
        "queries/extract-skills.rq",
        "ontology/templates.ttl",
        "templates/hello.tmpl"
      ],
      "impact": "Build failure - cannot generate code from hello.tmpl",
      "suggestion": "Remove hello.tmpl's dependency on extract-skills.rq, or extract-skills.rq's dependency on templates.ttl"
    }
  ],
  "orphan_nodes": [
    {
      "file": "queries/legacy-extract.rq",
      "impact": "Unused file - safe to delete or integrate",
      "suggestion": "Delete this file or add it to ggen.toml's query list"
    }
  ],
  "critical_path": null
}
```

---

## Success Criteria

### Functional Requirements

- [ ] **Dependency Graph Construction:** Correctly parse all file formats (TTL, RQ, TMPL, TOML, RS)
- [ ] **Cycle Detection:** Identify all circular dependencies with exact cycle paths
- [ ] **Critical Path Calculation:** Determine longest dependency chain (worst-case build time)
- [ ] **Orphan Detection:** Find all disconnected nodes (unused files)
- [ ] **Visualization:** Generate Mermaid/DOT diagrams for documentation

### Non-Functional Requirements

- [ ] **Performance:** Analysis completes in <5 seconds for 200+ files
- [ ] **Accuracy:** 100% correct dependency extraction (no false positives/negatives)
- [ ] **Scalability:** Handle projects with 1000+ files
- [ ] **Usability:** Clear error messages with actionable suggestions

### Quality Gates (Chicago TDD)

- [ ] **Unit Tests:** Test graph algorithms (cycle detection, critical path, orphan detection)
- [ ] **Integration Tests:** Test with real ggen project files (TTL, RQ, TMPL parsing)
- [ ] **Property Tests:** Test graph properties (transitivity, symmetry, reflexivity)
- [ ] **OTEL Validation:** Verify spans for `dependency_graph.validate`, `graph.parse`, `cycle.detect`

---

## Architecture Insights

### Insight 1: Layered Architecture

**Ideal Dependency Pattern:**
```
Manifest (ggen.toml)
  ↓
Ontology (templates.ttl)
  ↓
Queries (extract-*.rq)
  ↓
Templates (template.tmpl)
  ↓
Generated Code (output.rs)
```

**Violation Example:**
```
Template → Generated Code (circular dependency if generated code imports template)
```

**MCP Tool Detects:**
- Backwards dependencies (generated code → templates)
- Skipped layers (templates → ontology, skipping queries)
- Cross-layer coupling (template importing unrelated query)

---

### Insight 2: Parallelization Opportunities

**Independent Subgraphs:**
```
Subgraph A:
  ggen.toml → templates.ttl → extract-skills.rq → hello.tmpl

Subgraph B:
  ggen.toml → templates.ttl → extract-templates.rq → goodbye.tmpl
```

**Parallel Build Strategy:**
- Build Subgraph A and Subgraph B in parallel
- Only shared dependency is `templates.ttl` (sequential bottleneck)
- Optimize `templates.ttl` to improve overall build time

**MCP Tool Identifies:**
- Shared dependencies (sequential bottlenecks)
- Independent subtrees (parallelization candidates)
- Critical path depth (sequential vs. parallel ratio)

---

### Insight 3: Architectural Entropy

**Entropy Metrics:**
- **Graph density:** Edges / (Nodes × (Nodes - 1) / 2) → Lower is better
- **Average node degree:** Total edges / Nodes → Lower is better
- **Critical path depth:** Shorter is better (faster builds)

**Entropy Trend:**
```
Week 1: Density = 0.05, Depth = 3 (clean architecture)
Week 5: Density = 0.12, Depth = 6 (accumulating technical debt)
Week 10: Density = 0.20, Depth = 9 (architecture degrading)
```

**MCP Tool Tracks:**
- Entropy metrics over time (historical analysis)
- Density trends (detect coupling increase)
- Critical path growth (identify complexity creep)

---

## Related Documentation

- **Architecture Overview:** `/Users/sac/ggen/docs/architecture/dependency-management.md`
- **MCP Tools Reference:** `/Users/sac/ggen/docs/mcp-quality-tools.md`
- **Chicago TDD:** `/Users/sac/ggen/.claude/rules/rust/testing.md`
- **OTEL Validation:** `/Users/sac/ggen/.claude/rules/otel-validation.md`

---

## Conclusion

The `validate_dependency_graph` MCP tool addresses a critical architectural need: **understanding and managing project dependencies at scale**. By automating dependency analysis, cycle detection, critical path identification, and orphan detection, the tool enables:

1. **Architectural Clarity:** Data-driven understanding of project structure
2. **Risk Mitigation:** Prevent build failures from circular dependencies
3. **Performance Optimization:** Identify bottlenecks and parallelization opportunities
4. **Technical Debt Reduction:** Remove orphan files and unnecessary dependencies
5. **Continuous Validation:** CI/CD integration to maintain architectural integrity

**The Job:** "Help me understand my project's dependencies so I can make informed architectural decisions, prevent build failures, and optimize performance."

**The Outcome:** A validated dependency graph with zero cycles, identified critical path, and documented orphans—enabling confident architectural evolution.

---

**Version:** 1.0.0
**Last Updated:** 2026-03-31
**Author:** Claude (ggen Project)
**Status:** Ready for Implementation
