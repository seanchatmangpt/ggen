# A2A Fixing Agents Documentation

## Overview

A2A (Agent-to-Agent) fixing agents are autonomous quality maintenance agents that continuously validate and fix issues in your ggen projects. These agents work automatically via MCP tools or standalone to ensure your code generation pipeline remains healthy and functional.

## Available Agents

| Agent | Purpose | Key Capabilities | Execution Time |
|-------|---------|------------------|----------------|
| **CycleBreaker** | Detect and fix circular dependencies | Import graph analysis, automatic cycle breaking | <3s |
| **SPARQLValidator** | Validate and fix SPARQL queries | Syntax checking, semantic validation, auto-fix | <2s |
| **TemplateValidator** | Validate and fix Tera templates | Compilation checking, syntax validation, auto-fix | <2s |

## Quick Start

### Option 1: Use via MCP Tools (Recommended)

**Start the ggen MCP server**:
```bash
ggen mcp start-server --transport stdio
```

**Use agents through MCP tools**:
```json
{
  "name": "fix_cycles",
  "arguments": {
    "ontology_path": "schema/ontology.ttl",
    "agent": "CycleBreaker"
  }
}
```

### Option 2: Run Standalone

**Detect and fix cycles**:
```bash
cargo run --bin cycle_breaker -- --ontology schema/ontology.ttl
```

**Validate SPARQL**:
```bash
cargo run --bin sparql_validator -- --query queries/extract.rq
```

**Validate templates**:
```bash
cargo run --bin template_validator -- --templates templates/
```

## Agent Details

### Agent 1: CycleBreaker

**Purpose**: Detect and automatically fix circular dependencies in import graphs

**Capabilities**:
- 🔍 **Cycle Detection**: Identify circular dependencies in templates and queries
- 🔧 **Automatic Fixing**: Break cycles using optimal strategies
- 📊 **Detailed Reporting**: Provide comprehensive cycle analysis
- 🛡️ **Safe Operations**: Automatic backup before modifications
- 🔄 **Rollback Support**: Restore original state if needed

**How It Works**:

1. **Import Graph Extraction**:
   - Parse all templates and queries
   - Extract `{% include "..." %}` directives
   - Extract SPARQL `FROM` and `SERVICE` clauses
   - Build directed dependency graph

2. **Cycle Detection**:
   - Use depth-first search (DFS) algorithm
   - Track recursion stack
   - Identify back edges (indicating cycles)
   - List all nodes in each cycle

3. **Cycle Breaking Strategies**:
   - **Remove Edge**: Delete problematic edge from graph
   - **Inline Template**: Copy template content inline
   - **Delay Loading**: Use lazy evaluation (future)

4. **Fix Application**:
   - Create timestamped backup
   - Apply chosen fix strategy
   - Validate fix resolved cycle
   - Provide detailed report

**Parameters**:
```typescript
{
  ontology_path: string;      // Path to .ttl ontology file
  backup?: boolean;           // Create backup (default: true)
  dry_run?: boolean;          // Preview changes (default: false)
  strategy?: "auto" | "remove_edge" | "inline_template";  // Fix strategy (default: "auto")
  verbose?: boolean;          // Enable detailed output (default: false)
  output_format?: "json" | "text";  // Output format (default: "json")
}
```

**Return Format**:
```json
{
  "agent": "CycleBreaker",
  "status": "success" | "partial" | "failed",
  "cycles_found": 2,
  "cycles_fixed": 2,
  "fixes_applied": [
    {
      "cycle": ["template_a.tera", "template_b.tera", "template_c.tera", "template_a.tera"],
      "cycle_length": 4,
      "strategy_used": "remove_edge",
      "edge_removed": "template_c.tera → template_a.tera",
      "files_modified": ["template_c.tera"],
      "reason": "Edge removal was safest option (no template content duplication)"
    },
    {
      "cycle": ["query_d.rq", "query_e.rq", "query_d.rq"],
      "cycle_length": 3,
      "strategy_used": "inline_template",
      "template_inlined": "query_e.rq",
      "files_modified": ["query_d.rq"],
      "reason": "Query was small enough to inline safely"
    }
  ],
  "backup_path": "/tmp/ggen_backup_20250330_143022",
  "rollback_available": true,
  "execution_summary": {
    "total_time_ms": 2345,
    "graph_nodes": 15,
    "graph_edges": 23,
    "dfs_traversal_steps": 47
  },
  "recommendations": [
    "Review fixed templates to ensure correctness",
    "Run validate_pipeline to confirm all cycles resolved",
    "Consider restructuring templates to prevent future cycles"
  ],
  "receipt": "sha256:abc123...",
  "elapsed_ms": 2345
}
```

**Usage Examples**:

**Basic cycle fixing**:
```bash
{
  "name": "fix_cycles",
  "arguments": {
    "ontology_path": "schema/ontology.ttl"
  }
}
```

**Dry-run to preview changes**:
```bash
{
  "name": "fix_cycles",
  "arguments": {
    "ontology_path": "schema/ontology.ttl",
    "dry_run": true,
    "verbose": true
  }
}
```

**Specific fix strategy**:
```bash
{
  "name": "fix_cycles",
  "arguments": {
    "ontology_path": "schema/ontology.ttl",
    "strategy": "remove_edge",
    "backup": true
  }
}
```

**Example Cycle Break**:

**Before** (cycle detected):
```
template_a.tera:
  {% include "template_b.tera" %}

template_b.tera:
  {% include "template_c.tera" %}

template_c.tera:
  {% include "template_a.tera" %}  # Creates cycle: A → B → C → A
```

**After** (fixed):
```
template_a.tera:
  {% include "template_b.tera" %}

template_b.tera:
  {% include "template_c.tera" %}

template_c.tera:
  {# Cycle broken: include to template_a.tera removed #}
  {# Original content of template_a.tera was: #}
  {# {% include "template_b.tera" %} #}
```

**Algorithm Details**:

**Cycle Detection (DFS-based)**:
```rust
fn detect_cycles(graph: &Graph) -> Vec<Vec<Node>> {
    let mut cycles = Vec::new();
    let mut visited = HashSet::new();
    let mut recursion_stack = Vec::new();

    for node in graph.nodes() {
        if !visited.contains(&node) {
            dfs_visit(node, &graph, &mut visited, &mut recursion_stack, &mut cycles);
        }
    }

    cycles
}

fn dfs_visit(
    node: Node,
    graph: &Graph,
    visited: &mut HashSet<Node>,
    recursion_stack: &mut Vec<Node>,
    cycles: &mut Vec<Vec<Node>>
) {
    visited.insert(node.clone());
    recursion_stack.push(node.clone());

    for neighbor in graph.neighbors(&node) {
        if !visited.contains(&neighbor) {
            dfs_visit(neighbor, graph, visited, recursion_stack, cycles);
        } else if recursion_stack.contains(&neighbor) {
            // Found a cycle!
            let cycle_start = recursion_stack.iter().position(|n| n == &neighbor).unwrap();
            let cycle = recursion_stack[cycle_start..].to_vec();
            cycles.push(cycle);
        }
    }

    recursion_stack.pop();
}
```

**Strategy Selection**:
```rust
fn select_strategy(cycle: &[Node], graph: &Graph) -> FixStrategy {
    // Strategy 1: Remove edge if cycle length > 3
    if cycle.len() > 3 {
        return FixStrategy::RemoveEdge;
    }

    // Strategy 2: Inline template if small file
    let smallest_node = cycle.iter().min_by_key(|n| n.file_size()).unwrap();
    if smallest_node.file_size() < 10_000 {  // < 10KB
        return FixStrategy::InlineTemplate(smallest_node.clone());
    }

    // Default: Remove edge
    FixStrategy::RemoveEdge
}
```

---

### Agent 2: SPARQLValidator

**Purpose**: Validate and automatically fix SPARQL queries

**Capabilities**:
- ✅ **Syntax Validation**: Check SPARQL 1.1 query syntax
- 🔍 **Semantic Validation**: Verify ontology namespace references
- 🔧 **Auto-Fix**: Correct common query errors
- 📝 **Optimization Suggestions**: Improve query performance
- 🌐 **Multi-Dialect Support**: SPARQL 1.1, SPARQL* (for RDF*)

**How It Works**:

1. **Syntax Parsing**:
   - Parse query using SPARQL 1.1 grammar
   - Check prefix declarations
   - Validate query form (SELECT/ASK/CONSTRUCT/DESCRIBE)
   - Verify triple patterns and filter expressions

2. **Semantic Validation**:
   - Load ontology for namespace checking
   - Verify prefix references exist in ontology
   - Check property and class references
   - Validate variable usage consistency

3. **Auto-Fix Rules**:
   - Add missing prefix declarations
   - Fix malformed filter expressions
   - Correct bracket mismatches
   - Normalize whitespace

4. **Optimization Analysis**:
   - Suggest SELECT DISTINCT for duplicate elimination
   - Recommend FILTER placement
   - Identify unused variables
   - Propose subquery optimization

**Parameters**:
```typescript
{
  query_path: string;          // Path to .rq SPARQL query file
  ontology_path: string;       // Path to .ttl ontology for semantic validation
  auto_fix?: boolean;          // Automatically fix issues (default: true)
  check_semantics?: boolean;   // Enable semantic validation (default: true)
  optimize?: boolean;          // Provide optimization suggestions (default: true)
  verbose?: boolean;           // Enable detailed output (default: false)
  output_format?: "json" | "text";  // Output format (default: "json")
}
```

**Return Format**:
```json
{
  "agent": "SPARQLValidator",
  "status": "valid" | "fixed" | "invalid",
  "query_info": {
    "query_type": "SELECT",
    "variables": ["?skill_name", "?system_prompt", "?language"],
    "prefixes": ["a2a:", "rdf:", "rdfs:"],
    "triple_patterns": 3,
    "filters": 1
  },
  "validation_details": {
    "syntax": {
      "status": "valid",
      "errors": [],
      "warnings": []
    },
    "semantics": {
      "status": "valid",
      "errors": [],
      "warnings": [
        "Variable ?unused_var is declared but never used"
      ]
    }
  },
  "fixes_applied": [
    {
      "issue": "Missing prefix declaration",
      "fix": "Added PREFIX ex: <http://example.org/ex#>",
      "line": 1
    }
  ],
  "optimization_suggestions": [
    "Consider using SELECT DISTINCT to eliminate duplicates",
    "Move FILTER(?x > 0) before triple pattern matching for better performance"
  ],
  "backup_path": "/tmp/ggen_backup_20250330_143022",
  "receipt": "sha256:def456...",
  "elapsed_ms": 456
}
```

**Usage Examples**:

**Basic validation**:
```bash
{
  "name": "validate_sparql",
  "arguments": {
    "query_path": "queries/extract-skills.rq",
    "ontology_path": "schema/ontology.ttl"
  }
}
```

**Auto-fix disabled**:
```bash
{
  "name": "validate_sparql",
  "arguments": {
    "query_path": "queries/extract-skills.rq",
    "ontology_path": "schema/ontology.ttl",
    "auto_fix": false
  }
}
```

**Example Fixes**:

**Fix 1: Missing Prefix**
```sparql
# Before (invalid)
SELECT * WHERE {
  ?s ex:property ?o .
}

# After (fixed)
PREFIX ex: <http://example.org/ex#>
SELECT * WHERE {
  ?s ex:property ?o .
}
```

**Fix 2: Missing Semicolon**
```sparql
# Before (invalid)
SELECT * WHERE {
  ?s a :Class ;
  ?p :Object .
}

# After (fixed)
SELECT * WHERE {
  ?s a :Class ;
     ?p :Object .
}
```

**Fix 3: Unused Variable**
```sparql
# Before (warning)
SELECT ?skill_name ?unused_var WHERE {
  ?skill a2a:skillName ?skill_name .
}

# After (fixed)
SELECT ?skill_name WHERE {
  ?skill a2a:skillName ?skill_name .
}
```

**Optimization Example**:
```sparql
# Before (suboptimal)
SELECT ?skill_name ?system_prompt WHERE {
  ?skill a2a:skillName ?skill_name ;
         a2a:hasSystemPrompt ?system_prompt .
  FILTER(?skill_name != "test")
}

# After (optimized)
SELECT ?skill_name ?system_prompt WHERE {
  ?skill a2a:skillName ?skill_name ;
         a2a:hasSystemPrompt ?system_prompt .
  FILTER(?skill_name != "test")
}
# Optimization: Move FILTER before property paths for early pruning
```

---

### Agent 3: TemplateValidator

**Purpose**: Validate and automatically fix Tera templates

**Capabilities**:
- ✅ **Syntax Validation**: Check Tera template syntax
- 🔍 **Variable Validation**: Verify variable references exist in context
- 🔧 **Auto-Fix**: Correct common template errors
- 📝 **Include Validation**: Check included templates exist
- 🎨 **Filter Validation**: Verify filter usage and parameters

**How It Works**:

1. **Template Parsing**:
   - Parse template using Tera parser
   - Check variable blocks `{% %}`
   - Check expression blocks `{{ }}`
   - Check comment blocks `{# #}`

2. **Variable Validation**:
   - Extract all variable references
   - Load ontology context for available variables
   - Check variables exist in context
   - Validate filter usage

3. **Include Validation**:
   - Extract `{% include "..." %}` statements
   - Verify included files exist
   - Check for circular includes
   - Validate include paths

4. **Auto-Fix Rules**:
   - Add missing `{% endif %}` and `{% endfor %}`
   - Fix malformed filter expressions
   - Correct variable name typos
   - Add default filters for optional variables

**Parameters**:
```typescript
{
  templates_dir: string;       // Path to templates directory
  ontology_path: string;       // Path to .ttl ontology for variable validation
  auto_fix?: boolean;          // Automatically fix issues (default: true)
  check_variables?: boolean;   // Validate variable references (default: true)
  check_includes?: boolean;    // Validate include statements (default: true)
  verbose?: boolean;           // Enable detailed output (default: false)
  output_format?: "json" | "text";  // Output format (default: "json")
}
```

**Return Format**:
```json
{
  "agent": "TemplateValidator",
  "status": "valid" | "fixed" | "invalid",
  "templates_validated": 5,
  "validation_details": {
    "syntax": {
      "status": "valid",
      "errors": [],
      "warnings": []
    },
    "variables": {
      "status": "valid",
      "errors": [],
      "warnings": [
        "Variable 'optional_var' in template_b.tera may be undefined"
      ]
    },
    "includes": {
      "status": "valid",
      "errors": [],
      "warnings": []
    }
  },
  "template_breakdown": [
    {
      "template": "skill.tera",
      "status": "valid",
      "variables_used": ["skill_name", "system_prompt", "generated_impl"],
      "includes": ["header.tera"],
      "errors": [],
      "warnings": []
    }
  ],
  "fixes_applied": [
    {
      "template": "template_b.tera",
      "issue": "Missing {% endif %}",
      "fix": "Added {% endif %} at line 23",
      "line": 23
    }
  ],
  "recommendations": [
    "Use default filter for optional variables",
    "Consider using {% set %} for complex expressions"
  ],
  "backup_path": "/tmp/ggen_backup_20250330_143022",
  "receipt": "sha256:ghi789...",
  "elapsed_ms": 789
}
```

**Usage Examples**:

**Basic validation**:
```bash
{
  "name": "validate_templates",
  "arguments": {
    "templates_dir": "templates/",
    "ontology_path": "schema/ontology.ttl"
  }
}
```

**Auto-fix disabled**:
```bash
{
  "name": "validate_templates",
  "arguments": {
    "templates_dir": "templates/",
    "ontology_path": "schema/ontology.ttl",
    "auto_fix": false
  }
}
```

**Example Fixes**:

**Fix 1: Unclosed Block**
```tera
{# Before (invalid) #}
{% if condition %}
  {{ value }}

{# After (fixed) #}
{% if condition %}
  {{ value }}
{% endif %}
```

**Fix 2: Undefined Variable**
```tera
{# Before (invalid) #}
{{ undefined_variable }}

{# After (fixed) #}
{{ undefined_variable|default(value="N/A") }}

{# Or check existence #}
{% if undefined_variable is defined %}
  {{ undefined_variable }}
{% endif %}
```

**Fix 3: Invalid Filter**
```tera
{# Before (invalid) #}
{{ value|invalid_filter }}

{# After (fixed) #}
{{ value|upper }}
{{ value|default(value="default") }}
```

**Fix 4: Missing Include**
```tera
{# Before (invalid) #}
{% include "non_existent.tera" %}

{# After (fixed) #}
{% include "existing.tera" %}
```

## Safety Features

### Automatic Backups

All agents create automatic backups before making modifications:

**Backup Structure**:
```
/tmp/ggen_backup_YYYYMMDD_HHMMSS/
├── ontology/
│   └── schema/
│       └── ontology.ttl
├── queries/
│   ├── extract-skills.rq
│   └── extract-properties.rq
├── templates/
│   ├── skill.tera
│   └── header.tera
└── manifest.json  # Backup metadata
```

**Backup Metadata** (`manifest.json`):
```json
{
  "backup_timestamp": "2025-03-30T14:30:22Z",
  "agent": "CycleBreaker",
  "reason": "Pre-fix backup before cycle breaking",
  "files_backed_up": 15,
  "backup_path": "/tmp/ggen_backup_20250330_143022",
  "original_paths": {
    "ontology": "/path/to/schema/ontology.ttl",
    "queries": "/path/to/queries/",
    "templates": "/path/to/templates/"
  }
}
```

### Dry-Run Mode

Preview changes without applying them:

```bash
{
  "name": "fix_cycles",
  "arguments": {
    "ontology_path": "schema/ontology.ttl",
    "dry_run": true,
    "verbose": true
  }
}
```

**Dry-run output**:
```json
{
  "status": "success",
  "dry_run": true,
  "changes_preview": [
    {
      "file": "template_c.tera",
      "change_type": "remove_line",
      "line": 3,
      "content": "{% include \"template_a.tera\" %}",
      "reason": "Breaking cycle A → B → C → A"
    }
  ],
  "recommendations": [
    "Review changes before applying without dry-run"
  ]
}
```

### Rollback Capability

Restore from backup if fixes cause issues:

```bash
# List available backups
ggen mcp list-backups

# Restore from specific backup
ggen mcp rollback --backup /tmp/ggen_backup_20250330_143022

# Verify rollback
ggen mcp validate-pipeline --ontology schema/ontology.ttl
```

## Integration with CI/CD

### GitHub Actions Workflow

```yaml
name: ggen Quality Agents

on: [push, pull_request]

jobs:
  quality-check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2

      - name: Install ggen
        run: cargo install ggen-cli

      - name: Run CycleBreaker
        run: |
          ggen mcp fix-cycles \
            --ontology schema/ontology.ttl \
            --dry-run true

      - name: Run SPARQLValidator
        run: |
          ggen mcp validate-sparql \
            --query queries/extract.rq \
            --ontology schema/ontology.ttl

      - name: Run TemplateValidator
        run: |
          ggen mcp validate-templates \
            --templates templates/ \
            --ontology schema/ontology.ttl

      - name: Validate Pipeline
        run: |
          ggen mcp validate-pipeline \
            --ontology schema/ontology.ttl
```

### Pre-Commit Hook

```bash
#!/bin/bash
# .git/hooks/pre-commit

echo "Running A2A fixing agents..."

# Detect and fix cycles
if ! ggen mcp fix-cycles --ontology schema/ontology.ttl --dry-run true; then
    echo "❌ Circular dependencies detected"
    echo "Run: ggen mcp fix-cycles --ontology schema/ontology.ttl"
    exit 1
fi

# Validate SPARQL
if ! ggen mcp validate-sparql --query queries/extract.rq --ontology schema/ontology.ttl; then
    echo "❌ SPARQL validation failed"
    exit 1
fi

# Validate templates
if ! ggen mcp validate-templates --templates templates/ --ontology schema/ontology.ttl; then
    echo "❌ Template validation failed"
    exit 1
fi

echo "✅ All quality checks passed"
exit 0
```

## Performance Characteristics

| Agent | Small Project | Medium Project | Large Project |
|-------|---------------|----------------|---------------|
| CycleBreaker | <1s | <3s | <7s |
| SPARQLValidator | <0.5s | <2s | <5s |
| TemplateValidator | <0.5s | <2s | <5s |

**Resource Usage**:
- **Memory**: ~50-200MB depending on project size
- **CPU**: Single-core utilization
- **Disk**: ~10-50MB for temporary backups

## Best Practices

### 1. Run Agents Regularly

```bash
# Before committing changes
ggen mcp fix-cycles --ontology schema/ontology.ttl
ggen mcp validate-sparql --query queries/extract.rq --ontology schema/ontology.ttl
ggen mcp validate-templates --templates templates/ --ontology schema/ontology.ttl

# Then run sync
ggen sync
```

### 2. Use Dry-Run First

```bash
# Preview changes
ggen mcp fix-cycles --ontology schema/ontology.ttl --dry-run true

# Review output
# Then apply for real
ggen mcp fix-cycles --ontology schema/ontology.ttl --backup true
```

### 3. Validate After Fixes

```bash
# Apply fixes
ggen mcp fix-cycles --ontology schema/ontology.ttl

# Validate results
ggen mcp validate-pipeline --ontology schema/ontology.ttl

# Run sync
ggen sync
```

### 4. Keep Backups

```bash
# Always use backup flag
ggen mcp fix-cycles --ontology schema/ontology.ttl --backup true

# Keep backups for rollback
# Don't delete backups immediately
```

## Troubleshooting

### Common Issues

**Issue 1**: "Agent failed to create backup"
- **Cause**: Insufficient disk space or permissions
- **Fix**: Check disk space and `/tmp` permissions

**Issue 2**: "Agent detected cycle but failed to fix"
- **Cause**: Complex cycle requiring manual intervention
- **Fix**: Review cycle manually, apply fix, re-run agent

**Issue 3**: "SPARQL validation passed but query fails"
- **Cause**: Semantic validation missed runtime error
- **Fix**: Test query manually, check endpoint

**Issue 4**: "Template validation passed but rendering fails"
- **Cause**: Variable context mismatch
- **Fix**: Check SPARQL query returns expected variables

**Issue 5**: "Rollback failed"
- **Cause**: Backup directory missing or corrupted
- **Fix**: Check backup integrity, restore manually

### Debug Mode

```bash
# Enable verbose output
ggen mcp fix-cycles --ontology schema/ontology.ttl --verbose true

# Check agent logs
tail -f /tmp/ggen_agents.log

# Enable debug logging
export RUST_LOG=debug
ggen mcp fix-cycles --ontology schema/ontology.ttl
```

## References

- **MCP Quality Tools**: [`docs/mcp-quality-tools.md`](mcp-quality-tools.md)
- **LLM Generation Integration**: [`docs/llm-generation-integration.md`](llm-generation-integration.md)
- **MCP Server Documentation**: [`crates/ggen-a2a-mcp/README.md`](../crates/ggen-a2a-mcp/README.md)
- **ggen CLI Reference**: [`README.md`](../README.md)

## Support

For issues, questions, or contributions:
- **GitHub Issues**: [https://github.com/seanchatmangpt/ggen/issues](https://github.com/seanchatmangpt/ggen/issues)
- **Documentation**: [`docs/`](../docs/)
- **Examples**: [`examples/`](../examples/)
