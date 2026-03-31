# MCP Quality Tools Documentation

## Overview

The ggen MCP server provides 4 specialized quality tools for validating and fixing your code generation pipeline. These tools offer comprehensive validation, automatic fixing, and safety features to ensure your ggen projects maintain high quality standards.

## Available Tools

| Tool | Purpose | Execution Time | Safety Features |
|------|---------|----------------|-----------------|
| **validate_pipeline** | Full μ₁-μ₅ pipeline validation | <5s | Read-only |
| **fix_cycles** | Detect and fix circular dependencies | <3s | Backup + dry-run |
| **validate_sparql** | Validate SPARQL queries | <2s | Read-only |
| **validate_templates** | Validate Tera templates | <2s | Read-only |

## Quick Start

### 1. Start the MCP Server

```bash
# Start with stdio transport (for Claude Desktop, etc.)
ggen mcp start-server --transport stdio

# Or start with HTTP transport
ggen mcp start-server --transport http --port 3000
```

### 2. Use Tools via MCP Client

**Example: Claude Desktop Configuration**

`~/.claude_desktop_config.json`:
```json
{
  "mcpServers": {
    "ggen": {
      "command": "ggen",
      "args": ["mcp", "start-server", "--transport", "stdio"]
    }
  }
}
```

**Example: Direct Tool Call**

```json
{
  "name": "validate_pipeline",
  "arguments": {
    "ontology_path": "schema/ontology.ttl",
    "queries_dir": "queries/",
    "output_dir": "src/generated/"
  }
}
```

## Tool Details

### Tool 1: validate_pipeline

**Purpose**: Comprehensive validation of the entire μ₁-μ₅ generation pipeline

**Parameters**:
```typescript
{
  ontology_path: string;      // Path to .ttl ontology file
  queries_dir?: string;       // Path to .rq SPARQL query files (default: "queries/")
  output_dir?: string;        // Output directory (default: "generated/")
  language?: string;          // Target language (default: "auto")
  verbose?: boolean;          // Enable detailed output (default: false)
}
```

**Validation Checks**:
- ✅ RDF syntax and semantic validity
- ✅ SPARQL query syntax and correctness
- ✅ Template compilation and variable references
- ✅ Dependency graph validation
- ✅ Output directory permissions
- ✅ Language-specific generator availability

**Return Format**:
```json
{
  "status": "success" | "partial" | "failed",
  "validation_summary": {
    "ontology": {
      "status": "valid" | "invalid",
      "triple_count": 127,
      "errors": [],
      "warnings": ["Optional warning"]
    },
    "sparql": {
      "status": "valid" | "invalid",
      "query_count": 5,
      "errors": [],
      "warnings": []
    },
    "templates": {
      "status": "valid" | "invalid",
      "template_count": 3,
      "errors": [],
      "warnings": []
    },
    "dependencies": {
      "status": "valid" | "invalid" | "cycles_detected",
      "cycle_count": 0,
      "errors": [],
      "warnings": []
    }
  },
  "overall_status": "valid" | "invalid" | "degraded",
  "recommendations": [
    "Fix SPARQL syntax error in extract-skills.rq line 15",
    "Add missing template variable 'skill_name' to skill.tera"
  ],
  "receipt": "sha256:abc123...",
  "elapsed_ms": 1234
}
```

**Usage Examples**:

**Basic validation**:
```bash
# Via MCP client
{
  "name": "validate_pipeline",
  "arguments": {
    "ontology_path": "schema/ontology.ttl"
  }
}

# Or via CLI
ggen mcp validate-pipeline --ontology schema/ontology.ttl
```

**Verbose mode with custom paths**:
```bash
{
  "name": "validate_pipeline",
  "arguments": {
    "ontology_path": "schema/ontology.ttl",
    "queries_dir": "custom/queries/",
    "output_dir": "dist/generated/",
    "language": "rust",
    "verbose": true
  }
}
```

**Error Scenarios**:

**Invalid RDF syntax**:
```json
{
  "status": "failed",
  "validation_summary": {
    "ontology": {
      "status": "invalid",
      "triple_count": 0,
      "errors": [
        "Parse error at line 45: Unexpected token '.'",
        "Invalid prefix 'ex2' not declared"
      ],
      "warnings": []
    }
  },
  "recommendations": [
    "Fix RDF syntax errors in ontology.ttl",
    "Declare all prefixes at the top of the file"
  ]
}
```

**Circular dependencies**:
```json
{
  "status": "partial",
  "validation_summary": {
    "dependencies": {
      "status": "cycles_detected",
      "cycle_count": 2,
      "errors": [],
      "warnings": [
        "Cycle detected: template_a.tera → template_b.tera → template_a.tera",
        "Cycle detected: query_c.rq → query_d.rq → query_c.rq"
      ]
    }
  },
  "recommendations": [
    "Run fix_cycles tool to automatically break cycles",
    "Manually review and restructure dependencies"
  ]
}
```

---

### Tool 2: fix_cycles

**Purpose**: Detect and automatically fix circular dependencies in the generation pipeline

**Parameters**:
```typescript
{
  ontology_path: string;      // Path to .ttl ontology file
  backup?: boolean;           // Create backup before fixing (default: true)
  dry_run?: boolean;          // Preview changes without applying (default: false)
  strategy?: "auto" | "remove_edge" | "inline_template";  // Fix strategy (default: "auto")
  verbose?: boolean;          // Enable detailed output (default: false)
}
```

**Fix Strategies**:
- **auto**: Automatically choose best strategy (default)
- **remove_edge**: Remove problematic edges from dependency graph
- **inline_template**: Inline template content to break dependency

**Safety Features**:
- 🛡️ **Automatic backup**: Creates timestamped backup before modifications
- 🧪 **Dry-run mode**: Preview changes without applying them
- 🔄 **Rollback capability**: Automatic rollback on failure
- 📊 **Detailed logging**: Complete audit trail of all changes

**Return Format**:
```json
{
  "status": "success" | "partial" | "failed",
  "cycles_found": 2,
  "cycles_fixed": 2,
  "fixes_applied": [
    {
      "cycle": ["template_a.tera", "template_b.tera", "template_c.tera", "template_a.tera"],
      "strategy_used": "remove_edge",
      "edge_removed": "template_c.tera → template_a.tera",
      "files_modified": ["template_c.tera"]
    },
    {
      "cycle": ["query_d.rq", "query_e.rq", "query_d.rq"],
      "strategy_used": "inline_template",
      "template_inlined": "query_e.rq",
      "files_modified": ["query_d.rq"]
    }
  ],
  "backup_path": "/tmp/ggen_backup_20250330_143022",
  "rollback_available": true,
  "recommendations": [
    "Review fixed templates to ensure correctness",
    "Run validate_pipeline to confirm all cycles resolved"
  ],
  "receipt": "sha256:def456...",
  "elapsed_ms": 2345
}
```

**Usage Examples**:

**Basic cycle fixing with backup**:
```bash
{
  "name": "fix_cycles",
  "arguments": {
    "ontology_path": "schema/ontology.ttl",
    "backup": true
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

# Output will show what would be changed without applying fixes
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

**Rollback**:
```bash
# If fixes cause issues, rollback from backup
ggen mcp rollback --backup /tmp/ggen_backup_20250330_143022
```

**Cycle Detection Algorithm**:

1. **Build dependency graph**:
   - Parse all templates and queries
   - Extract `{% include "..." %}` directives
   - Extract SPARQL `FROM` and `SERVICE` clauses
   - Build directed graph

2. **Detect cycles**:
   - Use depth-first search (DFS)
   - Track recursion stack
   - Identify back edges (cycles)

3. **Fix cycles**:
   - Analyze cycle structure
   - Choose optimal breaking strategy
   - Apply fix with backup
   - Validate fix

**Example Cycle Break**:

**Before** (cycle):
```
template_a.tera:
  {% include "template_b.tera" %}

template_b.tera:
  {% include "template_c.tera" %}

template_c.tera:
  {% include "template_a.tera" %}  # Creates cycle!
```

**After** (fixed):
```
template_a.tera:
  {% include "template_b.tera" %}

template_b.tera:
  {% include "template_c.tera" %}

template_c.tera:
  {# Cycle broken: include inlined #}
  {# Content from template_a.tera was here #}
```

---

### Tool 3: validate_sparql

**Purpose**: Validate SPARQL queries for syntax and semantic correctness

**Parameters**:
```typescript
{
  query_path: string;          // Path to .rq SPARQL query file
  ontology_path: string;       // Path to .ttl ontology for semantic validation
  check_semantics?: boolean;   // Enable semantic validation (default: true)
  verbose?: boolean;           // Enable detailed output (default: false)
}
```

**Validation Checks**:
- ✅ SPARQL syntax correctness
- ✅ Prefix declarations validity
- ✅ Query type compatibility (SELECT/ASK/CONSTRUCT/DESCRIBE)
- ✅ Variable usage consistency
- ✅ Ontology namespace references
- ✅ Filter expression validity
- ✅ Optional/Union clause structure

**Return Format**:
```json
{
  "status": "valid" | "invalid" | "warning",
  "query_info": {
    "query_type": "SELECT",
    "variables": ["?skill_name", "?system_prompt", "?language"],
    "prefixes": ["a2a:", "rdf:", "rdfs:"],
    "triple_patterns": 3
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
  "optimization_suggestions": [
    "Consider adding FILTER to reduce result set",
    "Use SELECT DISTINCT to avoid duplicates"
  ],
  "receipt": "sha256:ghi789...",
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

**Syntax-only validation (faster)**:
```bash
{
  "name": "validate_sparql",
  "arguments": {
    "query_path": "queries/extract-skills.rq",
    "ontology_path": "schema/ontology.ttl",
    "check_semantics": false
  }
}
```

**Error Scenarios**:

**Syntax error**:
```json
{
  "status": "invalid",
  "validation_details": {
    "syntax": {
      "status": "invalid",
      "errors": [
        "Parse error at line 12: Expected '.' or '}'",
        "Unclosed triple pattern",
        "Invalid FILTER expression"
      ],
      "warnings": []
    }
  },
  "recommendations": [
    "Fix syntax errors on lines 10-15",
    "Check bracket matching"
  ]
}
```

**Semantic error**:
```json
{
  "status": "invalid",
  "validation_details": {
    "semantics": {
      "status": "invalid",
      "errors": [
        "Prefix 'undefined:' not declared in ontology",
        "Property ex:nonExistentProperty not found in ontology"
      ],
      "warnings": [
        "Variable ?unused_var is declared but never used"
      ]
    }
  },
  "recommendations": [
    "Declare all prefixes in query",
    "Verify property names exist in ontology"
  ]
}
```

**Common SPARQL Issues**:

**Issue 1: Undefined prefix**
```sparql
# Wrong
SELECT * WHERE {
  ?s undefined:property ?o .
}

# Correct
PREFIX undefined: <http://example.org/undefined#>
SELECT * WHERE {
  ?s undefined:property ?o .
}
```

**Issue 2: Missing semicolon**
```sparql
# Wrong
SELECT * WHERE {
  ?s a :Class ;
  ?p :Object .
}

# Correct
SELECT * WHERE {
  ?s a :Class ;
     ?p :Object .
}
```

**Issue 3: Mismatched brackets**
```sparql
# Wrong
FILTER(?x > 0 && ?y < 10)

# Correct
FILTER(?x > 0 && ?y < 10)
```

---

### Tool 4: validate_templates

**Purpose**: Validate Tera templates for compilation and syntax errors

**Parameters**:
```typescript
{
  templates_dir: string;       // Path to templates directory
  ontology_path: string;       // Path to .ttl ontology for variable validation
  check_variables?: boolean;   // Validate variable references (default: true)
  verbose?: boolean;           // Enable detailed output (default: false)
}
```

**Validation Checks**:
- ✅ Template syntax correctness
- ✅ Variable block structure (`{% %}`)
- ✅ Expression block structure (`{{ }}`)
- ✅ Comment block structure (`{# #}`)
- ✅ Filter usage validity
- ✅ Macro definitions and calls
- ✅ Include statements validity
- ✅ Variable references against context

**Return Format**:
```json
{
  "status": "valid" | "invalid" | "warning",
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
        "Variable 'undefined_var' in template_b.tera not found in context"
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
  "recommendations": [
    "Remove or fix undefined variable 'undefined_var'",
    "Consider using default filter for optional variables"
  ],
  "receipt": "sha256:jkl012...",
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

**Syntax-only validation (faster)**:
```bash
{
  "name": "validate_templates",
  "arguments": {
    "templates_dir": "templates/",
    "ontology_path": "schema/ontology.ttl",
    "check_variables": false
  }
}
```

**Error Scenarios**:

**Syntax error**:
```json
{
  "status": "invalid",
  "validation_details": {
    "syntax": {
      "status": "invalid",
      "errors": [
        "template_b.tera:15 - Unclosed {% if %} block",
        "template_c.tera:23 - Unexpected EOF in {% for %} block"
      ],
      "warnings": []
    }
  },
  "recommendations": [
    "Add missing {% endif %} in template_b.tera",
    "Add missing {% endfor %} in template_c.tera"
  ]
}
```

**Variable error**:
```json
{
  "status": "invalid",
  "validation_details": {
    "variables": {
      "status": "invalid",
      "errors": [
        "template_a.tera:8 - Variable 'non_existent_var' not found in context"
      ],
      "warnings": [
        "template_b.tera:12 - Variable 'optional_var' may be undefined"
      ]
    }
  },
  "recommendations": [
    "Fix variable name 'non_existent_var' in template_a.tera",
    "Use default filter for 'optional_var': {{ optional_var|default(value='N/A') }}"
  ]
}
```

**Common Tera Template Issues**:

**Issue 1: Unclosed block**
```tera
{# Wrong #}
{% if condition %}
  {{ value }}
{# Missing {% endif %} #}

{# Correct #}
{% if condition %}
  {{ value }}
{% endif %}
```

**Issue 2: Undefined variable**
```tera
{# Wrong #}
{{ undefined_variable }}

{# Correct #}
{{ undefined_variable|default(value="N/A") }}

{# Or check existence #}
{% if undefined_variable is defined %}
  {{ undefined_variable }}
{% endif %}
```

**Issue 3: Invalid filter usage**
```tera
{# Wrong #}
{{ value|invalid_filter }}

{# Correct #}
{{ value|upper }}
{{ value|default(value="default") }}
```

**Issue 4: Missing include**
```tera
{# Wrong #}
{% include "non_existent.tera" %}

{# Correct #}
{% include "existing.tera" %}
```

## Integration with ggen Workflow

### Pre-Sync Validation

**Before running `ggen sync`**:
```bash
# 1. Validate entire pipeline
ggen mcp validate-pipeline --ontology schema/ontology.ttl

# 2. Fix any issues found
ggen mcp fix-cycles --ontology schema/ontology.ttl --backup true

# 3. Re-validate to confirm fixes
ggen mcp validate-pipeline --ontology schema/ontology.ttl

# 4. Run sync
ggen sync
```

### Continuous Validation

**In CI/CD pipeline**:
```yaml
# .github/workflows/ggen-validation.yml
name: ggen Quality Validation

on: [push, pull_request]

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Install ggen
        run: cargo install ggen-cli
      - name: Validate pipeline
        run: ggen mcp validate-pipeline --ontology schema/ontology.ttl
      - name: Validate SPARQL
        run: ggen mcp validate-sparql --query queries/extract.rq --ontology schema/ontology.ttl
      - name: Validate templates
        run: ggen mcp validate-templates --templates templates/ --ontology schema/ontology.ttl
      - name: Check for cycles
        run: ggen mcp fix-cycles --ontology schema/ontology.ttl --dry-run true
```

### Pre-Commit Hook

**`.git/hooks/pre-commit`**:
```bash
#!/bin/bash
# Pre-commit validation hook

echo "Running ggen quality checks..."

# Validate pipeline
if ! ggen mcp validate-pipeline --ontology schema/ontology.ttl; then
    echo "❌ Pipeline validation failed"
    echo "Run: ggen mcp validate-pipeline --ontology schema/ontology.ttl --verbose"
    exit 1
fi

# Check for cycles
if ! ggen mcp fix-cycles --ontology schema/ontology.ttl --dry-run true; then
    echo "❌ Circular dependencies detected"
    echo "Run: ggen mcp fix-cycles --ontology schema/ontology.ttl"
    exit 1
fi

echo "✅ All quality checks passed"
exit 0
```

## Error Handling and Recovery

### Backup Strategy

All modifying tools create automatic backups:

**Backup location**: `/tmp/ggen_backup_YYYYMMDD_HHMMSS/`

**Backup contents**:
- Original ontology files
- Original SPARQL queries
- Original templates
- Backup manifest (JSON)

**Rollback procedure**:
```bash
# List available backups
ggen mcp list-backups

# Restore from specific backup
ggen mcp rollback --backup /tmp/ggen_backup_20250330_143022

# Verify rollback
ggen mcp validate-pipeline --ontology schema/ontology.ttl
```

### Error Recovery

**Tool failure recovery**:
1. Check error message for specific issue
2. Review backup location
3. Apply manual fix if needed
4. Re-run validation
5. Restore from backup if necessary

**Example recovery workflow**:
```bash
# 1. Tool fails
ggen mcp fix-cycles --ontology schema/ontology.ttl
# Error: Failed to write fixed file

# 2. Check backup
ls -la /tmp/ggen_backup_*

# 3. Investigate issue
ggen mcp validate-pipeline --ontology schema/ontology.ttl --verbose

# 4. Apply manual fix or restore
ggen mcp rollback --backup /tmp/ggen_backup_20250330_143022

# 5. Re-validate
ggen mcp validate-pipeline --ontology schema/ontology.ttl
```

## Performance Considerations

### Execution Times

| Tool | Small Project | Medium Project | Large Project |
|------|---------------|----------------|---------------|
| validate_pipeline | <2s | <5s | <10s |
| fix_cycles | <1s | <3s | <7s |
| validate_sparql | <1s | <2s | <5s |
| validate_templates | <1s | <2s | <5s |

**Optimization tips**:
- Use `check_semantics: false` for faster syntax-only validation
- Run individual tools instead of full pipeline validation when possible
- Use dry-run mode to preview changes without overhead
- Cache validation results in CI/CD pipelines

### Resource Usage

**Memory**: ~50-200MB depending on project size
**CPU**: Single-core utilization (no parallel processing yet)
**Disk**: ~10-50MB for backups (temporary)

## Best Practices

### 1. Validation Workflow

```bash
# Always validate before fixing
ggen mcp validate-pipeline --ontology schema/ontology.ttl

# Fix issues with backup
ggen mcp fix-cycles --ontology schema/ontology.ttl --backup true

# Re-validate after fixes
ggen mcp validate-pipeline --ontology schema/ontology.ttl

# Then run sync
ggen sync
```

### 2. Safe Modifications

```bash
# Always use dry-run first
ggen mcp fix-cycles --ontology schema/ontology.ttl --dry-run true

# Review changes
# Then apply for real
ggen mcp fix-cycles --ontology schema/ontology.ttl --backup true
```

### 3. Continuous Quality

```bash
# Add to pre-commit hooks
# Add to CI/CD pipeline
# Run regularly during development
```

### 4. Troubleshooting

```bash
# Use verbose mode for debugging
ggen mcp validate-pipeline --ontology schema/ontology.ttl --verbose

# Check backups before restoring
ggen mcp list-backups

# Validate individual components
ggen mcp validate-sparql --query queries/extract.rq --ontology schema/ontology.ttl
ggen mcp validate-templates --templates templates/ --ontology schema/ontology.ttl
```

## Troubleshooting

### Common Issues

**Issue 1**: "Tool not found"
- **Cause**: MCP server not started
- **Fix**: Start server with `ggen mcp start-server`

**Issue 2**: "Permission denied"
- **Cause**: No write access to backup directory
- **Fix**: Check `/tmp` permissions or set custom backup location

**Issue 3**: "Ontology not found"
- **Cause**: Incorrect path to ontology file
- **Fix**: Use absolute path or check current directory

**Issue 4**: "Backup failed"
- **Cause**: Insufficient disk space
- **Fix**: Free up disk space or use custom backup location

**Issue 5**: "Rollback failed"
- **Cause**: Backup directory missing or corrupted
- **Fix**: Check backup integrity, restore manually if needed

## References

- **MCP Server Documentation**: [`crates/ggen-a2a-mcp/README.md`](../crates/ggen-a2a-mcp/README.md)
- **A2A Fixing Agents**: [`docs/a2a-fixing-agents.md`](a2a-fixing-agents.md)
- **LLM Generation Integration**: [`docs/llm-generation-integration.md`](llm-generation-integration.md)
- **ggen CLI Reference**: [`README.md`](../README.md)

## Support

For issues, questions, or contributions:
- **GitHub Issues**: [https://github.com/seanchatmangpt/ggen/issues](https://github.com/seanchatmangpt/ggen/issues)
- **Documentation**: [`docs/`](../docs/)
- **Examples**: [`examples/`](../examples/)
