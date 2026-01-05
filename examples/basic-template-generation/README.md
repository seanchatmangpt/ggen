# Basic Template Generation Example

A comprehensive tutorial demonstrating ggen's core functionality for generating Rust code from templates using YAML frontmatter and Tera templating.

**Status**: ✅ Complete and validated
**Difficulty**: ⭐ Beginner
**Time**: 15-30 minutes
**Focus**: Template fundamentals and code generation basics

---

## 1. Overview

This example teaches you how to use ggen to generate production-quality Rust code from reusable templates. You'll learn:

- **Template anatomy**: YAML frontmatter configuration + Tera template body
- **Variable substitution**: Inserting values into templates
- **Filters**: Transforming variables (snake_case, pascal_case, capitalize, etc.)
- **Conditional rendering**: {% if %} blocks for optional code sections
- **Loops**: {% for %} blocks for iterating over collections
- **Code generation workflow**: From specification to working code

**What this teaches**: This is the foundation for understanding ggen. Once you master these concepts, you can move on to more complex patterns like RDF ontologies, SPARQL queries, and multi-file generation.

**Key files**:
- `ontology/templates.ttl` - RDF specification of template concepts
- `ggen.toml` - 10 generation rules
- `templates/rust-module.tera` - Simple module template
- `templates/rust-struct.tera` - Complex struct template
- `validate.mjs` - Validation script
- `golden/` - Expected output files

---

## 2. Prerequisites

### Required Knowledge

- **YAML syntax** - Understanding basic key-value configuration
- **Rust basics** - Familiarity with structs, modules, and derive macros
- **Template concepts** - Understanding variable substitution and filters

### Required Tools

- `ggen` CLI (5.0.0+) installed and in PATH
- `Node.js` 18+ (for validation script)
- Rust toolchain (optional, for compiling generated code)

### Quick Check

```bash
# Verify ggen is installed
ggen --version

# Should output: ggen 5.0.x or later

# Verify Node.js
node --version

# Should output: v18.x or later
```

### Environment Setup

```bash
# Clone ggen repository
git clone https://github.com/seanchatmangpt/ggen

# Navigate to this example
cd ggen/examples/basic-template-generation

# Make validation script executable
chmod +x validate.mjs
```

---

## 3. Quick Start

### Run Full Example (5 minutes)

```bash
# 1. Navigate to example directory
cd examples/basic-template-generation

# 2. Validate environment
./validate.mjs

# 3. View template files
cat templates/rust-module.tera
cat templates/rust-struct.tera

# 4. Review generated output
cat golden/generated/user_service.rs
cat golden/generated/user.rs

# 5. Understand the flow
# templates/ → (ggen processes) → generated/
```

### Generate Code (once ggen is built)

```bash
# Generate all outputs
ggen sync

# Outputs will appear in:
# - generated/user_service.rs
# - generated/user.rs
# - generated/TEMPLATE_GUIDE.md
# - And more...
```

---

## 4. Architecture & Key Concepts

### The Template Generation Pipeline

```
┌──────────────────────────────────┐
│   ggen.toml Configuration        │
│  (10 generation rules)           │
└──────────────┬────────────────────┘
               │
               ▼
┌──────────────────────────────────┐
│   ontology/templates.ttl         │
│  (RDF specification)             │
└──────────────┬────────────────────┘
               │
               ▼
┌──────────────────────────────────┐
│   SPARQL Queries                 │
│  (Extract data from ontology)    │
└──────────────┬────────────────────┘
               │
               ▼
┌──────────────────────────────────┐
│   Tera Templates                 │
│  (Transform data to code)        │
└──────────────┬────────────────────┘
               │
               ▼
┌──────────────────────────────────┐
│   Generated Output               │
│  (.rs files, .md documentation)  │
└──────────────────────────────────┘
```

### Template Anatomy

Every ggen template has two parts:

#### Part 1: YAML Frontmatter

Located between `---` delimiters at the start:

```yaml
---
to: "output/path/{{ variable | filter }}.rs"
vars:
  variable_name: type
  another_var: type
---
```

**Fields**:
- `to`: Output file path (supports variables and filters)
- `vars`: Variables required by this template

#### Part 2: Tera Template Body

Everything after the frontmatter:

```rust
// Use variables: {{ variable_name }}
// Apply filters: {{ variable_name | snake_case }}
// Conditionals: {% if condition %}...{% endif %}
// Loops: {% for item in items %}...{% endfor %}
```

### Variable Types

| Type | Example | Usage |
|------|---------|-------|
| `string` | `"hello"` | Text values |
| `boolean` | `true` / `false` | Conditional logic |
| `number` | `42` | Numeric values |
| `array` | `["a", "b"]` | Iterate with {% for %} |
| `object` | `{key: "value"}` | Nested structures |

### Common Filters

| Filter | Example | Output |
|--------|---------|--------|
| `snake_case` | `UserService` → `user_service` | Module names |
| `pascal_case` | `user_service` → `UserService` | Type names |
| `camel_case` | `user_service` → `userService` | JS/JSON |
| `capitalize` | `user` → `User` | Capitalize first |
| `upper` | `user` → `USER` | All uppercase |
| `lower` | `USER` → `user` | All lowercase |

### Tera Syntax Reference

```tera
{{ variable }}                    # Variable substitution
{{ variable | filter }}           # Apply filter
{% if condition %}...{% endif %}  # Conditional
{% for x in y %}...{% endfor %}   # Loop
{# comment #}                     # Comment
```

---

## 5. File Structure

```
examples/basic-template-generation/
├── ggen.toml                      # Generation configuration (10 rules)
├── ontology/
│   └── templates.ttl              # RDF specification (SHACL validation)
├── templates/                     # Tera template files
│   ├── rust-module.tera           # Simple module template
│   ├── rust-struct.tera           # Complex struct template
│   ├── documentation.tera         # Guide generation
│   ├── tests.tera                 # Test module generation
│   ├── filters-reference.tera     # Filter documentation
│   ├── variable-types.tera        # Type documentation
│   ├── frontmatter-examples.tera  # YAML examples
│   ├── tera-syntax-guide.tera     # Syntax reference
│   └── troubleshooting-guide.tera # Error solutions
├── golden/                        # Expected outputs (test fixtures)
│   └── generated/
│       ├── user_service.rs        # Golden file for module
│       └── user.rs                # Golden file for struct
├── validate.mjs                   # Validation script (Node.js)
└── README.md                      # This file
```

### Key Directories

- **`ggen.toml`**: Manifest defining all 10 generation rules
- **`ontology/`**: RDF ontology with SHACL constraints
- **`templates/`**: 9 Tera templates producing various outputs
- **`golden/`**: Expected outputs for regression testing
- **`generated/`**: Output directory (created by ggen sync)

---

## 6. Step-by-Step Tutorial

### Step 1: Understand Template Files (5 minutes)

Examine the two main templates:

```bash
# View simple module template
cat templates/rust-module.tera

# Notice:
# - YAML frontmatter with "to:" and "vars:"
# - Variable substitutions {{ module_name }}
# - Filters: | snake_case
# - Conditional: {% if has_validation %}
# - Tera conditionals and loops
```

**Learning**: This template shows basic variable substitution and conditionals.

```bash
# View complex struct template
cat templates/rust-struct.tera

# Notice:
# - Multiple variable types (string, boolean, array)
# - Loop: {% for field in fields %}
# - Nested filters: {{ struct_name | pascal_case }}
# - Complex Rust code generation
```

**Learning**: This template demonstrates loops and multiple filters.

### Step 2: Review the Ontology (5 minutes)

The RDF ontology defines template concepts:

```bash
# View ontology
cat ontology/templates.ttl

# You'll see:
# - Classes: Template, RustModule, RustStruct, TeraFilter, VariableType
# - Properties: name, description, fields, has_validation
# - SHACL validation constraints (sh:NodeShape)
# - Example instances (user_service_module, user_struct, filters)
```

**Learning**: The ontology is the specification. SPARQL queries extract data from it.

### Step 3: Examine Configuration (5 minutes)

Review how generation rules work:

```bash
# View generation manifest
cat ggen.toml

# You'll see:
# 1. [project] section: name, version, description
# 2. [ontology] section: source file, prefixes
# 3. [[inference.rules]]: CONSTRUCT queries (optional enrichment)
# 4. [[generation.rules]]: SELECT queries + templates
# Each rule: SPARQL query → Template → Output file
```

**Learning**: ggen.toml orchestrates the entire generation pipeline.

### Step 4: Inspect Golden Files (5 minutes)

View expected outputs:

```bash
# Module output
cat golden/generated/user_service.rs

# Notice:
# - Variables substituted: {{ module_name }} → user_service
# - Filters applied: {{ module_name | snake_case }}
# - Conditionals rendered: {% if has_validation %} → generated
# - Complete, compilable Rust code
```

```bash
# Struct output
cat golden/generated/user.rs

# Notice:
# - Conditional ID field: generated because has_id: true
# - Loops rendered: multiple fields from array
# - Builder pattern: auto-generated from template
# - Full implementation with tests
```

**Learning**: Golden files show what correct output looks like.

### Step 5: Run Validation (3 minutes)

Execute the validation script:

```bash
# Run validation
./validate.mjs

# Checks:
# ✅ ggen installation
# ✅ Specification closure (SHACL)
# ✅ Code generation infrastructure
# ✅ Golden file structure
# ✅ Template syntax
# ✅ README completeness
```

### Step 6: Generate Code (when ggen is built)

```bash
# Generate all outputs from specifications
ggen sync

# This runs all 10 generation rules:
# 1. generate-rust-module
# 2. generate-rust-struct
# 3. generate-documentation
# 4. generate-test-module
# 5. generate-filters-reference
# 6. generate-variable-types
# 7. generate-frontmatter-examples
# 8. generate-tera-syntax-guide
# 9. generate-troubleshooting-guide
# 10. Additional synthesis rules
```

---

## 7. Configuration Reference

### ggen.toml Structure

```toml
[project]
name = "basic-template-generation"
version = "0.1.0"

[ontology]
source = "ontology/templates.ttl"

[[generation.rules]]
name = "generate-rust-module"
sparql = """
PREFIX tmpl: <https://ggen.io/ontology/templates#>
SELECT ?moduleName ?moduleDescription
WHERE {
  ?module a tmpl:RustModule ;
    tmpl:name ?moduleName ;
    rdfs:comment ?moduleDescription .
}
"""
template = { file = "templates/rust-module.tera" }
output_file = "generated/{{ module_name | snake_case }}.rs"
mode = "Overwrite"
```

### SPARQL Query Patterns

**Select all entities of a type:**
```sparql
PREFIX tmpl: <https://ggen.io/ontology/templates#>
SELECT ?name ?description
WHERE {
  ?entity a tmpl:RustModule ;
    tmpl:name ?name ;
    rdfs:comment ?description .
}
```

**Select with optional properties:**
```sparql
SELECT ?name ?hasValidation
WHERE {
  ?module a tmpl:RustModule ;
    tmpl:name ?name .
  OPTIONAL { ?module tmpl:hasValidation ?hasValidation }
}
```

**Select with filters:**
```sparql
SELECT ?name
WHERE {
  ?filter a tmpl:TeraFilter ;
    tmpl:name ?name .
}
ORDER BY ?name
```

---

## 8. Troubleshooting

### Template Validation Fails

**Error**: `Invalid YAML frontmatter`

**Cause**: Frontmatter syntax error or missing delimiters

**Solution**:
1. Check templates start with `---`
2. Verify YAML indentation (2 spaces, not tabs)
3. Ensure `---` closes the frontmatter

```yaml
---
to: "path/{{ var }}.rs"  # ✅ Correct
vars:
  var: string
---
```

### Unknown Filter

**Error**: `Unknown filter: xyz`

**Cause**: Using unsupported Tera filter

**Solution**:
1. Check filter spelling
2. Use only Tera built-in filters:
   - `snake_case`, `pascal_case`, `camel_case`
   - `capitalize`, `upper`, `lower`, `trim`
3. Apply with pipe: `{{ var | filter }}`

### Variable Not Provided

**Error**: `Variable 'xyz' not provided`

**Cause**: Template requires variable not in context

**Solution**:
1. Add to `vars:` section in frontmatter
2. Verify spelling matches template usage
3. Ensure type is correct (string, boolean, array, etc.)

### Generation Fails

**Error**: `Failed to render template`

**Cause**: Template syntax error

**Solution**:
1. Check Tera syntax: `{{ }}`, `{% %}`, `{# #}`
2. Verify variable names
3. Check loop/conditional closure:
   ```tera
   {% if condition %}      # Opens
   ...
   {% endif %}             # Must close
   ```

### Generated Code Doesn't Compile

**Error**: Rust compilation errors

**Solution**:
1. Check variable substitutions produce valid Rust
2. Verify filter output (e.g., `snake_case` produces valid identifiers)
3. Test template manually with ggen library
4. Check golden files for reference implementation

---

## 9. Next Steps

### Learning Progression

**Level 1 - Templates (you are here)**
- ✅ Basic template anatomy
- ✅ Variable substitution and filters
- ✅ Conditional rendering and loops

**Level 2 - RDF Ontologies** (next)
- RDF/Turtle syntax
- SHACL constraints
- Ontology design patterns
- See: `examples/source-code-analysis/`

**Level 3 - SPARQL Queries**
- SELECT queries for data extraction
- CONSTRUCT queries for enrichment
- Query optimization
- See: `examples/openapi/`

**Level 4 - Multi-File Generation**
- Multiple output files per rule
- Inter-file references
- Project scaffolding
- See: `examples/complete-project-generation/`

**Level 5 - AI-Powered Generation**
- Intent-driven templates
- LLM integration
- Template learning
- See: `examples/ai-template-creation/`

### Practice Exercises

1. **Modify the module template**
   - Add a new method to the generated module
   - Update conditionals to show/hide code sections
   - Verify golden files still match

2. **Create a new template**
   - Add `trait.tera` for trait generation
   - Define SPARQL query in ggen.toml
   - Add ontology instances
   - Generate and validate

3. **Extend the ontology**
   - Add new entity type to `templates.ttl`
   - Add SHACL validation constraints
   - Create corresponding template
   - Run full generation pipeline

### Related Examples

- **`source-code-analysis/`** - Extract templates from existing code
- **`openapi/`** - Generate OpenAPI specs, Zod schemas, TypeScript types
- **`mcp-integration/`** - Integrate with Claude Desktop via MCP
- **`complete-project-generation/`** - Generate full Rust web services

### Resources

- **Tera Documentation**: https://keats.github.io/tera/
- **RDF Primer**: https://www.w3.org/TR/rdf11-primer/
- **SPARQL Query Language**: https://www.w3.org/TR/sparql11-query/
- **ggen Documentation**: https://seanchatmangpt.github.io/ggen

### Success Criteria

You've mastered this example when you can:

- [ ] Explain YAML frontmatter purpose and structure
- [ ] List 5+ common Tera filters and their uses
- [ ] Write conditional templates with {% if %}
- [ ] Create loops with {% for %}
- [ ] Understand variable substitution
- [ ] Read and modify SPARQL queries
- [ ] Run ggen sync successfully
- [ ] Verify generated output matches golden files
- [ ] Troubleshoot template errors
- [ ] Extend templates with new features

---

## Summary

This example demonstrates ggen's core capability: **transforming specifications (RDF ontologies) into code (Rust modules) using templates (Tera syntax)**.

You've learned:
- Template anatomy (frontmatter + body)
- Variable substitution and filters
- Conditional and loop rendering
- The complete generation pipeline
- How to validate and debug templates

**Next**: Move to RDF ontologies and SPARQL queries in `examples/source-code-analysis/`

Happy generating! 🚀
