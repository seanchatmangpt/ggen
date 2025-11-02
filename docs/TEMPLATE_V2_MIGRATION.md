# Template v2 Migration Guide

## Overview

This guide explains the migration from v1 template rendering to v2 RDF-integrated template system.

## Key Changes

### v1 → v2 Architecture

```rust
// v1: Simple template rendering
template.render(context)

// v2: RDF-integrated rendering (with backward compatibility)
template.render_with_rdf(rdf_files, graph, tera, context, template_path)

// v2: Maintains v1 API for backward compatibility
template.render(tera, context)  // Still works!
```

### New Capabilities

1. **RDF/SPARQL Integration**
   - Load RDF files via CLI/API (not frontmatter)
   - Execute SPARQL queries in templates
   - Access query results in Tera templates

2. **TTL → Template Generation**
   - Generate templates FROM RDF metadata
   - Reverse operation: RDF describes template structure

3. **Preprocessor Support**
   - Advanced template processing
   - Freeze blocks for incremental updates
   - Custom preprocessing stages

## CLI Usage

### v1 Style (Backward Compatible)

```bash
# Basic template rendering (works as before)
ggen template generate \
  --template my-template.tmpl \
  --output output.txt \
  --var name=Alice \
  --var version=1.0
```

### v2 Style (RDF Integration)

```bash
# Render with RDF data
ggen template generate \
  --template my-template.tmpl \
  --rdf data.ttl \
  --rdf schema.ttl \
  --output output.txt \
  --var name=Alice

# Use preprocessor for advanced features
ggen template generate \
  --template my-template.tmpl \
  --rdf data.ttl \
  --output output.txt \
  --preprocessor

# Generate template FROM RDF metadata
ggen template generate \
  --from-rdf \
  --rdf template-metadata.ttl \
  --template generated.tmpl
```

## API Usage

### Domain Layer (v1 Compatible)

```rust
use ggen::domain::template::{GenerateFileOptions, generate_file};

let options = GenerateFileOptions::new(
    PathBuf::from("template.tmpl"),
    PathBuf::from("output.txt")
)
.with_var("name", "Alice")
.with_var("version", "1.0");

let result = generate_file(&options)?;
println!("Generated: {}", result.output_path.display());
```

### Domain Layer (v2 RDF Integration)

```rust
use ggen::domain::template::{RenderWithRdfOptions, render_with_rdf};

let options = RenderWithRdfOptions::new(
    PathBuf::from("template.tmpl"),
    PathBuf::from("output.txt")
)
.with_rdf_file(PathBuf::from("data.ttl"))
.with_rdf_file(PathBuf::from("schema.ttl"))
.with_var("name", "Alice")
.with_preprocessor(true);

let result = render_with_rdf(&options)?;
println!("Generated: {}", result.output_path.display());
println!("RDF files loaded: {}", result.rdf_files_loaded);
println!("SPARQL queries executed: {}", result.sparql_queries_executed);
```

### Generate Template FROM RDF

```rust
use ggen::domain::template::generate_from_rdf;

let rdf_files = vec![PathBuf::from("template-metadata.ttl")];
let output_template = PathBuf::from("generated.tmpl");

let result_path = generate_from_rdf(rdf_files, output_template)?;
println!("Generated template: {}", result_path.display());
```

## Template Frontmatter Changes

### v1 Frontmatter (DEPRECATED Fields)

```yaml
---
to: "output.txt"
# ❌ REMOVED: RDF files now loaded via CLI/API
rdf:
  - data.ttl
  - schema.ttl
# ❌ REMOVED: Variables now come from CLI/API
vars:
  name: Alice
  version: "1.0"
---
```

### v2 Frontmatter (Recommended)

```yaml
---
to: "output.txt"

# ✅ Inline RDF (convenience feature, still supported)
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "@prefix ex: <http://example.org/> ."
  - "ex:Alice a ex:Person ; ex:age 30 ."

# ✅ SPARQL queries (results available in template)
sparql:
  people: "SELECT ?person ?name WHERE { ?person a ex:Person ; ex:name ?name }"
  count: "SELECT (COUNT(?p) AS ?total) WHERE { ?p a ex:Person }"
---
```

## Template Body Changes

### Accessing SPARQL Results

```tera
{# v2: Access SPARQL query results #}
Found {{ sparql_results.people | length }} person(s)

{# First result column value #}
First person: {{ sparql_first(results=sparql_results.people, column="name") }}

{# All values from a column #}
Names: {{ sparql_values(results=sparql_results.people, column="name") }}

{# Check if results are empty #}
{% if sparql_empty(results=sparql_results.people) %}
  No people found
{% endif %}

{# Count results #}
Total: {{ sparql_count(results=sparql_results.people) }}
```

### Backward Compatible Variables

```tera
{# v1 & v2: Variables work the same #}
Hello, {{ name }}!
Version: {{ version }}
```

## Migration Checklist

- [ ] **Review frontmatter**: Remove `rdf:` and `vars:` fields
- [ ] **Update CLI calls**: Add `--rdf` for RDF files, use `--var` for variables
- [ ] **Update API calls**: Use `RenderWithRdfOptions` if using RDF, or keep `GenerateFileOptions` for v1 compatibility
- [ ] **Test SPARQL queries**: Verify query results are accessible in templates
- [ ] **Enable preprocessor**: Add `--preprocessor` flag if using advanced features
- [ ] **Update documentation**: Reference v2 API in your project docs

## Integration with v2 RDF System

### RDF Files → Template Context

```rust
// Load RDF files and make them available to templates
let rdf_files = vec![
    PathBuf::from("project.ttl"),
    PathBuf::from("team.ttl"),
];

let options = RenderWithRdfOptions::new(template_path, output_path)
    .with_rdf_files(rdf_files);

// RDF data is now available for SPARQL queries in template
let result = render_with_rdf(&options)?;
```

### SPARQL Query Results → Tera Context

Templates can execute SPARQL queries and access results:

```yaml
---
sparql:
  team_members: |
    SELECT ?name ?role WHERE {
      ?person a :TeamMember ;
              :name ?name ;
              :role ?role .
    }
---
# Team Members
{% for member in sparql_results.team_members %}
- {{ member.name }}: {{ member.role }}
{% endfor %}
```

## Backward Compatibility

All v1 template functionality is preserved:

- ✅ Templates without RDF work as before
- ✅ `GenerateFileOptions` API unchanged
- ✅ CLI commands support v1 usage patterns
- ✅ Frontmatter fields (to, from, inject, etc.) work unchanged
- ✅ Tera filters and functions available

## Performance Considerations

- **RDF Loading**: RDF files are loaded once per render
- **SPARQL Execution**: Queries execute during frontmatter processing
- **Caching**: Results cached in frontmatter for template access
- **Preprocessor**: Adds minimal overhead, use only when needed

## Example: Complete v2 Template

### RDF Data (team.ttl)

```turtle
@prefix : <http://example.org/> .

:alice a :TeamMember ;
    :name "Alice" ;
    :role "Developer" ;
    :skills "Rust, Python" .

:bob a :TeamMember ;
    :name "Bob" ;
    :role "Designer" ;
    :skills "Figma, UI/UX" .
```

### Template (team-readme.tmpl)

```yaml
---
to: "TEAM.md"

prefixes:
  ex: "http://example.org/"

sparql:
  team: |
    SELECT ?name ?role ?skills WHERE {
      ?person a ex:TeamMember ;
              ex:name ?name ;
              ex:role ?role ;
              ex:skills ?skills .
    }
    ORDER BY ?name
---
# {{ project_name }} Team

{% for member in sparql_results.team %}
## {{ member.name }}
- **Role**: {{ member.role }}
- **Skills**: {{ member.skills }}
{% endfor %}

Total team members: {{ sparql_count(results=sparql_results.team) }}
```

### CLI Usage

```bash
ggen template generate \
  --template team-readme.tmpl \
  --rdf team.ttl \
  --output TEAM.md \
  --var project_name="My Project"
```

### Output (TEAM.md)

```markdown
# My Project Team

## Alice
- **Role**: Developer
- **Skills**: Rust, Python

## Bob
- **Role**: Designer
- **Skills**: Figma, UI/UX

Total team members: 2
```

## Troubleshooting

### Template Not Found
```
Error: Template not found: /path/to/template.tmpl
```
**Solution**: Verify template path is correct and file exists.

### RDF Parse Error
```
Error: Failed to parse RDF: ...
```
**Solution**: Validate TTL syntax using online validators or `rapper` tool.

### SPARQL Query Error
```
Error: SPARQL solution error: ...
```
**Solution**: Test query in SPARQL playground, check prefix declarations.

### Missing Query Results
```
Template renders but sparql_results.* is empty
```
**Solution**: Verify query name matches frontmatter SPARQL key.

## Next Steps

1. Review [RDF API Quick Reference](./RDF_API_QUICK_REFERENCE.md)
2. Explore [Template RDF API Tests](../ggen-core/tests/template_rdf_api_tests.rs)
3. Check [Migration V1 to V2](./MIGRATION_V1_TO_V2.md) for broader changes
4. See [Core Team Recommendations](./CORE_TEAM_RECOMMENDATIONS.md) for best practices
