# Graph-Driven Nuxt Generation with ggen

## The Problem

The AI was creating **raw Nuxt files** (`.vue`, `.ts`) directly in the marketplace templates directory. This violates ggen's core principle: **all code should be generated from RDF knowledge graphs**.

## The Solution: Template + Graph Architecture

ggen uses a three-layer architecture:

```
┌─────────────────────────────────────────┐
│  1. RDF Knowledge Graph (.ttl)          │  ← Semantic metadata
│     Defines WHAT to generate            │
├─────────────────────────────────────────┤
│  2. SPARQL Queries                       │  ← Data extraction
│     Extracts metadata from graph        │
├─────────────────────────────────────────┤
│  3. ggen Templates (.tmpl)               │  ← Code generation
│     Renders code using graph data       │
└─────────────────────────────────────────┘
           ↓
    Generated Code (.vue, .ts)
```

## Example: Generating a Nuxt Page

### ❌ WRONG: Create raw file directly

```bash
# This bypasses the graph-driven approach
cat > pages/index.vue << 'EOF'
<template>
  <div>Hello</div>
</template>
EOF
```

### ✅ CORRECT: Define in graph, generate from template

**Step 1: Define in RDF graph** (`marketplace-project.ttl`)

```turtle
@prefix nuxt: <http://ggen.io/nuxt#> .

mp:IndexPage rdf:type nuxt:Page ;
    nuxt:name "index" ;
    nuxt:path "/" ;
    nuxt:template """<div>
  <h1>Template Marketplace</h1>
  <PackageGrid :packages="packages" />
</div>""" ;
    nuxt:script """const { packages, fetchPackages } = usePackages()

onMounted(async () => {
  await fetchPackages()
})""" .
```

**Step 2: Create ggen template** (`templates/page.tmpl`)

```yaml
---
to: "pages/{{page_name}}.vue"
rdf:
  - "graphs/marketplace-project.ttl"
  - "graphs/nuxt-ontology.ttl"
sparql:
  - |
    SELECT ?template ?script
    WHERE {
      ?page a nuxt:Page ;
            nuxt:name "{{page_name}}" ;
            nuxt:template ?template ;
            nuxt:script ?script .
    }
---
<script setup lang="ts">
{{ sparql_results.script }}
</script>

<template>
{{ sparql_results.template }}
</template>
```

**Step 3: Generate code**

```bash
ggen gen templates/page.tmpl \
  --vars page_name=index \
  --graph graphs/marketplace-project.ttl
```

## Benefits of Graph-Driven Generation

1. **Single Source of Truth**: The RDF graph is the authoritative source
2. **Deterministic**: Same graph + same template = identical output
3. **Queryable**: Use SPARQL to find relationships and dependencies
4. **Evolvable**: Change the graph, regenerate everything
5. **Semantic**: Rich metadata enables intelligent tooling
6. **Language-Agnostic**: Same graph can generate multiple implementations

## Directory Structure

```
cli/marketplace/templates/nuxt-marketplace/
├── graphs/
│   ├── nuxt-ontology.ttl        # Nuxt vocabulary (Component, Page, etc.)
│   └── marketplace-project.ttl  # Specific marketplace artifacts
├── templates/
│   ├── page.tmpl                # Generates pages/*.vue
│   ├── component.tmpl           # Generates components/*.vue
│   ├── composable.tmpl          # Generates composables/*.ts
│   ├── layout.tmpl              # Generates layouts/*.vue
│   └── api.tmpl                 # Generates server/api/*.ts
├── generate.sh                  # Generation orchestration script
└── README.md                    # Usage instructions
```

## Ontology Design

The `nuxt-ontology.ttl` defines the vocabulary:

```turtle
nuxt:Page rdf:type rdfs:Class ;
    rdfs:label "Nuxt Page" .

nuxt:Component rdf:type rdfs:Class ;
    rdfs:label "Nuxt Component" .

nuxt:Composable rdf:type rdfs:Class ;
    rdfs:label "Nuxt Composable" .

nuxt:name rdf:type rdf:Property ;
    rdfs:domain nuxt:Component ;
    rdfs:range xsd:string .

nuxt:template rdf:type rdf:Property ;
    rdfs:domain nuxt:Component ;
    rdfs:range xsd:string .

nuxt:usesComposable rdf:type rdf:Property ;
    rdfs:domain nuxt:Component ;
    rdfs:range nuxt:Composable .
```

## Generation Workflow

```bash
# 1. Design your application semantically in RDF
vim graphs/marketplace-project.ttl

# 2. Create templates that query the graph
vim templates/page.tmpl

# 3. Generate all artifacts
bash generate.sh

# 4. Review generated code
ls -R generated/

# 5. Modify graph and regenerate (deterministic!)
vim graphs/marketplace-project.ttl
bash generate.sh
```

## SPARQL Query Examples

### Find all pages and their routes

```sparql
SELECT ?page ?name ?path
WHERE {
  ?page a nuxt:Page ;
        nuxt:name ?name ;
        nuxt:path ?path .
}
```

### Find components used by a page

```sparql
SELECT ?component ?componentName
WHERE {
  mp:IndexPage nuxt:usesComponent ?component .
  ?component nuxt:name ?componentName .
}
```

### Find all composables

```sparql
SELECT ?composable ?name ?returnType
WHERE {
  ?composable a nuxt:Composable ;
              nuxt:name ?name ;
              nuxt:returnType ?returnType .
}
```

## Key Principles

1. **Never create raw source files in templates directory**
   - Templates directory should ONLY contain `.tmpl` files and RDF graphs

2. **All application structure lives in RDF**
   - Pages, components, composables defined semantically
   - SPARQL queries extract the structure
   - Templates render the code

3. **Generation is deterministic**
   - Same graph + same seed = identical output every time
   - Critical for reproducible builds and testing

4. **Graph is the contract**
   - Frontend and backend can share the same ontology
   - Multiple implementations from one semantic model
   - Enables cross-language consistency

## Testing Graph-Driven Generation

```bash
# Verify RDF graphs are valid Turtle
rapper -i turtle graphs/nuxt-ontology.ttl

# Test SPARQL queries
sparql --data=graphs/marketplace-project.ttl --query=test.sparql

# Generate with determinism verification
ggen gen templates/page.tmpl --vars page_name=index --determinism 42
ggen gen templates/page.tmpl --vars page_name=index --determinism 42
diff output1.vue output2.vue  # Should be identical
```

## Migration from Raw Files

If raw files were created, migrate them to graph-driven:

1. **Extract metadata from existing files**
   ```bash
   # Analyze existing component
   cat pages/index.vue
   ```

2. **Define in RDF graph**
   ```turtle
   mp:IndexPage rdf:type nuxt:Page ;
       nuxt:name "index" ;
       nuxt:template "..." ;
       nuxt:script "..." .
   ```

3. **Create template that generates the file**
   ```yaml
   ---
   to: "pages/{{page_name}}.vue"
   sparql:
     - "SELECT ?template ?script WHERE { ... }"
   ---
   <template>{{ sparql_results.template }}</template>
   ```

4. **Delete raw file, generate from graph**
   ```bash
   rm pages/index.vue
   ggen gen templates/page.tmpl --vars page_name=index
   ```

## Advanced: Multi-Language Generation

The same RDF graph can generate multiple implementations:

```
marketplace-project.ttl  (semantic model)
        │
        ├─→ templates/nuxt/page.tmpl     → pages/index.vue
        ├─→ templates/react/page.tmpl    → pages/index.tsx
        └─→ templates/docs/page.tmpl     → docs/pages.md
```

This enables:
- Vue + React from same specification
- Auto-generated documentation
- Type definitions across languages
- API contracts shared between frontend/backend

## References

- [ggen README](../README.md)
- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [SPARQL Tutorial](https://www.w3.org/TR/sparql11-query/)
- [Tera Templates](https://tera.netlify.app/)
