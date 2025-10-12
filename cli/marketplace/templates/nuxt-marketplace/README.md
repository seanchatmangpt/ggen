# Nuxt Marketplace - Graph-Driven Generation Template

This is a **ggen template pack** that demonstrates the CORRECT way to generate Nuxt 4 applications from RDF knowledge graphs.

## 🚨 Important: This is NOT a Regular Nuxt Project

This directory contains:
- ✅ **RDF ontologies** defining Nuxt concepts semantically
- ✅ **ggen templates** that query the graph
- ✅ **Generation scripts** that produce Vue/TypeScript code
- ❌ **NO raw `.vue` or `.ts` files** (those are generated)

## Architecture

```
Graph-Driven Generation Flow:
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

1. Define Application Semantically (RDF)
   graphs/marketplace-project.ttl
        │
        │  mp:IndexPage rdf:type nuxt:Page ;
        │      nuxt:name "index" ;
        │      nuxt:template "..." .
        ↓

2. Query Graph with SPARQL
   templates/page.tmpl
        │
        │  SELECT ?template ?script WHERE {
        │    ?page nuxt:name "index" ;
        │          nuxt:template ?template
        │  }
        ↓

3. Generate Code with Tera
   ggen gen templates/page.tmpl --vars page_name=index
        ↓

4. Output: pages/index.vue
   <template>{{ data from graph }}</template>
```

## Quick Start

### 1. Install ggen

```bash
brew install ggen
# or
cargo install ggen
```

### 2. Generate Marketplace Application

```bash
cd cli/marketplace/templates/nuxt-marketplace
bash generate.sh
```

This will:
1. ✅ Validate RDF graphs
2. ✅ Query graphs with SPARQL
3. ✅ Generate pages, components, composables
4. ✅ Output complete Nuxt 4 project

### 3. Review Generated Files

```bash
ls -R generated/
# generated/
#   ├── pages/
#   │   ├── index.vue
#   │   └── package-[id].vue
#   ├── components/
#   │   ├── PackageCard.vue
#   │   └── PackageGrid.vue
#   └── composables/
#       ├── usePackages.ts
#       ├── usePackage.ts
#       └── useSearch.ts
```

## File Structure

```
nuxt-marketplace/
├── graphs/                         # RDF knowledge graphs
│   ├── nuxt-ontology.ttl          # Nuxt vocabulary definition
│   └── marketplace-project.ttl    # Marketplace application structure
│
├── templates/                      # ggen templates (NOT source code!)
│   ├── page.tmpl                  # Generates pages/*.vue
│   ├── component.tmpl             # Generates components/*.vue
│   ├── composable.tmpl            # Generates composables/*.ts
│   ├── layout.tmpl                # Generates layouts/*.vue
│   └── api.tmpl                   # Generates server/api/*.ts
│
├── generate.sh                     # Generation orchestration
└── README.md                       # This file
```

## The RDF Ontology

### Nuxt Concepts as Semantic Classes

```turtle
# graphs/nuxt-ontology.ttl

nuxt:Page rdf:type rdfs:Class .
nuxt:Component rdf:type rdfs:Class .
nuxt:Composable rdf:type rdfs:Class .
nuxt:Layout rdf:type rdfs:Class .
nuxt:Middleware rdf:type rdfs:Class .
nuxt:API rdf:type rdfs:Class .
```

### Marketplace Application Definition

```turtle
# graphs/marketplace-project.ttl

mp:IndexPage rdf:type nuxt:Page ;
    nuxt:name "index" ;
    nuxt:path "/" ;
    nuxt:usesComposable mp:UsePackagesComposable ;
    nuxt:template """<div>
      <h1>Template Marketplace</h1>
      <PackageGrid :packages="packages" />
    </div>""" .

mp:PackageCard rdf:type nuxt:Component ;
    nuxt:name "PackageCard" ;
    nuxt:hasProps "package: Package" ;
    nuxt:emits "['install', 'view']" .
```

## Template Examples

### Page Template

```yaml
# templates/page.tmpl
---
to: "pages/{{page_name}}.vue"
rdf:
  - "graphs/marketplace-project.ttl"
sparql:
  - |
    SELECT ?template ?script WHERE {
      ?page nuxt:name "{{page_name}}" ;
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

### Component Template

```yaml
# templates/component.tmpl
---
to: "components/{{component_name}}.vue"
sparql:
  - |
    SELECT ?template ?script ?style WHERE {
      ?comp nuxt:name "{{component_name}}" ;
            nuxt:template ?template ;
            nuxt:script ?script .
      OPTIONAL { ?comp nuxt:style ?style }
    }
---
<script setup lang="ts">
{{ sparql_results.script }}
</script>

<template>
{{ sparql_results.template }}
</template>

<style scoped>
{{ sparql_results.style }}
</style>
```

## Generation Commands

### Generate All Artifacts

```bash
bash generate.sh
```

### Generate Individual Artifacts

```bash
# Generate a single page
ggen gen templates/page.tmpl \
  --vars page_name=index \
  --graph graphs/marketplace-project.ttl

# Generate a single component
ggen gen templates/component.tmpl \
  --vars component_name=PackageCard \
  --graph graphs/marketplace-project.ttl

# Generate a composable
ggen gen templates/composable.tmpl \
  --vars composable_name=usePackages \
  --graph graphs/marketplace-project.ttl
```

## Benefits

### 1. Deterministic Generation
Same graph + same template = identical output every time.

```bash
ggen gen templates/page.tmpl --vars page_name=index --determinism 42
ggen gen templates/page.tmpl --vars page_name=index --determinism 42
diff page1.vue page2.vue  # Identical!
```

### 2. Single Source of Truth
The RDF graph is the authoritative definition of your application.

### 3. Multi-Language Generation
Generate Vue, React, documentation from the same graph:

```
marketplace-project.ttl
    ├─→ Nuxt/Vue (this template)
    ├─→ Next.js/React
    ├─→ OpenAPI spec
    └─→ Documentation
```

### 4. Semantic Queries
Use SPARQL to explore relationships:

```sparql
# Find all pages using a specific composable
SELECT ?page ?pageName WHERE {
  ?page a nuxt:Page ;
        nuxt:name ?pageName ;
        nuxt:usesComposable mp:UsePackagesComposable .
}
```

### 5. Evolvable Architecture
Change the graph, regenerate everything:

```bash
vim graphs/marketplace-project.ttl  # Modify graph
bash generate.sh                     # Regenerate all
```

## Extending the Template

### Add a New Page

1. **Define in graph:**
   ```turtle
   mp:AboutPage rdf:type nuxt:Page ;
       nuxt:name "about" ;
       nuxt:path "/about" ;
       nuxt:template "..." .
   ```

2. **Generate:**
   ```bash
   ggen gen templates/page.tmpl --vars page_name=about
   ```

### Add a New Component

1. **Define in graph:**
   ```turtle
   mp:SearchBar rdf:type nuxt:Component ;
       nuxt:name "SearchBar" ;
       nuxt:template "..." .
   ```

2. **Generate:**
   ```bash
   ggen gen templates/component.tmpl --vars component_name=SearchBar
   ```

## Testing

### Validate RDF Graphs

```bash
# Install rapper (RDF parser)
brew install raptor

# Validate graphs
rapper -i turtle graphs/nuxt-ontology.ttl
rapper -i turtle graphs/marketplace-project.ttl
```

### Test SPARQL Queries

```bash
# Install SPARQL CLI
brew install apache-jena

# Test queries
sparql --data=graphs/marketplace-project.ttl \
       --query=test-query.sparql
```

### Verify Determinism

```bash
# Generate twice with same seed
ggen gen templates/page.tmpl --determinism 42 -o out1/
ggen gen templates/page.tmpl --determinism 42 -o out2/

# Should be byte-identical
diff -r out1/ out2/
```

## Integration with Nuxt Project

Once generated, copy artifacts to your Nuxt project:

```bash
# Generate artifacts
bash generate.sh

# Copy to Nuxt project
cp -r generated/pages/* ~/my-nuxt-app/pages/
cp -r generated/components/* ~/my-nuxt-app/components/
cp -r generated/composables/* ~/my-nuxt-app/composables/

# Install dependencies and run
cd ~/my-nuxt-app
pnpm install
pnpm dev
```

## See Also

- [Graph-Driven Nuxt Generation Guide](../../../../docs/GRAPH_DRIVEN_NUXT_GENERATION.md)
- [ggen Documentation](https://seanchatmangpt.github.io/ggen/)
- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [SPARQL Query Language](https://www.w3.org/TR/sparql11-query/)

## License

MIT - See main ggen LICENSE
