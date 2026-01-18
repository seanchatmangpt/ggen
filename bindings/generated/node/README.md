# ggen-bindings (Node.js)

Multi-language bindings for ggen code generator - Node.js ESM bindings.

**⚠️ Generated Code**: This package is generated from RDF ontology using the 3T approach (TOML, Tera, Turtle). Do not edit manually.

## Source

- **Ontology**: `../../../schema/ggen-ffi-api.ttl`
- **Templates**: `../../../templates/node-bindings/`
- **Configuration**: `../../ggen.toml`
- **Generator**: `ggen sync` (when CLI is fixed)

## Generation Method

This code was generated using the **3T approach**:

1. **Turtle (RDF)**: API described in `schema/ggen-ffi-api.ttl`
2. **Tera (Templates)**: Code templates in `templates/node-bindings/`
3. **TOML (Config)**: Generation config in `bindings/ggen.toml`

### Manual Generation

Due to a CLI verb discovery bug (see `../CLI_BUG.md`), this was generated manually by:

1. Reading the ontology: `schema/ggen-ffi-api.ttl`
2. Extracting function/type metadata
3. Applying templates from `templates/node-bindings/templates/index.mjs.tera`
4. Outputting to `bindings/generated/node/index.mjs`

### Regeneration (when CLI is fixed)

```bash
cd bindings/
ggen sync
```

## Installation

```bash
npm install ggen-bindings
```

**Note**: This package requires the native Rust addon to be built separately using Neon or N-API.

## Usage

```javascript
import { parseRdf, generateCode, validateOntology, executeSparql } from 'ggen-bindings';

// Parse an RDF file
const triples = await parseRdf('./ontology.ttl');
console.log(`Parsed ${triples.length} triples`);

// Generate code from template
const code = await generateCode({
  templatePath: './template.tera',
  ontologyPath: './schema.ttl',
  outputPath: './generated.js'
});

// Validate ontology
const validation = await validateOntology('./schema.ttl');
if (!validation.valid) {
  console.error('Errors:', validation.errors);
}

// Execute SPARQL query
const results = await executeSparql('./schema.ttl', `
  PREFIX ffi: <https://ggen.dev/ontology/node-ffi#>
  SELECT ?func ?name WHERE {
    ?func a ffi:Function ;
          ffi:functionName ?name .
  }
`);
```

## API

See type definitions in `index.mjs` (JSDoc annotations) or `index.d.mts` (TypeScript declarations).

### Functions

- `parseRdf(filePath, format?)` - Parse RDF file into triples
- `generateCode(config)` - Generate code from ontology + template
- `validateOntology(ontologyPath)` - Validate RDF ontology
- `executeSparql(ontologyPath, query)` - Execute SPARQL queries

### Types

- `RdfFormat` - Supported RDF formats ('turtle', 'ntriples', 'rdfxml', 'jsonld')
- `Triple` - RDF triple structure (subject, predicate, object)
- `GenerationConfig` - Code generation configuration
- `ValidationResult` - Ontology validation result

## License

MIT
