# Node.js Bindings Generator (3T Approach)

This directory contains the **3T** (TOML, Tera, Turtle) template system for generating Node.js bindings from Rust code using RDF ontologies.

## Overview

The Node.js bindings generator uses ggen's ontology-driven approach to create type-safe, deterministic Node.js bindings for Rust libraries.

### The 3T Stack

1. **TOML** (`node-bindings.toml`): Configuration
   - Project metadata
   - RDF prefixes
   - SPARQL queries
   - Build targets
   - Lifecycle hooks

2. **Tera** (`templates/*.tera`): Code generation templates
   - `package.json.tera`: npm package configuration
   - `index.d.ts.tera`: TypeScript declarations
   - `index.js.tera`: CommonJS wrapper
   - `index.mjs.tera`: ES Module wrapper
   - `README.md.tera`: Documentation
   - `tsconfig.json.tera`: TypeScript configuration
   - `binding.gyp.tera`: Native build configuration

3. **Turtle** (`ontology/node-ffi-ontology.ttl`): FFI domain model
   - Defines vocabulary for describing Node.js bindings
   - Models functions, parameters, types, structs, enums
   - Provides semantic foundation for code generation

## Directory Structure

```
node-bindings/
├── node-bindings.toml          # Configuration
├── ontology/
│   └── node-ffi-ontology.ttl   # FFI domain model
├── templates/
│   ├── package.json.tera       # npm package config
│   ├── index.d.ts.tera         # TypeScript declarations
│   ├── index.js.tera           # CommonJS wrapper
│   ├── index.mjs.tera          # ES Module wrapper
│   ├── native-loader.js.tera   # Native module loader
│   ├── tsconfig.json.tera      # TS configuration
│   ├── .npmignore.tera         # npm exclusions
│   ├── README.md.tera          # Documentation
│   └── binding.gyp.tera        # Native build config
├── examples/
│   └── example-usage.ttl       # Sample FFI definitions
└── README.md                   # This file
```

## Usage

### 1. Define Your API in RDF

Create a Turtle file describing your Rust API:

```turtle
@prefix ffi: <https://ggen.dev/ontology/node-ffi#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

:MyModule a ffi:Module ;
    ffi:name "my-rust-lib" ;
    ffi:version "1.0.0" ;
    ffi:hasFunction :HelloFunction .

:HelloFunction a ffi:Function ;
    ffi:functionName "hello" ;
    ffi:rustName "say_hello" ;
    ffi:returnType "string" ;
    ffi:async false ;
    rdfs:comment "Says hello" ;
    ffi:hasParameter :NameParam .

:NameParam a ffi:Parameter ;
    ffi:paramName "name" ;
    ffi:type "string" ;
    ffi:required true .
```

### 2. Generate Bindings

```bash
ggen generate \
  --template templates/node-bindings \
  --ontology my-api.ttl \
  --output generated/node-bindings
```

### 3. Build the Native Module

```bash
cd generated/node-bindings
npm install
npm run build
```

### 4. Use in Node.js

```javascript
const myLib = require('./generated/node-bindings');

const result = myLib.hello('World');
console.log(result); // "Hello, World!"
```

## Features

### Type Safety
- **TypeScript declarations** auto-generated from RDF
- **Type mappings** between Rust and TypeScript
- **Compile-time checks** for function signatures

### Error Handling
- **Native module loader** with helpful error messages
- **Exception wrapping** for better debugging
- **Validation** of parameters and return values

### Modern JavaScript
- **CommonJS** support for Node.js
- **ES Modules** support for modern environments
- **Dual package** with proper exports field

### Deterministic Output
- **No manual coding** - everything generated from ontology
- **Reproducible builds** - same ontology = same output
- **Version control friendly** - consistent formatting

## Ontology Schema

The FFI ontology defines these core classes:

- **ffi:Module**: A Node.js module
- **ffi:Function**: An exported function
- **ffi:Parameter**: A function parameter
- **ffi:Type**: A type mapping (Rust ↔ TypeScript)
- **ffi:Struct**: A Rust struct (→ TS interface)
- **ffi:Enum**: A Rust enum (→ TS enum/union)
- **ffi:Field**: A struct field
- **ffi:EnumVariant**: An enum variant

## SPARQL Queries

The TOML configuration includes pre-defined SPARQL queries:

1. **functions_query**: Extract all functions and metadata
2. **parameters_query**: Extract function parameters
3. **types_query**: Extract type mappings

These queries are executed against your ontology to populate Tera templates.

## Template Variables

Templates have access to:

- `module_name`: Module name from RDF
- `module_version`: Semantic version
- `module_description`: Module description
- `functions`: Array of function objects
- `types`: Array of type mappings
- `structs`: Array of struct definitions
- `enums`: Array of enum definitions

## Customization

### Add Custom Templates

Create new `.tera` files in `templates/`:

```tera
{# my-custom.tera #}
// Generated from {{ module_name }}
{% for func in functions %}
export const {{ func.functionName }} = ...;
{% endfor %}
```

### Modify SPARQL Queries

Edit `node-bindings.toml`:

```toml
[sparql]
my_query = """
PREFIX ffi: <https://ggen.dev/ontology/node-ffi#>
SELECT ?name WHERE { ?x ffi:name ?name }
"""
```

### Extend the Ontology

Add new properties to `node-ffi-ontology.ttl`:

```turtle
ffi:customProperty a rdf:Property ;
    rdfs:domain ffi:Function ;
    rdfs:range xsd:string .
```

## Integration with ggen

This template integrates with ggen's lifecycle hooks:

- **pre_generate**: Run `cargo make check` before generation
- **post_generate**: Run `npm install && npm run build` after

Configure in `node-bindings.toml`:

```toml
[lifecycle.phases.post_generate]
scripts = [
    "cd generated/node-bindings && npm install",
    "cd generated/node-bindings && npm test"
]
```

## Examples

See `examples/example-usage.ttl` for a complete example demonstrating:

- Module definition
- Function exports
- Parameter types
- Struct interfaces
- Enum types
- Type mappings
- Async functions
- Error handling

## Best Practices

1. **Version your ontology**: Use semantic versioning in RDF
2. **Document everything**: Add `rdfs:comment` to all entities
3. **Type mappings**: Define explicit Rust ↔ TypeScript mappings
4. **Validate early**: Use SHACL shapes to validate your ontology
5. **Test generation**: Run `cargo make test` after changes

## Troubleshooting

### Native module not found
```bash
npm run build  # Rebuild the native addon
```

### TypeScript errors
```bash
npm run build:ts  # Rebuild TypeScript declarations
```

### Outdated bindings
```bash
ggen generate --force  # Regenerate from ontology
```

## License

MIT

## Contributing

Contributions welcome! This is part of the ggen project.

## References

- [ggen Documentation](https://ggen.dev)
- [Neon Bindings](https://neon-bindings.com)
- [Node-API](https://nodejs.org/api/n-api.html)
- [RDF Turtle](https://www.w3.org/TR/turtle/)
- [Tera Templates](https://tera.netlify.app)
