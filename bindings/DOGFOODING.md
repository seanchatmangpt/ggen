# Dogfooding: Using ggen to Generate ggen's Own Bindings

## What is Dogfooding?

"Eating our own dog food" means **using ggen itself to generate ggen's multi-language bindings**.

Instead of manually writing Node.js, Python, and Erlang bindings, we:
1. Describe ggen's API once in RDF (`schema/ggen-ffi-api.ttl`)
2. Use `ggen sync` to generate all bindings automatically
3. Prove that ggen works by using it on itself!

## The Flow

```
┌──────────────────────────────────────────┐
│ schema/ggen-ffi-api.ttl                  │
│ (Describes ggen's API in RDF)            │
│ - parseRdf(file, format) → Triple[]      │
│ - generateCode(config) → string          │
│ - validateOntology(file) → Result        │
│ - executeSparql(file, query) → any       │
└──────────────┬───────────────────────────┘
               │
               ▼
┌──────────────────────────────────────────┐
│ ggen sync (THE TOOL ITSELF!)             │
│ $ ggen sync \                            │
│     --from schema/ggen-ffi-api.ttl \     │
│     --templates templates/node-bindings  │
└──────────────┬───────────────────────────┘
               │
               ├─────────────┬─────────────┬─────────────┐
               ▼             ▼             ▼             ▼
       ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐
       │ Node.js  │  │ Python   │  │ Erlang   │  │  Rust    │
       │ index.mjs│  │__init__.py│  │ ggen.erl │  │  lib.rs  │
       └──────────┘  └──────────┘  └──────────┘  └──────────┘
    (GENERATED!)  (GENERATED!)  (GENERATED!)  (GENERATED!)
```

## Files Created

### 1. API Ontology (Source of Truth)

**File**: `schema/ggen-ffi-api.ttl`

Describes ggen's actual API:
- 4 functions: parseRdf, generateCode, validateOntology, executeSparql
- 3 structs: Triple, GenerationConfig, ValidationResult
- 3 types: RdfFormat, Triple, GenerationConfig

### 2. Configuration

**File**: `bindings/ggen.toml`

Tells ggen where to find:
- Ontology: `../schema/`
- Templates: `../templates/`
- Output: `generated/`

### 3. Templates (Already Exist!)

```
templates/
├── node-bindings/      # Node.js/MJS/JSDoc templates
├── python-bindings/    # Python/PyO3 templates (TODO)
└── erlang-bindings/    # Erlang/Rustler templates (TODO)
```

## Commands to Run

### Generate Node.js Bindings

```bash
cd bindings/
../target/release/ggen sync \
  --from ../schema/ggen-ffi-api.ttl \
  --templates ../templates/node-bindings \
  --to generated/node/
```

### Generate Python Bindings

```bash
../target/release/ggen sync \
  --from ../schema/ggen-ffi-api.ttl \
  --templates ../templates/python-bindings \
  --to generated/python/
```

### Generate Erlang Bindings

```bash
../target/release/ggen sync \
  --from ../schema/ggen-ffi-api.ttl \
  --templates ../templates/erlang-bindings \
  --to generated/erlang/
```

## Expected Output (Node.js Example)

**File**: `bindings/generated/node/index.mjs`

```javascript
/**
 * ggen-bindings - Node.js bindings (ESM)
 * Generated from RDF ontology - DO NOT EDIT MANUALLY
 */

import { loadNative } from './native-loader.mjs';
const native = loadNative();

/**
 * @typedef {'turtle'|'ntriples'|'rdfxml'|'jsonld'} RdfFormat
 */

/**
 * @typedef {Object} Triple
 * @property {string} subject - Subject URI or blank node
 * @property {string} predicate - Predicate URI
 * @property {string} object - Object value
 */

/**
 * Parse an RDF file and return triples
 * @param {string} filePath - Path to RDF file to parse
 * @param {RdfFormat=} format - RDF serialization format
 * @returns {Promise<Triple[]>}
 * @throws {Error} If parsing fails
 */
export const parseRdf = native.parseRdf;

/**
 * Generate code from RDF ontology using Tera template
 * @param {GenerationConfig} config - Code generation configuration
 * @returns {Promise<string>}
 * @throws {Error} If generation fails
 */
export const generateCode = native.generateCode;

// ... more functions ...
```

## Why This Matters

**If ggen can't generate its own bindings, it doesn't work!**

This is the ultimate test:
- ✅ Templates work correctly
- ✅ SPARQL queries extract data properly
- ✅ Tera rendering produces valid code
- ✅ Multi-language support actually functions

## Next Steps

1. ✅ Created `schema/ggen-ffi-api.ttl`
2. ✅ Created `bindings/ggen.toml`
3. ⏳ Building ggen from source
4. 🔜 Run `ggen sync` for Node.js
5. 🔜 Verify generated bindings compile
6. 🔜 Add Python templates
7. 🔜 Add Erlang templates
8. 🔜 Generate all three languages
9. 🔜 Build and test all bindings

---

**We're using ggen to build ggen. That's AGI-level meta! 🚀**
