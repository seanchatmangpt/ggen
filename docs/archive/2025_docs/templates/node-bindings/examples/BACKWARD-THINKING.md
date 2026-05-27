# Backward Thinking: Output → Ontology → Queries → Templates

This document explains the **data-driven** approach to generating Node.js bindings.

## The Problem with Forward Thinking

❌ **Forward (Wrong)**: Template → Hardcoded Structure → Output

```tera
{# Hardcoded! #}
export const parseRdf = native.parseRdf;
export const generateCode = native.generateCode;
```

This creates **inflexible** templates that can't adapt to different ontologies.

## The Solution: Backward Thinking

✅ **Backward (Right)**: Desired Output → Data Model → SPARQL → Template

### Step 1: Define Desired Output

**What do we want to generate?**

```javascript
// desired-output.mjs

/**
 * Parse RDF file and return triples
 * @param {string} filePath - Path to RDF file
 * @param {ParseOptions=} options - Parse options
 * @returns {Promise<Triple[]>} Array of RDF triples
 * @throws {Error} If parsing fails
 */
export const parseRdf = native.parseRdf;
```

### Step 2: Identify Required Data

**What data is needed to generate this?**

From the output, we need:
- Function name: `"parseRdf"`
- Description: `"Parse RDF file and return triples"`
- Parameters:
  - Name: `"filePath"`, Type: `"string"`, Required: `true`
  - Name: `"options"`, Type: `"ParseOptions"`, Required: `false`
- Return type: `"Triple[]"`
- Async: `true`
- Throws: `true`

### Step 3: Design RDF Ontology

**How do we represent this data in RDF?**

```turtle
:ParseRdfFunction a ffi:Function ;
    ffi:name "parseRdf" ;
    ffi:rustName "parse_rdf_file" ;
    ffi:returnType "Triple[]" ;
    ffi:async true ;
    ffi:throws true ;
    rdfs:comment "Parse RDF file and return triples" ;
    ffi:hasParameter :FilePathParam, :OptionsParam .

:FilePathParam a ffi:Parameter ;
    ffi:name "filePath" ;
    ffi:type "string" ;
    ffi:required true ;
    rdfs:comment "Path to RDF file" .

:OptionsParam a ffi:Parameter ;
    ffi:name "options" ;
    ffi:type "ParseOptions" ;
    ffi:required false ;
    rdfs:comment "Parse options" .
```

### Step 4: Create SPARQL Queries

**How do we extract this data?**

```sparql
PREFIX ffi: <https://ggen.dev/ontology/node-ffi#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?funcName ?description ?returnType ?async ?throws
WHERE {
  ?func a ffi:Function ;
        ffi:name ?funcName ;
        ffi:returnType ?returnType .

  OPTIONAL { ?func rdfs:comment ?description }
  OPTIONAL { ?func ffi:async ?async }
  OPTIONAL { ?func ffi:throws ?throws }
}
ORDER BY ?funcName
```

### Step 5: Build Tera Templates

**How do we transform query results into output?**

```tera
{%- for func in functions %}
/**
 * {{ func.description }}
{%- for param in func.parameters %}
 * @param {{'{'}}{{ param.type }}{% if not param.required %}={% endif %}} {{ param.name }}{% if param.description %} - {{ param.description }}{% endif %}
{%- endfor %}
 * @returns {{'{'}}{% if func.async %}Promise<{{ func.returnType }}>{% else %}{{ func.returnType }}{% endif %}}
{%- if func.throws %}
 * @throws {Error} If operation fails
{%- endif %}
 */
export const {{ func.funcName }} = native.{{ func.funcName }};
{% endfor %}
```

## The Data Flow

```
┌─────────────────────────────────────────────────────────────┐
│ 1. RDF Ontology (Source of Truth)                          │
│    - Describes functions, parameters, types                 │
│    - Turtle format (.ttl)                                   │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ 2. SPARQL Queries (Data Extraction)                        │
│    - Extract functions, parameters, types                   │
│    - Defined in TOML configuration                         │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ 3. Query Results (Structured Data)                         │
│    - JSON/Arrays passed to templates                        │
│    - { funcName: "parseRdf", async: true, ... }            │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ 4. Tera Templates (Transformation)                         │
│    - Loop over query results                                │
│    - Generate MJS/JSDoc output                             │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ 5. Generated Output (index.mjs)                            │
│    - Pure JavaScript with JSDoc                             │
│    - Matches desired-output.mjs                            │
└─────────────────────────────────────────────────────────────┘
```

## Key Principle

> **The ontology is the schema, SPARQL is the query, Tera is the view**

This is like a database-driven application:
- **RDF** = Database (stores data)
- **SPARQL** = SQL queries (retrieves data)
- **Tera** = Templates/Views (renders data)

## Benefits

1. **Flexible**: Change ontology → different output
2. **Reusable**: Same templates work for any ontology following the schema
3. **Maintainable**: Data and presentation separated
4. **Deterministic**: Same ontology always produces same output
5. **Type-safe**: Ontology validates structure via SHACL

## Example: Adding a New Function

**Just add to the ontology:**

```turtle
:NewFunction a ffi:Function ;
    ffi:name "newFunction" ;
    ffi:returnType "string" ;
    ffi:async false ;
    rdfs:comment "Does something new" .
```

**Templates automatically pick it up!** No code changes needed.

## Contrast

| Approach | Templates | Ontology | Flexibility |
|----------|-----------|----------|-------------|
| **Forward** | Hardcoded | Ignored | Low |
| **Backward** | Data-driven | Source of truth | High |

---

**Remember**: Think from output backwards to data, not from data forward to output.
