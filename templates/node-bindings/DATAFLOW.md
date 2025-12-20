# Node.js Bindings: Data-Driven Generation Flow

## The Backward-Thinking Process

```
OUTPUT → DATA → ONTOLOGY → SPARQL → TEMPLATE
  ↑                                      ↓
  └──────────────────────────────────────┘
          (Generated Code)
```

## Step-by-Step Example

### 1. Start with Desired Output

**File**: `examples/desired-output.mjs`

```javascript
/**
 * Parse RDF file and return triples
 * @param {string} filePath - Path to RDF file
 * @param {ParseOptions=} options - Parse options
 * @returns {Promise<Triple[]>} Array of RDF triples
 * @throws {Error} If parsing fails
 */
export const parseRdf = native.parseRdf;
```

### 2. Identify Required Data

From the output, we extract:

```json
{
  "funcName": "parseRdf",
  "rustName": "parse_rdf_file",
  "description": "Parse RDF file and return triples",
  "returnType": "Triple[]",
  "async": true,
  "throws": true,
  "parameters": [
    {
      "name": "filePath",
      "type": "string",
      "required": true,
      "description": "Path to RDF file"
    },
    {
      "name": "options",
      "type": "ParseOptions",
      "required": false,
      "description": "Parse options"
    }
  ]
}
```

**File**: `examples/query-results-example.json`

### 3. Model Data in RDF Ontology

**File**: `examples/concrete-ontology.ttl`

```turtle
:ParseRdfFunction a ffi:Function ;
    ffi:functionName "parseRdf" ;
    ffi:rustName "parse_rdf_file" ;
    ffi:returnType "Triple[]" ;
    ffi:async true ;
    ffi:throws true ;
    rdfs:comment "Parse RDF file and return triples" ;
    ffi:hasParameter :ParseRdfFilePathParam, :ParseRdfOptionsParam .

:ParseRdfFilePathParam a ffi:Parameter ;
    ffi:paramName "filePath" ;
    ffi:type "string" ;
    ffi:required true ;
    rdfs:comment "Path to RDF file" .

:ParseRdfOptionsParam a ffi:Parameter ;
    ffi:paramName "options" ;
    ffi:type "ParseOptions" ;
    ffi:required false ;
    rdfs:comment "Parse options" .
```

### 4. Extract Data with SPARQL

**File**: `queries/functions.sparql`

```sparql
PREFIX ffi: <https://ggen.dev/ontology/node-ffi#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?funcName ?rustName ?description ?returnType ?async ?throws
       ?paramName ?paramType ?paramRequired ?paramDescription
WHERE {
  ?func a ffi:Function ;
        ffi:functionName ?funcName ;
        ffi:returnType ?returnType .

  OPTIONAL { ?func ffi:rustName ?rustName }
  OPTIONAL { ?func rdfs:comment ?description }
  OPTIONAL { ?func ffi:async ?async }
  OPTIONAL { ?func ffi:throws ?throws }

  OPTIONAL {
    ?func ffi:hasParameter ?param .
    ?param ffi:paramName ?paramName ;
           ffi:type ?paramType .
    OPTIONAL { ?param ffi:required ?paramRequired }
    OPTIONAL { ?param rdfs:comment ?paramDescription }
  }
}
ORDER BY ?funcName ?paramName
```

### 5. Transform Data with Tera Template

**File**: `examples/data-driven-template.mjs.tera`

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

### 6. Get Generated Output

The template processes query results → produces `desired-output.mjs`

## The Complete Pipeline

```
┌─────────────────────────────────────────────────────────────────┐
│ concrete-ontology.ttl (INPUT)                                   │
│ ─────────────────────────────────────────────────────────────── │
│ :ParseRdfFunction a ffi:Function ;                              │
│     ffi:functionName "parseRdf" ;                               │
│     ffi:returnType "Triple[]" ;                                 │
│     ffi:async true ;                                            │
│     ffi:hasParameter :FilePathParam .                           │
└─────────────────────────┬───────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│ functions.sparql (QUERY)                                        │
│ ─────────────────────────────────────────────────────────────── │
│ SELECT ?funcName ?returnType ?async                            │
│ WHERE { ?func a ffi:Function ; ... }                           │
└─────────────────────────┬───────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│ Query Results (JSON/ARRAY)                                      │
│ ─────────────────────────────────────────────────────────────── │
│ { funcName: "parseRdf", returnType: "Triple[]", async: true }  │
└─────────────────────────┬───────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│ data-driven-template.mjs.tera (TRANSFORM)                       │
│ ─────────────────────────────────────────────────────────────── │
│ {% for func in functions %}                                     │
│ export const {{ func.funcName }} = ...                         │
│ {% endfor %}                                                    │
└─────────────────────────┬───────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────────────┐
│ desired-output.mjs (OUTPUT)                                     │
│ ─────────────────────────────────────────────────────────────── │
│ export const parseRdf = native.parseRdf;                        │
└─────────────────────────────────────────────────────────────────┘
```

## Directory Structure

```
templates/node-bindings/
├── examples/
│   ├── BACKWARD-THINKING.md          # Conceptual explanation
│   ├── desired-output.mjs            # What we want to generate
│   ├── query-results-example.json    # SPARQL query results
│   ├── data-driven-template.mjs.tera # Pure data-driven template
│   └── concrete-ontology.ttl         # Real ontology instance
├── queries/
│   ├── functions.sparql              # Extract functions
│   ├── types.sparql                  # Extract types
│   ├── structs.sparql                # Extract structs
│   └── enums.sparql                  # Extract enums
├── ontology/
│   └── node-ffi-ontology.ttl         # Schema (vocabulary)
└── templates/
    ├── index.mjs.tera                # Main ESM template
    ├── index.js.tera                 # CommonJS template
    ├── package.json.tera             # Package config
    └── ...                           # Other templates
```

## Key Principles

### 1. Ontology = Schema, Instance = Data

- **Schema** (`ontology/node-ffi-ontology.ttl`): Defines what a Function, Parameter, Type CAN be
- **Instance** (`examples/concrete-ontology.ttl`): Describes ACTUAL functions to generate

```turtle
# Schema (defines vocabulary)
ffi:Function a rdfs:Class .
ffi:functionName a rdf:Property ; rdfs:domain ffi:Function .

# Instance (uses vocabulary)
:ParseRdfFunction a ffi:Function ;
    ffi:functionName "parseRdf" .
```

### 2. SPARQL = Data Extraction

SPARQL queries are like SQL views - they extract structured data from the ontology:

```sparql
SELECT ?funcName WHERE { ?func a ffi:Function ; ffi:functionName ?funcName }
```

### 3. Tera = Presentation Layer

Templates receive query results as variables and transform them:

```tera
{%- for func in functions %}
export const {{ func.funcName }} = ...
{% endfor %}
```

### 4. No Hardcoding in Templates

❌ **Wrong**:
```tera
export const parseRdf = native.parseRdf;
export const generateCode = native.generateCode;
```

✅ **Right**:
```tera
{%- for func in functions %}
export const {{ func.funcName }} = native.{{ func.funcName }};
{% endfor %}
```

## Testing the Flow

1. **Create ontology**: `examples/concrete-ontology.ttl`
2. **Run SPARQL queries**: `queries/*.sparql`
3. **Get query results**: JSON/arrays
4. **Apply templates**: `examples/data-driven-template.mjs.tera`
5. **Verify output**: Should match `examples/desired-output.mjs`

## Benefits

| Aspect | Hardcoded | Data-Driven |
|--------|-----------|-------------|
| Flexibility | Fixed | Dynamic |
| Reusability | Low | High |
| Maintainability | Hard | Easy |
| Add new function | Change template | Add to ontology |
| Change structure | Rewrite template | Update SPARQL |

## Example: Adding a New Function

### Hardcoded Approach ❌
1. Edit template
2. Add hardcoded function
3. Regenerate
4. Repeat for every function

### Data-Driven Approach ✅
1. Add to ontology:
```turtle
:NewFunction a ffi:Function ;
    ffi:functionName "newFunc" ;
    ffi:returnType "string" .
```
2. Regenerate (template unchanged!)

---

**Remember**: The ontology is the single source of truth. Templates just transform it.
