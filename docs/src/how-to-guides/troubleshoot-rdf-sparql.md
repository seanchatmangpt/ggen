# Troubleshoot RDF & SPARQL Issues

Common RDF and SPARQL errors you might encounter and how to fix them. This guide covers validation errors, query issues, type mismatches, and ontology problems.

---

## Quick Diagnosis

**My ontology doesn't generate code:**
→ See [Ontology Validation](#ontology-validation)

**SPARQL query returns empty results:**
→ See [SPARQL Query Problems](#sparql-query-problems)

**Generated code has wrong types:**
→ See [Type Mapping Issues](#type-mapping-issues)

**Oxigraph parsing error:**
→ See [RDF Parsing Errors](#rdf-parsing-errors)

**Template won't render:**
→ See [Template Rendering Issues](#template-rendering-issues)

---

## RDF Parsing Errors

### Error: Invalid RDF Syntax

```
Error: Failed to parse ontology: invalid RDF syntax at line 42
```

**Causes:**
- Malformed Turtle syntax
- Invalid URI format
- Missing prefix declarations
- Unclosed literals or URIs

**Solutions:**

1. **Check Turtle syntax** against W3C specification:
   ```turtle
   # WRONG - missing colon after prefix
   @prefix ex http://example.com/

   # CORRECT
   @prefix ex: <http://example.com/>
   ```

2. **Validate URIs** are properly formatted:
   ```turtle
   # WRONG - space in URI
   @prefix ex: <http://example.com/ domain>

   # CORRECT
   @prefix ex: <http://example.com/domain/>
   ```

3. **Close all literals properly**:
   ```turtle
   # WRONG - unclosed string
   ex:name rdfs:label "User

   # CORRECT
   ex:name rdfs:label "User" .
   ```

4. **Validate with online tool**:
   - Use http://ttl.summerofcode.be/ to validate Turtle
   - Check for syntax errors interactively

**Prevention:**
```bash
# Validate ontology before using
ggen graph validate --ontology domain.ttl
```

---

### Error: Undefined Prefix

```
Error: Prefix 'ex' is not defined
```

**Causes:**
- Prefix used but not declared at top of file
- Typo in prefix name
- Prefix defined after usage

**Solutions:**

1. **Declare prefixes at top**:
   ```turtle
   # CORRECT - prefixes first
   @prefix ex: <http://example.com/> .
   @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

   ex:User a rdfs:Class .
   ```

2. **Check spelling**:
   ```turtle
   # WRONG
   examp:User a rdfs:Class .

   # CORRECT
   ex:User a rdfs:Class .
   ```

3. **Use common prefixes**:
   ```turtle
   @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
   @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
   @prefix owl: <http://www.w3.org/2002/07/owl#> .
   @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
   ```

---

### Error: Invalid URI Format

```
Error: Invalid URI: example.com/User (expected <...>)
```

**Causes:**
- URI not wrapped in angle brackets
- Invalid characters in URI
- Relative URIs without base

**Solutions:**

1. **Wrap URIs in angle brackets**:
   ```turtle
   # WRONG
   ex:User a rdfs:Class .
   ex:name rdfs:domain String .

   # CORRECT
   <http://example.com/User> a rdfs:Class .
   ex:name rdfs:domain xsd:string .
   ```

2. **Use prefixes for cleaner code**:
   ```turtle
   @prefix ex: <http://example.com/> .
   ex:User a rdfs:Class .          # expands to <http://example.com/User>
   ```

3. **Define base URI for relative references**:
   ```turtle
   @base <http://example.com/> .
   @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

   <User> a rdfs:Class .           # expands to <http://example.com/User>
   ```

---

## Ontology Validation

### Error: No Classes Found

```
Error: Template expects at least 1 class, found 0
```

**Causes:**
- No `rdfs:Class` definitions
- Classes defined with wrong predicate
- Empty ontology file

**Solutions:**

1. **Define classes explicitly**:
   ```turtle
   @prefix ex: <http://example.com/> .
   @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

   ex:User a rdfs:Class ;
       rdfs:label "User" .
   ```

2. **Use correct predicate**:
   ```turtle
   # WRONG - this just asserts a type, not class definition
   ex:myclass rdf:type example:CustomClass .

   # CORRECT - defines it as an RDF class
   ex:User a rdfs:Class .
   ```

3. **Check ontology is loaded**:
   ```bash
   # Verify file exists and has content
   wc -l domain.ttl    # Should be > 0

   # View first few triples
   head -20 domain.ttl
   ```

---

### Error: Classes Have No Properties

```
Warning: Class User has 0 properties, skipping
```

**Causes:**
- Properties exist but domain/range not set
- Property domain doesn't match class URI
- Properties use wrong predicates

**Solutions:**

1. **Define properties with explicit domain/range**:
   ```turtle
   # WRONG - property without domain/range
   ex:name a rdf:Property .

   # CORRECT - domain links property to class
   ex:name a rdf:Property ;
       rdfs:domain ex:User ;
       rdfs:range xsd:string ;
       rdfs:label "name" .
   ```

2. **Ensure domain exactly matches class URI**:
   ```turtle
   # WRONG - class uses ex:, property uses different prefix
   ex:User a rdfs:Class .
   domain:name rdfs:domain example:User .  # Different URI!

   # CORRECT - same namespace
   @prefix ex: <http://example.com/> .
   ex:User a rdfs:Class .
   ex:name rdfs:domain ex:User .
   ```

3. **Use explicit label**:
   ```turtle
   ex:User a rdfs:Class ;
       rdfs:label "User" .       # Label for human readability

   ex:name a rdf:Property ;
       rdfs:domain ex:User ;
       rdfs:range xsd:string ;
       rdfs:label "name" .       # Label used in generated code
   ```

---

### Error: Undefined Property Range

```
Error: Property 'name' has no rdfs:range, cannot infer type
```

**Causes:**
- Property defined but no range specified
- Range is class (needs type mapping)
- Range is undefined type

**Solutions:**

1. **Specify type with rdfs:range**:
   ```turtle
   # WRONG - no range
   ex:name a rdf:Property ;
       rdfs:domain ex:User .

   # CORRECT - XSD type specified
   ex:name a rdf:Property ;
       rdfs:domain ex:User ;
       rdfs:range xsd:string .
   ```

2. **Use standard XSD types**:
   ```turtle
   @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

   ex:email rdfs:range xsd:string .        # String
   ex:age rdfs:range xsd:integer .         # Integer
   ex:score rdfs:range xsd:decimal .       # Decimal
   ex:active rdfs:range xsd:boolean .      # Boolean
   ex:created rdfs:range xsd:dateTime .    # DateTime
   ex:data rdfs:range xsd:base64Binary .   # Binary
   ```

3. **For object properties (relationships)**:
   ```turtle
   ex:author a rdf:Property ;
       rdfs:domain ex:Post ;
       rdfs:range ex:User ;      # Links to another class
       rdfs:label "author" .
   ```

---

## SPARQL Query Problems

### Error: SPARQL Syntax Error

```
Error: SPARQL syntax error at line 5: unexpected token
```

**Causes:**
- Missing dot after triple patterns
- Incorrect variable syntax (should start with ?)
- Unmatched braces
- Invalid operators

**Solutions:**

1. **Check for missing dots and braces**:
   ```sparql
   # WRONG - missing dot after pattern
   SELECT ?class
   WHERE {
       ?class a rdfs:Class

   # CORRECT
   SELECT ?class
   WHERE {
       ?class a rdfs:Class .
   }
   ```

2. **Variables must start with ?**:
   ```sparql
   # WRONG - variable without ?
   SELECT class
   WHERE { class a rdfs:Class . }

   # CORRECT
   SELECT ?class
   WHERE { ?class a rdfs:Class . }
   ```

3. **Match all braces**:
   ```sparql
   # WRONG - unclosed WHERE
   SELECT ?class
   WHERE {
       ?class a rdfs:Class .
       ?class rdfs:label ?label .

   # CORRECT
   SELECT ?class ?label
   WHERE {
       ?class a rdfs:Class .
       ?class rdfs:label ?label .
   }
   ```

4. **Proper operator syntax**:
   ```sparql
   # WRONG
   WHERE { ?x = 5 }

   # CORRECT
   WHERE { ?x a rdfs:Class . FILTER (?x = <http://example.com/5>) }
   ```

---

### Query Returns Empty Results

```
Query executed but returned 0 rows
```

**Causes:**
- Ontology doesn't match query patterns
- Wrong namespace prefixes
- Case-sensitive mismatches
- OPTIONAL not used for optional patterns

**Solutions:**

1. **Start simple and debug**:
   ```sparql
   # First, verify any classes exist
   SELECT COUNT(*)
   WHERE {
       ?x a rdfs:Class .
   }
   ```

2. **Check exact URIs match**:
   ```sparql
   # If query returns nothing, examine actual URIs
   SELECT ?class
   WHERE {
       ?class a ?type .
   }
   LIMIT 10

   # Then use exact URIs in queries
   ```

3. **Use OPTIONAL for might-not-exist data**:
   ```sparql
   # WRONG - fails if property missing
   SELECT ?className ?propName
   WHERE {
       ?class a rdfs:Class ;
              rdfs:label ?className .
       ?prop rdfs:domain ?class ;
             rdfs:label ?propName .
   }

   # CORRECT - handles missing properties
   SELECT ?className ?propName
   WHERE {
       ?class a rdfs:Class ;
              rdfs:label ?className .
       OPTIONAL {
           ?prop rdfs:domain ?class ;
                 rdfs:label ?propName .
       }
   }
   ```

4. **Debug with FILTER**:
   ```sparql
   # See what you're actually matching
   SELECT ?class
   WHERE {
       ?class a ?type .
       FILTER (STR(?type) = "http://www.w3.org/2000/01/rdf-schema#Class")
   }
   ```

---

### Query Performance Issues

```
Query takes >5 seconds to complete
```

**Causes:**
- Inefficient triple patterns
- Large graph with complex traversal
- GROUP_CONCAT on huge datasets
- Multiple OPTIONAL clauses with matches

**Solutions:**

1. **Use specific patterns over generic**:
   ```sparql
   # SLOW - queries entire graph
   SELECT ?x WHERE { ?x ?p ?y . }

   # FAST - specific pattern
   SELECT ?class WHERE { ?class a rdfs:Class . }
   ```

2. **Filter early, aggregate late**:
   ```sparql
   # SLOW - aggregate then filter
   SELECT ?class (COUNT(*) as ?count)
   WHERE { ?prop rdfs:domain ?class . }
   GROUP BY ?class
   HAVING (COUNT(*) > 0)

   # FASTER - filter in WHERE
   SELECT ?class (COUNT(?prop) as ?count)
   WHERE {
       ?prop rdfs:domain ?class .
       FILTER (?prop != <http://dummy>)
   }
   GROUP BY ?class
   ```

3. **Limit results during development**:
   ```sparql
   SELECT ?class
   WHERE { ?class a rdfs:Class . }
   LIMIT 100  # Test with small set first
   ```

4. **Check query with EXPLAIN** (if supported):
   ```bash
   ggen graph query --explain --query query.rq --ontology domain.ttl
   ```

---

## Type Mapping Issues

### Error: Unknown Type in Generated Code

```rust
pub struct User {
    pub age: UnknownType,  // Unmapped type
}
```

**Causes:**
- RDF type not in type mapping table
- Custom type without mapping defined
- Typo in type name

**Solutions:**

1. **Check type mapping in template**:
   ```tera
   {% set type_map = {
       "xsd:string": "String",
       "xsd:integer": "i64",
       "xsd:decimal": "f64",
       # Add missing types here
   } %}

   {{ type_map.get(property.rdf_type, "String") }}
   ```

2. **Add missing type**:
   ```sparql
   # First, identify what type is used
   SELECT DISTINCT ?type
   WHERE { ?prop rdfs:range ?type . }
   ```

   ```tera
   # Then add to type map
   {% set type_map = {
       "xsd:string": "String",
       "xsd:integer": "i64",
       "xsd:date": "chrono::NaiveDate",    # Add custom mapping
       "custom:MoneyType": "Decimal",       # Add domain-specific
   } %}
   ```

3. **Provide fallback for unknown types**:
   ```tera
   {{ type_map.get(property.rdf_type, "String") }}  # Defaults to String
   ```

---

### Error: Type Incompatibility

```
Generated code doesn't compile due to type mismatch
```

**Causes:**
- XSD type mapped to incompatible Rust type
- Property allows multiple values but type is singular
- Optional vs required mismatch

**Solutions:**

1. **Use Option for optional properties**:
   ```tera
   {% set cardinality = get_cardinality(property) %}
   pub {{ property.name }}: {% if cardinality == "optional" %}Option<{% endif %}String{% if cardinality == "optional" %}>{% endif %},
   ```

2. **Use Vec for multiple values**:
   ```sparql
   # Query identifies multi-valued properties
   SELECT ?property (MAX(?maxCount) as ?max)
   WHERE {
       ?property sh:maxCount ?maxCount .
   }
   GROUP BY ?property
   ```

   ```tera
   {% if property.maxCount > 1 %}
   pub {{ property.name }}: Vec<String>,
   {% else %}
   pub {{ property.name }}: String,
   {% endif %}
   ```

3. **Map complex types correctly**:
   ```tera
   {% set type_map = {
       "xsd:string": "String",
       "xsd:integer": "i64",
       "xsd:dateTime": "chrono::DateTime<chrono::Utc>",
       "xsd:date": "chrono::NaiveDate",
       "xsd:base64Binary": "Vec<u8>",
   } %}
   ```

---

## Template Rendering Issues

### Error: SPARQL Query in Template Fails

```
Error rendering template: SPARQL query returned no results
```

**Causes:**
- Query doesn't match ontology structure
- Query path incorrect in template
- Missing SPARQL variables

**Solutions:**

1. **Test SPARQL independently**:
   ```bash
   # Test query before putting in template
   ggen graph query --query query.rq --ontology domain.ttl
   ```

2. **Check query path in template**:
   ```tera
   # WRONG - incorrect path
   {% query "extract-classes.rq" as classes %}

   # CORRECT - full path
   {% query "queries/extract-classes.rq" as classes %}
   ```

3. **Verify query returns data**:
   ```tera
   {% query "queries/extract-classes.rq" as classes %}

   {% if classes is empty %}
   // ERROR: Query returned no results
   // Check ontology structure
   {% else %}
   // Generate code from results
   {% endif %}
   ```

---

### Error: Template Variable Undefined

```
Error: Undefined variable 'property.type'
```

**Causes:**
- Query doesn't return expected columns
- Variable name mismatch
- SPARQL aliases not matching template usage

**Solutions:**

1. **Name SPARQL variables explicitly**:
   ```sparql
   # WRONG - vague naming
   SELECT ?p ?r
   WHERE { ?class rdfs:label ?p . ?prop rdfs:range ?r . }

   # CORRECT - clear naming
   SELECT ?className ?propertyName ?propertyType
   WHERE { ?class rdfs:label ?className .
           ?prop rdfs:label ?propertyName .
           ?prop rdfs:range ?propertyType . }
   ```

2. **Match template variable names**:
   ```tera
   # Template expects propertyType
   {% for prop in properties %}
   pub {{ prop.propertyName }}: {{ map_type(prop.propertyType) }},
   {% endfor %}

   # SPARQL must provide it
   SELECT ?propertyName ?propertyType
   ```

3. **Debug with safe defaults**:
   ```tera
   {% for prop in properties %}
   pub {{ prop.propertyName | default: "unknown" }}: {{ prop.propertyType | default: "String" }},
   {% endfor %}
   ```

---

## Common Patterns & Solutions

### Checking ontology structure:

```bash
# List all classes
ggen graph query --ontology domain.ttl \
  --query - << 'EOF'
SELECT ?class ?label
WHERE {
    ?class a rdfs:Class .
    OPTIONAL { ?class rdfs:label ?label . }
}
ORDER BY ?label
EOF

# List all properties
ggen graph query --ontology domain.ttl \
  --query - << 'EOF'
SELECT ?prop ?domain ?range
WHERE {
    ?prop a rdf:Property .
    OPTIONAL { ?prop rdfs:domain ?domain . }
    OPTIONAL { ?prop rdfs:range ?range . }
}
EOF
```

### Debugging templates:

```bash
# Generate with verbose output
ggen template generate-rdf \
  --ontology domain.ttl \
  --template . \
  --output /tmp/debug \
  --verbose

# Check generated file
cat /tmp/debug/output.rs
```

### Validating ontologies:

```bash
# Use ggen's validation
ggen graph validate --ontology domain.ttl --verbose

# Use external validator
curl -X POST http://ttl.summerofcode.be/validate \
  --data-binary @domain.ttl \
  -H "Content-Type: text/turtle"
```

---

## When to Ask for Help

If you've tried the above and still have issues:

1. **Collect diagnostics**:
   ```bash
   ggen utils doctor > diagnostics.txt
   ```

2. **Include in issue report**:
   - Error message (exact text)
   - Minimal ontology that reproduces issue
   - SPARQL query if applicable
   - Template if applicable
   - `ggen --version`

3. **Minimal reproducible example**:
   ```turtle
   # Smallest ontology that causes the problem
   @prefix ex: <http://example.com/> .
   @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

   ex:User a rdfs:Class ;
       rdfs:label "User" .
   ```

---

## Tools for Debugging

### Online RDF Validators
- http://ttl.summerofcode.be/ - Turtle validator
- https://www.w3.org/RDF-Turtle/ - W3C reference

### SPARQL Testers
- https://sparql.all.org/visualizer - Visual SPARQL debugger
- https://query.wikidata.org/ - Real-world SPARQL examples

### RDF Viewers
- http://rdf.visualdatasets.com/ - RDF graph visualization
- https://www.w3.org/2005/ajar/rdf-icons.html - RDF icon reference

### ggen Tools
```bash
ggen graph validate           # Validate ontology
ggen graph query              # Run SPARQL queries
ggen template test            # Test template rendering
ggen utils doctor             # System diagnostics
```

---

## Additional Resources

- **RDF Tutorial:** https://www.w3.org/2004/03/trix/trix.html
- **SPARQL Spec:** https://www.w3.org/TR/sparql11-query/
- **Turtle Format:** https://www.w3.org/TR/turtle/
- **SHACL Shapes:** https://www.w3.org/TR/shacl/
- **Oxigraph Docs:** https://github.com/oxigraph/oxigraph

For more patterns, see [SPARQL Cookbook](../reference/sparql-cookbook.md).
