# How to Create Templates

Complete guide to creating production-quality templates. Templates are the heart of ggen—they transform RDF ontologies into code in any language. This guide covers basic templates through advanced patterns like frozen sections, incremental generation, type mapping, and marketplace distribution.

## Part 1: Template Fundamentals

### What is a Template?

A template is a blueprint that transforms RDF data into code. It combines:
1. **Tera syntax** - Template language (variables, loops, conditionals)
2. **SPARQL queries** - Extract structured data from RDF ontologies
3. **Frontmatter metadata** - Configuration and options
4. **Frozen sections** - Preserve manual code during regeneration

### Template Structure

```
my-template/
├── models.rs.tmpl              # Template file (Tera syntax)
├── ggen.toml                   # Template manifest
├── frontmatter.yaml            # Frontmatter defaults
├── queries/
│   ├── extract-classes.rq      # SPARQL: extract all classes
│   ├── extract-properties.rq   # SPARQL: get properties
│   └── extract-relationships.rq # SPARQL: get relationships
├── tests/
│   ├── fixtures/
│   │   └── sample-ontology.ttl # Test ontology
│   └── expected-output.rs      # Expected generated code
└── README.md                   # Documentation
```

### Creating Your First Template

**Step 1:** Create template directory

```bash
mkdir my-template
cd my-template
```

**Step 2:** Create `ggen.toml` manifest

```toml
[template]
name = "rust-models"
version = "1.0.0"
authors = ["Your Name"]
description = "Generate Rust model structs from RDF ontologies"
license = "MIT"

[template.files]
"models.rs.tmpl" = "src/models.rs"

[template.metadata]
language = "rust"
framework = "serde"
minimum_ontology_size = 1

[template.frontmatter-defaults]
derive = ["Debug", "Clone", "Serialize", "Deserialize"]
visibility = "pub"
```

**Step 3:** Create SPARQL query

```sparql
# queries/extract-classes.rq
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?className (GROUP_CONCAT(?propName; separator=",") as ?properties)
WHERE {
    ?class a rdfs:Class ;
            rdfs:label ?className .
    OPTIONAL {
        ?property rdfs:domain ?class ;
                   rdfs:label ?propName .
    }
}
GROUP BY ?className
ORDER BY ?className
```

**Step 4:** Create template file

```tera
# models.rs.tmpl
{% query "queries/extract-classes.rq" as classes %}
use serde::{Deserialize, Serialize};

{% for class in classes %}
/// Auto-generated from RDF ontology
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ class.name }} {
    {% for prop in class.properties %}
    pub {{ prop.name }}: String,
    {% endfor %}
}
{% endfor %}
```

**Step 5:** Test your template

```bash
# Create test ontology
cat > test-ontology.ttl << 'EOF'
@prefix ex: <http://example.com/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:User a rdfs:Class ;
    rdfs:label "User" .

ex:name a rdf:Property ;
    rdfs:domain ex:User ;
    rdfs:label "name" .
EOF

# Generate
ggen template generate-rdf \
  --ontology test-ontology.ttl \
  --template . \
  --output generated/

# Verify
cat generated/src/models.rs
```

---

## Part 2: Advanced Template Features

### Frontmatter Specification

Frontmatter is YAML metadata at the top of template files. It controls how generation works and allows runtime configuration.

**Full Frontmatter Specification:**

```yaml
---
# OUTPUT CONFIGURATION
to: "src/models.rs"                    # Output file path (required)
skip_if_exists: false                  # Don't overwrite existing file
mode: "append" | "replace" | "merge"   # How to handle file updates

# VARIABLE DECLARATION
vars:
  visibility:
    type: "string"
    default: "pub"
    description: "Struct visibility (pub or pub(crate))"

  derive:
    type: "array"
    default: ["Debug", "Clone"]
    description: "Derive attributes"

  include_validation:
    type: "boolean"
    default: false
    description: "Generate validation methods"

# RDF ONTOLOGY CONFIGURATION
rdf:
  - domain.ttl                         # Ontology files to load
  - namespaces.ttl

# SPARQL CONFIGURATION
sparql:
  queries:
    - queries/extract-classes.rq       # SPARQL queries to preload
    - queries/extract-properties.rq
  prefixes:
    ex: "http://example.com/"          # Custom namespace prefixes
    schema: "http://schema.org/"

# FROZEN SECTIONS
frozen:
  - "/// FROZEN: "                     # Mark manual code regions
  - "// MANUAL CODE BELOW"

# VALIDATION
validation:
  required_prefixes:                   # Required in ontology
    - "rdfs"
    - "rdf"
  min_classes: 1                       # At least 1 class
  max_classes: 1000                    # At most 1000 classes

# INCLUDES & MACROS
includes:
  - "common/type-mapping.jinja2"       # Include other template files

# DEPENDENCIES
dependencies:
  serde: "1.0"
  tokio: { version = "1.0", optional: true }

# METADATA
metadata:
  author: "Your Name"
  license: "MIT"
  tags: ["models", "rust", "serde"]
---
```

**Example Frontmatter:**

```yaml
---
to: "src/models.rs"
vars:
  derive:
    type: "array"
    default: ["Debug", "Clone", "Serialize", "Deserialize"]
    description: "Derive macro attributes"

rdf:
  - domain.ttl

sparql:
  queries:
    - queries/extract-classes.rq
    - queries/extract-properties.rq
  prefixes:
    ex: "http://example.com/"

frozen:
  - "// MANUAL: "

validation:
  required_prefixes: ["rdfs", "rdf"]
  min_classes: 1
---
```

### Frozen Sections (Incremental Generation)

Frozen sections preserve manual code during regeneration. They're critical for iterative development where humans add code after generation.

**Marking Frozen Sections:**

```rust
// models.rs
// FROZEN: Manual implementation below
// DO NOT REGENERATE THIS SECTION
impl User {
    pub fn new(name: String) -> Self {
        Self {
            id: uuid::Uuid::new_v4(),
            name,
        }
    }

    pub fn is_admin(&self) -> bool {
        // Custom business logic added manually
        self.role == "admin"
    }
}
// FROZEN END
```

**Regeneration Strategy:**

When regenerating:
1. Parse frozen markers (`// FROZEN:` ... `// FROZEN END`)
2. Extract content between markers
3. Regenerate the entire file
4. Re-insert frozen sections in their original locations
5. Merge conflicts if structure changed

**Template Example with Frozen Support:**

```tera
{% query "queries/extract-classes.rq" as classes %}
use serde::{Deserialize, Serialize};
use uuid::Uuid;

{% for class in classes %}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ class.name }} {
    {% for prop in class.properties %}
    pub {{ prop.name }}: {{ prop.rust_type }},
    {% endfor %}
}

impl {{ class.name }} {
    pub fn new({% for prop in class.required_properties %}{{ prop.name }}: {{ prop.rust_type }}{% if not loop.last %}, {% endif %}{% endfor %}) -> Self {
        Self {
            {% for prop in class.properties %}
            {{ prop.name }}: {% if prop.has_default %}{{ prop.default_value }}{% else %}{{ prop.name }}{% endif %},
            {% endfor %}
        }
    }
}

// FROZEN: Custom implementations - regeneration preserves this section
// Add your custom methods here
// FROZEN END
{% endfor %}
```

**Best Practices for Frozen Sections:**

1. **Clear markers** - Use consistent, grep-able markers (`// FROZEN:`)
2. **Document intent** - Explain why code is frozen
3. **Version both** - Track generated AND manual code in git
4. **Test regeneration** - Validate frozen sections survive regeneration
5. **Merge strategies** - Define conflict resolution before issues arise

---

### Type Mapping Reference

Map RDF types to target language types. This is critical for generating type-safe code.

**XSD Type to Rust Mapping:**

```tera
{% set xsd_to_rust = {
    "xsd:string": "String",
    "xsd:integer": "i64",
    "xsd:int": "i32",
    "xsd:short": "i16",
    "xsd:byte": "i8",
    "xsd:unsignedInteger": "u64",
    "xsd:unsignedInt": "u32",
    "xsd:decimal": "f64",
    "xsd:float": "f32",
    "xsd:boolean": "bool",
    "xsd:dateTime": "chrono::DateTime<chrono::Utc>",
    "xsd:date": "chrono::NaiveDate",
    "xsd:time": "chrono::NaiveTime",
    "xsd:anyURI": "String",
    "xsd:base64Binary": "Vec<u8>",
    "rdfs:Literal": "String",
} %}

{% macro map_type(rdf_type) %}
    {{ xsd_to_rust.get(rdf_type, "String") }}
{% endmacro %}

pub struct {{ class.name }} {
    {% for prop in class.properties %}
    pub {{ prop.name }}: {{ map_type(prop.rdf_type) }},
    {% endfor %}
}
```

**XSD Type to TypeScript Mapping:**

```tera
{% set xsd_to_ts = {
    "xsd:string": "string",
    "xsd:integer": "bigint",
    "xsd:int": "number",
    "xsd:decimal": "number",
    "xsd:float": "number",
    "xsd:boolean": "boolean",
    "xsd:dateTime": "Date",
    "xsd:date": "Date",
    "xsd:anyURI": "string",
    "xsd:base64Binary": "Uint8Array",
} %}

export interface {{ class.name }} {
    {% for prop in class.properties %}
    {{ prop.name }}: {{ xsd_to_ts.get(prop.rdf_type, "any") }};
    {% endfor %}
}
```

**Custom Type Mappings:**

```tera
{% set custom_types = {
    "ex:UserId": "UserId",
    "ex:Email": "EmailAddress",
    "ex:Money": "Decimal",
} %}

{% set type_map = custom_types | merge(xsd_to_rust) %}

pub struct {{ class.name }} {
    {% for prop in class.properties %}
    pub {{ prop.name }}: {{ type_map.get(prop.type, "String") }},
    {% endfor %}
}
```

---

### Advanced SPARQL Patterns

These patterns extract complex data from RDF ontologies. Use them as building blocks.

**Pattern 1: Extract All Classes with Properties**

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

SELECT ?className (GROUP_CONCAT(?propName; separator="|") as ?properties)
       (GROUP_CONCAT(?propType; separator="|") as ?types)
WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?className .

    OPTIONAL {
        ?prop rdfs:domain ?class ;
              rdfs:label ?propName ;
              rdfs:range ?propType .
    }
}
GROUP BY ?className
ORDER BY ?className
```

**Pattern 2: Class Hierarchy (Inheritance)**

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?subClass ?superClass
WHERE {
    ?subClass rdfs:subClassOf ?superClass .
    FILTER (?superClass != owl:Thing)
}
ORDER BY ?subClass
```

**Pattern 3: Find Required vs Optional Properties**

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX sh: <http://www.w3.org/ns/shacl#>

SELECT ?className ?propName ?required
WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?className .

    ?prop rdfs:domain ?class ;
          rdfs:label ?propName .

    # Check if property is required via SHACL
    OPTIONAL {
        ?shape sh:targetClass ?class ;
               sh:property [ sh:path ?prop ; sh:minCount 1 ] .
        BIND(true as ?required)
    }

    BIND(COALESCE(?required, false) as ?isRequired)
}
ORDER BY ?className ?propName
```

**Pattern 4: Relationships Between Classes**

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?sourceClass ?targetClass ?relationship
WHERE {
    ?source a rdfs:Class ;
            rdfs:label ?sourceClass .

    ?target a rdfs:Class ;
            rdfs:label ?targetClass .

    ?prop rdfs:domain ?source ;
          rdfs:range ?target ;
          rdfs:label ?relationship .

    FILTER (?source != ?target)
}
ORDER BY ?sourceClass ?relationship
```

**Pattern 5: Aggregation with Validation**

```sparql
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?className (COUNT(?prop) as ?propCount) (GROUP_CONCAT(?propName; separator=",") as ?propList)
WHERE {
    ?class a rdfs:Class ;
           rdfs:label ?className .

    ?prop rdfs:domain ?class ;
          rdfs:label ?propName .
}
GROUP BY ?className
HAVING (COUNT(?prop) > 0)
ORDER BY DESC(?propCount)
```

---

## Part 3: Testing & Quality

### Unit Testing Templates

Create test suites for templates:

```rust
// tests/templates/rust_models_test.rs
#[cfg(test)]
mod rust_models_template_tests {
    use ggen::template::TemplateRenderer;
    use std::fs;

    #[test]
    fn test_basic_class_generation() {
        let ontology = r#"
            @prefix ex: <http://example.com/> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

            ex:User a rdfs:Class ;
                rdfs:label "User" .

            ex:name a rdf:Property ;
                rdfs:domain ex:User ;
                rdfs:label "name" .
        "#;

        let result = TemplateRenderer::new("templates/rust-models")
            .render(ontology)
            .expect("template rendering should succeed");

        assert!(result.contains("pub struct User"));
        assert!(result.contains("pub name: String"));
    }

    #[test]
    fn test_multiple_properties() {
        let ontology = r#"
            @prefix ex: <http://example.com/> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

            ex:User a rdfs:Class ;
                rdfs:label "User" .

            ex:name rdfs:domain ex:User ; rdfs:label "name" .
            ex:email rdfs:domain ex:User ; rdfs:label "email" .
            ex:age rdfs:domain ex:User ; rdfs:label "age" .
        "#;

        let result = TemplateRenderer::new("templates/rust-models")
            .render(ontology)
            .expect("should render");

        assert!(result.contains("pub name:"));
        assert!(result.contains("pub email:"));
        assert!(result.contains("pub age:"));
    }

    #[test]
    fn test_regeneration_preserves_frozen_sections() {
        let original = r#"
pub struct User {
    pub id: String,
}

// FROZEN: Custom implementation
impl User {
    pub fn new() -> Self { Self { id: "1".to_string() } }
}
// FROZEN END
"#;

        let regenerated = regenerate_template_preserving_frozen(original);

        assert!(regenerated.contains("// FROZEN: Custom implementation"));
        assert!(regenerated.contains("impl User {"));
    }

    #[test]
    fn test_sparql_query_works() {
        let ontology = fs::read_to_string("tests/fixtures/sample-ontology.ttl")
            .expect("fixture should exist");

        let renderer = TemplateRenderer::new("templates/rust-models");
        let result = renderer.render(&ontology).expect("should work");

        // Verify generated code compiles
        assert!(is_valid_rust_code(&result));
    }
}
```

### Golden File Testing

Use "golden files" to ensure consistent output:

```bash
# Regenerate golden files when intentionally changing templates
ggen template test \
  --template ./my-template \
  --ontology tests/fixtures/sample.ttl \
  --update-golden

# Verify output matches golden file
ggen template test \
  --template ./my-template \
  --ontology tests/fixtures/sample.ttl \
  --golden tests/expected-output.rs
```

### Error Handling in Templates

Handle missing or invalid data gracefully:

```tera
{% query "queries/extract-classes.rq" as classes %}

{% if classes is empty %}
// ERROR: No classes found in ontology
// Template requires at least one rdfs:Class
// Please check your ontology file
{% else %}

{% for class in classes %}
pub struct {{ class.name }} {
    {% if class.properties is empty %}
    // WARNING: Class {{ class.name }} has no properties
    pub id: String,
    {% else %}
    {% for prop in class.properties %}
    pub {{ prop.name }}: {{ map_type(prop.rdf_type) }},
    {% endfor %}
    {% endif %}
}
{% endfor %}

{% endif %}
```

---

## Part 4: Publishing & Distribution

### Creating Marketplace Packages

Package templates for distribution:

```bash
# Create package directory
mkdir -p ggen-packages/rust-models-template
cd ggen-packages/rust-models-template

# Add template files
cp -r ../../my-template/* .

# Create package manifest
cat > ggen.toml << 'EOF'
[package]
name = "rust-models-template"
version = "1.0.0"
authors = ["Your Name"]
description = "Generate Rust model structs from RDF ontologies"
license = "MIT"

[package.dependencies]
serde = "1.0"

[[package.templates]]
name = "rust-models"
path = "models.rs.tmpl"
language = "rust"
framework = "serde"
EOF

# Create README
cat > README.md << 'EOF'
# Rust Models Template

Generates Rust struct definitions from RDF ontologies with serde support.

## Usage

```bash
ggen template generate-rdf \
  --ontology domain.ttl \
  --template rust-models \
  --output src/
```

## Requirements
- Rust 1.70+
- Ontology with `rdfs:Class` and `rdf:Property` definitions
EOF

# Publish to marketplace
ggen marketplace publish \
  --path . \
  --visibility public
```

### Versioning Templates

Follow semantic versioning:

```
MAJOR.MINOR.PATCH

MAJOR: Breaking changes (output format changes)
MINOR: New features (new SPARQL patterns, new variables)
PATCH: Bug fixes (typos, query corrections)
```

**Version Compatibility:**

```toml
[package]
name = "rust-models-template"
version = "2.0.0"

[package.compatibility]
# This template works with ggen versions:
ggen = ">= 2.6.0, < 3.0.0"
# Supports Rust versions:
rust = ">= 1.70.0"
```

---

## Part 5: Best Practices

### Do's ✓

1. **Use SPARQL for extraction** - Don't hardcode structure
2. **Parameterize everything** - Use frontmatter variables
3. **Document SPARQL queries** - Comment complex queries
4. **Test with multiple ontologies** - Ensure robustness
5. **Version your templates** - Track changes explicitly
6. **Preserve manual code** - Use frozen sections
7. **Validate inputs** - Check ontology before generation
8. **Type-map correctly** - Match RDF types to language types

### Don'ts ✗

1. **Don't hardcode class names** - Extract from RDF
2. **Don't assume ontology structure** - Handle variations
3. **Don't lose manual code** - Regeneration will overwrite
4. **Don't ignore type mapping** - Type-unsafe code fails
5. **Don't skip testing** - Test with real ontologies
6. **Don't publish without docs** - Templates need explanation
7. **Don't use complex queries** - Keep SPARQL readable
8. **Don't forget error messages** - Help users debug

---

## Quick Reference

| Task | Command |
|------|---------|
| Create template | `mkdir my-template && cd my-template` |
| Test template | `ggen template test --template . --ontology test.ttl` |
| Generate code | `ggen template generate-rdf --template . --ontology domain.ttl --output src/` |
| Publish template | `ggen marketplace publish --path .` |
| List templates | `ggen marketplace list --filter=template` |
| Install template | `ggen marketplace install my-template --version 1.0.0` |

---

## Next Steps

- **Test your template:** See [Testing Guide](testing-strategy.md)
- **Advanced SPARQL:** See [SPARQL Reference](../reference/rdf-sparql.md)
- **Publish to marketplace:** See [Marketplace Publishing](marketplace-packages.md)
- **View examples:** Check `examples/*/templates/` for real templates

