# How to Create Templates

Guide to creating custom templates for code generation.

## Template Basics

Templates use Tera syntax with SPARQL queries to extract data from RDF ontologies.

### Template Structure

```
templates/
└── rust-models/
    ├── models.rs.tmpl      # Template file
    └── queries/
        └── extract-classes.rq  # SPARQL query
```

### Basic Template

Create `templates/rust-models/models.rs.tmpl`:

```tera
{% query "queries/extract-classes.rq" as classes %}
{% for class in classes %}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ class.name }} {
    {% for prop in class.properties %}
    pub {{ prop.name }}: {{ prop.rust_type }},
    {% endfor %}
}
{% endfor %}
```

### SPARQL Query

Create `templates/rust-models/queries/extract-classes.rq`:

```sparql
SELECT ?class ?property ?type
WHERE {
    ?class a rdfs:Class .
    ?property rdfs:domain ?class .
    ?property rdfs:range ?type .
}
ORDER BY ?class ?property
```

## Template Syntax

### Variables

```tera
{{ variable_name }}
```

### Loops

```tera
{% for item in items %}
  {{ item.name }}
{% endfor %}
```

### Conditionals

```tera
{% if condition %}
  {{ content }}
{% endif %}
```

### SPARQL Queries

```tera
{% query "path/to/query.rq" as results %}
{% for row in results %}
  {{ row.field }}
{% endfor %}
```

## Template Metadata

Add frontmatter to templates:

```yaml
---
to: src/models.rs
vars:
  - class_name
  - output_dir
rdf:
  - domain.ttl
sparql:
  - queries/extract-classes.rq
---
```

## Type Mapping

Map RDF types to target language types:

```tera
{% set type_map = {
    "xsd:string": "String",
    "xsd:integer": "i32",
    "xsd:decimal": "f64",
    "xsd:boolean": "bool",
    "xsd:dateTime": "DateTime<Utc>"
} %}

{{ type_map.get(property.type, "String") }}
```

## Advanced Patterns

### Nested Structures

```tera
{% for class in classes %}
pub struct {{ class.name }} {
    {% for prop in class.properties %}
    {% if prop.is_object %}
    pub {{ prop.name }}: {{ prop.target_class }},
    {% else %}
    pub {{ prop.name }}: {{ prop.type }},
    {% endif %}
    {% endfor %}
}
{% endfor %}
```

### Validation Code

```tera
impl {{ class.name }} {
    pub fn validate(&self) -> Result<(), ValidationError> {
        {% for prop in class.properties %}
        {% if prop.required %}
        if self.{{ prop.name }}.is_empty() {
            return Err(ValidationError::MissingField("{{ prop.name }}"));
        }
        {% endif %}
        {% endfor %}
        Ok(())
    }
}
```

## Testing Templates

Test templates with sample ontologies:

```bash
# Generate from test ontology
ggen template generate-rdf \
  --ontology test.ttl \
  --template rust-models \
  --output test-output/

# Verify output
cat test-output/models.rs
```

## Best Practices

1. **Use SPARQL for extraction:** Don't hardcode structure
2. **Make templates reusable:** Parameterize with variables
3. **Validate inputs:** Check ontology structure before generation
4. **Document templates:** Add comments explaining SPARQL queries
5. **Test thoroughly:** Use multiple ontologies to verify

## Publishing Templates

Publish to marketplace:

```bash
ggen marketplace publish \
  --name "my-rust-template" \
  --version "1.0.0" \
  --template-dir ./templates
```

## Next Steps

- **Template reference:** [Template Syntax Reference](../reference/templates.md)
- **SPARQL guide:** [RDF/SPARQL Reference](../reference/rdf-sparql.md)
- **Examples:** See `examples/` directory for template examples

