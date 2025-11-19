# Reference: Template Context Variables

## Global Context

These variables are available in all templates:

```
{{ ontology.name }}                # Pack name
{{ ontology.version }}             # Pack version
{{ ontology.namespace }}           # Base namespace URI
{{ ontology.description }}         # Pack description
{{ ontology.author }}              # Pack author
{{ generated_at }}                 # ISO 8601 timestamp
{{ target_language }}              # typescript, rust, python, go
{{ features }}                     # Array of enabled features
```

## Class Variables

Inside `{{ for class in classes }}...{{ endfor }}`:

```
{{ class.name }}                   # Class name (e.g., "Product")
{{ class.namespace }}              # Full namespace URI
{{ class.description }}            # Documentation string
{{ class.parent_class }}           # Parent class name
{{ class.is_abstract }}            # Boolean
{{ class.properties }}             # Array of properties
{{ class.required_properties }}    # Array of required props
{{ class.relationships }}          # Array of relationships
```

## Property Variables

Inside `{{ for prop in properties }}...{{ endfor }}`:

```
{{ prop.name }}                    # Property name
{{ prop.type }}                    # Type name (e.g., "string", "Product")
{{ prop.description }}             # Documentation
{{ prop.is_required }}             # Boolean
{{ prop.cardinality }}             # one, many, or optional
{{ prop.default_value }}           # Default value or null
{{ prop.constraints }}             # Array of validation constraints
{{ prop.example }}                 # Example value
{{ prop.aliases }}                 # Array of alternative names
```

## Constraint Variables

```
{{ constraint.type }}              # min, max, length, pattern, enum
{{ constraint.value }}             # Constraint value
{{ constraint.message }}           # Error message
```

## Filters

### Naming Filters

```
{{ class.name | pascalcase }}      # ProductListing
{{ class.name | camelcase }}       # productListing
{{ class.name | snakecase }}       # product_listing
{{ class.name | kebabcase }}       # product-listing
{{ class.name | uppercase }}       # PRODUCT_LISTING
{{ class.name | lowercase }}       # product_listing
```

### Type Filters

```
{{ prop.type | to_typescript }}   # Converts to TS type
{{ prop.type | to_rust }}         # Converts to Rust type
{{ prop.type | to_python }}       # Converts to Python type
{{ prop.type | nullable }}        # Adds null to type
{{ prop.type | optional }}        # Adds Optional/? modifier
```

### String Filters

```
{{ prop.description | safe }}      # Escapes HTML/special chars
{{ prop.description | indent(2) }} # Indents by 2 spaces
{{ prop.description | truncate(80) }} # Truncates at 80 chars
```

## Conditional Helpers

```
{% if prop.is_required %}required{% endif %}
{% if prop.cardinality == "many" %}array{% endif %}
{% if class.is_abstract %}abstract class{% endif %}
{% if feature_enabled("zod") %}generate validators{% endif %}
```

## Example: TypeScript Type Template

```typescript
{{ for class in classes }}
/**
 * {{ class.description }}
 */
export interface {{ class.name }} {
  {{ for prop in class.properties }}
  /**
   * {{ prop.description }}
   {{ if prop.constraints }}
   * Constraints: {{ prop.constraints | join(", ") }}
   {{ endif }}
   */
  {{ prop.name | camelcase }}{{ if not prop.is_required }}?{{ endif }}: {{ prop.type | to_typescript }};
  {{ endfor }}
}
{{ endfor }}
```

## Example: Rust Struct Template

```rust
{{ for class in classes }}
/// {{ class.description }}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ class.name | pascalcase }} {
  {{ for prop in class.properties }}
  /// {{ prop.description }}
  pub {{ prop.name | snakecase }}: {{ prop.type | to_rust }},
  {{ endfor }}
}
{{ endfor }}
```
