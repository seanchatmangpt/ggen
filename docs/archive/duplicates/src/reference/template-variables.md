<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Template Variables Reference](#template-variables-reference)
  - [Global Variables](#global-variables)
    - [Ontology Information](#ontology-information)
    - [Configuration Variables](#configuration-variables)
  - [Class Variables](#class-variables)
    - [Available in Class Loops](#available-in-class-loops)
    - [Type Information](#type-information)
    - [Examples](#examples)
  - [Property Variables](#property-variables)
    - [Available in Property Loops](#available-in-property-loops)
    - [Type Mapping Variables](#type-mapping-variables)
    - [Examples](#examples-1)
  - [Relationship Variables](#relationship-variables)
    - [Available in Relationship Loops](#available-in-relationship-loops)
  - [Type Mapping Reference](#type-mapping-reference)
    - [TypeScript Type Mapping](#typescript-type-mapping)
    - [Python Type Mapping](#python-type-mapping)
    - [Rust Type Mapping](#rust-type-mapping)
  - [Built-in Filters](#built-in-filters)
    - [String Filters](#string-filters)
    - [Collection Filters](#collection-filters)
    - [Logic Filters](#logic-filters)
  - [Looping Variables](#looping-variables)
    - [forloop Object](#forloop-object)
  - [Conditional Variables](#conditional-variables)
    - [Existence Checks](#existence-checks)
    - [Type Checks](#type-checks)
  - [Enumeration Variables](#enumeration-variables)
    - [For Enumeration Classes](#for-enumeration-classes)
  - [Examples](#examples-2)
    - [Complete Class Template](#complete-class-template)
    - [Interface Template](#interface-template)
    - [Enum Template](#enum-template)
  - [Advanced: Custom Variables](#advanced-custom-variables)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Template Variables Reference

Complete list of variables available in ggen code generation templates.

## Global Variables

### Ontology Information

```liquid
{{ ontology_name }}          # Name of the ontology
{{ ontology_version }}       # Version number
{{ ontology_namespace }}     # Base namespace URI
{{ generation_timestamp }}   # When code was generated
{{ ggen_version }}          # ggen version used
```

**Example usage**:
```liquid
/**
 * Generated from {{ ontology_name }} v{{ ontology_version }}
 * Generated at {{ generation_timestamp }}
 * By ggen {{ ggen_version }}
 */
```

### Configuration Variables

```liquid
{{ config.language }}        # Target language (typescript, python, rust)
{{ config.namespace }}       # Generated code namespace
{{ config.include_docs }}    # Include documentation (boolean)
{{ config.include_validation }} # Include validation (boolean)
{{ config.style }}          # Code style (camelCase, snake_case, etc)
```

## Class Variables

### Available in Class Loops

```liquid
{% for class in classes %}
  {{ class.uri }}           # RDF URI
  {{ class.name }}          # Class name
  {{ class.label }}         # Display label
  {{ class.comment }}       # Documentation comment
  {{ class.properties }}    # List of properties (see below)
  {{ class.parent_classes }} # List of parent classes
  {{ class.child_classes }}  # List of child classes
  {{ class.cardinality }}   # Cardinality constraint
{% endfor %}
```

### Type Information

```liquid
{{ class.is_abstract }}     # True if abstract class
{{ class.is_enum }}         # True if enumeration
{{ class.is_interface }}    # True if interface-like
{{ class.implements }}      # List of interfaces implemented
```

### Examples

**Simple template**:
```liquid
{% for class in classes %}
export interface {{ class.name }} {
  // Properties defined below
}
{% endfor %}
```

**With conditional**:
```liquid
{% for class in classes %}
  {% if class.is_abstract %}
  /**
   * Abstract class: {{ class.comment }}
   */
  export abstract class {{ class.name }} {
  {% else %}
  export class {{ class.name }} {
  {% endif %}
    // Implementation
  }
{% endfor %}
```

## Property Variables

### Available in Property Loops

```liquid
{% for property in class.properties %}
  {{ property.uri }}              # RDF URI
  {{ property.name }}             # Property name
  {{ property.label }}            # Display label
  {{ property.comment }}          # Documentation
  {{ property.type }}             # Type name
  {{ property.range }}            # RDF range
  {{ property.cardinality }}      # min..max
  {{ property.is_required }}      # boolean
  {{ property.is_array }}         # Is collection
  {{ property.default_value }}    # Default value if any
{% endfor %}
```

### Type Mapping Variables

```liquid
{{ property.mapped_type }}      # Type for target language
{{ property.typescript_type }}  # TypeScript type
{{ property.python_type }}      # Python type
{{ property.rust_type }}        # Rust type
```

### Examples

**Property iteration**:
```liquid
{% for property in class.properties %}
/**
 * {{ property.label }}
 * {{ property.comment }}
 */
{{ property.name }}: {{ property.mapped_type }};
{% endfor %}
```

**With validation**:
```liquid
{% for property in class.properties %}
  {% if property.is_required %}
  [required] {{ property.name }}: {{ property.mapped_type }};
  {% else %}
  [optional] {{ property.name }}?: {{ property.mapped_type }};
  {% endif %}
{% endfor %}
```

## Relationship Variables

### Available in Relationship Loops

```liquid
{% for relationship in class.relationships %}
  {{ relationship.name }}         # Relationship name
  {{ relationship.source }}       # Source class
  {{ relationship.target }}       # Target class
  {{ relationship.type }}         # Relationship type
  {{ relationship.cardinality }}  # 1:1, 1:N, N:M
  {{ relationship.is_inverse }}   # Is inverse relationship
{% endfor %}
```

## Type Mapping Reference

### TypeScript Type Mapping

```liquid
xsd:string         -> string
xsd:integer        -> number
xsd:decimal        -> number
xsd:boolean        -> boolean
xsd:dateTime       -> Date
xsd:date           -> Date
xsd:anyURI         -> string
[Class]            -> Class (reference)
```

### Python Type Mapping

```liquid
xsd:string         -> str
xsd:integer        -> int
xsd:decimal        -> float
xsd:boolean        -> bool
xsd:dateTime       -> datetime
xsd:date           -> date
xsd:anyURI         -> str
[Class]            -> Class (reference)
```

### Rust Type Mapping

```liquid
xsd:string         -> String
xsd:integer        -> i64
xsd:decimal        -> f64
xsd:boolean        -> bool
xsd:dateTime       -> DateTime<Utc>
xsd:date           -> NaiveDate
xsd:anyURI         -> String
[Class]            -> Class (reference)
```

## Built-in Filters

### String Filters

```liquid
{{ class.name | capitalize }}      # First letter uppercase
{{ class.name | downcase }}        # All lowercase
{{ class.name | upcase }}          # ALL UPPERCASE
{{ "user_name" | replace: "_", "" }} # Replace characters
{{ "test" | prepend: "prefix_" }}  # Add prefix
{{ "test" | append: "_suffix" }}   # Add suffix
```

### Collection Filters

```liquid
{{ classes | size }}               # Count items
{{ properties | first }}           # First item
{{ properties | last }}            # Last item
{{ properties | join: ", " }}      # Join with separator
{{ properties | map: "name" }}     # Extract field from each
{{ properties | where: "type", "string" }} # Filter by condition
```

### Logic Filters

```liquid
{% if class.properties | size > 0 %}
  Has {{ class.properties | size }} properties
{% endif %}

{% if property.is_required == true %}
  Required field
{% endif %}
```

## Looping Variables

### forloop Object

```liquid
{% for class in classes %}
  {{ forloop.index }}        # Loop counter (1-based)
  {{ forloop.index0 }}       # Loop counter (0-based)
  {{ forloop.first }}        # true on first iteration
  {{ forloop.last }}         # true on last iteration
  {{ forloop.length }}       # Total iterations
{% endfor %}
```

**Example**:
```liquid
{% for property in class.properties %}
  {% unless forloop.last %}{{ property.name }}, {% endunless %}
  {% if forloop.last %}{{ property.name }}{% endif %}
{% endfor %}
```

## Conditional Variables

### Existence Checks

```liquid
{% if class %}
  Class is defined
{% endif %}

{% if class.comment %}
  Has documentation
{% endif %}

{% if property.default_value %}
  Has default value
{% endif %}
```

### Type Checks

```liquid
{% if property.is_required %}
  Required field
{% endif %}

{% if property.is_array %}
  Multiple values
{% endif %}

{% if class.is_enum %}
  Enumeration type
{% endif %}
```

## Enumeration Variables

### For Enumeration Classes

```liquid
{% if class.is_enum %}
  {% for member in class.enum_members %}
    {{ member.name }}       # Member name
    {{ member.value }}      # Member value
    {{ member.comment }}    # Member documentation
  {% endfor %}
{% endif %}
```

**Example**:
```liquid
{% if class.is_enum %}
export enum {{ class.name }} {
  {% for member in class.enum_members %}
  {{ member.name }} = "{{ member.value }}",
  {% endfor %}
}
{% endif %}
```

## Examples

### Complete Class Template

```liquid
/**
 * {{ class.label }}
 * {{ class.comment }}
 * Generated from {{ ontology_name }}
 */
export class {{ class.name }} {
  {% for property in class.properties %}
  /**
   * {{ property.label }}
   * {{ property.comment }}
   */
  {% if property.is_required %}
  public {{ property.name }}: {{ property.mapped_type }};
  {% else %}
  public {{ property.name }}?: {{ property.mapped_type }};
  {% endif %}
  {% endfor %}
}
```

### Interface Template

```liquid
export interface {{ class.name }} {
  {% for property in class.properties %}
  /** {{ property.comment }} */
  {{ property.name }}{% unless property.is_required %}?{% endunless %}: {{ property.mapped_type }};
  {% endfor %}
}
```

### Enum Template

```liquid
{% if class.is_enum %}
export enum {{ class.name }} {
  {% for member in class.enum_members %}
  {{ member.name }} = "{{ member.value }}",
  {% endfor %}
}
{% endif %}
```

## Advanced: Custom Variables

Define custom variables in `ggen.toml`:

```toml
[generation]
template_vars = {
  api_version = "v1",
  author = "Your Team",
  include_validation = true
}
```

Access in templates:

```liquid
/**
 * API {{ api_version }}
 * Author: {{ author }}
 */

{% if include_validation %}
// Validation code included
{% endif %}
```

## Summary

You now know:
- ✅ All global variables available
- ✅ Class and property variables
- ✅ Type mapping for all languages
- ✅ Built-in template filters
- ✅ Conditional and looping patterns
- ✅ How to create advanced templates

Your templates are now fully featured!
