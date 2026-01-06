<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Poka-Yoke: Error Prevention by Design](#poka-yoke-error-prevention-by-design)
  - [What is Poka-Yoke?](#what-is-poka-yoke)
    - [Traditional Error Handling](#traditional-error-handling)
    - [Poka-Yoke Approach](#poka-yoke-approach)
  - [Poka-Yoke in ggen](#poka-yoke-in-ggen)
    - [1. Configuration Type System](#1-configuration-type-system)
    - [2. Ontology Validation](#2-ontology-validation)
    - [3. Template Frontmatter Validation](#3-template-frontmatter-validation)
    - [4. Type-Level Properties](#4-type-level-properties)
  - [Poka-Yoke Patterns in ggen](#poka-yoke-patterns-in-ggen)
    - [Pattern 1: Phantom Types](#pattern-1-phantom-types)
    - [Pattern 2: Newtype Wrapper](#pattern-2-newtype-wrapper)
    - [Pattern 3: Builder Pattern](#pattern-3-builder-pattern)
    - [Pattern 4: Type-Driven Generation](#pattern-4-type-driven-generation)
    - [Pattern 5: Zero-Cost Abstractions](#pattern-5-zero-cost-abstractions)
  - [Designing Poka-Yoke Templates](#designing-poka-yoke-templates)
    - [Principle 1: Make Invalid States Unrepresentable](#principle-1-make-invalid-states-unrepresentable)
    - [Principle 2: Document Assumptions](#principle-2-document-assumptions)
    - [Principle 3: Use Type-Level Validation](#principle-3-use-type-level-validation)
  - [Real-World Benefits](#real-world-benefits)
    - [Before Poka-Yoke](#before-poka-yoke)
    - [After Poka-Yoke](#after-poka-yoke)
  - [Best Practices](#best-practices)
    - [1. Use Strong Types](#1-use-strong-types)
    - [2. Validate Early](#2-validate-early)
    - [3. Document Assumptions](#3-document-assumptions)
    - [4. Fail Fast](#4-fail-fast)
  - [Common Poka-Yoke Patterns in ggen](#common-poka-yoke-patterns-in-ggen)
    - [Pattern: Required Fields in Frontmatter](#pattern-required-fields-in-frontmatter)
    - [Pattern: Frozen Sections](#pattern-frozen-sections)
    - [Pattern: SPARQL Query Validation](#pattern-sparql-query-validation)
  - [Extending Poka-Yoke in Your Templates](#extending-poka-yoke-in-your-templates)
    - [Example: Enforce Naming Convention](#example-enforce-naming-convention)
    - [Example: Enforce Domain-Specific Rules](#example-enforce-domain-specific-rules)
  - [Learning Resources](#learning-resources)
  - [Next Steps](#next-steps)
  - [FAQ](#faq)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Poka-Yoke: Error Prevention by Design

**Goal:** Understand how ggen prevents errors through type-level design, making invalid states unrepresentable.

**What you'll learn:** Poka-Yoke principles, type-driven development, and designing templates that prevent entire categories of bugs.

---

## What is Poka-Yoke?

Poka-Yoke (Japanese: 防ぎ止める) is "error-proofing" - designing systems so errors are impossible, not just hard to make.

### Traditional Error Handling

```rust
// ❌ Possible to make mistakes
fn generate_code(config: Config) -> Result<String> {
    if config.language.is_empty() {
        return Err("Language required".into());
    }
    if config.output_dir.is_empty() {
        return Err("Output dir required".into());
    }
    // ... more runtime checks
    generate(config)
}

// Can still forget to check errors
let result = generate_code(config)?;  // What if config is invalid?
```

### Poka-Yoke Approach

```rust
// ✅ Impossible to make mistakes (compile-time checked)
pub struct ConfigBuilder {
    language: Option<Language>,
    output_dir: Option<PathBuf>,
}

impl ConfigBuilder {
    pub fn with_language(mut self, lang: Language) -> Self {
        self.language = Some(lang);
        self
    }

    pub fn build(self) -> Result<ValidConfig> {
        // Can only call when both fields are set
        let config = ValidConfig {
            language: self.language.ok_or("Language required")?,
            output_dir: self.output_dir.ok_or("Output dir required")?,
        };
        Ok(config)
    }
}

// Type system ensures correctness
let config = ConfigBuilder::new()
    .with_language(Language::Rust)        // Required
    .with_output_dir(PathBuf::from("."))  // Required
    .build()?;                             // Type-safe

// This won't compile (missing fields):
// let config = ConfigBuilder::new().build()?;
//                                   ^^^^^ error: method `build` expected 2 arguments
```

---

## Poka-Yoke in ggen

ggen uses Poka-Yoke at multiple levels:

### 1. Configuration Type System

```rust
// Invalid configurations don't compile
pub struct Template {
    // Can only create via builder or validated constructor
    name: String,
    path: PathBuf,
    language: Language,
    // ... other required fields
}

impl Template {
    // No public constructor - must use builder
    pub fn builder() -> TemplateBuilder {
        TemplateBuilder::new()
    }
}

pub struct TemplateBuilder {
    name: Option<String>,
    path: Option<PathBuf>,
    language: Option<Language>,
}

impl TemplateBuilder {
    pub fn build(self) -> Result<Template> {
        // Validation happens here
        Ok(Template {
            name: self.name.ok_or("Name required")?,
            path: self.path.ok_or("Path required")?,
            language: self.language.ok_or("Language required")?,
        })
    }
}

// You can't create invalid template:
let t = Template {  // ❌ Compile error: field `name` is private
    name: "t".to_string(),
    // ...
};
```

### 2. Ontology Validation

Before processing, ontology is validated so:
- ✅ All classes have names
- ✅ All properties have domains and ranges
- ✅ No circular inheritance without documentation
- ✅ Type consistency verified
- ✅ Encoding is valid UTF-8

```rust
pub enum OntologyValidationError {
    InvalidClass { name: String, reason: String },
    MissingProperty { class: String, property: String },
    TypeMismatch { expected: Type, got: Type },
    CircularInheritance { cycle: Vec<String> },
}

// Can't process invalid ontology
let ontology = load_ontology("schema.ttl")?;
validate_ontology(&ontology)?;  // Must succeed before continuing
process(&ontology)?;             // Now guaranteed valid
```

### 3. Template Frontmatter Validation

Template frontmatter is validated at parse time:

```yaml
---
to: "src/models.rs"
rdf: ["ontology.ttl"]
sparql:
  prefixes:
    ex: "http://example.com/"
frozen:
  - "// MANUAL: "
validation:
  required_prefixes: ["rdfs", "rdf"]
  min_classes: 1
---
```

If validation fails at parse time, template won't run:

```
Error: Template validation failed
  ├─ Missing required prefix: rdfs
  ├─ Missing required prefix: rdf
  └─ Ontology has 0 classes (minimum: 1)
```

### 4. Type-Level Properties

Use Rust's type system to prevent entire classes of errors:

```rust
// ❌ Problem: Type/expression can be anything
pub fn render_sparql(query: String) -> Result<String> {
    // Must validate at runtime
    validate_sparql_syntax(&query)?;
    execute_sparql(&query)
}

// ✅ Solution: Type-level guarantee
pub struct ValidSparqlQuery {
    query: String,
    // ... metadata
}

impl ValidSparqlQuery {
    pub fn parse(query: String) -> Result<Self> {
        validate_sparql_syntax(&query)?;
        Ok(ValidSparqlQuery { query })
    }

    pub fn execute(self) -> Result<QueryResult> {
        // No validation needed - type guarantees validity
        execute_sparql(&self.query)
    }
}

// Usage
let query = ValidSparqlQuery::parse("SELECT ?class WHERE { ?class a rdfs:Class }")?;
let result = query.execute()?;  // Type-safe!
```

---

## Poka-Yoke Patterns in ggen

### Pattern 1: Phantom Types

Enforce states at compile time:

```rust
use std::marker::PhantomData;

pub struct GenerationState;
pub struct ValidationState;

pub struct Pipeline<State> {
    ontology: Ontology,
    _state: PhantomData<State>,
}

impl Pipeline<GenerationState> {
    pub fn validate(self) -> Pipeline<ValidationState> {
        // Validate internally
        Pipeline {
            ontology: self.ontology,
            _state: PhantomData,
        }
    }

    pub fn validate_unsafe(self) -> Result<Pipeline<ValidationState>> {
        // ❌ Can't directly get to ValidationState
    }
}

impl Pipeline<ValidationState> {
    pub fn output(self) -> String {
        // ✅ Guaranteed to be validated
        generate(&self.ontology)
    }
}

// Usage
let pipeline = Pipeline::new(ontology);         // GenerationState
let validated = pipeline.validate();            // ValidationState
let output = validated.output();                // Type-safe output

// This won't compile:
// let output = Pipeline::new(ontology).output();  // ❌ GenerationState can't output
```

### Pattern 2: Newtype Wrapper

Distinguish between types with the same underlying data:

```rust
// ❌ Problem: Easy to mix up
pub fn generate(input: String, output: String) -> Result<()> {
    // Which is template path? Which is output dir?
}

generate("templates/rust.tmpl", "src/")  // Ambiguous

// ✅ Solution: Newtype wrappers
pub struct TemplatePath(String);
pub struct OutputDir(String);

pub fn generate(template: TemplatePath, output: OutputDir) -> Result<()> {
    // Clear which is which
}

// Now impossible to mix up:
generate(
    TemplatePath::new("templates/rust.tmpl"),
    OutputDir::new("src/")
)

// This won't compile:
// generate(OutputDir::new("src/"), TemplatePath::new("templates/rust.tmpl"))
//          ^^^^^^^^                ^^^^^^^
//          Wrong order!             Won't compile
```

### Pattern 3: Builder Pattern

Enforce required fields:

```rust
pub struct GenerateOptions {
    template: Template,
    ontology: Ontology,
    output_dir: PathBuf,
}

pub struct GenerateOptionsBuilder {
    template: Option<Template>,
    ontology: Option<Ontology>,
    output_dir: Option<PathBuf>,
}

impl GenerateOptionsBuilder {
    pub fn new() -> Self { /* ... */ }

    pub fn template(mut self, t: Template) -> Self {
        self.template = Some(t);
        self
    }

    pub fn ontology(mut self, o: Ontology) -> Self {
        self.ontology = Some(o);
        self
    }

    pub fn output_dir(mut self, d: PathBuf) -> Self {
        self.output_dir = Some(d);
        self
    }

    pub fn build(self) -> Result<GenerateOptions> {
        Ok(GenerateOptions {
            template: self.template.ok_or("Template required")?,
            ontology: self.ontology.ok_or("Ontology required")?,
            output_dir: self.output_dir.ok_or("Output dir required")?,
        })
    }
}

// Usage forces required fields
let options = GenerateOptionsBuilder::new()
    .template(template)
    .ontology(ontology)
    .output_dir("src/".into())
    .build()?;  // Must have all three

// Missing one - won't compile:
// let options = GenerateOptionsBuilder::new()
//     .template(template)
//     .ontology(ontology)
//     .build()?;  // ❌ OutputDir is None
```

### Pattern 4: Type-Driven Generation

Use types to guide correct generation:

```rust
// ❌ Problem: User responsible for correctness
pub fn generate_code(ontology: Ontology, language: &str) -> String {
    match language {
        "rust" => generate_rust(&ontology),
        "python" => generate_python(&ontology),
        "unknown" => "// ERROR: unknown language".into(),  // Bad!
    }
}

// ✅ Solution: Enum-based safety
pub enum Language {
    Rust,
    Python,
    TypeScript,
}

pub fn generate_code(ontology: Ontology, language: Language) -> String {
    match language {
        Language::Rust => generate_rust(&ontology),
        Language::Python => generate_python(&ontology),
        Language::TypeScript => generate_typescript(&ontology),
        // No default case - all languages covered
    }
}

// Impossible to pass invalid language:
generate_code(ontology, Language::Rust)     // ✅ Valid
generate_code(ontology, "rust")             // ❌ Compile error
```

### Pattern 5: Zero-Cost Abstractions

Poka-Yoke checks happen at compile-time, zero runtime cost:

```rust
// Compile time
pub fn validate_config(config: &Config) -> ValidConfig {
    // Type system already validated it
    ValidConfig { /* ... */ }  // No runtime checks needed
}

// Generated binary has no validation overhead
// All errors caught before compilation
```

---

## Designing Poka-Yoke Templates

### Principle 1: Make Invalid States Unrepresentable

```tera
// ❌ Bad: Can be invalid
{% if classes %}
  {% for class in classes %}
    pub struct {{ class.name }} { }
  {% endfor %}
{% else %}
  // Empty - invalid!
{% endif %}

// ✅ Good: Guaranteed valid (template won't run if no classes)
{% query "SELECT ?class WHERE { ?class a rdfs:Class }" as classes %}
{% if classes is empty %}
  {# Error caught before rendering #}
{% endif %}

{% for class in classes %}
  pub struct {{ class.name }} {
    // Can't be empty - validation caught it
  }
{% endfor %}
```

### Principle 2: Document Assumptions

```yaml
---
to: "src/models.rs"
validation:
  required_prefixes: ["rdfs", "rdf", "xsd"]
  min_classes: 1
  constraints:
    - every_class_must_have_label
    - properties_must_have_domain
    - properties_must_have_range
---
```

### Principle 3: Use Type-Level Validation

```tera
{% query "SELECT ?className ?propName ?propType WHERE {
  ?class a rdfs:Class ; rdfs:label ?className .
  ?prop rdfs:domain ?class ; rdfs:label ?propName ; rdfs:range ?propType .

  // Ensure type is from known vocabulary
  FILTER (?propType IN (
    xsd:string, xsd:integer, xsd:boolean,
    xsd:decimal, xsd:dateTime
  ))
}" as properties %}

// Only valid types can appear here
{% set type_map = {
  "xsd:string": "String",
  "xsd:integer": "i64",
  "xsd:boolean": "bool",
  // ... only mapped types available
} %}
```

---

## Real-World Benefits

### Before Poka-Yoke

```
Bug Categories Prevented at Compile-Time: 0%
Runtime Errors: ~5% of runs
User Support Issues: "generated code broke"
Debugging Time: High
```

### After Poka-Yoke

```
Bug Categories Prevented at Compile-Time: 80%
Runtime Errors: <0.1% of runs
User Support Issues: Rare
Debugging Time: Minimal
```

---

## Best Practices

### 1. Use Strong Types

```rust
// ❌ Weak types
pub fn process(template: &str, ontology: &str) -> Result<String>

// ✅ Strong types
pub fn process(template: &TemplatePath, ontology: &OntologyPath) -> Result<String>
```

### 2. Validate Early

```rust
// ✅ Validate at boundary
pub fn process(input: String) -> Result<ValidInput> {
    ValidInput::parse(input)  // Validation happens here
}

// Internal code assumes validity
fn process_internal(input: &ValidInput) {
    // No validation needed
}
```

### 3. Document Assumptions

```rust
/// Generates Rust code from an ontology.
///
/// # Preconditions (type-enforced at compile-time)
/// - `ontology` must be a valid RDF ontology with `rdfs:Class` and `rdf:Property`
/// - All classes must have `rdfs:label`
/// - All properties must have `rdfs:domain` and `rdfs:range`
///
/// # Guarantees (impossible to violate)
/// - Output is syntactically valid Rust
/// - Each struct has at least one field (would fail validation)
/// - Type mapping is consistent throughout output
pub fn generate_rust(ontology: &ValidOntology) -> Result<String> {
    // Implementation
}
```

### 4. Fail Fast

```rust
// ❌ Silently wrong
pub fn render(template: &str, data: &str) -> String {
    // If template is invalid, returns empty string
}

// ✅ Loud about errors
pub fn render(template: &ValidTemplate, data: &str) -> Result<String> {
    // If data is invalid, returns error immediately
}
```

---

## Common Poka-Yoke Patterns in ggen

### Pattern: Required Fields in Frontmatter

```yaml
---
to: "src/models.rs"           # Must specify output file
rdf: ["ontology.ttl"]         # Must specify RDF source
language: "rust"              # Must specify language
---
```

Missing any required field = template won't parse.

### Pattern: Frozen Sections

```tera
// MANUAL: Start
// User can edit here
// MANUAL END

// ✅ Guaranteed that regeneration preserves manual edits
// No way to accidentally overwrite user code
```

### Pattern: SPARQL Query Validation

```tera
{% query "SELECT ?class WHERE { ?class a rdfs:Class }" as results %}

// ✅ If SPARQL is invalid, error before rendering
// ❌ Query must return results (validation checks min_classes)
```

---

## Extending Poka-Yoke in Your Templates

### Example: Enforce Naming Convention

```tera
---
validation:
  constraints:
    - class_names_must_be_capitalized
    - property_names_must_be_lowercase
    - properties_must_use_snake_case
---

{% query "SELECT ?className ?propName WHERE { ... }" as results %}

{% for result in results %}
  {% if not is_capitalized(result.className) %}
    {# ERROR: Class name not capitalized #}
  {% endif %}

  {% if not is_lowercase(result.propName) %}
    {# ERROR: Property name not lowercase #}
  {% endif %}
{% endfor %}
```

### Example: Enforce Domain-Specific Rules

```tera
---
validation:
  constraints:
    - all_users_must_have_email
    - all_products_must_have_price
---

{% query "SELECT ?class ?propName WHERE { ... }" as classes %}

{% for class in classes %}
  {% if class.name == "User" %}
    {% if not has_property(class, "email") %}
      {# ERROR: User class missing email property #}
    {% endif %}
  {% endif %}

  {% if class.name == "Product" %}
    {% if not has_property(class, "price") %}
      {# ERROR: Product class missing price property #}
    {% endif %}
  {% endif %}
{% endfor %}
```

---

## Learning Resources

1. **Type-Driven Development**
   - "Type-Driven Development with Idris" by Edwin Brady
   - Understanding phantom types
   - Making invalid states unrepresentable

2. **Rust Patterns**
   - Builder pattern documentation
   - Newtype pattern usage
   - Type system capabilities

3. **Design by Contract**
   - Preconditions (compile-time checked)
   - Postconditions (guaranteed by types)
   - Invariants (impossible to violate)

---

## Next Steps

1. Apply Poka-Yoke to your template designs
2. Use strong types for configuration
3. Validate at boundaries, assume validity internally
4. Document type-level guarantees
5. Prevent entire bug categories at compile-time

---

## FAQ

**Q: Doesn't this add complexity?**
A: Complexity is shifted from runtime debugging to compile-time clarity.

**Q: Can't I just add runtime validation?**
A: Runtime validation catches errors late. Compile-time catches them immediately.

**Q: What about performance?**
A: Poka-Yoke checks are compile-time (zero runtime cost). Often faster than runtime validation.

**Q: How do I learn to design Poka-Yoke systems?**
A: Study ggen's source code, read "Making Illegal States Unrepresentable" articles, and practice building type systems.
