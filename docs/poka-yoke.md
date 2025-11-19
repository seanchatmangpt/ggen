# Poka-Yoke Mistake-Proofing for Code Generation

## Overview

The Poka-Yoke module implements manufacturing-grade mistake-proofing principles in code generation, using Rust's type system to make errors physically impossible at compile time.

## Design Philosophy

Poka-Yoke (ポカヨケ) is a Japanese term meaning "mistake-proofing" or "error-proofing". In manufacturing, it refers to mechanisms that prevent errors by design. We apply these principles to code generation:

1. **Physical Constraints**: Use the type system to prevent mistakes physically
2. **Compile-Time Validation**: Catch errors at compile time, not runtime
3. **Type-State Pattern**: Track state in the type system to enforce workflows
4. **Phantom Types**: Encode semantic information at the type level
5. **Sealed Traits**: Prevent unsafe extensions

## Core Components

### 1. Ontology Guards (`ontology_guards`)

Compile-time type guards for ontology transformations preventing invalid schema migrations.

#### Features

- **Schema Version Tracking**: Phantom types track schema versions at compile time
- **Transformation Guards**: Only valid transformations can be constructed
- **Namespace Validation**: Compile-time namespace compatibility checking
- **Progressive Validation**: Unvalidated → Validated → SchemaChecked

#### Example

```rust
use ggen_domain::poka_yoke::ontology_guards::{
    ValidatedOntology, V1_0_0, V2_0_0, OntologyTransform, Unvalidated
};

// Create validated ontology
let ontology_v1 = ValidatedOntology::<V1_0_0, Unvalidated>::new(graph)
    .validate()?
    .check_schema()?;

// Transform to V2 (compile-time checked)
let transform = OntologyTransform::<V1_0_0, V2_0_0>::new()
    .expect("V1 -> V2 is always valid");

let ontology_v2 = transform.apply(ontology_v1)?;

// This does NOT compile - backward migration prevented:
// let backward = OntologyTransform::<V2_0_0, V1_0_0>::new();
// Returns None - invalid transformation
```

### 2. Template Selection (`template_selection`)

Fool-proof template selection with physical constraints preventing incompatible template usage.

#### Features

- **Template Type Markers**: Sealed trait prevents invalid template types
- **Compatibility Checking**: Compile-time template/schema compatibility
- **Physical Constraints**: Path validation, depth limits, forbidden components
- **Progressive Selection**: Unselected → Selected → Validated

#### Example

```rust
use ggen_domain::poka_yoke::template_selection::{
    TemplateSelector, RustLibrary, V2_0_0
};

let selector = TemplateSelector::for_schema::<V2_0_0>();

// Rust library compatible with v2.0.0
let template = selector
    .select::<RustLibrary>(path)?
    .select()?
    .validate(&ontology)?;

// NextJS requires v2.1.0+ - won't compile for v2.0.0
// let nextjs = selector.select::<NextJsApp>(path)?; // Error!
```

### 3. API Builder (`api_builder`)

Error-impossible API design using type-state pattern ensuring complete configuration.

#### Features

- **Type-State Pattern**: Builder states tracked at type level
- **Required Fields**: `generate()` only available when complete
- **Phantom Types**: Track template type and schema version
- **Progressive Building**: Incomplete → WithTemplate → WithOntology → WithOutput → Complete

#### Example

```rust
use ggen_domain::poka_yoke::api_builder::{
    GeneratorBuilder, Incomplete
};

// All required fields must be set - enforced by type system
let generator = GeneratorBuilder::<V2_0_0, RustLibrary, Incomplete>::new()
    .with_template(template)    // → WithTemplate
    .with_ontology(ontology)    // → WithOntology
    .with_output(output_path)   // → WithOutput
    .complete()                 // → Complete
    .build();                   // Only available when Complete

// This does NOT compile - missing required fields:
// let incomplete = GeneratorBuilder::new()
//     .with_template(template)
//     .build(); // Error: method not available!

let result = generator.generate()?;
```

### 4. Semantic Projection (`semantic_projection`)

Zero-defect semantic projections with physical constraints on RDF triple construction.

#### Features

- **Triple Component Markers**: Subject/Predicate/Object tracked at type level
- **Component Validators**: Each component validated for its role
- **Progressive Validation**: Unchecked → TypeChecked → SemanticChecked → FullyValidated
- **Physical Constraints**: Triple count limits, namespace requirements

#### Example

```rust
use ggen_domain::poka_yoke::semantic_projection::{
    SemanticProjection, TripleValidator, Unchecked
};

// Create projection
let mut projection = SemanticProjection::<V2_0_0, Unchecked>::new(ontology);

// Validate triple components
let validator = TripleValidator::<V2_0_0>::new();
let triple = validator.build_triple(
    "http://ggen.io/schema/v2#Class",      // Subject
    "http://www.w3.org/.../rdf-syntax-ns#type",  // Predicate
    "http://www.w3.org/.../rdf-schema#Class",    // Object
)?;

projection.add_triple(triple);

// Progressive validation
let fully_validated = projection
    .type_check()?           // → TypeChecked
    .semantic_check()?       // → SemanticChecked
    .full_validate()?;       // → FullyValidated
```

## Type Safety Guarantees

### Compile-Time Enforcement

1. **Invalid States Unrepresentable**
   - Incomplete builders cannot generate code
   - Invalid schema transformations cannot be constructed
   - Incompatible templates cannot be selected

2. **Progressive Validation**
   - Each validation level requires the previous
   - State transitions tracked in type system
   - No runtime state checking needed

3. **Component Role Enforcement**
   - Subject/Predicate/Object cannot be confused
   - Template types cannot be mixed
   - Schema versions always compatible

### Runtime Safety

While most safety is compile-time, runtime validation ensures:

- File system operations succeed
- Graph queries are valid
- Template rendering works correctly

## Usage Patterns

### Pattern 1: Safe Ontology Migration

```rust
// Load V1 ontology
let v1_ontology = load_and_validate::<V1_0_0>(path)?;

// Migrate to V2
let transform = OntologyTransform::<V1_0_0, V2_0_0>::new()
    .expect("Forward migration always valid");
let v2_ontology = transform.apply(v1_ontology)?;

// Use V2 features
generate_code_v2(v2_ontology)?;
```

### Pattern 2: Template Selection with Validation

```rust
let selector = TemplateSelector::for_schema::<V2_1_0>();

let template = selector
    .select::<NextJsApp>(template_path)?
    .select()?
    .validate(&ontology)?;

assert_eq!(template.schema_version(), "2.1.0");
```

### Pattern 3: Complete Generation Workflow

```rust
let result = GeneratorBuilder::new()
    .with_template(template)
    .with_ontology(ontology)
    .with_output(output_dir)
    .with_variable("name", "my_project")
    .with_dry_run(false)
    .complete()
    .build()
    .generate()?;

println!("Generated {} files", result.file_count());
```

### Pattern 4: Semantic Projection Building

```rust
let validator = TripleValidator::<V2_0_0>::new();
let mut projection = SemanticProjection::new(ontology);

// Add validated triples
for (s, p, o) in raw_triples {
    let triple = validator.build_triple(s, p, o)?;
    projection.add_triple(triple);
}

// Validate fully
let validated = projection
    .type_check()?
    .semantic_check()?
    .full_validate()?;
```

## Physical Constraints Reference

### Template Path Constraints

- **Max Depth**: 10 levels
- **Forbidden Components**: `..`, `.git`, `target`
- **Required Extensions**: `.md` for templates

### Ontology Constraints

- **Namespace Validation**: Must match schema version
- **Required Predicates**: Core RDF/RDFS predicates
- **SHACL Validation**: Optional but recommended

### Semantic Projection Constraints

- **Max Triples**: 10,000 (configurable)
- **Strict Namespace**: Enforce schema namespace
- **SHACL Required**: Optional enforcement

## Integration with Existing Code

The Poka-Yoke system integrates with existing ggen components:

```rust
use ggen_domain::{
    graph::core::Graph,
    poka_yoke::{
        ontology_guards::*,
        template_selection::*,
        api_builder::*,
    },
};

// Use existing Graph type
let graph = Graph::new()?;

// Wrap with Poka-Yoke validation
let ontology = ValidatedOntology::<V2_0_0, _>::new(graph)
    .validate()?
    .check_schema()?;

// Continue with safe generation
```

## Testing

The Poka-Yoke system includes comprehensive tests:

```bash
# Run all Poka-Yoke tests
cargo test -p ggen-domain poka_yoke

# Run specific module tests
cargo test -p ggen-domain ontology_guards
cargo test -p ggen-domain template_selection
cargo test -p ggen-domain api_builder
cargo test -p ggen-domain semantic_projection
```

## Performance

Poka-Yoke adds **zero runtime overhead** - all validation is compile-time:

- Phantom types: Zero-cost abstractions
- Type-state pattern: Compile-time only
- Sealed traits: No vtable overhead

Runtime validation only occurs for:
- File system operations
- Graph queries
- Template rendering

## Future Enhancements

1. **SHACL Integration**: Full SHACL constraint validation
2. **Schema Registry**: Dynamic schema version loading
3. **Template Marketplace**: Compile-time template compatibility checking
4. **Macro Support**: Derive macros for custom validators
5. **IDE Integration**: Error messages and autocomplete

## References

- [Poka-Yoke Principles](https://en.wikipedia.org/wiki/Poka-yoke)
- [Type-State Pattern in Rust](https://cliffle.com/blog/rust-typestate/)
- [Phantom Types](https://doc.rust-lang.org/rust-by-example/generics/phantom.html)
- [Making Invalid States Unrepresentable](https://geeklaunch.io/blog/make-invalid-states-unrepresentable/)

## License

Same as ggen main project (MIT/Apache-2.0)
