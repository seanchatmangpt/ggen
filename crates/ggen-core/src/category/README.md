# Category Theory Foundations for Type-Safe Ontology Morphisms

This module provides a rigorous category-theoretic framework for type-safe code generation and ontology transformations in ggen. It ensures correctness through algebraic structures and compile-time verification.

## Table of Contents

1. [Overview](#overview)
2. [Core Concepts](#core-concepts)
3. [Module Structure](#module-structure)
4. [Key Features](#key-features)
5. [Mathematical Foundations](#mathematical-foundations)
6. [Usage Examples](#usage-examples)
7. [Advanced Topics](#advanced-topics)
8. [Future Work](#future-work)

## Overview

Category theory provides a universal language for describing mathematical structures and their relationships. This module applies category theory to:

- **Type-safe transformations**: Guarantee correctness of RDF → Code mappings
- **Compositional pipelines**: Build complex transformations from simple ones
- **Provable properties**: Use type-level proofs for structural preservation
- **Abstract interfaces**: Define transformations independent of implementation

### Why Category Theory?

Traditional code generation often relies on string templates and ad-hoc transformations. This approach is error-prone and difficult to verify. Category theory provides:

1. **Formal semantics**: Mathematical precision about what transformations do
2. **Composition laws**: Guarantees about how transformations combine
3. **Type safety**: Compile-time verification of transformation correctness
4. **Abstraction**: Reusable patterns across different domains

## Core Concepts

### Categories

A **category** C consists of:
- **Objects**: Things in the category (Obj(C))
- **Morphisms**: Structure-preserving maps between objects (Hom(A, B))
- **Composition**: Combining morphisms (∘)
- **Identity**: Do-nothing morphism for each object (id_A)

**Category Laws**:
1. **Associativity**: `(h ∘ g) ∘ f = h ∘ (g ∘ f)`
2. **Identity**: `f ∘ id_A = f = id_B ∘ f`

### Functors

A **functor** F: C → D is a structure-preserving map between categories:
- Maps objects: `A ∈ C` → `F(A) ∈ D`
- Maps morphisms: `f: A → B` → `F(f): F(A) → F(B)`

**Functor Laws**:
1. **Identity preservation**: `F(id_A) = id_F(A)`
2. **Composition preservation**: `F(g ∘ f) = F(g) ∘ F(f)`

### Natural Transformations

A **natural transformation** η: F → G between functors F, G: C → D consists of:
- Components: `η_A: F(A) → G(A)` for each object A
- **Naturality**: For every morphism `f: A → B`:
  ```
  G(f) ∘ η_A = η_B ∘ F(f)
  ```

This ensures transformations are "uniform" across the category.

### Monoidal Categories

A **monoidal category** is a category with:
- **Tensor product**: `⊗: C × C → C`
- **Unit object**: I
- **Associator**: `(A ⊗ B) ⊗ C ≅ A ⊗ (B ⊗ C)`
- **Unitors**: `I ⊗ A ≅ A ≅ A ⊗ I`

Used for modeling sequential composition of pipeline stages.

### Yoneda Lemma

The **Yoneda lemma** states that for any functor F: C → Set and object A:
```
Nat(Hom(A, -), F) ≅ F(A)
```

Natural transformations from the hom-functor correspond bijectively to elements of F(A).

**Applications**:
- Templates as representable functors
- Type-safe code generation
- Universal properties

## Module Structure

```
category/
├── mod.rs                 # Module exports and common types
├── base.rs                # Core category theory traits
├── functor.rs             # Functorial mappings between domains
├── monoidal.rs            # Monoidal categories for pipelines
├── natural.rs             # Natural transformations
├── ontology.rs            # Type-safe ontology morphisms
├── yoneda.rs              # Yoneda lemma applications
└── README.md              # This file
```

### Module Dependencies

```
base.rs (Category, Morphism, Object)
   ↓
functor.rs (Functor, RdfCategory, CodeCategory)
   ↓
monoidal.rs (MonoidalCategory, TemplatePipelineMonoid)
   ↓
natural.rs (NaturalTransformation, VerticalComposition)
   ↓
ontology.rs (OntologyMorphism, TypeSafeMorphism)
   ↓
yoneda.rs (Representable, CodeGenerator)
```

## Key Features

### 1. Type-Safe Morphisms

Morphisms carry compile-time proofs of their properties:

```rust
struct OntologyMorphism<P> {
    source: Ontology,
    target: Ontology,
    class_map: BTreeMap<String, String>,
    property_map: BTreeMap<String, String>,
    proof: Proof<P>,  // Compile-time verification
}

type TypeSafeMorphism = OntologyMorphism<PreservesTypes>;
```

### 2. Functorial RDF → Code Mapping

```rust
let functor = RdfCodeFunctor::new(TargetLanguage::Rust);

let rdf_class = RdfObject::Class {
    uri: "http://example.org#Product",
    properties: vec!["name", "price"],
};

let code = functor.map_object(&rdf_class)?;
// Generates: pub struct Product { name: String, price: f64 }
```

### 3. Monoidal Pipeline Composition

```rust
let pipeline = TemplatePipelineMonoid::new()
    .append(PipelineStage::Parse { template_path: "..." })
    .append(PipelineStage::LoadRdf { rdf_files: vec![...] })
    .append(PipelineStage::ExecuteSparql { query: "..." })
    .append(PipelineStage::RenderBody { context: vec![...] });

let result = pipeline.execute()?;
```

### 4. Yoneda-based Code Generation

```rust
let mut generator = CodeGenerator::new();
generator.register_template(
    "struct_template".to_string(),
    TemplateDefinition::new(
        "struct_template".to_string(),
        schema,
        "pub struct {{name}} { ... }".to_string(),
        OutputType::Rust,
    ),
);

let context = GenerationContext::new()
    .with_variable("name".to_string(), "Product".to_string());

let code = generator.generate("struct_template", &context)?;
```

## Mathematical Foundations

### RDF Category

**Objects**: RDF entities
- RDF triples `(subject, predicate, object)`
- RDF graphs (sets of triples)
- Ontology classes
- Ontology properties
- SPARQL queries

**Morphisms**: RDF transformations
- SPARQL query transformations
- Graph transformations
- Ontology mappings

### Code Category

**Objects**: Code entities
- Type definitions (struct, enum, interface)
- Function definitions
- Modules
- Templates

**Morphisms**: Code transformations
- Type transformations (add field, change type)
- Refactorings
- Template instantiations

### RDF → Code Functor

The functor `F: RDF → Code` maps:

| RDF Object | Code Object |
|------------|-------------|
| `owl:Class` | `struct` / `class` |
| `owl:ObjectProperty` | Relationship / reference |
| `owl:DatatypeProperty` | Field / attribute |
| `rdf:subClassOf` | Inheritance / trait |
| `SPARQL SELECT` | Query function |

**Preservation Properties**:
- Class hierarchy preserved as type hierarchy
- Property domains/ranges become field types
- Axioms become type constraints

### Pipeline Monoidal Category

**Objects**: Pipeline stages
- Parse
- Load RDF
- Execute SPARQL
- Render template
- Apply plan

**Tensor Product**: Sequential composition
```
P₁ ⊗ P₂ = "run P₁, then run P₂"
```

**Unit**: Identity pipeline (no-op)

**Monoidal Laws**:
1. `(P₁ ⊗ P₂) ⊗ P₃ ≅ P₁ ⊗ (P₂ ⊗ P₃)` (associativity)
2. `I ⊗ P ≅ P ≅ P ⊗ I` (unit laws)

### Ontology Morphisms

An **ontology morphism** `φ: O₁ → O₂` consists of:
- Class mapping: `C₁ → C₂`
- Property mapping: `P₁ → P₂`

**Preservation Requirements**:
1. **Domain/Range**: If `p: C → D` in O₁, then `φ(p): φ(C) → φ(D)` in O₂
2. **Subclass**: If `C ⊑ D` in O₁, then `φ(C) ⊑ φ(D)` in O₂
3. **Property chains**: Preserved through composition

## Usage Examples

### Example 1: RDF Class to Rust Struct

```rust
use ggen_core::category::{RdfCodeFunctor, RdfObject, TargetLanguage};

let functor = RdfCodeFunctor::new(TargetLanguage::Rust);

let product_class = RdfObject::Class {
    uri: "http://ecommerce.org#Product".to_string(),
    properties: vec![
        "http://ecommerce.org#name".to_string(),
        "http://ecommerce.org#price".to_string(),
        "http://ecommerce.org#sku".to_string(),
    ],
};

let rust_struct = functor.map_object(&product_class)?;

// Result:
// pub struct Product {
//     pub name: String,
//     pub price: f64,
//     pub sku: String,
// }
```

### Example 2: Composing Ontology Morphisms

```rust
use ggen_core::category::{Ontology, OntologyMorphism, MorphismComposition};

// Create three ontologies
let ontology_a = Ontology::new("http://a.org".to_string());
let ontology_b = Ontology::new("http://b.org".to_string());
let ontology_c = Ontology::new("http://c.org".to_string());

// Create morphisms
let f: OntologyMorphism = /* A → B */;
let g: OntologyMorphism = /* B → C */;

// Compose: A → C
let h = MorphismComposition::compose(&f, &g)?;

// h preserves all structure transitively
assert!(h.verify().is_ok());
```

### Example 3: Template Pipeline with Monoid

```rust
use ggen_core::category::{TemplatePipelineMonoid, PipelineStage};

// Build a complete generation pipeline
let pipeline = TemplatePipelineMonoid::new()
    .append(PipelineStage::Parse {
        template_path: "templates/rust-struct.tmpl".to_string(),
    })
    .append(PipelineStage::RenderFrontmatter {
        variables: vec![
            ("project_name".to_string(), "my-project".to_string()),
        ],
    })
    .append(PipelineStage::LoadRdf {
        rdf_files: vec!["ontology.ttl".to_string()],
        inline_rdf: None,
    })
    .append(PipelineStage::ExecuteSparql {
        query: r#"
            SELECT ?class ?property WHERE {
                ?class a owl:Class .
                ?property rdfs:domain ?class .
            }
        "#.to_string(),
    })
    .append(PipelineStage::RenderBody {
        context: vec![],
    })
    .append(PipelineStage::ApplyPlan {
        output_path: "src/models.rs".to_string(),
        dry_run: false,
    });

// Execute with monoidal composition
let result = pipeline.execute()?;
```

### Example 4: Yoneda Lemma for Type-Safe Generation

```rust
use ggen_core::category::{
    CodeGenerator, TemplateDefinition, TemplateSchema,
    GenerationContext, OutputType,
};

// Define a template schema (representing object)
let mut schema = TemplateSchema::new();
schema.required.push("class_name".to_string());
schema.required.push("properties".to_string());

// Create a template (representable functor)
let template = TemplateDefinition::new(
    "rust_struct".to_string(),
    schema,
    r#"
pub struct {{class_name}} {
{{#each properties}}
    pub {{name}}: {{type}},
{{/each}}
}
    "#.to_string(),
    OutputType::Rust,
);

// Register with generator
let mut generator = CodeGenerator::new();
generator.register_template("rust_struct".to_string(), template);

// Generate code (natural transformation via Yoneda)
let context = GenerationContext::new()
    .with_variable("class_name".to_string(), "User".to_string());

let code = generator.generate("rust_struct", &context)?;
```

## Advanced Topics

### Adjoint Functors

Functors `F: C → D` and `G: D → C` are **adjoint** if:
```
Hom_D(F(A), B) ≅ Hom_C(A, G(B))
```

**Application**: Free/forgetful functors between RDF and Code categories
- Free functor: Minimal code structure from RDF
- Forgetful functor: Extract RDF semantics from code

### Limits and Colimits

**Limits** generalize:
- Products: `A × B`
- Pullbacks: Fiber products
- Equalizers: Constraint satisfaction

**Colimits** generalize:
- Coproducts: `A + B`
- Pushouts: Gluing constructions
- Coequalizers: Quotients

**Application**: Merging ontologies, combining templates

### Enriched Categories

A category **enriched** over a monoidal category V has hom-objects in V:
```
Hom(A, B) ∈ Obj(V)
```

**Application**:
- Probabilistic transformations (enriched over [0, 1])
- Graded transformations (enriched over ordered sets)
- Quantified transformations (enriched over vector spaces)

### 2-Categories

A **2-category** has:
- 0-cells: Objects (categories)
- 1-cells: Morphisms (functors)
- 2-cells: 2-morphisms (natural transformations)

**Composition**:
- Vertical: Compose natural transformations
- Horizontal: Compose across functors

**Application**: Transformations between transformation strategies

## Future Work

### Planned Enhancements

1. **Dependent Types**
   - Use Rust's type system more extensively
   - Encode more category laws at compile time
   - Indexed categories and fibrations

2. **Homotopy Type Theory**
   - Higher-dimensional structure
   - Path induction for equality
   - Univalence for equivalences

3. **Topos Theory**
   - Subobject classifiers for constraints
   - Exponential objects for higher-order functions
   - Internal logic for reasoning

4. **Operads**
   - Multi-ary operations
   - Composition trees
   - Algebraic theories

### Research Directions

1. **Formal Verification**
   - Coq/Lean proofs of category laws
   - Verified functors and natural transformations
   - Certified code generation

2. **Optimization**
   - Functor fusion
   - Rewrite rules using category laws
   - Compile-time evaluation

3. **Distributed Systems**
   - Sheaves for distributed data
   - Factorization systems for consistency
   - Fibrations for modular systems

## References

### Category Theory

- **Mac Lane, S.** (1998). *Categories for the Working Mathematician*
- **Awodey, S.** (2010). *Category Theory* (2nd ed.)
- **Riehl, E.** (2016). *Category Theory in Context*

### Applied Category Theory

- **Spivak, D.** (2014). *Category Theory for the Sciences*
- **Fong, B. & Spivak, D.** (2019). *Seven Sketches in Compositionality*
- **Bradley, T.** (2021). *What is Applied Category Theory?*

### Categorical Semantics

- **Pierce, B.** (1991). *Basic Category Theory for Computer Scientists*
- **Barr, M. & Wells, C.** (1995). *Category Theory for Computing Science*
- **Crole, R.** (1993). *Categories for Types*

### Ontologies and RDF

- **W3C** (2014). *RDF 1.1 Semantics*
- **W3C** (2012). *OWL 2 Web Ontology Language Primer*
- **Allemang, D. & Hendler, J.** (2011). *Semantic Web for the Working Ontologist*

## License

This module is part of ggen and is licensed under the MIT License.

## Contributors

- Implementation based on category theory research
- Inspired by categorical semantics of programming languages
- Contributions welcome!

---

*"Category theory is the mathematics of mathematics."* - Barry Mazur
