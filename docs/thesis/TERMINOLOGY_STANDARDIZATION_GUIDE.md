# Terminology Standardization Guide
## PhD Thesis: Ontology-Driven Code Generation

**Purpose**: This document establishes canonical terminology for consistent usage throughout the thesis. All variants listed should be replaced with the standardized term.

**Last Updated**: 2026-01-06

---

## 1. RDF Ontology vs. RDF Specification vs. Semantic Model

**TERM**: RDF ontology
**VARIANTS TO ELIMINATE**: RDF specification, semantic model, ontological model, RDF schema (when referring to user-defined ontologies)
**DEFINITION**: A formal, explicit specification of a shared conceptualization expressed using RDF (Resource Description Framework), typically including classes, properties, and relationships defined using RDFS and OWL vocabularies.
**CONTEXT**: Use "RDF ontology" when referring to the graph-based knowledge representation. Use "ontology specification" when referring to the written/serialized form (Turtle, RDF/XML).
**EXAMPLE**: "The RDF ontology defines entity classes and their properties, which are then queried using SPARQL to extract data for code generation."
**REASON**: "RDF ontology" is the established W3C terminology. "Semantic model" is too vague, and "RDF specification" conflates the ontology with the RDF standard itself.
**REFERENCES**: Chapters 1, 2, 3, 4, 5, 6, 7, 8, 9

---

## 2. API Contract vs. API Specification vs. API Definition

**TERM**: API contract (first mention), then "contract"
**VARIANTS TO ELIMINATE**: API specification (except when referring to OpenAPI Specification specifically), API definition, interface specification
**DEFINITION**: A formal agreement defining the structure, behavior, and constraints of an API's request/response interactions, typically encompassing endpoints, data schemas, and validation rules.
**CONTEXT**: Use "API contract" to emphasize the binding nature of the specification between provider and consumer. Use "OpenAPI specification" when referring specifically to the OpenAPI format.
**EXAMPLE**: "The API contract is generated deterministically from the RDF ontology, ensuring that all artifacts (TypeScript types, validation schemas, documentation) remain synchronized."
**REASON**: "Contract" emphasizes the formal agreement aspect and is more precise than the generic "specification." It also distinguishes from "OpenAPI Specification" (the format) vs "API contract" (the semantic content).
**REFERENCES**: Chapters 1, 4, 5, 6, 7, 8, 9

---

## 3. Code Generation vs. Artifact Generation vs. Code Synthesis

**TERM**: Code generation (general), artifact generation (specific outputs)
**VARIANTS TO ELIMINATE**: Code synthesis, artifact synthesis, automated generation, programmatic generation
**DEFINITION**: The automated creation of source code or structured artifacts from higher-level specifications. Code generation refers broadly to the process; artifact generation refers to specific outputs (TypeScript files, OpenAPI YAML, validation schemas).
**CONTEXT**: Use "code generation" when discussing the overall process or methodology. Use "artifact generation" when referring to specific output files or types of outputs.
**EXAMPLE**: "Code generation from RDF ontologies produces multiple artifacts, including TypeScript interfaces, Zod validation schemas, and type guard functions."
**REASON**: "Code generation" is standard industry terminology. "Artifact generation" provides specificity when discussing particular outputs. "Synthesis" is unnecessarily academic and less precise.
**REFERENCES**: Chapters 1, 2, 3, 4, 5, 6, 7, 8

---

## 4. Type Guard vs. Type Predicate vs. Guard Function vs. Runtime Validator

**TERM**: Type guard (primary), type predicate (TypeScript-specific terminology)
**VARIANTS TO ELIMINATE**: Guard function, runtime validator, type checker, validation guard
**DEFINITION**: A runtime function that performs type checking and narrows the type of a value within a type system, typically using the TypeScript `value is Type` predicate syntax.
**CONTEXT**: Use "type guard" as the primary term. Use "type predicate" when specifically discussing the TypeScript `is` keyword syntax. Avoid "runtime validator" which conflates type checking with data validation.
**EXAMPLE**: "Type guards bridge the gap between compile-time types and runtime validation, using type predicates to inform the TypeScript compiler of type narrowing."
**REASON**: "Type guard" is the established TypeScript terminology. "Type predicate" is the syntax form. Other terms lack precision and are not standard in the TypeScript ecosystem.
**REFERENCES**: Chapters 6, 7, 8

---

## 5. Template Rendering vs. Template Expansion vs. Template Generation

**TERM**: Template rendering (all contexts)
**VARIANTS TO ELIMINATE**: Template expansion, template generation, template execution, template processing
**DEFINITION**: The process of combining a template (containing placeholders and logic) with contextual data to produce final output text, typically source code or configuration files.
**CONTEXT**: Use "template rendering" consistently for all Tera template operations. The term aligns with standard terminology in template engines (Jinja2, Tera, Handlebars).
**EXAMPLE**: "Template rendering proceeds in two phases: first, SPARQL queries execute against the RDF ontology; second, query results are passed to Tera templates for rendering into code artifacts."
**REASON**: "Rendering" is standard terminology in template engines and clearly distinguishes the process from code generation overall. "Expansion" is less precise, and "generation" creates ambiguity with "code generation."
**REFERENCES**: Chapters 3, 4, 5, 6, 7

---

## 6. Entity vs. Domain Object vs. Class vs. Resource

**TERM**: Entity (domain model), Class (RDF/OWL), Resource (RDF subject)
**VARIANTS TO ELIMINATE**: Domain object, data object, model object, business entity
**DEFINITION**: Entity refers to a domain concept (e.g., User, Post). Class refers to the RDF/OWL representation (owl:Class). Resource refers to any RDF subject identified by a URI.
**CONTEXT**: Use "entity" when discussing domain modeling and business logic. Use "class" when discussing RDF/OWL ontology structure. Use "resource" when discussing RDF graph topology or URIs.
**EXAMPLE**: "The User entity is represented as an owl:Class in the RDF ontology, with each user instance being a distinct RDF resource identified by a unique URI."
**REASON**: These terms have specific meanings in their respective contexts. Conflating them reduces precision. This standardization aligns with W3C terminology and domain-driven design principles.
**REFERENCES**: Chapters 1, 2, 4, 5, 6, 7, 8, 9

---

## 7. Specification vs. Specification Document vs. Spec vs. Contract

**TERM**: Specification (formal), spec (informal)
**VARIANTS TO ELIMINATE**: Specification document, spec document, specification artifact
**DEFINITION**: A precise, formal description of requirements, structure, or behavior. "Specification" is the full form used in formal contexts; "spec" is acceptable in informal contexts or when space is limited.
**CONTEXT**: Use "specification" in academic writing, section headings, and formal descriptions. Use "spec" in informal explanations, code comments, or when referring casually.
**EXAMPLE**: "The OpenAPI specification defines all API endpoints; from this spec, we generate client libraries and validation code."
**REASON**: "Specification" is the academically rigorous term. "Spec" is widely understood as an informal abbreviation. Avoid "specification document" as redundant.
**REFERENCES**: Chapters 1, 2, 4, 8, 9, 10

---

## 8. Deterministic vs. Determinism vs. Reproducible

**TERM**: Deterministic (adjective), determinism (noun); do NOT use "reproducible" as synonym
**VARIANTS TO ELIMINATE**: Reproducible generation, repeatable, consistent output
**DEFINITION**: Deterministic means that identical inputs always produce identical outputs, with no dependence on external state, randomness, or timing. Determinism is the property. "Reproducible" refers to the ability to recreate an environment/result, which is a related but distinct concept.
**CONTEXT**: Use "deterministic" when describing code generation behavior. Use "determinism" when discussing the property as a requirement. Do not use "reproducible" as a synonym—it implies replication across environments, not identical output from identical input.
**EXAMPLE**: "Deterministic code generation ensures that the same RDF ontology always produces byte-identical output artifacts, regardless of execution environment or timestamp."
**REASON**: Determinism is a stronger guarantee than reproducibility. Deterministic systems have no randomness; reproducible systems can be recreated but may have acceptable variation. Academic rigor requires this distinction.
**REFERENCES**: Chapters 1, 3, 8, 10; docs/thesis/deterministic-generation.md

---

## 9. Schema vs. Ontology vs. Model

**TERM**: Ontology (semantic/conceptual), schema (structural), model (conceptual)
**VARIANTS TO ELIMINATE**: Schema definition, ontology schema, data model (when referring to RDF)
**DEFINITION**: Ontology refers to formal, semantic knowledge representation with reasoning capabilities (OWL). Schema refers to structural data definitions (JSON Schema, XML Schema, database schemas). Model refers to abstract conceptual representations.
**CONTEXT**: Use "ontology" for RDF/OWL knowledge graphs. Use "schema" for structural validation (JSON Schema, database DDL). Use "model" for abstract concepts before formalization.
**EXAMPLE**: "The RDF ontology captures semantic relationships between entities; SHACL schemas validate conformance; the conceptual model informs both."
**REASON**: These terms have distinct meanings in their respective domains. "Ontology" implies formal semantics and reasoning; "schema" implies structure and validation; "model" implies abstraction. Conflating them reduces precision.
**REFERENCES**: Chapters 1, 2, 4, 6, 8

---

## 10. Validation Schema vs. Validator vs. Validation Rules

**TERM**: Validation schema (Zod definition), validator (function)
**VARIANTS TO ELIMINATE**: Validation rule set, validation object, schema validator
**DEFINITION**: A validation schema is a declarative definition of constraints (e.g., Zod schema object). A validator is an executable function that checks data against a schema.
**CONTEXT**: Use "validation schema" when referring to the Zod schema definition itself. Use "validator" when referring to the function that performs validation (e.g., `schema.parse()`).
**EXAMPLE**: "The validation schema defines constraints using Zod's fluent API; the validator function applies this schema to runtime data, throwing errors on violation."
**REASON**: This distinction separates declaration (schema) from execution (validator). It aligns with Zod documentation and functional programming principles.
**REFERENCES**: Chapters 5, 6, 7, 8

---

## 11. Artifact vs. Artifact Type vs. Output vs. Generated Code

**TERM**: Artifact (general output), artifact type (category distinction)
**VARIANTS TO ELIMINATE**: Output file, generated file, code artifact, generation output
**DEFINITION**: An artifact is any file or structured output produced by the code generation process. Artifact type distinguishes categories (e.g., TypeScript types vs. validation schemas vs. documentation).
**CONTEXT**: Use "artifact" when referring to generated outputs generically. Use "artifact type" when distinguishing between categories of outputs.
**EXAMPLE**: "Code generation produces multiple artifacts: TypeScript interfaces, Zod validation schemas, and OpenAPI specifications represent different artifact types."
**REASON**: "Artifact" is established terminology in software engineering for build outputs. "Artifact type" provides categorical precision when needed. Avoid redundant phrases like "code artifact."
**REFERENCES**: Chapters 1, 3, 4, 5, 6, 7, 8

---

## 12. SPARQL Query vs. SPARQL Pattern vs. Query Pattern

**TERM**: SPARQL query (complete query), pattern (WHERE clause pattern)
**VARIANTS TO ELIMINATE**: Query statement, SPARQL statement, graph pattern
**DEFINITION**: A SPARQL query is a complete query including PREFIX declarations, query form (SELECT/CONSTRUCT), WHERE clause, and modifiers (ORDER BY, LIMIT). A pattern is the graph pattern matching structure within the WHERE clause.
**CONTEXT**: Use "SPARQL query" when referring to the complete query text. Use "pattern" when discussing the graph pattern matching structure within WHERE clauses.
**EXAMPLE**: "The SPARQL query extracts entity metadata by matching patterns in the RDF graph, with the WHERE clause defining the triple patterns to match."
**REASON**: This distinction aligns with SPARQL specification terminology. "Query" is the complete statement; "pattern" is the matching structure. Conflating them reduces clarity.
**REFERENCES**: Chapters 2, 3, 4, 5, 6, 7

---

## 13. Triple vs. Statement vs. RDF Statement

**TERM**: Triple (singular), RDF triple (when emphasizing RDF context)
**VARIANTS TO ELIMINATE**: RDF statement (except when citing W3C specs), statement, fact
**DEFINITION**: A triple is the fundamental unit of RDF, consisting of subject, predicate, and object. "RDF triple" emphasizes the RDF context when needed for clarity.
**CONTEXT**: Use "triple" in most contexts. Use "RDF triple" when first introducing the concept or when distinguishing from other triple structures.
**EXAMPLE**: "Each RDF triple expresses a single fact about a resource; multiple triples combine to form a knowledge graph representing the domain ontology."
**REASON**: "Triple" is concise and universally understood in RDF contexts. "RDF triple" provides emphasis when needed. "Statement" is technically correct but less common in practice.
**REFERENCES**: Chapters 1, 2

---

## 14. Configuration vs. Configuration File vs. Config

**TERM**: Configuration (formal), config (informal)
**VARIANTS TO ELIMINATE**: Configuration file, configuration object, settings
**DEFINITION**: Configuration refers to the set of parameters, options, and settings that control system behavior. "Configuration" is the formal term; "config" is acceptable in informal contexts.
**CONTEXT**: Use "configuration" in formal writing and academic contexts. Use "config" in code comments, informal explanations, or when space is limited (e.g., file names: `config.yaml`).
**EXAMPLE**: "The system configuration is defined in YAML files; developers can modify config parameters to adjust template behavior."
**REASON**: "Configuration" is the academically rigorous term. "Config" is widely understood informally. Avoid verbose phrases like "configuration file" when "configuration" suffices.
**REFERENCES**: Chapters 3, 8

---

## 15. Consistency vs. Consistency Constraint vs. Consistency Check

**TERM**: Consistency (property), consistency constraint (SHACL), consistency checking (process)
**VARIANTS TO ELIMINATE**: Data consistency, schema consistency, validation consistency
**DEFINITION**: Consistency is the property that data conforms to defined constraints. A consistency constraint is a formal rule (SHACL shape). Consistency checking is the validation process.
**CONTEXT**: Use "consistency" when discussing the property. Use "consistency constraint" when referring to SHACL shapes or formal rules. Use "consistency checking" when describing validation processes.
**EXAMPLE**: "SHACL consistency constraints enforce that all User entities have valid email addresses; consistency checking validates instances against these constraints at runtime."
**REASON**: These distinctions separate the property (consistency), the specification (constraint), and the process (checking). This aligns with SHACL terminology and formal methods.
**REFERENCES**: Chapters 1, 2, 4, 6, 8, 9

---

## Additional Terminology Notes

### W3C Standards Capitalization
- Use "Resource Description Framework (RDF)" on first mention, "RDF" thereafter
- Use "SPARQL Protocol and RDF Query Language (SPARQL)" on first mention, "SPARQL" thereafter
- Use "Web Ontology Language (OWL)" on first mention, "OWL" thereafter
- Use "Shapes Constraint Language (SHACL)" on first mention, "SHACL" thereafter

### Format Specifications
- "Turtle" (capitalized) when referring to the RDF serialization format
- "JSON Schema" (capitalized) when referring to the specification
- "OpenAPI" or "OpenAPI Specification" (not "OpenAPI spec" in formal writing)

### Programming Language Terminology
- "TypeScript" (capitalized, one word)
- "JavaScript" (capitalized, one word)
- "Rust" (capitalized)
- "Python" (capitalized)

### File and Path References
- Use forward slashes (Unix-style) for all paths: `src/templates/entity.tera`
- Use backticks for file names in prose: `schema.ttl`
- Use code blocks for longer paths or file listings

---

## Implementation Checklist

**When editing the thesis, systematically:**
1. Search for each variant term
2. Replace with canonical term
3. Verify context appropriateness
4. Check cross-references remain valid
5. Update glossary if needed

**Tools for consistency checking:**
```bash
# Search for variant terms
grep -r "semantic model" thesis/*.tex
grep -r "code synthesis" thesis/*.tex
grep -r "reproducible generation" thesis/*.tex

# Verify canonical usage
grep -r "RDF ontology" thesis/*.tex | wc -l
grep -r "API contract" thesis/*.tex | wc -l
grep -r "type guard" thesis/*.tex | wc -l
```

---

## References

- W3C RDF 1.1 Primer: https://www.w3.org/TR/rdf11-primer/
- W3C SPARQL 1.1 Query Language: https://www.w3.org/TR/sparql11-query/
- W3C OWL 2 Web Ontology Language: https://www.w3.org/TR/owl2-overview/
- W3C SHACL Shapes Constraint Language: https://www.w3.org/TR/shacl/
- TypeScript Documentation: https://www.typescriptlang.org/docs/
- Zod Documentation: https://zod.dev/

---

**Approval**: This terminology guide should be reviewed and approved by thesis committee before final submission.

**Compliance**: All thesis chapters, appendices, and supplementary materials must conform to this standardization.
