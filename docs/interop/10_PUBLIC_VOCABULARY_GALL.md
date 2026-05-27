# Public Vocabulary GALL Specification

## 1. Purpose
Open Ontologies and public vocabularies act as a survivability checkpoint (GALL) for the projections emitted by ggen. They DO NOT provide the operational authority (`O*`); they ensure that the output of Genesis-bearing parts can be understood by the outside world.

## 2. Public Vocabulary Rule
**Open Ontologies validates survivability. It does not become `O*`.**
To ensure interoperability, the ggen projection layer MUST map local `RelationPage` predicates to standardized, publicly resolvable URIs (e.g., W3C PROV, FOAF, DCAT, schema.org) whenever projecting to RDF or OCEL.

## 3. No Private Predicate Authority in Public Evidence
When emitting public evidence (e.g., an audit receipt or external projection), the use of arbitrary "private predicates" (e.g., `urn:mycompany:custom_status`) is strictly forbidden if a public equivalent exists. This prevents vendor lock-in and namespace laundering.

## 4. Namespace Laundering Risks
ggen must not allow parts to invent new structural semantics on the fly. If a part constructs consequence, that consequence must map to a known, registered vocabulary shape.

## 5. SHACL Validation Checkpoint
The projected output MUST pass SHACL (Shapes Constraint Language) validation. This proves that the local `Construct8` construction mathematically aligns with the required public shapes.

## 6. External Tool Survivability
If the ggen projection cannot be parsed by Apache Jena, QLever, or standard Python `rdflib`, it is considered a failure of the projection membrane.

## 7. Relationship to Genesis
Genesis operates on local, 8-bit IDs inside a `RelationPage`. Genesis knows nothing about URIs, RDF, or SHACL.

## 8. Relationship to ggen
ggen is the sole owner of the mapping between the local 8-bit IDs and the global Public Vocabularies.

## 9. Boundary Validation Table

| Boundary artifact | Public vocabularies used | Validator | Pass criteria | Failure mode |
| :--- | :--- | :--- | :--- | :--- |
| **PROV-O Export** | `prov:Activity`, `prov:Entity` | SHACL / Jena | Parses & Conforms | Namespace error |
| **OCEL 2.0 Export** | OCEL Core Schema | `wasm4pm` / `pictl` | Conformance | Missing mandatory fields |
| **DCAT Catalog** | `dcat:Dataset` | DCAT Validator | Parses | Invalid URI scheme |
| **Generic RDF** | `rdf:type`, `rdfs:label` | QLever | Loads & Queries | Syntax error |

## 10. Definition of Done
The Public Vocabulary GALL is DONE when an automated CI pipeline can take a raw `Construct8` packet, project it via ggen to Turtle, and successfully run a strict SHACL validation suite against it using only public W3C ontologies.

| Status | Component | File/Artifact Evidence |
| :--- | :--- | :--- |
| **PARTIAL** | SHACL Refusal Projection | `crates/ggen-projection/src/lib.rs` |
| **MISSING** | Automated SHACL CI Gate | Needs implementation |
