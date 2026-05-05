# Reference: Ontology Packs & Metadata Configuration

This document provides a technical reference for defining Ontology Packs in the `ggen` ecosystem. Ontology packs are specialized template packs that bundle RDF/OWL ontologies alongside code generation templates.

## Structure of `OntologyPackMetadata`

The configuration schema (typically defined in `ggen.toml` or `package.toml`) extends the base `GpackMetadata` with an `ontology` block containing the following structures:

### `OntologyConfig`

The top-level configuration for ontology capabilities within the pack.

| Field | Type | Description |
|-------|------|-------------|
| `ontologies` | `Vec<OntologyDefinition>` | A list of ontologies included in this pack. |
| `default_namespace` | `Option<String>` | The default namespace used for generated code. |
| `targets` | `Vec<CodeGenTarget>` | Target languages and templates supported by this pack. |
| `template_paths` | `Map<String, String>` | Language-specific paths for templates. |
| `prefixes` | `Map<String, String>` | Common RDF prefixes used within the pack context. |

### `OntologyDefinition`

Defines an individual ontology vocabulary (e.g., `schema.org`, `foaf`).

| Field | Type | Description |
|-------|------|-------------|
| `id` | `String` | The internal identifier (e.g., `"schema.org"`). |
| `name` | `String` | Human-readable name. |
| `version` | `String` | Ontology version. |
| `namespace` | `String` | Base URI for the schema. |
| `file_path` | `String` | Path to the ontology file relative to the pack. |
| `format` | `OntologyFormat` | File format of the ontology. |
| `description` | `String` | Description of the vocabulary. |
| `spec_url` | `Option<String>` | URL to the official specification. |

### Supported `OntologyFormat` Values

The system supports the following serialized ontology formats:
- `turtle` (`.ttl`)
- `rdfxml` (`.rdf`, `.xml`)
- `ntriples` (`.nt`)
- `jsonld` (`.jsonld`)

### `CodeGenTarget`

Maps target programming languages to template outputs.

| Field | Type | Description |
|-------|------|-------------|
| `language` | `String` | Target language (e.g., `"typescript"`, `"rust"`). |
| `features` | `Vec<String>` | Feature flags to enable in generation (e.g., `["zod", "utilities"]`). |
| `template_path` | `String` | Path to the `.tera` template used for generation. |
| `output_pattern` | `String` | Destination file pattern (e.g., `"{class_name}.ts"`). |

## Example `ggen.toml` Configuration

```toml
[ontology]
default_namespace = "https://schema.org/"

[[ontology.ontologies]]
id = "schema.org"
name = "Schema.org"
version = "15.0"
namespace = "https://schema.org/"
file_path = "ontologies/schema-org.ttl"
format = "turtle"
description = "Schema.org vocabulary"

[[ontology.targets]]
language = "typescript"
features = ["zod", "graphql"]
template_path = "templates/ts/model.tera"
output_pattern = "{class_name}.ts"
```
