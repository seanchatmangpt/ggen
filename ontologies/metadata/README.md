# Dublin Core Ontologies

Canonical RDF/Turtle ontologies from the Dublin Core Metadata Initiative (DCMI).

## Quick Reference

| File | Namespace | Size | Lines | Type |
|------|-----------|------|-------|------|
| `dublin-core-terms.ttl` | http://purl.org/dc/terms/ | 29 KB | 560 | Extended terms (48+ properties) |
| `dublin-core-elements-1.1.ttl` | http://purl.org/dc/elements/1.1/ | 5.7 KB | 138 | Original 15 elements |
| `dublin-core-type-vocabulary.ttl` | http://purl.org/dc/dcmitype/ | 8.3 KB | 139 | Resource types (12 classes) |

## About Dublin Core

**Dublin Core Metadata Initiative (DCMI)** provides a standardized vocabulary for describing resources across all domains. The metadata terms enable discovery, semantic interoperability, and cross-domain resource sharing.

### Three Namespace Levels

1. **Dublin Core Elements (dc:)** — 15 basic properties for resource description
   - Original standard from 1998
   - Simple, essential metadata
   - Backward compatible

2. **Dublin Core Terms (dcterms:)** — 48+ extended properties
   - Released 2012, updated 2020
   - Adds refinements and relationships
   - Comprehensive metadata modeling

3. **DCMI Type Vocabulary (dcmitype:)** — 12 resource type classes
   - General taxonomy for resource types
   - Used with dcterms:type
   - Includes: Collection, Dataset, Image, Sound, Software, Text, etc.

## Usage Examples

### Load in ggen

```toml
# ggen.toml
[ontology]
imports = [
    "ontologies/metadata/dublin-core-terms.ttl",
    "ontologies/metadata/dublin-core-type-vocabulary.ttl"
]
```

### SPARQL Query

```sparql
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX dcmitype: <http://purl.org/dc/dcmitype/>

SELECT ?title ?creator ?type
WHERE {
    ?resource 
        dcterms:title ?title ;
        dcterms:creator ?creator ;
        dcterms:type ?type .
}
```

### RDF Triples

```turtle
<http://example.org/resource/123>
    dcterms:title "Sample Document" ;
    dcterms:creator <http://example.org/person/alice> ;
    dcterms:issued "2024-01-15"^^xsd:date ;
    dcterms:type dcmitype:Text ;
    dcterms:language "en" ;
    dcterms:rights "CC-BY-4.0" .
```

## File Formats

All files are in **Turtle/TTL** format:
- **Extension:** `.ttl`
- **MIME Type:** `text/turtle`
- **Parser:** Any RDF tool (Oxigraph, Jena, RDF4J, etc.)

## Standards & Validation

- ✓ Valid RDF/Turtle syntax
- ✓ W3C Turtle standard
- ✓ DCMI official vocabulary
- ✓ Compatible with Oxigraph, ggen, and all RDF processors
- ✓ Public domain / CC0 licensed

## Key Ontology Properties

### Dublin Core Terms (dcterms:)

**Basic Metadata:**
- `title`, `description`, `identifier`, `language`

**Attribution:**
- `creator`, `contributor`, `publisher`, `rights`, `rightsHolder`

**Dates:**
- `created`, `issued`, `modified`, `dateAccepted`, `dateCopyrighted`

**Relationships:**
- `relation`, `isPartOf`, `hasPart`, `isVersionOf`, `requires`

**Coverage:**
- `subject`, `coverage`, `spatial`, `temporal`

**Quality:**
- `conformsTo`, `provenance`, `educationLevel`, `instructionalMethod`

### Dublin Core Type Classes (dcmitype:)

```
Text, Image, Sound, MovingImage, InteractiveResource, 
Collection, Dataset, Event, PhysicalObject, Software, 
Service, StillImage
```

## Integration Notes

### With ggen Pipeline
- Include in `.specify/specs/*/` for schema-driven code generation
- Use in SPARQL queries for metadata extraction
- Apply in Tera templates for documentation generation

### With RDF Processors
- Oxigraph: Direct TTL import
- SHACL: Use Dublin Core shapes for validation
- SPARQL: Query across metadata domains

### With Linked Data
- All IRIs resolve via http://purl.org/
- Archive backup: https://purl.archive.org/
- Dereferenceble RDF at namespace URIs

## References

- **DCMI Home:** https://www.dublincore.org/
- **DCMI Terms:** https://www.dublincore.org/specifications/dublin-core/dcmi-terms/
- **DCMI Namespace:** https://www.dublincore.org/specifications/dublin-core/dcmi-namespace/
- **Turtle Spec:** https://www.w3.org/TR/turtle/
- **GitHub:** https://github.com/dcmi

## Download Details

| Metadata | Value |
|----------|-------|
| Downloaded | 2026-06-23 |
| Source | DCMI Official (dublincore.org) |
| Version | 2020-01-20 (latest snapshot) |
| Format | Turtle/TTL RDF |
| Total Size | 43.3 KB |
| Status | Canonical, production-ready |

---

See **DOWNLOAD_REPORT.md** for detailed information about each file, canonical sources, and verification details.
