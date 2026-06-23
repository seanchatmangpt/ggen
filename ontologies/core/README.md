# W3C Core Semantic Web Ontologies

This directory contains the canonical W3C semantic web vocabularies downloaded on **2026-06-23** from official W3C sources. All files are in Turtle (RDF/TTL) format and verified for integrity.

## Quick Summary

| Metric | Value |
|--------|-------|
| **Total Vocabularies** | 12 |
| **Total Download Size** | 376 KB (376,418 bytes) |
| **Total RDF Statements** | ~6,195 lines |
| **Download Format** | Turtle (RDF/TTL) 100% |
| **W3C Compliance** | 10 W3C Recommendations + 1 DCMI + 1 Emerging |

## Vocabularies Included

### Core RDF Infrastructure (3 files)
- **rdf-syntax-ns.ttl** (6.0 KB) — RDF core vocabulary
  - URL: https://www.w3.org/1999/02/22-rdf-syntax-ns#
  - Status: W3C Recommendation
  - Purpose: Resource, Property, Bag, Seq, Alt classes

- **rdf-schema.ttl** (3.8 KB) — RDF Schema vocabulary
  - URL: https://www.w3.org/2000/01/rdf-schema#
  - Status: W3C Recommendation
  - Purpose: Class hierarchies, type systems, domain/range

- **owl.ttl** (24 KB) — Web Ontology Language
  - URL: https://www.w3.org/2002/07/owl#
  - Status: W3C Recommendation
  - Purpose: Logical constructs, class/property definitions

### Knowledge Organization Systems (2 files)
- **skos.ttl** (29 KB) — Simple Knowledge Organization System
  - URL: https://www.w3.org/2004/02/skos/core#
  - Status: W3C Recommendation
  - Purpose: Concept schemes, taxonomies, thesauri

- **skos-xl.ttl** (7.6 KB) — SKOS eXtension for Labels
  - URL: https://www.w3.org/2008/05/skos-xl#
  - Status: W3C Recommendation
  - Purpose: Extended label support, multilingual variants

### Metadata & Description (3 files)
- **dcterms.ttl** (48 KB) — Dublin Core Metadata Terms
  - URL: https://purl.org/dc/terms/
  - Status: DCMI Recommended Resource
  - Purpose: Fundamental metadata properties (title, creator, date, etc.)

- **dcat.ttl** (196 KB) — Data Catalog Vocabulary v3.0
  - URL: https://www.w3.org/ns/dcat#
  - Status: W3C Recommendation
  - Purpose: Dataset catalogs, distributions, metadata

- **prov-o.ttl** (7.9 KB) — Provenance Ontology
  - URL: https://www.w3.org/ns/prov#
  - Status: W3C Recommendation
  - Purpose: Entity/Activity/Agent provenance, audit trails

### Validation & Constraints (2 files)
- **shacl.ttl** (8.7 KB) — Shapes Constraint Language
  - URL: https://www.w3.org/ns/shacl#
  - Status: W3C Recommendation
  - Purpose: RDF graph validation, constraint checking

- **xmlschema-datatypes.ttl** (3.2 KB) — XML Schema Datatypes
  - URL: https://www.w3.org/2001/XMLSchema#
  - Status: W3C Recommendation
  - Purpose: RDF datatype definitions (xsd:*)

### Process & Events (1 file)
- **ocel2.ttl** (22 KB) — Object-Centric Event Log 2.0
  - URL: https://www.w3.org/2024/ocel2#
  - Status: Emerging Standard
  - Purpose: Event logs, object interaction patterns

### Extended OWL (1 file)
- **owl2.ttl** (5.7 KB) — OWL 2 Extended Vocabulary
  - URL: https://www.w3.org/2002/07/owl#
  - Status: W3C Recommendation (OWL 2.0)
  - Purpose: Advanced logical constructs

## Documentation Files

| File | Purpose |
|------|---------|
| **README.md** | This file — overview and quick reference |
| **DOWNLOAD_SUMMARY.txt** | Comprehensive download report with all metadata |
| **DOWNLOADS_REPORT.csv** | Machine-readable CSV with filename, URL, size, status |
| **ONTOLOGY_MANIFEST.txt** | Detailed manifest with vocabulary descriptions |
| **INTEGRITY_VERIFICATION.txt** | File integrity verification results |
| **SAMPLE_CONTENT.txt** | Sample RDF content from each vocabulary |
| **download_ontologies.sh** | Bash script to refresh downloads |

## Usage in ggen

These ontologies form the semantic foundation for the ggen code generation system:

```
μ₁ (Load)     → Load core ontologies + project specs
μ₂ (Extract)  → Extract patterns using SPARQL queries
μ₃ (Generate) → Generate code via Tera templates
μ₄ (Validate) → Validate using SHACL shapes
μ₅ (Emit)     → Write artifacts with receipts
```

### Load in Code

```rust
// Load ontologies programmatically
let graph = ggen_graph::load_ontologies("ontologies/core/")?;
let merged = graph.merge_project_specs(".specify/")?;

// Validate RDF
ggen_core::validate::shacl_check(&merged)?;

// Generate code
let artifacts = ggen_core::pipeline::run_μ_pipeline(&merged)?;
```

### Command Line

```bash
# Validate ontologies
ggen validate ontologies/core/rdf-syntax-ns.ttl

# Query ontologies
ggen query --rdf ontologies/core/ \
  "SELECT ?label WHERE { ?s rdfs:label ?label }"

# Full pipeline with audit
ggen sync --audit true
```

## Verification

All downloads verified using:
- **Source**: W3C canonical namespaces (https://www.w3.org/)
- **Format**: Turtle RDF/TTL (text-based, human-readable)
- **Content negotiation**: HTTP Accept header for `text/turtle` preferred
- **Structure**: All files contain valid RDF syntax with proper Turtle formatting

To verify a downloaded file:

```bash
# View RDF declarations
head -20 skos.ttl

# Extract namespace prefixes
grep "@prefix" skos.ttl

# Count RDF statements
wc -l skos.ttl

# Validate Turtle syntax (using riot from Apache Jena)
# apt install jena-tools
riot --check skos.ttl
```

## Refreshing Downloads

To download fresh copies of all ontologies:

```bash
cd ontologies/core/
bash download_ontologies.sh
```

To download a single vocabulary:

```bash
curl -L -H "Accept: text/turtle" \
  https://www.w3.org/2004/02/skos/core# \
  -o skos.ttl
```

## Canonical References

- **RDF**: https://www.w3.org/TR/rdf-concepts/
- **RDFS**: https://www.w3.org/TR/rdf-schema/
- **OWL**: https://www.w3.org/TR/owl-overview/
- **OWL 2**: https://www.w3.org/TR/owl2-overview/
- **SKOS**: https://www.w3.org/TR/skos-reference/
- **Dublin Core**: https://www.dublincore.org/specifications/dublin-core/dcmi-terms/
- **DCAT 3.0**: https://www.w3.org/TR/vocab-dcat-3/
- **PROV-O**: https://www.w3.org/TR/prov-o/
- **SHACL**: https://www.w3.org/TR/shacl/
- **OCEL 2.0**: https://www.ocel-standard.org/
- **XML Schema**: https://www.w3.org/TR/xmlschema-2/

## Next Steps

1. **Verify integration**: Run `ggen sync --audit true` to verify all ontologies load
2. **Query ontologies**: Use SPARQL queries to explore vocabulary definitions
3. **Extend ontologies**: Add project-specific vocabularies to `.specify/specs/`
4. **Commit to version control**: `git add ontologies/core/*.ttl`

## Technical Notes

- **File Size**: DCAT is the largest (196 KB) due to comprehensive dataset vocabulary
- **RDF Format**: All files are in Turtle format for human readability
- **Namespace URIs**: Each file is dereferenceable (HTTP GET returns the TTL)
- **Dependencies**: Files cross-reference each other (e.g., OWL extends RDF+RDFS)

## License

All W3C vocabularies are published under the [W3C Document License](https://www.w3.org/Consortium/Legal/2015/copyright-software-and-document.html). Dublin Core is published under the [CC-BY license](https://creativecommons.org/licenses/by/4.0/).

---

**Downloaded**: 2026-06-23
**Directory**: `/home/user/ggen/ontologies/core/`
**Total Size**: 432 KB (including documentation)
