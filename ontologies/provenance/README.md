# W3C Provenance Ontologies & Vocabularies

**Downloaded:** 2026-06-23  
**Location:** `/home/user/ggen/ontologies/provenance/`  
**Total Size:** 2.9 MB across 18 files

## Overview

This directory contains canonical W3C provenance ontologies and supporting vocabularies for RDF-based code generation pipelines. All files downloaded directly from official W3C namespace URIs.

## Core PROV-O (Provenance Ontology)

The PROV-O ontology is W3C's standard for representing provenance information in RDF.

**Files:**
- **prov-o.rdf** (167 KB) — RDF/XML format (current canonical version)
- **prov-o.ttl** (111 KB) — Turtle format (current canonical version)
- **prov-o-20130430.rdf** (454 KB) — REC 2013-04-30 version (historical reference)
- **prov-n.txt** (769 B) — PROV-N notation (abstract syntax)

**Source:** https://www.w3.org/ns/prov#

**Key Classes:**
- `prov:Entity` — Things that can change (artifacts, documents, state)
- `prov:Activity` — Actions that produce/modify entities (compile, test, generate)
- `prov:Agent` — Responsible parties (people, software, organizations)
- `prov:Bundle` — Collections of provenance statements

**Key Relations:**
- `prov:wasGeneratedBy` — Entity ← Activity
- `prov:used` — Activity → Entity
- `prov:wasAttributedTo` — Entity → Agent
- `prov:wasAssociatedWith` — Activity → Agent
- `prov:wasDerivedFrom` — Entity ← Entity (dependency)
- `prov:wasInformedBy` — Activity ← Activity (sequencing)

## PROV Specifications (Normative Documents)

**prov-constraints.txt** (279 KB)
- Official PROV constraints and validity rules
- 42 constraints defining lawful provenance structures
- Required reading for receipt validation

**prov-sem.txt** (1.1 MB)
- PROV formal semantics
- First-order logic interpretation of provenance
- Defines inference rules for derived triples

## Supporting Vocabularies

### Temporal (OWL-Time)
**Files:** owl-time.rdf (133 KB), owl-time.ttl (100 KB)
- **Use:** Represent timestamps, intervals, and temporal ordering
- **Key Classes:** `time:Instant`, `time:Interval`, `time:TemporalEntity`
- **Integration:** Timestamp every PROV event with ISO 8601 dates

### Data Catalog (DCAT)
**Files:** dcat.rdf (243 KB), dcat.ttl (196 KB)
- **Use:** Catalog datasets, distributions, and versioning
- **Key Classes:** `dcat:Dataset`, `dcat:Distribution`, `dcat:Catalog`
- **Integration:** Track generated artifacts as DCAT datasets

### Knowledge Organization (SKOS)
**Files:** skos.rdf (29 KB), skos.ttl (653 B)
- **Use:** Semantic concept hierarchies (workflow patterns, error categories)
- **Key Classes:** `skos:Concept`, `skos:ConceptScheme`
- **Integration:** Define pattern taxonomies and classifications

### Contact Information
**Files:** vcard.rdf/ttl (21 KB each), foaf.rdf (273 B)
- **vcard:** Structured contact details (VCard standard)
- **foaf:** Social networks, agent properties (FOAF)
- **Integration:** Represent author/maintainer/reviewer contacts

### Language Codes
**File:** iso639-1.rdf (21 KB)
- **Use:** Language/locale specifications
- **Integration:** Localize documentation, set lang tags on strings

### RDF/OWL Foundation
**Files:** rdfs.rdf (10 KB), owl.rdf (32 KB)
- **Use:** RDF Schema and OWL vocabulary definitions
- **Note:** Usually imported automatically by other ontologies

---

## Integration with ggen

### Use Case 1: Artifact Provenance
```turtle
# Example: Track code generation pipeline
example:artifact-v1 a prov:Entity ;
  prov:wasGeneratedBy example:generate-activity ;
  prov:wasDerivedFrom example:ontology-input ;
  prov:wasAttributedTo example:ggen-cli .

example:generate-activity a prov:Activity ;
  prov:used example:ontology-input ;
  prov:wasAssociatedWith example:ggen-cli ;
  prov:startedAtTime "2026-06-23T05:48:00Z"^^xsd:dateTime ;
  prov:endedAtTime "2026-06-23T05:48:05Z"^^xsd:dateTime .
```

### Use Case 2: Receipt Validation
- Each `.ggen/receipts/*.json` can be converted to PROV RDF
- Validate against `prov-constraints` rules
- Build provenance chains across multiple generations

### Use Case 3: Process Mining
- Convert execution logs to OCEL + PROV
- Discover actual vs. intended process models
- Detect anomalies (missing stages, out-of-order execution)

---

## File Manifest

| Category | Files | Total Size |
|----------|-------|-----------|
| PROV-O Ontology | 4 files | 633 KB |
| PROV Specs | 2 files | 1.4 MB |
| Supporting Vocabs | 12 files | ~900 KB |
| **Total** | **18 files** | **2.9 MB** |

---

## Verification

All downloads from canonical W3C URIs:

```bash
# Verify prov-o.rdf matches official W3C version
curl -I https://www.w3.org/ns/prov.rdf

# Parse as valid RDF
rapper -i rdfxml prov-o.rdf | wc -l  # Should show ~10K triples

# Validate Turtle syntax
rapper -i turtle prov-o.ttl | wc -l
```

---

## References

- **PROV-O Specification:** https://www.w3.org/TR/prov-o/
- **PROV Constraints:** https://www.w3.org/TR/prov-constraints/
- **PROV Semantics:** https://www.w3.org/TR/prov-sem/
- **OWL-Time:** https://www.w3.org/2006/time
- **DCAT:** https://www.w3.org/ns/dcat
- **SKOS:** https://www.w3.org/2004/02/skos/core

---

## Next Steps

1. **Load into Oxigraph:** Import RDF into ggen's triple store
2. **SPARQL Queries:** Define provenance queries for audit/compliance
3. **Receipt Mapping:** Convert JSON receipts to PROV RDF
4. **Event Log Derivation:** Extract OCEL events from PROV graphs
5. **Process Conformance:** Validate execution against PROV constraints

