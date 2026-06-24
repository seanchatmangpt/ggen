# Provenance Ontologies & Vocabularies - Downloaded 2026-06-23

Canonical W3C sources for PROV-O and related provenance vocabularies.

## Core Provenance (PROV-O)

| Filename | URL | Version | Size | Format | Lines |
|----------|-----|---------|------|--------|-------|
| `prov-o.rdf` | https://www.w3.org/ns/prov.rdf | Current | 167 KB | RDF/XML | 2,288 |
| `prov-o.ttl` | https://www.w3.org/ns/prov.ttl | Current | 111 KB | Turtle | 2,466 |
| `prov-o-20130430.rdf` | https://www.w3.org/TR/2013/REC-prov-o-20130430/ | REC 2013-04-30 | 454 KB | RDF/XML | 10,611 |
| `prov-n.txt` | https://www.w3.org/ns/prov.n | Current | 769 B | PROV-N Notation | 16 |

## PROV Specifications (Technical Documents)

| Filename | URL | Type | Size | Lines |
|----------|-----|------|------|-------|
| `prov-constraints.txt` | https://www.w3.org/TR/prov-constraints/ | Specification | 279 KB | 5,486 |
| `prov-sem.txt` | https://www.w3.org/TR/prov-sem/ | Specification | 1.1 MB | 4,432 |

## Supporting Vocabularies

### Agents & Contact Information
| Filename | URL | Size | Format |
|----------|-----|------|--------|
| `foaf.rdf` | https://xmlns.com/foaf/spec/foaf.rdf | 273 B | RDF/XML |
| `vcard.rdf` | https://www.w3.org/2006/vcard/ns-2.76.rdf | 21 KB | RDF/XML |
| `vcard.ttl` | https://www.w3.org/2006/vcard/ns-2.76.ttl | 21 KB | Turtle |

### Temporal Information (OWL-Time)
| Filename | URL | Size | Format |
|----------|-----|------|--------|
| `owl-time.rdf` | https://www.w3.org/2006/time.rdf | 133 KB | RDF/XML |
| `owl-time.ttl` | https://www.w3.org/2006/time.ttl | 100 KB | Turtle |

### Data Catalog (DCAT)
| Filename | URL | Size | Format |
|----------|-----|------|--------|
| `dcat.rdf` | https://www.w3.org/ns/dcat.rdf | 243 KB | RDF/XML |
| `dcat.ttl` | https://www.w3.org/ns/dcat.ttl | 196 KB | Turtle |

### Knowledge Organization (SKOS)
| Filename | URL | Size | Format |
|----------|-----|------|--------|
| `skos.rdf` | https://www.w3.org/2004/02/skos/core.rdf | 29 KB | RDF/XML |
| `skos.ttl` | https://www.w3.org/2004/02/skos/core.ttl | 653 B | Turtle |

### Language Codes (ISO 639-1)
| Filename | URL | Size | Format |
|----------|-----|------|--------|
| `iso639-1.rdf` | https://www.w3.org/ns/language-codes.rdf | 21 KB | RDF/XML |

### RDF/OWL Foundation
| Filename | URL | Size | Format |
|----------|-----|------|--------|
| `rdfs.rdf` | https://www.w3.org/1999/02/22-rdf-syntax-ns.rdf | 10 KB | RDF/XML |
| `owl.rdf` | https://www.w3.org/2002/07/owl.rdf | 32 KB | RDF/XML |

## Total Size
**2.9 MB** across 18 files

## W3C Sources
All files downloaded from official W3C canonical URIs. These are the authoritative versions for provenance ontologies.

## Usage in ggen
These vocabularies integrate with ggen's RDF-based code generation pipeline:
- **PROV-O** — Process history, artifact provenance, state transitions
- **OWL-Time** — Temporal constraints and sequencing
- **DCAT** — Dataset/artifact cataloging
- **FOAF/VCard** — Agent/person metadata
- **SKOS** — Concept hierarchies for workflow patterns
- **ISO 639-1** — Language/locale codes

## Download Verification
To verify these downloads match canonical sources:
```bash
cd /home/user/ggen/ontologies/provenance
# Compare file sizes with official W3C versions
curl -I https://www.w3.org/ns/prov.rdf
```
