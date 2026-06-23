# Bibliographic & Library Ontologies Collection

Official RDF ontologies for bibliographic and publication metadata, downloaded from canonical sources.

## Quick Reference

| Ontology | File | Size | Use Case |
|----------|------|------|----------|
| **BIBO** | `bibo.rdf` | 52 KB | Publications, articles, books |
| **MADS** | `mads.rdf` | 90 KB | Authority data, subject headings |
| **BIBFRAME** | `bibframe.rdf` | 13 KB | Modern library framework (LC) |
| **DCTERMS** | `dcterms.rdf` | 665 KB | General metadata (foundation) |
| **Schema.org** | `schema-book.jsonld` | 207 KB | Web/SEO markup |
| **PROV** | `prov.owl` | 454 KB | Provenance and versioning |

## Files in This Directory

```
.
├── README.md                 # This file
├── MANIFEST.md              # Detailed ontology descriptions
├── DOWNLOAD_REPORT.md       # Technical file inventory and verification
├── bibo.rdf                 # Bibliographic Ontology
├── mads.rdf                 # Library of Congress Metadata Authority Description Schema
├── bibframe.rdf             # Library of Congress Bibliographic Framework
├── dcterms.rdf              # Dublin Core Terms (foundation)
├── schema-book.jsonld       # Schema.org JSON-LD context
└── prov.owl                 # W3C Provenance Ontology
```

## Key Information

**Download Date:** 2026-06-23  
**Total Size:** 1.5 MB  
**Format Mix:** RDF/XML, Turtle, JSON-LD, RDFa, W3C HTML  
**Authority:** Library of Congress, Dublin Core Initiative, W3C, Schema.org  
**Verification:** All files downloaded unmodified from official sources  

## Quick Start

### Load in a Triple Store
```bash
# Using Oxigraph (ggen's default RDF backend)
oxigraph_server --location data.oxigraph &
curl -X POST http://localhost:7878/store \
  -d @bibo.rdf \
  -H "Content-Type: application/rdf+xml"
```

### Querying SPARQL
```bash
# Example: Find all Book classes
SELECT ?book WHERE {
  ?book rdfs:subClassOf <http://purl.org/ontology/bibo/Book> .
}
```

### Using with ggen
```bash
# Copy ontologies to ggen specification directory
cp *.rdf /path/to/project/.specify/ontologies/

# Reference in ggen.toml
[sources]
ontology = ".specify/ontologies/bibo.rdf"
```

## Ontology Relationships

```
BIBFRAME (Library systems)
  ├─ imports: DCTERMS
  ├─ aligns with: MADS
  └─ can layer: PROV

BIBO (Publications)
  ├─ imports: DCTERMS
  └─ can layer: PROV

Schema.org (Web/SEO)
  ├─ overlaps: BIBO, BIBFRAME
  └─ imports: DCTERMS

DCTERMS (Foundation - used by all)
  └─ base vocabulary for metadata

PROV (Generic - applies to all)
  └─ orthogonal layer for provenance tracking
```

## Use Case Recommendations

1. **Library Management Systems:** BIBFRAME + MADS + DCTERMS
2. **Academic Publishing:** BIBO + DCTERMS + PROV
3. **Web Discovery/SEO:** Schema.org + DCTERMS
4. **Authority Control:** MADS (with BIBFRAME integration)
5. **Version/Derivation Tracking:** PROV (any of above)

## Technical Details

See `DOWNLOAD_REPORT.md` for:
- Detailed file inventory with namespaces
- Format specifications (RDF/XML, Turtle, JSON-LD, RDFa)
- Key classes and properties per ontology
- Integration paths and dependencies
- Version information and validation details

See `MANIFEST.md` for:
- Ontology descriptions and purposes
- Failed download attempts and reasons
- Relationship map
- Usage recommendations

## Verification

All files are authentic, unmodified downloads from official sources:

- **BIBO, DCTERMS:** Purl.org / Open Knowledge Foundation
- **MADS, BIBFRAME:** Library of Congress (Official)
- **Schema.org:** Schema.org Collaborative
- **PROV:** W3C Technical Recommendation

✓ No files were edited, truncated, or altered.  
✓ All namespace URIs are canonical and resolvable.  
✓ All files include complete ontology definitions.  

## Contact & References

- BIBO: http://purl.org/ontology/bibo/
- BIBFRAME: https://www.loc.gov/bibframe/
- MADS: https://www.loc.gov/standards/mads/
- Dublin Core: https://dublincore.org/
- Schema.org: https://schema.org/
- PROV-O: https://www.w3.org/TR/prov-o/

---

**Last Updated:** 2026-06-23  
**Stored in:** `/home/user/ggen/ontologies/bibliographic/`
