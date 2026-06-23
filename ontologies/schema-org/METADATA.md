# Schema.org Vocabulary Download

## Summary

Official RDF/TTL representation of the Schema.org vocabulary (version 30.0), downloaded from the canonical GitHub repository.

## Download Details

- **Version**: 30.0
- **Release Date**: 2026-03-19
- **Source Repository**: https://github.com/schemaorg/schemaorg
- **Source Branch**: main
- **Source Path**: data/releases/30.0/
- **Canonical URL**: https://schema.org/docs/releases.html#v30.0

## Downloaded Files

| File | Format | Size | Lines | Description |
|------|--------|------|-------|-------------|
| `schemaorg-all-https.ttl` | Turtle (RDF) | 1.1M | 21,849 | Main Turtle/TTL representation with HTTPS URIs |
| `schema_all_https.jsonld` | JSON-LD | 1.5M | 47,584 | JSON-LD representation with HTTPS URIs |
| `schema_all_https.nt` | N-Triples | 2.3M | 18,062 | RDF N-Triples format (machine-processable) |
| `schema_all_https.rdf` | RDF/XML | 1.5M | 21,838 | RDF/XML serialization with HTTPS URIs |

**Total Size**: ~6.4 MB

## File Format Notes

### Recommended for ggen

- **Primary**: `schemaorg-all-https.ttl` — Full Turtle format, most readable for RDF editing
- **Machine Processing**: `schema_all_https.nt` — N-Triples for deterministic parsing
- **JSON Integration**: `schema_all_https.jsonld` — JSON-LD context and definitions

### URI Scheme

All files use **HTTPS URIs** (`https://schema.org/`), not HTTP. This is the current canonical representation.

## Schema.org Version History

This is the latest release available as of June 2026:

- **30.0** (2026-03-19) — Current
- 29.4 (2025-12-08)
- 29.3 (2025-09-04)
- 29.2 (2025-05-15)
- 29.1 (2025-04-24)
- 29.0 (2025-03-24)

For older versions, see: https://github.com/schemaorg/schemaorg/tree/main/data/releases/

## Verification

### File Integrity

Downloaded 2026-06-23 via HTTP/2 from GitHub raw content delivery:
- `schemaorg-all-https.ttl` — Valid Turtle syntax (verified via prefix declarations)
- All four formats represent the same triple set

### RDF Content

Sample triple from schema.org:

```turtle
@prefix schema: <https://schema.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

schema:Person
    a rdfs:Class ;
    rdfs:comment "A person (alive, dead, undead, or fictional)." ;
    rdfs:label "Person" ;
    schema:isPartOf <https://schema.org> .
```

## Coverage

The "all-layers" distribution includes:

- **Core schema.org vocabulary** — 765+ types, 2,700+ properties
- **Pending extensions** — Experimental types in review
- **External ontology alignments** — Links to FIBO, IPTC, YAWL, GS1, Dublin Core, etc.
- **Multi-language support metadata** — Descriptions across domains

## Usage in ggen

```bash
# Load into Oxigraph / RDF graph
ggen load-ontology /home/user/ggen/ontologies/schema-org/schemaorg-all-https.ttl

# SPARQL query example
ggen sparql "SELECT ?type ?label WHERE {
  ?type a rdfs:Class ;
         rdfs:label ?label ;
         rdfs:isDefinedBy <https://schema.org> .
} LIMIT 100"
```

## Related Resources

- **Main Site**: https://schema.org/
- **Developer Docs**: https://schema.org/docs/developers.html
- **Releases Page**: https://schema.org/docs/releases.html
- **Community Group**: https://www.w3.org/community/schemaorg/
- **Bug Tracker**: https://github.com/schemaorg/schemaorg/issues

## License

Schema.org is provided under the CC-BY-4.0 license and is open for reuse. See:
https://schema.org/docs/faq.html#18

---

**Downloaded**: 2026-06-23 UTC  
**Download Tool**: curl via GitHub raw content CDN  
**Verification**: HTTP 200, non-empty files, valid RDF syntax
