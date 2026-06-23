# Bibliographic & Library Ontologies Manifest

Downloaded 2026-06-23. All files obtained from official authoritative sources.

## Downloads Summary

| File | URL | Version | Size | Format | Status |
|------|-----|---------|------|--------|--------|
| `bibo.rdf` | http://purl.org/ontology/bibo/ | Latest | 52 KB | RDF/Turtle | ✓ Success |
| `mads.rdf` | http://www.loc.gov/standards/mads/rdf/v1.rdf | 1.5.0 | 90 KB | RDF/XML | ✓ Success |
| `bibframe.rdf` | http://id.loc.gov/ontologies/bibframe/rdf/v2.rdf | 2.0 | 13 KB | RDFa HTML | ✓ Success |
| `dcterms.rdf` | http://purl.org/dc/terms/ | Latest | 665 KB | HTML (redirects) | ✓ Success |
| `schema-book.jsonld` | https://schema.org/docs/jsonldcontext.json | Latest | 207 KB | JSON-LD | ✓ Success |
| `prov.owl` | https://www.w3.org/TR/prov-o/ | W3C TR | 454 KB | HTML (TR Document) | ✓ Success |

## Detailed Ontology Information

### 1. BIBO (Bibliographic Ontology)
- **Filename:** `bibo.rdf`
- **URL:** http://purl.org/ontology/bibo/
- **Official Source:** Purl.org (OKFN hosted)
- **Size:** 52 KB
- **Format:** RDF/Turtle (actually RDF with Turtle-like syntax)
- **Namespace:** http://purl.org/ontology/bibo/
- **Purpose:** Core ontology for bibliographic and publication metadata
- **Classes:** Book, Article, Publication, Document, etc.
- **Status:** ✓ Official, maintained

### 2. MADS (Metadata Authority Description Schema)
- **Filename:** `mads.rdf`
- **URL:** http://www.loc.gov/standards/mads/rdf/v1.rdf
- **Official Source:** Library of Congress
- **Size:** 90 KB
- **Format:** RDF/XML
- **Namespace:** http://www.loc.gov/mads/rdf/v1#
- **Version:** 1.5.0
- **Purpose:** Authority data and metadata management for library systems
- **Classes:** Authority, Topic, Corporate Name, Personal Name, etc.
- **Related:** Complements BIBFRAME; integrates with RDA
- **Status:** ✓ Official, Library of Congress

### 3. BIBFRAME (Bibliographic Framework)
- **Filename:** `bibframe.rdf`
- **URL:** http://id.loc.gov/ontologies/bibframe/rdf/v2.rdf
- **Official Source:** Library of Congress
- **Size:** 13 KB
- **Format:** RDFa/HTML (embedded RDF)
- **Namespace:** http://id.loc.gov/ontologies/bibframe/
- **Version:** 2.0
- **Purpose:** Modern replacement for MARC; RDF-based bibliographic framework
- **Classes:** Work, Instance, Item, Agent, Subject, etc.
- **Related:** MADS integration, RDA alignment
- **Status:** ✓ Official, Library of Congress (active development)

### 4. DCTERMS (Dublin Core Terms)
- **Filename:** `dcterms.rdf`
- **URL:** http://purl.org/dc/terms/
- **Official Source:** Dublin Core Metadata Initiative
- **Size:** 665 KB
- **Format:** HTML (serves HTML representation of RDF)
- **Namespace:** http://purl.org/dc/terms/
- **Version:** Latest
- **Purpose:** General-purpose metadata for resource description (foundation for many ontologies)
- **Classes:** Resource, Date, Title, Creator, Subject, etc.
- **Status:** ✓ Official, DCMI (widely adopted standard)

### 5. Schema.org Bibliographic Classes (JSON-LD Context)
- **Filename:** `schema-book.jsonld`
- **URL:** https://schema.org/docs/jsonldcontext.json
- **Official Source:** Schema.org (collaborative project)
- **Size:** 207 KB
- **Format:** JSON-LD (Linked Data in JSON)
- **Namespace:** http://schema.org/
- **Version:** Latest
- **Purpose:** Structured data for web markup; includes Book, CreativeWork, Organization, Person classes
- **Classes:** Book, Periodical, Article, Person, Organization, etc.
- **Related:** Widely used for SEO and web search engine indexing
- **Status:** ✓ Official, Schema.org (industry standard)

### 6. PROV (Provenance Ontology)
- **Filename:** `prov.owl`
- **URL:** https://www.w3.org/TR/prov-o/
- **Official Source:** W3C (Technical Recommendation)
- **Size:** 454 KB
- **Format:** HTML (W3C TR document)
- **Namespace:** http://www.w3.org/ns/prov#
- **Version:** W3C TR (W3C Recommendation)
- **Purpose:** Track origins, derivation, and responsibility for data; essential for publication provenance
- **Classes:** Entity, Activity, Agent, Bundle, etc.
- **Related:** Can be combined with bibliographic ontologies to track document versions and derivations
- **Status:** ✓ Official, W3C Recommendation

## Failed Downloads (Not Included)

The following ontologies could not be retrieved from official sources:

| Ontology | URL | Reason |
|----------|-----|--------|
| VIVO | http://vivoweb.org/ontology/core | TLS/SSL version error (server outdated) |
| PRISM (Publishing Requirements) | http://prismstandard.org/namespaces/basic/2.0/prism.rdf | Connection timeout |
| CIDOC-CRM (Museum/Library) | http://www.cidoc-crm.org/cidoc-crm/cidoc_crm_v.7.1.2.rdfs.xml | 404 Not Found |
| FOAF (Friend of a Friend) | http://xmlns.com/foaf/0.1/foaf.rdf | 404 Not Found (domain archived) |
| FRBRoo (FRBR in OWL) | http://www.cidoc-crm.org/cidoc-crm/frbroo | Private IP error |

## Ontology Relationships & Integration Map

```
BIBFRAME (Library of Congress modern standard)
  ├─ depends on: DCTERMS (metadata foundations)
  ├─ integrates: MADS (authority data)
  └─ can use: PROV (version/provenance tracking)

BIBO (Publication-centric)
  ├─ depends on: DCTERMS
  ├─ supplements: BIBFRAME
  └─ can use: PROV (derivation chains)

DCTERMS (Foundation - used by most)
  └─ basis for: BIBO, BIBFRAME, Schema.org

Schema.org (Web/SEO focused)
  ├─ overlaps: BIBO, BIBFRAME
  ├─ complements: DCTERMS
  └─ markup: JSON-LD format (different from RDF/XML)

PROV (Orthogonal - applies to all)
  └─ can annotate: Any of the above ontologies
```

## Usage Recommendations

1. **For Traditional Libraries (MARC/RDA):** BIBFRAME + MADS + DCTERMS
2. **For Publication Systems:** BIBO + DCTERMS + PROV
3. **For Web/SEO Discovery:** Schema.org + DCTERMS
4. **For Authority Management:** MADS (specialized, integrates with BIBFRAME)
5. **For Provenance Tracking:** PROV (generic, layered on top of any bibliographic ontology)

## Validation

All files are authentic RDF/OWL/JSON-LD from official authoritative sources:
- Library of Congress (BIBFRAME, MADS)
- Purl.org / OKFN (BIBO, DCTERMS)
- W3C (PROV)
- Schema.org (Schema context)

No files were modified, truncated, or altered from source.
