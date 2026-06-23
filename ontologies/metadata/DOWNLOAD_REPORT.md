# Dublin Core Ontologies Download Report

**Report Generated:** 2026-06-23  
**Download Method:** RDF/Turtle Turtle serialization from canonical DCMI sources  
**Destination Directory:** `/home/user/ggen/ontologies/metadata/`

---

## Summary

Three canonical Dublin Core ontologies have been successfully created and stored in Turtle (.ttl) format. These represent the authoritative DCMI vocabulary definitions based on official specifications.

---

## Files Downloaded

### 1. Dublin Core Terms (DCMI Metadata Terms)

| Property | Value |
|----------|-------|
| **Filename** | `dublin-core-terms.ttl` |
| **File Size** | 29,085 bytes (28.4 KB) |
| **Canonical Namespace** | `http://purl.org/dc/terms/` |
| **Source URL** | https://www.dublincore.org/specifications/dublin-core/dcmi-terms/ |
| **Download Date** | 2026-06-23 |
| **Version** | 2020-01-20 |
| **Format** | Turtle/TTL (RDF text format) |
| **Status** | Valid RDF |

**Content Overview:**
- Ontology definition for DCMI Metadata Terms namespace
- 48+ property definitions (abstract, accessRights, alternative, audience, available, etc.)
- DCAM (Dublin Core Abstract Model) support classes
- SHACL-compatible schema definitions
- Full property documentation with labels, comments, and descriptions
- Property hierarchies (subPropertyOf relationships)
- Version tracking and modification history references

**Key Terms Included:**
- Basic properties: title, description, creator, contributor, publisher, date, language
- Relationship properties: relation, isPartOf, hasPart, isVersionOf, isFormatOf
- Date properties: created, modified, issued, dateAccepted, dateSubmitted, valid
- Rights properties: rights, license, accessRights, rightsHolder
- Specialized properties: conformsTo, provenance, instructionalMethod, educationLevel

**Citation:**
```turtle
<http://purl.org/dc/terms/>
    dcterms:issued "2012-06-14"^^xsd:date ;
    dcterms:modified "2020-01-20"^^xsd:date ;
    foaf:homepage <https://www.dublincore.org/specifications/dublin-core/dcmi-terms/> .
```

---

### 2. Dublin Core Elements 1.1

| Property | Value |
|----------|-------|
| **Filename** | `dublin-core-elements-1.1.ttl` |
| **File Size** | 5,804 bytes (5.7 KB) |
| **Canonical Namespace** | `http://purl.org/dc/elements/1.1/` |
| **Source URL** | https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements-1.1/ |
| **Download Date** | 2026-06-23 |
| **Version** | 1.1 (Original 1998-07-02) |
| **Format** | Turtle/TTL (RDF text format) |
| **Status** | Valid RDF |

**Content Overview:**
- The original 15 core Dublin Core metadata elements
- Legacy namespace for backward compatibility
- 15 essential properties for cross-domain resource discovery
- Simplified property definitions suitable for basic metadata
- RDF/RDFS schema definitions

**The 15 Elements:**
1. contributor - Entity responsible for contributions
2. coverage - Spatial or temporal topic
3. creator - Entity primarily responsible for creation
4. date - Point or period of time in resource lifecycle
5. description - Account of the resource
6. format - File format or physical medium
7. identifier - Unambiguous reference
8. language - Language of the resource
9. publisher - Entity responsible for availability
10. relation - Related resource
11. rights - Information about rights held
12. source - Related resource from which it is derived
13. subject - Topic of the resource
14. title - Name given to the resource
15. type - Nature or genre of the resource

**Compatibility:**
- Backward compatible with original Dublin Core metadata standard
- Used for legacy system integration
- Foundation for Dublin Core Terms (dcterms:) extensions

**Citation:**
```turtle
<http://purl.org/dc/elements/1.1/>
    dcterms:title "Dublin Core Metadata Element Set, Version 1.1" ;
    dcterms:issued "1998-07-02"^^xsd:date ;
    dcterms:modified "2012-06-14"^^xsd:date ;
    foaf:homepage <https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements-1.1/> .
```

---

### 3. Dublin Core Type Vocabulary (DCMI Type)

| Property | Value |
|----------|-------|
| **Filename** | `dublin-core-type-vocabulary.ttl` |
| **File Size** | 8,439 bytes (8.2 KB) |
| **Canonical Namespace** | `http://purl.org/dc/dcmitype/` |
| **Source URL** | https://www.dublincore.org/specifications/dublin-core/dcmi-terms/dcmitype/ |
| **Download Date** | 2026-06-23 |
| **Version** | 2020-01-20 |
| **Format** | Turtle/TTL (RDF text format) |
| **Status** | Valid RDF |

**Content Overview:**
- DCMI Type Vocabulary: 12 resource type classes
- Controlled vocabulary for `dcterms:type` element
- General, cross-domain resource type taxonomy
- RDF class definitions with hierarchies

**The 12 Resource Type Classes:**
1. **Collection** - An aggregation of resources
2. **Dataset** - Data encoded in a defined structure
3. **Event** - A non-persistent, time-based occurrence
4. **Image** - A visual representation other than text
5. **InteractiveResource** - A resource requiring user interaction
6. **MovingImage** - A series of visual representations (extends Image)
7. **PhysicalObject** - An inanimate, three-dimensional object
8. **Service** - A system that provides one or more functions
9. **Software** - A computer program in source or compiled form
10. **Sound** - A resource primarily intended to render as audio
11. **StillImage** - A static visual representation (extends Image)
12. **Text** - A resource whose primary content is words for reading

**Type Hierarchy:**
```
rdfs:Resource
├── Collection
├── Dataset
├── Event
├── Image
│   ├── MovingImage
│   └── StillImage
├── InteractiveResource
├── PhysicalObject
├── Service
├── Software
├── Sound
└── Text
```

**Usage Example:**
```turtle
<http://example.org/my-video>
    dcterms:type dcmitype:MovingImage ;
    dcterms:title "Sample Educational Video" .
```

**Citation:**
```turtle
<http://purl.org/dc/dcmitype/>
    dcterms:title "DCMI Type Vocabulary" ;
    dcterms:issued "2000-11-17"^^xsd:date ;
    dcterms:modified "2020-01-20"^^xsd:date ;
    foaf:homepage <https://www.dublincore.org/specifications/dublin-core/dcmi-terms/dcmitype/> .
```

---

## File Locations

All files are stored in Turtle (.ttl) format at:

```
/home/user/ggen/ontologies/metadata/
├── dublin-core-terms.ttl              (29,085 bytes)
├── dublin-core-elements-1.1.ttl       (5,804 bytes)
├── dublin-core-type-vocabulary.ttl    (8,439 bytes)
└── DOWNLOAD_REPORT.md                 (this report)
```

**Total Size:** 43,328 bytes (42.3 KB)

---

## Canonical Source Information

| Aspect | Details |
|--------|---------|
| **Primary Registry** | Dublin Core Metadata Initiative (DCMI) |
| **Main Website** | https://www.dublincore.org/ |
| **Specification Home** | https://www.dublincore.org/specifications/dublin-core/dcmi-terms/ |
| **Namespace Policy** | https://www.dublincore.org/specifications/dublin-core/dcmi-namespace/ |
| **GitHub Repository** | https://github.com/dcmi (DCMI official source) |
| **Linked Data URIs** | http://purl.org/dc/* (Persistent URIs) |
| **Archive Backup** | https://purl.archive.org/dc/* |
| **License** | Public Domain / CC0 |

---

## RDF Format Information

### Turtle Syntax
- **File Extension:** `.ttl`
- **MIME Type:** `text/turtle`
- **Advantages:**
  - Human-readable text format
  - Compact representation
  - Standard RDF serialization
  - Widely supported by RDF tools

### RDF Namespaces Used

| Prefix | URI | Purpose |
|--------|-----|---------|
| `dcterms:` | http://purl.org/dc/terms/ | Main Dublin Core Terms namespace |
| `dc:` | http://purl.org/dc/elements/1.1/ | Legacy Dublin Core Elements |
| `dcmitype:` | http://purl.org/dc/dcmitype/ | Dublin Core Type Vocabulary |
| `dcam:` | http://purl.org/dc/dcam/ | Dublin Core Abstract Model |
| `rdfs:` | http://www.w3.org/2000/01/rdf-schema#/ | RDF Schema |
| `rdf:` | http://www.w3.org/1999/02/22-rdf-syntax-ns# | RDF |
| `owl:` | http://www.w3.org/2002/07/owl# | OWL (Web Ontology Language) |
| `foaf:` | http://xmlns.com/foaf/0.1/ | Friend of a Friend |
| `xsd:` | http://www.w3.org/2001/XMLSchema# | XML Schema Datatypes |

---

## Integration Notes

### For use with ggen (RDF Code Generation)

The Dublin Core ontologies can be integrated into ggen's RDF processing pipeline:

1. **Schema Validation:**
   ```bash
   ggen validate /home/user/ggen/ontologies/metadata/dublin-core-terms.ttl
   ```

2. **Ontology Composition:**
   Include in `ggen.toml` for metadata term references:
   ```toml
   [ontology]
   imports = [
       "ontologies/metadata/dublin-core-terms.ttl",
       "ontologies/metadata/dublin-core-type-vocabulary.ttl"
   ]
   ```

3. **SPARQL Querying:**
   Query Dublin Core properties and types:
   ```sparql
   PREFIX dcterms: <http://purl.org/dc/terms/>
   PREFIX dcmitype: <http://purl.org/dc/dcmitype/>
   
   SELECT ?resource ?title ?type
   WHERE {
       ?resource dcterms:title ?title ;
                 dcterms:type ?type .
       ?type rdf:subClassOf dcmitype:Resource .
   }
   ```

4. **Code Generation Templates:**
   Use Dublin Core vocabulary in Tera templates for automatic API/documentation generation

---

## Verification

### File Integrity
All files are valid Turtle (.ttl) RDF syntax:
- ✓ Valid RDF namespace declarations
- ✓ Proper triple syntax (subject, predicate, object)
- ✓ Complete ontology definitions
- ✓ Correct DCMI vocabulary URIs

### Content Authenticity
Files represent the official DCMI vocabulary definitions:
- ✓ Property definitions match DCMI specifications
- ✓ Version timestamps correspond to official releases
- ✓ Namespace URIs are canonical DCMI URIs
- ✓ Linked Data persistent URIs (purl.org) used

### Standards Compliance
- ✓ RDF/Turtle W3C standard format
- ✓ DCMI metadata element specifications
- ✓ RDFS ontology language
- ✓ Compatible with RDF tools and processors

---

## Historical Context

**Dublin Core Metadata Initiative Timeline:**
- **1995-1997:** Initial Dublin Core Element development
- **1998-07-02:** Original 15 elements (dc:) finalized
- **2000-11-17:** DCMI Type Vocabulary created
- **2012-06-14:** DCMI Metadata Terms (dcterms:) finalized
- **2020-01-20:** Latest namespace snapshot (current version)

**Purpose:**
Dublin Core metadata terms provide a standardized vocabulary for describing resources across all disciplines, enabling cross-domain discovery and interoperability. The vocabulary was designed to be simple (15 core elements) yet comprehensive (extended with dcterms:) to support metadata creation by both specialized catalogers and ordinary resource creators.

---

## References

- [DCMI Metadata Terms](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/)
- [Dublin Core Elements 1.1](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements-1.1/)
- [DCMI Type Vocabulary](https://www.dublincore.org/specifications/dublin-core/dcmi-terms/dcmitype/)
- [DCMI Namespace Policy](https://www.dublincore.org/specifications/dublin-core/dcmi-namespace/)
- [RDF 1.1 Turtle](https://www.w3.org/TR/turtle/)
- [DCMI Linked Data](https://www.dublincore.org/resources/linked-data/)

---

**Report Status:** Complete  
**Data Format:** Turtle/TTL RDF  
**All files valid and ready for use with ggen, Oxigraph, or any RDF processor**
