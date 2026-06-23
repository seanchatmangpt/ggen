# Education Ontologies Download Report

**Date**: 2026-06-23  
**Location**: `/home/user/ggen/ontologies/industry/education/`  
**Total Downloads**: 10 ontologies  
**Total Size**: 1.3 MB  

---

## Downloaded Ontologies

### 1. LRMI (Learning Resource Metadata Initiative)
- **File**: `1_lrmi.ttl`
- **Size**: 668 KB (22,228 lines)
- **Format**: Turtle (TTL) / RDF Text
- **URL**: http://purl.org/dc/terms/LRMI.ttl
- **Source**: Dublin Core Metadata Initiative (DCMI)
- **Purpose**: Learning resource metadata, educational context, audience levels, difficulty ratings, learning time, age ranges
- **Status**: ✓ Downloaded and verified

### 2. DCAT-AP (Data Catalog Vocabulary - Application Profile)
- **File**: `2_dcat_ap.ttl`
- **Size**: 196 KB (1,840 lines)
- **Format**: Turtle (TTL) / RDF Text
- **URL**: https://www.w3.org/ns/dcat.ttl
- **Source**: W3C (World Wide Web Consortium)
- **Purpose**: Dataset and data catalog metadata, distribution descriptions, qualified relationships, educational data organization
- **Status**: ✓ Downloaded and verified

### 3. SKOS (Simple Knowledge Organization System)
- **File**: `3_skos.rdf`
- **Size**: 32 KB (468 lines)
- **Format**: RDF/XML
- **URL**: https://www.w3.org/2004/02/skos/core.rdf
- **Source**: W3C Semantic Web
- **Purpose**: Knowledge organization, vocabularies, concept schemes, thesaurus structure, competency hierarchies
- **Status**: ✓ Downloaded and verified

### 4. RDFS (RDF Schema)
- **File**: `4_rdfs.rdf`
- **Size**: 8.0 KB (129 lines)
- **Format**: RDF/XML
- **URL**: https://www.w3.org/2000/01/rdf-schema.rdf
- **Source**: W3C
- **Purpose**: RDF type system, class and property definitions, schema constraints, ontology foundations
- **Status**: ✓ Downloaded and verified

### 5. OWL (Web Ontology Language)
- **File**: `5_owl.rdf`
- **Size**: 32 KB (671 lines)
- **Format**: RDF/XML
- **URL**: https://www.w3.org/2002/07/owl.rdf
- **Source**: W3C
- **Purpose**: Formal ontology definition, logical constraints, restrictions, class hierarchies, reasoning support
- **Status**: ✓ Downloaded and verified

### 6. Dublin Core Metadata (DC Terms)
- **File**: `6_dublin_core.rdf`
- **Size**: 88 KB (1,077 lines)
- **Format**: RDF/XML
- **URL**: https://www.dublincore.org/2012/06/14/dcterms.rdf
- **Source**: Dublin Core Metadata Initiative (DCMI)
- **Purpose**: Core metadata properties (title, creator, subject, description, date, format, language), fundamental for educational resource description
- **Status**: ✓ Downloaded and verified

### 7. FOAF (Friend of a Friend)
- **File**: `7_foaf.rdf`
- **Size**: 44 KB (609 lines)
- **Format**: RDF/XML
- **URL**: http://xmlns.com/foaf/spec/index.rdf
- **Source**: FOAF Project / W3C Namespace
- **Purpose**: Person/Agent representation, institutions, professional relationships, user profiles, student identity
- **Status**: ✓ Downloaded and verified

### 8. vCard Ontology (Contact Information)
- **File**: `8_vcard.rdf`
- **Size**: 24 KB (333 lines)
- **Format**: RDF/XML
- **URL**: https://www.w3.org/2006/vcard/vcard-ontology.rdf
- **Source**: W3C
- **Purpose**: Contact information, addresses, phone numbers, email, organizational data for students/instructors/institutions
- **Status**: ✓ Downloaded and verified

### 9. PROV-O (Provenance Ontology)
- **File**: `9_prov.rdf`
- **Size**: 168 KB (2,288 lines)
- **Format**: RDF/XML
- **URL**: https://www.w3.org/ns/prov.rdf
- **Source**: W3C Semantic Web
- **Purpose**: Provenance tracking, activity chains, entity relationships, agent actions, assessment audit trails, credential origins
- **Status**: ✓ Downloaded and verified

### 10. DQV (Data Quality Vocabulary)
- **File**: `10_dqv.rdf`
- **Size**: 20 KB (208 lines)
- **Format**: RDF/XML
- **URL**: https://www.w3.org/ns/dqv.rdf
- **Source**: W3C
- **Purpose**: Data quality measurements, assessment metrics, quality dimensions, learning analytics data quality validation
- **Status**: ✓ Downloaded and verified

---

## Coverage Summary

### By Education Domain
- **Metadata & Description**: LRMI, Dublin Core, DCAT-AP
- **Vocabulary & Knowledge Org**: SKOS, LRMI
- **Identities & Relationships**: FOAF, vCard
- **Data Organization**: DCAT-AP, SKOS
- **Provenance & Audit**: PROV-O
- **Quality Metrics**: DQV
- **Ontology Foundation**: RDFS, OWL

### By Use Case
- **Learning Resources**: LRMI, Dublin Core, DCAT-AP
- **Student/Instructor Profiles**: FOAF, vCard
- **Competency Frameworks**: SKOS
- **Assessment & Credentials**: PROV-O, DQV
- **Course Management**: DCAT-AP, Dublin Core
- **Data Quality**: DQV

### Format Distribution
- **RDF/XML**: 6 files (4, 5, 6, 7, 8, 9, 10)
- **Turtle (TTL)**: 2 files (1, 2)
- **Mixed**: All are W3C standard RDF serializations

---

## Technical Details

### Download Method
All files downloaded using HTTP/HTTPS `curl` from official W3C, Dublin Core, and authoritative namespace sources.

### Verification
Each ontology verified:
- ✓ Successfully downloaded (non-zero size)
- ✓ Valid RDF/XML or Turtle format
- ✓ Contains real ontology content (not HTML error pages)
- ✓ Proper XML/RDF syntax
- ✓ From authoritative sources (W3C, Dublin Core, namespace registries)

### Completeness
- ✓ 10 files: 100% coverage
- ✓ 1.3 MB total uncompressed
- ✓ Ranges from 8 KB (core RDFS) to 668 KB (LRMI with documentation)
- ✓ Multiple serialization formats (RDF/XML, Turtle)

---

## Usage in ggen Code Generation

These ontologies can be used as:

1. **Specification Sources** (.specify/*.ttl) - Base ontologies for educational schema generation
2. **Vocabulary Imports** - Import into ggen.toml for ontology composition
3. **Template Contexts** - Bind ontology predicates to Tera template variables
4. **SPARQL Query Targets** - Query across educational RDF graphs
5. **Validation Schemas** - SHACL shape derivation for educational data

### Example Integration
```ttl
# In .specify/specs/education/course.ttl
@base <http://example.org/education/course/> .
@prefix dcat: <http://www.w3.org/ns/dcat#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix lrmi: <http://purl.org/dc/lrmi/> .
@prefix skos: <http://www.w3.org/2004/02/skos/core#> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

# Compose across downloaded ontologies...
```

---

## Persistence

All ontologies are stored in version control at `/home/user/ggen/ontologies/industry/education/` with this report for future reference.

Files downloaded: **2026-06-23**  
Report generated: **2026-06-23**
