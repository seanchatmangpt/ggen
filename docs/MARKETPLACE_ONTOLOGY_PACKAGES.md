# Marketplace Ontology Packages

**Status**: Design + Package Structure  
**Date**: 2026-06-23

## Overview

Domain-specific ontologies are distributed via ggen-marketplace as installable packages. This document defines the package structure, metadata format, and distribution strategy.

## Package Structure

Each ontology package is a ggen-pack with the following structure:

```
medical-fhir-2024/
├── ggen-pack.toml                 # Package manifest
├── MANIFEST.ttl                   # RDF manifest (DCAT)
├── ontologies/
│   ├── fhir-types.ttl            # Core FHIR types
│   ├── fhir-resources.ttl        # FHIR resources
│   └── ...
└── shapes/
    └── fhir-validation.shaclx    # Validation constraints
```

## Package Manifest Format

### ggen-pack.toml

```toml
[package]
name = "medical/fhir"
version = "2024.1.0"
description = "FHIR (Fast Healthcare Interoperability Resources) ontology and validation shapes"
authors = ["HL7 International", "ggen-marketplace"]
license = "CC-BY-4.0"

[ontology]
# Primary namespace URI
namespace = "http://hl7.org/fhir/"
# List of all ontology files in package
files = [
    "ontologies/fhir-types.ttl",
    "ontologies/fhir-resources.ttl"
]
# Total triple count for index optimization
triples = 45000

[dependencies]
# Core ontologies required
rdf = { embedded = true, namespace = "http://www.w3.org/1999/02/22-rdf-syntax-ns#" }
owl = { embedded = true, namespace = "http://www.w3.org/2002/07/owl#" }
dcterms = { embedded = true, namespace = "http://purl.org/dc/terms/" }

# Other domain packages this depends on
"medical/snomed" = "2024.1.0"

[validation]
shapes = ["shapes/fhir-validation.shaclx"]

[install]
# Where to cache downloaded ontologies
cache_dir = "~/.ggen/ontology-cache"
# Verify signatures before installation
verify_signatures = true

[keywords]
keywords = ["healthcare", "FHIR", "HL7", "medical", "clinical"]

[source]
# Authoritative URL for package
homepage = "https://hl7.org/fhir/r5/"
# Download URL
download = "https://build.fhir.org/ontology.ttl"
# Source repository
repository = "https://github.com/HL7/fhir-ontology"
# Documentation URL
documentation = "https://hl7.org/fhir/r5/ontology.html"
```

### MANIFEST.ttl (DCAT)

```turtle
@prefix dcat: <http://www.w3.org/ns/dcat#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix ggen: <http://ggen.dev/ontology/> .

:fhir-package a dcat:Dataset ;
    dcterms:title "FHIR Ontology" ;
    dcterms:description "Fast Healthcare Interoperability Resources" ;
    dcterms:issued "2024-01-15"^^xsd:date ;
    dcterms:modified "2024-06-23"^^xsd:date ;
    dcterms:version "2024.1.0" ;
    dcat:keyword "healthcare", "FHIR", "HL7" ;
    dcat:distribution :fhir-dist ;
    dcterms:creator :hl7 ;
    dcterms:conformsTo <http://www.w3.org/TR/vocab-dcat-2/> ;
    ggen:ontology-size 45000 ;  # Triple count
    ggen:package-size 2500000 ;  # Bytes
    ggen:embedded false .        # Not in core bundle

:fhir-dist a dcat:Distribution ;
    dcterms:format "Turtle" ;
    dcat:mediaType "text/turtle" ;
    dcat:downloadURL <https://build.fhir.org/ontology.ttl> ;
    dcterms:issued "2024-06-23"^^xsd:dateTime ;
    dcat:byteSize 2500000 ;
    dcat:checksum [
        a spdx:Checksum ;
        spdx:algorithm "SHA256" ;
        spdx:checksumValue "abc123..." ;
    ] .

:hl7 a foaf:Organization ;
    foaf:name "HL7 International" ;
    foaf:homepage <https://www.hl7.org/> .
```

## Domain Packages Index

### Healthcare

| Package | Namespace | Files | Size | Status |
|---------|-----------|-------|------|--------|
| `medical/fhir` | `http://hl7.org/fhir/` | 8 TTL | 2.5 MB | 📦 Marketplace |
| `medical/snomed` | `http://snomed.info/sct/` | 15 TTL | 18 MB | 📦 Marketplace |
| `medical/disease-ontology` | `http://purl.obolibrary.org/obo/DOID_` | 6 TTL | 28 MB | 📦 Marketplace |

### Financial

| Package | Namespace | Files | Size | Status |
|---------|-----------|-------|------|--------|
| `finance/fibo` | `https://spec.edmcouncil.org/fibo/` | 658 RDF | 63.8 MB | 📦 Marketplace |
| `finance/goodrelations` | `http://purl.org/goodrelations/v1#` | 2 RDF | 250 KB | 📦 Marketplace |

### Manufacturing

| Package | Namespace | Files | Size | Status |
|---------|-----------|-------|------|--------|
| `manufacturing/isa95` | `http://isa95.org/` | 4 TTL | 1.2 MB | 📦 Marketplace |
| `manufacturing/eclass` | `http://www.eclass.de/` | 3 RDF | 300 KB | 📦 Marketplace |

### Energy

| Package | Namespace | Files | Size | Status |
|---------|-----------|-------|------|--------|
| `energy/saref` | `https://saref.etsi.org/core/` | 5 TTL | 1.8 MB | 📦 Marketplace |

## Installation & Caching Strategy

### Default Behavior

```bash
# First use: download and cache
ggen ontology install medical/fhir@2024.1.0
# Downloads to ~/.ggen/ontology-cache/medical/fhir/2024.1.0/

# Subsequent uses: load from cache
ggen sync  # Uses cached FHIR ontology

# Update cached package
ggen ontology update medical/fhir  # Re-downloads if newer version available

# Clear cache
ggen ontology cache --clear medical/fhir  # Remove specific package
ggen ontology cache --clear-all           # Remove all domain packages
```

### Cache Structure

```
~/.ggen/ontology-cache/
├── medical/
│   ├── fhir/
│   │   ├── 2024.1.0/
│   │   │   ├── ggen-pack.toml
│   │   │   ├── ontologies/
│   │   │   │   ├── fhir-types.ttl
│   │   │   │   └── fhir-resources.ttl
│   │   │   └── MANIFEST.ttl
│   │   └── 2024.2.0/  # Multiple versions cached
│   └── snomed/
│       └── 2024.1.0/
├── finance/
│   └── fibo/
│       └── latest/
└── .index.json  # Local package index
```

### Offline Mode

```toml
# ggen.toml
[package]
offline = true  # Only use embedded + cached ontologies, never download

[ontology-dependencies]
medical-fhir = "2024.1.0"  # Must be pre-cached
manufacturing-isa95 = "*"  # Use any cached version
```

## Package Discovery

### Registry Query API (SPARQL)

```sparql
PREFIX dcat: <http://www.w3.org/ns/dcat#>
PREFIX dcterms: <http://purl.org/dc/terms/>
PREFIX ggen: <http://ggen.dev/ontology/>

SELECT ?name ?version ?description ?size
WHERE {
    ?package a ggen:OntologyPackage ;
             dcterms:title ?name ;
             dcterms:version ?version ;
             dcterms:description ?description ;
             ggen:package-size ?size .
    
    FILTER CONTAINS(STR(?name), "healthcare")
}
ORDER BY DESC(?version)
```

### CLI Search

```bash
# Search by keyword
ggen ontology search --keyword healthcare
# Output: medical/fhir, medical/snomed, medical/disease-ontology

# Search by domain
ggen ontology search --domain medical
# Output: All medical/* packages with versions

# Search by namespace
ggen ontology search --namespace "http://hl7.org/fhir/"
# Output: medical/fhir@latest

# Advanced search
ggen ontology search --size "<10MB" --keyword "manufacturing"
# Output: Packages matching criteria
```

## Dependency Resolution

When installing a package, ggen automatically:

1. **Resolves transitive dependencies**: If `medical/fhir` depends on `medical/snomed`, both are installed
2. **Checks embedded**: Uses core bundle if dependency is embedded (no download needed)
3. **Validates versions**: Ensures compatible versions are resolved
4. **Detects conflicts**: Errors if two packages require incompatible versions

### Example

```toml
[dependencies]
# User explicitly requests
"medical/fhir@2024.1.0"

# Automatic dependencies resolved
"medical/fhir@2024.1.0" → requires "dcterms" (embedded) ✓
"medical/fhir@2024.1.0" → requires "medical/snomed@2024.*" → downloads
"medical/snomed@2024.1.0" → requires "skos" (embedded) ✓
```

## Signature Verification

Each package distribution includes a cryptographic signature:

```toml
[package]
signature = "base64-encoded-ed25519-signature"
public_key = "path/to/public-key.pem"

[verification]
# Verify before installation
verify_signatures = true
# Trusted key registry
key_registry = "https://registry.ggen.dev/keys"
```

## Update Strategy

### Version Pinning

```toml
[ontology-dependencies]
# Exact version
medical-fhir = "2024.1.0"

# Minor version range (recommended)
medical-fhir = "2024.1.*"

# Major version range
medical-fhir = "2024.*"

# Latest stable
medical-fhir = "*"
```

### Security Updates

Packages can declare security-critical updates:

```toml
[package]
security_patch = true  # Critical security update
deprecated_versions = ["2024.0.*"]  # Warn users to upgrade
```

## Marketplace URL Scheme

All packages are addressed via:

```
registry://<namespace>/<package>@<version>
registry://medical/fhir@2024.1.0
registry://finance/fibo@latest
registry://manufacturing/isa95@*
```

The `OntologyResolver` understands this scheme and:
1. Queries the marketplace registry
2. Downloads if needed
3. Caches locally
4. Falls back to cached version on network errors

## Future Enhancements

### Phase 2: Private Registries

Support for private/corporate ontology registries:

```toml
[registry]
default = "https://registry.ggen.dev"
corporate = "https://ontology.company.com"

[ontology-dependencies]
"medical/fhir" = "2024.1.0"  # From public registry
"company/internal" = { registry = "corporate", version = "1.0.0" }
```

### Phase 3: Ontology Composition

Combine multiple ontologies with explicit imports:

```turtle
@prefix ggen: <http://ggen.dev/ontology/> .

:MyComposite a ggen:CompositeOntology ;
    ggen:imports <registry://medical/fhir@2024.1.0> ;
    ggen:imports <registry://manufacturing/isa95@latest> ;
    ggen:extends <http://example.org/domain-specific> .
```

### Phase 4: Marketplace UI

Web UI for browsing and installing packages:
- Search by domain/keyword
- View package metadata and dependencies
- 1-click installation
- Version history and changelogs

## Testing

### Package Validation Test

```bash
# Verify package structure and metadata
ggen ontology validate medical/fhir

# Output:
# ✓ ggen-pack.toml valid
# ✓ MANIFEST.ttl valid DCAT
# ✓ All ontology files found
# ✓ Triple count matches declared
# ✓ Dependencies resolvable
# ✓ Namespace URIs valid
```

### Integration Test

```bash
# Test full pipeline with marketplace packages
ggen sync --with-packages "medical/fhir@2024.1.0,manufacturing/isa95@latest"

# Should:
# 1. Download packages if not cached
# 2. Load core ontologies from bundle
# 3. Load domain packages from cache
# 4. Merge all ontologies
# 5. Run μ₁–μ₅ pipeline
# 6. Generate artifacts
```

---

**See also**:
- [Core Ontology Embedding](./architecture/CORE_ONTOLOGY_EMBEDDING.md)
- [ggen-marketplace](../crates/ggen-marketplace/)
- [Example Usage](./examples/embedding_usage.md)
