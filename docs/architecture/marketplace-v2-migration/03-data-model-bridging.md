# Data Model Bridging - V1 ↔ V2 Conversion

## Overview

The data model bridging layer enables seamless conversion between marketplace-v1 (tantivy-based) and marketplace-v2 (RDF-based) data formats.

## Core Data Models

### V1 Data Model (Current)

```rust
// ggen-marketplace/src/models.rs

/// V1 Package model (tantivy index format)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct V1Package {
    pub id: String,
    pub name: String,
    pub version: String,
    pub description: String,
    pub author: Option<String>,
    pub license: Option<String>,
    pub repository: Option<String>,
    pub homepage: Option<String>,
    pub tags: Vec<String>,
    pub category: Option<String>,
    pub dependencies: Vec<V1Dependency>,
    pub downloads: u32,
    pub stars: u32,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct V1Dependency {
    pub name: String,
    pub version: String,
    pub optional: bool,
}

/// V1 Manifest format (gpack.yaml)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct V1Manifest {
    pub package: V1PackageMetadata,
    pub dependencies: Option<HashMap<String, String>>,
    pub dev_dependencies: Option<HashMap<String, String>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct V1PackageMetadata {
    pub name: String,
    pub version: String,
    pub description: String,
    pub author: Option<String>,
    pub license: Option<String>,
}
```

### V2 Data Model (RDF-based)

```rust
// ggen-marketplace-v2/src/models.rs

/// V2 Package model (RDF triple format)
#[derive(Debug, Clone)]
pub struct V2Package {
    pub uri: PackageUri,
    pub metadata: PackageMetadata,
    pub triples: Vec<Triple>,
    pub signature: Option<Ed25519Signature>,
}

/// Package URI (unique identifier in RDF graph)
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct PackageUri(String);

impl PackageUri {
    pub fn new(name: &str, version: &str) -> Self {
        Self(format!("http://ggen.io/pkg/{}/{}", name, version))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

/// Package metadata (extracted from RDF triples)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageMetadata {
    pub name: String,
    pub version: String,
    pub description: String,
    pub author: Option<String>,
    pub license: Option<String>,
    pub repository: Option<String>,
    pub homepage: Option<String>,
    pub tags: Vec<String>,
    pub category: Option<String>,
    pub dependencies: Vec<Dependency>,
    pub downloads: u32,
    pub stars: u32,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,

    // V2-specific fields
    pub rdf_graph_uri: String,
    pub cryptographic_hash: String,
    pub signature_public_key: Option<String>,
}

/// RDF Triple
#[derive(Debug, Clone)]
pub struct Triple {
    pub subject: String,
    pub predicate: String,
    pub object: TripleObject,
}

#[derive(Debug, Clone)]
pub enum TripleObject {
    Uri(String),
    Literal(String),
    LiteralWithLang(String, String),
    LiteralWithType(String, String),
}
```

### Unified Data Model (Adapter Layer)

```rust
// ggen-domain/src/marketplace/models.rs

/// Unified package model (works with both v1 and v2)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnifiedPackage {
    // Common fields (present in both v1 and v2)
    pub id: PackageId,
    pub name: String,
    pub version: String,
    pub description: String,
    pub author: Option<String>,
    pub license: Option<String>,
    pub repository: Option<String>,
    pub homepage: Option<String>,
    pub tags: Vec<String>,
    pub category: Option<String>,
    pub dependencies: Vec<UnifiedDependency>,
    pub downloads: u32,
    pub stars: u32,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,

    // Backend metadata
    pub source_backend: BackendType,

    // V2-specific fields (optional for v1 compatibility)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rdf_graph_uri: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub cryptographic_hash: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature_public_key: Option<String>,

    // V1-specific fields (optional for v2)
    // (none currently - v1 is subset of v2)
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UnifiedDependency {
    pub name: String,
    pub version: String,
    pub optional: bool,
}

/// Package ID (backend-agnostic)
#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct PackageId(String);

impl PackageId {
    pub fn new(name: &str, version: &str) -> Self {
        Self(format!("{}@{}", name, version))
    }

    pub fn from_v1(id: &str) -> Self {
        Self(id.to_string())
    }

    pub fn from_v2(uri: &PackageUri) -> Self {
        // Extract name and version from URI
        let parts: Vec<&str> = uri.as_str().split('/').collect();
        let name = parts.get(parts.len() - 2).unwrap_or(&"unknown");
        let version = parts.get(parts.len() - 1).unwrap_or(&"0.0.0");
        Self::new(name, version)
    }

    pub fn to_v1(&self) -> String {
        self.0.clone()
    }

    pub fn to_v2(&self) -> PackageUri {
        let parts: Vec<&str> = self.0.split('@').collect();
        let name = parts.get(0).unwrap_or(&"unknown");
        let version = parts.get(1).unwrap_or(&"0.0.0");
        PackageUri::new(name, version)
    }
}
```

## Conversion Layer

### V1 → Unified Conversion

```rust
// ggen-domain/src/marketplace/conversion/v1_to_unified.rs

use ggen_marketplace::V1Package;

impl From<V1Package> for UnifiedPackage {
    fn from(v1: V1Package) -> Self {
        Self {
            id: PackageId::from_v1(&v1.id),
            name: v1.name,
            version: v1.version,
            description: v1.description,
            author: v1.author,
            license: v1.license,
            repository: v1.repository,
            homepage: v1.homepage,
            tags: v1.tags,
            category: v1.category,
            dependencies: v1.dependencies.into_iter().map(Into::into).collect(),
            downloads: v1.downloads,
            stars: v1.stars,
            created_at: v1.created_at,
            updated_at: v1.updated_at,
            source_backend: BackendType::V1,

            // V2 fields are None for v1 packages
            rdf_graph_uri: None,
            cryptographic_hash: None,
            signature: None,
            signature_public_key: None,
        }
    }
}

impl From<ggen_marketplace::V1Dependency> for UnifiedDependency {
    fn from(v1: ggen_marketplace::V1Dependency) -> Self {
        Self {
            name: v1.name,
            version: v1.version,
            optional: v1.optional,
        }
    }
}
```

### V2 → Unified Conversion

```rust
// ggen-domain/src/marketplace/conversion/v2_to_unified.rs

use ggen_marketplace_v2::{V2Package, PackageMetadata};

impl From<V2Package> for UnifiedPackage {
    fn from(v2: V2Package) -> Self {
        let meta = v2.metadata;

        Self {
            id: PackageId::from_v2(&v2.uri),
            name: meta.name,
            version: meta.version,
            description: meta.description,
            author: meta.author,
            license: meta.license,
            repository: meta.repository,
            homepage: meta.homepage,
            tags: meta.tags,
            category: meta.category,
            dependencies: meta.dependencies.into_iter().map(Into::into).collect(),
            downloads: meta.downloads,
            stars: meta.stars,
            created_at: meta.created_at,
            updated_at: meta.updated_at,
            source_backend: BackendType::V2,

            // V2-specific fields
            rdf_graph_uri: Some(meta.rdf_graph_uri),
            cryptographic_hash: Some(meta.cryptographic_hash),
            signature: v2.signature.map(|s| s.to_string()),
            signature_public_key: meta.signature_public_key,
        }
    }
}

impl From<ggen_marketplace_v2::Dependency> for UnifiedDependency {
    fn from(v2: ggen_marketplace_v2::Dependency) -> Self {
        Self {
            name: v2.name,
            version: v2.version,
            optional: v2.optional,
        }
    }
}
```

### Unified → V1 Conversion

```rust
// ggen-domain/src/marketplace/conversion/unified_to_v1.rs

impl TryFrom<UnifiedPackage> for V1Package {
    type Error = ConversionError;

    fn try_from(unified: UnifiedPackage) -> Result<Self, Self::Error> {
        // V1 doesn't support cryptographic signatures - warn if present
        if unified.signature.is_some() {
            tracing::warn!(
                "Package {} has cryptographic signature which will be lost in v1 conversion",
                unified.name
            );
        }

        Ok(Self {
            id: unified.id.to_v1(),
            name: unified.name,
            version: unified.version,
            description: unified.description,
            author: unified.author,
            license: unified.license,
            repository: unified.repository,
            homepage: unified.homepage,
            tags: unified.tags,
            category: unified.category,
            dependencies: unified.dependencies
                .into_iter()
                .map(TryInto::try_into)
                .collect::<Result<Vec<_>, _>>()?,
            downloads: unified.downloads,
            stars: unified.stars,
            created_at: unified.created_at,
            updated_at: unified.updated_at,
        })
    }
}
```

### Unified → V2 Conversion

```rust
// ggen-domain/src/marketplace/conversion/unified_to_v2.rs

use ggen_marketplace_v2::{V2Package, PackageMetadata, Triple, TripleObject};

impl TryFrom<UnifiedPackage> for V2Package {
    type Error = ConversionError;

    fn try_from(unified: UnifiedPackage) -> Result<Self, Self::Error> {
        let uri = unified.id.to_v2();

        // Build RDF triples from package metadata
        let triples = build_package_triples(&uri, &unified)?;

        // Parse signature if present
        let signature = unified.signature
            .map(|s| Ed25519Signature::from_string(&s))
            .transpose()?;

        let metadata = PackageMetadata {
            name: unified.name,
            version: unified.version,
            description: unified.description,
            author: unified.author,
            license: unified.license,
            repository: unified.repository,
            homepage: unified.homepage,
            tags: unified.tags,
            category: unified.category,
            dependencies: unified.dependencies
                .into_iter()
                .map(Into::into)
                .collect(),
            downloads: unified.downloads,
            stars: unified.stars,
            created_at: unified.created_at,
            updated_at: unified.updated_at,
            rdf_graph_uri: unified.rdf_graph_uri
                .unwrap_or_else(|| uri.as_str().to_string()),
            cryptographic_hash: unified.cryptographic_hash
                .unwrap_or_else(|| compute_hash(&unified)),
            signature_public_key: unified.signature_public_key,
        };

        Ok(V2Package {
            uri,
            metadata,
            triples,
            signature,
        })
    }
}

fn build_package_triples(uri: &PackageUri, pkg: &UnifiedPackage) -> Result<Vec<Triple>> {
    let mut triples = Vec::new();
    let subject = uri.as_str();

    // Basic metadata triples
    triples.push(Triple {
        subject: subject.to_string(),
        predicate: "http://www.w3.org/1999/02/22-rdf-syntax-ns#type".to_string(),
        object: TripleObject::Uri("http://ggen.io/pkg#Package".to_string()),
    });

    triples.push(Triple {
        subject: subject.to_string(),
        predicate: "http://ggen.io/pkg#name".to_string(),
        object: TripleObject::Literal(pkg.name.clone()),
    });

    triples.push(Triple {
        subject: subject.to_string(),
        predicate: "http://ggen.io/pkg#version".to_string(),
        object: TripleObject::Literal(pkg.version.clone()),
    });

    triples.push(Triple {
        subject: subject.to_string(),
        predicate: "http://ggen.io/pkg#description".to_string(),
        object: TripleObject::Literal(pkg.description.clone()),
    });

    // Optional fields
    if let Some(author) = &pkg.author {
        triples.push(Triple {
            subject: subject.to_string(),
            predicate: "http://ggen.io/pkg#author".to_string(),
            object: TripleObject::Literal(author.clone()),
        });
    }

    if let Some(license) = &pkg.license {
        triples.push(Triple {
            subject: subject.to_string(),
            predicate: "http://ggen.io/pkg#license".to_string(),
            object: TripleObject::Literal(license.clone()),
        });
    }

    // Tags as multiple triples
    for tag in &pkg.tags {
        triples.push(Triple {
            subject: subject.to_string(),
            predicate: "http://ggen.io/pkg#tag".to_string(),
            object: TripleObject::Literal(tag.clone()),
        });
    }

    // Dependencies as separate nodes
    for dep in &pkg.dependencies {
        let dep_uri = format!("{}#dep-{}", subject, dep.name);

        triples.push(Triple {
            subject: subject.to_string(),
            predicate: "http://ggen.io/pkg#dependency".to_string(),
            object: TripleObject::Uri(dep_uri.clone()),
        });

        triples.push(Triple {
            subject: dep_uri.clone(),
            predicate: "http://ggen.io/pkg#dependencyName".to_string(),
            object: TripleObject::Literal(dep.name.clone()),
        });

        triples.push(Triple {
            subject: dep_uri,
            predicate: "http://ggen.io/pkg#dependencyVersion".to_string(),
            object: TripleObject::Literal(dep.version.clone()),
        });
    }

    Ok(triples)
}
```

## RDF Schema Definition

### Package Ontology

```turtle
# ggen-marketplace-v2/schemas/package.ttl

@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix pkg: <http://ggen.io/pkg#> .

# Package class
pkg:Package a rdfs:Class ;
    rdfs:label "Package" ;
    rdfs:comment "A code generation package in the ggen marketplace" .

# Properties
pkg:name a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range xsd:string ;
    rdfs:label "name" ;
    rdfs:comment "Package name" .

pkg:version a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range xsd:string ;
    rdfs:label "version" ;
    rdfs:comment "Semantic version" .

pkg:description a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range xsd:string ;
    rdfs:label "description" ;
    rdfs:comment "Package description" .

pkg:author a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range xsd:string ;
    rdfs:label "author" ;
    rdfs:comment "Package author" .

pkg:license a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range xsd:string ;
    rdfs:label "license" ;
    rdfs:comment "License identifier (SPDX)" .

pkg:tag a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range xsd:string ;
    rdfs:label "tag" ;
    rdfs:comment "Package tag for categorization" .

pkg:category a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range xsd:string ;
    rdfs:label "category" ;
    rdfs:comment "Package category" .

pkg:dependency a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range pkg:Dependency ;
    rdfs:label "dependency" ;
    rdfs:comment "Package dependency" .

pkg:downloads a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range xsd:nonNegativeInteger ;
    rdfs:label "downloads" ;
    rdfs:comment "Download count" .

pkg:stars a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range xsd:nonNegativeInteger ;
    rdfs:label "stars" ;
    rdfs:comment "Star count" .

pkg:signature a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range xsd:string ;
    rdfs:label "signature" ;
    rdfs:comment "Ed25519 cryptographic signature" .

pkg:publicKey a rdf:Property ;
    rdfs:domain pkg:Package ;
    rdfs:range xsd:string ;
    rdfs:label "publicKey" ;
    rdfs:comment "Ed25519 public key for verification" .

# Dependency class
pkg:Dependency a rdfs:Class ;
    rdfs:label "Dependency" ;
    rdfs:comment "A package dependency" .

pkg:dependencyName a rdf:Property ;
    rdfs:domain pkg:Dependency ;
    rdfs:range xsd:string ;
    rdfs:label "dependencyName" ;
    rdfs:comment "Dependency package name" .

pkg:dependencyVersion a rdf:Property ;
    rdfs:domain pkg:Dependency ;
    rdfs:range xsd:string ;
    rdfs:label "dependencyVersion" ;
    rdfs:comment "Dependency version constraint" .

pkg:optional a rdf:Property ;
    rdfs:domain pkg:Dependency ;
    rdfs:range xsd:boolean ;
    rdfs:label "optional" ;
    rdfs:comment "Whether dependency is optional" .
```

## Manifest Format Migration

### V1 Manifest (gpack.yaml)

```yaml
package:
  name: example-package
  version: 1.0.0
  description: Example package
  author: user@example.com
  license: MIT

dependencies:
  base-template: ^2.0.0
  utils: 1.5.0

dev_dependencies:
  testing-tools: ^1.0.0
```

### V2 Manifest (gpack.yaml + RDF metadata)

```yaml
package:
  name: example-package
  version: 1.0.0
  description: Example package
  author: user@example.com
  license: MIT

  # V2-specific fields
  rdf:
    graph_uri: http://ggen.io/pkg/example-package/1.0.0
    ontology_version: 2.0.0

  # Cryptographic signing
  signature:
    public_key: "base64-encoded-ed25519-public-key"
    signature: "base64-encoded-signature"
    algorithm: Ed25519

dependencies:
  base-template: ^2.0.0
  utils: 1.5.0

dev_dependencies:
  testing-tools: ^1.0.0

# V2 RDF metadata (optional, for advanced use cases)
rdf_metadata:
  - subject: http://ggen.io/pkg/example-package/1.0.0
    predicate: http://ggen.io/pkg#compatibleWith
    object: "ggen >= 3.0.0"

  - subject: http://ggen.io/pkg/example-package/1.0.0
    predicate: http://ggen.io/pkg#maintainedBy
    object: http://github.com/example/example-package
```

## Bidirectional Conversion Testing

```rust
// ggen-domain/tests/conversion_roundtrip_tests.rs

#[test]
fn test_v1_to_unified_to_v1_roundtrip() {
    let original_v1 = create_test_v1_package();

    // V1 → Unified
    let unified: UnifiedPackage = original_v1.clone().into();

    // Unified → V1
    let converted_v1: V1Package = unified.try_into().unwrap();

    // Should be identical (minus insignificant fields)
    assert_eq!(original_v1.name, converted_v1.name);
    assert_eq!(original_v1.version, converted_v1.version);
    assert_eq!(original_v1.description, converted_v1.description);
    assert_eq!(original_v1.dependencies.len(), converted_v1.dependencies.len());
}

#[test]
fn test_v2_to_unified_to_v2_roundtrip() {
    let original_v2 = create_test_v2_package();

    // V2 → Unified
    let unified: UnifiedPackage = original_v2.clone().into();

    // Unified → V2
    let converted_v2: V2Package = unified.try_into().unwrap();

    // Should be identical
    assert_eq!(original_v2.uri, converted_v2.uri);
    assert_eq!(original_v2.metadata.name, converted_v2.metadata.name);
    assert_eq!(original_v2.metadata.version, converted_v2.metadata.version);
    assert_eq!(original_v2.signature, converted_v2.signature);
}

#[test]
fn test_v1_to_v2_conversion_adds_rdf_metadata() {
    let v1_pkg = create_test_v1_package();

    // V1 → Unified → V2
    let unified: UnifiedPackage = v1_pkg.into();
    let v2_pkg: V2Package = unified.try_into().unwrap();

    // V2 should have RDF metadata
    assert!(!v2_pkg.triples.is_empty());
    assert!(v2_pkg.metadata.rdf_graph_uri.starts_with("http://ggen.io/pkg/"));

    // But no signature (v1 doesn't have signing)
    assert!(v2_pkg.signature.is_none());
}

#[test]
fn test_v2_to_v1_conversion_loses_signature() {
    let v2_pkg = create_test_v2_package_with_signature();

    // V2 → Unified → V1
    let unified: UnifiedPackage = v2_pkg.into();
    let v1_pkg: V1Package = unified.try_into().unwrap();

    // V1 doesn't have signature field
    // (conversion should succeed but log warning)
}
```

## Schema Versioning Strategy

```rust
// ggen-domain/src/marketplace/schema_version.rs

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SchemaVersion {
    V1,  // Tantivy-based marketplace
    V2,  // RDF-based marketplace
}

impl SchemaVersion {
    pub fn from_package(pkg: &UnifiedPackage) -> Self {
        match pkg.source_backend {
            BackendType::V1 => SchemaVersion::V1,
            BackendType::V2 => SchemaVersion::V2,
            _ => SchemaVersion::V2,  // Default to latest
        }
    }

    pub fn supports_cryptographic_signing(&self) -> bool {
        matches!(self, SchemaVersion::V2)
    }

    pub fn supports_rdf_metadata(&self) -> bool {
        matches!(self, SchemaVersion::V2)
    }

    pub fn supports_sparql_queries(&self) -> bool {
        matches!(self, SchemaVersion::V2)
    }
}
```

## Migration Utilities

```rust
// ggen-cli/src/cmds/marketplace/migrate.rs

/// Migrate v1 marketplace data to v2 format
#[verb]
fn migrate_to_v2(dry_run: bool, backup: bool) -> Result<MigrationOutput> {
    execute_async_verb(async move {
        let v1_backend = V1Adapter::new(load_v1_config()?)?;
        let v2_backend = V2Adapter::new(load_v2_config()?)?;

        // Get all v1 packages
        let v1_packages = v1_backend.list_all().await?;

        let mut migrated = 0;
        let mut failed = 0;

        for v1_pkg in v1_packages {
            // Convert to unified format
            let unified: UnifiedPackage = v1_pkg.into();

            // Convert to v2 format
            let v2_pkg: V2Package = match unified.try_into() {
                Ok(pkg) => pkg,
                Err(e) => {
                    tracing::error!("Failed to convert package: {}", e);
                    failed += 1;
                    continue;
                }
            };

            // Write to v2 backend (unless dry run)
            if !dry_run {
                v2_backend.publish_package(&v2_pkg).await?;
            }

            migrated += 1;
        }

        Ok(MigrationOutput {
            total: v1_packages.len(),
            migrated,
            failed,
            dry_run,
        })
    })
}
```

## Success Criteria

| Criterion | Target | Validation |
|-----------|--------|------------|
| Lossless v1→v2 conversion | 100% | All v1 fields preserved in v2 |
| Backward compatible v2→v1 | 100% | V2 converts to v1 (minus v2-only fields) |
| Roundtrip accuracy | 100% | v1→unified→v1 identical |
| RDF triple generation | 100% | All packages convertible to valid RDF |
| Schema validation | 100% | All v2 packages validate against ontology |
| Migration tool success | >99% | Automated migration succeeds for all packages |
