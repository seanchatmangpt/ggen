<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Marketplace Packages Guide](#marketplace-packages-guide)
  - [Package Structure (package.toml)](#package-structure-packagetoml)
  - [RDF-Backed Package Registry](#rdf-backed-package-registry)
    - [Package RDF Model](#package-rdf-model)
  - [SPARQL Package Search](#sparql-package-search)
    - [Full-Text Search](#full-text-search)
    - [Filter by Framework](#filter-by-framework)
    - [Dependency Resolution](#dependency-resolution)
  - [Version Resolution](#version-resolution)
  - [Ed25519 Signing & Verification](#ed25519-signing--verification)
    - [Key Management](#key-management)
  - [Package Installation](#package-installation)
  - [Critical Rules](#critical-rules)
  - [Examples](#examples)
    - [Install Package from Marketplace](#install-package-from-marketplace)
    - [Search and Install with Dependency Resolution](#search-and-install-with-dependency-resolution)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Marketplace Packages Guide

## Package Structure (package.toml)

Each ggen package declares a `package.toml` manifest:

```toml
[package]
id = "http://marketplace.ggen.io/templates/rust-api"
name = "rust-api"
version = "1.0.5"
edition = "2021"
authors = ["ggen-team <team@ggen.io>"]
description = "Production-ready Rust REST API with Axum, PostgreSQL, and JWT auth"
repository = "https://github.com/seanchatmangpt/ggen"

[rdf]
ontology_uri = "http://vocab.ggen.io/template"
framework = "axum"
language = "rust"
pattern = "full-stack"

[metadata]
category = "api"
tags = ["rest", "async", "database", "auth"]
keywords = ["axum", "postgresql", "jwt", "docker"]
popularity_score = 9.2
verified = true
license = "Apache-2.0"

[[dependencies]]
package = "base-rust"
version = ">=1.0.0"

[[dependencies]]
package = "error-handling"
version = ">=1.0.0"

[[dependencies]]
package = "database-postgres"
version = ">=2.0.0"

[lifecycle]
install_command = "cargo make setup"
validate_command = "cargo make test"
cleanup_command = "cargo make clean"
```

## RDF-Backed Package Registry

The marketplace is an RDF graph where each package is a NamedNode with properties:

```sparql
SELECT ?pkg ?name ?version ?framework ?verified ?popularity
WHERE {
  ?pkg a ggen:Package .
  ?pkg rdfs:label ?name .
  ?pkg dcat:version ?version .
  ?pkg ggen:framework ?framework .
  ?pkg ggen:verified ?verified .
  ?pkg ggen:popularity ?popularity .
}
ORDER BY DESC(?popularity)
LIMIT 50
```

### Package RDF Model

```
<http://marketplace.ggen.io/packages/rust-api>
  a ggen:Package ;
  rdfs:label "rust-api" ;
  rdfs:comment "Production-ready Rust REST API" ;
  dcat:version "1.0.5" ;
  dcat:downloadURL <http://marketplace.ggen.io/downloads/rust-api-1.0.5.tar.gz> ;
  ggen:framework "axum" ;
  ggen:language "rust" ;
  ggen:verified true ;
  ggen:popularity "9.2" ;
  dcat:keyword "rest", "async", "auth" ;
  ggen:dependsOn <http://marketplace.ggen.io/packages/base-rust> ;
  foaf:maker <http://ggen.io/users/team> ;
  dcat:issued "2024-01-15"^^xsd:date .
```

## SPARQL Package Search

### Full-Text Search

```rust
pub fn search_packages(
    store: &Store,
    query: &str,
    limit: usize,
) -> Result<Vec<Package>> {
    let sparql = r#"
        SELECT ?pkg ?name ?version ?framework ?popularity
        WHERE {
            ?pkg a ggen:Package .
            ?pkg rdfs:label ?name .
            ?pkg dcat:version ?version .
            ?pkg ggen:framework ?framework .
            ?pkg ggen:popularity ?popularity .
            FILTER (CONTAINS(LCASE(?name), LCASE(?searchQuery)))
        }
        ORDER BY DESC(?popularity)
        LIMIT ?maxResults
    "#;

    let solutions = store.query(sparql)?;
    Ok(solutions.into_iter()
        .take(limit)
        .map(Package::from_solution)
        .collect())
}
```

### Filter by Framework

```rust
pub fn packages_by_framework(
    store: &Store,
    framework: &str,
) -> Result<Vec<Package>> {
    let sparql = r#"
        SELECT ?pkg ?name ?version ?description
        WHERE {
            ?pkg a ggen:Package .
            ?pkg rdfs:label ?name .
            ?pkg dcat:version ?version .
            ?pkg rdfs:comment ?description .
            ?pkg ggen:framework ?targetFramework .
            FILTER (?targetFramework = ?fw)
        }
        ORDER BY ?name
    "#;

    store.query(sparql).map(|solutions| {
        solutions.into_iter()
            .map(Package::from_solution)
            .collect()
    })
}
```

### Dependency Resolution

```rust
pub fn resolve_dependencies(
    store: &Store,
    pkg_id: &str,
) -> Result<Vec<Package>> {
    let sparql = r#"
        SELECT ?dep ?name ?version
        WHERE {
            <?packageId> ggen:dependsOn ?dep .
            ?dep rdfs:label ?name .
            ?dep dcat:version ?version .
        }
    "#;

    store.query(sparql).map(|solutions| {
        solutions.into_iter()
            .map(Package::from_solution)
            .collect()
    })
}
```

## Version Resolution

Packages declare semver constraints:

```rust
pub struct PackageDependency {
    pub package_id: String,
    pub version_constraint: String, // ">=1.0.0", "^2.1.0", "1.5.*"
}

pub fn resolve_compatible_version(
    store: &Store,
    package_id: &str,
    constraint: &str,
) -> Result<String> {
    // Query all published versions
    let sparql = r#"
        SELECT ?version
        WHERE {
            <package_id> dcat:version ?version .
        }
        ORDER BY DESC(?version)
    "#;

    let versions: Vec<String> = store.query(sparql)?
        .into_iter()
        .map(|s| s.get("version").unwrap_or_default())
        .collect();

    // Find first version matching constraint
    versions.into_iter()
        .find(|v| satisfies_constraint(v, constraint))
        .ok_or_else(|| "No compatible version found".into())
}

fn satisfies_constraint(version: &str, constraint: &str) -> bool {
    // Implement semver matching logic
    // ">=1.0.0" -> matches 1.0.0, 1.0.1, 1.1.0, 2.0.0, etc.
    // "^2.1.0" -> matches 2.1.0, 2.1.1, 2.9.9, but not 3.0.0
    // "1.5.*" -> matches 1.5.0, 1.5.1, 1.5.99, but not 1.6.0
    semver_match(version, constraint)
}
```

## Ed25519 Signing & Verification

Packages are cryptographically signed to ensure integrity:

```rust
use ed25519_dalek::{Keypair, PublicKey, Signature};
use std::fs;

pub struct SignedPackage {
    pub id: String,
    pub manifest: PackageManifest,
    pub signature: String, // base64-encoded
    pub public_key: String, // base64-encoded
}

pub fn sign_package(
    manifest: &PackageManifest,
    private_key: &Keypair,
) -> Result<SignedPackage> {
    let manifest_bytes = serde_json::to_vec(manifest)?;
    let signature = private_key.sign(&manifest_bytes);

    Ok(SignedPackage {
        id: manifest.id.clone(),
        manifest: manifest.clone(),
        signature: base64::encode(&signature.to_bytes()),
        public_key: base64::encode(&private_key.public.to_bytes()),
    })
}

pub fn verify_package(package: &SignedPackage) -> Result<bool> {
    let manifest_bytes = serde_json::to_vec(&package.manifest)?;
    let public_key_bytes = base64::decode(&package.public_key)?;
    let signature_bytes = base64::decode(&package.signature)?;

    let public_key = PublicKey::from_bytes(&public_key_bytes)
        .map_err(|e| format!("Invalid public key: {}", e))?;

    let signature = Signature::from_bytes(&signature_bytes)
        .map_err(|e| format!("Invalid signature: {}", e))?;

    public_key.verify_strict(&manifest_bytes, &signature)
        .map(|_| true)
        .map_err(|e| format!("Signature verification failed: {}", e))
}
```

### Key Management

```rust
pub fn generate_keypair() -> Result<(String, String)> {
    use rand::rngs::OsRng;

    let mut rng = OsRng;
    let keypair = Keypair::generate(&mut rng);

    Ok((
        base64::encode(&keypair.secret.to_bytes()),
        base64::encode(&keypair.public.to_bytes()),
    ))
}

pub fn load_private_key(key_file: &Path) -> Result<Keypair> {
    let contents = fs::read_to_string(key_file)?;
    let bytes = base64::decode(contents.trim())?;
    Keypair::from_bytes(&bytes)
        .map_err(|e| format!("Failed to load keypair: {}", e))
}
```

## Package Installation

Packages are installed via download and validation:

```rust
pub async fn install_package(
    package_id: &str,
    version: &str,
    install_dir: &Path,
) -> Result<()> {
    // 1. Download package archive
    let url = format!(
        "https://marketplace.ggen.io/downloads/{}-{}.tar.gz",
        package_id, version
    );
    let archive = download_package(&url).await?;

    // 2. Extract to install directory
    extract_tar_gz(&archive, install_dir)?;

    // 3. Validate signature
    let manifest = load_manifest(&install_dir.join("package.toml"))?;
    let signature_file = install_dir.join(".package-signature");
    verify_signature(&manifest, &signature_file)?;

    // 4. Run install command
    let install_command = manifest.lifecycle.install_command;
    run_command(&install_command, install_dir)?;

    Ok(())
}
```

## Critical Rules

1. **ALWAYS sign packages before publishing** - Use Ed25519 signatures
2. **ALWAYS verify signatures before installation** - Never skip verification
3. **DECLARE all dependencies** - Missing dependencies break installations
4. **USE semver versioning** - Follow semantic versioning (major.minor.patch)
5. **VALIDATE package.toml structure** - Use schema validation on load
6. **TEST installed packages** - Run validate_command after installation
7. **DOCUMENT breaking changes** - Include migration guides in descriptions
8. **LIMIT dependency depth** - Avoid circular or excessive transitive dependencies

---

## Examples

### Install Package from Marketplace

```rust
#[tokio::test]
async fn test_package_installation() {
    // Arrange
    let install_dir = Path::new("/tmp/packages");
    fs::create_dir_all(install_dir).unwrap();

    // Act
    install_package("rust-api", "1.0.5", install_dir)
        .await
        .unwrap();

    // Assert
    assert!(install_dir.join("Cargo.toml").exists());
    assert!(install_dir.join("src/main.rs").exists());
}
```

### Search and Install with Dependency Resolution

```rust
pub async fn install_with_dependencies(
    store: &Store,
    package_id: &str,
    version: &str,
    install_dir: &Path,
) -> Result<()> {
    // 1. Resolve all dependencies
    let mut to_install = vec![(package_id.to_string(), version.to_string())];
    let mut installed = HashSet::new();

    while let Some((pkg_id, pkg_version)) = to_install.pop() {
        if installed.contains(&pkg_id) {
            continue;
        }

        // Get dependencies
        let deps = resolve_dependencies(store, &pkg_id)?;
        for dep in deps {
            to_install.push((dep.id, dep.version));
        }

        // Install this package
        install_package(&pkg_id, &pkg_version, install_dir).await?;
        installed.insert(pkg_id);
    }

    Ok(())
}
```
