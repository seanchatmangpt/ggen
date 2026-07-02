# ggen-marketplace

Semantic, RDF-backed package management and marketplace plugin system for the `ggen` framework.

This crate manages the indexing, discovery, cryptographic verification, and installation of modular `ggen` generation packages. It uses an RDF (Resource Description Framework) data store with SPARQL query support to resolve semantic relationships, versions, and package dependencies.

## Features

- **RDF Semantic Registry**: Stores package metadata as RDF triples using [Oxigraph](https://github.com/oxigraph/oxigraph) as the local database (`RdfRegistry`).
- **SPARQL Discovery Engine**: Performs semantic and version search queries using W3C SPARQL standard queries (`SparqlSearchEngine`).
- **Cryptographic Trust Enforcement**: Integrates Ed25519 signature checks to verify the ownership, integrity, and origin of package bundles before installation.
- **Composition Receipts**: Records and signs the exact combination of packages utilized to generate workspace artifacts (`CompositionReceipt`).
- **DAG Version Migrations**: Tracks and executes upgrade paths and migrations between package versions using directed acyclic graphs (`Migrator`, `UpgradeEdge`).
- **GAT and HRTB Traits**: Uses Generic Associated Types (GATs) and Higher-Ranked Trait Bounds (HRTBs) to structure async repository operations without allocation or lifetime overheads.

## Usage

### 1. Registering and Querying Packages via SPARQL

```rust
use ggen_marketplace::marketplace::prelude::*;
use serde_json::json;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Initialize the semantic RDF registry
    let mut registry = RdfRegistry::new()?;

    // 2. Register a package manifest
    let manifest = Manifest {
        id: PackageId::new("math-solver", "1.2.0"),
        description: "Solves linear arithmetic constraints".to_string(),
        author: "Agent-Peter".to_string(),
        dependencies: vec![],
    };
    registry.register_package(&manifest).await?;

    // 3. Search for packages using SPARQL via the search engine
    let search_engine = SparqlSearchEngine::new(registry.store().clone());
    let query = r#"
        SELECT ?pkg ?desc ?ver WHERE {
            ?pkg rdf:type :Package ;
                 :description ?desc ;
                 :version ?ver .
            FILTER(CONTAINS(?desc, "solver"))
        }
    "#;
    
    let results = search_engine.query_sparql(query).await?;
    println!("Search results: {:?}", results);

    Ok(())
}
```

### 2. Validating and Installing a Package Bundle

```rust
use ggen_marketplace::marketplace::prelude::*;
use std::path::Path;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let package_path = Path::new("target/packages/math-solver-1.2.0.tar.gz");
    let install_destination = Path::new("lib/packages/");

    // 1. Validate package structure and cryptographic signature
    let validator = Validator::new();
    let verifier = MarketplaceVerifier::new(b"trusted-public-key-bytes");
    
    if validator.is_structurally_valid(package_path)? && verifier.verify_bundle(package_path)? {
        // 2. Install bundle to target workspace path
        let installer = Installer::new(install_destination);
        installer.install(package_path).await?;
        println!("Package installed successfully.");
    }

    Ok(())
}
```

### 3. Emitting a Composition Receipt for Audits

```rust
use ggen_marketplace::marketplace::prelude::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut receipt = CompositionReceipt::new("op-codegen-101");
    receipt.add_used_package(PackageId::new("math-solver", "1.2.0"));
    receipt.add_used_package(PackageId::new("printer-util", "0.4.1"));

    // Computes the combined receipt proof hash
    let hash = receipt.compute_proof_hash();
    println!("Composition Proof Hash: {}", hash);
    Ok(())
}
```

## Architecture

- **`src/marketplace/registry_rdf.rs`**: Core semantic data mapping from structures to RDF triples in `oxigraph`.
- **`src/marketplace/search_sparql.rs`**: SPARQL evaluator translating text queries to Oxigraph query syntax.
- **`src/marketplace/install.rs`**: Transactional unpacker for tar/zip bundles.
- **`src/marketplace/security.rs`**: PKI validation and Ed25519 signing keys verification.
- **`src/marketplace/composition_receipt.rs`**: Provenance receipt for active codegen environments.
- **`src/marketplace/migration.rs`**: Upgrade path evaluation and topology sorting of dependency versions.
