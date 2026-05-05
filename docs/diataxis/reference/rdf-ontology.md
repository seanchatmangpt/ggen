# Reference: ggen Marketplace RDF Ontology

This document provides a technical reference for the core RDF ontology used within the `ggen-marketplace`. All marketplace data is stored as RDF triples in `oxigraph`.

## Core Namespaces

- **GGEN:** `https://ggen.io/marketplace/` (Primary domain namespace)
- **RDF:** `http://www.w3.org/1999/02/22-rdf-syntax-ns#`
- **RDFS:** `http://www.w3.org/2000/01/rdf-schema#`
- **DC:** `http://purl.org/dc/elements/1.1/` (Dublin Core)
- **FOAF:** `http://xmlns.com/foaf/0.1/` (Friend of a Friend)
- **SKOS:** `http://www.w3.org/2004/02/skos/core#`
- **XSD:** `http://www.w3.org/2001/XMLSchema#`

## Key Classes

All classes fall under the `GGEN` namespace.

| Class Name         | Description |
|--------------------|-------------|
| `Package`          | Represents a software package published in the registry. |
| `PackageVersion`   | Represents a specific version of a package. |
| `Author`           | An individual or entity that authored a package. |
| `Dependency`       | A version-specific dependency link. |
| `License`          | Represents the licensing terms. |
| `Repository`       | Source code repository information. |
| `ValidationResult` | Represents an automated quality/security check result. |

## Key Properties

Properties map attributes to classes.

| Property Name    | Range       | Description |
|------------------|-------------|-------------|
| `packageId`      | `xsd:string`| The unique string identifier. |
| `name`           | `xsd:string`| The human-readable name of the package. |
| `hasVersion`     | `URI`       | Links a `Package` to a `PackageVersion`. |
| `hasDependency`  | `URI`       | Links a `PackageVersion` to a `Dependency`. |
| `hasAuthor`      | `URI`       | Links a `Package` to an `Author`. |
| `qualityScore`   | `xsd:integer`| The computed quality score of the package. |
| `trustTier`      | `xsd:string`| The security and trust classification. |
| `checksum`       | `xsd:string`| The cryptographic hash of the release artifact. |
| `signature`      | `xsd:string`| Ed25519 signature confirming provenance. |

## Pre-defined SPARQL Queries

The internal `Queries` module provides standardized SPARQL queries for operations such as:
- `all_packages()`: Retrieves all nodes of type `Package`.
- `search_by_name(name: &str)`: Case-insensitive search on the `name` property.
- `package_versions(package_id: &str)`: Iterates descending `hasVersion` links.
- `packages_by_quality(min_score: u32)`: Filters packages where `qualityScore >= min_score`.
- `trending_packages(limit: usize)`: Orders packages descending by download count.
