//! RDF Mapper: Bidirectional conversion between Package and RDF triples
//!
//! This module provides complete mapping between Package domain models
//! and RDF representations, ensuring data integrity and semantic richness.

use crate::error::{Error, Result};
use crate::models::{Package, PackageId, PackageMetadata, PackageVersion, ReleaseInfo};
use crate::ontology::{Classes, Namespaces, Properties};
use chrono::{DateTime, Utc};
use oxigraph::model::{GraphNameRef, Literal, NamedNode, QuadRef, Term};
use oxigraph::store::Store;
use std::sync::Arc;
use tracing::debug;

/// RDF Mapper for Package <-> RDF triple conversion
pub struct RdfMapper {
    store: Arc<Store>,
}

impl RdfMapper {
    /// Create a new RDF mapper
    pub fn new(store: Arc<Store>) -> Self {
        Self { store }
    }

    /// Convert a Package to RDF triples and insert into store
    ///
    /// Maps all package fields to semantic RDF predicates:
    /// - Metadata: id, name, description, authors, license, etc.
    /// - Versions: version history as RDF facts
    /// - Releases: release info with checksums and dates
    /// - Relationships: dependencies as semantic links
    pub async fn package_to_rdf(&self, package: &Package) -> Result<()> {
        debug!("Converting package {} to RDF", package.metadata.id);

        let package_uri = self.create_package_uri(&package.metadata.id)?;

        // Insert package type
        self.insert_triple(
            &package_uri,
            &Self::named_node(&format!("{}type", Namespaces::RDF))?,
            &Self::named_node(&Classes::package())?,
        )?;

        // Insert package ID
        self.insert_literal_triple(
            &package_uri,
            &Properties::package_id(),
            package.metadata.id.as_str(),
        )?;

        // Insert name
        self.insert_literal_triple(&package_uri, &Properties::name(), &package.metadata.name)?;

        // Insert description
        self.insert_literal_triple(
            &package_uri,
            &Properties::description(),
            &package.metadata.description,
        )?;

        // Insert authors
        for (idx, author) in package.metadata.authors.iter().enumerate() {
            let author_uri = Self::named_node(&format!(
                "{}packages/{}/authors/{}",
                Namespaces::GGEN,
                package.metadata.id,
                idx
            ))?;

            // Link package to author
            self.insert_triple(
                &package_uri,
                &Self::named_node(&Properties::has_author())?,
                &author_uri,
            )?;

            // Author name
            let author_name_pred = Self::named_node(&Properties::author_name())?;
            let author_name_lit = Literal::new_simple_literal(author);
            let quad = QuadRef::new(
                &author_uri,
                &author_name_pred,
                &author_name_lit,
                GraphNameRef::DefaultGraph,
            );
            self.store
                .insert(quad)
                .map_err(|e| Error::RegistryError(e.to_string()))?;
        }

        // Insert license
        self.insert_literal_triple(
            &package_uri,
            &Properties::license(),
            &package.metadata.license,
        )?;

        // Insert repository URL
        if let Some(ref repo) = package.metadata.repository {
            self.insert_literal_triple(&package_uri, &Properties::repository_url(), repo)?;
        }

        // Insert homepage URL
        if let Some(ref homepage) = package.metadata.homepage {
            self.insert_literal_triple(&package_uri, &Properties::homepage_url(), homepage)?;
        }

        // Insert keywords
        for keyword in &package.metadata.keywords {
            self.insert_literal_triple(&package_uri, &Properties::keywords(), keyword)?;
        }

        // Insert quality score
        if let Some(quality_score) = package.metadata.quality_score {
            let quality_pred = Self::named_node(&Properties::quality_score())?;
            let quality_lit =
                Literal::new_typed_literal(&quality_score.value().to_string(), xsd_integer());
            let quad = QuadRef::new(
                &package_uri,
                &quality_pred,
                &quality_lit,
                GraphNameRef::DefaultGraph,
            );
            self.store
                .insert(quad)
                .map_err(|e| Error::RegistryError(e.to_string()))?;
        }

        // Insert download count
        self.insert_integer_triple(
            &package_uri,
            &Properties::downloads(),
            package.metadata.downloads,
        )?;

        // Insert timestamps
        self.insert_datetime_triple(
            &package_uri,
            &Self::named_node(&Properties::created_at())?,
            &package.metadata.created_at,
        )?;
        self.insert_datetime_triple(
            &package_uri,
            &Self::named_node(&Properties::updated_at())?,
            &package.metadata.updated_at,
        )?;

        // Insert latest version
        self.insert_literal_triple(
            &package_uri,
            &format!("{}latestVersion", Namespaces::GGEN),
            package.latest_version.as_str(),
        )?;

        // Insert all versions
        for version in &package.versions {
            let version_uri = self.create_version_uri(&package.metadata.id, version)?;

            // Link package to version
            self.insert_triple(
                &package_uri,
                &Self::named_node(&Properties::has_version())?,
                &version_uri,
            )?;

            // Version type
            self.insert_triple(
                &version_uri,
                &Self::named_node(&format!("{}type", Namespaces::RDF))?,
                &Self::named_node(&Classes::package_version())?,
            )?;

            // Version string
            self.insert_literal_triple(&version_uri, &Properties::version(), version.as_str())?;
        }

        // Insert release information
        for (version, release) in &package.releases {
            let version_uri = self.create_version_uri(&package.metadata.id, version)?;

            // Release date
            self.insert_datetime_triple(
                &version_uri,
                &Self::named_node(&format!("{}releasedAt", Namespaces::GGEN))?,
                &release.released_at,
            )?;

            // Changelog
            self.insert_literal_triple(
                &version_uri,
                &format!("{}changelog", Namespaces::GGEN),
                &release.changelog,
            )?;

            // Checksum
            self.insert_literal_triple(&version_uri, &Properties::checksum(), &release.checksum)?;

            // Download URL
            self.insert_literal_triple(
                &version_uri,
                &format!("{}downloadUrl", Namespaces::GGEN),
                &release.download_url,
            )?;

            // Dependencies
            for (dep_idx, dep) in release.dependencies.iter().enumerate() {
                let dep_uri = Self::named_node(&format!(
                    "{}packages/{}/versions/{}/dependencies/{}",
                    Namespaces::GGEN,
                    package.metadata.id,
                    version,
                    dep_idx
                ))?;

                // Link version to dependency
                self.insert_triple(
                    &version_uri,
                    &Self::named_node(&Properties::has_dependency())?,
                    &dep_uri,
                )?;

                // Dependency type
                self.insert_triple(
                    &dep_uri,
                    &Self::named_node(&format!("{}type", Namespaces::RDF))?,
                    &Self::named_node(&Classes::dependency())?,
                )?;

                // Dependency package ID
                self.insert_literal_triple(&dep_uri, &Properties::package_id(), dep.id.as_str())?;

                // Dependency version requirement
                self.insert_literal_triple(
                    &dep_uri,
                    &format!("{}versionReq", Namespaces::GGEN),
                    &dep.version_req,
                )?;

                // Optional flag
                let optional_pred = Self::named_node(&format!("{}optional", Namespaces::GGEN))?;
                let optional_lit =
                    Literal::new_typed_literal(&dep.optional.to_string(), xsd_boolean());
                let quad = QuadRef::new(
                    &dep_uri,
                    &optional_pred,
                    &optional_lit,
                    GraphNameRef::DefaultGraph,
                );
                self.store
                    .insert(quad)
                    .map_err(|e| Error::RegistryError(e.to_string()))?;
            }
        }

        debug!(
            "Successfully converted package {} to RDF",
            package.metadata.id
        );
        Ok(())
    }

    /// Reconstruct a Package from RDF triples
    ///
    /// Queries the RDF store for all triples related to a package
    /// and reconstructs the complete Package domain model.
    pub async fn rdf_to_package(&self, package_id: &PackageId) -> Result<Package> {
        debug!("Reconstructing package {} from RDF", package_id);

        let package_uri = self.create_package_uri(package_id)?;

        // Query basic metadata
        let metadata = self
            .query_package_metadata(&package_uri, package_id)
            .await?;

        // Query versions
        let versions = self.query_package_versions(&package_uri).await?;
        if versions.is_empty() {
            return Err(Error::RegistryError(format!(
                "Package {} has no versions in RDF store",
                package_id
            )));
        }

        // Query latest version
        let latest_version = self.query_latest_version(&package_uri).await?;

        // Query release information for all versions
        let mut releases = indexmap::IndexMap::new();
        for version in &versions {
            if let Ok(release) = self.query_release_info(package_id, version).await {
                releases.insert(version.clone(), release);
            }
        }

        let package = Package {
            metadata,
            latest_version,
            versions,
            releases,
        };

        debug!("Successfully reconstructed package {} from RDF", package_id);
        Ok(package)
    }

    /// Query package metadata from RDF store
    async fn query_package_metadata(
        &self, package_uri: &NamedNode, package_id: &PackageId,
    ) -> Result<PackageMetadata> {
        // Query for all metadata fields
        let query = format!(
            r#"
            SELECT ?name ?desc ?license ?repo ?homepage ?quality ?downloads ?created ?updated WHERE {{
                <{}> <{}> ?name .
                <{}> <{}> ?desc .
                <{}> <{}> ?license .
                OPTIONAL {{ <{}> <{}> ?repo }}
                OPTIONAL {{ <{}> <{}> ?homepage }}
                OPTIONAL {{ <{}> <{}> ?quality }}
                OPTIONAL {{ <{}> <{}> ?downloads }}
                OPTIONAL {{ <{}> <{}> ?created }}
                OPTIONAL {{ <{}> <{}> ?updated }}
            }}
            "#,
            package_uri.as_str(),
            Properties::name(),
            package_uri.as_str(),
            Properties::description(),
            package_uri.as_str(),
            Properties::license(),
            package_uri.as_str(),
            Properties::repository_url(),
            package_uri.as_str(),
            Properties::homepage_url(),
            package_uri.as_str(),
            Properties::quality_score(),
            package_uri.as_str(),
            Properties::downloads(),
            package_uri.as_str(),
            Properties::created_at(),
            package_uri.as_str(),
            Properties::updated_at(),
        );

        // Extract data from results without holding across await
        let (
            name,
            description,
            license,
            repository,
            homepage,
            quality_score,
            downloads,
            created_at,
            updated_at,
        ) = {
            let results = self
                .store
                .query(&query)
                .map_err(|e| Error::SearchError(format!("Metadata query failed: {}", e)))?;

            if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = results {
                if let Some(Ok(solution)) = solutions.next() {
                    let name = Self::extract_literal(&solution, "name")?;
                    let description = Self::extract_literal(&solution, "desc")?;
                    let license = Self::extract_literal(&solution, "license")?;
                    let repository = Self::extract_optional_literal(&solution, "repo");
                    let homepage = Self::extract_optional_literal(&solution, "homepage");

                    let quality_score = Self::extract_optional_literal(&solution, "quality")
                        .and_then(|s| s.parse::<u32>().ok())
                        .and_then(|v| crate::models::QualityScore::new(v).ok());

                    let downloads = Self::extract_optional_literal(&solution, "downloads")
                        .and_then(|s| s.parse::<u64>().ok())
                        .unwrap_or(0);

                    let created_at = Self::extract_optional_literal(&solution, "created")
                        .and_then(|s| DateTime::parse_from_rfc3339(&s).ok())
                        .map(|dt| dt.with_timezone(&Utc))
                        .unwrap_or_else(Utc::now);

                    let updated_at = Self::extract_optional_literal(&solution, "updated")
                        .and_then(|s| DateTime::parse_from_rfc3339(&s).ok())
                        .map(|dt| dt.with_timezone(&Utc))
                        .unwrap_or_else(Utc::now);

                    (
                        name,
                        description,
                        license,
                        repository,
                        homepage,
                        quality_score,
                        downloads,
                        created_at,
                        updated_at,
                    )
                } else {
                    return Err(Error::PackageNotFound {
                        package_id: package_id.to_string(),
                    });
                }
            } else {
                return Err(Error::PackageNotFound {
                    package_id: package_id.to_string(),
                });
            }
        };

        // Query authors and keywords (after results dropped)
        let authors = self.query_authors(package_uri).await?;
        let keywords = self.query_keywords(package_uri).await?;

        let mut metadata = PackageMetadata::new(package_id.clone(), name, description, license);
        metadata.authors = authors;
        metadata.repository = repository;
        metadata.homepage = homepage;
        metadata.keywords = keywords;
        metadata.quality_score = quality_score;
        metadata.downloads = downloads;
        metadata.created_at = created_at;
        metadata.updated_at = updated_at;

        Ok(metadata)
    }

    /// Query package versions
    async fn query_package_versions(&self, package_uri: &NamedNode) -> Result<Vec<PackageVersion>> {
        let query = format!(
            r#"
            SELECT ?version WHERE {{
                <{}> <{}> ?versionUri .
                ?versionUri <{}> ?version .
            }}
            ORDER BY DESC(?version)
            "#,
            package_uri.as_str(),
            Properties::has_version(),
            Properties::version(),
        );

        let results = self
            .store
            .query(&query)
            .map_err(|e| Error::SearchError(format!("Version query failed: {}", e)))?;

        let mut versions = Vec::new();
        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                if let Ok(solution) = solution {
                    if let Some(version_str) = Self::extract_optional_literal(&solution, "version")
                    {
                        if let Ok(version) = PackageVersion::new(version_str) {
                            versions.push(version);
                        }
                    }
                }
            }
        }

        Ok(versions)
    }

    /// Query latest version
    async fn query_latest_version(&self, package_uri: &NamedNode) -> Result<PackageVersion> {
        let query = format!(
            r#"
            SELECT ?latestVersion WHERE {{
                <{}> <{}latestVersion> ?latestVersion .
            }}
            "#,
            package_uri.as_str(),
            Namespaces::GGEN,
        );

        let results = self
            .store
            .query(&query)
            .map_err(|e| Error::SearchError(format!("Latest version query failed: {}", e)))?;

        if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = results {
            if let Some(Ok(solution)) = solutions.next() {
                if let Some(version_str) =
                    Self::extract_optional_literal(&solution, "latestVersion")
                {
                    return PackageVersion::new(version_str);
                }
            }
        }

        Err(Error::RegistryError("Latest version not found".to_string()))
    }

    /// Query release information for a specific version
    async fn query_release_info(
        &self, package_id: &PackageId, version: &PackageVersion,
    ) -> Result<ReleaseInfo> {
        let version_uri = self.create_version_uri(package_id, version)?;

        let query = format!(
            r#"
            SELECT ?releasedAt ?changelog ?checksum ?downloadUrl WHERE {{
                <{}> <{}releasedAt> ?releasedAt .
                <{}> <{}changelog> ?changelog .
                <{}> <{}> ?checksum .
                <{}> <{}downloadUrl> ?downloadUrl .
            }}
            "#,
            version_uri.as_str(),
            Namespaces::GGEN,
            version_uri.as_str(),
            Namespaces::GGEN,
            version_uri.as_str(),
            Properties::checksum(),
            version_uri.as_str(),
            Namespaces::GGEN,
        );

        // Extract all data from the iterator before await to ensure Send
        // Use a block scope to ensure results is dropped before await
        let (released_at, changelog, checksum, download_url) = {
            let results = self
                .store
                .query(&query)
                .map_err(|e| Error::SearchError(format!("Release info query failed: {}", e)))?;

            match results {
                oxigraph::sparql::QueryResults::Solutions(mut solutions) => {
                    if let Some(Ok(solution)) = solutions.next() {
                        let released_at = Self::extract_optional_literal(&solution, "releasedAt")
                            .and_then(|s| DateTime::parse_from_rfc3339(&s).ok())
                            .map(|dt| dt.with_timezone(&Utc))
                            .unwrap_or_else(Utc::now);

                        let changelog = Self::extract_literal(&solution, "changelog")?;
                        let checksum = Self::extract_literal(&solution, "checksum")?;
                        let download_url = Self::extract_literal(&solution, "downloadUrl")?;

                        // Iterator and results are dropped here when block exits
                        Ok((released_at, changelog, checksum, download_url))
                    } else {
                        Err(Error::RegistryError(format!(
                            "Release info not found for version {}",
                            version
                        )))
                    }
                }
                _ => Err(Error::RegistryError(format!(
                    "Release info not found for version {}",
                    version
                ))),
            }
        }?;

        // Now we can await safely - results and iterator are dropped
        let dependencies = self.query_dependencies(&version_uri).await?;

        Ok(ReleaseInfo {
            version: version.clone(),
            released_at,
            changelog,
            checksum,
            download_url,
            dependencies,
        })
    }

    /// Query authors for a package
    async fn query_authors(&self, package_uri: &NamedNode) -> Result<Vec<String>> {
        let query = format!(
            r#"
            SELECT ?authorName WHERE {{
                <{}> <{}> ?author .
                ?author <{}> ?authorName .
            }}
            "#,
            package_uri.as_str(),
            Properties::has_author(),
            Properties::author_name(),
        );

        self.query_string_list(&query, "authorName").await
    }

    /// Query keywords for a package
    async fn query_keywords(&self, package_uri: &NamedNode) -> Result<Vec<String>> {
        let query = format!(
            r#"
            SELECT ?keyword WHERE {{
                <{}> <{}> ?keyword .
            }}
            "#,
            package_uri.as_str(),
            Properties::keywords(),
        );

        self.query_string_list(&query, "keyword").await
    }

    /// Query dependencies for a version
    async fn query_dependencies(
        &self, version_uri: &NamedNode,
    ) -> Result<Vec<crate::models::PackageDependency>> {
        let query = format!(
            r#"
            SELECT ?depId ?versionReq ?optional WHERE {{
                <{}> <{}> ?dep .
                ?dep <{}> ?depId .
                ?dep <{}versionReq> ?versionReq .
                ?dep <{}optional> ?optional .
            }}
            "#,
            version_uri.as_str(),
            Properties::has_dependency(),
            Properties::package_id(),
            Namespaces::GGEN,
            Namespaces::GGEN,
        );

        let results = self
            .store
            .query(&query)
            .map_err(|e| Error::SearchError(format!("Dependencies query failed: {}", e)))?;

        let mut dependencies = Vec::new();
        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                if let Ok(solution) = solution {
                    let dep_id_str = Self::extract_literal(&solution, "depId")?;
                    let version_req = Self::extract_literal(&solution, "versionReq")?;
                    let optional = Self::extract_optional_literal(&solution, "optional")
                        .and_then(|s| s.parse::<bool>().ok())
                        .unwrap_or(false);

                    if let Ok(dep_id) = PackageId::new(dep_id_str) {
                        dependencies.push(crate::models::PackageDependency {
                            id: dep_id,
                            version_req,
                            optional,
                        });
                    }
                }
            }
        }

        Ok(dependencies)
    }

    /// Helper: Query list of strings
    async fn query_string_list(&self, query: &str, var_name: &str) -> Result<Vec<String>> {
        let results = self
            .store
            .query(query)
            .map_err(|e| Error::SearchError(format!("Query failed: {}", e)))?;

        let mut strings = Vec::new();
        if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
            for solution in solutions {
                if let Ok(solution) = solution {
                    if let Some(s) = Self::extract_optional_literal(&solution, var_name) {
                        strings.push(s);
                    }
                }
            }
        }

        Ok(strings)
    }

    // Helper methods for RDF operations

    fn create_package_uri(&self, package_id: &PackageId) -> Result<NamedNode> {
        NamedNode::new(format!("{}packages/{}", Namespaces::GGEN, package_id))
            .map_err(|e| Error::RegistryError(format!("Invalid package URI: {}", e)))
    }

    fn create_version_uri(
        &self, package_id: &PackageId, version: &PackageVersion,
    ) -> Result<NamedNode> {
        NamedNode::new(format!(
            "{}packages/{}/versions/{}",
            Namespaces::GGEN,
            package_id,
            version
        ))
        .map_err(|e| Error::RegistryError(format!("Invalid version URI: {}", e)))
    }

    fn named_node(uri: &str) -> Result<NamedNode> {
        NamedNode::new(uri).map_err(|e| Error::RegistryError(format!("Invalid URI {}: {}", uri, e)))
    }

    fn insert_triple(
        &self, subject: &NamedNode, predicate: &NamedNode, object: &NamedNode,
    ) -> Result<()> {
        let quad = QuadRef::new(subject, predicate, object, GraphNameRef::DefaultGraph);
        self.store
            .insert(quad)
            .map_err(|e| Error::RegistryError(format!("Failed to insert triple: {}", e)))
    }

    fn insert_literal_triple(
        &self, subject: &NamedNode, predicate_uri: &str, value: &str,
    ) -> Result<()> {
        let predicate = Self::named_node(predicate_uri)?;
        let literal = Literal::new_simple_literal(value);
        let quad = QuadRef::new(subject, &predicate, &literal, GraphNameRef::DefaultGraph);
        self.store
            .insert(quad)
            .map_err(|e| Error::RegistryError(format!("Failed to insert literal: {}", e)))
    }

    fn insert_integer_triple(
        &self, subject: &NamedNode, predicate_uri: &str, value: u64,
    ) -> Result<()> {
        let predicate = Self::named_node(predicate_uri)?;
        let literal = Literal::new_typed_literal(&value.to_string(), xsd_integer());
        let quad = QuadRef::new(subject, &predicate, &literal, GraphNameRef::DefaultGraph);
        self.store
            .insert(quad)
            .map_err(|e| Error::RegistryError(format!("Failed to insert integer: {}", e)))
    }

    fn insert_datetime_triple(
        &self, subject: &NamedNode, predicate: &NamedNode, datetime: &DateTime<Utc>,
    ) -> Result<()> {
        let literal = Literal::new_typed_literal(&datetime.to_rfc3339(), xsd_datetime());
        let quad = QuadRef::new(subject, predicate, &literal, GraphNameRef::DefaultGraph);
        self.store
            .insert(quad)
            .map_err(|e| Error::RegistryError(format!("Failed to insert datetime: {}", e)))
    }

    fn extract_literal(
        solution: &oxigraph::sparql::QuerySolution, var_name: &str,
    ) -> Result<String> {
        Self::extract_optional_literal(solution, var_name)
            .ok_or_else(|| Error::RegistryError(format!("Missing required field: {}", var_name)))
    }

    fn extract_optional_literal(
        solution: &oxigraph::sparql::QuerySolution, var_name: &str,
    ) -> Option<String> {
        solution.get(var_name).and_then(|term| {
            if let Term::Literal(lit) = term {
                Some(lit.value().to_string())
            } else {
                None
            }
        })
    }
}

// XSD datatype URIs
fn xsd_integer() -> NamedNode {
    NamedNode::new_unchecked("http://www.w3.org/2001/XMLSchema#integer")
}

fn xsd_boolean() -> NamedNode {
    NamedNode::new_unchecked("http://www.w3.org/2001/XMLSchema#boolean")
}

fn xsd_datetime() -> NamedNode {
    NamedNode::new_unchecked("http://www.w3.org/2001/XMLSchema#dateTime")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{PackageMetadata, PackageVersion};

    #[tokio::test]
    async fn test_package_to_rdf_basic() {
        let store = Arc::new(Store::new().unwrap());
        let mapper = RdfMapper::new(store);

        let id = PackageId::new("test-pkg").unwrap();
        let metadata = PackageMetadata::new(id.clone(), "Test Package", "A test", "MIT");
        let package = Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: indexmap::IndexMap::new(),
        };

        let result = mapper.package_to_rdf(&package).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_round_trip_conversion() {
        let store = Arc::new(Store::new().unwrap());
        let mapper = RdfMapper::new(store);

        let id = PackageId::new("roundtrip-test").unwrap();
        let mut metadata = PackageMetadata::new(id.clone(), "Round Trip", "Test package", "MIT");
        metadata.authors = vec!["Alice".to_string(), "Bob".to_string()];
        metadata.keywords = vec!["test".to_string(), "rdf".to_string()];

        let package = Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: indexmap::IndexMap::new(),
        };

        // Convert to RDF
        mapper.package_to_rdf(&package).await.unwrap();

        // Convert back from RDF
        let reconstructed = mapper.rdf_to_package(&id).await.unwrap();

        // Verify basic fields
        assert_eq!(reconstructed.metadata.id, package.metadata.id);
        assert_eq!(reconstructed.metadata.name, package.metadata.name);
        assert_eq!(
            reconstructed.metadata.description,
            package.metadata.description
        );
        assert_eq!(reconstructed.metadata.authors, package.metadata.authors);
        assert_eq!(reconstructed.versions, package.versions);
    }
}
