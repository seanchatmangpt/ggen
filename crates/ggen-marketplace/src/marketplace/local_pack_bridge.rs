//! Bridge from the local, on-disk pack registry into the marketplace domain model.
//!
//! Converts `packs_registry::types::Pack` (the data `ggen pack list`/`ggen
//! pack search` already read) into the `marketplace::models::Package` domain
//! model that `RdfRegistry`/`SparqlSearchEngine` operate on.
//!
//! These are two real, independently-tested data models in this crate that,
//! before this module existed, had no conversion path between them at all --
//! confirmed by grep: nothing in this crate or in `ggen-cli` ever constructed
//! a `marketplace::models::Package` from a `packs_registry::types::Pack`. That
//! meant `RdfRegistry`/`SparqlSearchEngine`'s real, tested keyword-overlap and
//! SKOS category-relation search had no way to ever see the packs a real
//! `ggen pack list` actually returns -- a search command built on top of it
//! would have been reachable but decorative (always searching an empty
//! registry). This module is the fix: a real, pure, fully-tested conversion,
//! with explicit `Err` (not silent skip, not `unwrap`) on inputs the target
//! model genuinely cannot represent.

use crate::marketplace::error::Result;
use crate::marketplace::models::{Package, PackageId, PackageMetadata, PackageVersion};

/// Convert one locally-installed pack into the marketplace `Package` domain
/// model, suitable for `RdfRegistry::create_package`/`update_package`.
///
/// # Errors
///
/// * [`Error::InvalidPackageId`] - `pack.id` is not a valid [`PackageId`]
/// * [`Error::InvalidVersion`] - `pack.version` does not parse as
///   MAJOR.MINOR.PATCH semver (the target model's `PackageVersion` requires
///   it; the source `Pack.version` does not). Real ggen packs mostly already
///   use dotted 3-part versions (including the `YY.M.D`-shaped release
///   versions this workspace itself uses, e.g. `26.7.13`), but this is not
///   guaranteed for every locally-installed pack, so callers ingesting many
///   packs at once (see `ggen pack related`) should treat one pack's
///   conversion failure as a per-pack skip-and-warn, not a fatal error for
///   the whole batch.
pub fn local_pack_to_marketplace_package(pack: &crate::packs_registry::types::Pack) -> Result<Package> {
    let id = PackageId::new(&pack.id)?;
    let version = PackageVersion::new(&pack.version)?;

    let mut metadata = PackageMetadata::new(
        id,
        pack.name.clone(),
        pack.description.clone(),
        pack.license.clone().unwrap_or_else(|| "UNKNOWN".to_string()),
    );
    metadata.authors = pack.author.clone().into_iter().collect();
    metadata.repository = pack.repository.clone();
    metadata.keywords = pack.keywords.clone();
    // `Pack.category` is a single required String field on the source model;
    // an empty string means "uncategorized" in that model's own convention
    // (see `packs_registry::types::Pack`'s own construction sites), which
    // must not become a spurious `""`-named category node on the target
    // side.
    metadata.categories = if pack.category.trim().is_empty() {
        Vec::new()
    } else {
        vec![pack.category.clone()]
    };
    metadata.registry_type = pack
        .registry_type
        .as_deref()
        .map(parse_registry_type)
        .unwrap_or_default();

    Ok(Package {
        metadata,
        latest_version: version.clone(),
        versions: vec![version],
        releases: indexmap::IndexMap::new(),
    })
}

fn parse_registry_type(s: &str) -> crate::marketplace::trust::RegistryType {
    match s {
        "crates.io" => crate::marketplace::trust::RegistryType::CratesIo,
        "npm" => crate::marketplace::trust::RegistryType::Npm,
        "pypi" => crate::marketplace::trust::RegistryType::PyPi,
        "github" => crate::marketplace::trust::RegistryType::GitHub,
        _ => crate::marketplace::trust::RegistryType::Ggen,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs_registry::types::{Pack, PackMetadata};
    use std::collections::HashMap;

    fn sample_pack() -> Pack {
        Pack {
            id: "web-starter-pack".to_string(),
            name: "Web Starter Pack".to_string(),
            version: "1.2.3".to_string(),
            description: "A starter pack for web projects".to_string(),
            category: "web".to_string(),
            author: Some("Jane Dev".to_string()),
            repository: Some("https://github.com/example/web-starter".to_string()),
            license: Some("MIT".to_string()),
            registry_type: None,
            packages: vec!["core".to_string()],
            templates: vec![],
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec!["web".to_string()],
            keywords: vec!["web".to_string(), "starter".to_string()],
            production_ready: true,
            metadata: PackMetadata::default(),
        }
    }

    #[test]
    fn converts_real_fields_across_the_two_domain_models() {
        let pack = sample_pack();
        let package = local_pack_to_marketplace_package(&pack)
            .expect("a well-formed local pack must convert successfully");

        assert_eq!(package.metadata.id.to_string(), "web-starter-pack");
        assert_eq!(package.metadata.name, "Web Starter Pack");
        assert_eq!(package.metadata.description, "A starter pack for web projects");
        assert_eq!(package.metadata.license, "MIT");
        assert_eq!(package.metadata.authors, vec!["Jane Dev".to_string()]);
        assert_eq!(
            package.metadata.repository,
            Some("https://github.com/example/web-starter".to_string())
        );
        assert_eq!(
            package.metadata.keywords,
            vec!["web".to_string(), "starter".to_string()]
        );
        assert_eq!(package.metadata.categories, vec!["web".to_string()]);
        assert_eq!(package.latest_version.as_str(), "1.2.3");
        assert_eq!(package.versions, vec![package.latest_version.clone()]);
    }

    #[test]
    fn empty_category_string_converts_to_zero_categories_not_one_blank_category() {
        let mut pack = sample_pack();
        pack.category = String::new();

        let package = local_pack_to_marketplace_package(&pack).expect("must convert");

        assert!(
            package.metadata.categories.is_empty(),
            "an empty source category string must not become a spurious blank category, got {:?}",
            package.metadata.categories
        );
    }

    #[test]
    fn missing_license_falls_back_to_unknown_rather_than_failing() {
        let mut pack = sample_pack();
        pack.license = None;

        let package = local_pack_to_marketplace_package(&pack).expect("must still convert");
        assert_eq!(package.metadata.license, "UNKNOWN");
    }

    #[test]
    fn non_semver_version_is_a_real_error_not_a_panic() {
        let mut pack = sample_pack();
        pack.version = "not-a-version".to_string();

        let result = local_pack_to_marketplace_package(&pack);
        assert!(
            result.is_err(),
            "a non-semver source version must surface as a real Err, not panic or silently \
             coerce to something else"
        );
    }

    #[test]
    fn invalid_package_id_is_a_real_error_not_a_panic() {
        let mut pack = sample_pack();
        pack.id = String::new();

        let result = local_pack_to_marketplace_package(&pack);
        assert!(
            result.is_err(),
            "an empty pack id must surface as a real Err from PackageId::new, not panic"
        );
    }

    /// End-to-end: convert a real local pack, ingest it into a real
    /// `RdfRegistry`, and confirm it is actually findable via the real SPARQL
    /// keyword-overlap search -- proving the bridge doesn't just produce a
    /// well-typed `Package` in isolation, but one the rest of the stack can
    /// genuinely use.
    #[tokio::test]
    async fn converted_package_is_findable_via_real_registry_search() {
        use crate::marketplace::registry_rdf::RdfRegistry;

        let registry = RdfRegistry::new();
        let package = local_pack_to_marketplace_package(&sample_pack())
            .expect("sample pack must convert");
        registry
            .create_package(package)
            .await
            .expect("converted package must insert into a real RdfRegistry");

        let results = registry
            .search_related_by_keywords(&["web".to_string()], 10)
            .expect("real delegated SPARQL search must succeed");
        assert!(
            results.iter().any(|p| p.contains("web-starter-pack")),
            "the converted+ingested pack must be findable by its real keywords, got {:?}",
            results
        );

        let by_category = registry
            .search_related_by_category("web", 10)
            .expect("real delegated SKOS-expansion search must succeed");
        assert!(
            by_category.iter().any(|p| p.contains("web-starter-pack")),
            "the converted+ingested pack must be findable by its real category, got {:?}",
            by_category
        );
    }
}
