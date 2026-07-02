#![allow(clippy::expect_used, clippy::panic)]
//! Namespace consistency regression tests (P0-03).
//!
//! ## What this proves
//!
//! The marketplace had competing RDF property-naming systems that shared the
//! same `MARKETPLACE_NS` graph namespace but used incompatible local property
//! names. The most insidious instance: the enum-based ontology resolved
//! `Property::PackageName` to `http://purl.org/dc/terms/title` (`dc:title`),
//! while the production insert path (`rdf_mapper::package_to_rdf`) and the
//! string-based `Properties::name()` helper wrote package names under
//! `https://ggen.io/marketplace/name`. A SPARQL `SELECT` built with the
//! enum therefore queried `dc:title` against triples stored under `ggen:name`
//! and silently returned ZERO rows — data loss with no error.
//!
//! ## Chicago TDD
//!
//! These tests use a REAL `oxigraph::store::Store` and REAL SPARQL execution
//! (no mocks, no test doubles). The INSERT side is built from the canonical
//! `Properties`/`Classes` helpers (the single source of truth), and the SELECT
//! side is built by the actual query builders (`MarketplaceQueries`) and the
//! enum (`Property`). Assertions are on observable query results
//! (solution counts) and on the property-URI strings each side emits.
//!
//! Before the fix, `test_inserted_package_is_findable_by_query_builder` and
//! `test_package_name_uri_agrees_across_insert_and_query_sides` FAIL (empty
//! results / `dc:title` mismatch). After the fix they PASS.

use ggen_marketplace::marketplace::ontology::{Classes, Properties, MARKETPLACE_NS};
use ggen_marketplace::marketplace::rdf::ontology::Property;
use ggen_marketplace::marketplace::rdf::sparql_queries::{MarketplaceQueries, SearchParams};
use oxigraph::sparql::{QueryResults, SparqlEvaluator};
use oxigraph::store::Store;

/// Build a canonical package URI exactly as the production code does.
fn package_uri(id: &str) -> String {
    format!("{MARKETPLACE_NS}{id}")
}

/// Insert one package into a real oxigraph store using ONLY the canonical
/// `Properties`/`Classes` helpers. This mirrors `rdf_mapper::package_to_rdf`
/// (the production insert path): package node + version node, all predicates
/// derived from the single source of truth.
fn insert_canonical_package(store: &Store, id: &str, name: &str, description: &str, version: &str) {
    let pkg = package_uri(id);
    let version_node = format!("{MARKETPLACE_NS}{id}/{version}");

    // Every predicate URI comes from the canonical helper — if the helper and
    // the enum/query side ever diverge again, this test breaks.
    let insert = format!(
        r#"
        INSERT DATA {{
            <{pkg}> a <{pkg_class}> ;
                <{p_id}> "{id}" ;
                <{p_name}> "{name}" ;
                <{p_desc}> "{description}" ;
                <{p_downloads}> "42"^^<http://www.w3.org/2001/XMLSchema#integer> ;
                <{p_created}> "2026-01-01T00:00:00Z"^^<http://www.w3.org/2001/XMLSchema#dateTime> ;
                <{p_has_version}> <{version_node}> .
            <{version_node}> a <{version_class}> ;
                <{p_version}> "{version}" .
        }}
        "#,
        pkg = pkg,
        pkg_class = Classes::package(),
        version_class = Classes::package_version(),
        version_node = version_node,
        p_id = Properties::package_id(),
        p_name = Properties::name(),
        p_desc = Properties::description(),
        p_downloads = Properties::downloads(),
        p_created = Properties::created_at(),
        p_has_version = Properties::has_version(),
        p_version = Properties::version(),
        id = id,
        name = name,
        description = description,
        version = version,
    );

    store
        .update(&insert)
        .expect("canonical INSERT DATA must succeed against a real store");
}

/// Run a SELECT and count the solution rows (real SPARQL evaluation).
fn count_solutions(store: &Store, query: &str) -> usize {
    let results = SparqlEvaluator::new()
        .parse_query(query)
        .expect("query must parse")
        .on_store(store)
        .execute()
        .expect("query must execute");
    match results {
        QueryResults::Solutions(solutions) => {
            let mut count = 0usize;
            for solution in solutions {
                solution.expect("valid solution");
                count += 1;
            }
            count
        }
        _ => panic!("expected SELECT solutions from query:\n{query}"),
    }
}

/// CORE REGRESSION: a package inserted under the canonical namespace must be
/// returned by the query builders. This FAILS before the fix because
/// `search_packages` consumed `Property::PackageName` (== `dc:title`) and
/// `get_package_details` hardcoded `dc:title`, neither of which matches the
/// inserted `ggen:name` triple.
#[test]
fn test_inserted_package_is_findable_by_query_builder() {
    // Arrange: real store, canonical insert.
    let store = Store::new().expect("in-memory store");
    insert_canonical_package(
        &store,
        "acme-base",
        "Acme Base",
        "Core acme package",
        "1.0.0",
    );

    // Act: build the SELECT with the real query builder, execute on real store.
    let search = MarketplaceQueries::search_packages(&SearchParams::default())
        .expect("search query validates")
        .to_string();
    let search_rows = count_solutions(&store, &search);

    let details = MarketplaceQueries::get_package_details(&package_uri("acme-base"))
        .expect("details query validates")
        .to_string();
    let details_rows = count_solutions(&store, &details);

    // Assert: observable, non-empty results across both query paths.
    assert!(
        search_rows >= 1,
        "search_packages returned {search_rows} rows for a canonically-inserted \
         package; namespace mismatch causes silent data loss.\nQuery:\n{search}"
    );
    assert!(
        details_rows >= 1,
        "get_package_details returned {details_rows} rows for a canonically-inserted \
         package; namespace mismatch causes silent data loss.\nQuery:\n{details}"
    );
}

/// The same property URI string must be produced by both the insert side
/// (canonical `Properties`) and the query side (enum `Property`) for the
/// package-identity fields. This is the exact invariant whose violation caused
/// P0-03. Before the fix `Property::PackageName.uri()` was `dc:title`.
#[test]
fn test_package_name_uri_agrees_across_insert_and_query_sides() {
    // packageName: enum query side == canonical insert side.
    assert_eq!(
        Property::PackageName.uri(),
        Properties::name(),
        "Property::PackageName (query side) must equal Properties::name() (insert side)"
    );
    assert_eq!(
        Property::PackageName.uri(),
        format!("{MARKETPLACE_NS}name"),
        "package name must live under MARKETPLACE_NS"
    );
    assert_ne!(
        Property::PackageName.uri(),
        "http://purl.org/dc/terms/title",
        "package name must NOT route to dc:title (the P0-03 silent-data-loss bug)"
    );

    // packageId: there is no dedicated enum variant; assert the canonical
    // helper is itself under MARKETPLACE_NS and matches what control.rs and
    // rdf_mapper insert (`ggen:packageId`).
    assert_eq!(
        Properties::package_id(),
        format!("{MARKETPLACE_NS}packageId"),
        "packageId must live under MARKETPLACE_NS"
    );

    // description likewise agrees and is under MARKETPLACE_NS.
    assert_eq!(
        Property::PackageDescription.uri(),
        Properties::description()
    );
    assert_eq!(
        Property::PackageDescription.uri(),
        format!("{MARKETPLACE_NS}description")
    );
}

/// Negative control: the OLD query predicate (`dc:title`) must NOT match the
/// canonically-inserted data. This documents precisely why the bug was silent
/// (a real, syntactically-valid query that returns nothing) and guards against
/// a regression that re-introduces `dc:title` on the package-name field.
#[test]
fn test_legacy_dc_title_predicate_finds_nothing_against_canonical_data() {
    let store = Store::new().expect("in-memory store");
    insert_canonical_package(
        &store,
        "acme-base",
        "Acme Base",
        "Core acme package",
        "1.0.0",
    );

    let legacy_query = format!(
        "
        SELECT ?name WHERE {{
            <{pkg}> <http://purl.org/dc/terms/title> ?name .
        }}
        ",
        pkg = package_uri("acme-base"),
    );
    let legacy_rows = count_solutions(&store, &legacy_query);
    assert_eq!(
        legacy_rows, 0,
        "dc:title must NOT match canonical ggen:name data — this is the silent \
         data-loss path the fix removes from the query builders"
    );

    // And the corrected predicate (ggen:name) DOES find it.
    let canonical_query = format!(
        "
        SELECT ?name WHERE {{
            <{pkg}> <{p_name}> ?name .
        }}
        ",
        pkg = package_uri("acme-base"),
        p_name = Properties::name(),
    );
    assert_eq!(
        count_solutions(&store, &canonical_query),
        1,
        "canonical ggen:name predicate must find the inserted package name"
    );
}

/// Invariant guard for the enum: every redirected data-bearing property must
/// satisfy `uri() == MARKETPLACE_NS + local_name()` (no drift between the two
/// accessor methods) and must never resolve to a dc:/foaf: term.
#[test]
fn test_data_properties_have_no_uri_local_name_drift() {
    let data_props = [
        Property::PackageName,
        Property::PackageDescription,
        Property::PackageLicense,
        Property::PackageHomepage,
        Property::PackageRepository,
        Property::HasVersion,
        Property::HasDependency,
        Property::HasAuthor,
        Property::PublicKey,
    ];

    for prop in data_props {
        let uri = prop.uri();
        assert_eq!(
            uri,
            format!("{MARKETPLACE_NS}{}", prop.local_name()),
            "uri()/local_name() drift for {prop:?}"
        );
        assert!(
            uri.starts_with(MARKETPLACE_NS),
            "{prop:?} must resolve under MARKETPLACE_NS, got {uri}"
        );
        assert!(
            !uri.starts_with("http://purl.org/dc/") && !uri.starts_with("http://xmlns.com/foaf/"),
            "{prop:?} must not route to dc:/foaf:, got {uri}"
        );
    }
}
