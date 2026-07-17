//! Standard ontology registry and namespace enumeration.
//!
//! Ported from `ggen-core/src/validation/standard_ontologies.rs` (`StandardOntology`) and
//! `ggen-core/src/domain/ontology/mod.rs` (`get_standard_namespaces`/`NamespaceInfo`) during
//! the ggen-core retirement migration (`specs/014-ggen-core-replacement`, Phase 3
//! "non-colliding noun re-points"): `ggen-cli`'s `cmds/ontology.rs` calls
//! `get_standard_namespaces()` directly (`namespaces()` verb), and that function's own body
//! calls `StandardOntology::all()`/`.namespace()` internally — so both need to move together
//! even though `cmds/ontology.rs`'s own (dead) `use ggen_core::validation::StandardOntology`
//! import never named it directly.
//!
//! ## Deviation from the ggen-core original: `StandardOntologyValidator` and
//! ## `OntologyScreeningConfig` are dropped
//!
//! The ggen-core original bundles three things in one file: the `StandardOntology` enum
//! (zero ggen-core-internal dependencies — pure `&'static str` literals + `std::HashSet`),
//! `StandardOntologyValidator` (namespace/config validation, depends on
//! `crate::validation::error::{Result, ValidationError}` — a large SHACL/oxigraph-flavored
//! error enum with 8 variants, only one of which, `OxigraphError`, is ever constructed here),
//! and `OntologyScreeningConfig` (BIG BANG 80/20 screening flags). Neither
//! `StandardOntologyValidator` nor `OntologyScreeningConfig` is reachable from
//! `cmds/ontology.rs` — confirmed via grep, `ontology.rs` only ever calls
//! `get_standard_namespaces()`, whose body only touches `StandardOntology::all()` and
//! `.namespace()`. Per the task's documented fallback for large/tangled ports, this port
//! extracts just the reachable slice (the enum itself, which has no unrelated-error-type
//! coupling) and drops the validator/screening-config types rather than dragging in
//! ggen-core's whole SHACL validation error surface for code nothing here calls. If a future
//! caller needs namespace/config validation, it should be re-added against a
//! marketplace-native error type, not by resurrecting `ValidationError`.

/// Standard ontologies approved for use in ggen projects.
///
/// Byte-for-byte identical to the ggen-core original's enum and inherent methods (see module
/// docs for what was dropped).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StandardOntology {
    /// schema.org (v3.13.0+) - Web structured data vocabulary
    SchemaOrg,
    /// FOAF (Friend of a Friend) - Social networks and profiles
    Foaf,
    /// Dublin Core (v1.1) - Metadata and document properties
    DublinCore,
    /// SKOS (Simple Knowledge Organization System) - Thesauri
    Skos,
    /// Big Five Personality Traits (OCEAN) - Behavioral modeling
    BigFive,
}

impl StandardOntology {
    /// Get the canonical namespace/prefix for this ontology
    pub fn namespace(&self) -> &'static str {
        match self {
            StandardOntology::SchemaOrg => "https://schema.org/",
            StandardOntology::Foaf => "http://xmlns.com/foaf/0.1/",
            StandardOntology::DublinCore => "http://purl.org/dc/elements/1.1/",
            StandardOntology::Skos => "http://www.w3.org/2004/02/skos/core#",
            StandardOntology::BigFive => "https://ggen.io/ontologies/big-five#",
        }
    }

    /// Get human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            StandardOntology::SchemaOrg => "schema.org",
            StandardOntology::Foaf => "FOAF",
            StandardOntology::DublinCore => "Dublin Core",
            StandardOntology::Skos => "SKOS",
            StandardOntology::BigFive => "Big Five Traits",
        }
    }

    /// Get recommended version constraint
    pub fn version_constraint(&self) -> &'static str {
        match self {
            StandardOntology::SchemaOrg => "^3.13.0",
            StandardOntology::Foaf => "~0.1.0",
            StandardOntology::DublinCore => "^1.1.0",
            StandardOntology::Skos => "~0.3.0",
            StandardOntology::BigFive => "^0.1.0",
        }
    }

    /// Get link to canonical specification
    pub fn spec_url(&self) -> &'static str {
        match self {
            StandardOntology::SchemaOrg => "https://schema.org/",
            StandardOntology::Foaf => "http://xmlns.com/foaf/spec/",
            StandardOntology::DublinCore => "http://dublincore.org/documents/dces/",
            StandardOntology::Skos => "https://www.w3.org/2009/08/skos-reference/skos.html",
            StandardOntology::BigFive => "https://ggen.io/docs/big-five",
        }
    }

    /// All approved standard ontologies
    pub fn all() -> &'static [StandardOntology] {
        &[
            StandardOntology::SchemaOrg,
            StandardOntology::Foaf,
            StandardOntology::DublinCore,
            StandardOntology::Skos,
            StandardOntology::BigFive,
        ]
    }

    /// All approved namespace URIs
    pub fn approved_namespaces() -> std::collections::HashSet<&'static str> {
        Self::all().iter().map(|o| o.namespace()).collect()
    }

    /// Resolve a namespace to a standard ontology
    pub fn from_namespace(namespace: &str) -> Option<Self> {
        match namespace {
            "https://schema.org/" | "http://schema.org/" => Some(StandardOntology::SchemaOrg),
            "http://xmlns.com/foaf/0.1/" | "http://xmlns.com/foaf/" => Some(StandardOntology::Foaf),
            "http://purl.org/dc/elements/1.1/" | "http://purl.org/dc/terms/" => {
                Some(StandardOntology::DublinCore)
            }
            "http://www.w3.org/2004/02/skos/core#" => Some(StandardOntology::Skos),
            "https://ggen.io/ontologies/big-five#" | "https://ggen.io/big-five#" => {
                Some(StandardOntology::BigFive)
            }
            _ => None,
        }
    }
}

/// Namespace info returned by [`get_standard_namespaces`].
///
/// Ported verbatim from `ggen-core/src/domain/ontology/mod.rs`.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq, Eq)]
pub struct NamespaceInfo {
    pub prefix: String,
    pub uri: String,
    pub source: String,
}

/// Enumerate all standard namespaces available to `ggen`.
///
/// Combines the embedded core stubs (rdf/rdfs/owl) with the [`StandardOntology`] registry
/// (schema.org, FOAF, Dublin Core, SKOS, Big Five), deduplicated by URI.
///
/// Ported verbatim (logic and dedup ordering) from
/// `ggen-core/src/domain/ontology/mod.rs::get_standard_namespaces`.
pub fn get_standard_namespaces() -> Vec<NamespaceInfo> {
    let mut namespaces = Vec::new();

    // Add Core Stubs
    let core_ontologies = crate::ontology_core::CoreOntologyBundle::all();
    for ont in core_ontologies {
        namespaces.push(NamespaceInfo {
            prefix: ont.name.to_string(),
            uri: ont.namespace.to_string(),
            source: "core-stub".to_string(),
        });
    }

    // Add registered public ontologies from CoreOntologyBundle as bundled standards
    for ont in core_ontologies {
        namespaces.push(NamespaceInfo {
            prefix: ont.name.to_string(),
            uri: ont.namespace.to_string(),
            source: "bundled-standard".to_string(),
        });
    }

    // Add registered public ontologies from StandardOntology as bundled standards
    for std_ont in StandardOntology::all() {
        let prefix = match std_ont {
            StandardOntology::SchemaOrg => "schema",
            StandardOntology::Foaf => "foaf",
            StandardOntology::DublinCore => "dc",
            StandardOntology::Skos => "skos",
            StandardOntology::BigFive => "bigfive",
        };
        namespaces.push(NamespaceInfo {
            prefix: prefix.to_string(),
            uri: std_ont.namespace().to_string(),
            source: "bundled-standard".to_string(),
        });
    }

    // Deduplicate (prioritizing "bundled-standard" over "core-stub")
    // 1. Sort by URI, and then by source alphabetically ("bundled-standard" < "core-stub")
    namespaces.sort_by(|a, b| match a.uri.cmp(&b.uri) {
        std::cmp::Ordering::Equal => a.source.cmp(&b.source),
        ord => ord,
    });
    // 2. dedup_by keeps the first entry of consecutive duplicates
    namespaces.dedup_by(|a, b| a.uri == b.uri);

    namespaces
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_standard_ontology_namespaces() {
        assert_eq!(
            StandardOntology::SchemaOrg.namespace(),
            "https://schema.org/"
        );
        assert_eq!(
            StandardOntology::Foaf.namespace(),
            "http://xmlns.com/foaf/0.1/"
        );
        assert_eq!(
            StandardOntology::DublinCore.namespace(),
            "http://purl.org/dc/elements/1.1/"
        );
        assert_eq!(
            StandardOntology::Skos.namespace(),
            "http://www.w3.org/2004/02/skos/core#"
        );
    }

    #[test]
    fn test_resolve_namespace_schema_org() {
        let result = StandardOntology::from_namespace("https://schema.org/");
        assert_eq!(result, Some(StandardOntology::SchemaOrg));
    }

    #[test]
    fn test_resolve_namespace_foaf() {
        let result = StandardOntology::from_namespace("http://xmlns.com/foaf/0.1/");
        assert_eq!(result, Some(StandardOntology::Foaf));
    }

    #[test]
    fn test_reject_custom_namespace() {
        let result = StandardOntology::from_namespace("https://custom.example.com/ontology#");
        assert_eq!(result, None);
    }

    #[test]
    fn test_standard_ontology_all() {
        let all = StandardOntology::all();
        assert_eq!(all.len(), 5);
    }

    #[test]
    fn test_standard_ontology_names() {
        assert_eq!(StandardOntology::SchemaOrg.name(), "schema.org");
        assert_eq!(StandardOntology::Foaf.name(), "FOAF");
        assert_eq!(StandardOntology::DublinCore.name(), "Dublin Core");
        assert_eq!(StandardOntology::Skos.name(), "SKOS");
        assert_eq!(StandardOntology::BigFive.name(), "Big Five Traits");
    }

    #[test]
    fn test_approved_namespaces_set() {
        let namespaces = StandardOntology::approved_namespaces();
        assert_eq!(namespaces.len(), 5); // 5 standard ontologies
        assert!(namespaces.contains("https://schema.org/"));
        assert!(namespaces.contains("http://xmlns.com/foaf/0.1/"));
    }

    /// Chicago-TDD proof (real data, no mocks) that the ported ontology_core trio produces the
    /// expected namespace set end-to-end: core stubs (rdf/rdfs/owl) + standard registry
    /// (schema.org/FOAF/Dublin Core/SKOS/Big Five), deduplicated to 8 total entries with every
    /// namespace's `source` correctly attributed to "bundled-standard" (both CoreOntologyBundle
    /// and StandardOntology namespaces get promoted over the raw "core-stub" duplicate).
    #[test]
    fn test_get_standard_namespaces_end_to_end() {
        let namespaces = get_standard_namespaces();

        // 3 core stubs (rdf, rdfs, owl) + 5 standard-registry ontologies (schema.org, FOAF,
        // Dublin Core, SKOS, Big Five) = 8 unique namespace URIs after dedup.
        assert_eq!(
            namespaces.len(),
            8,
            "expected 3 core stubs + 5 standard ontologies, got: {:?}",
            namespaces
        );

        let uris: std::collections::HashSet<&str> =
            namespaces.iter().map(|n| n.uri.as_str()).collect();

        for expected_uri in [
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            "http://www.w3.org/2000/01/rdf-schema#",
            "http://www.w3.org/2002/07/owl#",
            "https://schema.org/",
            "http://xmlns.com/foaf/0.1/",
            "http://purl.org/dc/elements/1.1/",
            "http://www.w3.org/2004/02/skos/core#",
            "https://ggen.io/ontologies/big-five#",
        ] {
            assert!(
                uris.contains(expected_uri),
                "missing expected namespace {} in {:?}",
                expected_uri,
                namespaces
            );
        }

        // rdf/rdfs/owl exist in both CoreOntologyBundle and were pushed as both "core-stub" and
        // "bundled-standard" candidates before dedup — the sort-then-dedup must keep the
        // "bundled-standard" copy (bundled-standard < core-stub alphabetically).
        for core_uri in [
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
            "http://www.w3.org/2000/01/rdf-schema#",
            "http://www.w3.org/2002/07/owl#",
        ] {
            let entry = namespaces
                .iter()
                .find(|n| n.uri == core_uri)
                .unwrap_or_else(|| panic!("missing {}", core_uri));
            assert_eq!(
                entry.source, "bundled-standard",
                "core stub {} should be promoted to bundled-standard after dedup",
                core_uri
            );
        }

        // No duplicate URIs survive dedup.
        assert_eq!(
            uris.len(),
            namespaces.len(),
            "dedup should leave exactly one entry per URI"
        );
    }
}
