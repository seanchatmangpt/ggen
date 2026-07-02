#![allow(clippy::unwrap_used, clippy::expect_used)]
use chrono::Utc;
use ggen_config::ReceiptChain;
use ggen_marketplace::marketplace::{
    compatibility::{CompatibilityDimension, Conflict, ConflictSeverity},
    composition_receipt::{CompositionReceipt, RuntimeProfile},
    models::{Package, PackageId, PackageMetadata, PackageVersion, ReleaseInfo},
    profile::{CustomProfileEntry, ProfileConfig, ReceiptSpec, RuntimeConstraint},
    rdf::poka_yoke::SparqlQuery,
    rdf::rdf_control::{ControlPlaneError, RdfControlPlane},
    rdf_mapper::RdfMapper,
    trust::{RegistryClass, RegistryType, TrustTier},
    validation::{ReadmeValidator, Validator},
};
use oxigraph::store::Store;
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::Arc;

// 1. Verify determinism of receipt JSON serialization
#[test]
fn test_challenger_receipt_serialization_determinism() {
    // We populate maps in different insertion orders and ensure that the JSON representation is identical.

    // Test for versions & ownership_map in CompositionReceipt
    let mut versions_1 = BTreeMap::new();
    versions_1.insert("alpha".to_string(), "1.0.0".to_string());
    versions_1.insert("beta".to_string(), "2.0.0".to_string());
    versions_1.insert("gamma".to_string(), "3.0.0".to_string());

    let mut versions_2 = BTreeMap::new();
    versions_2.insert("gamma".to_string(), "3.0.0".to_string());
    versions_2.insert("beta".to_string(), "2.0.0".to_string());
    versions_2.insert("alpha".to_string(), "1.0.0".to_string());

    let receipt_1 = CompositionReceipt {
        receipt_id: None,
        parent_receipt_id: None,
        atomic_packs: vec![],
        bundle_aliases: vec![],
        versions: versions_1,
        signatures: vec![],
        ontology_fragments: vec![],
        queries_executed: vec![],
        templates_rendered: vec![],
        validators_applied: vec![],
        policies_enforced: vec![],
        conflicts: vec![],
        ownership_map: BTreeMap::new(),
        artifact_hashes: vec![],
        runtime_context: RuntimeProfile {
            profile_id: "test".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        },
        receipt_chain: ReceiptChain::new(),
    };

    let receipt_2 = CompositionReceipt {
        receipt_id: None,
        parent_receipt_id: None,
        atomic_packs: vec![],
        bundle_aliases: vec![],
        versions: versions_2,
        signatures: vec![],
        ontology_fragments: vec![],
        queries_executed: vec![],
        templates_rendered: vec![],
        validators_applied: vec![],
        policies_enforced: vec![],
        conflicts: vec![],
        ownership_map: BTreeMap::new(),
        artifact_hashes: vec![],
        runtime_context: RuntimeProfile {
            profile_id: "test".to_string(),
            runtime_constraints: vec![],
            trust_requirement: TrustTier::Experimental,
        },
        receipt_chain: ReceiptChain::new(),
    };

    let json_1 = serde_json::to_string(&receipt_1).unwrap();
    let json_2 = serde_json::to_string(&receipt_2).unwrap();
    assert_eq!(
        json_1, json_2,
        "BTreeMap serialization must be deterministic regardless of insertion order"
    );

    // Test for Conflict context
    let mut context_1 = BTreeMap::new();
    context_1.insert("key1".to_string(), "val1".to_string());
    context_1.insert("key2".to_string(), "val2".to_string());

    let mut context_2 = BTreeMap::new();
    context_2.insert("key2".to_string(), "val2".to_string());
    context_2.insert("key1".to_string(), "val1".to_string());

    let conflict_1 = Conflict {
        dimension: CompatibilityDimension::OntologyNamespace,
        packs: vec![],
        description: "conflict".to_string(),
        severity: ConflictSeverity::Error,
        resolution: None,
        context: context_1,
    };

    let conflict_2 = Conflict {
        dimension: CompatibilityDimension::OntologyNamespace,
        packs: vec![],
        description: "conflict".to_string(),
        severity: ConflictSeverity::Error,
        resolution: None,
        context: context_2,
    };

    let c_json_1 = serde_json::to_string(&conflict_1).unwrap();
    let c_json_2 = serde_json::to_string(&conflict_2).unwrap();
    assert_eq!(c_json_1, c_json_2);

    // Test for RuntimeConstraint metadata
    let mut rc_metadata_1 = BTreeMap::new();
    rc_metadata_1.insert("a".to_string(), "1".to_string());
    rc_metadata_1.insert("b".to_string(), "2".to_string());

    let mut rc_metadata_2 = BTreeMap::new();
    rc_metadata_2.insert("b".to_string(), "2".to_string());
    rc_metadata_2.insert("a".to_string(), "1".to_string());

    let rc_1 = RuntimeConstraint {
        allowed_runtimes: vec![],
        forbid_defaults: false,
        require_explicit: false,
        metadata: rc_metadata_1,
    };

    let rc_2 = RuntimeConstraint {
        allowed_runtimes: vec![],
        forbid_defaults: false,
        require_explicit: false,
        metadata: rc_metadata_2,
    };

    assert_eq!(
        serde_json::to_string(&rc_1).unwrap(),
        serde_json::to_string(&rc_2).unwrap()
    );

    // ProfileConfig profiles
    let mut profiles_1 = BTreeMap::new();
    profiles_1.insert(
        "first".to_string(),
        CustomProfileEntry {
            name: "First".to_string(),
            description: "First Profile".to_string(),
            trust_tier: TrustTier::EnterpriseApproved,
            receipt_spec: ReceiptSpec::default(),
            policies: vec![],
            runtime_constraints: vec![],
            metadata: BTreeMap::new(),
        },
    );
    profiles_1.insert(
        "second".to_string(),
        CustomProfileEntry {
            name: "Second".to_string(),
            description: "Second Profile".to_string(),
            trust_tier: TrustTier::Experimental,
            receipt_spec: ReceiptSpec::default(),
            policies: vec![],
            runtime_constraints: vec![],
            metadata: BTreeMap::new(),
        },
    );

    let mut profiles_2 = BTreeMap::new();
    profiles_2.insert(
        "second".to_string(),
        CustomProfileEntry {
            name: "Second".to_string(),
            description: "Second Profile".to_string(),
            trust_tier: TrustTier::Experimental,
            receipt_spec: ReceiptSpec::default(),
            policies: vec![],
            runtime_constraints: vec![],
            metadata: BTreeMap::new(),
        },
    );
    profiles_2.insert(
        "first".to_string(),
        CustomProfileEntry {
            name: "First".to_string(),
            description: "First Profile".to_string(),
            trust_tier: TrustTier::EnterpriseApproved,
            receipt_spec: ReceiptSpec::default(),
            policies: vec![],
            runtime_constraints: vec![],
            metadata: BTreeMap::new(),
        },
    );

    let pc_1 = ProfileConfig {
        profiles: profiles_1,
    };
    let pc_2 = ProfileConfig {
        profiles: profiles_2,
    };

    assert_eq!(
        serde_json::to_string(&pc_1).unwrap(),
        serde_json::to_string(&pc_2).unwrap()
    );
}

// 2. Trust tier comparison priorities and Blocked package rejection
#[test]
fn test_challenger_trust_tier_comparisons() {
    // Priorities verify:
    // EnterpriseCertified => 1
    // EnterpriseApproved => 2
    // CommunityReviewed => 3
    // ProductionReady => 4
    // Experimental => 5
    // Quarantined => 6
    // Blocked => 7

    assert!(TrustTier::EnterpriseCertified.priority() < TrustTier::EnterpriseApproved.priority());
    assert!(TrustTier::EnterpriseApproved.priority() < TrustTier::CommunityReviewed.priority());
    assert!(TrustTier::CommunityReviewed.priority() < TrustTier::ProductionReady.priority());
    assert!(TrustTier::ProductionReady.priority() < TrustTier::Experimental.priority());
    assert!(TrustTier::Experimental.priority() < TrustTier::Quarantined.priority());
    assert!(TrustTier::Quarantined.priority() < TrustTier::Blocked.priority());

    // Meets requirement checking: lower numeric priority is more trustworthy.
    // Experimental (5) meets Quarantined requirement (6)
    assert!(TrustTier::Experimental.meets_requirement(TrustTier::Quarantined));
    // Quarantined (6) does NOT meet Experimental requirement (5)
    assert!(!TrustTier::Quarantined.meets_requirement(TrustTier::Experimental));

    // Blocked (7) package rejection MUST apply for ALL requirements (even Blocked itself).
    assert!(!TrustTier::Blocked.meets_requirement(TrustTier::Blocked));
    assert!(!TrustTier::Blocked.meets_requirement(TrustTier::Quarantined));
    assert!(!TrustTier::Blocked.meets_requirement(TrustTier::EnterpriseCertified));

    // General compliance checks
    assert!(TrustTier::EnterpriseCertified.is_regulated_compliant());
    assert!(TrustTier::EnterpriseApproved.is_regulated_compliant());
    assert!(!TrustTier::ProductionReady.is_regulated_compliant());
    assert!(!TrustTier::Experimental.is_regulated_compliant());
    assert!(!TrustTier::Quarantined.is_regulated_compliant());
    assert!(!TrustTier::Blocked.is_regulated_compliant());

    assert!(TrustTier::EnterpriseCertified.is_production_ready());
    assert!(TrustTier::EnterpriseApproved.is_production_ready());
    assert!(TrustTier::ProductionReady.is_production_ready());
    assert!(!TrustTier::Experimental.is_production_ready());
    assert!(!TrustTier::Quarantined.is_production_ready());
    assert!(!TrustTier::Blocked.is_production_ready());
}

// 3. RDF registry class dynamic mapping and SPARQL queries
#[tokio::test]
async fn test_challenger_rdf_registry_class_roundtrip() {
    let classes = vec![
        RegistryClass::Public {
            url: "https://crates.io".to_string(),
            registry_type: RegistryType::CratesIo,
        },
        RegistryClass::PrivateEnterprise {
            url: "https://registry.enterprise.com".to_string(),
            require_signature: true,
            allow_unlisted: false,
        },
        RegistryClass::PrivateEnterprise {
            url: "https://registry.enterprise.com".to_string(),
            require_signature: false,
            allow_unlisted: true,
        },
        RegistryClass::MirroredAirGapped {
            primary_url: "https://primary.registry.com".to_string(),
            mirror_path: PathBuf::from("/var/lib/ggen/mirror"),
            sync_interval_seconds: 3600,
        },
    ];

    for (idx, reg_class) in classes.into_iter().enumerate() {
        let store = Arc::new(Store::new().unwrap());
        let mapper = RdfMapper::new(store);

        let name = format!("pkg-class-challenger-{}", idx);
        let id = PackageId::new(&name).unwrap();
        let metadata = PackageMetadata::new(id.clone(), "Challenger Package", "A challenge", "MIT");

        let version = PackageVersion::new("2.1.0").unwrap();
        let mut releases = indexmap::IndexMap::new();
        releases.insert(
            version.clone(),
            ReleaseInfo {
                version: version.clone(),
                released_at: Utc::now(),
                changelog: "Changelog".to_string(),
                checksum: "9999999999999999999999999999999999999999999999999999999999999999"
                    .to_string(),
                signature: Some("sig999".to_string()),
                download_url: "https://download.com/pkg".to_string(),
                dependencies: vec![],
                trust_tier: TrustTier::EnterpriseCertified,
                registry_class: reg_class.clone(),
            },
        );

        let package = Package {
            metadata,
            latest_version: version.clone(),
            versions: vec![version.clone()],
            releases,
        };

        mapper.package_to_rdf(&package).unwrap();
        let reconstructed = mapper.rdf_to_package(&id).unwrap();

        let reconstructed_release = reconstructed.releases.get(&version).unwrap();
        assert_eq!(
            reconstructed_release.registry_class, reg_class,
            "Registry class roundtrip must match exactly"
        );
    }
}

// 4. Case-insensitivity of the SPARQL query injection check
fn setup_temp_control_plane_config() -> String {
    let test_dir =
        std::env::temp_dir().join(format!("ggen-challenger-config-{}", uuid::Uuid::new_v4()));
    std::fs::create_dir_all(&test_dir).unwrap();

    let config_content = format!(
        r#"@prefix ggen: <http://ggen.io/schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<http://ggen.io/schema#marketplace> a ggen:MarketplaceConfig ;
    ggen:registryUrl "https://test.registry.dev" ;
    ggen:cacheDir "/tmp/test-cache" ;
    ggen:maxDownloadSize "1000000"^^xsd:integer ;
    ggen:validationEnabled "true"^^xsd:boolean ;
    ggen:autoUpdateEnabled "false"^^xsd:boolean ."#
    );

    let config_path = test_dir.join("marketplace.ttl");
    std::fs::write(&config_path, config_content).unwrap();

    let validation_path = test_dir.join("validation-rules.ttl");
    std::fs::write(&validation_path, "# validation rules").unwrap();

    test_dir.to_str().unwrap().to_string()
}

#[test]
fn test_challenger_sparql_injection_case_insensitivity() {
    let config_dir = setup_temp_control_plane_config();
    let control_plane = RdfControlPlane::new(&config_dir).unwrap();

    let injection_patterns = vec![
        // uppercase
        "SELECT * WHERE { ?s ?p ?o } ; DROP GRAPH <test>",
        "SELECT * WHERE { ?s ?p ?o } ; DELETE WHERE { ?s ?p ?o }",
        "SELECT * WHERE { ?s ?p ?o } ; INSERT DATA { <test> <test> <test> }",
        "SELECT * WHERE { ?s ?p ?o } ; CLEAR GRAPH <test>",
        "SELECT * WHERE { ?s ?p ?o } ; DELETE { ?s ?p ?o }",
        // lowercase
        "select * where { ?s ?p ?o } ; drop graph <test>",
        "select * where { ?s ?p ?o } ; delete where { ?s ?p ?o }",
        "select * where { ?s ?p ?o } ; insert data { <test> <test> <test> }",
        "select * where { ?s ?p ?o } ; clear graph <test>",
        "select * where { ?s ?p ?o } ; delete { ?s ?p ?o }",
        // mixed case
        "select * where { ?s ?p ?o } ; DrOp GrApH <test>",
        "select * where { ?s ?p ?o } ; DeLeTe WhErE { ?s ?p ?o }",
        "select * where { ?s ?p ?o } ; InSeRt DaTa { <test> <test> <test> }",
        "select * where { ?s ?p ?o } ; ClEaR GrApH <test>",
    ];

    for query_str in injection_patterns {
        let query = SparqlQuery::new()
            .select(&["?s"])
            .where_pattern(query_str)
            .validate()
            .unwrap();

        let result = control_plane.execute_query(&query);
        assert!(
            matches!(result, Err(ControlPlaneError::SecurityViolation { .. })),
            "Expected SecurityViolation error for query: {}",
            query_str
        );
    }

    // A clean query should not trigger it
    let clean_query = SparqlQuery::new()
        .select(&["?s"])
        .where_pattern("?s ?p ?o")
        .validate()
        .unwrap();
    let result = control_plane.execute_query(&clean_query);
    assert!(!matches!(
        result,
        Err(ControlPlaneError::SecurityViolation { .. })
    ));

    let _ = std::fs::remove_dir_all(&config_dir);
}

// 5. README file physical presence validation in ReadmeValidator
#[tokio::test]
async fn test_challenger_readme_validator_physical_presence() {
    let validator = ReadmeValidator;

    // Check with a package ID
    let package_id = "challenger-test-pkg";
    let id = PackageId::new(package_id).unwrap();
    let metadata = PackageMetadata::new(id, "Challenger Test Package", "Description", "MIT");
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    // Ensure candidate directory is clean/does not exist initially
    let pkg_dir = std::path::PathBuf::from(format!("marketplace/packages/{package_id}"));
    let _ = std::fs::remove_dir_all(&pkg_dir);

    // 1. Without README, validation must fail
    let check = validator.validate(&package).await.unwrap();
    assert!(
        !check.passed,
        "Validation must fail when README is physically missing"
    );
    assert_eq!(check.message, "Missing README file in package directory");

    // 2. With README (case-insensitive checks)
    let readme_variants = vec![
        "README.md",
        "readme.txt",
        "README",
        "readme",
        "Readme.markdown",
    ];

    for variant in readme_variants {
        std::fs::create_dir_all(&pkg_dir).unwrap();
        let readme_file = pkg_dir.join(variant);
        std::fs::write(&readme_file, "some readme content").unwrap();

        let check = validator.validate(&package).await.unwrap();

        // Clean up
        let _ = std::fs::remove_dir_all(&pkg_dir);

        assert!(
            check.passed,
            "Validation must pass with README variant: {}",
            variant
        );
        assert_eq!(check.message, "README file present");
    }
}
