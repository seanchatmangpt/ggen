#![allow(clippy::unwrap_used)]
use chrono::Utc;
use ggen_marketplace::marketplace::composition_receipt::{CompositionReceipt, RuntimeProfile};
use ggen_marketplace::marketplace::models::{
    Package, PackageId, PackageMetadata, PackageVersion, ReleaseInfo,
};
use ggen_marketplace::marketplace::rdf_mapper::RdfMapper;
use ggen_marketplace::marketplace::trust::{RegistryClass, RegistryType, TrustTier};
use ggen_marketplace::marketplace::validation::{ReadmeValidator, Validator};
use oxigraph::store::Store;
use std::sync::Arc;

#[test]
fn test_challenger_receipt_determinism() {
    // Construct receipt 1
    let profile = RuntimeProfile {
        profile_id: "test-profile".to_string(),
        runtime_constraints: vec!["c1".to_string(), "c2".to_string()],
        trust_requirement: TrustTier::Experimental,
    };
    let mut receipt1 = CompositionReceipt::new(profile.clone());

    // Insert versions in order A then B
    receipt1
        .versions
        .insert("pkg_a".to_string(), "1.0.0".to_string());
    receipt1
        .versions
        .insert("pkg_b".to_string(), "2.0.0".to_string());

    // Insert ownership targets in order TargetA then TargetB
    let record_a = ggen_marketplace::marketplace::composition_receipt::OwnershipRecord {
        target: "target_a".to_string(),
        class: "exclusive".to_string(),
        owner_pack: "pkg_a".to_string(),
        merge_strategy: None,
    };
    let record_b = ggen_marketplace::marketplace::composition_receipt::OwnershipRecord {
        target: "target_b".to_string(),
        class: "exclusive".to_string(),
        owner_pack: "pkg_b".to_string(),
        merge_strategy: None,
    };
    receipt1
        .ownership_map
        .insert("target_a".to_string(), record_a.clone());
    receipt1
        .ownership_map
        .insert("target_b".to_string(), record_b.clone());

    // Construct receipt 2 with identical contents but inserted in reverse order
    let mut receipt2 = CompositionReceipt::new(profile);
    receipt2
        .versions
        .insert("pkg_b".to_string(), "2.0.0".to_string());
    receipt2
        .versions
        .insert("pkg_a".to_string(), "1.0.0".to_string());
    receipt2
        .ownership_map
        .insert("target_b".to_string(), record_b);
    receipt2
        .ownership_map
        .insert("target_a".to_string(), record_a);

    // Compute receipt IDs
    receipt1.compute_receipt_id().unwrap();
    receipt2.compute_receipt_id().unwrap();

    // Assert receipt IDs are identical (determinism of serialization)
    assert_eq!(receipt1.receipt_id, receipt2.receipt_id);

    // Assert JSON representation is identical
    let json1 = receipt1.to_json().unwrap();
    let json2 = receipt2.to_json().unwrap();
    assert_eq!(json1, json2);
}

#[test]
fn test_challenger_trust_tier_priorities() {
    // Priority order: lower priority numeric value means more trusted
    assert!(TrustTier::EnterpriseCertified.priority() < TrustTier::EnterpriseApproved.priority());
    assert!(TrustTier::EnterpriseApproved.priority() < TrustTier::CommunityReviewed.priority());
    assert!(TrustTier::CommunityReviewed.priority() < TrustTier::ProductionReady.priority());
    assert!(TrustTier::ProductionReady.priority() < TrustTier::Experimental.priority());
    assert!(TrustTier::Experimental.priority() < TrustTier::Quarantined.priority());
    assert!(TrustTier::Quarantined.priority() < TrustTier::Blocked.priority());

    // Meets requirement checks
    // Blocked meets nothing
    assert!(!TrustTier::Blocked.meets_requirement(TrustTier::Blocked));
    assert!(!TrustTier::Blocked.meets_requirement(TrustTier::Quarantined));
    assert!(!TrustTier::Blocked.meets_requirement(TrustTier::EnterpriseCertified));

    // Quarantined vs Experimental
    assert!(!TrustTier::Quarantined.meets_requirement(TrustTier::Experimental));
    assert!(TrustTier::Experimental.meets_requirement(TrustTier::Quarantined));

    // EnterpriseApproved meets itself and lower tiers (like Experimental, Quarantined)
    assert!(TrustTier::EnterpriseApproved.meets_requirement(TrustTier::EnterpriseApproved));
    assert!(TrustTier::EnterpriseApproved.meets_requirement(TrustTier::Experimental));
    assert!(TrustTier::EnterpriseApproved.meets_requirement(TrustTier::Quarantined));
    // but not higher tiers
    assert!(!TrustTier::EnterpriseApproved.meets_requirement(TrustTier::EnterpriseCertified));
}

#[tokio::test]
async fn test_challenger_rdf_dynamic_mapping() {
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
        RegistryClass::MirroredAirGapped {
            primary_url: "https://primary.registry.com".to_string(),
            mirror_path: std::path::PathBuf::from("/var/lib/ggen/mirror"),
            sync_interval_seconds: 3600,
        },
    ];

    for (idx, reg_class) in classes.into_iter().enumerate() {
        let store = Arc::new(Store::new().unwrap());
        let mapper = RdfMapper::new(store);

        let name = format!("pkg-challenger-{}", idx);
        let id = PackageId::new(&name).unwrap();
        let metadata = PackageMetadata::new(id.clone(), "Challenger Package", "A test", "MIT");

        let version = PackageVersion::new("2.0.0").unwrap();
        let mut releases = indexmap::IndexMap::new();
        releases.insert(
            version.clone(),
            ReleaseInfo {
                version: version.clone(),
                released_at: Utc::now(),
                changelog: "Changelog text".to_string(),
                checksum: "digest123456789".to_string(),
                signature: Some("signature123".to_string()),
                download_url: "https://download.com".to_string(),
                dependencies: vec![],
                trust_tier: TrustTier::CommunityReviewed,
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
        assert_eq!(reconstructed_release.registry_class, reg_class);
    }
}

#[tokio::test]
async fn test_challenger_readme_validator() {
    let validator = ReadmeValidator;

    // Test Case 1: Package directory doesn't exist or is empty (should fail)
    let id_missing = PackageId::new("pkg-no-readme").unwrap();
    let metadata_missing =
        PackageMetadata::new(id_missing, "No README Pkg", "A test package", "MIT");
    let package_missing = Package {
        metadata: metadata_missing,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    let result_missing = validator.validate(&package_missing).await.unwrap();
    assert!(!result_missing.passed);
    assert_eq!(
        result_missing.message,
        "Missing README file in package directory"
    );

    // Test Case 2: Package directory exists and has README (should pass)
    let id_present = PackageId::new("pkg-with-readme").unwrap();

    // Create the package directory and README locally
    let pkg_dir = std::path::PathBuf::from("marketplace/packages/pkg-with-readme");
    std::fs::create_dir_all(&pkg_dir).unwrap();
    std::fs::write(pkg_dir.join("README.md"), "# README\n").unwrap();

    let metadata_present =
        PackageMetadata::new(id_present, "With README Pkg", "A test package", "MIT");
    let package_present = Package {
        metadata: metadata_present,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    let result_present = validator.validate(&package_present).await.unwrap();
    // Clean up
    let _ = std::fs::remove_dir_all(&pkg_dir);

    assert!(result_present.passed);
    assert_eq!(result_present.message, "README file present");

    // Test Case 3: Case insensitivity of filename (e.g. lowercase readme without extension)
    let id_case = PackageId::new("pkg-readme-case");
    let id_case = id_case.unwrap();
    let pkg_dir_case = std::path::PathBuf::from("marketplace/packages/pkg-readme-case");
    std::fs::create_dir_all(&pkg_dir_case).unwrap();
    std::fs::write(pkg_dir_case.join("readme"), "some text").unwrap();

    let metadata_case = PackageMetadata::new(id_case, "Case README Pkg", "A test package", "MIT");
    let package_case = Package {
        metadata: metadata_case,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    let result_case = validator.validate(&package_case).await.unwrap();
    // Clean up
    let _ = std::fs::remove_dir_all(&pkg_dir_case);

    assert!(result_case.passed);
    assert_eq!(result_case.message, "README file present");
}
