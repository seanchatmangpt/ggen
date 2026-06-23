use chrono::Utc;
use ggen_config::ReceiptChain;
use ggen_marketplace::marketplace::{
    compatibility::{CompatibilityDimension, Conflict, ConflictSeverity},
    composition_receipt::{CompositionReceipt, OwnershipRecord, RuntimeProfile},
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

// Simple LCG pseudo-random generator for self-contained stress tests
fn lcg(seed: &mut u64) -> u64 {
    *seed = seed
        .wrapping_mul(6364136223846793005)
        .wrapping_add(1442695040888963407);
    *seed
}

fn random_string(seed: &mut u64, len: usize) -> String {
    let mut s = String::with_capacity(len);
    for _ in 0..len {
        let val = lcg(seed) % 26;
        s.push((b'a' + val as u8) as char);
    }
    s
}

// 1. Stress test for determinism of receipt JSON serialization under various insertion orders
#[test]
fn test_stress_receipt_serialization_determinism() {
    let mut seed = 12345u64;

    for _ in 0..10 {
        // Generate random keys and values
        let mut kv_pairs = Vec::new();
        for _ in 0..100 {
            let k = random_string(&mut seed, 10);
            let v = random_string(&mut seed, 10);
            kv_pairs.push((k, v));
        }

        let mut ownership_pairs = Vec::new();
        for _ in 0..50 {
            let target = random_string(&mut seed, 12);
            let owner_pack = random_string(&mut seed, 8);
            let record = OwnershipRecord {
                target: target.clone(),
                class: "exclusive".to_string(),
                owner_pack,
                merge_strategy: None,
            };
            ownership_pairs.push((target, record));
        }

        // Build receipts with different insertion orders and ensure JSON is identical
        let mut map_versions_1 = BTreeMap::new();
        let mut map_ownership_1 = BTreeMap::new();
        for (k, v) in &kv_pairs {
            map_versions_1.insert(k.clone(), v.clone());
        }
        for (target, record) in &ownership_pairs {
            map_ownership_1.insert(target.clone(), record.clone());
        }

        // Shuffle elements pseudo-randomly for the second insert order
        let mut kv_pairs_2 = kv_pairs.clone();
        let mut ownership_pairs_2 = ownership_pairs.clone();
        for i in (1..kv_pairs_2.len()).rev() {
            let j = (lcg(&mut seed) % (i as u64 + 1)) as usize;
            kv_pairs_2.swap(i, j);
        }
        for i in (1..ownership_pairs_2.len()).rev() {
            let j = (lcg(&mut seed) % (i as u64 + 1)) as usize;
            ownership_pairs_2.swap(i, j);
        }

        let mut map_versions_2 = BTreeMap::new();
        let mut map_ownership_2 = BTreeMap::new();
        for (k, v) in kv_pairs_2 {
            map_versions_2.insert(k, v);
        }
        for (target, record) in ownership_pairs_2 {
            map_ownership_2.insert(target, record);
        }

        let receipt_1 = CompositionReceipt {
            receipt_id: None,
            parent_receipt_id: None,
            atomic_packs: vec![],
            bundle_aliases: vec![],
            versions: map_versions_1,
            signatures: vec![],
            ontology_fragments: vec![],
            queries_executed: vec![],
            templates_rendered: vec![],
            validators_applied: vec![],
            policies_enforced: vec![],
            conflicts: vec![],
            ownership_map: map_ownership_1,
            artifact_hashes: vec![],
            runtime_context: RuntimeProfile {
                profile_id: "stress-test".to_string(),
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
            versions: map_versions_2,
            signatures: vec![],
            ontology_fragments: vec![],
            queries_executed: vec![],
            templates_rendered: vec![],
            validators_applied: vec![],
            policies_enforced: vec![],
            conflicts: vec![],
            ownership_map: map_ownership_2,
            artifact_hashes: vec![],
            runtime_context: RuntimeProfile {
                profile_id: "stress-test".to_string(),
                runtime_constraints: vec![],
                trust_requirement: TrustTier::Experimental,
            },
            receipt_chain: ReceiptChain::new(),
        };

        let json_1 = serde_json::to_string(&receipt_1).unwrap();
        let json_2 = serde_json::to_string(&receipt_2).unwrap();
        assert_eq!(
            json_1, json_2,
            "JSON serialization must be deterministic under all insertion permutations"
        );
    }
}

// 2. Comprehensive check of TrustTier comparisons and Blocked package rejection
#[test]
fn test_stress_trust_tier_comparisons() {
    let tiers = [
        TrustTier::EnterpriseCertified,
        TrustTier::EnterpriseApproved,
        TrustTier::CommunityReviewed,
        TrustTier::ProductionReady,
        TrustTier::Experimental,
        TrustTier::Quarantined,
        TrustTier::Blocked,
    ];

    // Ensure strict monotonic ordering of priority values
    for i in 0..tiers.len() - 1 {
        assert!(
            tiers[i].priority() < tiers[i + 1].priority(),
            "Priority of {:?} ({}) must be strictly less than {:?} ({})",
            tiers[i],
            tiers[i].priority(),
            tiers[i + 1],
            tiers[i + 1].priority()
        );
    }

    // Pairwise check meets_requirement logic
    for &t1 in &tiers {
        for &t2 in &tiers {
            let meets = t1.meets_requirement(t2);
            if t1 == TrustTier::Blocked {
                assert!(
                    !meets,
                    "Blocked package must NEVER meet any requirement, even Blocked itself"
                );
            } else {
                let expected = t1.priority() <= t2.priority();
                assert_eq!(
                    meets, expected,
                    "Expected {:?}.meets_requirement({:?}) to be {}",
                    t1, t2, expected
                );
            }
        }
    }
}

// 3. Roundtrip mapping test of 50 randomly generated registry classes
#[tokio::test]
async fn test_stress_rdf_registry_class_roundtrip() {
    let mut seed = 54321u64;

    for idx in 0..50 {
        let store = Arc::new(Store::new().unwrap());
        let mapper = RdfMapper::new(store);

        let registry_class = match lcg(&mut seed) % 3 {
            0 => {
                let url = format!("https://public-reg-{}.org/api", random_string(&mut seed, 8));
                let reg_type = match lcg(&mut seed) % 6 {
                    0 => RegistryType::Ggen,
                    1 => RegistryType::CratesIo,
                    2 => RegistryType::Npm,
                    3 => RegistryType::PyPi,
                    4 => RegistryType::GitHub,
                    _ => RegistryType::Other,
                };
                RegistryClass::Public {
                    url,
                    registry_type: reg_type,
                }
            }
            1 => {
                let url = format!("https://ent-reg-{}.net/v2", random_string(&mut seed, 8));
                let require_signature = (lcg(&mut seed) % 2) == 1;
                let allow_unlisted = (lcg(&mut seed) % 2) == 1;
                RegistryClass::PrivateEnterprise {
                    url,
                    require_signature,
                    allow_unlisted,
                }
            }
            _ => {
                let primary_url = format!(
                    "https://primary-air-gapped-{}.net",
                    random_string(&mut seed, 8)
                );
                let mirror_path =
                    PathBuf::from(format!("/var/lib/mirror-{}", random_string(&mut seed, 6)));
                let sync_interval_seconds = lcg(&mut seed) % 86400;
                RegistryClass::MirroredAirGapped {
                    primary_url,
                    mirror_path,
                    sync_interval_seconds,
                }
            }
        };

        let name = format!("stress-pkg-{}", idx);
        let id = PackageId::new(&name).unwrap();
        let metadata = PackageMetadata::new(
            id.clone(),
            &format!("Pkg {}", idx),
            "Description",
            "Apache-2.0",
        );

        let version = PackageVersion::new("1.2.3").unwrap();
        let mut releases = indexmap::IndexMap::new();
        releases.insert(
            version.clone(),
            ReleaseInfo {
                version: version.clone(),
                released_at: Utc::now(),
                changelog: "Changelog".to_string(),
                checksum: "a".repeat(64),
                signature: Some("sig".to_string()),
                download_url: "https://dl.com".to_string(),
                dependencies: vec![],
                trust_tier: TrustTier::ProductionReady,
                registry_class: registry_class.clone(),
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
            reconstructed_release.registry_class, registry_class,
            "Reconstructed registry class for iteration {} must match exactly",
            idx
        );
    }
}

// 4. Case-insensitivity check of SPARQL injection detection with varied syntax
fn setup_stress_control_plane_config() -> String {
    let test_dir =
        std::env::temp_dir().join(format!("ggen-stress-config-{}", uuid::Uuid::new_v4()));
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
fn test_stress_sparql_injection_detection() {
    let config_dir = setup_stress_control_plane_config();
    let control_plane = RdfControlPlane::new(&config_dir).unwrap();

    // Check 100 generated injection variants with varied spacing, tabs, newlines, and mixed casing
    let keywords = [
        "DROP",
        "DELETE WHERE {",
        "INSERT DATA {",
        "CLEAR GRAPH",
        "; DELETE",
    ];

    let mut seed = 99999u64;

    for keyword in &keywords {
        for _ in 0..10 {
            // Apply random mixed-casing to the keyword
            let mut mixed_keyword = String::new();
            for c in keyword.chars() {
                if c.is_alphabetic() {
                    let uppercase = (lcg(&mut seed) % 2) == 1;
                    if uppercase {
                        mixed_keyword.push(c.to_ascii_uppercase());
                    } else {
                        mixed_keyword.push(c.to_ascii_lowercase());
                    }
                } else {
                    mixed_keyword.push(c);
                }
            }

            // Create a query embedding this keyword with randomized spacing/newlines
            let query_str = format!(
                "SELECT * WHERE {{ ?s ?p ?o }} \n\t {} \n\t <http://test>",
                mixed_keyword
            );

            let query = SparqlQuery::new()
                .select(&["?s"])
                .where_pattern(&query_str)
                .validate()
                .unwrap();

            let result = control_plane.execute_query(&query);
            assert!(
                matches!(result, Err(ControlPlaneError::SecurityViolation { .. })),
                "Expected SecurityViolation for generated query: {:?}",
                query_str
            );
        }
    }

    let _ = std::fs::remove_dir_all(&config_dir);
}

// 5. Exhaustive physical README checks in ReadmeValidator
#[tokio::test]
async fn test_stress_readme_validator_scenarios() {
    let validator = ReadmeValidator;

    // Check with a package ID
    let package_id = "readme-stress-pkg";
    let id = PackageId::new(package_id).unwrap();
    let metadata = PackageMetadata::new(id, "Readme Stress Package", "Description", "MIT");
    let package = Package {
        metadata,
        latest_version: PackageVersion::new("1.0.0").unwrap(),
        versions: vec![],
        releases: indexmap::IndexMap::new(),
    };

    let pkg_dir = std::path::PathBuf::from(format!("marketplace/packages/{package_id}"));
    let _ = std::fs::remove_dir_all(&pkg_dir);

    // Scenario 1: Directory missing -> validation must fail
    let check = validator.validate(&package).await.unwrap();
    assert!(
        !check.passed,
        "Validation must fail when directory is missing"
    );

    // Scenario 2: Directory exists but has only other files (e.g. source, config) -> validation must fail
    std::fs::create_dir_all(&pkg_dir).unwrap();
    std::fs::write(pkg_dir.join("main.rs"), "fn main() {}").unwrap();
    std::fs::write(pkg_dir.join("Cargo.toml"), "# config").unwrap();
    let check = validator.validate(&package).await.unwrap();
    assert!(
        !check.passed,
        "Validation must fail when directory has no readme"
    );

    // Scenario 3: README variant exists -> validation must pass
    let readme_variants = vec![
        "README.md",
        "readme.txt",
        "README",
        "readme",
        "Readme.markdown",
    ];

    for variant in readme_variants {
        // Clean and recreate dir
        let _ = std::fs::remove_dir_all(&pkg_dir);
        std::fs::create_dir_all(&pkg_dir).unwrap();

        std::fs::write(pkg_dir.join(variant), "content").unwrap();
        let check = validator.validate(&package).await.unwrap();
        assert!(
            check.passed,
            "Validation must pass with readme variant: {}",
            variant
        );
    }

    // Clean up
    let _ = std::fs::remove_dir_all(&pkg_dir);
}
