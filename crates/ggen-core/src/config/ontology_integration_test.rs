//! Integration tests for ontology configuration with ggen.toml and ggen.lock

#[cfg(test)]
mod tests {
    use crate::config::{
        AgentRole, CompositionMetadata, CompositionStrategy, HiveQueen, LockedPackage,
        LockfileManager, OntologyConfig, OntologyLockfile, OntologyPackRef, TargetConfig,
    };
    use std::collections::BTreeMap;
    use std::path::PathBuf;

    /// Test creating ontology configuration from ggen.toml
    #[test]
    fn test_ggen_toml_ontology_section() {
        let config = OntologyConfig::new()
            .with_pack(OntologyPackRef {
                name: "schema-org".to_string(),
                version: "^3.13.0".to_string(),
                namespace: Some("https://schema.org/".to_string()),
                classes: Some(vec!["Product".to_string(), "Offer".to_string()]),
                properties: None,
                source: None,
            })
            .with_composition(CompositionStrategy::Union);

        assert!(config.validate().is_ok());
        assert_eq!(config.packs.len(), 1);
        assert_eq!(config.pack_names(), vec!["schema-org"]);
    }

    /// Test ggen.lock file creation for reproducible builds
    #[test]
    fn test_ggen_lock_creation() {
        let mut packages = BTreeMap::new();

        packages.insert(
            "schema-org".to_string(),
            LockedPackage {
                version: "3.13.0".to_string(),
                resolved: "registry://ggen-marketplace/schema-org@3.13.0".to_string(),
                integrity: "sha256-abc123def456".to_string(),
                location: ".ggen/packages/schema-org/3.13.0".to_string(),
                namespace: Some("https://schema.org/".to_string()),
                classes_count: 788,
                properties_count: 2500,
                dependencies: BTreeMap::new(),
                installed_at: "2024-01-15T10:30:00Z".to_string(),
            },
        );

        let composition = CompositionMetadata {
            strategy: "union".to_string(),
            total_classes: 788,
            total_properties: 2500,
            conflicts_resolved: 0,
            validation_status: "valid".to_string(),
        };

        let lockfile =
            LockfileManager::create(packages, composition).expect("Should create lockfile");

        assert!(lockfile.validate().is_ok());
        assert_eq!(lockfile.packages.len(), 1);
        assert_eq!(lockfile.composition.total_classes, 788);
    }

    /// Test reproducible builds with ggen.lock version spec
    #[test]
    fn test_lock_file_version_spec() {
        let mut packages = BTreeMap::new();

        packages.insert(
            "schema-org".to_string(),
            LockedPackage {
                version: "3.13.0".to_string(),
                resolved: "registry://ggen-marketplace/schema-org@3.13.0".to_string(),
                integrity: "sha256-abc123".to_string(),
                location: ".ggen/packages/schema-org/3.13.0".to_string(),
                namespace: None,
                classes_count: 788,
                properties_count: 2500,
                dependencies: BTreeMap::new(),
                installed_at: "2024-01-15T10:30:00Z".to_string(),
            },
        );

        packages.insert(
            "dublin-core".to_string(),
            LockedPackage {
                version: "1.11.0".to_string(),
                resolved: "registry://ggen-marketplace/dublin-core@1.11.0".to_string(),
                integrity: "sha256-def456".to_string(),
                location: ".ggen/packages/dublin-core/1.11.0".to_string(),
                namespace: None,
                classes_count: 35,
                properties_count: 50,
                dependencies: BTreeMap::new(),
                installed_at: "2024-01-15T10:30:00Z".to_string(),
            },
        );

        let composition = CompositionMetadata {
            strategy: "union".to_string(),
            total_classes: 823,
            total_properties: 2550,
            conflicts_resolved: 0,
            validation_status: "valid".to_string(),
        };

        let lockfile = LockfileManager::create(packages, composition).unwrap();
        let version_spec = lockfile.to_version_spec();

        // Verify exact versions for reproducible builds
        assert_eq!(version_spec.get("schema-org"), Some(&"3.13.0".to_string()));
        assert_eq!(version_spec.get("dublin-core"), Some(&"1.11.0".to_string()));
    }

    /// Test hive queen configuration orchestration
    #[tokio::test]
    async fn test_hive_queen_orchestration() {
        let config = OntologyConfig::new()
            .with_pack(OntologyPackRef {
                name: "schema-org".to_string(),
                version: "3.13.0".to_string(),
                namespace: None,
                classes: None,
                properties: None,
                source: None,
            })
            .with_composition(CompositionStrategy::Union);

        let hive = HiveQueen::new(config).await;
        assert!(hive.is_ok());

        let hive = hive.unwrap();
        // Verify agents were spawned
        assert!(hive.agents.len() >= 4);

        // Verify required agents exist
        let has_analyzer = hive.agents.iter().any(|a| a.role == AgentRole::Analyzer);
        let has_resolver = hive
            .agents
            .iter()
            .any(|a| a.role == AgentRole::VersionResolver);
        let has_validator = hive.agents.iter().any(|a| a.role == AgentRole::Validator);

        assert!(has_analyzer);
        assert!(has_resolver);
        assert!(has_validator);
    }

    /// Test code generation target configuration
    #[test]
    fn test_generation_targets() {
        let mut config = OntologyConfig::new().with_pack(OntologyPackRef {
            name: "schema-org".to_string(),
            version: "3.13.0".to_string(),
            namespace: None,
            classes: None,
            properties: None,
            source: None,
        });

        let typescript_target = TargetConfig {
            language: "typescript".to_string(),
            output_dir: PathBuf::from("src/generated/ts"),
            features: vec!["zod".to_string(), "utilities".to_string()],
            template_path: None,
            hooks: None,
        };

        let rust_target = TargetConfig {
            language: "rust".to_string(),
            output_dir: PathBuf::from("src/generated/rs"),
            features: vec!["serde".to_string()],
            template_path: None,
            hooks: None,
        };

        config = config
            .with_target("typescript".to_string(), typescript_target)
            .with_target("rust".to_string(), rust_target);

        // Verify multiple targets configured
        assert_eq!(config.targets.len(), 2);
        assert_eq!(
            config.target_languages(),
            vec!["rust", "typescript"] // BTreeMap is sorted
        );
    }

    /// Test composition strategy selection
    #[test]
    fn test_composition_strategies() {
        let union_config = OntologyConfig::new().with_composition(CompositionStrategy::Union);

        let intersection_config =
            OntologyConfig::new().with_composition(CompositionStrategy::Intersection);

        let priority_config = OntologyConfig::new().with_composition(CompositionStrategy::Priority);

        assert_eq!(union_config.composition, CompositionStrategy::Union);
        assert_eq!(
            intersection_config.composition,
            CompositionStrategy::Intersection
        );
        assert_eq!(priority_config.composition, CompositionStrategy::Priority);
    }

    /// Test pack filtering by namespace
    #[test]
    fn test_pack_namespace_filtering() {
        let pack_with_namespace = OntologyPackRef {
            name: "schema-org".to_string(),
            version: "3.13.0".to_string(),
            namespace: Some("https://schema.org/ecommerce".to_string()),
            classes: Some(vec!["Product".to_string()]),
            properties: None,
            source: None,
        };

        let config = OntologyConfig::new().with_pack(pack_with_namespace);

        let pack = &config.packs[0];
        assert_eq!(
            pack.namespace,
            Some("https://schema.org/ecommerce".to_string())
        );
        assert_eq!(pack.classes, Some(vec!["Product".to_string()]));
    }

    /// Test CLI environment variable export from lock file
    #[test]
    fn test_lock_file_env_export() {
        let mut packages = BTreeMap::new();

        packages.insert(
            "schema-org".to_string(),
            LockedPackage {
                version: "3.13.0".to_string(),
                resolved: "registry://ggen-marketplace/schema-org@3.13.0".to_string(),
                integrity: "sha256-abc123".to_string(),
                location: ".ggen/packages/schema-org/3.13.0".to_string(),
                namespace: None,
                classes_count: 788,
                properties_count: 2500,
                dependencies: BTreeMap::new(),
                installed_at: "2024-01-15T10:30:00Z".to_string(),
            },
        );

        let composition = CompositionMetadata {
            strategy: "union".to_string(),
            total_classes: 788,
            total_properties: 2500,
            conflicts_resolved: 0,
            validation_status: "valid".to_string(),
        };

        let lockfile = LockfileManager::create(packages, composition).unwrap();
        let env_vars = lockfile.to_env_vars();

        // Verify environment variables can be exported for CI/CD
        assert!(env_vars.contains_key("GGEN_PACK_SCHEMA_ORG_VERSION"));
        assert!(env_vars.contains_key("GGEN_PACK_SCHEMA_ORG_INTEGRITY"));
        assert!(env_vars.contains_key("GGEN_LOCK_GENERATED_AT"));
    }
}
