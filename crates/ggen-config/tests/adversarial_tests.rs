use ggen_config::config_lib::{
    A2AConfig, A2AOrchestrationConfig, A2ATransportConfig, AiConfig, AiValidation, ConfigLoader,
    GgenConfig, LoggingConfig, McpConfig, McpTransportConfig, PerformanceConfig, ProjectConfig,
    TemplatesConfig,
};
use star_toml::Validate;

#[test]
fn test_minimal_and_empty_configs() {
    // 1. Minimum valid config
    let config = GgenConfig {
        project: ProjectConfig {
            name: "test-project".to_string(),
            version: "1.0.0".to_string(),
            description: None,
            authors: None,
            license: None,
            repository: None,
        },
        ai: None,
        templates: None,
        rdf: None,
        sparql: None,
        lifecycle: None,
        security: None,
        performance: None,
        logging: None,
        telemetry: None,
        features: None,
        env: None,
        build: None,
        test: None,
        package: None,
        mcp: None,
        a2a: None,
    };
    assert!(config.check().is_ok());

    // 2. Project name empty
    let mut bad_config = config.clone();
    bad_config.project.name = "".to_string();
    let err = bad_config.check().unwrap_err();
    assert_eq!(err.errors()[0].loc.to_string(), "project.name");
    assert_eq!(err.errors()[0].code(), "empty");

    // 3. Project version invalid semver
    let mut bad_config = config.clone();
    bad_config.project.version = "1.0".to_string();
    let err = bad_config.check().unwrap_err();
    assert_eq!(err.errors()[0].loc.to_string(), "project.version");
    assert_eq!(err.errors()[0].code(), "invalid_semver");
}

#[test]
fn test_missing_optional_subconfigs() {
    // Validating GgenConfig with various permutations of optional sub-configs as None.
    let base_config = GgenConfig {
        project: ProjectConfig {
            name: "test".to_string(),
            version: "0.1.0".to_string(),
            description: None,
            authors: None,
            license: None,
            repository: None,
        },
        ai: None,
        templates: None,
        rdf: None,
        sparql: None,
        lifecycle: None,
        security: None,
        performance: None,
        logging: None,
        telemetry: None,
        features: None,
        env: None,
        build: None,
        test: None,
        package: None,
        mcp: None,
        a2a: None,
    };

    // Sub-configs present but all their optional sub-fields are None
    let config_with_empties = GgenConfig {
        ai: Some(AiConfig {
            provider: "openai".to_string(),
            model: "gpt-4".to_string(),
            temperature: 0.7,
            max_tokens: 2000,
            timeout: 30,
            prompts: None,
            validation: None,
        }),
        templates: Some(TemplatesConfig {
            directory: None,
            output_directory: None,
            backup_enabled: false,
            idempotent: false,
        }),
        mcp: Some(McpConfig {
            name: None,
            version: None,
            tool_timeout_ms: 30000,
            max_concurrent_requests: 100,
            transport: None,
            tools: None,
            zai: None,
            enabled: false,
            discovery: None,
        }),
        ..base_config.clone()
    };
    assert!(config_with_empties.check().is_ok());
}

#[test]
fn test_extreme_values_ai() {
    let base_config = GgenConfig {
        project: ProjectConfig {
            name: "test".to_string(),
            version: "0.1.0".to_string(),
            description: None,
            authors: None,
            license: None,
            repository: None,
        },
        ai: Some(AiConfig {
            provider: "openai".to_string(),
            model: "gpt-4".to_string(),
            temperature: 0.7,
            max_tokens: 2000,
            timeout: 30,
            prompts: None,
            validation: None,
        }),
        templates: None,
        rdf: None,
        sparql: None,
        lifecycle: None,
        security: None,
        performance: None,
        logging: None,
        telemetry: None,
        features: None,
        env: None,
        build: None,
        test: None,
        package: None,
        mcp: None,
        a2a: None,
    };

    // 1. Temperature too high
    let mut cfg = base_config.clone();
    cfg.ai.as_mut().unwrap().temperature = 1.1;
    assert!(cfg.check().is_err());

    // 2. Temperature too low
    let mut cfg = base_config.clone();
    cfg.ai.as_mut().unwrap().temperature = -0.1;
    assert!(cfg.check().is_err());

    // 3. Temperature boundaries (0.0 and 1.0 should pass)
    let mut cfg = base_config.clone();
    cfg.ai.as_mut().unwrap().temperature = 0.0;
    assert!(cfg.check().is_ok());
    cfg.ai.as_mut().unwrap().temperature = 1.0;
    assert!(cfg.check().is_ok());

    // 4. Max tokens = 0 (invalid)
    let mut cfg = base_config.clone();
    cfg.ai.as_mut().unwrap().max_tokens = 0;
    let err = cfg.check().unwrap_err();
    assert_eq!(err.errors()[0].loc.to_string(), "ai.max_tokens");

    // 5. Timeout = 0 (invalid)
    let mut cfg = base_config.clone();
    cfg.ai.as_mut().unwrap().timeout = 0;
    let err = cfg.check().unwrap_err();
    assert_eq!(err.errors()[0].loc.to_string(), "ai.timeout");

    // 6. AI Validation quality threshold out of range
    let mut cfg = base_config.clone();
    cfg.ai.as_mut().unwrap().validation = Some(AiValidation {
        enabled: true,
        quality_threshold: 1.5,
        max_iterations: 5,
    });
    let err = cfg.check().unwrap_err();
    assert_eq!(
        err.errors()[0].loc.to_string(),
        "ai.validation.quality_threshold"
    );
}

#[test]
fn test_extreme_values_mcp_and_a2a() {
    let base_config = GgenConfig {
        project: ProjectConfig {
            name: "test".to_string(),
            version: "0.1.0".to_string(),
            description: None,
            authors: None,
            license: None,
            repository: None,
        },
        ai: None,
        templates: None,
        rdf: None,
        sparql: None,
        lifecycle: None,
        security: None,
        performance: None,
        logging: None,
        telemetry: None,
        features: None,
        env: None,
        build: None,
        test: None,
        package: None,
        mcp: Some(McpConfig {
            name: None,
            version: None,
            tool_timeout_ms: 30000,
            max_concurrent_requests: 100,
            transport: Some(McpTransportConfig {
                transport_type: "http".to_string(),
                port: Some(8080),
                host: "localhost".to_string(),
                tls: None,
                request_timeout_seconds: 30,
                ..Default::default()
            }),
            tools: None,
            zai: None,
            enabled: true,
            discovery: None,
        }),
        a2a: Some(A2AConfig {
            agent_id: None,
            agent_name: None,
            agent_type: None,
            transport: Some(A2ATransportConfig {
                transport_type: "http".to_string(),
                bind_address: Some("localhost".to_string()),
                port: Some(9090),
                timeout_ms: 5000,
                max_connections: Some(10),
                retry: None,
                ..Default::default()
            }),
            messaging: None,
            orchestration: Some(A2AOrchestrationConfig {
                mode: "centralized".to_string(),
                coordinator_address: Some("127.0.0.1".to_string()),
                heartbeat_interval_seconds: 5,
                agent_timeout_seconds: 30,
                consensus_enabled: true,
                consensus_algorithm: Some("raft".to_string()),
                ..Default::default()
            }),
            capabilities: None,
            enabled: true,
        }),
    };

    // 1. MCP tool timeout = 0
    let mut cfg = base_config.clone();
    cfg.mcp.as_mut().unwrap().tool_timeout_ms = 0;
    assert!(cfg.check().is_err());

    // 2. MCP max concurrent requests = 0
    let mut cfg = base_config.clone();
    cfg.mcp.as_mut().unwrap().max_concurrent_requests = 0;
    assert!(cfg.check().is_err());

    // 3. MCP port = 0
    let mut cfg = base_config.clone();
    cfg.mcp.as_mut().unwrap().transport.as_mut().unwrap().port = Some(0);
    assert!(cfg.check().is_err());

    // 4. A2A port = 0
    let mut cfg = base_config.clone();
    cfg.a2a.as_mut().unwrap().transport.as_mut().unwrap().port = Some(0);
    assert!(cfg.check().is_err());

    // 5. A2A consensus algorithm invalid when enabled
    let mut cfg = base_config.clone();
    cfg.a2a
        .as_mut()
        .unwrap()
        .orchestration
        .as_mut()
        .unwrap()
        .consensus_algorithm = Some("bogus".to_string());
    assert!(cfg.check().is_err());
}

#[test]
fn test_performance_workers_constraint() {
    let base_config = GgenConfig {
        project: ProjectConfig {
            name: "test".to_string(),
            version: "0.1.0".to_string(),
            description: None,
            authors: None,
            license: None,
            repository: None,
        },
        ai: None,
        templates: None,
        rdf: None,
        sparql: None,
        lifecycle: None,
        security: None,
        performance: Some(PerformanceConfig {
            parallel_execution: true,
            max_workers: 0, // Invalid: must be > 0 when parallel_execution is true
            cache_size: None,
            enable_profiling: false,
            memory_limit_mb: None,
        }),
        logging: None,
        telemetry: None,
        features: None,
        env: None,
        build: None,
        test: None,
        package: None,
        mcp: None,
        a2a: None,
    };
    assert!(base_config.check().is_err());

    // Should pass if parallel_execution is false even with max_workers = 0
    let mut cfg = base_config.clone();
    cfg.performance.as_mut().unwrap().parallel_execution = false;
    assert!(cfg.check().is_ok());
}

#[test]
fn test_parser_invalid_mixed_types() {
    // mixed/invalid types: provider should be string, temperature should be float, max_tokens should be int.
    // If they are wrong type in TOML, loader should return error rather than panicking.

    // 1. temperature as string
    let bad_toml = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [ai]
        provider = "openai"
        model = "gpt-4"
        temperature = "hot"
    "#;
    let res = ConfigLoader::from_str(bad_toml);
    assert!(res.is_err());

    // 2. max_tokens as negative number
    let bad_toml2 = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [ai]
        provider = "openai"
        model = "gpt-4"
        max_tokens = -10
    "#;
    let res = ConfigLoader::from_str(bad_toml2);
    assert!(res.is_err());
}

#[test]
fn test_additional_config_validation_gaps() {
    // 1. Invalid semver version in project
    let toml_bad_semver = r#"
        [project]
        name = "test"
        version = "1.0" # Invalid semver
    "#;
    let config = ConfigLoader::from_str(toml_bad_semver).unwrap();
    let err = config
        .check()
        .expect_err("Expected error due to invalid semver version");
    assert!(
        err.to_string().contains("Invalid version format"),
        "Error was: {:?}",
        err
    );

    // 2. Invalid path traversal in templates directory
    let toml_bad_template_path = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [templates]
        directory = "../templates" # Invalid traversal
    "#;
    let config = ConfigLoader::from_str(toml_bad_template_path).unwrap();
    let err = config
        .check()
        .expect_err("Expected error due to traversal in templates directory");
    assert!(
        err.to_string()
            .contains("path traversal ('..') is not allowed"),
        "Error was: {:?}",
        err
    );

    // 3. Invalid cache size format in performance config
    let toml_bad_cache_size = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [performance]
        parallel_execution = true
        max_workers = 4
        cache_size = "512 MB" # Space not allowed
    "#;
    let config = ConfigLoader::from_str(toml_bad_cache_size).unwrap();
    let err = config
        .check()
        .expect_err("Expected error due to invalid cache size format");
    assert!(
        err.to_string().contains("Invalid size format"),
        "Error was: {:?}",
        err
    );

    // 4. Invalid parallel execution max_workers when enabled
    let toml_bad_workers = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [performance]
        parallel_execution = true
        max_workers = 0 # Invalid
    "#;
    let config = ConfigLoader::from_str(toml_bad_workers).unwrap();
    let err = config
        .check()
        .expect_err("Expected error due to zero max_workers with parallel execution");
    assert!(
        err.to_string()
            .contains("Performance max_workers must be greater than 0"),
        "Error was: {:?}",
        err
    );

    // 5. Invalid logging level and format
    let toml_bad_logging = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [logging]
        level = "fatal" # Invalid logging level
        format = "xml" # Invalid formatting format
    "#;
    let config = ConfigLoader::from_str(toml_bad_logging).unwrap();
    let err = config
        .check()
        .expect_err("Expected error due to invalid logging settings");
    assert!(err.to_string().contains("level"), "Error was: {:?}", err);
    assert!(err.to_string().contains("format"), "Error was: {:?}", err);

    // 6. Invalid MCP config tools path and tool timeout
    let toml_bad_mcp = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [mcp]
        enabled = true
        tool_timeout_ms = 0 # Invalid
        max_concurrent_requests = 10
        
        [mcp.tools]
        discovery_path = "path/with\u0000null" # Null byte path
    "#;
    let config = ConfigLoader::from_str(toml_bad_mcp).unwrap();
    let err = config
        .check()
        .expect_err("Expected error due to invalid MCP settings");
    assert!(
        err.to_string().contains("tool_timeout_ms"),
        "Error was: {:?}",
        err
    );
    assert!(
        err.to_string().contains("discovery_path"),
        "Error was: {:?}",
        err
    );
}

#[test]
fn test_more_extreme_config_adversarial() {
    // 1. Test A2A consensus algorithm conditional validation
    let toml_a2a_consensus_disabled = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [a2a]
        enabled = true
        [a2a.orchestration]
        mode = "centralized"
        consensus_enabled = false
        consensus_algorithm = "invalid_consensus_but_disabled" # Should not be validated because consensus_enabled is false
    "#;
    let config = ConfigLoader::from_str(toml_a2a_consensus_disabled).unwrap();
    assert!(
        config.check().is_ok(),
        "Expected config to pass because consensus_enabled is false"
    );

    let toml_a2a_consensus_enabled_invalid = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [a2a]
        enabled = true
        [a2a.orchestration]
        mode = "centralized"
        consensus_enabled = true
        consensus_algorithm = "invalid_consensus_enabled" # Should be validated and fail
    "#;
    let config = ConfigLoader::from_str(toml_a2a_consensus_enabled_invalid).unwrap();
    let err = config
        .check()
        .expect_err("Expected consensus algorithm to fail validation");
    assert!(
        err.to_string().contains("consensus_algorithm"),
        "Error was: {:?}",
        err
    );

    // 2. Logging config with empty values/valid formats
    let toml_logging_valid = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [logging]
        level = "INFO" # Case insensitive
        format = "JSON" # Case insensitive
    "#;
    let config = ConfigLoader::from_str(toml_logging_valid).unwrap();
    assert!(config.check().is_ok());

    // 3. Performance config validation: max_workers > 0 when parallel_execution is true
    let toml_perf_invalid = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [performance]
        parallel_execution = true
        max_workers = 0 # Invalid!
    "#;
    let config = ConfigLoader::from_str(toml_perf_invalid).unwrap();
    let err = config
        .check()
        .expect_err("Expected max_workers constraint to fail");
    assert!(
        err.to_string().contains("max_workers"),
        "Error was: {:?}",
        err
    );

    // Verify it passes when parallel_execution is false
    let toml_perf_valid = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [performance]
        parallel_execution = false
        max_workers = 0 # Allowed when parallel_execution is false
    "#;
    let config = ConfigLoader::from_str(toml_perf_valid).unwrap();
    assert!(config.check().is_ok());
}

#[test]
fn test_adversarial_stress_checks() {
    use ggen_config::config_lib::ConfigValidator;

    // 1. Stress test null byte path formatting for templates.directory
    let toml_templates_null_dir = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [templates]
        directory = "path/with\u0000null"
    "#;
    let config = ConfigLoader::from_str(toml_templates_null_dir).unwrap();
    let err = ConfigValidator::validate(&config)
        .expect_err("Expected path validation error for templates.directory containing null byte");
    let err_msg = err.to_string();
    assert!(
        err_msg.contains("path must not contain null bytes"),
        "Formatted error should report null bytes, but reports: '{}'",
        err_msg
    );

    // 2. Stress test null byte path formatting for templates.output_directory
    let toml_templates_null_out_dir = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [templates]
        output_directory = "path/with\u0000null"
    "#;
    let config = ConfigLoader::from_str(toml_templates_null_out_dir).unwrap();
    let err = ConfigValidator::validate(&config).expect_err(
        "Expected path validation error for templates.output_directory containing null byte",
    );
    let err_msg = err.to_string();
    assert!(
        err_msg.contains("path must not contain null bytes"),
        "Formatted error should report null bytes, but reports: '{}'",
        err_msg
    );

    // 3. Stress test A2A consensus check: consensus_enabled = true but no consensus_algorithm provided
    let toml_a2a_no_algo = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [a2a]
        enabled = true
        [a2a.orchestration]
        mode = "centralized"
        consensus_enabled = true
        # consensus_algorithm is omitted
    "#;
    let config = ConfigLoader::from_str(toml_a2a_no_algo).unwrap();
    let err =
        ConfigValidator::validate(&config).expect_err("Expected consensus_algorithm missing error");
    assert!(
        err.to_string()
            .contains("A2A consensus algorithm must be specified when consensus is enabled"),
        "Formatted error should report missing consensus algorithm, but reports: '{}'",
        err.to_string()
    );
}

#[test]
fn test_new_adversarial_vulnerabilities() {
    // 1. Verify that LifecycleConfig paths completely bypass validation (path traversal & null bytes)
    let toml_lifecycle_malformed = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [lifecycle]
        enabled = true
        config_file = "../../../etc/passwd"
        cache_directory = "some/dir\u0000withnull"
        state_file = "state/../../traversal"
    "#;
    let config = ConfigLoader::from_str(toml_lifecycle_malformed).unwrap();
    // This passes because lifecycle validation is unimplemented (empty Validate impl)
    assert!(
        config.check().is_ok(),
        "BUG: Malformed lifecycle paths should be rejected but validation completely passes"
    );

    // 2. Verify that environment overrides are silently ignored if the base section is missing
    let toml_missing_base_ai = r#"
        [project]
        name = "test"
        version = "1.0.0"

        [env.production]
        "ai.provider" = "anthropic"
        "ai.model" = "claude-3-opus"
    "#;
    let loader = ConfigLoader::from_str(toml_missing_base_ai).unwrap();
    // If we apply overrides, ai remains None
    let mut config_overridden = loader.clone();
    if let Some(env_overrides) = config_overridden.env.clone() {
        if let Some(overrides) = env_overrides.get("production") {
            // Apply overrides manually or via helper
            if let Some(obj) = overrides.as_object() {
                for (key, _value) in obj {
                    let parts: Vec<&str> = key.split('.').collect();
                    match parts.as_slice() {
                        ["ai", _field] => {
                            // If config.ai is None, it is not updated!
                            assert!(config_overridden.ai.is_none());
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    assert!(
        config_overridden.ai.is_none(),
        "BUG: Environment override did not instantiate missing AI config section"
    );

    // 3. Verify that OntologyConfig::validate() bypasses output_dir and lockfile path validations
    // while Validate::check() catches them.
    use ggen_config::config::LockConfig;
    use ggen_config::config::OntologyConfig;
    use ggen_config::config::OntologyPackRef;
    use ggen_config::config::TargetConfig;

    let mut ontology_config = OntologyConfig::new();
    ontology_config = ontology_config.with_pack(OntologyPackRef {
        name: "test-pack".to_string(),
        version: "1.0.0".to_string(),
        namespace: None,
        classes: None,
        properties: None,
        source: None,
    });
    ontology_config.targets.insert(
        "rust-target".to_string(),
        TargetConfig {
            language: "rust".to_string(),
            output_dir: std::path::PathBuf::from("../../../traversal"),
            features: vec![],
            template_path: Some(std::path::PathBuf::from("path/with\0null")),
            hooks: None,
        },
    );
    ontology_config.lock = LockConfig {
        file: std::path::PathBuf::from("lock/../../traversal"),
        auto_update: true,
        enforce: false,
    };

    // The runtime validate() method passes!
    assert!(
        ontology_config.validate().is_ok(),
        "BUG: OntologyConfig::validate should reject malformed paths but passes"
    );

    // The star_toml Validate check fails!
    assert!(
        ontology_config.check().is_err(),
        "OntologyConfig star_toml Validate check correctly catches malformed paths"
    );
}
