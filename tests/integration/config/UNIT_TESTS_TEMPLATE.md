# Unit Tests Template for ggen-config Crate

Once CODER creates `crates/ggen-config/`, create these test files:

## crates/ggen-config/tests/parser_tests.rs

```rust
// Unit tests for TOML parsing
// Focus: Edge cases, malformed input, large files

use ggen_config::parser::*;
use anyhow::Result;

#[test]
fn test_parse_empty_file() {
    let result = parse_config("");
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("empty"));
}

#[test]
fn test_parse_minimal_config() -> Result<()> {
    let toml = r#"
        [project]
        name = "test"
        version = "1.0.0"
    "#;
    let config = parse_config(toml)?;
    assert_eq!(config.project.name, "test");
    Ok(())
}

#[test]
fn test_parse_invalid_toml_syntax() {
    let toml = r#"
        [project
        name = "broken
    "#;
    let result = parse_config(toml);
    assert!(result.is_err());
}

#[test]
fn test_parse_unknown_fields_warning() -> Result<()> {
    let toml = r#"
        [project]
        name = "test"
        version = "1.0.0"
        unknown_field = "should warn"
    "#;
    let config = parse_config(toml)?;
    // Should parse but warn about unknown field
    Ok(())
}

#[test]
fn test_parse_large_config_file() -> Result<()> {
    // Test performance with large config
    let mut sections = vec!["[project]\nname = \"test\"\nversion = \"1.0.0\""];
    for i in 0..1000 {
        sections.push(&format!("[custom_{}]\nvalue = \"data\"", i));
    }
    let toml = sections.join("\n");
    let start = std::time::Instant::now();
    let _config = parse_config(&toml)?;
    let duration = start.elapsed();
    assert!(duration.as_millis() < 100, "Parsing too slow: {:?}", duration);
    Ok(())
}
```

## crates/ggen-config/tests/validator_tests.rs

```rust
// Unit tests for schema validation
// Focus: Required fields, constraints, type checking

use ggen_config::validator::*;

#[test]
fn test_validate_missing_required_project_name() {
    let config = Config {
        project: Project {
            name: "".into(),
            version: "1.0.0".into(),
            ..Default::default()
        },
        ..Default::default()
    };
    let result = validate_config(&config);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("name"));
}

#[test]
fn test_validate_invalid_semver() {
    let config = Config {
        project: Project {
            name: "test".into(),
            version: "not-semver".into(),
            ..Default::default()
        },
        ..Default::default()
    };
    let result = validate_config(&config);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("version"));
}

#[test]
fn test_validate_performance_limits() {
    let config = Config {
        performance: Some(Performance {
            memory_limit_mb: 0, // Invalid
            cpu_limit_percent: 150, // Invalid
            ..Default::default()
        }),
        ..Default::default()
    };
    let result = validate_config(&config);
    assert!(result.is_err());
}

#[test]
fn test_validate_ai_provider_settings() -> Result<()> {
    let config = Config {
        ai: Some(AiConfig {
            provider: "openai".into(),
            model: "gpt-4".into(),
            temperature: 0.7,
            max_tokens: 2000,
            ..Default::default()
        }),
        ..Default::default()
    };
    validate_config(&config)?;
    Ok(())
}
```

## crates/ggen-config/tests/resolver_tests.rs

```rust
// Unit tests for dependency resolution
// Focus: Workspace member resolution, version constraints

use ggen_config::resolver::*;

#[test]
fn test_resolve_workspace_dependencies() -> Result<()> {
    let workspace = Workspace {
        members: vec!["pkg-a".into(), "pkg-b".into()],
        dependencies: hashmap! {
            "serde".into() => "1.0".into(),
        },
        ..Default::default()
    };

    let member = MemberConfig {
        project: Project {
            name: "pkg-a".into(),
            ..Default::default()
        },
        ..Default::default()
    };

    let resolved = resolve_with_workspace(&member, &workspace)?;
    assert!(resolved.has_dependency("serde"));
    Ok(())
}

#[test]
fn test_resolve_version_constraints() -> Result<()> {
    let constraints = vec![
        ("^1.0.0", "1.5.3", true),
        ("^1.0.0", "2.0.0", false),
        ("~1.2.3", "1.2.9", true),
        ("~1.2.3", "1.3.0", false),
        (">=1.0, <2.0", "1.9.9", true),
    ];

    for (constraint, version, should_match) in constraints {
        let result = version_matches(constraint, version)?;
        assert_eq!(result, should_match,
            "Constraint {} with version {} should be {}",
            constraint, version, should_match);
    }
    Ok(())
}
```

## crates/ggen-config/tests/workspace_tests.rs

```rust
// Unit tests for workspace features
// Focus: Member discovery, shared config, exclusions

use ggen_config::workspace::*;

#[test]
fn test_discover_workspace_members() -> Result<()> {
    let workspace_dir = tempdir()?;
    create_member(&workspace_dir, "packages/api")?;
    create_member(&workspace_dir, "packages/cli")?;

    let members = discover_members(&workspace_dir)?;
    assert_eq!(members.len(), 2);
    assert!(members.contains(&PathBuf::from("packages/api")));
    Ok(())
}

#[test]
fn test_workspace_exclusions() -> Result<()> {
    let workspace = Workspace {
        members: vec!["packages/*".into()],
        exclude: vec!["packages/legacy".into()],
        ..Default::default()
    };

    let members = expand_workspace_members(&workspace)?;
    assert!(!members.contains(&PathBuf::from("packages/legacy")));
    Ok(())
}

#[test]
fn test_workspace_shared_configuration() -> Result<()> {
    let workspace = Workspace {
        marketplace: Some(MarketplaceConfig {
            registry_url: "https://registry.test".into(),
            ..Default::default()
        }),
        ..Default::default()
    };

    let member = load_member_with_workspace("packages/api", &workspace)?;
    assert_eq!(
        member.marketplace.unwrap().registry_url,
        "https://registry.test"
    );
    Ok(())
}
```

## Test Execution Plan

1. CODER implements `ggen-config` crate structure
2. Create these unit test files in `crates/ggen-config/tests/`
3. Uncomment integration test assertions
4. Run full test suite: `cargo test`
5. Verify 100% pass rate and <2s execution time
6. Report results via hooks

## Quality Checklist

- [ ] All test files created
- [ ] Tests follow Arrange-Act-Assert pattern
- [ ] Clear, descriptive test names
- [ ] Error messages validated
- [ ] Edge cases covered (80/20)
- [ ] Fast execution (<100ms per test)
- [ ] No flaky tests
- [ ] Tests are isolated (no dependencies)
