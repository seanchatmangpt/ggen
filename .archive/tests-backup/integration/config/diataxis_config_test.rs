//! Diataxis ggen.toml validation and resolution tests
use anyhow::Result;
use ggen_utils::project_config::{DiataxisQuadrant, GgenConfig};
use std::fs;
use std::path::PathBuf;

fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/config")
        .join(name)
}

fn load_fixture(name: &str) -> Result<GgenConfig> {
    let path = fixture_path(name);
    let content = fs::read_to_string(path)?;
    Ok(toml::from_str(&content)?)
}

#[test]
fn test_diataxis_config_validates_and_resolves() -> Result<()> {
    let config = load_fixture("diataxis.ggen.toml")?;

    config.validate()?;
    let sections = config
        .resolved_diataxis_sections()
        .expect("diataxis sections");

    assert!(sections.iter().any(|s| matches!(s.quadrant, DiataxisQuadrant::Tutorials)));
    assert!(sections.iter().any(|s| matches!(s.quadrant, DiataxisQuadrant::Reference)));

    let tutorials = sections
        .iter()
        .find(|s| matches!(s.quadrant, DiataxisQuadrant::Tutorials))
        .unwrap();
    assert_eq!(tutorials.output, "generated/docs/tutorials");
    assert_eq!(tutorials.navigation.len(), 1);

    Ok(())
}
