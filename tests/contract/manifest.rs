// Manifest validation contract tests
use ggen_core::domain::packs::types::PackFile;

#[test]
fn test_manifest_parsing_and_validation() {
    let toml_content = r#"
[pack]
id = "test-pack"
name = "Test Pack"
version = "1.0.0"
description = "A pack for testing manifest parser contract"
category = "test"
packages = ["pkg1", "pkg2"]
"#;

    let parsed: Result<PackFile, _> = toml::from_str(toml_content);
    assert!(parsed.is_ok());

    let pack_file = parsed.unwrap();
    let pack = pack_file.pack;

    assert_eq!(pack.id, "test-pack");
    assert_eq!(pack.name, "Test Pack");
    assert_eq!(pack.version, "1.0.0");
    assert_eq!(
        pack.description,
        "A pack for testing manifest parser contract"
    );
    assert_eq!(pack.category, "test");
    assert_eq!(pack.packages, vec!["pkg1".to_string(), "pkg2".to_string()]);
}
