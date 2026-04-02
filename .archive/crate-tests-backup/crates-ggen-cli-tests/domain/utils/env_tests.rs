use ggen_cli_lib::domain::utils::*;
use std::io::Write;
use tempfile::NamedTempFile;

#[test]
fn test_load_env_file() {
    let mut temp_file = NamedTempFile::new().unwrap();
    writeln!(temp_file, "KEY1=value1").unwrap();
    writeln!(temp_file, "KEY2=value2").unwrap();
    writeln!(temp_file, "# Comment").unwrap();
    writeln!(temp_file, "").unwrap();

    let result = load_env_file(temp_file.path());
    assert!(result.is_ok());

    let env_vars = result.unwrap();
    assert_eq!(env_vars.get("KEY1"), Some(&"value1".to_string()));
    assert_eq!(env_vars.get("KEY2"), Some(&"value2".to_string()));
}

#[test]
fn test_load_env_file_nonexistent() {
    let result = load_env_file(std::path::Path::new("/nonexistent/.env"));
    assert!(result.is_err());
}

#[test]
fn test_get_env_or() {
    let result = get_env_or("NONEXISTENT_VAR_12345", "default");
    assert_eq!(result, "default");
}

#[test]
fn test_is_ci() {
    // Just ensure it doesn't panic
    let _ = is_ci();
}
