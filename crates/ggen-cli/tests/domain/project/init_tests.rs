use ggen_cli_lib::domain::project::*;
use std::path::PathBuf;
use tempfile::TempDir;

#[tokio::test]
async fn test_init_project_success() {
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().join("test_project");

    let result = init_project(&project_path, "test_project").await;
    assert!(result.is_ok());
    assert!(project_path.exists());
    assert!(project_path.join("src").exists());
    assert!(project_path.join("tests").exists());
}

#[tokio::test]
async fn test_init_project_empty_name() {
    let temp_dir = TempDir::new().unwrap();
    let result = init_project(temp_dir.path(), "").await;
    assert!(result.is_err());
}

#[test]
fn test_is_project() {
    let temp_dir = TempDir::new().unwrap();
    assert!(is_project(temp_dir.path()));
    assert!(!is_project(std::path::Path::new("/nonexistent")));
}
