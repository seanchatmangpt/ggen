use ggen_domain::project::*;
use tempfile::TempDir;

#[tokio::test]
async fn test_build_project_success() {
    let temp_dir = TempDir::new().unwrap();
    let result = build_project(temp_dir.path()).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_build_project_nonexistent() {
    let result = build_project(std::path::Path::new("/nonexistent")).await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_clean_project() {
    let temp_dir = TempDir::new().unwrap();
    let target_dir = temp_dir.path().join("target");
    std::fs::create_dir(&target_dir).unwrap();

    let result = clean_project(temp_dir.path()).await;
    assert!(result.is_ok());
    assert!(!target_dir.exists());
}
