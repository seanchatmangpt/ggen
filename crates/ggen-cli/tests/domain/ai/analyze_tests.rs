use ggen_cli_lib::domain::ai::*;

#[tokio::test]
async fn test_analyze_code_success() {
    let result = analyze_code("fn main() {}").await;
    assert!(result.is_ok());
    assert!(result.unwrap().contains("characters"));
}

#[tokio::test]
async fn test_analyze_code_empty() {
    let result = analyze_code("").await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_analyze_project_nonexistent() {
    let result = analyze_project(std::path::Path::new("/nonexistent/path")).await;
    assert!(result.is_err());
}
