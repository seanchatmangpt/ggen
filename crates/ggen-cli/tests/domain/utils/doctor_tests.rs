use ggen_cli_lib::domain::utils::*;

#[tokio::test]
async fn test_run_diagnostics() {
    let result = run_diagnostics().await;
    assert!(result.is_ok());
    let report = result.unwrap();
    // At least one tool should be available on test system
    assert!(report.rust_installed || report.cargo_installed || report.git_installed);
}

#[test]
fn test_check_tool() {
    // Test with a command that should exist
    let result = check_tool("ls");
    // On Unix systems, ls should exist
    #[cfg(unix)]
    assert!(result);
}
