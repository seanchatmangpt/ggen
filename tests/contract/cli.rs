// CLI public API contract tests
use ggen_cli_lib::run_for_node;

#[tokio::test]
async fn test_cli_contract_help_command() {
    let args = vec!["--help".to_string()];
    let result = run_for_node(args).await;
    assert!(result.is_ok());
    let run_res = result.unwrap();

    // Help command should exit successfully or with clap's default help exit code
    assert!(run_res.code == 0 || run_res.code == 2);
}
