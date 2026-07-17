// CLI public API contract tests
// ARCHIVED (2026-07-16, v26.7.16 publish-safety fix): root no longer depends on
// ggen-cli-lib (see Cargo.toml's `autobins = false` comment -- ggen-cli-lib is now
// `publish = false` since it has a real dependency on ggen-engine, so root can't
// depend on it either without breaking `cargo publish --dry-run -p ggen`). Gated
// behind an empty feature rather than deleted; run with
// `cargo test --workspace --features root-ggen-cli-lib-tests` if this coverage is
// needed again (e.g. from crates/ggen-cli/tests/ instead, where the dependency
// naturally belongs).
#![cfg(feature = "root-ggen-cli-lib-tests")]
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
