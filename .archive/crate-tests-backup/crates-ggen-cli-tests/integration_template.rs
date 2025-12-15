//! Template list test
//!
//! This test validates the most basic CLI command: listing templates.
//! If this works, the core CLI command dispatch is functional.

use assert_cmd::Command;

#[test]
fn test_template_list() {
    Command::cargo_bin("ggen-cli-lib")
        .unwrap()
        .args(&["template", "list"])
        .assert()
        .success();
}
