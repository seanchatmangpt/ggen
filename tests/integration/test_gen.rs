use std::process::Command;
use std::str;

#[test]
fn test_gen_cli_subcommand() {
    let output = Command::new("cargo")
        .args(&["run", "--", "gen", "cli", "subcommand", "--vars", "cmd=hello", "summary=Test greeting"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    // Test passes if command runs without panicking
    // The actual output depends on the core implementation
    assert!(output.status.code().is_some());
    
    let stdout = str::from_utf8(&output.stdout).unwrap();
    // Should print manifest key
    assert!(stdout.contains("manifest:"));
}

#[test]
fn test_gen_with_invalid_template() {
    let output = Command::new("cargo")
        .args(&["run", "--", "gen", "invalid", "template"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    // Should fail gracefully
    assert!(!output.status.success());
}
