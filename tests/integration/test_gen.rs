use chicago_tdd_tools::prelude::*;
use std::process::Command;
use std::str;

test!(test_gen_cli_subcommand, {
    // Arrange
    let mut cmd = Command::new("cargo");
    cmd.args(&["run", "--", "gen", "cli", "subcommand", "--vars", "cmd=hello", "summary=Test greeting"])
        .current_dir("..");

    // Act
    let output = cmd.output().expect("Failed to execute command");

    // Assert
    assert!(output.status.code().is_some());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("manifest:"));
});

test!(test_gen_with_invalid_template, {
    // Arrange
    let mut cmd = Command::new("cargo");
    cmd.args(&["run", "--", "gen", "invalid", "template"])
        .current_dir("..");

    // Act
    let output = cmd.output().expect("Failed to execute command");

    // Assert
    assert!(!output.status.success());
});
