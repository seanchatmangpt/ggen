use std::process::Command;

#[test]
fn test_help_command() {
    let output = Command::new("cargo")
        .args(&["run", "--", "--help"])
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("reasoner"));
}

#[test]
fn test_classifier_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "classifier", "--help"])
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("classify"));
}

#[test]
fn test_ontology_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "ontology", "--help"])
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("load"));
}

#[test]
fn test_inference_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "inference", "--help"])
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("derive"));
}

#[test]
fn test_validator_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "validator", "--help"])
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("check"));
}
