use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

/// Chicago TDD Test Suite for CLI Application Template
/// Tests real CLI execution, file I/O, and integration scenarios

#[test]
fn test_cli_help_output() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Usage"));
}

#[test]
fn test_command_execution_with_arguments() {
    let temp_dir = TempDir::new().unwrap();
    let output_file = temp_dir.path().join("output.txt");

    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("process")
        .arg("input.txt")
        .arg("--output")
        .arg(output_file.to_str().unwrap())
        .arg("--verbose");

    cmd.assert().success();

    assert!(output_file.exists(), "Output file should be created");
}

#[test]
fn test_subcommand_execution() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("project")
        .arg("new")
        .arg("my-project")
        .arg("--template")
        .arg("rust");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Created project: my-project"));
}

#[test]
fn test_required_argument_validation() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("process");

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("required"));
}

#[test]
fn test_option_with_default_value() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("build")
        .arg("--target")
        .arg("debug");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("target: debug"));
}

#[test]
fn test_multiple_option_values() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("lint")
        .arg("--exclude")
        .arg("*.test.js")
        .arg("--exclude")
        .arg("dist/*");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Excludes: 2"));
}

#[test]
fn test_config_file_loading_toml() {
    let temp_dir = TempDir::new().unwrap();
    let config_file = temp_dir.path().join("config.toml");

    fs::write(&config_file, r#"
[settings]
verbose = true
output_dir = "/tmp/output"
max_workers = 4
"#).unwrap();

    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("--config")
        .arg(config_file.to_str().unwrap())
        .arg("run");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("max_workers: 4"));
}

#[test]
fn test_config_file_loading_yaml() {
    let temp_dir = TempDir::new().unwrap();
    let config_file = temp_dir.path().join("config.yaml");

    fs::write(&config_file, r#"
settings:
  verbose: true
  output_dir: /tmp/output
  max_workers: 4
"#).unwrap();

    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("--config")
        .arg(config_file.to_str().unwrap())
        .arg("run");

    cmd.assert().success();
}

#[test]
fn test_config_file_loading_json() {
    let temp_dir = TempDir::new().unwrap();
    let config_file = temp_dir.path().join("config.json");

    fs::write(&config_file, r#"{
  "settings": {
    "verbose": true,
    "output_dir": "/tmp/output",
    "max_workers": 4
  }
}"#).unwrap();

    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("--config")
        .arg(config_file.to_str().unwrap())
        .arg("run");

    cmd.assert().success();
}

#[test]
fn test_environment_variable_binding() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.env("CLI_VERBOSE", "true")
        .env("CLI_OUTPUT_DIR", "/tmp/test")
        .arg("run");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("output_dir: /tmp/test"));
}

#[test]
fn test_validation_regex_pattern() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("user")
        .arg("add")
        .arg("invalid-email");

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Invalid email format"));
}

#[test]
fn test_validation_range_check() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("server")
        .arg("start")
        .arg("--port")
        .arg("99999");

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Port must be between"));
}

#[test]
fn test_enum_option_validation() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("build")
        .arg("--mode")
        .arg("invalid");

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Invalid value"));

    let mut cmd2 = Command::cargo_bin("test-cli").unwrap();
    cmd2.arg("build")
        .arg("--mode")
        .arg("debug");

    cmd2.assert().success();
}

#[test]
fn test_shell_completion_bash() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("completion")
        .arg("bash");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("complete -F"));
}

#[test]
fn test_shell_completion_zsh() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("completion")
        .arg("zsh");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("compdef"));
}

#[test]
fn test_shell_completion_fish() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("completion")
        .arg("fish");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("complete -c"));
}

#[test]
fn test_verbose_logging_levels() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("-v")
        .arg("run");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("INFO"));

    let mut cmd2 = Command::cargo_bin("test-cli").unwrap();
    cmd2.arg("-vv")
        .arg("run");

    cmd2.assert()
        .success()
        .stdout(predicate::str::contains("DEBUG"));
}

#[test]
fn test_version_flag() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("--version");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("1.0.0"));
}

#[test]
fn test_file_input_processing() {
    let temp_dir = TempDir::new().unwrap();
    let input_file = temp_dir.path().join("input.txt");
    fs::write(&input_file, "test data\n").unwrap();

    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("process")
        .arg(input_file.to_str().unwrap());

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Processed 1 lines"));
}

#[test]
fn test_file_output_creation() {
    let temp_dir = TempDir::new().unwrap();
    let output_file = temp_dir.path().join("output.txt");

    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("generate")
        .arg("--output")
        .arg(output_file.to_str().unwrap());

    cmd.assert().success();

    assert!(output_file.exists());
    let content = fs::read_to_string(&output_file).unwrap();
    assert!(!content.is_empty());
}

#[test]
fn test_interactive_prompt_simulation() {
    use std::process::Stdio;
    use std::io::Write;

    let mut cmd = std::process::Command::new(env!("CARGO_BIN_EXE_test-cli"))
        .arg("init")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let stdin = cmd.stdin.as_mut().unwrap();
    stdin.write_all(b"my-project\n").unwrap();
    stdin.write_all(b"rust\n").unwrap();

    let output = cmd.wait_with_output().unwrap();
    assert!(output.status.success());
}

#[test]
fn test_error_handling_invalid_path() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("process")
        .arg("/nonexistent/file.txt");

    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("No such file"));
}

#[test]
fn test_error_handling_permission_denied() {
    // This test is platform-specific and may need adjustment
    #[cfg(unix)]
    {
        let temp_dir = TempDir::new().unwrap();
        let readonly_file = temp_dir.path().join("readonly.txt");
        fs::write(&readonly_file, "test").unwrap();

        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&readonly_file).unwrap().permissions();
        perms.set_mode(0o444);
        fs::set_permissions(&readonly_file, perms).unwrap();

        let mut cmd = Command::cargo_bin("test-cli").unwrap();
        cmd.arg("write")
            .arg(readonly_file.to_str().unwrap());

        cmd.assert()
            .failure()
            .stderr(predicate::str::contains("Permission denied"));
    }
}

#[test]
fn test_concurrent_command_execution() {
    use std::thread;
    use std::sync::Arc;

    let handles: Vec<_> = (0..5).map(|i| {
        thread::spawn(move || {
            let mut cmd = Command::cargo_bin("test-cli").unwrap();
            cmd.arg("process")
                .arg(format!("input-{}.txt", i))
                .arg("--parallel");

            cmd.assert().success();
        })
    }).collect();

    for handle in handles {
        handle.join().unwrap();
    }
}

#[test]
fn test_signal_handling_ctrl_c() {
    // Platform-specific test for signal handling
    #[cfg(unix)]
    {
        use std::process::{Command, Stdio};
        use std::time::Duration;
        use nix::sys::signal::{self, Signal};
        use nix::unistd::Pid;

        let mut child = Command::new(env!("CARGO_BIN_EXE_test-cli"))
            .arg("long-running")
            .stdout(Stdio::null())
            .spawn()
            .unwrap();

        std::thread::sleep(Duration::from_millis(100));

        signal::kill(Pid::from_raw(child.id() as i32), Signal::SIGINT).unwrap();

        let status = child.wait().unwrap();
        assert!(!status.success());
    }
}

#[test]
fn test_path_argument_validation() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("migrate")
        .arg("--source")
        .arg("/path/to/source")
        .arg("--dest")
        .arg("/path/to/dest");

    cmd.assert().success();
}

#[test]
fn test_color_output_with_env_var() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.env("NO_COLOR", "1")
        .arg("status");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Status:").not().contains("\x1b["));
}

#[test]
fn test_custom_help_template() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("process")
        .arg("--help");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("USAGE:"))
        .stdout(predicate::str::contains("ARGS:"))
        .stdout(predicate::str::contains("OPTIONS:"));
}

#[test]
fn test_command_alias() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("rm")
        .arg("file.txt");

    cmd.assert().success();

    let mut cmd2 = Command::cargo_bin("test-cli").unwrap();
    cmd2.arg("remove")
        .arg("file.txt");

    cmd2.assert().success();
}

#[test]
fn test_global_options_inheritance() {
    let mut cmd = Command::cargo_bin("test-cli").unwrap();
    cmd.arg("--verbose")
        .arg("project")
        .arg("build");

    cmd.assert()
        .success()
        .stdout(predicate::str::contains("verbose: true"));
}
