//! Real compiled-binary conformance tests GENERATED from the admitted CLI graph.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};

use serde_json::Value;

const BINARY: &str = env!("CARGO_BIN_EXE_inventoryctl");

struct TestWorkspace {
    root: PathBuf,
}

impl TestWorkspace {
    fn new(label: &str) -> Self {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("the test clock must be after the Unix epoch")
            .as_nanos();
        let root = std::env::temp_dir().join(format!(
            "inventoryctl-{label}-{}-{nonce}",
            std::process::id()
        ));
        fs::create_dir_all(&root).expect("create isolated CLI test workspace");
        Self { root }
    }

    #[allow(dead_code)]
    fn path(&self) -> &Path {
        &self.root
    }

    fn run(&self, noun: &str, verb: &str, arguments: &[&str]) -> Output {
        let mut command = Command::new(BINARY);
        command
            .current_dir(&self.root)
            .arg(noun)
            .arg(verb)
            .args(arguments);
        command.output().expect("execute generated CLI binary")
    }
}

impl Drop for TestWorkspace {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.root);
    }
}

#[allow(dead_code)]
fn parse_stdout(output: &Output) -> Value {
    serde_json::from_slice(&output.stdout).unwrap_or_else(|error| {
        panic!(
            "generated CLI stdout must be JSON: {error}; stdout={:?}; stderr={:?}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        )
    })
}

#[test]
fn real_cli_item_create_matches_admitted_behavior() {
    let workspace = TestWorkspace::new("item-create");
    let arguments: &[&str] = &["--tag", "production", "Widget"];
    let output = workspace.run("item", "create", arguments);
    assert!(
        output.status.success(),
        "generated CLI command failed: stdout={:?}; stderr={:?}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let value = parse_stdout(&output);
    let digest = value["blake3"]
        .as_str()
        .expect("write receipt must contain a BLAKE3 digest");
    assert_eq!(
        digest.len(),
        64,
        "BLAKE3 digest must be 32 bytes in hexadecimal"
    );
    let persisted = workspace
        .path()
        .join(".inventory/items")
        .join(format!("{}.json", "Widget"));
    let bytes = fs::read(&persisted).expect("filesystem behavior must persist the admitted record");
    assert_eq!(blake3::hash(&bytes).to_hex().as_str(), digest);

    let duplicate = workspace.run("item", "create", arguments);
    assert!(
        !duplicate.status.success(),
        "the generated no-clobber boundary must reject duplicate record creation"
    );
    let bytes_after_refusal =
        fs::read(&persisted).expect("the first record must survive duplicate refusal");
    assert_eq!(
        bytes_after_refusal, bytes,
        "duplicate refusal must not mutate the record"
    );
}

#[test]
fn real_cli_item_create_refuses_missing_required_arguments() {
    let workspace = TestWorkspace::new("item-create-missing");
    let output = workspace.run("item", "create", &[]);
    assert!(
        !output.status.success(),
        "clap must reject a generated command when required arguments are omitted"
    );
}

#[test]
fn real_cli_item_list_matches_admitted_behavior() {
    let workspace = TestWorkspace::new("item-list");
    let arguments: &[&str] = &[];
    let output = workspace.run("item", "list", arguments);
    assert!(
        output.status.success(),
        "generated CLI command failed: stdout={:?}; stderr={:?}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let value = parse_stdout(&output);
    assert!(
        !value.is_null(),
        "successful generated behavior must return a JSON value"
    );
    let replay = workspace.run("item", "list", arguments);
    assert!(replay.status.success(), "deterministic replay must succeed");
    assert_eq!(
        replay.stdout, output.stdout,
        "deterministic replay must reproduce byte-identical stdout"
    );
}

#[test]
fn real_cli_math_add_matches_admitted_behavior() {
    let workspace = TestWorkspace::new("math-add");
    let arguments: &[&str] = &["2", "3"];
    let output = workspace.run("math", "add", arguments);
    assert!(
        output.status.success(),
        "generated CLI command failed: stdout={:?}; stderr={:?}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let value = parse_stdout(&output);
    assert!(
        !value.is_null(),
        "successful generated behavior must return a JSON value"
    );
    let replay = workspace.run("math", "add", arguments);
    assert!(replay.status.success(), "deterministic replay must succeed");
    assert_eq!(
        replay.stdout, output.stdout,
        "deterministic replay must reproduce byte-identical stdout"
    );
}

#[test]
fn real_cli_math_add_refuses_missing_required_arguments() {
    let workspace = TestWorkspace::new("math-add-missing");
    let output = workspace.run("math", "add", &[]);
    assert!(
        !output.status.success(),
        "clap must reject a generated command when required arguments are omitted"
    );
}

#[test]
fn real_cli_system_echo_matches_admitted_behavior() {
    let workspace = TestWorkspace::new("system-echo");
    let arguments: &[&str] = &["--uppercase", "hello"];
    let output = workspace.run("system", "echo", arguments);
    assert!(
        output.status.success(),
        "generated CLI command failed: stdout={:?}; stderr={:?}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let value = parse_stdout(&output);
    assert!(
        !value.is_null(),
        "successful generated behavior must return a JSON value"
    );
    let replay = workspace.run("system", "echo", arguments);
    assert!(replay.status.success(), "deterministic replay must succeed");
    assert_eq!(
        replay.stdout, output.stdout,
        "deterministic replay must reproduce byte-identical stdout"
    );
}

#[test]
fn real_cli_system_echo_refuses_missing_required_arguments() {
    let workspace = TestWorkspace::new("system-echo-missing");
    let output = workspace.run("system", "echo", &[]);
    assert!(
        !output.status.success(),
        "clap must reject a generated command when required arguments are omitted"
    );
}

#[test]
fn real_cli_system_ping_matches_admitted_behavior() {
    let workspace = TestWorkspace::new("system-ping");
    let arguments: &[&str] = &[];
    let output = workspace.run("system", "ping", arguments);
    assert!(
        output.status.success(),
        "generated CLI command failed: stdout={:?}; stderr={:?}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let value = parse_stdout(&output);
    assert!(
        !value.is_null(),
        "successful generated behavior must return a JSON value"
    );
    let replay = workspace.run("system", "ping", arguments);
    assert!(replay.status.success(), "deterministic replay must succeed");
    assert_eq!(
        replay.stdout, output.stdout,
        "deterministic replay must reproduce byte-identical stdout"
    );
}

#[test]
fn real_cli_system_refuse_matches_admitted_behavior() {
    let workspace = TestWorkspace::new("system-refuse");
    let arguments: &[&str] = &[];
    let output = workspace.run("system", "refuse", arguments);
    assert!(
        !output.status.success(),
        "refusal behavior must exit nonzero"
    );
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("DEMONSTRATION_REFUSAL"),
        "stderr must carry the admitted refusal code: {:?}",
        String::from_utf8_lossy(&output.stderr)
    );
    let replay = workspace.run("system", "refuse", arguments);
    assert!(
        !replay.status.success(),
        "refusal replay must remain nonzero"
    );
    assert_eq!(
        replay.stderr, output.stderr,
        "deterministic refusal replay must reproduce byte-identical stderr"
    );
}

#[test]
fn real_cli_refuses_unknown_surface() {
    let workspace = TestWorkspace::new("unknown-command");
    let output = workspace.run("ggen-absent-noun", "ggen-absent-verb", &[]);
    assert!(
        !output.status.success(),
        "unknown command surface must fail closed"
    );
}
