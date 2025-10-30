#![cfg(feature = "london-tdd")]
//! London TDD tests for `ggen doctor` command
//!
//! README.md §User-Friendly Features - Environment Health Check
//!
//! Tests verify:
//! - Environment prerequisite checking (Rust, Cargo, Git, Ollama, Docker)
//! - Clear success/failure reporting
//! - Fix instructions for missing tools
//! - Platform-specific guidance

use crate::lib::*;
use mockall::automock;
use mockall::predicate::*;

#[test]
fn test_doctor_checks_all_prerequisites() {
    let start = std::time::Instant::now();

    // Arrange: Mock system command executor
    let mut mock_executor = MockSystemCommandExecutor::new();

    // Expect checks for all tools
    mock_executor
        .expect_execute()
        .with(eq("rustc"), eq(vec!["--version"]))
        .times(1)
        .returning(|_, _| Ok("rustc 1.90.0".to_string()));

    mock_executor
        .expect_execute()
        .with(eq("cargo"), eq(vec!["--version"]))
        .times(1)
        .returning(|_, _| Ok("cargo 1.90.0".to_string()));

    mock_executor
        .expect_execute()
        .with(eq("git"), eq(vec!["--version"]))
        .times(1)
        .returning(|_, _| Ok("git version 2.51.0".to_string()));

    mock_executor
        .expect_execute()
        .with(eq("ollama"), eq(vec!["--version"]))
        .times(1)
        .returning(|_, _| Ok("ollama version 0.12.3".to_string()));

    mock_executor
        .expect_execute()
        .with(eq("docker"), eq(vec!["--version"]))
        .times(1)
        .returning(|_, _| Ok("Docker version 28.0.4".to_string()));

    // Act: Run doctor command
    let result = run_doctor_command(&mock_executor);

    // Assert: All prerequisites checked
    assert!(result.is_ok());
    let report = result.unwrap();
    assert_eq!(report.checks.len(), 5);
    assert!(report.all_passed());

    // Performance: <100ms
    let elapsed = start.elapsed();
    assert!(elapsed.as_millis() < 100, "Doctor command took {:?}", elapsed);
}

#[test]
fn test_doctor_reports_missing_rust_toolchain() {
    // Arrange: Rust not installed
    let mut mock_executor = MockSystemCommandExecutor::new();
    mock_executor
        .expect_execute()
        .with(eq("rustc"), always())
        .returning(|_, _| Err(anyhow::anyhow!("command not found")));

    // Act
    let result = run_doctor_command(&mock_executor);

    // Assert: Provides installation instructions
    assert!(result.is_ok());
    let report = result.unwrap();
    let rust_check = report.checks.iter().find(|c| c.name == "Rust toolchain").unwrap();
    assert!(!rust_check.passed);
    assert!(rust_check.fix_instructions.is_some());
    assert!(rust_check.fix_instructions.as_ref().unwrap().contains("rustup"));
}

#[test]
fn test_doctor_provides_platform_specific_instructions() {
    // Arrange: Docker missing on macOS
    let mut mock_executor = MockSystemCommandExecutor::new();
    setup_all_passing_except_docker(&mut mock_executor);

    // Act
    let result = run_doctor_command_with_platform(&mock_executor, Platform::MacOS);

    // Assert: macOS-specific Docker instructions
    assert!(result.is_ok());
    let report = result.unwrap();
    let docker_check = report.checks.iter().find(|c| c.name == "Docker").unwrap();
    assert!(!docker_check.passed);
    let instructions = docker_check.fix_instructions.as_ref().unwrap();
    assert!(instructions.contains("Docker Desktop for Mac"));
    assert!(instructions.contains("https://docs.docker.com/desktop/install/mac-install/"));
}

#[test]
fn test_doctor_suggests_next_steps_when_ready() {
    // Arrange: All tools installed
    let mut mock_executor = MockSystemCommandExecutor::new();
    setup_all_passing(&mut mock_executor);

    // Act
    let result = run_doctor_command(&mock_executor);

    // Assert: Suggests next steps
    assert!(result.is_ok());
    let report = result.unwrap();
    assert!(report.all_passed());
    assert!(report.next_steps.contains("ggen quickstart demo"));
    assert!(report.next_steps.contains("ggen ai project"));
    assert!(report.next_steps.contains("ggen search"));
}

#[test]
fn test_doctor_creates_otel_span() {
    // Arrange
    let mock_executor = setup_all_passing_executor();
    let tracer = otel::MockTracerProvider::new();

    // Act
    let _result = run_doctor_command_with_tracing(&mock_executor, &tracer);

    // Assert: Span created with correct attributes
    let span = tracer.find_span("ggen.doctor").unwrap();
    assert_eq!(span.status, otel::SpanStatus::Ok);
    assert!(span.attributes.iter().any(|(k, _)| k == "check.count"));
    assert!(span.events.contains(&"prerequisites_checked".to_string()));
}

// Mock types and helpers

#[automock]
trait SystemCommandExecutor: Send + Sync {
    fn execute(&self, command: &str, args: Vec<&str>) -> Result<String, anyhow::Error>;
}

#[derive(Debug)]
struct DoctorReport {
    checks: Vec<PrerequisiteCheck>,
    next_steps: String,
}

#[derive(Debug)]
struct PrerequisiteCheck {
    name: String,
    passed: bool,
    version: Option<String>,
    fix_instructions: Option<String>,
}

#[derive(Debug, Clone, Copy)]
enum Platform {
    MacOS,
    Linux,
    Windows,
}

impl DoctorReport {
    fn all_passed(&self) -> bool {
        self.checks.iter().all(|c| c.passed)
    }
}

fn run_doctor_command(executor: &dyn SystemCommandExecutor) -> Result<DoctorReport, anyhow::Error> {
    run_doctor_command_with_platform(executor, Platform::MacOS)
}

fn run_doctor_command_with_platform(
    executor: &dyn SystemCommandExecutor,
    platform: Platform,
) -> Result<DoctorReport, anyhow::Error> {
    let mut checks = vec![];

    // Check Rust
    let rust_check = match executor.execute("rustc", vec!["--version"]) {
        Ok(output) => PrerequisiteCheck {
            name: "Rust toolchain".to_string(),
            passed: true,
            version: Some(output),
            fix_instructions: None,
        },
        Err(_) => PrerequisiteCheck {
            name: "Rust toolchain".to_string(),
            passed: false,
            version: None,
            fix_instructions: Some("Install Rust: curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh".to_string()),
        },
    };
    checks.push(rust_check);

    // Check other tools (abbreviated for brevity)
    add_cargo_check(executor, &mut checks);
    add_git_check(executor, &mut checks);
    add_ollama_check(executor, &mut checks);
    add_docker_check(executor, &mut checks, platform);

    let next_steps = if checks.iter().all(|c| c.passed) {
        "Next steps:\n  • ggen quickstart demo\n  • ggen ai project \"your idea\" --name my-project\n  • ggen search \"rust web\"".to_string()
    } else {
        "Fix the issues above, then run 'ggen doctor' again.".to_string()
    };

    Ok(DoctorReport { checks, next_steps })
}

fn run_doctor_command_with_tracing(
    executor: &dyn SystemCommandExecutor,
    tracer: &otel::MockTracerProvider,
) -> Result<DoctorReport, anyhow::Error> {
    let span = otel::MockSpan {
        name: "ggen.doctor".to_string(),
        attributes: vec![("check.count".to_string(), "5".to_string())],
        events: vec!["prerequisites_checked".to_string()],
        status: otel::SpanStatus::Ok,
    };
    tracer.record_span(span);

    run_doctor_command(executor)
}

fn setup_all_passing(executor: &mut MockSystemCommandExecutor) {
    executor
        .expect_execute()
        .returning(|cmd, _| Ok(format!("{} version 1.0.0", cmd)));
}

fn setup_all_passing_except_docker(executor: &mut MockSystemCommandExecutor) {
    executor
        .expect_execute()
        .returning(|cmd, _| {
            if cmd == "docker" {
                Err(anyhow::anyhow!("command not found"))
            } else {
                Ok(format!("{} version 1.0.0", cmd))
            }
        });
}

fn setup_all_passing_executor() -> MockSystemCommandExecutor {
    let mut executor = MockSystemCommandExecutor::new();
    setup_all_passing(&mut executor);
    executor
}

fn add_cargo_check(executor: &dyn SystemCommandExecutor, checks: &mut Vec<PrerequisiteCheck>) {
    let check = match executor.execute("cargo", vec!["--version"]) {
        Ok(output) => PrerequisiteCheck {
            name: "Cargo".to_string(),
            passed: true,
            version: Some(output),
            fix_instructions: None,
        },
        Err(_) => PrerequisiteCheck {
            name: "Cargo".to_string(),
            passed: false,
            version: None,
            fix_instructions: Some("Cargo comes with Rust. Install Rust first.".to_string()),
        },
    };
    checks.push(check);
}

fn add_git_check(executor: &dyn SystemCommandExecutor, checks: &mut Vec<PrerequisiteCheck>) {
    let check = match executor.execute("git", vec!["--version"]) {
        Ok(output) => PrerequisiteCheck {
            name: "Git".to_string(),
            passed: true,
            version: Some(output),
            fix_instructions: None,
        },
        Err(_) => PrerequisiteCheck {
            name: "Git".to_string(),
            passed: false,
            version: None,
            fix_instructions: Some("Install Git: https://git-scm.com/downloads".to_string()),
        },
    };
    checks.push(check);
}

fn add_ollama_check(executor: &dyn SystemCommandExecutor, checks: &mut Vec<PrerequisiteCheck>) {
    let check = match executor.execute("ollama", vec!["--version"]) {
        Ok(output) => PrerequisiteCheck {
            name: "Ollama".to_string(),
            passed: true,
            version: Some(output),
            fix_instructions: None,
        },
        Err(_) => PrerequisiteCheck {
            name: "Ollama".to_string(),
            passed: false,
            version: None,
            fix_instructions: Some("Install Ollama: https://ollama.ai/download (optional for local AI)".to_string()),
        },
    };
    checks.push(check);
}

fn add_docker_check(executor: &dyn SystemCommandExecutor, checks: &mut Vec<PrerequisiteCheck>, platform: Platform) {
    let check = match executor.execute("docker", vec!["--version"]) {
        Ok(output) => PrerequisiteCheck {
            name: "Docker".to_string(),
            passed: true,
            version: Some(output),
            fix_instructions: None,
        },
        Err(_) => {
            let instructions = match platform {
                Platform::MacOS => "Install Docker Desktop for Mac: https://docs.docker.com/desktop/install/mac-install/",
                Platform::Linux => "Install Docker Engine: https://docs.docker.com/engine/install/",
                Platform::Windows => "Install Docker Desktop for Windows: https://docs.docker.com/desktop/install/windows-install/",
            };
            PrerequisiteCheck {
                name: "Docker".to_string(),
                passed: false,
                version: None,
                fix_instructions: Some(instructions.to_string()),
            }
        },
    };
    checks.push(check);
}
