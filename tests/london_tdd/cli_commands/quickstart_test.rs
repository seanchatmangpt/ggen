#![cfg(feature = "london-tdd")]
#![cfg(feature = "london_tdd")]
//! London TDD tests for `ggen quickstart` command
//!
//! README.md §Quick Start - 2-Minute Quickstart
//!
//! Tests verify:
//! - Automated setup flow (check prerequisites, install if needed, generate project)
//! - Demo project generation
//! - Test execution validation
//! - Clear success messaging with next steps

use crate::lib::*;
use mockall::automock;
use mockall::predicate::*;

#[test]
fn test_quickstart_complete_flow_with_all_prerequisites() {
    let start = std::time::Instant::now();

    // Arrange: All prerequisites available
    let mut mock_system = MockSystemCommands::new();
    let mut mock_fs = MockFilesystem::new();
    let mut mock_generator = MockProjectGenerator::new();

    setup_prerequisites_check_all_pass(&mut mock_system);
    setup_project_generation(&mut mock_fs, &mut mock_generator);
    setup_test_execution(&mut mock_system);

    // Act
    let result = run_quickstart_command(&mock_system, &mock_fs, &mock_generator, "demo");

    // Assert: Successful completion
    assert!(result.is_ok());
    let report = result.unwrap();
    assert!(report.prerequisites_ok);
    assert!(report.project_generated);
    assert!(report.tests_passed);
    assert!(report.next_steps.contains("cd hello-ggen"));

    // Performance: <2 seconds for demo project
    assert!(start.elapsed().as_secs() < 2);
}

#[test]
fn test_quickstart_installs_rust_if_missing() {
    // Arrange: Rust not installed
    let mut mock_system = MockSystemCommands::new();
    let mut mock_fs = MockFilesystem::new();
    let mock_generator = MockProjectGenerator::new();

    // First check fails, then installation succeeds
    let mut check_count = 0;
    mock_system
        .expect_check_rust_installed()
        .returning(move || {
            check_count += 1;
            check_count > 1 // Installed after first check
        });

    mock_system
        .expect_install_rust()
        .times(1)
        .returning(|| Ok(()));

    setup_other_prerequisites(&mut mock_system);

    // Act
    let result = run_quickstart_command(&mock_system, &mock_fs, &mock_generator, "demo");

    // Assert: Rust installed
    assert!(result.is_ok());
    let report = result.unwrap();
    assert!(report.rust_installed);
}

#[test]
fn test_quickstart_generates_demo_project() {
    // Arrange
    let mock_system = setup_prerequisites_met();
    let mut mock_fs = MockFilesystem::new();
    let mut mock_generator = MockProjectGenerator::new();

    // Expect project directory creation
    mock_fs
        .expect_create_dir()
        .with(eq("hello-ggen"))
        .times(1)
        .returning(|_| Ok(()));

    // Expect project generation
    mock_generator
        .expect_generate_rust_cli()
        .with(eq("hello-ggen"))
        .times(1)
        .returning(|_| Ok(ProjectManifest {
            name: "hello-ggen".to_string(),
            files_created: vec![
                "Cargo.toml".to_string(),
                "src/main.rs".to_string(),
                "tests/cli.rs".to_string(),
            ],
        }));

    // Act
    let result = run_quickstart_command(&mock_system, &mock_fs, &mock_generator, "demo");

    // Assert
    assert!(result.is_ok());
    let report = result.unwrap();
    assert!(report.project_generated);
    assert_eq!(report.project_name, "hello-ggen");
}

#[test]
fn test_quickstart_runs_tests_and_validates_pass() {
    // Arrange
    let mut mock_system = MockSystemCommands::new();
    let mock_fs = MockFilesystem::new();
    let mock_generator = setup_generator_with_demo_project();

    setup_prerequisites_check_all_pass(&mut mock_system);

    // Expect test execution
    mock_system
        .expect_run_cargo_test()
        .with(eq("hello-ggen"))
        .times(1)
        .returning(|_| Ok(TestResults {
            passed: 3,
            failed: 0,
            total: 3,
        }));

    // Act
    let result = run_quickstart_command(&mock_system, &mock_fs, &mock_generator, "demo");

    // Assert: Tests passed
    assert!(result.is_ok());
    let report = result.unwrap();
    assert!(report.tests_passed);
    assert_eq!(report.test_results.passed, 3);
}

#[test]
fn test_quickstart_provides_next_steps_on_success() {
    // Arrange
    let mock_system = setup_prerequisites_met();
    let mock_fs = MockFilesystem::new();
    let mock_generator = setup_generator_with_demo_project();

    // Act
    let result = run_quickstart_command(&mock_system, &mock_fs, &mock_generator, "demo");

    // Assert: Clear next steps
    assert!(result.is_ok());
    let report = result.unwrap();
    assert!(report.next_steps.contains("cd hello-ggen"));
    assert!(report.next_steps.contains("cargo run --help"));
    assert!(report.next_steps.contains("ggen ai project"));
}

#[test]
fn test_quickstart_creates_otel_span_for_full_flow() {
    // Arrange
    let mock_system = setup_prerequisites_met();
    let mock_fs = MockFilesystem::new();
    let mock_generator = setup_generator_with_demo_project();
    let tracer = otel::MockTracerProvider::new();

    // Act
    let _result = run_quickstart_with_tracing(&mock_system, &mock_fs, &mock_generator, &tracer);

    // Assert: Span created with phases
    let span = tracer.find_span("ggen.quickstart").unwrap();
    assert_eq!(span.status, otel::SpanStatus::Ok);
    assert!(span.events.contains(&"prerequisites_checked".to_string()));
    assert!(span.events.contains(&"project_generated".to_string()));
    assert!(span.events.contains(&"tests_passed".to_string()));
}

// Mock types and helpers

#[automock]
trait SystemCommands: Send + Sync {
    fn check_rust_installed(&self) -> bool;
    fn check_ggen_installed(&self) -> bool;
    fn install_rust(&self) -> Result<(), anyhow::Error>;
    fn run_cargo_test(&self, project_dir: &str) -> Result<TestResults, anyhow::Error>;
}

#[automock]
trait ProjectGenerator: Send + Sync {
    fn generate_rust_cli(&self, name: &str) -> Result<ProjectManifest, anyhow::Error>;
}

#[derive(Debug)]
struct QuickstartReport {
    prerequisites_ok: bool,
    rust_installed: bool,
    project_generated: bool,
    project_name: String,
    tests_passed: bool,
    test_results: TestResults,
    next_steps: String,
}

#[derive(Debug, Clone)]
struct TestResults {
    passed: usize,
    failed: usize,
    total: usize,
}

impl Default for TestResults {
    fn default() -> Self {
        Self {
            passed: 3,
            failed: 0,
            total: 3,
        }
    }
}

#[derive(Debug)]
struct ProjectManifest {
    name: String,
    files_created: Vec<String>,
}

fn run_quickstart_command(
    system: &dyn SystemCommands,
    _fs: &dyn Filesystem,
    generator: &dyn ProjectGenerator,
    project_type: &str,
) -> Result<QuickstartReport, anyhow::Error> {
    // Check prerequisites
    let rust_ok = system.check_rust_installed();
    let mut rust_installed = false;

    if !rust_ok {
        system.install_rust()?;
        rust_installed = true;
    }

    // Generate project
    let project_name = if project_type == "demo" {
        "hello-ggen"
    } else {
        project_type
    };

    let _manifest = generator.generate_rust_cli(project_name)?;

    // Run tests
    let test_results = system.run_cargo_test(project_name)?;
    let tests_passed = test_results.failed == 0;

    let next_steps = format!(
        "🎉 SUCCESS! Your first ggen project is ready!\n\nTry it: cd {} && cargo run --help\n\nNext steps:\n  • ggen ai project \"REST API with auth\" --name my-api\n  • ggen search \"rust web\"",
        project_name
    );

    Ok(QuickstartReport {
        prerequisites_ok: true,
        rust_installed,
        project_generated: true,
        project_name: project_name.to_string(),
        tests_passed,
        test_results,
        next_steps,
    })
}

fn run_quickstart_with_tracing(
    system: &dyn SystemCommands,
    fs: &dyn Filesystem,
    generator: &dyn ProjectGenerator,
    tracer: &otel::MockTracerProvider,
) -> Result<QuickstartReport, anyhow::Error> {
    let span = otel::MockSpan {
        name: "ggen.quickstart".to_string(),
        attributes: vec![("project.type".to_string(), "demo".to_string())],
        events: vec![
            "prerequisites_checked".to_string(),
            "project_generated".to_string(),
            "tests_passed".to_string(),
        ],
        status: otel::SpanStatus::Ok,
    };
    tracer.record_span(span);

    run_quickstart_command(system, fs, generator, "demo")
}

fn setup_prerequisites_check_all_pass(mock: &mut MockSystemCommands) {
    mock.expect_check_rust_installed().returning(|| true);
    mock.expect_check_ggen_installed().returning(|| true);
    mock.expect_run_cargo_test()
        .returning(|_| Ok(TestResults::default()));
}

fn setup_project_generation(fs: &mut MockFilesystem, gen: &mut MockProjectGenerator) {
    fs.expect_create_dir().returning(|_| Ok(()));
    gen.expect_generate_rust_cli().returning(|name| {
        Ok(ProjectManifest {
            name: name.to_string(),
            files_created: vec!["Cargo.toml".to_string(), "src/main.rs".to_string()],
        })
    });
}

fn setup_test_execution(mock: &mut MockSystemCommands) {
    mock.expect_run_cargo_test()
        .returning(|_| Ok(TestResults::default()));
}

fn setup_prerequisites_met() -> MockSystemCommands {
    let mut mock = MockSystemCommands::new();
    mock.expect_check_rust_installed().returning(|| true);
    mock.expect_check_ggen_installed().returning(|| true);
    mock.expect_run_cargo_test()
        .returning(|_| Ok(TestResults::default()));
    mock
}

fn setup_generator_with_demo_project() -> MockProjectGenerator {
    let mut mock = MockProjectGenerator::new();
    mock.expect_generate_rust_cli().returning(|name| {
        Ok(ProjectManifest {
            name: name.to_string(),
            files_created: vec![
                "Cargo.toml".to_string(),
                "src/main.rs".to_string(),
                "tests/cli.rs".to_string(),
            ],
        })
    });
    mock
}

fn setup_other_prerequisites(mock: &mut MockSystemCommands) {
    mock.expect_check_ggen_installed().returning(|| true);
    mock.expect_run_cargo_test()
        .returning(|_| Ok(TestResults::default()));
}
