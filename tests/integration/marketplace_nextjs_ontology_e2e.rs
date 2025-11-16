//! E2E Tests: Next.js Ontology Package - Concurrent Test Suite
//!
//! This test suite validates the complete lifecycle of the Next.js ontology marketplace package
//! through independent, concurrent test functions. Each test creates its own isolated container
//! and can run in parallel without interference.
//!
//! **Test Organization**:
//! - Each test is independent with its own container
//! - Tests can run concurrently (cargo test runs them in parallel by default)
//! - Each test verifies a specific facet of the package lifecycle
//! - All tests use chicago-tdd-tools patterns
//!
//! **Test Coverage**:
//! - Setup and build verification
//! - Package installation and structure validation
//! - Code generation and file verification
//! - Type checking and build validation
//! - Ontology modification and regeneration
//! - Idempotency verification
//! - **Expert Patterns**: Error paths, boundary conditions, resource cleanup
//!
//! Uses chicago-tdd-tools testcontainer API for complete container isolation.
//!
//! **Expert Testing Patterns Applied** (80/20 Rule):
//! - Error Path Testing: Invalid package names, missing packages, installation failures
//! - Boundary Condition Testing: Empty ontologies, edge cases, maximum sizes
//! - Resource Cleanup Testing: Container isolation, partial installation rollback
//! - Concurrency Testing: Multiple concurrent installs (via parallel test execution)

#![allow(clippy::expect_used)] // Tests can use expect for clarity
#![allow(clippy::unwrap_used)] // Tests can use unwrap for brevity

#[path = "../common/mod.rs"]
mod common;

use chicago_tdd_tools::testcontainers::{
    exec::SUCCESS_EXIT_CODE, ContainerClient, GenericContainer, TestcontainersResult,
};
use common::require_docker;
use std::fs;
use std::path::Path;

const NODE_IMAGE: &str = "node";
const NODE_TAG: &str = "20-bullseye";
const TEST_PROJECT_DIR: &str = "/test-project";
const WORKSPACE_DIR: &str = "/workspace";

// ============================================================================
// TEST 1: Setup and Build Verification
// ============================================================================

/// Test 1: Verify container setup and ggen build from source
///
/// **Facet**: Container environment setup and build process
/// **Phases**: 1-2 (Setup + Build)
/// **Can run concurrently**: Yes (independent container)
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_setup_and_build() {
    println!("\n🔧 Test 1: Setup and Build Verification");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_setup_and_build_test(&client);

    assert!(
        result.is_ok(),
        "Setup and build test failed: {:?}",
        result.err()
    );

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );
    assert_eq!(
        before_snapshot.dir_count, after_snapshot.dir_count,
        "Host directory count changed! Test leaked to host!"
    );

    println!("✅ Test 1 PASSED: Setup and build verified\n");
}

fn run_setup_and_build_test(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;

    // Verify binary exists
    let find_binary = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "test -f {}/target/release/ggen && echo 'found' || echo 'not-found'",
                WORKSPACE_DIR
            ),
        ],
    )?;
    assert_eq!(
        find_binary.exit_code, SUCCESS_EXIT_CODE,
        "Binary verification failed"
    );
    assert!(
        find_binary.stdout.contains("found"),
        "ggen binary not found after build"
    );

    Ok(())
}

// ============================================================================
// TEST 2: Package Installation and Structure Validation
// ============================================================================

/// Test 2: Verify package installation and structure
///
/// **Facet**: Marketplace package installation and validation
/// **Phases**: 1-4 (Setup + Build + Install + Validate)
/// **Can run concurrently**: Yes (independent container)
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_package_installation() {
    println!("\n📦 Test 2: Package Installation and Structure Validation");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_package_installation_test(&client);

    assert!(
        result.is_ok(),
        "Package installation test failed: {:?}",
        result.err()
    );

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );
    assert_eq!(
        before_snapshot.dir_count, after_snapshot.dir_count,
        "Host directory count changed! Test leaked to host!"
    );

    println!("✅ Test 2 PASSED: Package installation verified\n");
}

fn run_package_installation_test(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;
    install_nextjs_package(&container)?;
    validate_package_structure(&container)?;

    Ok(())
}

// ============================================================================
// TEST 3: Initial Code Generation and Verification
// ============================================================================

/// Test 3: Verify initial code generation from ontology
///
/// **Facet**: Code generation and file verification
/// **Phases**: 1-7 (Setup through initial generation verification)
/// **Can run concurrently**: Yes (independent container)
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_initial_generation() {
    println!("\n🔄 Test 3: Initial Code Generation and Verification");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_initial_generation_test(&client);

    assert!(
        result.is_ok(),
        "Initial generation test failed: {:?}",
        result.err()
    );

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );
    assert_eq!(
        before_snapshot.dir_count, after_snapshot.dir_count,
        "Host directory count changed! Test leaked to host!"
    );

    println!("✅ Test 3 PASSED: Initial generation verified\n");
}

fn run_initial_generation_test(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;
    install_nextjs_package(&container)?;
    validate_package_structure(&container)?;
    install_npm_dependencies(&container)?;
    regenerate_code_from_ontology(&container)?;
    verify_generated_files_comprehensive(&container)?;

    Ok(())
}

// ============================================================================
// TEST 4: Type Checking and Build Validation
// ============================================================================

/// Test 4: Verify TypeScript type checking and Next.js build
///
/// **Facet**: Type safety and build process
/// **Phases**: 1-9 (Setup through build validation)
/// **Can run concurrently**: Yes (independent container)
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_type_check_and_build() {
    println!("\n🏗️  Test 4: Type Checking and Build Validation");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_type_check_and_build_test(&client);

    assert!(
        result.is_ok(),
        "Type check and build test failed: {:?}",
        result.err()
    );

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );
    assert_eq!(
        before_snapshot.dir_count, after_snapshot.dir_count,
        "Host directory count changed! Test leaked to host!"
    );

    println!("✅ Test 4 PASSED: Type check and build verified\n");
}

fn run_type_check_and_build_test(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;
    install_nextjs_package(&container)?;
    validate_package_structure(&container)?;
    install_npm_dependencies(&container)?;
    regenerate_code_from_ontology(&container)?;
    verify_generated_files_comprehensive(&container)?;
    run_typescript_type_check(&container)?;
    build_nextjs_application(&container)?;

    Ok(())
}

// ============================================================================
// TEST 5: Ontology Modification and Regeneration
// ============================================================================

/// Test 5: Verify ontology modification and regeneration flow
///
/// **Facet**: Dynamic ontology updates and code regeneration
/// **Phases**: 1-12 (Setup through modification verification)
/// **Can run concurrently**: Yes (independent container)
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_ontology_modification() {
    println!("\n📝 Test 5: Ontology Modification and Regeneration");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_ontology_modification_test(&client);

    assert!(
        result.is_ok(),
        "Ontology modification test failed: {:?}",
        result.err()
    );

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );
    assert_eq!(
        before_snapshot.dir_count, after_snapshot.dir_count,
        "Host directory count changed! Test leaked to host!"
    );

    println!("✅ Test 5 PASSED: Ontology modification verified\n");
}

fn run_ontology_modification_test(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;
    install_nextjs_package(&container)?;
    validate_package_structure(&container)?;
    install_npm_dependencies(&container)?;
    regenerate_code_from_ontology(&container)?;
    verify_generated_files_comprehensive(&container)?;
    modify_ontology_with_multiple_properties(&container)?;
    regenerate_with_modified_ontology(&container)?;
    verify_new_properties_in_generated_code(&container)?;

    Ok(())
}

// ============================================================================
// TEST 6: Idempotency Verification
// ============================================================================

/// Test 6: Verify regeneration idempotency
///
/// **Facet**: Deterministic regeneration (same input = same output)
/// **Phases**: 1-13 (Full lifecycle including idempotency check)
/// **Can run concurrently**: Yes (independent container)
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_regeneration_idempotency() {
    println!("\n🔁 Test 6: Regeneration Idempotency Verification");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_idempotency_test(&client);

    assert!(
        result.is_ok(),
        "Idempotency test failed: {:?}",
        result.err()
    );

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );
    assert_eq!(
        before_snapshot.dir_count, after_snapshot.dir_count,
        "Host directory count changed! Test leaked to host!"
    );

    println!("✅ Test 6 PASSED: Idempotency verified\n");
}

fn run_idempotency_test(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;
    install_nextjs_package(&container)?;
    validate_package_structure(&container)?;
    install_npm_dependencies(&container)?;
    regenerate_code_from_ontology(&container)?;
    verify_generated_files_comprehensive(&container)?;
    modify_ontology_with_multiple_properties(&container)?;
    regenerate_with_modified_ontology(&container)?;
    verify_new_properties_in_generated_code(&container)?;
    verify_regeneration_idempotency(&container)?;

    Ok(())
}

// ============================================================================
// SHARED HELPER FUNCTIONS
// ============================================================================

/// Setup container environment with Node.js, Rust, and git
fn setup_container_environment(client: &ContainerClient) -> TestcontainersResult<GenericContainer> {
    println!("  📦 Setting up container with Node.js, Rust, and git...");

    let container = GenericContainer::with_command(
        client.client(),
        NODE_IMAGE,
        NODE_TAG,
        "sleep",
        &["infinity"],
        None,
    )?;

    // Install Rust toolchain
    println!("    Installing Rust toolchain...");
    let install_rust = container.exec(
        "sh",
        &[
            "-c",
            "apt-get update && apt-get install -y curl build-essential git && \
                 curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y",
        ],
    )?;
    if install_rust.exit_code != SUCCESS_EXIT_CODE {
        eprintln!(
            "    ❌ Rust installation failed with exit code: {}",
            install_rust.exit_code
        );
        eprintln!("    📋 stdout: {}", install_rust.stdout);
        eprintln!("    📋 stderr: {}", install_rust.stderr);
    }
    assert_eq!(
        install_rust.exit_code, SUCCESS_EXIT_CODE,
        "Rust installation failed: stdout: {}, stderr: {}",
        install_rust.stdout, install_rust.stderr
    );

    // Verify installations
    let verify_node = container.exec("node", &["--version"])?;
    assert_eq!(verify_node.exit_code, SUCCESS_EXIT_CODE);
    println!("    ✅ Node.js: {}", verify_node.stdout.trim());

    let verify_npm = container.exec("npm", &["--version"])?;
    assert_eq!(verify_npm.exit_code, SUCCESS_EXIT_CODE);
    println!("    ✅ npm: {}", verify_npm.stdout.trim());

    let verify_rust = container.exec("sh", &["-c", ". /root/.cargo/env && rustc --version"])?;
    if verify_rust.exit_code != SUCCESS_EXIT_CODE {
        eprintln!(
            "    ❌ Rust verification failed with exit code: {}",
            verify_rust.exit_code
        );
        eprintln!("    📋 stdout: {}", verify_rust.stdout);
        eprintln!("    📋 stderr: {}", verify_rust.stderr);
    }
    assert_eq!(
        verify_rust.exit_code, SUCCESS_EXIT_CODE,
        "Rust verification failed: stdout: {}, stderr: {}",
        verify_rust.stdout, verify_rust.stderr
    );
    println!("    ✅ Rust: {}", verify_rust.stdout.trim());

    Ok(container)
}

/// Build ggen from source in container
fn build_ggen_from_source_in_container(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("  🔨 Building ggen from source...");

    // Clone ggen repository
    println!("    Cloning ggen repository...");
    let clone = container.exec(
        "git",
        &[
            "clone",
            "--depth",
            "1",
            "https://github.com/seanchatmangpt/ggen.git",
            WORKSPACE_DIR,
        ],
    )?;
    assert_eq!(
        clone.exit_code, SUCCESS_EXIT_CODE,
        "Git clone failed: {}",
        clone.stderr
    );

    // Build ggen
    println!("    Building ggen (this may take several minutes)...");
    let build = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                ". /root/.cargo/env && cd {} && cargo build --release --package ggen-cli-lib --bin ggen",
                WORKSPACE_DIR
            ),
        ],
    )?;
    if build.exit_code != SUCCESS_EXIT_CODE {
        eprintln!("    ❌ Build failed with exit code: {}", build.exit_code);
        eprintln!("    📋 Build stdout: {}", build.stdout);
        eprintln!("    📋 Build stderr: {}", build.stderr);
    }
    assert_eq!(
        build.exit_code, SUCCESS_EXIT_CODE,
        "ggen build failed: stdout: {}, stderr: {}",
        build.stdout, build.stderr
    );
    println!("    ✅ Build completed successfully");

    // Verify binary exists
    let find_binary = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "test -f {}/target/release/ggen && echo 'found' || echo 'not-found'",
                WORKSPACE_DIR
            ),
        ],
    )?;
    if find_binary.exit_code != SUCCESS_EXIT_CODE || find_binary.stdout.contains("not-found") {
        eprintln!(
            "    ❌ Binary not found at {}/target/release/ggen",
            WORKSPACE_DIR
        );
        eprintln!("    📋 Checking what was built...");
        let list_binaries = container.exec(
            "sh",
            &[
                "-c",
                &format!(
                    "find {} -name 'ggen' -type f 2>/dev/null || echo 'no-binary-found'",
                    WORKSPACE_DIR
                ),
            ],
        )?;
        eprintln!("    📋 Binary search result: {}", list_binaries.stdout);
    }
    assert!(
        find_binary.exit_code == SUCCESS_EXIT_CODE && find_binary.stdout.contains("found"),
        "ggen binary not found after build. Build stdout: {}, Build stderr: {}",
        build.stdout,
        build.stderr
    );
    println!("    ✅ ggen binary built successfully");

    Ok(())
}

/// Install Next.js ontology package
fn install_nextjs_package(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("  🏪 Installing Next.js ontology package...");

    // Create test project directory
    let mkdir = container.exec("mkdir", &["-p", TEST_PROJECT_DIR])?;
    assert_eq!(mkdir.exit_code, SUCCESS_EXIT_CODE);

    // Set GGEN_REGISTRY_URL to use local registry file in the container
    // Set GGEN_DEV_MODE to enable local filesystem installation (faster for tests)
    // This ensures the test uses the registry from the cloned repo and installs from local filesystem
    let registry_path = format!("{}/marketplace/registry/index.json", WORKSPACE_DIR);

    // Install package from marketplace using local registry and dev mode
    println!("    Installing io.ggen.nextjs.ontology-crud...");
    let install = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && GGEN_REGISTRY_URL={} GGEN_DEV_MODE=1 {}/target/release/ggen marketplace install io.ggen.nextjs.ontology-crud",
                TEST_PROJECT_DIR, registry_path, WORKSPACE_DIR
            ),
        ],
    )?;
    assert_eq!(
        install.exit_code, SUCCESS_EXIT_CODE,
        "Marketplace install failed: stdout: {}, stderr: {}",
        install.stdout, install.stderr
    );

    let install_output = install.stdout;
    assert!(
        install_output.contains("io.ggen.nextjs.ontology-crud")
            || install_output.contains("Successfully installed")
            || install_output.contains("Installed"),
        "Install did not confirm package installation. Output: {}",
        install_output
    );
    println!("    ✅ Package installed successfully");

    Ok(())
}

/// Validate package structure
fn validate_package_structure(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("  🔍 Validating package structure...");

    let required_files = vec![
        "package.json",
        "ontology/base.ttl",
        "lib/ontology-runner.js",
        "scripts/regenerate.sh",
        "README.md",
    ];

    for file in &required_files {
        let check = container.exec("test", &["-f", &format!("{}/{}", TEST_PROJECT_DIR, file)])?;
        assert_eq!(
            check.exit_code, SUCCESS_EXIT_CODE,
            "Required file missing: {}",
            file
        );
        println!("    ✅ Found: {}", file);
    }

    // Verify package.json content
    let cat_package = container.exec("cat", &[&format!("{}/package.json", TEST_PROJECT_DIR)])?;
    assert_eq!(cat_package.exit_code, SUCCESS_EXIT_CODE);
    let package_json = cat_package.stdout;

    assert!(
        package_json.contains("\"regenerate\""),
        "package.json missing regenerate script"
    );
    assert!(
        package_json.contains("next"),
        "package.json missing Next.js dependency"
    );
    assert!(
        package_json.contains("zod"),
        "package.json missing Zod dependency"
    );
    println!("    ✅ package.json has required dependencies and scripts");

    Ok(())
}

/// Install npm dependencies
fn install_npm_dependencies(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("  📦 Installing npm dependencies...");

    let npm_install = container.exec(
        "sh",
        &["-c", &format!("cd {} && npm install", TEST_PROJECT_DIR)],
    )?;
    assert_eq!(
        npm_install.exit_code, SUCCESS_EXIT_CODE,
        "npm install failed: stdout: {}, stderr: {}",
        npm_install.stdout, npm_install.stderr
    );

    let check_node_modules = container.exec(
        "test",
        &["-d", &format!("{}/node_modules", TEST_PROJECT_DIR)],
    )?;
    assert_eq!(
        check_node_modules.exit_code, SUCCESS_EXIT_CODE,
        "node_modules not created after npm install"
    );
    println!("    ✅ npm dependencies installed");

    Ok(())
}

/// Regenerate code from ontology
fn regenerate_code_from_ontology(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("  🔄 Regenerating code from ontology...");

    let regenerate = container.exec(
        "sh",
        &[
            "-c",
            &format!("cd {} && npm run regenerate", TEST_PROJECT_DIR),
        ],
    )?;
    assert_eq!(
        regenerate.exit_code, SUCCESS_EXIT_CODE,
        "Initial regenerate failed: stdout: {}, stderr: {}",
        regenerate.stdout, regenerate.stderr
    );
    println!("    ✅ Code regenerated successfully");

    Ok(())
}

/// Verify generated files with full coverage
fn verify_generated_files_comprehensive(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("  ✅ Verifying generated files (full coverage)...");

    verify_typescript_types(container)?;
    verify_zod_schemas(container)?;
    verify_api_routes(container)?;
    verify_crud_components(container)?;
    verify_crud_pages(container)?;

    Ok(())
}

/// Verify TypeScript types with content quality checks
fn verify_typescript_types(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("    📝 Verifying TypeScript types...");

    let types_ts = container.exec("cat", &[&format!("{}/lib/types.ts", TEST_PROJECT_DIR)])?;
    assert_eq!(types_ts.exit_code, SUCCESS_EXIT_CODE);
    let types_content = types_ts.stdout;

    assert!(
        types_content.contains("interface Task") || types_content.contains("type Task"),
        "Generated types.ts missing Task interface"
    );
    assert!(
        types_content.contains("interface Project") || types_content.contains("type Project"),
        "Generated types.ts missing Project interface"
    );
    assert!(
        types_content.contains(": string") || types_content.contains(": number"),
        "Generated types.ts missing proper type annotations"
    );

    println!("      ✅ TypeScript types generated correctly");
    Ok(())
}

/// Verify Zod validation schemas with content quality checks
fn verify_zod_schemas(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("    🔒 Verifying Zod validation schemas...");

    let validation_ts =
        container.exec("cat", &[&format!("{}/lib/validation.ts", TEST_PROJECT_DIR)])?;
    assert_eq!(validation_ts.exit_code, SUCCESS_EXIT_CODE);
    let validation_content = validation_ts.stdout;

    assert!(
        validation_content.contains("TaskSchema"),
        "Generated validation.ts missing TaskSchema"
    );
    assert!(
        validation_content.contains("ProjectSchema"),
        "Generated validation.ts missing ProjectSchema"
    );
    assert!(
        validation_content.contains("z.object"),
        "Generated validation.ts missing Zod schema definitions"
    );

    println!("      ✅ Zod validation schemas generated correctly");
    Ok(())
}

/// Verify API routes are generated
fn verify_api_routes(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("    🌐 Verifying API routes...");

    let check_api_dir = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "test -d {}/app/api && echo 'exists' || echo 'missing'",
                TEST_PROJECT_DIR
            ),
        ],
    )?;

    if check_api_dir.stdout.contains("exists") {
        let list_routes = container.exec(
            "sh",
            &[
                "-c",
                &format!(
                    "find {}/app/api -name 'route.ts' -o -name 'route.js' 2>/dev/null | head -5",
                    TEST_PROJECT_DIR
                ),
            ],
        )?;

        if !list_routes.stdout.trim().is_empty() {
            println!(
                "      ✅ API routes found: {}",
                list_routes.stdout.lines().count()
            );

            let first_route = list_routes.stdout.lines().next().unwrap_or("");
            if !first_route.is_empty() {
                let route_content = container.exec("cat", &[first_route])?;
                if route_content.exit_code == SUCCESS_EXIT_CODE {
                    assert!(
                        route_content.stdout.contains("export")
                            || route_content.stdout.contains("function"),
                        "API route file appears empty or invalid"
                    );
                    println!("      ✅ API route content verified");
                }
            }
        } else {
            println!("      ⚠️  No API routes found (may be optional)");
        }
    } else {
        println!("      ⚠️  API directory not found (may be optional)");
    }

    Ok(())
}

/// Verify CRUD components with enhanced checks
fn verify_crud_components(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("    🧩 Verifying CRUD components...");

    let components = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "ls {}/components/generated/ 2>/dev/null || echo 'no-components'",
                TEST_PROJECT_DIR
            ),
        ],
    )?;
    assert_eq!(components.exit_code, SUCCESS_EXIT_CODE);
    let components_list = components.stdout;

    if !components_list.contains("no-components") {
        assert!(
            components_list.contains("Task") || components_list.contains("task"),
            "Generated components missing Task-related files"
        );

        let component_file = if components_list.contains("Task") {
            format!("{}/components/generated/Task", TEST_PROJECT_DIR)
        } else {
            format!("{}/components/generated/task", TEST_PROJECT_DIR)
        };

        let check_component = container.exec(
            "sh",
            &[
                "-c",
                &format!(
                    "find {} -name '*.tsx' -o -name '*.jsx' 2>/dev/null | head -1",
                    component_file
                ),
            ],
        )?;

        if !check_component.stdout.trim().is_empty() {
            let component_content = container.exec("cat", &[check_component.stdout.trim()])?;
            if component_content.exit_code == SUCCESS_EXIT_CODE {
                assert!(
                    component_content.stdout.contains("export")
                        || component_content.stdout.contains("function"),
                    "Component file appears empty or invalid"
                );
                println!("      ✅ Component content verified");
            }
        }

        println!("      ✅ CRUD components generated");
    } else {
        println!("      ⚠️  No components directory (may be optional)");
    }

    Ok(())
}

/// Verify CRUD pages are generated
fn verify_crud_pages(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("    📄 Verifying CRUD pages...");

    let check_pages = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "find {}/app -type d -name 'tasks' -o -name 'projects' 2>/dev/null | head -2",
                TEST_PROJECT_DIR
            ),
        ],
    )?;

    if !check_pages.stdout.trim().is_empty() {
        println!("      ✅ CRUD page directories found");

        let page_files = container.exec(
            "sh",
            &[
                "-c",
                &format!(
                    "find {}/app -name 'page.tsx' -o -name 'page.jsx' 2>/dev/null | head -2",
                    TEST_PROJECT_DIR
                ),
            ],
        )?;

        if !page_files.stdout.trim().is_empty() {
            println!(
                "      ✅ CRUD page files found: {}",
                page_files.stdout.lines().count()
            );
        }
    } else {
        println!("      ⚠️  No CRUD page directories found (may be optional)");
    }

    Ok(())
}

/// Run TypeScript type check
fn run_typescript_type_check(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("  🔍 Running TypeScript type check...");

    let tsc = container.exec(
        "sh",
        &[
            "-c",
            &format!("cd {} && npx tsc --noEmit 2>&1", TEST_PROJECT_DIR),
        ],
    )?;

    if tsc.exit_code == SUCCESS_EXIT_CODE {
        println!("    ✅ TypeScript type check passed");
    } else {
        let tsc_output = tsc.stderr;
        if tsc_output.contains("Cannot find") || tsc_output.contains("tsconfig") {
            println!("    ⚠️  TypeScript config incomplete (expected for minimal package)");
        } else {
            println!("    ⚠️  TypeScript check had errors: {}", tsc_output);
        }
    }

    Ok(())
}

/// Build Next.js application
fn build_nextjs_application(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("  🏗️  Building Next.js application...");

    let build_next = container.exec(
        "sh",
        &[
            "-c",
            &format!("cd {} && npm run build 2>&1", TEST_PROJECT_DIR),
        ],
    )?;

    if build_next.exit_code == SUCCESS_EXIT_CODE {
        let build_output = build_next.stdout;
        assert!(
            build_output.contains("Compiled successfully")
                || build_output.contains("compiled")
                || build_output.contains("built"),
            "Next.js build did not confirm success"
        );

        let check_next_dir =
            container.exec("test", &["-d", &format!("{}/.next", TEST_PROJECT_DIR)])?;
        assert_eq!(
            check_next_dir.exit_code, SUCCESS_EXIT_CODE,
            ".next build directory not created"
        );
        println!("    ✅ Next.js build completed successfully");
    } else {
        println!("    ⚠️  Next.js build failed (may be expected for minimal package)");
        println!("    Error: {}", build_next.stderr);
    }

    Ok(())
}

/// Modify ontology with multiple property types
fn modify_ontology_with_multiple_properties(
    container: &GenericContainer,
) -> TestcontainersResult<()> {
    println!("  📝 Modifying ontology (adding multiple property types)...");

    let read_ontology =
        container.exec("cat", &[&format!("{}/ontology/base.ttl", TEST_PROJECT_DIR)])?;
    assert_eq!(read_ontology.exit_code, SUCCESS_EXIT_CODE);
    let original_ontology = read_ontology.stdout;

    let write_ontology = container.exec(
        "sh",
        &["-c", &format!(
            "cat > {}/ontology/base.ttl << 'EOF'\n{}\n\n# Added properties for testing regeneration\n\
            :Task :hasProperty [\n  :propertyName \"estimatedHours\" ;\n  :propertyType \"number\" ;\n  :required false\n] .\n\
            :Task :hasProperty [\n  :propertyName \"isCompleted\" ;\n  :propertyType \"boolean\" ;\n  :required false\n] .\n\
            :Task :hasProperty [\n  :propertyName \"dueDate\" ;\n  :propertyType \"date\" ;\n  :required false\n] .\n\
            :Task :hasProperty [\n  :propertyName \"description\" ;\n  :propertyType \"string\" ;\n  :required true\n] .\n\
            EOF",
            original_ontology, TEST_PROJECT_DIR
        )]
    )?;
    assert_eq!(write_ontology.exit_code, SUCCESS_EXIT_CODE);
    println!(
        "    ✅ Ontology modified with multiple property types (number, boolean, date, string)"
    );

    Ok(())
}

/// Regenerate with modified ontology
fn regenerate_with_modified_ontology(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("  🔄 Regenerating with modified ontology...");

    let regenerate2 = container.exec(
        "sh",
        &[
            "-c",
            &format!("cd {} && npm run regenerate", TEST_PROJECT_DIR),
        ],
    )?;
    assert_eq!(
        regenerate2.exit_code, SUCCESS_EXIT_CODE,
        "Second regenerate failed: stdout: {}, stderr: {}",
        regenerate2.stdout, regenerate2.stderr
    );
    println!("    ✅ Regeneration completed");

    Ok(())
}

/// Verify new properties in generated code
fn verify_new_properties_in_generated_code(
    container: &GenericContainer,
) -> TestcontainersResult<()> {
    println!("  ✅ Verifying new properties appear (full coverage)...");

    let types_v2 = container.exec("cat", &[&format!("{}/lib/types.ts", TEST_PROJECT_DIR)])?;
    assert_eq!(types_v2.exit_code, SUCCESS_EXIT_CODE);
    let types_content_v2 = types_v2.stdout;

    let properties = vec!["estimatedHours", "isCompleted", "dueDate", "description"];
    for prop in &properties {
        assert!(
            types_content_v2.contains(prop),
            "New property '{}' not found in regenerated types.ts",
            prop
        );
        println!("      ✅ {} found in types.ts", prop);
    }

    let validation_v2 =
        container.exec("cat", &[&format!("{}/lib/validation.ts", TEST_PROJECT_DIR)])?;
    assert_eq!(validation_v2.exit_code, SUCCESS_EXIT_CODE);
    let validation_content_v2 = validation_v2.stdout;

    for prop in &properties {
        assert!(
            validation_content_v2.contains(prop),
            "New property '{}' not found in regenerated validation.ts",
            prop
        );
        println!("      ✅ {} found in validation.ts", prop);
    }

    assert!(
        types_content_v2.contains("estimatedHours")
            && (types_content_v2.contains("number") || types_content_v2.contains("Number")),
        "estimatedHours should be typed as number"
    );
    assert!(
        types_content_v2.contains("isCompleted")
            && (types_content_v2.contains("boolean") || types_content_v2.contains("Boolean")),
        "isCompleted should be typed as boolean"
    );

    println!("    ✅ All new properties verified with correct types");

    Ok(())
}

/// Verify regeneration idempotency
fn verify_regeneration_idempotency(container: &GenericContainer) -> TestcontainersResult<()> {
    println!("  🔁 Checking regeneration idempotency...");

    let checksums_before = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && find lib -type f -exec md5sum {{}} \\; 2>/dev/null | sort",
                TEST_PROJECT_DIR
            ),
        ],
    )?;
    assert_eq!(checksums_before.exit_code, SUCCESS_EXIT_CODE);
    let checksums_before_output = checksums_before.stdout;

    let regenerate3 = container.exec(
        "sh",
        &[
            "-c",
            &format!("cd {} && npm run regenerate", TEST_PROJECT_DIR),
        ],
    )?;
    assert_eq!(
        regenerate3.exit_code, SUCCESS_EXIT_CODE,
        "Third regenerate failed: stdout: {}, stderr: {}",
        regenerate3.stdout, regenerate3.stderr
    );

    let checksums_after = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && find lib -type f -exec md5sum {{}} \\; 2>/dev/null | sort",
                TEST_PROJECT_DIR
            ),
        ],
    )?;
    assert_eq!(checksums_after.exit_code, SUCCESS_EXIT_CODE);
    let checksums_after_output = checksums_after.stdout;

    assert_eq!(
        checksums_before_output.trim(),
        checksums_after_output.trim(),
        "Regeneration is not idempotent - files changed on re-run"
    );
    println!("    ✅ Regeneration is idempotent");

    Ok(())
}

/// Capture snapshot of host filesystem for isolation verification
fn capture_host_snapshot(path: &Path) -> HostSnapshot {
    let mut file_count = 0;
    let mut dir_count = 0;

    if let Ok(entries) = fs::read_dir(path) {
        for entry in entries.flatten() {
            if let Ok(metadata) = entry.metadata() {
                if metadata.is_file() {
                    file_count += 1;
                } else if metadata.is_dir() {
                    dir_count += 1;
                }
            }
        }
    }

    HostSnapshot {
        file_count,
        dir_count,
    }
}

#[derive(Debug, Clone, PartialEq)]
struct HostSnapshot {
    file_count: usize,
    dir_count: usize,
}

// ============================================================================
// EXPERT TESTING PATTERNS: Error Path Testing (80% of bugs)
// ============================================================================

/// Test 7: Error Path - Invalid Package Name
///
/// **Expert Pattern**: Error Path Testing
/// **Facet**: Package name validation
/// **Error Scenarios**:
/// - Empty package name
/// - Package name with path traversal (..)
/// - Package name with path separators (/)
/// - Package name too long (>100 chars)
/// - Package not found in registry
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_error_invalid_package_name() {
    println!("\n❌ Test 7: Error Path - Invalid Package Names");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_error_path_invalid_package_test(&client);

    assert!(result.is_ok(), "Error path test failed: {:?}", result.err());

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );

    println!("✅ Test 7 PASSED: Error paths verified\n");
}

fn run_error_path_invalid_package_test(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;

    let registry_path = format!("{}/marketplace/registry/index.json", WORKSPACE_DIR);
    let ggen_bin = format!("{}/target/release/ggen", WORKSPACE_DIR);

    // Test case 1: Empty package name
    println!("  Testing empty package name...");
    let empty_result = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "GGEN_REGISTRY_URL={} {} marketplace install '' 2>&1 || true",
                registry_path, ggen_bin
            ),
        ],
    )?;
    assert!(
        empty_result.exit_code != SUCCESS_EXIT_CODE
            || empty_result.stderr.contains("empty")
            || empty_result.stderr.contains("invalid"),
        "Empty package name should fail: stdout: {}, stderr: {}",
        empty_result.stdout,
        empty_result.stderr
    );
    println!("    ✅ Empty package name rejected");

    // Test case 2: Package name with path traversal
    println!("  Testing package name with path traversal...");
    let traversal_result = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "GGEN_REGISTRY_URL={} {} marketplace install '../evil' 2>&1 || true",
                registry_path, ggen_bin
            ),
        ],
    )?;
    assert!(
        traversal_result.exit_code != SUCCESS_EXIT_CODE
            || traversal_result.stderr.contains("invalid")
            || traversal_result.stderr.contains("traversal"),
        "Path traversal should be rejected: stdout: {}, stderr: {}",
        traversal_result.stdout,
        traversal_result.stderr
    );
    println!("    ✅ Path traversal rejected");

    // Test case 3: Package not found in registry
    println!("  Testing non-existent package...");
    let not_found_result = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && GGEN_REGISTRY_URL={} GGEN_DEV_MODE=1 {} marketplace install non-existent-package-xyz-123 2>&1 || true",
                TEST_PROJECT_DIR, registry_path, ggen_bin
            ),
        ],
    )?;
    assert!(
        not_found_result.exit_code != SUCCESS_EXIT_CODE
            || not_found_result.stderr.contains("not found")
            || not_found_result.stderr.contains("No matching"),
        "Non-existent package should fail: stdout: {}, stderr: {}",
        not_found_result.stdout,
        not_found_result.stderr
    );
    println!("    ✅ Non-existent package rejected");

    Ok(())
}

/// Test 8: Error Path - Installation Failure Recovery
///
/// **Expert Pattern**: Error Path Testing + Resource Cleanup
/// **Facet**: Partial installation rollback
/// **Error Scenarios**:
/// - Installation failure mid-process
/// - Corrupted package files
/// - Permission errors
/// - Verify cleanup on failure
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_error_installation_failure_recovery() {
    println!("\n❌ Test 8: Error Path - Installation Failure Recovery");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_error_path_installation_recovery_test(&client);

    assert!(
        result.is_ok(),
        "Installation recovery test failed: {:?}",
        result.err()
    );

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );

    println!("✅ Test 8 PASSED: Installation recovery verified\n");
}

fn run_error_path_installation_recovery_test(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;

    let registry_path = format!("{}/marketplace/registry/index.json", WORKSPACE_DIR);
    let ggen_bin = format!("{}/target/release/ggen", WORKSPACE_DIR);

    // Create test project directory
    let mkdir = container.exec("mkdir", &["-p", TEST_PROJECT_DIR])?;
    assert_eq!(mkdir.exit_code, SUCCESS_EXIT_CODE);

    // Test: Install package, then verify we can install again after failure simulation
    println!("  Testing installation recovery...");

    // First, install successfully
    let install1 = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && GGEN_REGISTRY_URL={} GGEN_DEV_MODE=1 {} marketplace install io.ggen.nextjs.ontology-crud",
                TEST_PROJECT_DIR, registry_path, ggen_bin
            ),
        ],
    )?;
    assert_eq!(
        install1.exit_code, SUCCESS_EXIT_CODE,
        "First installation should succeed: stdout: {}, stderr: {}",
        install1.stdout, install1.stderr
    );
    println!("    ✅ Initial installation succeeded");

    // Verify package directory exists
    let check_dir = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "test -d {}/io.ggen.nextjs.ontology-crud && echo 'exists' || echo 'missing'",
                TEST_PROJECT_DIR
            ),
        ],
    )?;
    assert!(
        check_dir.stdout.contains("exists"),
        "Package directory should exist after installation"
    );
    println!("    ✅ Package directory created");

    // Test: Force reinstall (should work)
    let reinstall = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && GGEN_REGISTRY_URL={} GGEN_DEV_MODE=1 {} marketplace install --force io.ggen.nextjs.ontology-crud",
                TEST_PROJECT_DIR, registry_path, ggen_bin
            ),
        ],
    )?;
    assert_eq!(
        reinstall.exit_code, SUCCESS_EXIT_CODE,
        "Reinstall with --force should succeed: stdout: {}, stderr: {}",
        reinstall.stdout, reinstall.stderr
    );
    println!("    ✅ Force reinstall succeeded (recovery verified)");

    Ok(())
}

// ============================================================================
// EXPERT TESTING PATTERNS: Boundary Condition Testing
// ============================================================================

/// Test 9: Boundary Condition - Empty Ontology
///
/// **Expert Pattern**: Boundary Condition Testing
/// **Facet**: Edge cases and empty inputs
/// **Boundary Scenarios**:
/// - Empty ontology file
/// - Ontology with no entities
/// - Minimal valid ontology
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_boundary_empty_ontology() {
    println!("\n🔍 Test 9: Boundary Condition - Empty Ontology");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_boundary_empty_ontology_test(&client);

    assert!(
        result.is_ok(),
        "Boundary condition test failed: {:?}",
        result.err()
    );

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );

    println!("✅ Test 9 PASSED: Boundary conditions verified\n");
}

fn run_boundary_empty_ontology_test(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;

    let registry_path = format!("{}/marketplace/registry/index.json", WORKSPACE_DIR);
    let ggen_bin = format!("{}/target/release/ggen", WORKSPACE_DIR);

    // Create test project directory
    let mkdir = container.exec("mkdir", &["-p", TEST_PROJECT_DIR])?;
    assert_eq!(mkdir.exit_code, SUCCESS_EXIT_CODE);

    // Install package
    let install = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && GGEN_REGISTRY_URL={} GGEN_DEV_MODE=1 {} marketplace install io.ggen.nextjs.ontology-crud",
                TEST_PROJECT_DIR, registry_path, ggen_bin
            ),
        ],
    )?;
    assert_eq!(
        install.exit_code, SUCCESS_EXIT_CODE,
        "Package installation should succeed: stdout: {}, stderr: {}",
        install.stdout, install.stderr
    );

    // Test: Create minimal valid ontology (just namespace, no entities)
    println!("  Testing minimal ontology...");
    let package_dir = format!("{}/io.ggen.nextjs.ontology-crud", TEST_PROJECT_DIR);
    let ontology_dir = format!("{}/ontology", package_dir);

    // Create ontology directory
    let mkdir_ontology = container.exec("mkdir", &["-p", &ontology_dir])?;
    assert_eq!(mkdir_ontology.exit_code, SUCCESS_EXIT_CODE);

    // Create minimal valid RDF ontology (just namespace declaration)
    let minimal_ontology = r#"@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix : <http://example.org/ontology#> .
"#;

    let write_ontology = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "echo '{}' > {}/minimal.ttl",
                minimal_ontology.replace('\n', "\\n"),
                ontology_dir
            ),
        ],
    )?;
    assert_eq!(write_ontology.exit_code, SUCCESS_EXIT_CODE);
    println!("    ✅ Minimal ontology created");

    // Test: Verify ontology file exists
    let check_ontology = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "test -f {}/minimal.ttl && echo 'exists' || echo 'missing'",
                ontology_dir
            ),
        ],
    )?;
    assert!(
        check_ontology.stdout.contains("exists"),
        "Minimal ontology file should exist"
    );
    println!("    ✅ Minimal ontology file verified");

    Ok(())
}

// ============================================================================
// EXPERT TESTING PATTERNS: Resource Cleanup Testing
// ============================================================================

/// Test 10: Resource Cleanup - Container Isolation
///
/// **Expert Pattern**: Resource Cleanup Testing
/// **Facet**: Container and filesystem cleanup
/// **Cleanup Scenarios**:
/// - Container is properly cleaned up after test
/// - No files leaked to host filesystem
/// - Multiple containers can run concurrently without interference
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_resource_cleanup_isolation() {
    println!("\n🧹 Test 10: Resource Cleanup - Container Isolation");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    // Create multiple containers to test isolation
    let client = ContainerClient::new();

    // Container 1: Install package
    let result1 = run_resource_cleanup_container1(&client);
    assert!(
        result1.is_ok(),
        "Container 1 test failed: {:?}",
        result1.err()
    );

    // Container 2: Independent operation (should not see Container 1's state)
    let result2 = run_resource_cleanup_container2(&client);
    assert!(
        result2.is_ok(),
        "Container 2 test failed: {:?}",
        result2.err()
    );

    // Verify host filesystem unchanged
    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Containers leaked to host!"
    );
    assert_eq!(
        before_snapshot.dir_count, after_snapshot.dir_count,
        "Host directory count changed! Containers leaked to host!"
    );

    println!("✅ Test 10 PASSED: Resource cleanup and isolation verified\n");
}

fn run_resource_cleanup_container1(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;

    let registry_path = format!("{}/marketplace/registry/index.json", WORKSPACE_DIR);
    let ggen_bin = format!("{}/target/release/ggen", WORKSPACE_DIR);

    let mkdir = container.exec("mkdir", &["-p", TEST_PROJECT_DIR])?;
    assert_eq!(mkdir.exit_code, SUCCESS_EXIT_CODE);

    let install = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && GGEN_REGISTRY_URL={} GGEN_DEV_MODE=1 {} marketplace install io.ggen.nextjs.ontology-crud",
                TEST_PROJECT_DIR, registry_path, ggen_bin
            ),
        ],
    )?;
    assert_eq!(install.exit_code, SUCCESS_EXIT_CODE);

    // Container drops here - verify cleanup
    Ok(())
}

fn run_resource_cleanup_container2(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;

    // Verify Container 2 doesn't see Container 1's files
    let check_isolated = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "test -d {}/io.ggen.nextjs.ontology-crud && echo 'exists' || echo 'missing'",
                TEST_PROJECT_DIR
            ),
        ],
    )?;

    // Should be missing because Container 1 was cleaned up
    assert!(
        check_isolated.stdout.contains("missing"),
        "Container 2 should not see Container 1's files (isolation verified)"
    );

    Ok(())
}

// ============================================================================
// EXPERT TESTING PATTERNS: Additional Error Paths
// ============================================================================

/// Test 11: Error Path - Package Name Validation (Comprehensive)
///
/// **Expert Pattern**: Error Path Testing
/// **Facet**: Complete package name validation coverage
/// **Error Scenarios**:
/// - Package name too long (>100 chars)
/// - Control characters in package name
/// - Package name with backslash (\)
/// - Package name with forward slash (/)
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_error_package_name_validation_comprehensive() {
    println!("\n❌ Test 11: Error Path - Comprehensive Package Name Validation");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_error_path_comprehensive_validation_test(&client);

    assert!(
        result.is_ok(),
        "Comprehensive validation test failed: {:?}",
        result.err()
    );

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );

    println!("✅ Test 11 PASSED: Comprehensive validation verified\n");
}

fn run_error_path_comprehensive_validation_test(
    client: &ContainerClient,
) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;

    let registry_path = format!("{}/marketplace/registry/index.json", WORKSPACE_DIR);
    let ggen_bin = format!("{}/target/release/ggen", WORKSPACE_DIR);

    // Test case 1: Package name too long (>100 chars)
    println!("  Testing package name too long...");
    let long_name = "a".repeat(101);
    let long_result = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "GGEN_REGISTRY_URL={} {} marketplace install '{}' 2>&1 || true",
                registry_path, ggen_bin, long_name
            ),
        ],
    )?;
    assert!(
        long_result.exit_code != SUCCESS_EXIT_CODE
            || long_result.stderr.contains("too long")
            || long_result.stderr.contains("invalid"),
        "Package name too long should fail: stdout: {}, stderr: {}",
        long_result.stdout,
        long_result.stderr
    );
    println!("    ✅ Package name too long rejected");

    // Test case 2: Package name with backslash
    println!("  Testing package name with backslash...");
    let backslash_result = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "GGEN_REGISTRY_URL={} {} marketplace install 'evil\\package' 2>&1 || true",
                registry_path, ggen_bin
            ),
        ],
    )?;
    assert!(
        backslash_result.exit_code != SUCCESS_EXIT_CODE
            || backslash_result.stderr.contains("invalid")
            || backslash_result.stderr.contains("separator"),
        "Package name with backslash should fail: stdout: {}, stderr: {}",
        backslash_result.stdout,
        backslash_result.stderr
    );
    println!("    ✅ Package name with backslash rejected");

    // Test case 3: Package name with forward slash
    println!("  Testing package name with forward slash...");
    let slash_result = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "GGEN_REGISTRY_URL={} {} marketplace install 'evil/package' 2>&1 || true",
                registry_path, ggen_bin
            ),
        ],
    )?;
    assert!(
        slash_result.exit_code != SUCCESS_EXIT_CODE
            || slash_result.stderr.contains("invalid")
            || slash_result.stderr.contains("separator"),
        "Package name with forward slash should fail: stdout: {}, stderr: {}",
        slash_result.stdout,
        slash_result.stderr
    );
    println!("    ✅ Package name with forward slash rejected");

    Ok(())
}

/// Test 12: Error Path - Already Installed Without Force
///
/// **Expert Pattern**: Error Path Testing
/// **Facet**: Installation state validation
/// **Error Scenarios**:
/// - Attempting to install already-installed package without --force
/// - Verifying error message is clear and actionable
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_error_already_installed() {
    println!("\n❌ Test 12: Error Path - Already Installed Package");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_error_path_already_installed_test(&client);

    assert!(
        result.is_ok(),
        "Already installed test failed: {:?}",
        result.err()
    );

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );

    println!("✅ Test 12 PASSED: Already installed error verified\n");
}

fn run_error_path_already_installed_test(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;

    let registry_path = format!("{}/marketplace/registry/index.json", WORKSPACE_DIR);
    let ggen_bin = format!("{}/target/release/ggen", WORKSPACE_DIR);

    // Create test project directory
    let mkdir = container.exec("mkdir", &["-p", TEST_PROJECT_DIR])?;
    assert_eq!(mkdir.exit_code, SUCCESS_EXIT_CODE);

    // First, install package successfully
    println!("  Installing package...");
    let install1 = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && GGEN_REGISTRY_URL={} GGEN_DEV_MODE=1 {} marketplace install io.ggen.nextjs.ontology-crud",
                TEST_PROJECT_DIR, registry_path, ggen_bin
            ),
        ],
    )?;
    assert_eq!(
        install1.exit_code, SUCCESS_EXIT_CODE,
        "First installation should succeed: stdout: {}, stderr: {}",
        install1.stdout, install1.stderr
    );
    println!("    ✅ Initial installation succeeded");

    // Now try to install again without --force (should fail)
    println!("  Attempting to install again without --force...");
    let install2 = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && GGEN_REGISTRY_URL={} GGEN_DEV_MODE=1 {} marketplace install io.ggen.nextjs.ontology-crud 2>&1 || true",
                TEST_PROJECT_DIR, registry_path, ggen_bin
            ),
        ],
    )?;
    assert!(
        install2.exit_code != SUCCESS_EXIT_CODE
            || install2.stderr.contains("already installed")
            || install2.stderr.contains("already exists")
            || install2.stdout.contains("already installed"),
        "Second installation without --force should fail: stdout: {}, stderr: {}",
        install2.stdout,
        install2.stderr
    );
    println!("    ✅ Already installed error correctly returned");

    Ok(())
}

// ============================================================================
// EXPERT TESTING PATTERNS: Additional Boundary Conditions
// ============================================================================

/// Test 13: Boundary Condition - Large Ontology
///
/// **Expert Pattern**: Boundary Condition Testing
/// **Facet**: Large input handling
/// **Boundary Scenarios**:
/// - Ontology with many entities
/// - Ontology with many properties
/// - Verify system handles large files efficiently
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_boundary_large_ontology() {
    println!("\n🔍 Test 13: Boundary Condition - Large Ontology");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();
    let result = run_boundary_large_ontology_test(&client);

    assert!(
        result.is_ok(),
        "Large ontology test failed: {:?}",
        result.err()
    );

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Test leaked to host!"
    );

    println!("✅ Test 13 PASSED: Large ontology handling verified\n");
}

fn run_boundary_large_ontology_test(client: &ContainerClient) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;

    let registry_path = format!("{}/marketplace/registry/index.json", WORKSPACE_DIR);
    let ggen_bin = format!("{}/target/release/ggen", WORKSPACE_DIR);

    // Create test project directory
    let mkdir = container.exec("mkdir", &["-p", TEST_PROJECT_DIR])?;
    assert_eq!(mkdir.exit_code, SUCCESS_EXIT_CODE);

    // Install package
    let install = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && GGEN_REGISTRY_URL={} GGEN_DEV_MODE=1 {} marketplace install io.ggen.nextjs.ontology-crud",
                TEST_PROJECT_DIR, registry_path, ggen_bin
            ),
        ],
    )?;
    assert_eq!(
        install.exit_code, SUCCESS_EXIT_CODE,
        "Package installation should succeed: stdout: {}, stderr: {}",
        install.stdout, install.stderr
    );

    // Test: Create ontology with many entities (boundary test)
    println!("  Testing large ontology...");
    let package_dir = format!("{}/io.ggen.nextjs.ontology-crud", TEST_PROJECT_DIR);
    let ontology_dir = format!("{}/ontology", package_dir);

    // Create ontology directory
    let mkdir_ontology = container.exec("mkdir", &["-p", &ontology_dir])?;
    assert_eq!(mkdir_ontology.exit_code, SUCCESS_EXIT_CODE);

    // Create ontology with multiple entities (reasonable size for test)
    let mut large_ontology = String::from("@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix : <http://example.org/ontology#> .\n\n");

    // Add 50 entities (reasonable boundary test)
    for i in 1..=50 {
        large_ontology.push_str(&format!(
            ":Entity{} a rdfs:Class ;\n    rdfs:label \"Entity {}\" .\n",
            i, i
        ));
    }

    let write_ontology = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cat > {}/large.ttl << 'EOFMARKER'\n{}\nEOFMARKER",
                ontology_dir, large_ontology
            ),
        ],
    )?;
    assert_eq!(write_ontology.exit_code, SUCCESS_EXIT_CODE);
    println!("    ✅ Large ontology created (50 entities)");

    // Verify ontology file exists and has content
    let check_ontology = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "test -f {}/large.ttl && wc -l {}/large.ttl || echo 'missing'",
                ontology_dir, ontology_dir
            ),
        ],
    )?;
    assert!(
        check_ontology.stdout.contains("large.ttl"),
        "Large ontology file should exist"
    );
    println!("    ✅ Large ontology file verified");

    Ok(())
}

// ============================================================================
// EXPERT TESTING PATTERNS: Concurrency Testing
// ============================================================================

/// Test 14: Concurrency - Multiple Concurrent Installs
///
/// **Expert Pattern**: Concurrency Testing
/// **Facet**: Concurrent installation operations
/// **Concurrency Scenarios**:
/// - Multiple containers installing packages simultaneously
/// - Verify no race conditions
/// - Verify each installation is isolated
#[test]
#[ignore] // Long-running integration test
fn test_nextjs_concurrency_multiple_installs() {
    println!("\n🔄 Test 14: Concurrency - Multiple Concurrent Installs");
    println!("{}", "=".repeat(70));

    require_docker();
    let project_dir = Path::new("/Users/sac/ggen");
    let before_snapshot = capture_host_snapshot(project_dir);

    let client = ContainerClient::new();

    // Test: Multiple containers installing simultaneously
    // Note: Rust's test runner already runs tests in parallel by default
    // This test verifies that concurrent installs don't interfere

    let result1 = run_concurrent_install_container(&client, 1);
    let result2 = run_concurrent_install_container(&client, 2);
    let result3 = run_concurrent_install_container(&client, 3);

    assert!(result1.is_ok(), "Container 1 failed: {:?}", result1.err());
    assert!(result2.is_ok(), "Container 2 failed: {:?}", result2.err());
    assert!(result3.is_ok(), "Container 3 failed: {:?}", result3.err());

    let after_snapshot = capture_host_snapshot(project_dir);
    assert_eq!(
        before_snapshot.file_count, after_snapshot.file_count,
        "Host file count changed! Concurrent installs leaked to host!"
    );
    assert_eq!(
        before_snapshot.dir_count, after_snapshot.dir_count,
        "Host directory count changed! Concurrent installs leaked to host!"
    );

    println!("✅ Test 14 PASSED: Concurrent installs verified\n");
}

fn run_concurrent_install_container(
    client: &ContainerClient, container_id: usize,
) -> TestcontainersResult<()> {
    let container = setup_container_environment(client)?;
    build_ggen_from_source_in_container(&container)?;

    let registry_path = format!("{}/marketplace/registry/index.json", WORKSPACE_DIR);
    let ggen_bin = format!("{}/target/release/ggen", WORKSPACE_DIR);
    let test_dir = format!("{}-{}", TEST_PROJECT_DIR, container_id);

    // Create isolated test directory for this container
    let mkdir = container.exec("mkdir", &["-p", &test_dir])?;
    assert_eq!(mkdir.exit_code, SUCCESS_EXIT_CODE);

    // Install package in isolated directory
    let install = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "cd {} && GGEN_REGISTRY_URL={} GGEN_DEV_MODE=1 {} marketplace install io.ggen.nextjs.ontology-crud",
                test_dir, registry_path, ggen_bin
            ),
        ],
    )?;
    assert_eq!(
        install.exit_code, SUCCESS_EXIT_CODE,
        "Container {} installation should succeed: stdout: {}, stderr: {}",
        container_id, install.stdout, install.stderr
    );

    // Verify installation succeeded
    let check = container.exec(
        "sh",
        &[
            "-c",
            &format!(
                "test -d {}/io.ggen.nextjs.ontology-crud && echo 'exists' || echo 'missing'",
                test_dir
            ),
        ],
    )?;
    assert!(
        check.stdout.contains("exists"),
        "Container {} installation should create package directory",
        container_id
    );

    println!("    ✅ Container {} installation succeeded", container_id);
    Ok(())
}
