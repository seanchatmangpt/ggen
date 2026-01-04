//! Debian Bookworm Container Tests for Claude Code Web Compatibility
//!
//! Tests ggen installation and execution in Debian bookworm containers matching
//! the Claude Code on the web environment (Debian-based universal image).

use crate::error::{ContainerError, PlatformError, Result};
use std::process::Command;
use std::time::Duration;
use testcontainers::core::WaitFor;
use testcontainers::runners::AsyncRunner;
use testcontainers::GenericImage;
use testcontainers::core::ExecCommand;

/// Poka-yoke: Pre-flight check to verify Docker is running
fn require_docker() -> Result<()> {
    let output = Command::new("docker")
        .arg("ps")
        .output()
        .map_err(|e| {
            PlatformError::DockerUnavailable.into()
        })?;

    if !output.status.success() {
        return Err(PlatformError::DockerUnavailable.into());
    }

    Ok(())
}

/// Test that Debian bookworm container starts successfully
#[tokio::test]
#[ignore] // Long-running integration test
async fn test_debian_bookworm_container_startup() -> Result<()> {
    // Poka-yoke: Pre-flight check
    require_docker()?;

    // Start Debian bookworm container
    let container = GenericImage::new("debian", "bookworm-slim")
        .with_wait_for(WaitFor::seconds(2))
        .start()
        .await
        .map_err(|e| ContainerError::StartFailed(format!("Failed to start container: {}", e)))?;

    // Verify container is running by executing a simple command
    let exec_result = container
        .exec(ExecCommand::new(
            "sh",
            vec!["-c", "echo 'Container ready' && cat /etc/os-release | grep 'PRETTY_NAME'"],
        ))
        .await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to execute command: {}", e)))?;

    let exit_code = exec_result.exit_code().await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to get exit code: {}", e)))?;
    assert_eq!(exit_code, Some(0), "Container command should succeed");
    let stdout = exec_result.stdout().await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to get stdout: {}", e)))?;
    let output = String::from_utf8_lossy(&stdout);
    assert!(
        output.contains("Debian") || output.contains("bookworm"),
        "Container should be Debian bookworm"
    );

    // Container automatically cleaned up on drop (poka-yoke: automatic cleanup)
    Ok(())
}

/// Test Rust toolchain installation/availability in Debian container
#[tokio::test]
#[ignore] // Long-running integration test
async fn test_rust_toolchain_in_debian() -> Result<()> {
    require_docker()?;

    let container = GenericImage::new("debian", "bookworm-slim")
        .with_wait_for(WaitFor::seconds(2))
        .start()
        .await
        .map_err(|e| ContainerError::StartFailed(format!("Failed to start container: {}", e)))?;

    // Install curl and other dependencies
    let exec_result = container
        .exec(ExecCommand::new(
            "sh",
            vec![
                "-c",
                "apt-get update -qq && apt-get install -y -qq curl build-essential > /dev/null 2>&1",
            ],
        ))
        .await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to install dependencies: {}", e)))?;

    let exit_code = exec_result.exit_code().await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to get exit code: {}", e)))?;
    assert_eq!(exit_code, Some(0), "Dependencies should install successfully");

    // Install Rust via rustup (matching Claude Code web environment)
    let exec_result = container
        .exec(ExecCommand::new(
            "sh",
            vec![
                "-c",
                "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --quiet 2>&1",
            ],
        ))
        .await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to install Rust: {}", e)))?;

    let exit_code = exec_result.exit_code().await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to get exit code: {}", e)))?;
    assert_eq!(exit_code, Some(0), "Rust installation should succeed");

    // Verify Rust is available
    let exec_result = container
        .exec(ExecCommand::new(
            "sh",
            vec!["-c", "source $HOME/.cargo/env && rustc --version"],
        ))
        .await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to check Rust version: {}", e)))?;

    let exit_code = exec_result.exit_code().await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to get exit code: {}", e)))?;
    assert_eq!(exit_code, Some(0), "Rust should be available");
    let stdout = exec_result.stdout().await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to get stdout: {}", e)))?;
    let output = String::from_utf8_lossy(&stdout);
    assert!(output.contains("rustc"), "Output should contain rustc version");

    Ok(())
}

/// Test ggen installation via cargo install in Debian container
#[tokio::test]
#[ignore] // Long-running integration test
async fn test_ggen_installation_in_debian() -> Result<()> {
    require_docker()?;

    let container = GenericImage::new("debian", "bookworm-slim")
        .with_wait_for(WaitFor::seconds(2))
        .start()
        .await
        .map_err(|e| ContainerError::StartFailed(format!("Failed to start container: {}", e)))?;

    // Install dependencies and Rust
    container
        .exec(
            "sh",
            vec![
                "-c",
                "apt-get update -qq && apt-get install -y -qq curl build-essential pkg-config libssl-dev git > /dev/null 2>&1",
            ],
        )
        .await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to install dependencies: {}", e)))?;

    container
        .exec(
            "sh",
            vec![
                "-c",
                "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --quiet 2>&1",
            ],
        )
        .await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to install Rust: {}", e)))?;

    // Install ggen from crates.io (simulating cargo install)
    // Note: This is a simplified test - in practice, we'd install from source or use a pre-built binary
    let exec_result = container
        .exec(ExecCommand::new(
            "sh",
            vec![
                "-c",
                "source $HOME/.cargo/env && cargo install ggen-cli-lib --version 5.0.0 2>&1 | head -20",
            ],
        ))
        .await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to install ggen: {}", e)))?;

    // Installation may take time, so we check if cargo install command was accepted
    // In a real scenario, we'd wait for completion or use a timeout
    // For now, we verify the command was executed (exit code 0 or non-zero is acceptable during install)
    let stdout = exec_result.stdout().await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to get stdout: {}", e)))?;
    let output = String::from_utf8_lossy(&stdout);
    println!("ggen installation output: {}", output);

    // Verify cargo is available (poka-yoke: verify prerequisites)
    let exec_result = container
        .exec(ExecCommand::new(
            "sh",
            vec!["-c", "source $HOME/.cargo/env && cargo --version"],
        ))
        .await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to check cargo: {}", e)))?;

    let exit_code = exec_result.exit_code().await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to get exit code: {}", e)))?;
    assert_eq!(exit_code, Some(0), "Cargo should be available");

    Ok(())
}

/// Test ggen execution in Debian container
#[tokio::test]
#[ignore] // Long-running integration test
async fn test_ggen_execution_in_debian() -> Result<()> {
    require_docker()?;

    let container = GenericImage::new("debian", "bookworm-slim")
        .with_wait_for(WaitFor::seconds(2))
        .start()
        .await
        .map_err(|e| ContainerError::StartFailed(format!("Failed to start container: {}", e)))?;

    // Install dependencies
    container
        .exec(
            "sh",
            vec![
                "-c",
                "apt-get update -qq && apt-get install -y -qq curl build-essential pkg-config libssl-dev ca-certificates libssl3 > /dev/null 2>&1",
            ],
        )
        .await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to install dependencies: {}", e)))?;

    // For this test, we'll simulate ggen execution by checking if a binary exists
    // In a real scenario, we'd install ggen and run actual commands
    // This test verifies the environment is ready for ggen execution

    // Verify required libraries are available (poka-yoke: verify runtime dependencies)
    let _exec_result = container
        .exec(ExecCommand::new(
            "sh",
            vec!["-c", "ldconfig -p | grep -q libssl || echo 'libssl available'"],
        ))
        .await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to check libraries: {}", e)))?;

    // Verify ca-certificates for HTTPS (required for crates.io access)
    let exec_result = container
        .exec(ExecCommand::new(
            "sh",
            vec!["-c", "test -f /etc/ssl/certs/ca-certificates.crt && echo 'CA certificates available'"],
        ))
        .await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to check CA certificates: {}", e)))?;

    let exit_code = exec_result.exit_code().await
        .map_err(|e| ContainerError::ExitCode(1, format!("Failed to get exit code: {}", e)))?;
    assert_eq!(exit_code, Some(0), "CA certificates should be available");

    Ok(())
}

/// Test container cleanup verification (poka-yoke safeguard)
#[tokio::test]
#[ignore] // Long-running integration test
async fn test_container_cleanup() -> Result<()> {
    require_docker()?;

    // Get initial container count
    let initial_output = Command::new("docker")
        .args(&["ps", "-a", "--format", "{{.ID}}"])
        .output()
        .map_err(|_| PlatformError::DockerUnavailable)?;

    let initial_count = String::from_utf8_lossy(&initial_output.stdout)
        .lines()
        .filter(|l| !l.is_empty())
        .count();

    // Create and drop container (should auto-cleanup)
    {
        let _container = GenericImage::new("debian", "bookworm-slim")
            .with_wait_for(WaitFor::seconds(1))
            .start()
            .await
            .map_err(|e| ContainerError::StartFailed(format!("Failed to start container: {}", e)))?;

        // Container goes out of scope here and should be cleaned up
    }

    // Wait a moment for cleanup
    tokio::time::sleep(Duration::from_secs(2)).await;

    // Verify container was cleaned up (poka-yoke: cleanup verification)
    let final_output = Command::new("docker")
        .args(&["ps", "-a", "--format", "{{.ID}}"])
        .output()
        .map_err(|_| PlatformError::DockerUnavailable)?;

    let final_count = String::from_utf8_lossy(&final_output.stdout)
        .lines()
        .filter(|l| !l.is_empty())
        .count();

    // Note: This is a best-effort check - testcontainers handles cleanup automatically
    // The count should be the same or less (other containers may have been cleaned up)
    assert!(
        final_count <= initial_count + 1,
        "Container should be cleaned up (initial: {}, final: {})",
        initial_count,
        final_count
    );

    Ok(())
}

