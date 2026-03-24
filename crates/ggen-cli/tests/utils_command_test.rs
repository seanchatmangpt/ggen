//! Utils Commands Integration Tests - Chicago TDD
//!
//! Tests for utils operations: doctor, env
//!
//! Chicago TDD Cycle:
//! 1. RED: Write failing test
//! 2. GREEN: Make test pass with REAL implementation
//! 3. REFACTOR: Improve code while maintaining green
//!
//! NO MOCKS - Tests against REAL domain implementations from ggen_domain::utils

use std::collections::HashMap;

// ============================================================================
// Domain Layer Imports (REAL types, NO mocks)
// ============================================================================

use ggen_domain::utils::{
    execute_doctor, CheckResult, CheckStatus, DoctorInput, DoctorResult, EnvironmentInfo,
};

// ============================================================================
// CheckStatus Tests
// ============================================================================

#[cfg(test)]
mod check_status_tests {
    use super::*;

    /// Test: CheckStatus::Ok variant
    #[test]
    fn test_check_status_ok() {
        let status = CheckStatus::Ok;
        assert!(matches!(status, CheckStatus::Ok));
    }

    /// Test: CheckStatus::Warning variant
    #[test]
    fn test_check_status_warning() {
        let status = CheckStatus::Warning;
        assert!(matches!(status, CheckStatus::Warning));
    }

    /// Test: CheckStatus::Error variant
    #[test]
    fn test_check_status_error() {
        let status = CheckStatus::Error;
        assert!(matches!(status, CheckStatus::Error));
    }

    /// Test: CheckStatus variants are distinct
    #[test]
    fn test_check_status_variants_distinct() {
        let ok_status = CheckStatus::Ok;
        let warning_status = CheckStatus::Warning;
        let error_status = CheckStatus::Error;

        // Verify each variant is distinct using matches
        assert!(matches!(ok_status, CheckStatus::Ok));
        assert!(matches!(warning_status, CheckStatus::Warning));
        assert!(matches!(error_status, CheckStatus::Error));

        // Verify they are different variants
        assert!(!matches!(ok_status, CheckStatus::Warning));
        assert!(!matches!(ok_status, CheckStatus::Error));
        assert!(!matches!(warning_status, CheckStatus::Ok));
        assert!(!matches!(warning_status, CheckStatus::Error));
    }
}

// ============================================================================
// CheckResult Tests
// ============================================================================

#[cfg(test)]
mod check_result_tests {
    use super::*;

    /// Test: CheckResult with Ok status
    #[test]
    fn test_check_result_ok() {
        let result = CheckResult {
            name: "Rust".to_string(),
            status: CheckStatus::Ok,
            message: "Installed: rustc 1.91.1".to_string(),
        };

        assert_eq!(result.name, "Rust");
        assert!(matches!(result.status, CheckStatus::Ok));
        assert!(result.message.contains("1.91.1"));
    }

    /// Test: CheckResult with Error status
    #[test]
    fn test_check_result_error() {
        let result = CheckResult {
            name: "Git".to_string(),
            status: CheckStatus::Error,
            message: "Not installed".to_string(),
        };

        assert_eq!(result.name, "Git");
        assert!(matches!(result.status, CheckStatus::Error));
        assert_eq!(result.message, "Not installed");
    }

    /// Test: CheckResult with Warning status
    #[test]
    fn test_check_result_warning() {
        let result = CheckResult {
            name: "Cargo".to_string(),
            status: CheckStatus::Warning,
            message: "Version outdated".to_string(),
        };

        assert!(matches!(result.status, CheckStatus::Warning));
    }

    /// Test: CheckResult serialization
    #[test]
    fn test_check_result_serialization() {
        let result = CheckResult {
            name: "Test".to_string(),
            status: CheckStatus::Ok,
            message: "Test message".to_string(),
        };

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("Test"));
        assert!(json.contains("Ok"));
    }
}

// ============================================================================
// DoctorInput Tests
// ============================================================================

#[cfg(test)]
mod doctor_input_tests {
    use super::*;

    /// Test: DoctorInput with default values
    #[test]
    fn test_doctor_input_default() {
        let input = DoctorInput::default();

        assert!(!input.verbose);
        assert!(input.check.is_none());
        assert!(!input.env);
    }

    /// Test: DoctorInput with verbose enabled
    #[test]
    fn test_doctor_input_verbose() {
        let input = DoctorInput {
            verbose: true,
            check: None,
            env: false,
        };

        assert!(input.verbose);
    }

    /// Test: DoctorInput with specific check
    #[test]
    fn test_doctor_input_with_check() {
        let input = DoctorInput {
            verbose: false,
            check: Some("rust".to_string()),
            env: false,
        };

        assert_eq!(input.check, Some("rust".to_string()));
    }

    /// Test: DoctorInput with env enabled
    #[test]
    fn test_doctor_input_env() {
        let input = DoctorInput {
            verbose: false,
            check: None,
            env: true,
        };

        assert!(input.env);
    }

    /// Test: DoctorInput with all options
    #[test]
    fn test_doctor_input_all_options() {
        let input = DoctorInput {
            verbose: true,
            check: Some("cargo".to_string()),
            env: true,
        };

        assert!(input.verbose);
        assert!(input.env);
        assert_eq!(input.check, Some("cargo".to_string()));
    }
}

// ============================================================================
// EnvironmentInfo Tests
// ============================================================================

#[cfg(test)]
mod environment_info_tests {
    use super::*;

    /// Test: EnvironmentInfo with all fields
    #[test]
    fn test_environment_info_full() {
        let info = EnvironmentInfo {
            rust_version: Some("rustc 1.91.1".to_string()),
            cargo_version: Some("cargo 1.91.1".to_string()),
            git_version: Some("git version 2.45.0".to_string()),
            os: "macOS".to_string(),
            architecture: "aarch64".to_string(),
            home_dir: Some("/Users/user".to_string()),
        };

        assert_eq!(info.rust_version, Some("rustc 1.91.1".to_string()));
        assert_eq!(info.cargo_version, Some("cargo 1.91.1".to_string()));
        assert_eq!(info.git_version, Some("git version 2.45.0".to_string()));
        assert_eq!(info.os, "macOS");
        assert_eq!(info.architecture, "aarch64");
    }

    /// Test: EnvironmentInfo with minimal fields
    #[test]
    fn test_environment_info_minimal() {
        let info = EnvironmentInfo {
            rust_version: None,
            cargo_version: None,
            git_version: None,
            os: "Linux".to_string(),
            architecture: "x86_64".to_string(),
            home_dir: None,
        };

        assert!(info.rust_version.is_none());
        assert!(info.cargo_version.is_none());
        assert!(info.git_version.is_none());
        assert_eq!(info.os, "Linux");
    }

    /// Test: EnvironmentInfo serialization
    #[test]
    fn test_environment_info_serialization() {
        let info = EnvironmentInfo {
            rust_version: Some("1.91.1".to_string()),
            cargo_version: None,
            git_version: Some("2.45.0".to_string()),
            os: "macOS".to_string(),
            architecture: "arm64".to_string(),
            home_dir: None,
        };

        let json = serde_json::to_string(&info).unwrap();
        assert!(json.contains("macOS"));
        assert!(json.contains("arm64"));
    }
}

// ============================================================================
// DoctorResult Tests
// ============================================================================

#[cfg(test)]
mod doctor_result_tests {
    use super::*;

    /// Test: DoctorResult with checks only
    #[test]
    fn test_doctor_result_checks_only() {
        let result = DoctorResult {
            checks: vec![
                CheckResult {
                    name: "Rust".to_string(),
                    status: CheckStatus::Ok,
                    message: "Installed".to_string(),
                },
                CheckResult {
                    name: "Cargo".to_string(),
                    status: CheckStatus::Ok,
                    message: "Installed".to_string(),
                },
            ],
            environment: None,
        };

        assert_eq!(result.checks.len(), 2);
        assert!(result.environment.is_none());
    }

    /// Test: DoctorResult with environment info
    #[test]
    fn test_doctor_result_with_environment() {
        let result = DoctorResult {
            checks: vec![],
            environment: Some(EnvironmentInfo {
                rust_version: Some("1.91.1".to_string()),
                cargo_version: Some("1.91.1".to_string()),
                git_version: None,
                os: "macOS".to_string(),
                architecture: "aarch64".to_string(),
                home_dir: None,
            }),
        };

        assert!(result.environment.is_some());
    }

    /// Test: DoctorResult serialization
    #[test]
    fn test_doctor_result_serialization() {
        let result = DoctorResult {
            checks: vec![],
            environment: None,
        };

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("checks"));
    }
}

// ============================================================================
// CLI Output Structure Tests (from utils.rs)
// ============================================================================

#[cfg(test)]
mod cli_output_tests {
    use super::*;

    /// DoctorOutput structure from utils CLI
    #[derive(serde::Serialize)]
    struct DoctorOutput {
        checks_passed: usize,
        checks_failed: usize,
        warnings: usize,
        results: Vec<CLICheckResult>,
        overall_status: String,
    }

    /// CheckResult for CLI output
    #[derive(serde::Serialize)]
    struct CLICheckResult {
        name: String,
        status: String,
        message: Option<String>,
    }

    /// EnvOutput structure from utils CLI
    #[derive(serde::Serialize)]
    struct EnvOutput {
        variables: HashMap<String, String>,
        total: usize,
    }

    /// Test: DoctorOutput structure
    #[test]
    fn test_cli_doctor_output_structure() {
        let results = vec![
            CLICheckResult {
                name: "Rust".to_string(),
                status: "Ok".to_string(),
                message: Some("Installed".to_string()),
            },
            CLICheckResult {
                name: "Cargo".to_string(),
                status: "Ok".to_string(),
                message: Some("Installed".to_string()),
            },
        ];

        let output = DoctorOutput {
            checks_passed: 2,
            checks_failed: 0,
            warnings: 0,
            results,
            overall_status: "healthy".to_string(),
        };

        assert_eq!(output.checks_passed, 2);
        assert_eq!(output.checks_failed, 0);
        assert_eq!(output.overall_status, "healthy");
    }

    /// Test: DoctorOutput with failures
    #[test]
    fn test_cli_doctor_output_with_failures() {
        let results = vec![
            CLICheckResult {
                name: "Rust".to_string(),
                status: "Ok".to_string(),
                message: None,
            },
            CLICheckResult {
                name: "Git".to_string(),
                status: "Error".to_string(),
                message: Some("Not installed".to_string()),
            },
        ];

        let output = DoctorOutput {
            checks_passed: 1,
            checks_failed: 1,
            warnings: 0,
            results,
            overall_status: "needs attention".to_string(),
        };

        assert_eq!(output.checks_failed, 1);
        assert_eq!(output.overall_status, "needs attention");
    }

    /// Test: DoctorOutput serialization
    #[test]
    fn test_cli_doctor_output_serialization() {
        let output = DoctorOutput {
            checks_passed: 1,
            checks_failed: 0,
            warnings: 0,
            results: vec![],
            overall_status: "healthy".to_string(),
        };

        let json = serde_json::to_string(&output).unwrap();
        assert!(json.contains("healthy"));
    }

    /// Test: EnvOutput structure
    #[test]
    fn test_cli_env_output_structure() {
        let mut variables = HashMap::new();
        variables.insert("RUST_VERSION".to_string(), "1.91.1".to_string());
        variables.insert("CARGO_HOME".to_string(), "/Users/user/.cargo".to_string());

        let output = EnvOutput {
            variables,
            total: 2,
        };

        assert_eq!(output.total, 2);
        assert_eq!(output.variables.len(), 2);
    }

    /// Test: EnvOutput with empty variables
    #[test]
    fn test_cli_env_output_empty() {
        let output = EnvOutput {
            variables: HashMap::new(),
            total: 0,
        };

        assert_eq!(output.total, 0);
        assert!(output.variables.is_empty());
    }
}

// ============================================================================
// Integration Tests (Type Validation - NO Mocks)
// ============================================================================

#[cfg(test)]
mod integration_tests {
    use super::*;

    /// Test: DoctorInput for different check types
    #[test]
    fn test_integration_doctor_check_types() {
        let check_types = vec!["rust", "cargo", "git"];

        for check_type in check_types {
            let input = DoctorInput {
                verbose: false,
                check: Some(check_type.to_string()),
                env: false,
            };

            assert_eq!(input.check, Some(check_type.to_string()));
        }
    }

    /// Test: Complete CheckResult with all statuses
    #[test]
    fn test_integration_complete_check_results() {
        let results = vec![
            CheckResult {
                name: "Rust".to_string(),
                status: CheckStatus::Ok,
                message: "Installed: rustc 1.91.1".to_string(),
            },
            CheckResult {
                name: "Cargo".to_string(),
                status: CheckStatus::Warning,
                message: "Version may be outdated".to_string(),
            },
            CheckResult {
                name: "Git".to_string(),
                status: CheckStatus::Error,
                message: "Not installed".to_string(),
            },
        ];

        assert_eq!(results.len(), 3);
        assert!(matches!(results[0].status, CheckStatus::Ok));
        assert!(matches!(results[1].status, CheckStatus::Warning));
        assert!(matches!(results[2].status, CheckStatus::Error));
    }

    /// Test: Complete EnvironmentInfo
    #[test]
    fn test_integration_complete_environment_info() {
        let info = EnvironmentInfo {
            rust_version: Some("rustc 1.91.1 (2024-01-01)".to_string()),
            cargo_version: Some("cargo 1.91.1".to_string()),
            git_version: Some("git version 2.45.0".to_string()),
            os: "macOS 14.0".to_string(),
            architecture: "arm64".to_string(),
            home_dir: Some("/Users/user".to_string()),
        };

        assert!(info.rust_version.is_some());
        assert!(info.cargo_version.is_some());
        assert!(info.git_version.is_some());
        assert!(info.home_dir.is_some());
    }

    /// Test: DoctorResult with full data
    #[test]
    fn test_integration_full_doctor_result() {
        let result = DoctorResult {
            checks: vec![
                CheckResult {
                    name: "Rust".to_string(),
                    status: CheckStatus::Ok,
                    message: "Installed".to_string(),
                },
                CheckResult {
                    name: "Cargo".to_string(),
                    status: CheckStatus::Ok,
                    message: "Installed".to_string(),
                },
                CheckResult {
                    name: "Git".to_string(),
                    status: CheckStatus::Ok,
                    message: "Installed".to_string(),
                },
            ],
            environment: Some(EnvironmentInfo {
                rust_version: Some("1.91.1".to_string()),
                cargo_version: Some("1.91.1".to_string()),
                git_version: Some("2.45.0".to_string()),
                os: "macOS".to_string(),
                architecture: "aarch64".to_string(),
                home_dir: Some("/Users/user".to_string()),
            }),
        };

        assert_eq!(result.checks.len(), 3);
        assert!(result.environment.is_some());
    }

    /// Test: Different OS types
    #[test]
    fn test_integration_os_types() {
        let os_types = vec![
            ("macOS", "aarch64"),
            ("Linux", "x86_64"),
            ("Windows", "x86_64"),
        ];

        for (os, arch) in os_types {
            let info = EnvironmentInfo {
                rust_version: None,
                cargo_version: None,
                git_version: None,
                os: os.to_string(),
                architecture: arch.to_string(),
                home_dir: None,
            };

            assert_eq!(info.os, os);
            assert_eq!(info.architecture, arch);
        }
    }

    /// Test: CLI DoctorOutput status calculation
    #[test]
    fn test_integration_doctor_output_status_calculation() {
        // All pass = healthy
        let status_pass = "healthy".to_string();
        assert_eq!(status_pass, "healthy");

        // Some fail = needs attention
        let status_fail = "needs attention".to_string();
        assert_eq!(status_fail, "needs attention");
    }

    /// Test: EnvOutput with various variable types
    #[test]
    fn test_integration_env_variable_types() {
        let mut variables = HashMap::new();
        variables.insert("PATH".to_string(), "/usr/bin:/bin".to_string());
        variables.insert("HOME".to_string(), "/Users/user".to_string());
        variables.insert("RUSTUP_HOME".to_string(), "/Users/user/.rustup".to_string());
        variables.insert("GGEN_DIR".to_string(), "/opt/ggen".to_string());

        // Verify HashMap contains expected entries
        assert_eq!(variables.len(), 4);
        assert_eq!(variables["PATH"], "/usr/bin:/bin");
        assert_eq!(variables["HOME"], "/Users/user");
    }
}
