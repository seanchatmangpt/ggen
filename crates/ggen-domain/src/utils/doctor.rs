//! System diagnostics - domain layer
//!
//! Pure business logic for system health checks.

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

/// System check status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CheckStatus {
    Ok,
    Warning,
    Error,
}

/// System check result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckResult {
    pub name: String,
    pub status: CheckStatus,
    pub message: String,
}

/// Doctor command input (pure domain type)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct DoctorInput {
    /// Show detailed output with fix instructions
    pub verbose: bool,

    /// Run specific check (e.g., "rust", "cargo", "git")
    pub check: Option<String>,

    /// Show environment information
    pub env: bool,
}

/// Doctor command result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DoctorResult {
    pub checks: Vec<CheckResult>,
    pub environment: Option<EnvironmentInfo>,
}

/// Environment information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvironmentInfo {
    pub rust_version: Option<String>,
    pub cargo_version: Option<String>,
    pub git_version: Option<String>,
    pub os: String,
    pub architecture: String,
    pub home_dir: Option<String>,
}

/// Execute doctor checks (pure domain function)
pub async fn execute_doctor(input: DoctorInput) -> Result<DoctorResult> {
    let mut checks = Vec::new();

    // Check Rust
    if input.check.is_none() || input.check.as_deref() == Some("rust") {
        let rust_check = check_rust().await?;
        checks.push(rust_check);
    }

    // Check Cargo
    if input.check.is_none() || input.check.as_deref() == Some("cargo") {
        let cargo_check = check_cargo().await?;
        checks.push(cargo_check);
    }

    // Check Git
    if input.check.is_none() || input.check.as_deref() == Some("git") {
        let git_check = check_git().await?;
        checks.push(git_check);
    }

    // Check Marketplace
    if input.check.is_none() || input.check.as_deref() == Some("marketplace") {
        let marketplace_check = check_marketplace().await?;
        checks.push(marketplace_check);
    }

    // Check User Cache
    if input.check.is_none() || input.check.as_deref() == Some("cache") {
        let cache_check = check_cache().await?;
        checks.push(cache_check);
    }

    // Check Observability Stack (Anti-Cheating Gate)
    if input.check.is_none() || input.check.as_deref() == Some("observability") {
        let observability_check = check_observability().await?;
        checks.push(observability_check);
    }

    // Check SLO Performance (Vision 2030 Gate)
    if input.check.is_none() || input.check.as_deref() == Some("slo") {
        let slo_check = check_slo().await?;
        checks.push(slo_check);
    }

    // Collect environment info if requested
    let environment = if input.env {
        Some(collect_environment().await?)
    } else {
        None
    };

    Ok(DoctorResult {
        checks,
        environment,
    })
}

/// Check Marketplace health
async fn check_marketplace() -> Result<CheckResult> {
    use std::path::PathBuf;

    let cache_dir = if let Some(dir) = dirs::cache_dir() {
        dir.join("ggen").join("packs")
    } else {
        PathBuf::from(".cache").join("ggen").join("packs")
    };

    let db_path = cache_dir.join("marketplace.db");

    if !db_path.exists() {
        return Ok(CheckResult {
            name: "Marketplace DB".to_string(),
            status: CheckStatus::Warning,
            message: "RDF store not found. Run 'ggen marketplace sync' to initialize.".to_string(),
        });
    }

    // Try to open the store (check for corruption/locks)
    match oxigraph::store::Store::open(&db_path) {
        Ok(_) => Ok(CheckResult {
            name: "Marketplace DB".to_string(),
            status: CheckStatus::Ok,
            message: format!("RDF store healthy: {}", db_path.display()),
        }),
        Err(e) => Ok(CheckResult {
            name: "Marketplace DB".to_string(),
            status: CheckStatus::Error,
            message: format!("RDF store error (possibly locked or corrupt): {}", e),
        }),
    }
}

/// Check User Cache health
async fn check_cache() -> Result<CheckResult> {
    let mut pack_count = 0;

    if let Some(home) = dirs::home_dir() {
        let user_packs = home.join(".ggen").join("packs");
        if user_packs.exists() {
            if let Ok(entries) = std::fs::read_dir(user_packs) {
                pack_count = entries
                    .filter_map(|e| e.ok())
                    .filter(|e| e.path().is_dir())
                    .count();
            }
        }
    }

    Ok(CheckResult {
        name: "User Cache".to_string(),
        status: CheckStatus::Ok,
        message: format!(
            "Found {} packs in global user cache (~/.ggen/packs)",
            pack_count
        ),
    })
}

/// Check Observability Stack health (Anti-Cheating Gate)
async fn check_observability() -> Result<CheckResult> {
    use std::net::TcpStream;
    use std::time::Duration;

    // Standard OTel/Tempo/Jaeger ports
    let targets = [
        ("OTel Collector (gRPC)", "127.0.0.1:4317"),
        ("Tempo (HTTP)", "127.0.0.1:3200"),
    ];

    let mut reachable = Vec::new();
    let mut unreachable = Vec::new();

    for (name, addr) in targets {
        if TcpStream::connect_timeout(&addr.parse().unwrap(), Duration::from_millis(100)).is_ok() {
            reachable.push(name);
        } else {
            unreachable.push(name);
        }
    }

    if unreachable.is_empty() {
        Ok(CheckResult {
            name: "Observability Stack".to_string(),
            status: CheckStatus::Ok,
            message: "All observability services are reachable on localhost".to_string(),
        })
    } else if reachable.is_empty() {
        Ok(CheckResult {
            name: "Observability Stack".to_string(),
            status: CheckStatus::Warning,
            message: format!(
                "No local observability services found (tried: {}). Chicago TDD tests will skip boundary validation.",
                unreachable.join(", ")
            ),
        })
    } else {
        Ok(CheckResult {
            name: "Observability Stack".to_string(),
            status: CheckStatus::Warning,
            message: format!(
                "Partial observability: {} reachable, {} unreachable.",
                reachable.join(", "),
                unreachable.join(", ")
            ),
        })
    }
}

/// Check SLO Performance (Vision 2030 Gate)
async fn check_slo() -> Result<CheckResult> {
    use ggen_core::v6::vocabulary::VocabularyRegistry;
    use std::collections::BTreeSet;
    use std::time::Instant;

    let start = Instant::now();

    // Deep SLO Check: Execute a full Vocabulary Validation Cycle
    // This measures the real architectural bottleneck of the v6 pipeline.
    let registry = VocabularyRegistry::with_standard_vocabularies();

    // Test a synthetic set of 10 namespaces to measure resolution latency
    let mut test_ns = BTreeSet::new();
    test_ns.insert("http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string());
    test_ns.insert("http://www.w3.org/2000/01/rdf-schema#".to_string());
    test_ns.insert("http://www.w3.org/2002/07/owl#".to_string());
    test_ns.insert("http://ggen.dev/v6#".to_string());
    test_ns.insert("https://ggen.io/marketplace/".to_string());
    test_ns.insert("http://www.w3.org/2001/XMLSchema#".to_string());
    test_ns.insert("http://www.w3.org/ns/shacl#".to_string());
    test_ns.insert("https://schema.org/".to_string());
    test_ns.insert("http://purl.org/dc/terms/".to_string());
    test_ns.insert("http://xmlns.com/foaf/0.1/".to_string());

    let _ = registry.validate_namespaces(&test_ns);

    let duration = start.elapsed();
    let limit = std::time::Duration::from_millis(10);

    if duration < limit {
        Ok(CheckResult {
            name: "SLO Performance".to_string(),
            status: CheckStatus::Ok,
            message: format!(
                "Vocabulary validation latency: {:?} (< 10ms target)",
                duration
            ),
        })
    } else {
        Ok(CheckResult {
            name: "SLO Performance".to_string(),
            status: CheckStatus::Warning,
            message: format!(
                "Vocabulary validation latency: {:?} exceeds 10ms target (Vision 2030 violation)",
                duration
            ),
        })
    }
}

/// Check Rust installation
async fn check_rust() -> Result<CheckResult> {
    use std::process::Command;

    let output = Command::new("rustc").arg("--version").output();

    match output {
        Ok(output) if output.status.success() => {
            let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
            Ok(CheckResult {
                name: "Rust".to_string(),
                status: CheckStatus::Ok,
                message: format!("Installed: {}", version),
            })
        }
        _ => Ok(CheckResult {
            name: "Rust".to_string(),
            status: CheckStatus::Error,
            message: "Not installed. Install from https://rustup.rs".to_string(),
        }),
    }
}

/// Check Cargo installation
async fn check_cargo() -> Result<CheckResult> {
    use std::process::Command;

    let output = Command::new("cargo").arg("--version").output();

    match output {
        Ok(output) if output.status.success() => {
            let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
            Ok(CheckResult {
                name: "Cargo".to_string(),
                status: CheckStatus::Ok,
                message: format!("Installed: {}", version),
            })
        }
        _ => Ok(CheckResult {
            name: "Cargo".to_string(),
            status: CheckStatus::Error,
            message: "Not installed. Install Rust from https://rustup.rs".to_string(),
        }),
    }
}

/// Check Git installation
async fn check_git() -> Result<CheckResult> {
    use std::process::Command;

    let output = Command::new("git").arg("--version").output();

    match output {
        Ok(output) if output.status.success() => {
            let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
            Ok(CheckResult {
                name: "Git".to_string(),
                status: CheckStatus::Ok,
                message: format!("Installed: {}", version),
            })
        }
        _ => Ok(CheckResult {
            name: "Git".to_string(),
            status: CheckStatus::Warning,
            message: "Not installed. Optional but recommended".to_string(),
        }),
    }
}

/// Collect environment information
async fn collect_environment() -> Result<EnvironmentInfo> {
    use std::process::Command;

    let rust_version = Command::new("rustc")
        .arg("--version")
        .output()
        .ok()
        .and_then(|o| {
            o.status
                .success()
                .then(|| String::from_utf8_lossy(&o.stdout).trim().to_string())
        });

    let cargo_version = Command::new("cargo")
        .arg("--version")
        .output()
        .ok()
        .and_then(|o| {
            o.status
                .success()
                .then(|| String::from_utf8_lossy(&o.stdout).trim().to_string())
        });

    let git_version = Command::new("git")
        .arg("--version")
        .output()
        .ok()
        .and_then(|o| {
            o.status
                .success()
                .then(|| String::from_utf8_lossy(&o.stdout).trim().to_string())
        });

    Ok(EnvironmentInfo {
        rust_version,
        cargo_version,
        git_version,
        os: std::env::consts::OS.to_string(),
        architecture: std::env::consts::ARCH.to_string(),
        home_dir: dirs::home_dir().map(|p| p.to_string_lossy().to_string()),
    })
}
