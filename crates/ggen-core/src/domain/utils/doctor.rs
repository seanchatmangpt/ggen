//! System diagnostics - domain layer
//!
//! Pure business logic for system health checks.

use crate::utils::error::Result;
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
    pub recovery: Option<String>,
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
            recovery: Some("ggen marketplace sync".to_string()),
        });
    }

    // Try to open the store (check for corruption/locks)
    match oxigraph::store::Store::open(&db_path) {
        Ok(_) => Ok(CheckResult {
            name: "Marketplace DB".to_string(),
            status: CheckStatus::Ok,
            message: format!("RDF store healthy: {}", db_path.display()),
            recovery: None,
        }),
        Err(e) => Ok(CheckResult {
            name: "Marketplace DB".to_string(),
            status: CheckStatus::Error,
            message: format!("RDF store error (possibly locked or corrupt): {}", e),
            recovery: Some("rm -rf ~/.cache/ggen/packs/marketplace.db && ggen marketplace sync".to_string()),
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
        recovery: None,
    })
}

/// Check Observability Stack health (Anti-Cheating Gate)
async fn check_observability() -> Result<CheckResult> {
    use reqwest::Client;
    use std::time::Duration;

    let client = Client::builder()
        .timeout(Duration::from_millis(500))
        .build()
        .map_err(|e| {
            crate::utils::error::Error::new(&format!("Failed to build HTTP client: {}", e))
        })?;

    // 1. Check Tempo Health (Deep Check)
    let tempo_url = "http://127.0.0.1:3200/ready";
    let tempo_status = client.get(tempo_url).send().await;

    // 2. Check OTel Health Check extension (standard port 13133)
    let otel_url = "http://127.0.0.1:13133/";
    let otel_status = client.get(otel_url).send().await;

    let mut reachable = Vec::new();
    let mut failures = Vec::new();

    match tempo_status {
        Ok(resp) if resp.status().is_success() => {
            reachable.push("Tempo (API)");
        }
        Ok(resp) => {
            failures.push(format!("Tempo returned {}", resp.status()));
        }
        Err(_) => {
            failures.push("Tempo unreachable (port 3200)".to_string());
        }
    }

    match otel_status {
        Ok(resp) if resp.status().is_success() => {
            reachable.push("OTel Collector (Health)");
        }
        Ok(resp) => {
            failures.push(format!("OTel Collector returned {}", resp.status()));
        }
        Err(_) => {
            failures.push("OTel Collector unreachable (port 13133)".to_string());
        }
    }

    // 3. Evidence Check: Fetch recent traces from Tempo (The "Un-fakeable" Proof)
    let mut trace_evidence = false;
    if reachable.contains(&"Tempo (API)") {
        let query_url = "http://127.0.0.1:3200/api/search?limit=1&start=now-10m";
        if let Ok(resp) = client.get(query_url).send().await {
            if let Ok(text) = resp.text().await {
                if text.contains("traceID") {
                    trace_evidence = true;
                }
            }
        }
    }

    if reachable.len() == 2 && trace_evidence {
        Ok(CheckResult {
            name: "Observability Stack".to_string(),
            status: CheckStatus::Ok,
            message: "All services healthy. Recent trace evidence confirmed in Tempo.".to_string(),
            recovery: None,
        })
    } else if reachable.is_empty() {
        Ok(CheckResult {
            name: "Observability Stack".to_string(),
            status: CheckStatus::Warning,
            message: format!(
                "No observability services found (tried: Tempo, OTel). Errors: {}",
                failures.join(", ")
            ),
            recovery: Some("docker compose -f docker-compose.otel.yml up -d".to_string()),
        })
    } else {
        let evidence_msg = if trace_evidence {
            "Traces confirmed."
        } else {
            "NO recent trace evidence found (system might be failing to emit)."
        };
        Ok(CheckResult {
            name: "Observability Stack".to_string(),
            status: CheckStatus::Warning,
            message: format!(
                "Partial health: {}. {}. {}",
                reachable.join(", "),
                failures.join(", "),
                evidence_msg
            ),
            recovery: Some("docker compose -f docker-compose.otel.yml restart".to_string()),
        })
    }
}

/// Check SLO Performance (Vision 2030 Gate)
async fn check_slo() -> Result<CheckResult> {
    use crate::pipeline_engine::vocabulary::VocabularyRegistry;
    use oxigraph::model::*;
    use oxigraph::store::Store;
    use rayon::prelude::*;
    use std::collections::BTreeSet;
    use std::time::{Duration, Instant};

    let total_start = Instant::now();

    // 1. Vocabulary Registry Contention Test
    let vocab_start = Instant::now();
    let registry = VocabularyRegistry::with_standard_vocabularies();
    let mut test_ns = BTreeSet::new();
    test_ns.insert("http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string());
    test_ns.insert("http://www.w3.org/2000/01/rdf-schema#".to_string());
    test_ns.insert("http://www.w3.org/2002/07/owl#".to_string());
    test_ns.insert("http://ggen.dev/v6#".to_string());

    // Stress test: 1000 parallel lookups to measure lock contention
    (0..1000).into_par_iter().for_each(|_| {
        let _ = registry.validate_namespaces(&test_ns);
    });
    let vocab_duration = vocab_start.elapsed();

    // 2. RDF Graph Synthesis Throughput Test (Oxigraph)
    let graph_start = Instant::now();
    let store =
        Store::new().map_err(|e| crate::utils::error::Error::new(&format!("Graph error: {}", e)))?;
    for i in 0..2000 {
        let s = NamedNode::new(format!("http://ggen.io/s{}", i)).unwrap();
        let p = NamedNode::new("http://ggen.io/p").unwrap();
        let o = Literal::from(i);
        store
            .insert(&Quad::new(s, p, o, GraphName::DefaultGraph))
            .unwrap();
    }
    let graph_duration = graph_start.elapsed();

    // 3. Template Rendering Bottleneck Test (Tera)
    let template_start = Instant::now();
    let mut tera = tera::Tera::default();
    let mut context = tera::Context::new();
    context.insert("name", "GGen Doctor");
    context.insert("items", &(0..100).collect::<Vec<i32>>());
    let template = "Hello {{ name }}! Count: {% for i in items %}{{ i }}{% if not loop.last %}, {% endif %}{% endfor %}";
    for _ in 0..500 {
        let _ = tera.render_str(template, &context).unwrap();
    }
    let template_duration = template_start.elapsed();

    let total_duration = total_start.elapsed();

    // SLO Thresholds (Vision 2030 Standards)
    let vocab_limit = Duration::from_millis(50);
    let graph_limit = Duration::from_millis(100);
    let template_limit = Duration::from_millis(150);

    let mut violations = Vec::new();
    if vocab_duration > vocab_limit {
        violations.push(format!("Vocab Registry slow ({:?})", vocab_duration));
    }
    if graph_duration > graph_limit {
        violations.push(format!("Graph Synthesis slow ({:?})", graph_duration));
    }
    if template_duration > template_limit {
        violations.push(format!("Template Rendering slow ({:?})", template_duration));
    }

    if violations.is_empty() {
        Ok(CheckResult {
            name: "SLO Performance".to_string(),
            status: CheckStatus::Ok,
            message: format!(
                "Architectural benchmarks passed. Total: {:?}. (Vocab: {:?}, Graph: {:?}, Template: {:?})",
                total_duration, vocab_duration, graph_duration, template_duration
            ),
            recovery: None,
        })
    } else {
        Ok(CheckResult {
            name: "SLO Performance".to_string(),
            status: CheckStatus::Warning,
            message: format!(
                "SLO Violations: {}. Total: {:?}.",
                violations.join(", "),
                total_duration
            ),
            recovery: Some("cargo build --release; reduce background CPU load".to_string()),
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
                recovery: None,
            })
        }
        _ => Ok(CheckResult {
            name: "Rust".to_string(),
            status: CheckStatus::Error,
            message: "Not installed. Install Rust from https://rustup.rs".to_string(),
            recovery: Some("curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh".to_string()),
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
                recovery: None,
            })
        }
        _ => Ok(CheckResult {
            name: "Cargo".to_string(),
            status: CheckStatus::Error,
            message: "Not installed. Install Rust from https://rustup.rs".to_string(),
            recovery: Some("curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh".to_string()),
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
                recovery: None,
            })
        }
        _ => Ok(CheckResult {
            name: "Git".to_string(),
            status: CheckStatus::Warning,
            message: "Not installed. Optional but recommended".to_string(),
            recovery: Some("brew install git".to_string()),
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
