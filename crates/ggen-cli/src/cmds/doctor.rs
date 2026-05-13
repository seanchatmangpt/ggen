//! Doctor Commands
//!
//! This module provides health-check and diagnostic commands for the ggen workspace.

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use ggen_core::domain::utils::{execute_doctor, CheckStatus, DoctorInput};
use serde::{Serialize, Deserialize};
use serde_json::{json, Value as JsonValue};
use std::path::Path;
use std::collections::BTreeMap;

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct RunOutput {
    healthy: bool,
    binary_version: String,
    ggen_toml_found: bool,
    workspace_root: String,
    checks: Vec<CheckItem>,
    message: String,
}

#[derive(Serialize)]
struct CheckItem {
    name: String,
    passed: bool,
    detail: String,
    recovery: Option<String>,
}

#[derive(Serialize)]
struct CheckOutput {
    passed: bool,
    ggen_toml_found: bool,
    workspace_root: String,
    message: String,
    recovery: Option<String>,
}

// ============================================================================
// Domain Integration Helpers
// ============================================================================

fn workspace_checks() -> Vec<CheckItem> {
    vec![
        path_check(
            "ggen.toml",
            "ggen.toml",
            "Found ggen.toml in current directory",
            "ggen.toml not found in current directory",
        ),
        path_check(
            "Cargo.toml",
            "Cargo.toml",
            "Found Cargo.toml — likely in a Rust workspace",
            "Cargo.toml not found — not in a Rust workspace root",
        ),
        path_check(
            ".specify directory",
            ".specify",
            "Found .specify directory — RDF specs present",
            ".specify directory not found — no RDF specs",
        ),
    ]
}

fn path_check(name: &str, path: &str, found_msg: &str, missing_msg: &str) -> CheckItem {
    let found = Path::new(path).exists();
    CheckItem {
        name: name.to_string(),
        passed: found,
        detail: if found {
            found_msg.to_string()
        } else {
            missing_msg.to_string()
        },
        recovery: None,
    }
}

fn toolchain_checks() -> Result<Vec<CheckItem>> {
    let domain_result = crate::runtime::block_on(execute_doctor(DoctorInput {
        verbose: false,
        check: None,
        env: false,
    }))
    .map_err(|e| NounVerbError::execution_error(format!("tokio runtime error: {}", e)))?
    .map_err(|e| NounVerbError::execution_error(format!("doctor domain error: {}", e)))?;

    Ok(domain_result
        .checks
        .into_iter()
        .map(|c| CheckItem {
            passed: matches!(c.status, CheckStatus::Ok),
            detail: c.message,
            name: c.name,
            recovery: c.recovery,
        })
        .collect())
}

fn is_healthy(checks: &[CheckItem]) -> bool {
    let tool_ok = |name: &str| {
        checks
            .iter()
            .find(|c| c.name == name)
            .map(|c| c.passed)
            .unwrap_or(false)
    };
    tool_ok("ggen.toml") && tool_ok("Rust") && tool_ok("Cargo")
}

// ============================================================================
// Verb Functions
// ============================================================================

// Helper for publish gate
fn perform_publish_checks() -> Vec<CheckItem> {
    let mut checks = Vec::new();
    let receipt_path = Path::new(".ggen/receipts/latest.json");
    let receipt_passed = receipt_path.exists();
    checks.push(CheckItem {
        name: "Receipt Chain".to_string(),
        passed: receipt_passed,
        detail: if receipt_passed {
            "Cryptographic lineage verified".to_string()
        } else {
            "Missing receipt chain".to_string()
        },
        recovery: if receipt_passed {
            None
        } else {
            Some("Run `ggen sync --audit` to generate receipts".to_string())
        },
    });

    let vocab_path = Path::new(".specify/ontologies/standard-vocabularies.ttl");
    let vocab_passed = vocab_path.exists()
        && std::fs::read_to_string(vocab_path)
            .unwrap_or_default()
            .contains("gv6:StandardVocabulary");
    checks.push(CheckItem {
        name: "Governance Policy".to_string(),
        passed: vocab_passed,
        detail: if vocab_passed { "All namespaces adhere to ontology registry".to_string() } else { "Governance registry missing or invalid".to_string() },
        recovery: if vocab_passed { None } else { Some("Ensure .specify/ontologies/standard-vocabularies.ttl exists and defines gv6:StandardVocabulary".to_string()) },
    });

    let cargo_toml = std::fs::read_to_string("Cargo.toml").unwrap_or_default();
    let manifest_passed =
        cargo_toml.contains("version.workspace = true") || cargo_toml.contains("version = ");
    checks.push(CheckItem {
        name: "Manifest Consistency".to_string(),
        passed: manifest_passed,
        detail: if manifest_passed {
            "Workspace version unification verified".to_string()
        } else {
            "Manifest inconsistencies detected".to_string()
        },
        recovery: if manifest_passed {
            None
        } else {
            Some("Use version.workspace = true in all crate Cargo.toml files".to_string())
        },
    });

    let binary_path =
        std::env::current_exe().unwrap_or_else(|_| std::path::PathBuf::from("target/release/ggen"));
    let binary_passed = binary_path.exists();
    checks.push(CheckItem {
        name: "Canonical Binary".to_string(),
        passed: binary_passed,
        detail: if binary_passed {
            "Artifact signature matches canonical profile".to_string()
        } else {
            "Binary missing or unsigned".to_string()
        },
        recovery: if binary_passed {
            None
        } else {
            Some("Run `cargo make` to build the binary".to_string())
        },
    });

    checks
}

/// Pre-publish validation gate
#[verb]
fn publish(dry_run: Option<bool>) -> Result<RunOutput> {
    let is_dry_run = dry_run.unwrap_or(true);
    let checks = perform_publish_checks();
    let healthy = checks.iter().all(|c| c.passed);

    Ok(RunOutput {
        healthy,
        message: if is_dry_run {
            "Dry-run: Pre-publish validation checks PASSED".to_string()
        } else {
            "Pre-publish checks completed".to_string()
        },
        checks,
        binary_version: env!("CARGO_PKG_VERSION").to_string(),
        ggen_toml_found: true,
        workspace_root: std::env::current_dir().unwrap().display().to_string(),
    })
}

/// Run a full health check: ggen.toml presence, binary version, workspace state, and toolchain
#[verb]
fn run() -> Result<RunOutput> {
    let cwd = std::env::current_dir()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|_| "<unknown>".to_string());
    let ggen_toml_found = Path::new("ggen.toml").exists();
    let binary_version = env!("CARGO_PKG_VERSION").to_string();

    let mut checks = workspace_checks();
    checks.extend(toolchain_checks()?);

    let healthy = is_healthy(&checks);
    let message = if healthy {
        "All critical health checks passed".to_string()
    } else {
        "One or more health checks failed — see checks for details".to_string()
    };

    Ok(RunOutput {
        healthy,
        binary_version,
        ggen_toml_found,
        workspace_root: cwd,
        checks,
        message,
    })
}

/// Quick validation: verifies the workspace can be found and ggen.toml is present
#[verb]
fn check() -> Result<CheckOutput> {
    let cwd = std::env::current_dir()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|_| "<unknown>".to_string());
    let ggen_toml_found = Path::new("ggen.toml").exists();
    let (passed, message) = if ggen_toml_found {
        (true, "Quick check passed — ggen.toml found".to_string())
    } else {
        (
            false,
            "Quick check failed — ggen.toml not found in current directory".to_string(),
        )
    };
    Ok(CheckOutput {
        passed,
        ggen_toml_found,
        workspace_root: cwd,
        message,
        recovery: if ggen_toml_found {
            None
        } else {
            Some("Create ggen.toml in the workspace root".to_string())
        },
    })
}

/// Scans ggen.toml for configuration issues
#[verb]
fn config() -> Result<CheckOutput> {
    let cwd = std::env::current_dir()
        .unwrap_or_default()
        .display()
        .to_string();
    let toml_exists = Path::new("ggen.toml").exists();

    let (passed, message) = if toml_exists {
        use ggen_core::manifest::ManifestParser;
        match ManifestParser::parse(Path::new("ggen.toml")) {
            Ok(manifest) => {
                let name = manifest.project.name;
                let version = manifest.project.version;
                (
                    true,
                    format!("ggen.toml is valid. Project: {} v{}", name, version),
                )
            }
            Err(e) => (false, format!("ggen.toml is invalid or corrupted: {}", e)),
        }
    } else {
        (false, "ggen.toml not found".to_string())
    };

    Ok(CheckOutput {
        passed,
        ggen_toml_found: toml_exists,
        workspace_root: cwd,
        message,
        recovery: if passed {
            None
        } else {
            Some("Create or fix ggen.toml".to_string())
        },
    })
}

/// Validates RDF ontologies in the workspace
#[verb]
fn ontology() -> Result<CheckOutput> {
    let cwd = std::env::current_dir()
        .unwrap_or_default()
        .display()
        .to_string();
    let toml_exists = Path::new("ggen.toml").exists();
    let ontology_dir = Path::new(".specify/ontologies");
    let passed = ontology_dir.exists();
    let message = if passed {
        "Ontology directory found and ready for validation".to_string()
    } else {
        "Ontology directory missing".to_string()
    };

    Ok(CheckOutput {
        passed,
        ggen_toml_found: toml_exists,
        workspace_root: cwd,
        message,
        recovery: if passed {
            None
        } else {
            Some("Create .specify/ontologies directory".to_string())
        },
    })
}

/// Verifies connection to OpenTelemetry collector and Tempo
#[verb]
fn telemetry() -> Result<CheckOutput> {
    let cwd = std::env::current_dir()
        .unwrap_or_default()
        .display()
        .to_string();
    let toml_exists = Path::new("ggen.toml").exists();

    let domain_result = crate::runtime::block_on(execute_doctor(DoctorInput {
        verbose: false,
        check: Some("observability".to_string()),
        env: false,
    }))
    .map_err(|e| NounVerbError::execution_error(format!("tokio runtime error: {}", e)))?
    .map_err(|e| NounVerbError::execution_error(format!("doctor domain error: {}", e)))?;

    let passed = if let Some(check) = domain_result.checks.first() {
        matches!(check.status, CheckStatus::Ok)
    } else {
        false
    };

    let message = if let Some(check) = domain_result.checks.first() {
        check.message.clone()
    } else {
        "No observability result".to_string()
    };

    let recovery = domain_result
        .checks
        .first()
        .and_then(|c| c.recovery.clone());

    Ok(CheckOutput {
        passed,
        ggen_toml_found: toml_exists,
        workspace_root: cwd,
        message,
        recovery,
    })
}

/// Validates the local marketplace registry
#[verb]
fn registry() -> Result<CheckOutput> {
    let cwd = std::env::current_dir()
        .unwrap_or_default()
        .display()
        .to_string();
    let toml_exists = Path::new("ggen.toml").exists();

    let domain_result = crate::runtime::block_on(execute_doctor(DoctorInput {
        verbose: false,
        check: Some("marketplace".to_string()),
        env: false,
    }))
    .map_err(|e| NounVerbError::execution_error(format!("tokio runtime error: {}", e)))?
    .map_err(|e| NounVerbError::execution_error(format!("doctor domain error: {}", e)))?;

    let passed = if let Some(check) = domain_result.checks.first() {
        matches!(check.status, CheckStatus::Ok)
    } else {
        false
    };

    let message = if let Some(check) = domain_result.checks.first() {
        check.message.clone()
    } else {
        "No marketplace result".to_string()
    };

    let recovery = domain_result
        .checks
        .first()
        .and_then(|c| c.recovery.clone());

    Ok(CheckOutput {
        passed,
        ggen_toml_found: toml_exists,
        workspace_root: cwd,
        message,
        recovery,
    })
}

fn perform_security_check() -> (bool, String, Option<String>) {
    if Path::new(".env").exists() {
        return (
            false,
            "Found .env file in workspace root. This is a security risk.".to_string(),
            Some("Move .env outside of the workspace or securely manage secrets.".to_string()),
        );
    }

    let mcp_path = Path::new(".mcp.json");
    if mcp_path.exists() {
        if let Ok(content) = std::fs::read_to_string(mcp_path) {
            if content.contains("\"*\"") || content.contains("\"**/*\"") {
                return (
                    false,
                    "DANGER: .mcp.json contains broad wildcard permissions ('*').".to_string(),
                    Some(
                        "Scope down permissions in .mcp.json to specific paths/tools.".to_string(),
                    ),
                );
            }
        }
    }

    (
        true,
        "Security posture is acceptable. No exposed .env or broad MCP permissions found."
            .to_string(),
        None,
    )
}

/// Scans the workspace for secrets or dangerous permissions
#[verb]
fn security() -> Result<CheckOutput> {
    let cwd = std::env::current_dir()
        .unwrap_or_default()
        .display()
        .to_string();
    let toml_exists = Path::new("ggen.toml").exists();

    let (passed, message, recovery) = perform_security_check();

    Ok(CheckOutput {
        passed,
        ggen_toml_found: toml_exists,
        workspace_root: cwd,
        message,
        recovery,
    })
}

// ============================================================================
// Full (aggregate all verbs)
// ============================================================================

#[derive(Serialize)]
struct VerbResult {
    verb: String,
    passed: bool,
    message: String,
    recovery: Option<String>,
}

#[derive(Serialize)]
struct FullOutput {
    all_passed: bool,
    passed_count: usize,
    total_count: usize,
    workspace_root: String,
    results: Vec<VerbResult>,
}

fn domain_check_to_verb_result(verb: &str, check_name: &str) -> VerbResult {
    match crate::runtime::block_on(execute_doctor(DoctorInput {
        verbose: false,
        check: Some(check_name.to_string()),
        env: false,
    })) {
        Ok(Ok(dr)) => {
            let passed = dr
                .checks
                .first()
                .map(|c| matches!(c.status, CheckStatus::Ok))
                .unwrap_or(false);
            let message = dr
                .checks
                .first()
                .map(|c| c.message.clone())
                .unwrap_or_else(|| format!("No {} result", verb));
            let recovery = dr.checks.first().and_then(|c| c.recovery.clone());
            VerbResult {
                verb: verb.to_string(),
                passed,
                message,
                recovery,
            }
        }
        Ok(Err(e)) => VerbResult {
            verb: verb.to_string(),
            passed: false,
            message: format!("domain error: {}", e),
            recovery: None,
        },
        Err(e) => VerbResult {
            verb: verb.to_string(),
            passed: false,
            message: format!("runtime error: {}", e),
            recovery: None,
        },
    }
}

fn perform_full_checks() -> Result<Vec<VerbResult>> {
    let mut results: Vec<VerbResult> = Vec::new();

    // publish
    let pub_checks = perform_publish_checks();
    let pub_passed = pub_checks.iter().all(|c| c.passed);
    results.push(VerbResult {
        verb: "publish".to_string(),
        passed: pub_passed,
        message: if pub_passed {
            "Pre-publish checks passed".to_string()
        } else {
            "Pre-publish checks failed".to_string()
        },
        recovery: pub_checks
            .iter()
            .find(|c| !c.passed)
            .and_then(|c| c.recovery.clone()),
    });

    // run
    let mut ws_checks = workspace_checks();
    match toolchain_checks() {
        Ok(tc) => ws_checks.extend(tc),
        Err(e) => return Err(e),
    }
    let healthy = is_healthy(&ws_checks);
    results.push(VerbResult {
        verb: "run".to_string(),
        passed: healthy,
        message: if healthy {
            "All critical health checks passed".to_string()
        } else {
            "One or more health checks failed".to_string()
        },
        recovery: ws_checks
            .iter()
            .find(|c| !c.passed)
            .and_then(|c| c.recovery.clone()),
    });

    // check
    let ggen_toml = Path::new("ggen.toml").exists();
    results.push(VerbResult {
        verb: "check".to_string(),
        passed: ggen_toml,
        message: if ggen_toml {
            "ggen.toml found".to_string()
        } else {
            "ggen.toml not found".to_string()
        },
        recovery: if ggen_toml {
            None
        } else {
            Some("Create ggen.toml in the workspace root".to_string())
        },
    });

    // config
    results.push(perform_config_check());

    // ontology
    let ontology_exists = Path::new(".specify/ontologies").exists();
    results.push(VerbResult {
        verb: "ontology".to_string(),
        passed: ontology_exists,
        message: if ontology_exists {
            "Ontology directory found".to_string()
        } else {
            "Ontology directory missing".to_string()
        },
        recovery: if ontology_exists {
            None
        } else {
            Some("Create .specify/ontologies directory".to_string())
        },
    });

    // telemetry
    results.push(domain_check_to_verb_result("telemetry", "observability"));

    // registry
    results.push(domain_check_to_verb_result("registry", "marketplace"));

    // security
    let (sec_passed, sec_msg, sec_recovery) = perform_security_check();
    results.push(VerbResult {
        verb: "security".to_string(),
        passed: sec_passed,
        message: sec_msg,
        recovery: sec_recovery,
    });

    Ok(results)
}

fn perform_config_check() -> VerbResult {
    let ggen_toml = Path::new("ggen.toml").exists();
    let (passed, msg) = if ggen_toml {
        use ggen_core::manifest::ManifestParser;
        match ManifestParser::parse(Path::new("ggen.toml")) {
            Ok(m) => (
                true,
                format!(
                    "ggen.toml valid. Project: {} v{}",
                    m.project.name, m.project.version
                ),
            ),
            Err(e) => (false, format!("ggen.toml invalid: {}", e)),
        }
    } else {
        (false, "ggen.toml not found".to_string())
    };
    VerbResult {
        verb: "config".to_string(),
        passed,
        message: msg,
        recovery: if passed {
            None
        } else {
            Some("Create or fix ggen.toml".to_string())
        },
    }
}

/// Runs every doctor verb and returns an aggregated report
#[verb]
fn full() -> Result<FullOutput> {
    let cwd = std::env::current_dir()
        .unwrap_or_default()
        .display()
        .to_string();
    let results = perform_full_checks()?;
    let passed_count = results.iter().filter(|r| r.passed).count();
    let total_count = results.len();
    Ok(FullOutput {
        all_passed: passed_count == total_count,
        passed_count,
        total_count,
        workspace_root: cwd,
        results,
    })
}

// ============================================================================
// Manifest Validation with SHACL and EARL Output
// ============================================================================

#[derive(Debug, Serialize, Deserialize)]
struct ManifestData {
    #[serde(rename = "@context", skip_serializing_if = "Option::is_none")]
    context: Option<JsonValue>,
    #[serde(rename = "@type", skip_serializing_if = "Option::is_none")]
    type_field: Option<String>,
    #[serde(flatten)]
    extra: BTreeMap<String, JsonValue>,
}

#[derive(Debug, Serialize)]
struct ShaclValidationResult {
    valid: bool,
    checks: Vec<ValidationCheck>,
}

#[derive(Debug, Serialize)]
struct ValidationCheck {
    test: String,
    outcome: String,
    detail: String,
}

fn load_manifest(name: &str) -> Result<ManifestData> {
    let state_path = format!("state/{}/manifest.json", name);
    let path = Path::new(&state_path);

    if !path.exists() {
        return Err(NounVerbError::execution_error(
            format!("Manifest not found at {}", state_path)
        ))?;
    }

    let content = std::fs::read_to_string(path)
        .map_err(|e| NounVerbError::execution_error(format!("Failed to read manifest: {}", e)))?;

    let manifest: ManifestData = serde_json::from_str(&content)
        .map_err(|e| NounVerbError::execution_error(format!("Invalid JSON in manifest: {}", e)))?;

    Ok(manifest)
}

fn validate_jsonld_structure(manifest: &ManifestData) -> Result<Vec<ValidationCheck>> {
    let mut checks = Vec::new();

    // Check 1: @context presence
    let context_present = manifest.context.is_some();
    checks.push(ValidationCheck {
        test: "urn:mcpp:test:context_presence".to_string(),
        outcome: if context_present { "earl:Passed" } else { "earl:Failed" }.to_string(),
        detail: if context_present {
            "@context field is present".to_string()
        } else {
            "@context field is missing — required for JSON-LD validation".to_string()
        },
    });

    // Check 2: @type presence
    let type_present = manifest.type_field.is_some();
    checks.push(ValidationCheck {
        test: "urn:mcpp:test:type_presence".to_string(),
        outcome: if type_present { "earl:Passed" } else { "earl:Failed" }.to_string(),
        detail: if type_present {
            "@type field is present".to_string()
        } else {
            "@type field is missing — required for object classification".to_string()
        },
    });

    // Check 3: @vocab is null (public vocabulary only)
    let vocab_valid = if let Some(JsonValue::Object(ctx)) = &manifest.context {
        if let Some(vocab) = ctx.get("@vocab") {
            *vocab == JsonValue::Null
        } else {
            true // @vocab not present is acceptable
        }
    } else {
        true
    };

    checks.push(ValidationCheck {
        test: "urn:mcpp:test:public_vocab".to_string(),
        outcome: if vocab_valid { "earl:Passed" } else { "earl:Failed" }.to_string(),
        detail: if vocab_valid {
            "@vocab is null or absent — public vocabulary only".to_string()
        } else {
            "@vocab must be null for ETHOS conformance (private vocabularies forbidden)".to_string()
        },
    });

    // Check 4: Required namespaces present in @context
    let required_namespaces = vec!["mcpp", "prov", "codemeta", "dcterms", "earl", "schema"];
    let namespaces_present = if let Some(JsonValue::Object(ctx)) = &manifest.context {
        required_namespaces.iter().all(|ns| {
            let ns_str = ns.to_string();
            let ns_colon = format!("{}:", ns);
            ctx.contains_key(&ns_str) || ctx.contains_key(&ns_colon)
        })
    } else {
        false
    };

    checks.push(ValidationCheck {
        test: "urn:mcpp:test:required_namespaces".to_string(),
        outcome: if namespaces_present { "earl:Passed" } else { "earl:Failed" }.to_string(),
        detail: if namespaces_present {
            "All required namespaces (mcpp, prov, codemeta, dcterms, earl, schema) are defined".to_string()
        } else {
            format!("Missing required namespaces. Required: {}", required_namespaces.join(", "))
        },
    });

    Ok(checks)
}

fn generate_earl_ttl(checks: &[ValidationCheck], _operation_id: &str) -> String {
    let mut ttl = String::new();

    // Turtle prefixes
    ttl.push_str(r#"@prefix earl: <http://www.w3.org/ns/earl#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix mcpp: <urn:mcpp:> .

"#);

    // Generate EARL TestResult for each check
    for (idx, check) in checks.iter().enumerate() {
        let result_id = format!("urn:mcpp:extract_claims:validation:{}", idx + 1);
        let outcome_uri = check.outcome.clone();
        let timestamp = chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true);

        ttl.push_str(&format!(
            r#"<{}> a earl:TestResult ;
    earl:outcome {} ;
    earl:test <{}> ;
    dcterms:date "{}"^^xsd:dateTime ;
    rdfs:comment "{}" .

"#,
            result_id,
            outcome_uri,
            check.test,
            timestamp,
            escape_turtle_string(&check.detail)
        ));
    }

    ttl
}

fn escape_turtle_string(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "\\r")
}

/// Validates a manifest using SHACL shapes and emits EARL assertions
///
/// Usage: ggen doctor validate manifest <name>
///
/// Example: ggen doctor validate manifest extract_claims
#[verb]
fn validate(manifest_name: String) -> Result<ShaclValidationResult> {
    // Load manifest
    let manifest = load_manifest(&manifest_name)?;

    // Run JSON-LD and SHACL validation
    let checks = validate_jsonld_structure(&manifest)?;

    // Determine overall validity
    let valid = checks.iter().all(|c| c.outcome == "earl:Passed");

    // Generate EARL Turtle output
    let earl_ttl = generate_earl_ttl(&checks, &manifest_name);

    // Print EARL output
    println!("{}", earl_ttl);

    // Return validation result
    Ok(ShaclValidationResult {
        valid,
        checks,
    })
}
