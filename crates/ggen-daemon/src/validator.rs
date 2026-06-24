use std::{path::Path, time::Instant};
use tokio::process::Command;
use tracing::{info, warn};

/// Result of a language-aware post-generation validation pass.
#[derive(Debug, Clone)]
pub struct ValidationResult {
    pub passed: bool,
    /// Wall-clock milliseconds spent in validation subprocesses.
    pub elapsed_ms: u64,
    /// Human-readable summary of the first failure encountered, or `None` on pass.
    pub failure_summary: Option<String>,
}

/// Run language-aware validation on a cloned target repo after generation.
///
/// Validation is best-effort: if the required tool is absent (e.g. `cargo`
/// not on PATH), the check is skipped and `passed` is returned as `true` so
/// that the absence of a tool never blocks dispatch.  Only a *tool failure*
/// (non-zero exit) sets `passed = false`.
pub async fn validate_generated(local: &Path, language: Option<&str>) -> ValidationResult {
    let t = Instant::now();

    let result = match language.map(|l| l.to_lowercase()).as_deref() {
        Some("rust") => validate_rust(local).await,
        Some("python") => validate_python(local).await,
        Some("typescript") | Some("javascript") => validate_typescript(local).await,
        _ => {
            // Unknown or absent language: skip validation.
            info!("validator: no language-specific check for {:?}", language);
            return ValidationResult { passed: true, elapsed_ms: t.elapsed().as_millis() as u64, failure_summary: None };
        }
    };

    let elapsed_ms = t.elapsed().as_millis() as u64;
    match result {
        Ok(()) => ValidationResult { passed: true, elapsed_ms, failure_summary: None },
        Err(summary) => {
            warn!("validator: {}", summary);
            ValidationResult { passed: false, elapsed_ms, failure_summary: Some(summary) }
        }
    }
}

// ---------------------------------------------------------------------------
// Language backends
// ---------------------------------------------------------------------------

async fn validate_rust(local: &Path) -> Result<(), String> {
    // cargo check is the cheapest correctness gate — type-checks without linking.
    let cargo_toml = local.join("Cargo.toml");
    if !cargo_toml.exists() {
        info!("validator(rust): no Cargo.toml in {}, skipping", local.display());
        return Ok(());
    }
    run_tool(
        "cargo",
        &["check", "--manifest-path", cargo_toml.to_str().unwrap_or("Cargo.toml"), "--quiet"],
        local,
        30,
    )
    .await
}

async fn validate_python(local: &Path) -> Result<(), String> {
    // Collect all .py files and compile-check each with py_compile.
    let mut py_files: Vec<String> = Vec::new();
    collect_py_files(local, &mut py_files);

    if py_files.is_empty() {
        return Ok(());
    }

    // python -m py_compile accepts multiple files.
    let mut args = vec!["-m".to_owned(), "py_compile".to_owned()];
    args.extend(py_files);
    let args_ref: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    run_tool("python3", &args_ref, local, 30).await
}

async fn validate_typescript(local: &Path) -> Result<(), String> {
    let tsconfig = local.join("tsconfig.json");
    if !tsconfig.exists() {
        info!("validator(typescript): no tsconfig.json in {}, skipping", local.display());
        return Ok(());
    }
    run_tool("tsc", &["--noEmit", "--project", tsconfig.to_str().unwrap_or("tsconfig.json")], local, 60).await
}

// ---------------------------------------------------------------------------
// Shared subprocess helper
// ---------------------------------------------------------------------------

/// Run a subprocess with a timeout (in seconds).  Returns `Ok(())` on exit 0,
/// `Err(summary)` on non-zero exit or if the binary is not found.
/// A missing binary is treated as a skip (returns `Ok`), not a failure.
async fn run_tool(bin: &str, args: &[&str], cwd: &Path, timeout_secs: u64) -> Result<(), String> {
    let child = Command::new(bin)
        .args(args)
        .current_dir(cwd)
        .output();

    let output = match tokio::time::timeout(
        std::time::Duration::from_secs(timeout_secs),
        child,
    )
    .await
    {
        Ok(Ok(o)) => o,
        Ok(Err(e)) if e.kind() == std::io::ErrorKind::NotFound => {
            info!("validator: {} not found, skipping", bin);
            return Ok(());
        }
        Ok(Err(e)) => return Err(format!("{} spawn error: {}", bin, e)),
        Err(_) => return Err(format!("{} timed out after {}s", bin, timeout_secs)),
    };

    if output.status.success() {
        return Ok(());
    }

    let stderr = String::from_utf8_lossy(&output.stderr);
    let tail: String = stderr.chars().rev().take(500).collect::<String>().chars().rev().collect();
    Err(format!("{} failed (exit {}): {}", bin, output.status.code().unwrap_or(-1), tail.trim()))
}

fn collect_py_files(dir: &Path, out: &mut Vec<String>) {
    let Ok(entries) = std::fs::read_dir(dir) else { return };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
            if !name.starts_with('.') && name != "node_modules" && name != "__pycache__" {
                collect_py_files(&path, out);
            }
        } else if path.extension().and_then(|e| e.to_str()) == Some("py") {
            if let Some(s) = path.to_str() {
                out.push(s.to_owned());
            }
        }
    }
}
