#![allow(
    warnings,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    clippy::cargo,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::todo,
    clippy::unimplemented
)]
#![allow(missing_docs)] // Binary crate - documentation not required

//! Weaver smoke test binary
//!
//! This binary performs a basic smoke test of Weaver integration:
//! 1. Checks Weaver binary exists and runs `--version`
//! 2. Starts Weaver with test registry
//! 3. Sends a test span to Weaver
//! 4. Stops Weaver
//!
//! Requires `weaver` feature to be enabled.

#[cfg(feature = "weaver")]
fn main() -> Result<(), Box<dyn std::error::Error>> {
    use chicago_tdd_tools::observability::weaver::{
        send_test_span_to_weaver, WeaverValidationError, WeaverValidator,
    };
    use chicago_tdd_tools::observability::weaver::{DEFAULT_OTLP_GRPC_PORT, LOCALHOST};
    use std::path::PathBuf;
    use std::thread::sleep;
    use std::time::Duration;

    use chicago_tdd_tools::observability::weaver::types::WeaverLiveCheck;
    use std::process::Command;

    let registry_path = PathBuf::from("registry");
    if !registry_path.exists() {
        return Err(Box::new(WeaverValidationError::RegistryNotFound(format!(
            "{} (run cargo make weaver-bootstrap)",
            registry_path.display()
        ))));
    }

    let weaver_binary = WeaverLiveCheck::find_weaver_binary().ok_or_else(|| {
        Box::new(WeaverValidationError::BinaryNotFound) as Box<dyn std::error::Error>
    })?;
    let version_output = Command::new(&weaver_binary).arg("--version").output().map_err(|e| {
        Box::new(WeaverValidationError::ProcessStartFailed(format!(
            "Failed to execute {} --version: {e}",
            weaver_binary.display()
        ))) as Box<dyn std::error::Error>
    })?;
    if !version_output.status.success() {
        return Err(Box::new(WeaverValidationError::ProcessStartFailed(format!(
            "Weaver --version exited with status {}",
            version_output.status
        ))));
    }

    let mut validator = WeaverValidator::new(registry_path);
    validator.start()?;

    // Give Weaver a moment to process incoming telemetry before shutdown
    sleep(Duration::from_millis(1500));

    let endpoint = format!("http://{LOCALHOST}:{DEFAULT_OTLP_GRPC_PORT}/v1/traces");
    send_test_span_to_weaver(&endpoint, "weaver_smoke_span")?;

    // Give Weaver a moment to process incoming telemetry before shutdown
    sleep(Duration::from_millis(500));

    validator.stop()?;

    println!("Weaver smoke validation succeeded");
    Ok(())
}

#[cfg(not(feature = "weaver"))]
fn main() {
    eprintln!("Weaver feature not enabled. Build with --features weaver");
    std::process::exit(1);
}
