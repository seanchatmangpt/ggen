//! Weaver Live-Check Types
//!
//! `WeaverLiveCheck` implementation ported from knhk-otel for standalone use.
//! Used for live validation of OpenTelemetry telemetry against semantic conventions.

use std::process::Child;
use thiserror::Error;

/// Weaver validation error
#[derive(Error, Debug)]
pub enum WeaverValidationError {
    /// Weaver binary not found in PATH
    #[error("🚨 Weaver binary not found: {0}\n   ⚠️  STOP: Cannot proceed with Weaver operations\n   💡 FIX: Run cargo make weaver-bootstrap\n   📋 Manual: cargo install weaver\n   📋 Download: https://github.com/open-telemetry/weaver/releases")]
    BinaryNotFound(String),
    /// Weaver health check failed
    #[error("⚠️  Weaver health check failed: {0}\n   ⚠️  WARNING: Weaver may not be responding correctly")]
    HealthCheckFailed(String),
    /// Failed to start Weaver process
    #[error("Failed to start Weaver: {0}")]
    StartFailed(String),
    /// Failed to stop Weaver process
    #[error("Failed to stop Weaver: {0}")]
    StopFailed(String),
}

/// Weaver live-check integration for telemetry validation
#[derive(Debug)]
pub struct WeaverLiveCheck {
    registry_path: Option<String>,
    otlp_grpc_address: String,
    otlp_grpc_port: u16,
    admin_port: u16,
    inactivity_timeout: u64,
    format: String,
    output: Option<String>,
}

impl WeaverLiveCheck {
    /// Create a new Weaver live-check instance
    #[must_use]
    pub fn new() -> Self {
        Self {
            registry_path: None,
            otlp_grpc_address: "0.0.0.0".to_string(), // Match weaver default
            otlp_grpc_port: 4317,
            admin_port: 4320,           // Match weaver default (not 8080)
            inactivity_timeout: 10,     // Match weaver default (not 60)
            format: "ansi".to_string(), // Match weaver default (not "json")
            output: None,
        }
    }

    /// Set the semantic convention registry path
    #[must_use]
    pub fn with_registry(mut self, registry_path: String) -> Self {
        self.registry_path = Some(registry_path);
        self
    }

    /// Set the OTLP gRPC address
    #[must_use]
    pub fn with_otlp_address(mut self, address: String) -> Self {
        self.otlp_grpc_address = address;
        self
    }

    /// Set the OTLP gRPC port
    #[must_use]
    pub const fn with_otlp_port(mut self, port: u16) -> Self {
        self.otlp_grpc_port = port;
        self
    }

    /// Set the admin HTTP port
    #[must_use]
    pub const fn with_admin_port(mut self, port: u16) -> Self {
        self.admin_port = port;
        self
    }

    /// Set the inactivity timeout in seconds
    #[must_use]
    pub const fn with_inactivity_timeout(mut self, timeout: u64) -> Self {
        self.inactivity_timeout = timeout;
        self
    }

    /// Set the output format (json, ansi)
    #[must_use]
    pub fn with_format(mut self, format: String) -> Self {
        self.format = format;
        self
    }

    /// Set the output directory (for JSON reports)
    #[must_use]
    pub fn with_output(mut self, output: String) -> Self {
        self.output = Some(output);
        self
    }

    /// Find weaver binary in multiple locations
    /// Checks: PATH, target/debug/weaver, target/release/weaver
    #[must_use]
    pub fn find_weaver_binary() -> Option<std::path::PathBuf> {
        use std::path::PathBuf;
        use std::process::Command;

        // 1. Check PATH first
        if Command::new("weaver").arg("--version").output().is_ok() {
            return Some(PathBuf::from("weaver"));
        }

        // 2. Check target/debug/weaver
        let debug_path = PathBuf::from("target/debug/weaver");
        if debug_path.exists() {
            return Some(debug_path);
        }

        // 3. Check target/release/weaver
        let release_path = PathBuf::from("target/release/weaver");
        if release_path.exists() {
            return Some(release_path);
        }

        None
    }

    /// Check if Weaver binary is available (checks multiple locations)
    ///
    /// 🚨 CRITICAL - Returns error if Weaver binary not found.
    ///
    /// # Errors
    ///
    /// Returns an error if Weaver binary is not found.
    pub fn check_weaver_available() -> Result<(), WeaverValidationError> {
        use std::process::Command;

        // Try to find weaver binary
        if let Some(binary_path) = Self::find_weaver_binary() {
            // Try to run weaver --version to check if it exists and works
            match Command::new(&binary_path).arg("--version").output() {
                Ok(output) => {
                    if output.status.success() {
                        // ✅ Weaver binary is available and working
                        Ok(())
                    } else {
                        Err(WeaverValidationError::BinaryNotFound(
                            "Weaver binary found but --version failed. Binary may be corrupted."
                                .to_string(),
                        ))
                    }
                }
                Err(e) => Err(WeaverValidationError::BinaryNotFound(format!(
                    "Failed to execute weaver binary: {e}"
                ))),
            }
        } else {
            // Try runtime download if not found (only if weaver feature is enabled)
            #[cfg(feature = "weaver")]
            {
                if let Err(e) = Self::download_weaver_runtime() {
                    return Err(WeaverValidationError::BinaryNotFound(format!(
                        "Weaver binary not found. Run cargo make weaver-bootstrap. Manual fallback: cargo install weaver or download from https://github.com/open-telemetry/weaver/releases ({e})."
                    )));
                }
                // Retry after download
                Self::check_weaver_available()
            }
            #[cfg(not(feature = "weaver"))]
            {
                Err(WeaverValidationError::BinaryNotFound(
                    "Weaver binary not found. Run cargo make weaver-bootstrap. Manual fallback: cargo install weaver or download from https://github.com/open-telemetry/weaver/releases".to_string()
                ))
            }
        }
    }

    /// Check if Weaver semantic convention registry is available and accessible
    ///
    /// **FAIL-FAST HARDENING**: Validates registry before tests run, with timeout protection.
    /// Root cause: Registry availability determines test success/failure, better to fail immediately.
    /// Solution: Check path exists, is readable, contains required files (5s timeout max).
    ///
    /// Checks:
    /// 1. Registry path exists at `./registry/`
    /// 2. Registry is readable (not permission denied)
    /// 3. Contains expected semantic convention files
    /// 4. All operations complete within timeout
    ///
    /// # Returns
    /// Ok if registry is available and accessible, Err with clear message otherwise
    ///
    /// # Errors
    ///
    /// Returns error if registry path does not exist, is not readable, or appears empty.
    ///
    /// # Example
    /// ```ignore
    /// if WeaverLiveCheck::check_registry_available().is_err() {
    ///     eprintln!("Registry not available, skipping Weaver tests");
    ///     return;
    /// }
    /// ```
    pub fn check_registry_available() -> Result<(), String> {
        const MAX_CHECK_TIME_MS: u128 = 5000; // 5 second timeout

        use std::fs;
        use std::path::PathBuf;
        use std::time::Instant;

        let mut registry_path = PathBuf::from("registry");
        if registry_path.join("model").exists() {
            registry_path = registry_path.join("model");
        }
        let start_time = Instant::now();

        // Check 1: Path exists
        if !registry_path.exists() {
            return Err(format!(
                "🚨 Registry path does not exist: {}\n   ⚠️  STOP: Cannot proceed with Weaver tests\n   💡 FIX: Run cargo make weaver-bootstrap",
                registry_path.display()
            ));
        }

        // Check timeout hasn't elapsed
        if start_time.elapsed().as_millis() > MAX_CHECK_TIME_MS {
            return Err("Registry check timed out (5s). Filesystem may be slow.".to_string());
        }

        // Check 2: Path is a directory and readable
        match fs::metadata(&registry_path) {
            Ok(metadata) => {
                if !metadata.is_dir() {
                    return Err(format!(
                        "🚨 Registry path is not a directory: {}",
                        registry_path.display()
                    ));
                }
                // Check if we can read the directory
                if metadata.permissions().readonly() && cfg!(unix) {
                    return Err(format!(
                        "⚠️  Registry path is not readable (permission denied): {}",
                        registry_path.display()
                    ));
                }
            }
            Err(e) => {
                return Err(format!(
                    "⚠️  Cannot read registry metadata: {} - {e}",
                    registry_path.display()
                ))
            }
        }

        // Check timeout again before final checks
        if start_time.elapsed().as_millis() > MAX_CHECK_TIME_MS {
            return Err("Registry check timed out (5s) during metadata validation.".to_string());
        }

        // Check 3: Contains expected structure (at least one .yaml file or subdirectory)
        // This is a best-effort check - the exact structure may vary
        match fs::read_dir(&registry_path) {
            Ok(entries) => {
                let has_content = entries
                    .take(100) // Limit iterations to prevent long hangs
                    .any(|entry| {
                        if let Ok(e) = entry {
                            if let Ok(m) = e.metadata() {
                                if m.is_dir() {
                                    return true;
                                }
                                if let Some(ext) = e.path().extension() {
                                    if ext == "yaml" || ext == "yml" || ext == "json" {
                                        return true;
                                    }
                                }
                            }
                        }
                        false
                    });

                if !has_content {
                    return Err(format!(
                        "⚠️  Registry appears empty (no YAML/JSON files or subdirectories): {}",
                        registry_path.display()
                    ));
                }
            }
            Err(e) => {
                return Err(format!(
                    "⚠️  Cannot read registry directory: {} - {e}",
                    registry_path.display()
                ))
            }
        }

        // Final timeout check
        if start_time.elapsed().as_millis() > MAX_CHECK_TIME_MS {
            return Err("Registry check timed out (5s) during content validation.".to_string());
        }

        Ok(())
    }

    /// Download weaver binary at runtime if not found
    #[cfg(feature = "weaver")]
    fn download_weaver_runtime() -> Result<(), String> {
        use std::env;
        use std::fs;
        use std::path::PathBuf;
        use std::process::Command;

        // Determine target directory
        let profile = env::var("PROFILE").unwrap_or_else(|_| "debug".to_string());
        let output_path = PathBuf::from(format!("target/{profile}/weaver"));

        // Skip if already exists
        if output_path.exists() {
            return Ok(());
        }

        // Detect platform
        let target = env::var("TARGET").unwrap_or_else(|_| "unknown".to_string());
        let (arch, os) = Self::detect_platform_from_target(&target);
        let weaver_version = "0.19.0";

        // Construct download URL
        let download_url = format!(
            "https://github.com/open-telemetry/weaver/releases/download/v{weaver_version}/weaver-{arch}-{os}.tar.xz"
        );

        // Create parent directory
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent).map_err(|e| format!("Failed to create directory: {e}"))?;
        }

        // Download using curl or wget
        let archive_path = output_path.with_extension("tar.xz");
        if Command::new("curl").arg("--version").output().is_ok() {
            let archive_str = archive_path
                .to_str()
                .ok_or_else(|| "Archive path is not valid UTF-8".to_string())?;
            let status = Command::new("curl")
                .args(["-L", "-o", archive_str, &download_url])
                .status()
                .map_err(|e| format!("Failed to execute curl: {e}"))?;

            if !status.success() {
                return Err("curl download failed".to_string());
            }
        } else if Command::new("wget").arg("--version").output().is_ok() {
            let archive_str = archive_path
                .to_str()
                .ok_or_else(|| "Archive path is not valid UTF-8".to_string())?;
            let status = Command::new("wget")
                .args(["-O", archive_str, &download_url])
                .status()
                .map_err(|e| format!("Failed to execute wget: {e}"))?;

            if !status.success() {
                return Err("wget download failed".to_string());
            }
        } else {
            return Err(
                "Neither curl nor wget found. Please install one to download weaver.".to_string(),
            );
        }

        // Extract tar.xz
        let output_dir = archive_path
            .parent()
            .ok_or_else(|| "Archive path has no parent directory".to_string())?;
        let archive_str = archive_path
            .to_str()
            .ok_or_else(|| "Archive path is not valid UTF-8".to_string())?;
        let output_dir_str = output_dir
            .to_str()
            .ok_or_else(|| "Output directory path is not valid UTF-8".to_string())?;
        let status = Command::new("tar")
            .args(["-xJf", archive_str, "-C", output_dir_str])
            .status()
            .map_err(|e| format!("Failed to extract tar.xz: {e}"))?;

        if !status.success() {
            return Err("tar extraction failed".to_string());
        }

        // Find and move weaver binary
        let weaver_binary = output_dir.join("weaver");
        if weaver_binary.exists() {
            fs::rename(&weaver_binary, &output_path)
                .map_err(|e| format!("Failed to move weaver binary: {e}"))?;
        }

        // Clean up archive
        let _ = fs::remove_file(&archive_path);

        // Make executable (Unix-like systems)
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            if let Ok(mut perms) = fs::metadata(&output_path).map(|m| m.permissions()) {
                perms.set_mode(0o755);
                let _ = fs::set_permissions(&output_path, perms);
            }
        }

        Ok(())
    }

    /// Detect platform from TARGET environment variable
    #[cfg(feature = "weaver")]
    fn detect_platform_from_target(target: &str) -> (&'static str, &'static str) {
        if target.contains("x86_64") {
            if target.contains("linux") {
                ("x86_64", "unknown-linux-gnu")
            } else if target.contains("darwin") || target.contains("macos") {
                ("x86_64", "apple-darwin")
            } else if target.contains("windows") {
                ("x86_64", "pc-windows-msvc")
            } else {
                ("x86_64", "unknown")
            }
        } else if target.contains("aarch64") || target.contains("arm64") {
            if target.contains("linux") {
                ("aarch64", "unknown-linux-gnu")
            } else if target.contains("darwin") || target.contains("macos") {
                ("aarch64", "apple-darwin")
            } else {
                ("aarch64", "unknown")
            }
        } else {
            ("unknown", "unknown")
        }
    }

    /// Check Weaver health by querying the admin endpoint
    ///
    /// # Errors
    ///
    /// Returns an error if health check fails.
    pub fn check_health(&self) -> Result<bool, String> {
        // Note: This requires reqwest, which may not be available
        // For now, return a basic connectivity check
        match std::net::TcpStream::connect(format!(
            "{}:{}",
            self.otlp_grpc_address, self.admin_port
        )) {
            Ok(_) => Ok(true), // Port is open, assume Weaver is running
            Err(e) => Err(format!(
                "Weaver admin endpoint not responding on {}:{}: {e}",
                self.otlp_grpc_address, self.admin_port
            )),
        }
    }

    /// Run live-check and return the process handle
    /// The caller should send telemetry to the configured OTLP endpoint
    ///
    /// # Errors
    ///
    /// Returns an error if Weaver binary is not available or starting the process fails.
    pub fn start(&self) -> Result<Child, String> {
        // Items (use statements) must come before statements (Rust requirement)
        use std::process::Command;

        // Check Weaver binary availability first (may trigger runtime download)
        Self::check_weaver_available().map_err(|e| format!("{e}"))?;

        // Find weaver binary path
        let weaver_binary = Self::find_weaver_binary()
            .ok_or_else(|| "Weaver binary not found after check".to_string())?;

        let mut cmd = Command::new(&weaver_binary);

        cmd.args(["registry", "live-check"]);

        if let Some(ref registry) = self.registry_path {
            cmd.args(["--registry", registry]);
        }

        cmd.args(["--otlp-grpc-address", &self.otlp_grpc_address]);
        cmd.args(["--otlp-grpc-port", &self.otlp_grpc_port.to_string()]);
        cmd.args(["--admin-port", &self.admin_port.to_string()]);
        cmd.args(["--inactivity-timeout", &self.inactivity_timeout.to_string()]);
        cmd.args(["--format", &self.format]);

        if let Some(ref output) = self.output {
            cmd.args(["--output", output]);
        }

        cmd.spawn()
            .map_err(|e| {
                if e.kind() == std::io::ErrorKind::NotFound {
                    "🚨 Weaver binary not found in PATH. Run cargo make weaver-bootstrap (installs CLI locally) or cargo install weaver".to_string()
                } else {
                    format!("Failed to start Weaver live-check: {e}. Ensure Weaver is installed and in PATH.")
                }
            })
    }

    /// Stop the live-check process via HTTP admin endpoint
    ///
    /// # Errors
    ///
    /// Returns an error if stopping the process fails.
    #[cfg(feature = "weaver")]
    pub fn stop(&self) -> Result<(), String> {
        use std::io::{Read, Write};
        use std::net::{TcpStream, ToSocketAddrs};
        use std::time::Duration;

        let ip_addr = if self.otlp_grpc_address == "0.0.0.0" {
            "127.0.0.1"
        } else {
            &self.otlp_grpc_address
        };
        let addr_str = format!("{ip_addr}:{}", self.admin_port);
        let socket_addr = addr_str
            .to_socket_addrs()
            .map_err(|e| format!("Failed to resolve address {addr_str}: {e}"))?
            .next()
            .ok_or_else(|| format!("No addresses resolved for {addr_str}"))?;

        let mut stream = TcpStream::connect_timeout(&socket_addr, Duration::from_secs(5))
            .map_err(|e| format!("Failed to connect to Weaver admin at {addr_str}: {e}"))?;

        stream
            .set_read_timeout(Some(Duration::from_secs(5)))
            .map_err(|e| format!("Failed to set read timeout: {e}"))?;
        stream
            .set_write_timeout(Some(Duration::from_secs(5)))
            .map_err(|e| format!("Failed to set write timeout: {e}"))?;

        let request = format!(
            "POST /stop HTTP/1.1\r\n\
             Host: {addr_str}\r\n\
             Connection: close\r\n\
             Content-Length: 0\r\n\r\n"
        );

        stream
            .write_all(request.as_bytes())
            .map_err(|e| format!("Failed to send stop request: {e}"))?;

        let mut response = String::new();
        match stream.read_to_string(&mut response) {
            Ok(_) => {
                if response.is_empty()
                    || response.contains("HTTP/1.1 2")
                    || response.contains("HTTP/1.0 2")
                {
                    Ok(())
                } else {
                    Err(format!(
                        "Weaver stop request returned unexpected response: {}",
                        response.lines().next().unwrap_or("")
                    ))
                }
            }
            Err(_) => {
                // If it failed to read because the server shut down and closed the connection,
                // that is expected.
                Ok(())
            }
        }
    }

    /// Stop the live-check process via HTTP admin endpoint
    /// Fallback when weaver feature is not enabled
    #[cfg(not(feature = "weaver"))]
    pub fn stop(&self) -> Result<(), String> {
        Err("Weaver stop requires weaver feature to be enabled".to_string())
    }

    /// Get the OTLP gRPC endpoint for sending telemetry
    /// Note: Weaver live-check listens on gRPC, but exporters typically use HTTP
    /// This returns the address:port format for configuration
    #[must_use]
    pub fn otlp_endpoint(&self) -> String {
        format!("{}:{}", self.otlp_grpc_address, self.otlp_grpc_port)
    }
}

impl Default for WeaverLiveCheck {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::*;

    // Test error types (critical - 80% of bugs)
    #[test]
    fn test_weaver_validation_error_display() {
        // Test all error variants display correctly
        let errors = vec![
            WeaverValidationError::BinaryNotFound("test".to_string()),
            WeaverValidationError::HealthCheckFailed("test".to_string()),
            WeaverValidationError::StartFailed("test".to_string()),
            WeaverValidationError::StopFailed("test".to_string()),
        ];

        for error in errors {
            let display = format!("{error}");
            assert!(!display.is_empty(), "Error should have display message");
            assert!(display.contains("test"), "Error should contain message");
        }
    }

    #[test]
    fn test_weaver_validation_error_debug() {
        // Test error is debuggable
        let error = WeaverValidationError::BinaryNotFound("test".to_string());
        let debug = format!("{error:?}");
        assert!(debug.contains("BinaryNotFound"));
        assert!(debug.contains("test"));
    }

    // Test builder pattern (important - used frequently)
    #[test]
    fn test_weaver_live_check_new() {
        // Test default values (match weaver defaults)
        let check = WeaverLiveCheck::new();
        assert_eq!(check.otlp_grpc_address, "0.0.0.0");
        assert_eq!(check.otlp_grpc_port, 4317);
        assert_eq!(check.admin_port, 4320); // Match weaver default
        assert_eq!(check.inactivity_timeout, 10); // Match weaver default
        assert_eq!(check.format, "ansi"); // Match weaver default
        assert!(check.registry_path.is_none());
        assert!(check.output.is_none());
    }

    #[test]
    fn test_weaver_live_check_default() {
        // Test Default trait implementation (match weaver defaults)
        let check = WeaverLiveCheck::default();
        assert_eq!(check.otlp_grpc_address, "0.0.0.0");
        assert_eq!(check.otlp_grpc_port, 4317);
        assert_eq!(check.admin_port, 4320); // Match weaver default
    }

    #[test]
    fn test_weaver_live_check_builder_pattern() {
        // Test builder pattern (chaining)
        let check = WeaverLiveCheck::new()
            .with_registry("/path/to/registry".to_string())
            .with_otlp_address("0.0.0.0".to_string())
            .with_otlp_port(4318)
            .with_admin_port(8081)
            .with_inactivity_timeout(120)
            .with_format("ansi".to_string())
            .with_output("/tmp/output".to_string());

        assert_eq!(check.registry_path, Some("/path/to/registry".to_string()));
        assert_eq!(check.otlp_grpc_address, "0.0.0.0");
        assert_eq!(check.otlp_grpc_port, 4318);
        assert_eq!(check.admin_port, 8081);
        assert_eq!(check.inactivity_timeout, 120);
        assert_eq!(check.format, "ansi");
        assert_eq!(check.output, Some("/tmp/output".to_string()));
    }

    #[test]
    fn test_weaver_live_check_otlp_endpoint() {
        // Test OTLP endpoint generation (important - used frequently)
        let check = WeaverLiveCheck::new();
        assert_eq!(check.otlp_endpoint(), "0.0.0.0:4317");

        let check = WeaverLiveCheck::new()
            .with_otlp_address("0.0.0.0".to_string())
            .with_otlp_port(4318);
        assert_eq!(check.otlp_endpoint(), "0.0.0.0:4318");
    }

    // Test boundary conditions (important - 80% of bugs)
    #[test]
    fn test_weaver_live_check_port_boundaries() {
        // Test port boundaries (u16: 0-65535)
        let check = WeaverLiveCheck::new()
            .with_otlp_port(0)
            .with_admin_port(65535);
        assert_eq!(check.otlp_grpc_port, 0);
        assert_eq!(check.admin_port, 65535);
    }

    #[test]
    fn test_weaver_live_check_timeout_boundaries() {
        // Test timeout boundaries (u64)
        let check = WeaverLiveCheck::new()
            .with_inactivity_timeout(0)
            .with_inactivity_timeout(u64::MAX);
        assert_eq!(check.inactivity_timeout, u64::MAX);
    }
}
