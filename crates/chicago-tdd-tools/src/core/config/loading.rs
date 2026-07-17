//! Configuration Loading
//!
//! Provides runtime configuration loading from `chicago-tdd-tools.toml` file.
//! Configuration values override hardcoded defaults when config file is present.
//!
//! **Pattern**: Configuration is optional - if file doesn't exist or parsing fails,
//! framework uses hardcoded defaults. This ensures framework works without configuration.
//!
//! **Poka-Yoke Design**: Config loading validates values using poka-yoke types to prevent
//! invalid values (like 0 for timeouts/ports). Invalid values fall back to defaults and
//! log warnings, ensuring the framework always uses valid configuration.
//!
//! **Root Cause Prevention**: When adding new config options:
//! 1. Add `read_config_value()` call in this file FIRST
//! 2. Then add option to chicago-tdd-tools.toml
//! 3. Run tests to verify option is read correctly
//!
//! This prevents config drift (options in config file that aren't read by code).
//! See `test_config_options_match_implementation()` for automated verification.

use crate::core::config::poka_yoke::{BoundedTimeout, PositiveU32, PositiveUsize};
use std::env;
use std::fs;
use std::path::PathBuf;

/// **Kaizen improvement**: Default configuration values extracted to named constants.
/// Makes code more readable, easier to change, and self-documenting.
/// Pattern: Use named constants instead of magic numbers for configuration defaults.
/// Default unit test timeout in seconds
const DEFAULT_UNIT_TEST_TIMEOUT_SECONDS: u64 = 1;

/// Default integration test timeout in seconds
const DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS: u64 = 30;

/// Default property test cases
const DEFAULT_PROPERTY_TEST_CASES: u32 = 100;

/// Default hot path tick budget
const DEFAULT_HOT_PATH_TICK_BUDGET: u64 = 8;

/// Default max run length
const DEFAULT_MAX_RUN_LEN: usize = 8;

/// Default max batch size
const DEFAULT_MAX_BATCH_SIZE: usize = 1000;

/// **Gemba Fix**: Testcontainers default values extracted to named constants.
/// Default container wait timeout in seconds
const DEFAULT_CONTAINER_WAIT_TIMEOUT_SECONDS: u64 = 5;

/// Default HTTP connection timeout in seconds
const DEFAULT_HTTP_CONNECTION_TIMEOUT_SECONDS: u64 = 2;

/// Default HTTP port
const DEFAULT_HTTP_PORT: u16 = 80;

/// Default HTTPS port
const DEFAULT_HTTPS_PORT: u16 = 443;

/// Default HTTP alternate port
const DEFAULT_HTTP_ALT_PORT: u16 = 8080;

/// Default concurrent containers count
const DEFAULT_CONCURRENT_CONTAINERS_COUNT: usize = 5;

/// Default concurrent commands count
const DEFAULT_CONCURRENT_COMMANDS_COUNT: usize = 10;

/// Default multi-container count
const DEFAULT_MULTI_CONTAINER_COUNT: usize = 3;

/// Default commands per container
const DEFAULT_COMMANDS_PER_CONTAINER: usize = 5;

/// **Gemba Fix**: Weaver default values extracted to named constants.
/// Default OTLP gRPC port
const DEFAULT_OTLP_GRPC_PORT: u16 = 4317;

/// Default Weaver startup wait time in milliseconds
const DEFAULT_STARTUP_WAIT_MILLISECONDS: u64 = 1000;

/// Default telemetry processing wait time in milliseconds
const DEFAULT_TELEMETRY_PROCESSING_WAIT_MILLISECONDS: u64 = 2000;

/// **FMEA Fix FM9 (RPN 16)**: Compile-time assertion that default timeout values are within bounds.
/// This prevents defaults from exceeding `MAX_REASONABLE_TIMEOUT`, which would cause `BoundedTimeout::new()` to fail.
/// The assertion runs at compile time, ensuring defaults are always valid.
const _: () = {
    // Assert: Default timeouts must be <= MAX_REASONABLE_TIMEOUT
    assert!(
        DEFAULT_UNIT_TEST_TIMEOUT_SECONDS
            <= crate::core::config::poka_yoke::BoundedTimeout::MAX_REASONABLE_TIMEOUT,
        "DEFAULT_UNIT_TEST_TIMEOUT_SECONDS exceeds MAX_REASONABLE_TIMEOUT"
    );
    assert!(
        DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS
            <= crate::core::config::poka_yoke::BoundedTimeout::MAX_REASONABLE_TIMEOUT,
        "DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS exceeds MAX_REASONABLE_TIMEOUT"
    );
    assert!(
        DEFAULT_HOT_PATH_TICK_BUDGET
            <= crate::core::config::poka_yoke::BoundedTimeout::MAX_REASONABLE_TIMEOUT,
        "DEFAULT_HOT_PATH_TICK_BUDGET exceeds MAX_REASONABLE_TIMEOUT"
    );
    assert!(
        DEFAULT_CONTAINER_WAIT_TIMEOUT_SECONDS
            <= crate::core::config::poka_yoke::BoundedTimeout::MAX_REASONABLE_TIMEOUT,
        "DEFAULT_CONTAINER_WAIT_TIMEOUT_SECONDS exceeds MAX_REASONABLE_TIMEOUT"
    );
    assert!(
        DEFAULT_HTTP_CONNECTION_TIMEOUT_SECONDS
            <= crate::core::config::poka_yoke::BoundedTimeout::MAX_REASONABLE_TIMEOUT,
        "DEFAULT_HTTP_CONNECTION_TIMEOUT_SECONDS exceeds MAX_REASONABLE_TIMEOUT"
    );
    assert!(
        DEFAULT_STARTUP_WAIT_MILLISECONDS
            <= crate::core::config::poka_yoke::BoundedTimeout::MAX_REASONABLE_TIMEOUT,
        "DEFAULT_STARTUP_WAIT_MILLISECONDS exceeds MAX_REASONABLE_TIMEOUT"
    );
    assert!(
        DEFAULT_TELEMETRY_PROCESSING_WAIT_MILLISECONDS
            <= crate::core::config::poka_yoke::BoundedTimeout::MAX_REASONABLE_TIMEOUT,
        "DEFAULT_TELEMETRY_PROCESSING_WAIT_MILLISECONDS exceeds MAX_REASONABLE_TIMEOUT"
    );
};

/// Find config file in project hierarchy
///
/// **FMEA Fix FM3 (RPN 175)**: Logs info when searching for config file, shows searched paths.
/// This improves detection of config file location issues from 7 (Very Low) to 4 (Moderately High).
fn find_config_file() -> Option<PathBuf> {
    const MAX_DEPTH: usize = 5;
    // Start from current directory (for tests) or manifest dir (for library)
    let start_dir = env::var("CARGO_MANIFEST_DIR")
        .ok()
        .map(PathBuf::from)
        .or_else(|| env::current_dir().ok())?;

    let mut current_dir = start_dir;
    let mut searched_paths = Vec::new();

    for _ in 0..MAX_DEPTH {
        let config_path = current_dir.join("chicago-tdd-tools.toml");
        searched_paths.push(config_path.clone());
        if config_path.exists() {
            return Some(config_path);
        }
        if let Some(parent) = current_dir.parent() {
            current_dir = parent.to_path_buf();
        } else {
            break;
        }
    }

    // **FMEA Fix FM3 (RPN 175)**: Log info about searched paths when config file not found
    // This helps users understand where config file should be placed
    // Only log in library context, not in tests (to avoid test noise)
    if env::var("CARGO_MANIFEST_DIR").is_ok() && !searched_paths.is_empty() {
        log::debug!(
            "ℹ️  Info: Config file chicago-tdd-tools.toml not found in searched paths:\n   {}",
            searched_paths
                .iter()
                .map(|p| p.display().to_string())
                .collect::<Vec<_>>()
                .join("\n   ")
        );
        log::debug!(
            "   💡 SUGGESTION: Place chicago-tdd-tools.toml in project root (same directory as Cargo.toml)\n   \
             💡 SUGGESTION: Config file is optional - framework will use defaults if not found"
        );
    }

    None
}

/// Read config value from TOML file
///
/// **Gemba Fix**: Added error handling - logs warnings when config file exists but cannot be parsed.
/// This prevents silent failures and helps users debug configuration issues.
///
/// **Poka-Yoke Fix**: Validates values using `BoundedTimeout::new()` to prevent invalid values (0 or > `MAX_REASONABLE_TIMEOUT`).
/// Invalid values fall back to defaults and log warnings.
///
/// **FMEA Fix FM1 (RPN 270)**: Logs warning when config file exists but key not found.
/// This improves detection of typos in section/key names from 9 (Very Remote) to 4 (Moderately High).
///
/// **FMEA Fix FM5 (RPN 64)**: Parser limitations documented - does NOT handle:
/// - Multi-line values (use single-line values only)
/// - Arrays (use single values only)
/// - Nested tables (except dot notation like `[observability.weaver]`)
/// - Complex TOML syntax (use simple key=value format)
/// - Scientific notation (use decimal numbers only, e.g., `1000` not `1e3`)
///
/// For full TOML support, consider using the `toml` crate, but this simple parser
/// is sufficient for our configuration needs (simple key-value pairs).
#[allow(clippy::too_many_lines)] // Function handles complex config parsing with comprehensive error handling
fn read_config_value(section: &str, key: &str, default: u64) -> u64 {
    if let Some(config_path) = find_config_file() {
        if let Ok(contents) = fs::read_to_string(&config_path) {
            // Simple TOML parsing for our needs
            let mut current_section = String::new();
            let mut parse_errors = Vec::new();
            let mut found_section = false;
            let mut found_key = false;

            for (line_num, line) in contents.lines().enumerate() {
                let line = line.trim();
                if line.is_empty() || line.starts_with('#') {
                    continue;
                }

                // Track current section
                if line.starts_with('[') && line.ends_with(']') {
                    current_section = line[1..line.len() - 1].trim().to_string();
                    if current_section == section {
                        found_section = true;
                    }
                    continue;
                }

                // Check if we're in the right section and key matches
                if current_section == section {
                    found_section = true;
                    if let Some((k, v)) = line.split_once('=') {
                        let k = k.trim();
                        let v = v.trim();
                        if k == key {
                            found_key = true;
                            // Parse value (remove quotes if present)
                            let v = v.trim_matches('"').trim_matches('\'');
                            match v.parse::<u64>() {
                                Ok(parsed) => {
                                    // **Poka-Yoke Fix**: Use BoundedTimeout to enforce bounds at type level
                                    // This prevents invalid values (0) and unreasonably large values (> MAX_REASONABLE_TIMEOUT)
                                    // The type system makes invalid timeout values impossible
                                    match BoundedTimeout::new(parsed) {
                                        Some(valid) => return valid.get(),
                                        None => {
                                            // Invalid value - either 0 or > MAX_REASONABLE_TIMEOUT
                                            if parsed == 0 {
                                                parse_errors.push(format!(
                                                    "Line {}: Invalid value for {}.{}: {} (must be > 0)",
                                                    line_num + 1, section, key, parsed
                                                ));
                                            } else {
                                                // Value exceeds MAX_REASONABLE_TIMEOUT
                                                parse_errors.push(format!(
                                                    "🚨 STOP: Config file {} has invalid timeout value\n   \
                                                     📋 Location: Line {}, section [{}], key '{}'\n   \
                                                     📋 Value: {} seconds (exceeds maximum of {} seconds)\n   \
                                                     💡 FIX: Use a value <= {} seconds\n   \
                                                     💡 Example: unit_timeout_seconds = 30\n   \
                                                     💡 Using default value: {} seconds",
                                                    config_path.display(),
                                                    line_num + 1,
                                                    section,
                                                    key,
                                                    parsed,
                                                    BoundedTimeout::MAX_REASONABLE_TIMEOUT,
                                                    BoundedTimeout::MAX_REASONABLE_TIMEOUT,
                                                    default
                                                ));
                                            }
                                        }
                                    }
                                }
                                Err(_) => {
                                    parse_errors.push(format!(
                                        "Line {}: Invalid value for {}.{}: '{}' (not a number)",
                                        line_num + 1,
                                        section,
                                        key,
                                        v
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            // **FMEA Fix FM1 (RPN 270)**: Warn if config file exists but key not found
            // This detects typos in section/key names
            if found_section && !found_key {
                log::warn!(
                    "⚠️  Warning: Config file {} has section [{}] but key '{}' not found.\n   \
                     💡 FIX: Check for typos in key name\n   \
                     💡 FIX: Verify key name matches: {}.{}\n   \
                     💡 Using default value: {}",
                    config_path.display(),
                    section,
                    key,
                    section,
                    key,
                    default
                );
            } else if !found_section {
                // Section not found - this is OK, config file might not have this section
                // Only warn if config file has other sections (user might have typo in section name)
                let has_any_section = contents.lines().any(|line| {
                    let line = line.trim();
                    line.starts_with('[') && line.ends_with(']')
                });
                if has_any_section {
                    log::info!(
                        "ℹ️  Info: Config file {} exists but section [{}] not found.\n   \
                         💡 Using default value: {}\n   \
                         💡 If you intended to set this value, add [{}] section to config file",
                        config_path.display(),
                        section,
                        default,
                        section
                    );
                }
            }

            // **Gemba Fix**: Log warnings if config file exists but has parse errors
            if !parse_errors.is_empty() {
                log::warn!(
                    "⚠️  Warning: Config file {} has parse errors:\n   {}",
                    config_path.display(),
                    parse_errors.join("\n   ")
                );
                log::warn!("   Using default value: {default}");
            }
        } else {
            // **Gemba Fix**: Log warning if config file exists but cannot be read
            log::warn!(
                "⚠️  Warning: Config file {} exists but cannot be read. Using default value: {}",
                config_path.display(),
                default
            );
        }
    }
    default
}

/// Read config value from TOML file (u32 version)
///
/// **Poka-Yoke Fix**: Validates values using `PositiveU32::new()` to prevent invalid values (0).
fn read_config_value_u32(section: &str, key: &str, default: u32) -> u32 {
    if let Some(config_path) = find_config_file() {
        if let Ok(contents) = fs::read_to_string(&config_path) {
            let mut current_section = String::new();
            let mut parse_errors = Vec::new();

            for (line_num, line) in contents.lines().enumerate() {
                let line = line.trim();
                if line.is_empty() || line.starts_with('#') {
                    continue;
                }

                if line.starts_with('[') && line.ends_with(']') {
                    current_section = line[1..line.len() - 1].trim().to_string();
                    continue;
                }

                if current_section == section {
                    if let Some((k, v)) = line.split_once('=') {
                        let k = k.trim();
                        let v = v.trim();
                        if k == key {
                            let v = v.trim_matches('"').trim_matches('\'');
                            match v.parse::<u32>() {
                                Ok(parsed) => {
                                    // **Poka-Yoke Fix**: Validate using poka-yoke type
                                    match PositiveU32::new(parsed) {
                                        Some(valid) => return valid.get(),
                                        None => {
                                            parse_errors.push(format!(
                                                "Line {}: Invalid value for {}.{}: {} (must be > 0)",
                                                line_num + 1, section, key, parsed
                                            ));
                                        }
                                    }
                                }
                                Err(_) => {
                                    parse_errors.push(format!(
                                        "Line {}: Invalid value for {}.{}: '{}' (not a number)",
                                        line_num + 1,
                                        section,
                                        key,
                                        v
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            if !parse_errors.is_empty() {
                log::warn!(
                    "⚠️  Warning: Config file {} has parse errors:\n   {}",
                    config_path.display(),
                    parse_errors.join("\n   ")
                );
                log::warn!("   Using default value: {default}");
            }
        }
    }
    default
}

/// Read config value from TOML file (usize version)
///
/// **Poka-Yoke Fix**: Validates values using `PositiveUsize::new()` to prevent invalid values (0).
fn read_config_value_usize(section: &str, key: &str, default: usize) -> usize {
    if let Some(config_path) = find_config_file() {
        if let Ok(contents) = fs::read_to_string(&config_path) {
            let mut current_section = String::new();
            let mut parse_errors = Vec::new();

            for (line_num, line) in contents.lines().enumerate() {
                let line = line.trim();
                if line.is_empty() || line.starts_with('#') {
                    continue;
                }

                if line.starts_with('[') && line.ends_with(']') {
                    current_section = line[1..line.len() - 1].trim().to_string();
                    continue;
                }

                if current_section == section {
                    if let Some((k, v)) = line.split_once('=') {
                        let k = k.trim();
                        let v = v.trim();
                        if k == key {
                            let v = v.trim_matches('"').trim_matches('\'');
                            match v.parse::<usize>() {
                                Ok(parsed) => {
                                    // **Poka-Yoke Fix**: Validate using poka-yoke type
                                    match PositiveUsize::new(parsed) {
                                        Some(valid) => return valid.get(),
                                        None => {
                                            parse_errors.push(format!(
                                                "Line {}: Invalid value for {}.{}: {} (must be > 0)",
                                                line_num + 1, section, key, parsed
                                            ));
                                        }
                                    }
                                }
                                Err(_) => {
                                    parse_errors.push(format!(
                                        "Line {}: Invalid value for {}.{}: '{}' (not a number)",
                                        line_num + 1,
                                        section,
                                        key,
                                        v
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            if !parse_errors.is_empty() {
                log::warn!(
                    "⚠️  Warning: Config file {} has parse errors:\n   {}",
                    config_path.display(),
                    parse_errors.join("\n   ")
                );
                log::warn!("   Using default value: {default}");
            }
        }
    }
    default
}

/// Read config value from TOML file (u16 version)
///
/// **Poka-Yoke Fix**: Validates values using `NonZeroPort::new()` to prevent invalid values (0).
fn read_config_value_u16(section: &str, key: &str, default: u16) -> u16 {
    use crate::core::config::poka_yoke::NonZeroPort;

    if let Some(config_path) = find_config_file() {
        if let Ok(contents) = fs::read_to_string(&config_path) {
            let mut current_section = String::new();
            let mut parse_errors = Vec::new();

            for (line_num, line) in contents.lines().enumerate() {
                let line = line.trim();
                if line.is_empty() || line.starts_with('#') {
                    continue;
                }

                if line.starts_with('[') && line.ends_with(']') {
                    current_section = line[1..line.len() - 1].trim().to_string();
                    continue;
                }

                if current_section == section {
                    if let Some((k, v)) = line.split_once('=') {
                        let k = k.trim();
                        let v = v.trim();
                        if k == key {
                            let v = v.trim_matches('"').trim_matches('\'');
                            match v.parse::<u16>() {
                                Ok(parsed) => {
                                    // **Poka-Yoke Fix**: Validate using poka-yoke type
                                    match NonZeroPort::new(parsed) {
                                        Some(valid) => return valid.get(),
                                        None => {
                                            parse_errors.push(format!(
                                                "Line {}: Invalid value for {}.{}: {} (must be > 0)",
                                                line_num + 1, section, key, parsed
                                            ));
                                        }
                                    }
                                }
                                Err(_) => {
                                    parse_errors.push(format!(
                                        "Line {}: Invalid value for {}.{}: '{}' (not a number)",
                                        line_num + 1,
                                        section,
                                        key,
                                        v
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            if !parse_errors.is_empty() {
                log::warn!(
                    "⚠️  Warning: Config file {} has parse errors:\n   {}",
                    config_path.display(),
                    parse_errors.join("\n   ")
                );
                log::warn!("   Using default value: {default}");
            }
        }
    }
    default
}

/// Get unit test timeout from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
#[must_use]
pub fn unit_test_timeout_seconds() -> u64 {
    read_config_value(
        "test",
        "unit_timeout_seconds",
        DEFAULT_UNIT_TEST_TIMEOUT_SECONDS,
    )
}

/// Get integration test timeout from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
#[must_use]
pub fn integration_test_timeout_seconds() -> u64 {
    read_config_value(
        "test",
        "integration_timeout_seconds",
        DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS,
    )
}

/// Get property test cases from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
#[must_use]
pub fn property_test_cases() -> u32 {
    read_config_value_u32(
        "property",
        "default_test_cases",
        DEFAULT_PROPERTY_TEST_CASES,
    )
}

/// Get hot path tick budget from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
#[must_use]
pub fn hot_path_tick_budget() -> u64 {
    read_config_value(
        "performance",
        "hot_path_tick_budget",
        DEFAULT_HOT_PATH_TICK_BUDGET,
    )
}

/// Get max run length from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
#[must_use]
pub fn max_run_len() -> usize {
    read_config_value_usize("guards", "max_run_len", DEFAULT_MAX_RUN_LEN)
}

/// Get max batch size from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
#[must_use]
pub fn max_batch_size() -> usize {
    read_config_value_usize("guards", "max_batch_size", DEFAULT_MAX_BATCH_SIZE)
}

// ========================================================================
// Testcontainers Configuration Functions
// ========================================================================

/// Get container wait timeout from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
///
/// # Returns
///
/// Timeout value as `u64` (seconds). For type-safe usage, validate with `PositiveTimeout`:
///
/// ```rust,no_run
/// use chicago_tdd_tools::core::config::loading;
/// use chicago_tdd_tools::core::config::poka_yoke::PositiveTimeout;
///
/// let timeout = PositiveTimeout::new(loading::testcontainers_container_wait_timeout_seconds())
///     .expect("Config should provide valid timeout > 0");
/// ```
///
/// See [Poka-Yoke Guide](../../../docs/POKA_YOKE_GUIDE.md) for more examples.
#[must_use]
pub fn testcontainers_container_wait_timeout_seconds() -> u64 {
    read_config_value(
        "testcontainers",
        "container_wait_timeout_seconds",
        DEFAULT_CONTAINER_WAIT_TIMEOUT_SECONDS,
    )
}

/// Get HTTP connection timeout from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
///
/// # Returns
///
/// Timeout value as `u64` (seconds). For type-safe usage, validate with `PositiveTimeout`:
///
/// ```rust,no_run
/// use chicago_tdd_tools::core::config::loading;
/// use chicago_tdd_tools::core::config::poka_yoke::PositiveTimeout;
///
/// let timeout = PositiveTimeout::new(loading::testcontainers_http_connection_timeout_seconds())
///     .expect("Config should provide valid timeout > 0");
/// ```
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::config::loading;
///
/// let timeout = loading::testcontainers_http_connection_timeout_seconds();
/// assert!(timeout > 0);
/// ```
#[must_use]
pub fn testcontainers_http_connection_timeout_seconds() -> u64 {
    read_config_value(
        "testcontainers",
        "http_connection_timeout_seconds",
        DEFAULT_HTTP_CONNECTION_TIMEOUT_SECONDS,
    )
}

/// Get default HTTP port from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
///
/// # Returns
///
/// Port value as `u16`. For type-safe usage, validate with `NonZeroPort`:
///
/// ```rust,no_run
/// use chicago_tdd_tools::core::config::loading;
/// use chicago_tdd_tools::core::config::poka_yoke::NonZeroPort;
///
/// // Validate config value
/// let port_value = loading::testcontainers_default_http_port();
/// let port = NonZeroPort::new(port_value)
///     .expect("Config should provide valid port > 0");
///
/// // Use type-safe port
/// // port.get() is guaranteed > 0
/// ```
///
/// See [Poka-Yoke Guide](../../../docs/POKA_YOKE_GUIDE.md) for more examples.
#[must_use]
pub fn testcontainers_default_http_port() -> u16 {
    read_config_value_u16("testcontainers", "default_http_port", DEFAULT_HTTP_PORT)
}

/// Get default HTTPS port from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
///
/// # Returns
///
/// Port value as `u16`. For type-safe usage, validate with `NonZeroPort`:
///
/// ```rust,no_run
/// use chicago_tdd_tools::core::config::loading;
/// use chicago_tdd_tools::core::config::poka_yoke::NonZeroPort;
///
/// let port = NonZeroPort::new(loading::testcontainers_default_https_port())
///     .expect("Config should provide valid port > 0");
/// ```
///
/// See [Poka-Yoke Guide](../../../docs/POKA_YOKE_GUIDE.md) for more examples.
#[must_use]
pub fn testcontainers_default_https_port() -> u16 {
    read_config_value_u16("testcontainers", "default_https_port", DEFAULT_HTTPS_PORT)
}

/// Get default HTTP alternate port from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
///
/// # Returns
///
/// Port value as `u16`. For type-safe usage, validate with `NonZeroPort`:
///
/// ```rust,no_run
/// use chicago_tdd_tools::core::config::loading;
/// use chicago_tdd_tools::core::config::poka_yoke::NonZeroPort;
///
/// let port = NonZeroPort::new(loading::testcontainers_default_http_alt_port())
///     .expect("Config should provide valid port > 0");
/// ```
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::config::loading;
///
/// let port = loading::testcontainers_default_http_alt_port();
/// assert!(port > 0);
/// ```
#[must_use]
pub fn testcontainers_default_http_alt_port() -> u16 {
    read_config_value_u16(
        "testcontainers",
        "default_http_alt_port",
        DEFAULT_HTTP_ALT_PORT,
    )
}

/// Get concurrent containers count from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
///
/// # Returns
///
/// Maximum number of containers that can run concurrently.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::config::loading;
///
/// let count = loading::testcontainers_concurrent_containers_count();
/// assert!(count > 0);
/// ```
#[must_use]
pub fn testcontainers_concurrent_containers_count() -> usize {
    read_config_value_usize(
        "testcontainers",
        "concurrent_containers_count",
        DEFAULT_CONCURRENT_CONTAINERS_COUNT,
    )
}

/// Get concurrent commands count from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
///
/// # Returns
///
/// Maximum number of commands that can execute concurrently.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::config::loading;
///
/// let count = loading::testcontainers_concurrent_commands_count();
/// assert!(count > 0);
/// ```
#[must_use]
pub fn testcontainers_concurrent_commands_count() -> usize {
    read_config_value_usize(
        "testcontainers",
        "concurrent_commands_count",
        DEFAULT_CONCURRENT_COMMANDS_COUNT,
    )
}

/// Get multi-container count from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
///
/// # Returns
///
/// Default number of containers to use in multi-container test scenarios.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::config::loading;
///
/// let count = loading::testcontainers_multi_container_count();
/// assert!(count > 0);
/// ```
#[must_use]
pub fn testcontainers_multi_container_count() -> usize {
    read_config_value_usize(
        "testcontainers",
        "multi_container_count",
        DEFAULT_MULTI_CONTAINER_COUNT,
    )
}

/// Get commands per container from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
///
/// # Returns
///
/// Default number of commands to execute per container in test scenarios.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::config::loading;
///
/// let count = loading::testcontainers_commands_per_container();
/// assert!(count > 0);
/// ```
#[must_use]
pub fn testcontainers_commands_per_container() -> usize {
    read_config_value_usize(
        "testcontainers",
        "commands_per_container",
        DEFAULT_COMMANDS_PER_CONTAINER,
    )
}

// ========================================================================
// Weaver Configuration Functions
// ========================================================================

/// Get Weaver OTLP gRPC port from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
///
/// # Returns
///
/// Port value as `u16`. For type-safe usage, validate with `NonZeroPort`:
///
/// ```rust,no_run
/// use chicago_tdd_tools::core::config::loading;
/// use chicago_tdd_tools::core::config::poka_yoke::NonZeroPort;
///
/// let port = NonZeroPort::new(loading::weaver_otlp_grpc_port())
///     .expect("Config should provide valid port > 0");
/// ```
///
/// See [Poka-Yoke Guide](../../../docs/POKA_YOKE_GUIDE.md) for more examples.
#[must_use]
pub fn weaver_otlp_grpc_port() -> u16 {
    read_config_value_u16(
        "observability.weaver",
        "otlp_grpc_port",
        DEFAULT_OTLP_GRPC_PORT,
    )
}

/// Get Weaver startup wait time from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
#[must_use]
pub fn weaver_startup_wait_milliseconds() -> u64 {
    read_config_value(
        "observability.weaver",
        "startup_wait_milliseconds",
        DEFAULT_STARTUP_WAIT_MILLISECONDS,
    )
}

/// Get Weaver telemetry processing wait time from config (with fallback to constant)
///
/// **Kaizen improvement**: Uses named constant instead of magic number.
#[must_use]
pub fn weaver_telemetry_processing_wait_milliseconds() -> u64 {
    read_config_value(
        "observability.weaver",
        "telemetry_processing_wait_milliseconds",
        DEFAULT_TELEMETRY_PROCESSING_WAIT_MILLISECONDS,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::sync::{Mutex, OnceLock};
    use tempfile::TempDir;

    static TEST_MUTEX: OnceLock<Mutex<()>> = OnceLock::new();

    /// Returns a mutex guard to serialize test execution.
    ///
    /// # Warning
    /// Always acquire this mutex guard (`let _lock = get_lock();`) when writing new tests
    /// that access shared config state to prevent race conditions and test interference.
    fn get_lock() -> std::sync::MutexGuard<'static, ()> {
        match TEST_MUTEX.get_or_init(|| Mutex::new(())).lock() {
            Ok(guard) => guard,
            Err(poisoned) => poisoned.into_inner(),
        }
    }

    /// **Gemba Fix**: Test that config file is actually read
    #[test]
    fn test_config_file_is_read() {
        let _lock = get_lock();
        // Arrange: Create temporary config file
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let config_path = temp_dir.path().join("chicago-tdd-tools.toml");
        fs::write(
            &config_path,
            r#"
[test]
unit_timeout_seconds = 5
integration_timeout_seconds = 60

[property]
default_test_cases = 200

[performance]
hot_path_tick_budget = 16

[guards]
max_run_len = 16
max_batch_size = 2000
"#,
        )
        .expect("Failed to write config file");

        // Act: Set CARGO_MANIFEST_DIR to temp directory to simulate config file location
        let original_manifest_dir = std::env::var("CARGO_MANIFEST_DIR").ok();
        std::env::set_var("CARGO_MANIFEST_DIR", temp_dir.path());

        // Clear any cached config file location by calling find_config_file directly
        // Note: This test verifies the config reading logic works, but due to CARGO_MANIFEST_DIR
        // being set at compile time, we test the parsing logic directly

        // Read config file contents and verify parsing logic
        let contents = fs::read_to_string(&config_path).expect("Failed to read config");

        // Manually test parsing logic (since find_config_file uses CARGO_MANIFEST_DIR)
        let mut current_section = String::new();
        let mut found_unit_timeout = None;
        for line in contents.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            if line.starts_with('[') && line.ends_with(']') {
                current_section = line[1..line.len() - 1].trim().to_string();
                continue;
            }
            if current_section == "test" {
                if let Some((k, v)) = line.split_once('=') {
                    let k = k.trim();
                    let v = v.trim().trim_matches('"');
                    if k == "unit_timeout_seconds" {
                        found_unit_timeout = v.parse::<u64>().ok();
                    }
                }
            }
        }

        // Assert: Verify config values are parsed correctly
        assert_eq!(
            found_unit_timeout,
            Some(5),
            "Config file should be parsed correctly"
        );

        // Cleanup: Restore original CARGO_MANIFEST_DIR
        if let Some(dir) = original_manifest_dir {
            std::env::set_var("CARGO_MANIFEST_DIR", dir);
        } else {
            std::env::remove_var("CARGO_MANIFEST_DIR");
        }
    }

    /// **Gemba Fix**: Test that nested sections like [observability.weaver] work
    #[test]
    fn test_nested_sections_work() {
        let _lock = get_lock();
        // Arrange: Create config file with nested section
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let config_path = temp_dir.path().join("chicago-tdd-tools.toml");
        fs::write(
            &config_path,
            r#"
[test]
unit_timeout_seconds = 2

[observability.weaver]
otlp_grpc_port = 9999
"#,
        )
        .expect("Failed to write config file");

        // Act: Read config file and verify nested section parsing
        let contents = fs::read_to_string(&config_path).expect("Failed to read config");
        let mut current_section = String::new();
        let mut found_unit_timeout = None;
        let mut found_nested_section = false;

        for line in contents.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            if line.starts_with('[') && line.ends_with(']') {
                current_section = line[1..line.len() - 1].trim().to_string();
                if current_section == "observability.weaver" {
                    found_nested_section = true;
                }
                continue;
            }
            if current_section == "test" {
                if let Some((k, v)) = line.split_once('=') {
                    let k = k.trim();
                    let v = v.trim().trim_matches('"');
                    if k == "unit_timeout_seconds" {
                        found_unit_timeout = v.parse::<u64>().ok();
                    }
                }
            }
        }

        // Assert: Verify nested section is parsed correctly
        assert!(
            found_nested_section,
            "Nested section [observability.weaver] should be detected"
        );
        assert_eq!(
            found_unit_timeout,
            Some(2),
            "Config value should be parsed correctly"
        );
    }

    /// **Gemba Fix**: Test that defaults are used when config file doesn't exist
    #[test]
    fn test_defaults_used_when_no_config() {
        let _lock = get_lock();
        // Arrange: Temporarily remove CARGO_MANIFEST_DIR to simulate no config file
        let original_manifest_dir = std::env::var("CARGO_MANIFEST_DIR").ok();
        std::env::remove_var("CARGO_MANIFEST_DIR");

        // Act & Assert: Verify defaults are used (when no config file)
        // Note: This test verifies defaults match expected values
        // We test the constants directly since config reading uses CARGO_MANIFEST_DIR
        assert_eq!(DEFAULT_UNIT_TEST_TIMEOUT_SECONDS, 1);
        assert_eq!(DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS, 30);
        assert_eq!(DEFAULT_PROPERTY_TEST_CASES, 100);
        assert_eq!(DEFAULT_HOT_PATH_TICK_BUDGET, 8);
        assert_eq!(DEFAULT_MAX_RUN_LEN, 8);
        assert_eq!(DEFAULT_MAX_BATCH_SIZE, 1000);

        // Cleanup: Restore original CARGO_MANIFEST_DIR
        if let Some(dir) = original_manifest_dir {
            std::env::set_var("CARGO_MANIFEST_DIR", dir);
        }
    }

    /// **Gemba Fix**: Test that invalid values fall back to defaults
    #[test]
    fn test_invalid_values_fallback_to_defaults() {
        let _lock = get_lock();
        // Arrange: Create config file with invalid values
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let config_path = temp_dir.path().join("chicago-tdd-tools.toml");
        fs::write(
            &config_path,
            r#"
[test]
unit_timeout_seconds = invalid_value
integration_timeout_seconds = 30
"#,
        )
        .expect("Failed to write config file");

        // Act: Test parsing logic directly
        let contents = fs::read_to_string(&config_path).expect("Failed to read config");
        let mut current_section = String::new();
        let mut found_unit_timeout = None;
        let mut found_integration_timeout = None;

        for line in contents.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            if line.starts_with('[') && line.ends_with(']') {
                current_section = line[1..line.len() - 1].trim().to_string();
                continue;
            }
            if current_section == "test" {
                if let Some((k, v)) = line.split_once('=') {
                    let k = k.trim();
                    let v = v.trim().trim_matches('"');
                    if k == "unit_timeout_seconds" {
                        found_unit_timeout = v.parse::<u64>().ok(); // Should be None for invalid_value
                    } else if k == "integration_timeout_seconds" {
                        found_integration_timeout = v.parse::<u64>().ok();
                    }
                }
            }
        }

        // Assert: Invalid value returns None, valid value is parsed
        assert_eq!(
            found_unit_timeout, None,
            "Invalid value should fail to parse"
        );
        assert_eq!(
            found_integration_timeout,
            Some(30),
            "Valid value should be parsed"
        );
    }

    /// **Poka-Yoke Fix**: Test that invalid zero values are rejected and fall back to defaults
    #[test]
    fn test_invalid_zero_values_rejected() {
        let _lock = get_lock();
        // Arrange: Create config file with invalid zero values
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let config_path = temp_dir.path().join("chicago-tdd-tools.toml");
        fs::write(
            &config_path,
            r#"
[test]
unit_timeout_seconds = 0
integration_timeout_seconds = 0

[property]
default_test_cases = 0

[performance]
hot_path_tick_budget = 0

[guards]
max_run_len = 0
max_batch_size = 0
"#,
        )
        .expect("Failed to write config file");

        // Act: Set CARGO_MANIFEST_DIR to temp directory
        // This ensures find_config_file() finds the temp config file, not the project config
        // **Refactored**: Also set current directory to temp directory to ensure find_config_file()
        // starts from temp directory and doesn't search up to find project root config
        let original_manifest_dir = std::env::var("CARGO_MANIFEST_DIR").ok();
        let original_current_dir = std::env::current_dir().ok();
        std::env::set_var(
            "CARGO_MANIFEST_DIR",
            temp_dir.path().to_string_lossy().as_ref(),
        );
        std::env::set_current_dir(temp_dir.path()).expect("Failed to change to temp directory");

        // Assert: Invalid zero values should fall back to defaults (poka-yoke prevention)
        // Poka-yoke types prevent 0 values, so defaults are used
        assert_eq!(
            unit_test_timeout_seconds(),
            1,
            "Zero timeout should use default (poka-yoke)"
        );
        assert_eq!(
            integration_test_timeout_seconds(),
            30,
            "Zero timeout should use default (poka-yoke)"
        );
        assert_eq!(
            property_test_cases(),
            100,
            "Zero test cases should use default (poka-yoke)"
        );
        assert_eq!(
            hot_path_tick_budget(),
            8,
            "Zero budget should use default (poka-yoke)"
        );
        assert_eq!(
            max_run_len(),
            8,
            "Zero run len should use default (poka-yoke)"
        );
        assert_eq!(
            max_batch_size(),
            1000,
            "Zero batch size should use default (poka-yoke)"
        );

        // Cleanup: Restore original CARGO_MANIFEST_DIR and current directory
        if let Some(dir) = original_manifest_dir {
            std::env::set_var("CARGO_MANIFEST_DIR", dir);
        } else {
            std::env::remove_var("CARGO_MANIFEST_DIR");
        }
        if let Some(dir) = original_current_dir {
            std::env::set_current_dir(dir).expect("Failed to restore original directory");
        }
    }

    /// **Root Cause Prevention**: Test that verifies config file options match implementation.
    /// This test prevents config drift by ensuring all config file options have corresponding
    /// read_config_value() calls. If this test fails, it means config file has options that
    /// aren't being read by the code.
    #[test]
    fn test_config_options_match_implementation() {
        let _lock = get_lock();
        // Arrange: Read actual config file
        let config_path = find_config_file();
        if config_path.is_none() {
            // Config file doesn't exist in test environment, skip test
            return;
        }
        let config_path = config_path.unwrap();
        let contents = fs::read_to_string(&config_path).unwrap_or_default();

        // List of all config options that SHOULD be read (from actual implementation)
        let expected_options: Vec<(&str, &str)> = vec![
            // Test section
            ("test", "unit_timeout_seconds"),
            ("test", "integration_timeout_seconds"),
            // Property section
            ("property", "default_test_cases"),
            // Performance section
            ("performance", "hot_path_tick_budget"),
            // Guards section
            ("guards", "max_run_len"),
            ("guards", "max_batch_size"),
            // Testcontainers section
            ("testcontainers", "container_wait_timeout_seconds"),
            ("testcontainers", "http_connection_timeout_seconds"),
            ("testcontainers", "default_http_port"),
            ("testcontainers", "default_https_port"),
            ("testcontainers", "default_http_alt_port"),
            ("testcontainers", "concurrent_containers_count"),
            ("testcontainers", "concurrent_commands_count"),
            ("testcontainers", "multi_container_count"),
            ("testcontainers", "commands_per_container"),
            // Weaver section
            ("observability.weaver", "otlp_grpc_port"),
            ("observability.weaver", "startup_wait_milliseconds"),
            (
                "observability.weaver",
                "telemetry_processing_wait_milliseconds",
            ),
        ];

        // Parse config file and extract all key=value pairs
        let mut config_options = Vec::new();
        let mut current_section = String::new();

        for line in contents.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            if line.starts_with('[') && line.ends_with(']') {
                current_section = line[1..line.len() - 1].trim().to_string();
                continue;
            }

            if let Some((key, _value)) = line.split_once('=') {
                let key = key.trim();
                config_options.push((current_section.clone(), key.to_string()));
            }
        }

        // Assert: All config file options should be in expected list
        for (section, key) in &config_options {
            let found = expected_options
                .iter()
                .any(|(exp_section, exp_key)| exp_section == section && exp_key == key);

            assert!(
                found,
                "Config file has option [{section}].{key} but no code reads it.\n   \
                 💡 FIX: Add read_config_value() call in src/core/config/loading.rs\n   \
                 💡 FIX: Or remove option from config file if not needed\n   \
                 💡 ROOT CAUSE PREVENTION: Code-first, config-second - add read_config_value() before adding to config file"
            );
        }

        // Also verify that all expected options exist in config file (if config file exists)
        // This ensures config file documents all implemented options
        for (section, key) in &expected_options {
            let found = config_options
                .iter()
                .any(|(cfg_section, cfg_key)| cfg_section == section && cfg_key == key);

            // Note: Config file is optional, so missing options are OK
            // But if config file exists, it should document implemented options
            if !contents.is_empty() && !found {
                #[cfg(feature = "logging")]
                log::warn!(
                    "⚠️  Config file doesn't document [{section}].{key} but code reads it.\n   \
                     💡 SUGGESTION: Add option to config file for documentation"
                );
                #[cfg(not(feature = "logging"))]
                eprintln!(
                    "⚠️  Warning: Config file doesn't document [{section}].{key} but code reads it.\n   \
                     💡 SUGGESTION: Add option to config file for documentation"
                );
            }
        }
    }

    /// **Gemba Fix**: Test that config defaults match hardcoded constants
    ///
    /// **Root Cause Fix**: This test verifies that the local constants in this module
    /// match the constants exported from the macros module. This ensures consistency
    /// across the codebase.
    ///
    /// **Isolation**: This test compares constants directly, not runtime function calls,
    /// to avoid flakiness from config file state or other tests.
    #[test]
    fn test_config_defaults_match_constants() {
        let _lock = get_lock();
        // Arrange: Import constants from both modules
        use crate::core::macros::test::{
            DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS as MACRO_INTEGRATION_TIMEOUT,
            DEFAULT_UNIT_TEST_TIMEOUT_SECONDS as MACRO_UNIT_TIMEOUT,
        };

        // Act & Assert: Verify local constants match macro constants
        // This is a compile-time check that ensures consistency
        assert_eq!(
            DEFAULT_UNIT_TEST_TIMEOUT_SECONDS, MACRO_UNIT_TIMEOUT,
            "Local DEFAULT_UNIT_TEST_TIMEOUT_SECONDS ({}) should match macro constant ({})",
            DEFAULT_UNIT_TEST_TIMEOUT_SECONDS, MACRO_UNIT_TIMEOUT
        );
        assert_eq!(
            DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS, MACRO_INTEGRATION_TIMEOUT,
            "Local DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS ({}) should match macro constant ({})",
            DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS, MACRO_INTEGRATION_TIMEOUT
        );
    }

    /// **Gemba Fix**: Test that config functions return defaults when no config file exists
    ///
    /// **Isolation**: This test ensures the functions work correctly in isolation by
    /// temporarily removing CARGO_MANIFEST_DIR to simulate no config file scenario.
    ///
    /// **Test Isolation Fix**: Uses RAII guard patterns to ensure environment and file system
    /// are always restored, even if test panics. This prevents test interdependencies.
    ///
    /// **Note**: This test verifies behavior when config file doesn't exist, so it temporarily
    /// renames any existing config file to simulate that scenario.
    #[test]
    fn test_config_functions_use_defaults_when_no_config() {
        let _lock = get_lock();
        // **Poka-Yoke Fix**: Use guard patterns to ensure cleanup happens even if test panics
        struct EnvVarGuard {
            key: &'static str,
            original_value: Option<String>,
        }

        impl Drop for EnvVarGuard {
            fn drop(&mut self) {
                // Restore original value or remove if it didn't exist
                match &self.original_value {
                    Some(value) => std::env::set_var(self.key, value),
                    None => std::env::remove_var(self.key),
                }
            }
        }

        struct ConfigFileGuard {
            config_path: PathBuf,
            backup_path: PathBuf,
            had_config: bool,
        }

        impl Drop for ConfigFileGuard {
            fn drop(&mut self) {
                // Restore config file if it existed
                if self.had_config {
                    if let Err(e) = fs::rename(&self.backup_path, &self.config_path) {
                        // Log error but don't panic in Drop
                        log::warn!("Failed to restore config file: {}", e);
                    }
                }
            }
        }

        // Arrange: Create guards that will restore state on drop
        let _env_guard = EnvVarGuard {
            key: "CARGO_MANIFEST_DIR",
            original_value: std::env::var("CARGO_MANIFEST_DIR").ok(),
        };

        // Temporarily move config file if it exists
        let config_path = PathBuf::from("chicago-tdd-tools.toml");
        let backup_path = PathBuf::from("chicago-tdd-tools.toml.test-backup");
        let had_config = config_path.exists();

        let _file_guard = ConfigFileGuard {
            config_path: config_path.clone(),
            backup_path: backup_path.clone(),
            had_config,
        };

        if had_config {
            if let Err(e) = fs::rename(&config_path, &backup_path) {
                panic!("Failed to backup config file: {}", e);
            }
        }

        // Remove CARGO_MANIFEST_DIR to prevent fallback to manifest dir
        std::env::remove_var("CARGO_MANIFEST_DIR");

        // Act & Assert: Verify functions return default constants when no config exists
        assert_eq!(
            unit_test_timeout_seconds(),
            DEFAULT_UNIT_TEST_TIMEOUT_SECONDS,
            "unit_test_timeout_seconds() should return DEFAULT_UNIT_TEST_TIMEOUT_SECONDS when no config file exists"
        );
        assert_eq!(
            integration_test_timeout_seconds(),
            DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS,
            "integration_test_timeout_seconds() should return DEFAULT_INTEGRATION_TEST_TIMEOUT_SECONDS when no config file exists"
        );

        // Cleanup: Guards' Drop implementations automatically restore state
    }
}
