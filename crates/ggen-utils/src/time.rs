//! Time utilities for ggen
//!
//! This module provides utilities for working with timestamps and durations,
//! including Unix timestamp generation and human-readable duration formatting.
//!
//! ## Features
//!
//! - **Unix timestamps**: Get current Unix timestamp
//! - **Duration formatting**: Format durations in human-readable format
//! - **Time calculations**: Helper functions for time-based operations
//!
//! ## Examples
//!
//! ### Getting Current Timestamp
//!
//! ```rust
//! use ggen_utils::time::current_timestamp;
//!
//! # fn main() {
//! let timestamp = current_timestamp();
//! println!("Current Unix timestamp: {}", timestamp);
//! # }
//! ```
//!
//! ### Formatting Durations
//!
//! ```rust
//! use ggen_utils::time::format_duration;
//!
//! # fn main() {
//! assert_eq!(format_duration(30), "30s");
//! assert_eq!(format_duration(90), "1m 30s");
//! assert_eq!(format_duration(3661), "1h 1m 1s");
//! # }
//! ```

use std::time::{SystemTime, UNIX_EPOCH};

/// Get the current Unix timestamp.
///
/// Returns 0 if system time is before Unix epoch (should never happen on properly configured systems).
#[must_use]
pub fn current_timestamp() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// Format a duration in seconds to a human-readable string.
#[must_use]
pub fn format_duration(secs: u64) -> String {
    if secs < 60 {
        format!("{secs}s")
    } else if secs < 3600 {
        format!("{}m {}s", secs / 60, secs % 60)
    } else {
        format!("{}h {}m {}s", secs / 3600, (secs % 3600) / 60, secs % 60)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_current_timestamp() {
        let timestamp = current_timestamp();
        assert!(timestamp > 0);

        // Should be reasonable (after year 2020)
        assert!(timestamp > 1_577_836_800); // Jan 1, 2020
    }

    #[test]
    fn test_format_duration_seconds() {
        assert_eq!(format_duration(0), "0s");
        assert_eq!(format_duration(30), "30s");
        assert_eq!(format_duration(59), "59s");
    }

    #[test]
    fn test_format_duration_minutes() {
        assert_eq!(format_duration(60), "1m 0s");
        assert_eq!(format_duration(90), "1m 30s");
        assert_eq!(format_duration(125), "2m 5s");
        assert_eq!(format_duration(3599), "59m 59s");
    }

    #[test]
    fn test_format_duration_hours() {
        assert_eq!(format_duration(3600), "1h 0m 0s");
        assert_eq!(format_duration(3661), "1h 1m 1s");
        assert_eq!(format_duration(7325), "2h 2m 5s");
    }
}
