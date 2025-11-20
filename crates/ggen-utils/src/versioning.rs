// API Versioning Helpers
// Provides compile-time deprecation warnings and migration guidance

/// Deprecation macro with version tracking and migration notes
///
/// # Example
/// ```
/// use ggen_utils::deprecated_since;
///
/// #[deprecated_since!("1.0.0", "Use new_api() instead. See migration guide at docs/MIGRATION.md")]
/// pub fn old_api() -> Result<String, Error> {
///     // Legacy implementation
///     Ok("deprecated".to_string())
/// }
/// ```
#[macro_export]
macro_rules! deprecated_since {
    ($version:expr, $note:expr) => {
        #[deprecated(since = $version, note = $note)]
    };
}

/// Marks a feature as experimental with version tracking
///
/// # Example
/// ```
/// use ggen_utils::experimental;
///
/// #[experimental!("1.0.0", "This API may change in future versions")]
/// pub fn experimental_feature() -> Result<(), Error> {
///     // Experimental implementation
///     Ok(())
/// }
/// ```
#[macro_export]
macro_rules! experimental {
    ($since:expr, $note:expr) => {
        #[doc = concat!("⚠️ **Experimental** (since ", $since, "): ", $note)]
    };
}

/// Marks a breaking change with migration path
///
/// # Example
/// ```
/// use ggen_utils::breaking_change;
///
/// #[breaking_change!("2.0.0", "Signature changed. Old: fn(i32) -> i32, New: fn(i64) -> Result<i64, Error>")]
/// pub fn updated_api(value: i64) -> Result<i64, Error> {
///     Ok(value * 2)
/// }
/// ```
#[macro_export]
macro_rules! breaking_change {
    ($version:expr, $change:expr) => {
        #[doc = concat!("⚠️ **Breaking Change in ", $version, "**: ", $change)]
    };
}

/// Version compatibility checker
pub struct VersionChecker;

impl VersionChecker {
    /// Checks if a version is compatible with the current API
    ///
    /// # Example
    /// ```
    /// use ggen_utils::versioning::VersionChecker;
    ///
    /// let compatible = VersionChecker::is_compatible("1.0.0", "1.2.0");
    /// assert!(compatible.unwrap());
    /// ```
    pub fn is_compatible(requested: &str, current: &str) -> Result<bool, String> {
        let req_parts = parse_version(requested)?;
        let cur_parts = parse_version(current)?;

        // SemVer compatibility: same major version, current >= requested
        Ok(req_parts.0 == cur_parts.0
            && (cur_parts.1 > req_parts.1
                || (cur_parts.1 == req_parts.1 && cur_parts.2 >= req_parts.2)))
    }
}

fn parse_version(version: &str) -> Result<(u32, u32, u32), String> {
    let parts: Vec<&str> = version.split('.').collect();

    if parts.len() != 3 {
        return Err(format!("Invalid version format: {}", version));
    }

    let major = parts[0]
        .parse::<u32>()
        .map_err(|_| format!("Invalid major version: {}", parts[0]))?;
    let minor = parts[1]
        .parse::<u32>()
        .map_err(|_| format!("Invalid minor version: {}", parts[1]))?;
    let patch = parts[2]
        .parse::<u32>()
        .map_err(|_| format!("Invalid patch version: {}", parts[2]))?;

    Ok((major, minor, patch))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version_compatibility_same_version() {
        // Arrange
        let requested = "1.0.0";
        let current = "1.0.0";

        // Act
        let result = VersionChecker::is_compatible(requested, current);

        // Assert
        assert!(result.unwrap());
    }

    #[test]
    fn test_version_compatibility_minor_upgrade() {
        // Arrange
        let requested = "1.0.0";
        let current = "1.2.0";

        // Act
        let result = VersionChecker::is_compatible(requested, current);

        // Assert
        assert!(result.unwrap());
    }

    #[test]
    fn test_version_compatibility_major_incompatible() {
        // Arrange
        let requested = "1.0.0";
        let current = "2.0.0";

        // Act
        let result = VersionChecker::is_compatible(requested, current);

        // Assert
        assert!(!result.unwrap());
    }

    #[test]
    fn test_version_parsing_invalid_format() {
        // Arrange
        let invalid = "1.0";

        // Act
        let result = parse_version(invalid);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Invalid version format"));
    }
}
