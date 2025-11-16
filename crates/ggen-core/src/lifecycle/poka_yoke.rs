//! Poka-yoke (error prevention) types for lifecycle system
//!
//! This module provides type-level error prevention patterns that make certain
//! errors impossible at compile time.
//!
//! # Poka-Yoke Principle
//!
//! "Make invalid states unrepresentable" - Use the type system to prevent entire
//! classes of errors rather than detecting them at runtime.

use super::error::Result;
use std::path::{Path, PathBuf};

// ============================================================================
// NON-EMPTY PATH TYPE
// ============================================================================

/// Path that is guaranteed to be non-empty at type level
///
/// **Poka-yoke**: Type system prevents empty paths. Cannot construct a
/// `NonEmptyPath` from an empty string - the constructor returns `Err`.
///
/// # Invariants
/// - Inner path is never empty (enforced at construction)
/// - Type system prevents using empty paths where non-empty required
///
/// # Example
///
/// ```rust
/// use ggen_core::lifecycle::poka_yoke::NonEmptyPath;
/// use std::path::PathBuf;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// // ✅ Valid: Non-empty path
/// let path = NonEmptyPath::new(PathBuf::from("output.rs"))?;
/// assert_eq!(path.as_path().to_str(), Some("output.rs"));
///
/// // ❌ Invalid: Empty path rejected at construction
/// let empty = NonEmptyPath::new(PathBuf::from(""));
/// assert!(empty.is_err());
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NonEmptyPath(PathBuf);

/// Error type for empty path construction
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyPathError;

impl std::fmt::Display for EmptyPathError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Path cannot be empty")
    }
}

impl std::error::Error for EmptyPathError {}

impl NonEmptyPath {
    /// Create a new non-empty path
    ///
    /// Returns `Err(EmptyPathError)` if path is empty.
    ///
    /// **Poka-yoke**: Empty paths rejected at construction - cannot create invalid state.
    pub fn new(path: PathBuf) -> std::result::Result<Self, EmptyPathError> {
        if path.as_os_str().is_empty() {
            Err(EmptyPathError)
        } else {
            Ok(Self(path))
        }
    }

    /// Create from string (convenience method)
    ///
    /// Note: This is a convenience method. For trait-based parsing, use `FromStr::from_str`.
    pub fn from_string(s: &str) -> std::result::Result<Self, EmptyPathError> {
        if s.is_empty() {
            Err(EmptyPathError)
        } else {
            Ok(Self(PathBuf::from(s)))
        }
    }

    /// Get the inner path
    pub fn as_path(&self) -> &Path {
        &self.0
    }

    /// Get the inner PathBuf (consuming)
    pub fn into_path_buf(self) -> PathBuf {
        self.0
    }

    /// Join with another path component
    ///
    /// **Poka-yoke**: Result is guaranteed non-empty (base path is non-empty)
    pub fn join(&self, path: impl AsRef<Path>) -> NonEmptyPath {
        // Safety: Base path is non-empty, so result is non-empty
        NonEmptyPath(self.0.join(path))
    }
}

impl std::str::FromStr for NonEmptyPath {
    type Err = EmptyPathError;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        if s.is_empty() {
            Err(EmptyPathError)
        } else {
            Ok(Self(PathBuf::from(s)))
        }
    }
}

impl AsRef<Path> for NonEmptyPath {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl std::fmt::Display for NonEmptyPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.display())
    }
}

// ============================================================================
// NON-EMPTY STRING TYPE
// ============================================================================

/// String that is guaranteed to be non-empty at type level
///
/// **Poka-yoke**: Type system prevents empty strings. Cannot construct a
/// `NonEmptyString` from an empty string.
///
/// # Invariants
/// - Inner string is never empty (enforced at construction)
/// - Type system prevents using empty strings where non-empty required
///
/// # Example
///
/// ```rust
/// use ggen_core::lifecycle::poka_yoke::NonEmptyString;
///
/// // ✅ Valid: Non-empty string
/// let s = NonEmptyString::new("hello".to_string()).unwrap();
/// assert_eq!(s.as_str(), "hello");
///
/// // ❌ Invalid: Empty string rejected at construction
/// let empty = NonEmptyString::new("".to_string());
/// assert!(empty.is_err());
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NonEmptyString(String);

/// Error type for empty string construction
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EmptyStringError;

impl std::fmt::Display for EmptyStringError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "String cannot be empty")
    }
}

impl std::error::Error for EmptyStringError {}

impl NonEmptyString {
    /// Create a new non-empty string
    ///
    /// Returns `Err(EmptyStringError)` if string is empty.
    ///
    /// **Poka-yoke**: Empty strings rejected at construction.
    pub fn new(s: String) -> std::result::Result<Self, EmptyStringError> {
        if s.is_empty() {
            Err(EmptyStringError)
        } else {
            Ok(Self(s))
        }
    }

    /// Get the inner string
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Get the inner String (consuming)
    pub fn into_string(self) -> String {
        self.0
    }
}

impl AsRef<str> for NonEmptyString {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for NonEmptyString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

// ============================================================================
// BOUNDED INTEGER TYPES
// ============================================================================

/// Unsigned counter that uses saturating arithmetic
///
/// **Poka-yoke**: Cannot be negative (uses `u32`), cannot overflow (saturating arithmetic).
///
/// # Invariants
/// - Value is always >= 0 (enforced by type)
/// - Increment/decrement use saturating arithmetic (overflow/underflow prevented)
///
/// # Example
///
/// ```rust
/// use ggen_core::lifecycle::poka_yoke::Counter;
///
/// let mut counter = Counter::new(0);
/// counter.increment();  // Safe - cannot overflow
/// assert_eq!(counter.get(), 1);
///
/// counter.decrement();  // Safe - cannot underflow (saturates at 0)
/// assert_eq!(counter.get(), 0);
///
/// counter.decrement();  // Saturates at 0
/// assert_eq!(counter.get(), 0);
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Counter {
    value: u32, // Poka-yoke: u32 prevents negative values
}

impl Counter {
    /// Create a new counter with initial value
    ///
    /// **Poka-yoke**: Takes `u32`, cannot be negative.
    pub fn new(value: u32) -> Self {
        Self { value }
    }

    /// Increment counter (saturating)
    ///
    /// **Poka-yoke**: Saturating add prevents overflow.
    pub fn increment(&mut self) {
        self.value = self.value.saturating_add(1);
    }

    /// Decrement counter (saturating)
    ///
    /// **Poka-yoke**: Saturating sub prevents underflow.
    pub fn decrement(&mut self) {
        self.value = self.value.saturating_sub(1);
    }

    /// Add to counter (saturating)
    pub fn add(&mut self, n: u32) {
        self.value = self.value.saturating_add(n);
    }

    /// Subtract from counter (saturating)
    pub fn sub(&mut self, n: u32) {
        self.value = self.value.saturating_sub(n);
    }

    /// Get current value
    pub fn get(&self) -> u32 {
        self.value
    }

    /// Reset counter to zero
    pub fn reset(&mut self) {
        self.value = 0;
    }
}

impl Default for Counter {
    fn default() -> Self {
        Self::new(0)
    }
}

// ============================================================================
// TYPE-LEVEL FILE HANDLE STATES (OPTIONAL ENHANCEMENT)
// ============================================================================

use std::marker::PhantomData;

/// File handle in Open state
pub struct Open;

/// File handle in Closed state
pub struct Closed;

/// Type-level file handle that prevents use-after-close errors
///
/// **Poka-yoke**: Type system prevents reading from closed files.
/// Methods like `read()` only exist on `FileHandle<Open>`, not `FileHandle<Closed>`.
///
/// # Example
///
/// ```no_run
/// use ggen_core::lifecycle::poka_yoke::{FileHandle, Open};
/// use std::path::Path;
///
/// // Open file (type: FileHandle<Open>)
/// let mut file = FileHandle::<Open>::open(Path::new("test.txt")).unwrap();
///
/// // Can read when open
/// let data = file.read().unwrap();
///
/// // Close file (consumes FileHandle<Open>, returns FileHandle<Closed>)
/// let closed = file.close();
///
/// // ❌ Compile error: cannot read from closed file
/// // closed.read(); // Method doesn't exist on FileHandle<Closed>
/// ```
#[derive(Debug)]
pub struct FileHandle<State> {
    file: std::fs::File,
    _state: PhantomData<State>,
}

impl FileHandle<Open> {
    /// Open a file in read mode
    ///
    /// Returns `FileHandle<Open>` - type system knows file is open.
    pub fn open(path: &Path) -> Result<Self> {
        let file = std::fs::File::open(path)?;
        Ok(Self {
            file,
            _state: PhantomData,
        })
    }

    /// Read entire file contents
    ///
    /// **Poka-yoke**: Only available on `FileHandle<Open>` - cannot call on closed file.
    pub fn read(&mut self) -> Result<Vec<u8>> {
        use std::io::Read;
        let mut buffer = Vec::new();
        self.file.read_to_end(&mut buffer)?;
        Ok(buffer)
    }

    /// Close the file
    ///
    /// **Poka-yoke**: Consumes `FileHandle<Open>`, returns `FileHandle<Closed>`.
    /// After calling this, cannot call `read()` - type system prevents it.
    pub fn close(self) -> FileHandle<Closed> {
        FileHandle {
            file: self.file,
            _state: PhantomData,
        }
    }
}

impl FileHandle<Closed> {
    // No methods available - cannot read from closed file!
    // **Poka-yoke**: Compiler prevents calling `read()` on closed file.
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;

    test!(test_non_empty_path_valid, {
        let path = NonEmptyPath::new(PathBuf::from("test.txt")).unwrap();
        assert_eq!(path.as_path(), Path::new("test.txt"));
    });

    test!(test_non_empty_path_empty_rejected, {
        let empty = NonEmptyPath::new(PathBuf::from(""));
        assert!(empty.is_err());
        assert_eq!(empty.unwrap_err(), EmptyPathError);
    });

    test!(test_non_empty_path_join, {
        let base: NonEmptyPath = "base".parse().unwrap();
        let joined = base.join("file.txt");
        assert!(joined.as_path().to_str().unwrap().contains("base"));
        assert!(joined.as_path().to_str().unwrap().contains("file.txt"));
    });

    test!(test_non_empty_string_valid, {
        let s = NonEmptyString::new("hello".to_string()).unwrap();
        assert_eq!(s.as_str(), "hello");
    });

    test!(test_non_empty_string_empty_rejected, {
        let empty = NonEmptyString::new("".to_string());
        assert!(empty.is_err());
        assert_eq!(empty.unwrap_err(), EmptyStringError);
    });

    test!(test_counter_cannot_be_negative, {
        let mut counter = Counter::new(0);
        counter.decrement(); // Saturates at 0
        assert_eq!(counter.get(), 0);

        counter.decrement(); // Still 0
        assert_eq!(counter.get(), 0);
    });

    test!(test_counter_cannot_overflow, {
        let mut counter = Counter::new(u32::MAX);
        counter.increment(); // Saturates at MAX
        assert_eq!(counter.get(), u32::MAX);
    });

    test!(test_counter_normal_operations, {
        let mut counter = Counter::new(5);
        counter.increment();
        assert_eq!(counter.get(), 6);

        counter.decrement();
        assert_eq!(counter.get(), 5);

        counter.add(10);
        assert_eq!(counter.get(), 15);

        counter.sub(5);
        assert_eq!(counter.get(), 10);

        counter.reset();
        assert_eq!(counter.get(), 0);
    });

    test!(test_file_handle_type_safety, {
        // This test demonstrates compile-time safety (cannot actually run without files)
        // The important part is that this compiles and shows correct types

        // Type annotations show type-level state
        let _open_handle: Result<FileHandle<Open>> = FileHandle::open(Path::new("nonexistent"));

        // If we had an open handle, we could:
        // let mut open = open_handle.unwrap();
        // let _data = open.read(); // ✅ Valid - file is open

        // After closing:
        // let closed = open.close();
        // closed.read(); // ❌ Compile error - method doesn't exist
    });

    // Compile-time error test (uncomment to verify it fails to compile)
    // #[test]
    // fn test_invalid_transition_compile_error() {
    //     use tempfile::TempDir;
    //     let temp = TempDir::new().unwrap();
    //     let path = temp.path().join("test.txt");
    //     std::fs::write(&path, b"test").unwrap();
    //
    //     let file = FileHandle::<Open>::open(&path).unwrap();
    //     let closed = file.close();
    //     closed.read(); // ❌ Should fail to compile: method `read` not found
    // }
}
