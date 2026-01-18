use std::fmt;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct ProtectedPath(String);

#[derive(Debug, Clone)]
pub enum PathError {
    InvalidPath(String),
    ContainsDotDot,
    ContainsNullByte,
    Empty,
}

impl fmt::Display for PathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathError::InvalidPath(reason) => write!(f, "Invalid path: {}", reason),
            PathError::ContainsDotDot => write!(f, "Path traversal detected: contains '..'"),
            PathError::ContainsNullByte => write!(f, "Path contains null byte"),
            PathError::Empty => write!(f, "Path cannot be empty"),
        }
    }
}

impl std::error::Error for PathError {}

impl ProtectedPath {
    pub fn new(path: &str) -> Result<Self, PathError> {
        if path.is_empty() {
            return Err(PathError::Empty);
        }

        if path.contains('\0') {
            return Err(PathError::ContainsNullByte);
        }

        if path.contains("..") {
            return Err(PathError::ContainsDotDot);
        }

        Ok(ProtectedPath(path.to_string()))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn as_path(&self) -> &Path {
        Path::new(&self.0)
    }

    pub fn to_path_buf(&self) -> PathBuf {
        PathBuf::from(&self.0)
    }
}

impl AsRef<str> for ProtectedPath {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl AsRef<Path> for ProtectedPath {
    fn as_ref(&self) -> &Path {
        Path::new(&self.0)
    }
}

impl std::ops::Deref for ProtectedPath {
    type Target = str;

    fn deref(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for ProtectedPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_path() {
        let path = ProtectedPath::new("src/main.rs");
        assert!(path.is_ok());
        assert_eq!(path.unwrap().as_str(), "src/main.rs");
    }

    #[test]
    fn test_empty_path() {
        let path = ProtectedPath::new("");
        assert!(matches!(path, Err(PathError::Empty)));
    }

    #[test]
    fn test_path_traversal_prevented() {
        let path = ProtectedPath::new("../../../etc/passwd");
        assert!(matches!(path, Err(PathError::ContainsDotDot)));
    }

    #[test]
    fn test_null_byte_prevented() {
        let path = ProtectedPath::new("src/main\0.rs");
        assert!(matches!(path, Err(PathError::ContainsNullByte)));
    }

    #[test]
    fn test_as_path() {
        let path = ProtectedPath::new("src/main.rs").unwrap();
        assert_eq!(path.as_path(), Path::new("src/main.rs"));
    }
}
