use anyhow::Result;
use std::path::Path;
use std::fs;

/// EOL (End of Line) detection and normalization utilities.
pub struct EolNormalizer;

impl EolNormalizer {
    /// Detect the EOL style used in a file.
    /// 
    /// Returns the EOL string used in the file, or the platform default
    /// if no EOL is detected or the file is empty.
    pub fn detect_eol(file_path: &Path) -> Result<String> {
        if !file_path.exists() {
            return Ok(Self::platform_default());
        }
        
        let content = fs::read_to_string(file_path)?;
        Self::detect_eol_from_content(&content)
    }
    
    /// Detect EOL from file content.
    pub fn detect_eol_from_content(content: &str) -> Result<String> {
        // Look for CRLF first (Windows)
        if content.contains("\r\n") {
            return Ok("\r\n".to_string());
        }
        
        // Look for LF (Unix/Linux/macOS)
        if content.contains('\n') {
            return Ok("\n".to_string());
        }
        
        // Look for CR (old Mac)
        if content.contains('\r') {
            return Ok("\r".to_string());
        }
        
        // No EOL detected, use platform default
        Ok(Self::platform_default())
    }
    
    /// Get the platform default EOL.
    pub fn platform_default() -> String {
        if cfg!(windows) {
            "\r\n".to_string()
        } else {
            "\n".to_string()
        }
    }
    
    /// Normalize content to use the specified EOL.
    pub fn normalize_to_eol(content: &str, target_eol: &str) -> String {
        // First normalize to LF, then convert to target
        let normalized = content
            .replace("\r\n", "\n")  // CRLF -> LF
            .replace('\r', "\n");   // CR -> LF
        
        // Convert to target EOL
        normalized.replace('\n', target_eol)
    }
    
    /// Normalize content to match the EOL of a target file.
    pub fn normalize_to_match_file(content: &str, target_file: &Path) -> Result<String> {
        let target_eol = Self::detect_eol(target_file)?;
        Ok(Self::normalize_to_eol(content, &target_eol))
    }
}

/// Default skip_if pattern generator for injection templates.
pub struct SkipIfGenerator;

impl SkipIfGenerator {
    /// Generate a default skip_if pattern for exact substring match.
    /// 
    /// This creates a regex pattern that will match if the injection
    /// content already exists in the target file.
    pub fn generate_exact_match(content: &str) -> String {
        // Escape special regex characters in the content
        let escaped = regex::escape(content);
        format!("(?s){}", escaped) // (?s) enables dotall mode for multiline matching
    }
    
    /// Check if content already exists in target file using exact substring match.
    pub fn content_exists_in_file(content: &str, file_path: &Path) -> Result<bool> {
        if !file_path.exists() {
            return Ok(false);
        }
        
        let file_content = fs::read_to_string(file_path)?;
        Ok(file_content.contains(content))
    }
    
    /// Generate a default skip_if for idempotent injection.
    /// 
    /// This is more sophisticated than exact match - it looks for
    /// content that has already been injected by checking for
    /// rgen-specific markers or patterns.
    pub fn generate_idempotent_pattern(content: &str) -> String {
        // For now, use exact match
        // In the future, this could be enhanced to look for
        // rgen-specific markers or content signatures
        Self::generate_exact_match(content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn test_detect_eol_crlf() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        fs::write(temp_file.path(), "line1\r\nline2\r\n")?;
        
        let eol = EolNormalizer::detect_eol(temp_file.path())?;
        assert_eq!(eol, "\r\n");
        
        Ok(())
    }

    #[test]
    fn test_detect_eol_lf() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        fs::write(temp_file.path(), "line1\nline2\n")?;
        
        let eol = EolNormalizer::detect_eol(temp_file.path())?;
        assert_eq!(eol, "\n");
        
        Ok(())
    }

    #[test]
    fn test_detect_eol_cr() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        fs::write(temp_file.path(), "line1\rline2\r")?;
        
        let eol = EolNormalizer::detect_eol(temp_file.path())?;
        assert_eq!(eol, "\r");
        
        Ok(())
    }

    #[test]
    fn test_detect_eol_no_eol() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        fs::write(temp_file.path(), "single line")?;
        
        let eol = EolNormalizer::detect_eol(temp_file.path())?;
        assert_eq!(eol, EolNormalizer::platform_default());
        
        Ok(())
    }

    #[test]
    fn test_detect_eol_nonexistent_file() -> Result<()> {
        let eol = EolNormalizer::detect_eol(Path::new("/nonexistent/file"))?;
        assert_eq!(eol, EolNormalizer::platform_default());
        
        Ok(())
    }

    #[test]
    fn test_normalize_to_eol() {
        let content = "line1\r\nline2\rline3\nline4";
        
        // Normalize to LF
        let normalized_lf = EolNormalizer::normalize_to_eol(content, "\n");
        assert_eq!(normalized_lf, "line1\nline2\nline3\nline4");
        
        // Normalize to CRLF
        let normalized_crlf = EolNormalizer::normalize_to_eol(content, "\r\n");
        assert_eq!(normalized_crlf, "line1\r\nline2\r\nline3\r\nline4");
        
        // Normalize to CR
        let normalized_cr = EolNormalizer::normalize_to_eol(content, "\r");
        assert_eq!(normalized_cr, "line1\rline2\rline3\rline4");
    }

    #[test]
    fn test_normalize_to_match_file() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        fs::write(temp_file.path(), "existing\r\ncontent")?;
        
        let content_to_inject = "new\ncontent";
        let normalized = EolNormalizer::normalize_to_match_file(content_to_inject, temp_file.path())?;
        
        assert_eq!(normalized, "new\r\ncontent");
        
        Ok(())
    }

    #[test]
    fn test_generate_exact_match() {
        let content = "function hello() {\n  console.log('world');\n}";
        let pattern = SkipIfGenerator::generate_exact_match(content);
        
        // Should be a valid regex pattern
        assert!(regex::Regex::new(&pattern).is_ok());
        
        // Should match the original content
        let regex = regex::Regex::new(&pattern).unwrap();
        assert!(regex.is_match(content));
    }

    #[test]
    fn test_generate_exact_match_with_special_chars() {
        let content = "function test() {\n  return /^[a-z]+$/;\n}";
        let pattern = SkipIfGenerator::generate_exact_match(content);
        
        // Should escape special regex characters
        let regex = regex::Regex::new(&pattern).unwrap();
        assert!(regex.is_match(content));
        
        // Should not match similar but different content
        let different_content = "function test() {\n  return /^[A-Z]+$/;\n}";
        assert!(!regex.is_match(different_content));
    }

    #[test]
    fn test_content_exists_in_file() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        let content = "function hello() {\n  console.log('world');\n}";
        fs::write(temp_file.path(), &format!("// Header\n{}\n// Footer", content))?;
        
        // Content should exist
        assert!(SkipIfGenerator::content_exists_in_file(content, temp_file.path())?);
        
        // Different content should not exist
        let different_content = "function goodbye() {\n  console.log('moon');\n}";
        assert!(!SkipIfGenerator::content_exists_in_file(different_content, temp_file.path())?);
        
        Ok(())
    }

    #[test]
    fn test_content_exists_in_nonexistent_file() -> Result<()> {
        let result = SkipIfGenerator::content_exists_in_file("content", Path::new("/nonexistent/file"))?;
        assert!(!result);
        
        Ok(())
    }

    #[test]
    fn test_generate_idempotent_pattern() {
        let content = "function test() {}";
        let pattern = SkipIfGenerator::generate_idempotent_pattern(content);
        
        // Should generate a valid regex
        assert!(regex::Regex::new(&pattern).is_ok());
        
        // Should match the content
        let regex = regex::Regex::new(&pattern).unwrap();
        assert!(regex.is_match(content));
    }

    #[test]
    fn test_platform_default() {
        let default = EolNormalizer::platform_default();
        
        // Should be either \n or \r\n depending on platform
        assert!(default == "\n" || default == "\r\n");
    }
}
