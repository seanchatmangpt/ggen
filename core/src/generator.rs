use anyhow::{bail, Result};
use regex::Regex;
use sha2::{Digest, Sha256};
use std::fs;
use std::io::Write;
use std::path::Path;

use crate::template::Frontmatter;

/// Generator handles file injection and shell hooks
pub struct Generator;

impl Generator {
    /// Apply injection to target file based on frontmatter configuration
    pub fn apply_injection(
        target_path: &Path,
        content: &str,
        frontmatter: &Frontmatter,
        dry_run: bool,
    ) -> Result<()> {
        if !frontmatter.inject {
            return Ok(());
        }

        // Validate injection mode exclusivity
        Self::validate_injection_mode(frontmatter)?;

        // Check unless_exists condition
        if frontmatter.unless_exists && target_path.exists() {
            return Ok(()); // Skip if file already exists
        }

        // Check if target file exists
        if !target_path.exists() {
            if frontmatter.force {
                // Create new file with content
                if let Some(parent) = target_path.parent() {
                    fs::create_dir_all(parent)?;
                }
                fs::write(target_path, content)?;
                return Ok(());
            } else {
                bail!("Target file '{}' does not exist and force is not enabled. Use force: true to create new files", target_path.display());
            }
        }

        // Create backup if requested
        if frontmatter.backup.unwrap_or(false) {
            Self::create_backup(target_path)?;
        }

        // Read existing content
        let existing_content = fs::read_to_string(target_path)?;
        
        // Check skip_if condition
        if let Some(skip_pattern) = &frontmatter.skip_if {
            let regex = Regex::new(skip_pattern)?;
            if regex.is_match(&existing_content) {
                return Ok(()); // Skip injection
            }
        }

        // Prepare content with sentinel if idempotency is enabled
        let content_to_inject = if frontmatter.idempotent {
            Self::add_idempotency_sentinel(content)?
        } else {
            content.to_string()
        };
        
        // Check idempotency if enabled
        if frontmatter.idempotent {
            if Self::check_idempotency(&existing_content, &content_to_inject, frontmatter)? {
                return Ok(()); // Skip injection - already present
            }
        }
        
        // Apply injection based on mode
        let new_content = if frontmatter.prepend {
            format!("{}{}", content_to_inject, existing_content)
        } else if frontmatter.append {
            format!("{}{}", existing_content, content_to_inject)
        } else if let Some(before_pattern) = &frontmatter.before {
            Self::inject_before(&existing_content, &content_to_inject, before_pattern)?
        } else if let Some(after_pattern) = &frontmatter.after {
            Self::inject_after(&existing_content, &content_to_inject, after_pattern)?
        } else if let Some(line_num) = frontmatter.at_line {
            Self::inject_at_line(&existing_content, &content_to_inject, line_num as usize)?
        } else {
            // Default: append
            format!("{}{}", existing_content, content_to_inject)
        };

        // Handle eof_last: ensure content ends with newline if specified
        let final_content = if frontmatter.eof_last {
            if !new_content.ends_with('\n') {
                format!("{}\n", new_content)
            } else {
                new_content
            }
        } else {
            new_content.trim_end_matches('\n').to_string()
        };

        if !dry_run {
            Self::atomic_write(target_path, &final_content)?;
        }
        Ok(())
    }

    /// Execute shell hooks with content piped to stdin
    pub fn execute_shell_hooks(
        sh_before: Option<&str>,
        sh_after: Option<&str>,
        content: &str,
        vars: &std::collections::BTreeMap<String, String>,
        dry_run: bool,
    ) -> Result<()> {
        if !dry_run {
            if let Some(cmd) = sh_before {
                Self::execute_shell_command(cmd, content, vars)?;
            }
            
            if let Some(cmd) = sh_after {
                Self::execute_shell_command(cmd, content, vars)?;
            }
        }
        
        Ok(())
    }

    fn atomic_write(target_path: &Path, content: &str) -> Result<()> {
        use tempfile::NamedTempFile;
        
        // Create temp file in same directory as target
        let parent_dir = target_path.parent()
            .ok_or_else(|| anyhow::anyhow!("Target path has no parent directory"))?;
        
        let mut temp_file = NamedTempFile::new_in(parent_dir)?;
        temp_file.write_all(content.as_bytes())?;
        temp_file.flush()?;
        
        // Preserve file permissions if target exists
        if target_path.exists() {
            let metadata = std::fs::metadata(target_path)?;
            temp_file.as_file_mut().set_permissions(metadata.permissions())?;
        }
        
        // Atomically rename temp file to target
        temp_file.persist(target_path)?;
        
        Ok(())
    }

    fn create_backup(target_path: &Path) -> Result<()> {
        if !target_path.exists() {
            return Ok(()); // Nothing to backup
        }
        
        // Create backup with .bak extension
        let backup_path = if let Some(extension) = target_path.extension().and_then(|ext| ext.to_str()) {
            target_path.with_extension(format!("{}.bak", extension))
        } else {
            target_path.with_extension("bak")
        };
        
        // If backup already exists, add timestamp
        let final_backup_path = if backup_path.exists() {
            use std::time::{SystemTime, UNIX_EPOCH};
            let timestamp = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .as_secs();
            
            if let Some(extension) = target_path.extension().and_then(|ext| ext.to_str()) {
                target_path.with_extension(format!("{}.bak.{}", extension, timestamp))
            } else {
                target_path.with_extension(format!("bak.{}", timestamp))
            }
        } else {
            backup_path
        };
        
        fs::copy(target_path, &final_backup_path)?;
        Ok(())
    }

    fn add_idempotency_sentinel(content: &str) -> Result<String> {
        // Generate content hash sentinel (SHA256 first 16 chars)
        let mut hasher = Sha256::new();
        hasher.update(content.as_bytes());
        let content_hash = format!("{:x}", hasher.finalize());
        let sentinel = format!("<!-- rgen:{} -->", &content_hash[..16]);
        
        // Add sentinel to content
        Ok(format!("{}\n{}", content, sentinel))
    }

    fn check_idempotency(
        existing: &str,
        content_with_sentinel: &str,
        _frontmatter: &Frontmatter,
    ) -> Result<bool> {
        // Extract the sentinel from the content
        if let Some(sentinel_start) = content_with_sentinel.rfind("<!-- rgen:") {
            if let Some(sentinel_end) = content_with_sentinel[sentinel_start..].find(" -->") {
                let sentinel = &content_with_sentinel[sentinel_start..sentinel_start + sentinel_end + 4];
                
                // Check if sentinel already exists in target file
                if existing.contains(sentinel) {
                    return Ok(true); // Already injected
                }
            }
        }
        
        Ok(false) // Not yet injected
    }

    fn validate_injection_mode(frontmatter: &Frontmatter) -> Result<()> {
        let modes = [
            frontmatter.prepend,
            frontmatter.append,
            frontmatter.before.is_some(),
            frontmatter.after.is_some(),
            frontmatter.at_line.is_some(),
        ];
        if modes.iter().filter(|&&m| m).count() > 1 {
            bail!("Multiple injection modes specified. Only one of prepend, append, before, after, or at_line should be used");
        }
        Ok(())
    }

    fn inject_before(existing: &str, new_content: &str, pattern: &str) -> Result<String> {
        let regex = Regex::new(pattern)?;
        if let Some(mat) = regex.find(existing) {
            let mut result = existing.to_string();
            result.insert_str(mat.start(), new_content);
            Ok(result)
        } else {
            bail!("Pattern '{}' not found in target file. Check your regex pattern", pattern);
        }
    }

    fn inject_after(existing: &str, new_content: &str, pattern: &str) -> Result<String> {
        let regex = Regex::new(pattern)?;
        if let Some(mat) = regex.find(existing) {
            let mut result = existing.to_string();
            result.insert_str(mat.end(), new_content);
            Ok(result)
        } else {
            bail!("Pattern '{}' not found in target file. Check your regex pattern", pattern);
        }
    }

    fn inject_at_line(existing: &str, new_content: &str, line_num: usize) -> Result<String> {
        let lines: Vec<&str> = existing.lines().collect();
        
        // Handle edge cases
        if line_num == 0 {
            bail!("Line number must be >= 1 (got {})", line_num);
        }
        
        if line_num > lines.len() + 1 {
            bail!("Line number {} exceeds file length + 1 ({})", line_num, lines.len() + 1);
        }

        let mut result = Vec::new();
        
        // Insert at the beginning (line 1)
        if line_num == 1 {
            result.push(new_content);
            result.extend(lines);
        }
        // Insert at the end (line == lines.len() + 1)
        else if line_num == lines.len() + 1 {
            result.extend(lines);
            result.push(new_content);
        }
        // Insert in the middle
        else {
            for (i, line) in lines.iter().enumerate() {
                result.push(line);
                if i + 1 == line_num - 1 { // Convert to 0-based index
                    result.push(new_content);
                }
            }
        }

        Ok(result.join("\n"))
    }

    fn execute_shell_command(
        cmd: &str,
        content: &str,
        vars: &std::collections::BTreeMap<String, String>,
    ) -> Result<()> {
        use std::process::{Command, Stdio};
        
        // Create environment variables from template vars
        let mut env_vars = std::collections::HashMap::new();
        for (k, v) in vars {
            env_vars.insert(k.clone(), v.clone());
        }
        
        let mut child = Command::new("sh")
            .arg("-c")
            .arg(cmd)
            .envs(&env_vars)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        // Write content to stdin and explicitly drop it to signal EOF
        if let Some(mut stdin) = child.stdin.take() {
            use std::io::Write;
            stdin.write_all(content.as_bytes())?;
            drop(stdin); // Signal EOF
        }

        let output = child.wait_with_output()?;
        
        // Log output if RGEN_TRACE is set
        if std::env::var("RGEN_TRACE").is_ok() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            let stderr = String::from_utf8_lossy(&output.stderr);
            
            eprintln!("Shell command: {}", cmd);
            eprintln!("Exit code: {}", output.status);
            if !stdout.is_empty() {
                eprintln!("STDOUT:\n{}", stdout);
            }
            if !stderr.is_empty() {
                eprintln!("STDERR:\n{}", stderr);
            }
        }
        
        if !output.status.success() {
            let stderr = String::from_utf8_lossy(&output.stderr);
            bail!("Shell command '{}' failed with exit code {}: {}", 
                  cmd, output.status, stderr);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn test_inject_before() -> Result<()> {
        let existing = "line1\nline2\nline3";
        let new_content = "injected\n";
        let pattern = "line2";
        
        let result = Generator::inject_before(existing, new_content, pattern)?;
        assert_eq!(result, "line1\ninjected\nline2\nline3");
        Ok(())
    }

    #[test]
    fn test_inject_after() -> Result<()> {
        let existing = "line1\nline2\nline3";
        let new_content = "\ninjected";
        let pattern = "line2";
        
        let result = Generator::inject_after(existing, new_content, pattern)?;
        assert_eq!(result, "line1\nline2\ninjected\nline3");
        Ok(())
    }

    #[test]
    fn test_inject_at_line() -> Result<()> {
        let existing = "line1\nline2\nline3";
        let new_content = "injected";
        
        let result = Generator::inject_at_line(existing, new_content, 2)?;
        assert_eq!(result, "line1\ninjected\nline2\nline3");
        Ok(())
    }

    #[test]
    fn test_apply_injection_force() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        let path = temp_file.path();
        
        let mut frontmatter = Frontmatter::default();
        frontmatter.inject = true;
        frontmatter.force = true;
        
        Generator::apply_injection(path, "new content", &frontmatter, false)?;
        
        let content = fs::read_to_string(path)?;
        assert_eq!(content, "new content");
        Ok(())
    }

    #[test]
    fn test_skip_if_condition() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        let path = temp_file.path();
        fs::write(path, "existing content with SKIP_ME marker")?;
        
        let mut frontmatter = Frontmatter::default();
        frontmatter.inject = true;
        frontmatter.skip_if = Some("SKIP_ME".to_string());
        
        Generator::apply_injection(path, "new content", &frontmatter, false)?;
        
        // Content should be unchanged due to skip_if
        let content = fs::read_to_string(path)?;
        assert_eq!(content, "existing content with SKIP_ME marker");
        Ok(())
    }

    #[test]
    fn test_unless_exists() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        let path = temp_file.path();
        fs::write(path, "existing content")?;
        
        let mut frontmatter = Frontmatter::default();
        frontmatter.inject = true;
        frontmatter.unless_exists = true;
        
        Generator::apply_injection(path, "new content", &frontmatter, false)?;
        
        // Content should be unchanged due to unless_exists
        let content = fs::read_to_string(path)?;
        assert_eq!(content, "existing content");
        Ok(())
    }

    #[test]
    fn test_eof_last_with_newline() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        let path = temp_file.path();
        fs::write(path, "existing content")?;
        
        let mut frontmatter = Frontmatter::default();
        frontmatter.inject = true;
        frontmatter.append = true;
        frontmatter.eof_last = true;
        
        Generator::apply_injection(path, "new content", &frontmatter, false)?;
        
        let content = fs::read_to_string(path)?;
        assert!(content.ends_with('\n'));
        Ok(())
    }

    #[test]
    fn test_eof_last_without_newline() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        let path = temp_file.path();
        fs::write(path, "existing content")?;
        
        let mut frontmatter = Frontmatter::default();
        frontmatter.inject = true;
        frontmatter.append = true;
        frontmatter.eof_last = false;
        
        Generator::apply_injection(path, "new content\n", &frontmatter, false)?;
        
        let content = fs::read_to_string(path)?;
        assert!(!content.ends_with('\n'));
        Ok(())
    }

    #[test]
    fn test_inject_at_line_edge_cases() -> Result<()> {
        let existing = "line1\nline2\nline3";
        
        // Test inserting at line 1 (beginning)
        let result = Generator::inject_at_line(existing, "injected", 1)?;
        assert_eq!(result, "injected\nline1\nline2\nline3");
        
        // Test inserting at line 4 (end)
        let result = Generator::inject_at_line(existing, "injected", 4)?;
        assert_eq!(result, "line1\nline2\nline3\ninjected");
        
        // Test inserting in middle
        let result = Generator::inject_at_line(existing, "injected", 2)?;
        assert_eq!(result, "line1\ninjected\nline2\nline3");
        
        Ok(())
    }

    #[test]
    fn test_inject_at_line_errors() -> Result<()> {
        let existing = "line1\nline2";
        
        // Test line 0 (invalid)
        let result = Generator::inject_at_line(existing, "injected", 0);
        assert!(result.is_err());
        
        // Test line beyond end + 1
        let result = Generator::inject_at_line(existing, "injected", 4);
        assert!(result.is_err());
        
        Ok(())
    }

    #[test]
    fn test_idempotency() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        let path = temp_file.path();
        fs::write(path, "existing content")?;
        
        let mut frontmatter = Frontmatter::default();
        frontmatter.inject = true;
        frontmatter.append = true;
        frontmatter.idempotent = true;
        
        let content = "new content";
        
        // First injection
        Generator::apply_injection(path, content, &frontmatter, false)?;
        let first_content = fs::read_to_string(path)?;
        
        // Second injection should be skipped
        Generator::apply_injection(path, content, &frontmatter, false)?;
        let second_content = fs::read_to_string(path)?;
        
        // Content should be the same (idempotent)
        assert_eq!(first_content, second_content);
        
        // Content should contain the sentinel
        assert!(second_content.contains("<!-- rgen:"));
        
        Ok(())
    }

    #[test]
    fn test_backup_creation() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        let path = temp_file.path();
        fs::write(path, "original content")?;
        
        let mut frontmatter = Frontmatter::default();
        frontmatter.inject = true;
        frontmatter.append = true;
        frontmatter.backup = Some(true);
        
        Generator::apply_injection(path, "new content", &frontmatter, false)?;
        
        // Check that backup was created
        let backup_path = path.with_extension("bak");
        assert!(backup_path.exists());
        
        let backup_content = fs::read_to_string(&backup_path)?;
        assert_eq!(backup_content, "original content");
        
        Ok(())
    }

    #[test]
    fn test_dry_run() -> Result<()> {
        let temp_file = NamedTempFile::new()?;
        let path = temp_file.path();
        let original_content = "existing content";
        fs::write(path, original_content)?;
        
        let mut frontmatter = Frontmatter::default();
        frontmatter.inject = true;
        frontmatter.append = true;
        
        // Dry run should not modify the file
        Generator::apply_injection(path, "new content", &frontmatter, true)?;
        
        let content = fs::read_to_string(path)?;
        assert_eq!(content, original_content);
        
        Ok(())
    }

    #[test]
    fn test_validation_multiple_modes() -> Result<()> {
        let mut frontmatter = Frontmatter::default();
        frontmatter.inject = true;
        frontmatter.prepend = true;
        frontmatter.append = true; // Multiple modes
        
        let temp_file = NamedTempFile::new()?;
        let path = temp_file.path();
        fs::write(path, "content")?;
        
        let result = Generator::apply_injection(path, "new", &frontmatter, false);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Multiple injection modes"));
        
        Ok(())
    }

    #[test]
    fn test_force_creation() -> Result<()> {
        let temp_dir = tempfile::tempdir()?;
        let path = temp_dir.path().join("nonexistent.txt");
        
        let mut frontmatter = Frontmatter::default();
        frontmatter.inject = true;
        frontmatter.force = true;
        
        Generator::apply_injection(&path, "new content", &frontmatter, false)?;
        
        assert!(path.exists());
        let content = fs::read_to_string(&path)?;
        assert_eq!(content, "new content");
        
        Ok(())
    }
}
