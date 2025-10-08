use anyhow::{bail, Result};
use crate::template::Frontmatter;

/// Validate frontmatter configuration for common issues.
/// 
/// This performs basic validation to catch configuration errors early
/// and provide helpful error messages to users.
pub fn validate_frontmatter(front: &Frontmatter) -> Result<()> {
    // Validate field combinations first (most specific errors)
    validate_field_combinations(front)?;
    
    // Validate injection mode exclusivity
    validate_injection_modes(front)?;
    
    // Validate required fields last (most general errors)
    validate_required_fields(front)?;
    
    Ok(())
}

/// Validate that only one injection mode is specified.
fn validate_injection_modes(front: &Frontmatter) -> Result<()> {
    if !front.inject {
        return Ok(()); // No injection, no validation needed
    }
    
    let mut modes = Vec::new();
    
    if front.prepend {
        modes.push("prepend");
    }
    if front.append {
        modes.push("append");
    }
    if front.before.is_some() {
        modes.push("before");
    }
    if front.after.is_some() {
        modes.push("after");
    }
    if front.at_line.is_some() {
        modes.push("at_line");
    }
    
    if modes.is_empty() {
        // No injection mode specified, default to append
        return Ok(());
    }
    
    if modes.len() > 1 {
        bail!(
            "Multiple injection modes specified: {}. Only one of prepend, append, before, after, or at_line should be used.",
            modes.join(", ")
        );
    }
    
    Ok(())
}

/// Validate that required fields are present.
fn validate_required_fields(front: &Frontmatter) -> Result<()> {
    // For non-injection templates, 'to' is required
    if !front.inject && front.to.is_none() {
        bail!("Field 'to' is required for non-injection templates. Specify where to write the output file.");
    }
    
    // For injection templates, at least one injection mode should be specified
    if front.inject {
        let has_injection_mode = front.prepend 
            || front.append 
            || front.before.is_some() 
            || front.after.is_some() 
            || front.at_line.is_some();
            
        if !has_injection_mode {
            // This is OK - we'll default to append
        }
    }
    
    Ok(())
}

/// Validate field combinations and constraints.
fn validate_field_combinations(front: &Frontmatter) -> Result<()> {
    // Validate at_line constraints
    if let Some(line_num) = front.at_line {
        if line_num == 0 {
            bail!("at_line must be >= 1 (got {})", line_num);
        }
    }
    
    // Validate skip_if with injection
    if front.skip_if.is_some() && !front.inject {
        bail!("skip_if can only be used with inject: true");
    }
    
    // Validate idempotent with injection
    if front.idempotent && !front.inject {
        bail!("idempotent can only be used with inject: true");
    }
    
    // Validate backup with injection
    if front.backup.unwrap_or(false) && !front.inject {
        bail!("backup can only be used with inject: true");
    }
    
    // Validate unless_exists with injection
    if front.unless_exists && !front.inject {
        bail!("unless_exists can only be used with inject: true");
    }
    
    // Validate force with injection
    if front.force && !front.inject {
        bail!("force can only be used with inject: true");
    }
    
    // Validate eof_last with injection
    if front.eof_last && !front.inject {
        bail!("eof_last can only be used with inject: true");
    }
    
    Ok(())
}

/// Warn about unknown frontmatter keys.
/// 
/// This is a best-effort warning system. Since we can't easily detect
/// all unknown keys from the parsed Frontmatter struct, this function
/// is meant to be called with the raw YAML data.
pub fn warn_unknown_keys(raw_yaml: &str, known_keys: &[&str]) {
    // This is a simplified implementation
    // In a real implementation, you'd parse the YAML and check for unknown keys
    // For now, we'll just log a general warning if there are many keys
    
    let key_count = raw_yaml.lines()
        .filter(|line| line.contains(':') && !line.trim_start().starts_with('#'))
        .count();
    
    if key_count > known_keys.len() + 5 { // Allow some buffer for nested structures
        eprintln!("Warning: Template may contain unknown frontmatter keys. Known keys: {}", known_keys.join(", "));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_injection_modes_single() {
        let mut front = Frontmatter::default();
        front.inject = true;
        front.prepend = true;
        
        assert!(validate_frontmatter(&front).is_ok());
    }

    #[test]
    fn test_validate_injection_modes_multiple() {
        let mut front = Frontmatter::default();
        front.inject = true;
        front.prepend = true;
        front.append = true;
        
        let result = validate_frontmatter(&front);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Multiple injection modes"));
    }

    #[test]
    fn test_validate_injection_modes_none() {
        let mut front = Frontmatter::default();
        front.inject = true;
        // No injection mode specified - should default to append
        
        assert!(validate_frontmatter(&front).is_ok());
    }

    #[test]
    fn test_validate_required_fields_no_inject() {
        let front = Frontmatter::default();
        // No 'to' field and no inject
        
        let result = validate_frontmatter(&front);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Field 'to' is required"));
    }

    #[test]
    fn test_validate_required_fields_with_inject() {
        let mut front = Frontmatter::default();
        front.inject = true;
        // No 'to' field but inject is true - should be OK
        
        assert!(validate_frontmatter(&front).is_ok());
    }

    #[test]
    fn test_validate_at_line_zero() {
        let mut front = Frontmatter::default();
        front.inject = true;
        front.at_line = Some(0);
        
        let result = validate_frontmatter(&front);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("at_line must be >= 1"));
    }

    #[test]
    fn test_validate_skip_if_without_inject() {
        let mut front = Frontmatter::default();
        front.skip_if = Some("pattern".to_string());
        // inject is false
        
        let result = validate_frontmatter(&front);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("skip_if can only be used with inject: true"));
    }

    #[test]
    fn test_validate_idempotent_without_inject() {
        let mut front = Frontmatter::default();
        front.idempotent = true;
        // inject is false
        
        let result = validate_frontmatter(&front);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("idempotent can only be used with inject: true"));
    }

    #[test]
    fn test_validate_backup_without_inject() {
        let mut front = Frontmatter::default();
        front.backup = Some(true);
        // inject is false
        
        let result = validate_frontmatter(&front);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("backup can only be used with inject: true"));
    }

    #[test]
    fn test_validate_unless_exists_without_inject() {
        let mut front = Frontmatter::default();
        front.unless_exists = true;
        // inject is false
        
        let result = validate_frontmatter(&front);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("unless_exists can only be used with inject: true"));
    }

    #[test]
    fn test_validate_force_without_inject() {
        let mut front = Frontmatter::default();
        front.force = true;
        // inject is false
        
        let result = validate_frontmatter(&front);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("force can only be used with inject: true"));
    }

    #[test]
    fn test_validate_eof_last_without_inject() {
        let mut front = Frontmatter::default();
        front.eof_last = true;
        // inject is false
        
        let result = validate_frontmatter(&front);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("eof_last can only be used with inject: true"));
    }

    #[test]
    fn test_validate_valid_combinations() {
        let mut front = Frontmatter::default();
        front.inject = true;
        front.append = true;
        front.skip_if = Some("pattern".to_string());
        front.idempotent = true;
        front.backup = Some(true);
        front.unless_exists = true;
        front.force = true;
        front.eof_last = true;
        front.at_line = Some(5);
        
        // Should be valid
        assert!(validate_frontmatter(&front).is_ok());
    }
}
