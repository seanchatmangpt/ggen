//! Audit trail writer for persisting execution records to JSON

use std::fs;
use std::path::Path;
use super::AuditTrail;

/// Writes audit trail records to JSON files
pub struct AuditTrailWriter;

impl AuditTrailWriter {
    /// Write an audit trail to a JSON file
    pub fn write(
        trail: &AuditTrail,
        output_path: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // Ensure output directory exists
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }

        // Serialize and write JSON
        let json = trail.to_json()?;
        fs::write(output_path, json)?;
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_write_audit_trail() {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let output_path = temp_dir.path().join("audit.json");
        
        let mut audit = AuditTrail::new("5.0.2", "test.toml", "test.ttl");
        audit.record_rule_executed();
        
        AuditTrailWriter::write(&audit, &output_path)
            .expect("Failed to write audit trail");
        
        assert!(output_path.exists(), "audit.json should exist");
        
        let content = fs::read_to_string(&output_path)
            .expect("Failed to read audit.json");
        assert!(content.contains("\"rules_executed\""));
    }
}
