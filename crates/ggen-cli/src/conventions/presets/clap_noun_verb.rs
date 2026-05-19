//! Clap noun-verb convention preset

use mcpp_utils::error::Result;
use std::fs;
use std::path::Path;

use super::ConventionPreset;

/// Clap noun-verb convention preset
pub struct ClapNounVerbPreset;

impl ConventionPreset for ClapNounVerbPreset {
    fn name(&self) -> &str {
        "clap-noun-verb"
    }

    fn create_structure(&self, root: &Path) -> Result<()> {
        // Create directory structure
        let dirs = [
            ".mcpp/rdf",
            ".mcpp/templates/clap-noun-verb",
            "src/cmds",
            "src/domain",
        ];

        for dir in &dirs {
            fs::create_dir_all(root.join(dir))?;
        }

        // Write convention config
        fs::write(root.join(".mcpp/convention.toml"), self.config_content())?;

        // Write RDF files
        for (path, content) in self.rdf_files() {
            fs::write(root.join(".mcpp/rdf").join(path), content)?;
        }

        // Write template files
        for (path, content) in self.templates() {
            fs::write(root.join(".mcpp/templates").join(path), content)?;
        }

        Ok(())
    }

    fn rdf_files(&self) -> Vec<(&str, &str)> {
        vec![(
            "example_command.rdf",
            include_str!("../../templates/rdf/example_command.rdf"),
        )]
    }

    fn templates(&self) -> Vec<(&str, &str)> {
        vec![
            (
                "clap-noun-verb/command.rs.tera",
                include_str!("../../templates/clap-noun-verb/command.rs.tera"),
            ),
            (
                "clap-noun-verb/domain.rs.tera",
                include_str!("../../templates/clap-noun-verb/domain.rs.tera"),
            ),
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_create_structure() {
        let temp = TempDir::new().unwrap();
        let root = temp.path();

        let preset = ClapNounVerbPreset;
        preset.create_structure(root).unwrap();

        // Verify structure
        assert!(root.join(".mcpp/rdf").exists());
        assert!(root.join(".mcpp/templates/clap-noun-verb").exists());
        assert!(root.join(".mcpp/convention.toml").exists());
        assert!(root.join("src/cmds").exists());
        assert!(root.join("src/domain").exists());
    }
}
