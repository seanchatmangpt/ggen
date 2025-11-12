//! Clap noun-verb convention preset

use anyhow::Result;
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
            ".ggen/rdf",
            ".ggen/templates/clap-noun-verb",
            "src/cmds",
            "src/domain",
        ];

        for dir in &dirs {
            fs::create_dir_all(root.join(dir))?;
        }

        // Write convention config
        fs::write(root.join(".ggen/convention.toml"), self.config_content())?;

        // Write RDF files
        for (path, content) in self.rdf_files() {
            fs::write(root.join(".ggen/rdf").join(path), content)?;
        }

        // Write template files
        for (path, content) in self.templates() {
            fs::write(root.join(".ggen/templates").join(path), content)?;
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
                "clap-noun-verb/command.rs.hbs",
                include_str!("../../templates/clap-noun-verb/command.rs.hbs"),
            ),
            (
                "clap-noun-verb/domain.rs.hbs",
                include_str!("../../templates/clap-noun-verb/domain.rs.hbs"),
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
        assert!(root.join(".ggen/rdf").exists());
        assert!(root.join(".ggen/templates/clap-noun-verb").exists());
        assert!(root.join(".ggen/convention.toml").exists());
        assert!(root.join("src/cmds").exists());
        assert!(root.join("src/domain").exists());
    }
}
