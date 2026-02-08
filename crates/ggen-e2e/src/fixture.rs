//! Test fixture management
//!
//! Handles loading ggen project fixtures (ontologies, templates, manifests)
//! and discovering fixtures in the test suite.

use crate::error::{FixtureError, Result};
use crate::golden::GoldenFile;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// A test fixture representing a complete ggen project
#[derive(Debug, Clone)]
pub struct TestFixture {
    /// Unique fixture identifier (e.g., "thesis-gen", "minimal")
    pub name: String,
    /// Path to fixture directory
    pub path: PathBuf,
    /// TTL/RDF ontology files
    pub ontology_files: Vec<PathBuf>,
    /// Tera template files
    pub template_files: Vec<PathBuf>,
    /// Path to ggen.toml manifest
    pub ggen_toml: PathBuf,
    /// Directory containing expected output (golden files)
    pub golden_dir: PathBuf,
}

impl TestFixture {
    /// Load a fixture from a directory
    pub fn load(path: impl AsRef<Path>, name: &str) -> Result<Self> {
        let path = path.as_ref().to_path_buf();

        if !path.exists() {
            return Err(FixtureError::NotFound(path).into());
        }

        let ggen_toml = path.join("ggen.toml");
        if !ggen_toml.exists() {
            return Err(FixtureError::MissingFile(ggen_toml).into());
        }

        let ontology_dir = path.join("ontology");
        let template_dir = path.join("templates");

        let ontology_files = if ontology_dir.exists() {
            discover_files(&ontology_dir, &["ttl", "rdf"])?
        } else {
            Vec::new()
        };

        let template_files = if template_dir.exists() {
            discover_files(&template_dir, &["tera", "jinja2"])?
        } else {
            Vec::new()
        };

        // Golden directory is typically in tests/e2e/golden/{fixture_name}
        let golden_dir = PathBuf::from("tests/e2e/golden").join(name);

        Ok(TestFixture {
            name: name.to_string(),
            path,
            ontology_files,
            template_files,
            ggen_toml,
            golden_dir,
        })
    }

    /// Copy fixture to a temporary directory
    pub fn copy_to_temp(&self) -> Result<TempDir> {
        let temp_dir = TempDir::new().map_err(|e| FixtureError::CopyFailed(e.to_string()))?;

        copy_dir_recursive(&self.path, temp_dir.path())
            .map_err(|e| FixtureError::CopyFailed(e.to_string()))?;

        Ok(temp_dir)
    }

    /// Get all golden files for this fixture
    pub fn golden_files(&self) -> Result<Vec<GoldenFile>> {
        let mut files = Vec::new();

        if !self.golden_dir.exists() {
            return Ok(files);
        }

        discover_golden_files(&self.golden_dir, &self.golden_dir, &mut files)?;
        Ok(files)
    }

    /// Check if fixture has required files
    pub fn validate(&self) -> Result<()> {
        if !self.ggen_toml.exists() {
            return Err(FixtureError::MissingFile(self.ggen_toml.clone()).into());
        }

        if self.ontology_files.is_empty() {
            return Err(FixtureError::Configuration(format!(
                "Fixture '{}' has no ontology files",
                self.name
            ))
            .into());
        }

        if self.template_files.is_empty() {
            return Err(FixtureError::Configuration(format!(
                "Fixture '{}' has no template files",
                self.name
            ))
            .into());
        }

        Ok(())
    }

    /// Get the fixture's ggen.toml content
    pub fn manifest_content(&self) -> Result<String> {
        fs::read_to_string(&self.ggen_toml).map_err(|e| FixtureError::Io(e).into())
    }
}

/// Discover fixtures in the standard location
pub fn discover_fixtures(fixtures_dir: &Path) -> Result<Vec<TestFixture>> {
    let mut fixtures = Vec::new();

    if !fixtures_dir.exists() {
        return Ok(fixtures);
    }

    for entry in fs::read_dir(fixtures_dir).map_err(FixtureError::Io)? {
        let entry = entry.map_err(FixtureError::Io)?;
        let path = entry.path();

        if path.is_dir() {
            if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                if let Ok(fixture) = TestFixture::load(&path, name) {
                    fixtures.push(fixture);
                }
            }
        }
    }

    Ok(fixtures)
}

/// Discover files with specific extensions in a directory
fn discover_files(dir: &Path, extensions: &[&str]) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    for entry in fs::read_dir(dir).map_err(FixtureError::Io)? {
        let entry = entry.map_err(FixtureError::Io)?;
        let path = entry.path();

        if path.is_file() {
            if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
                if extensions.contains(&ext) {
                    files.push(path);
                }
            }
        }
    }

    Ok(files)
}

/// Recursively discover golden files
fn discover_golden_files(dir: &Path, base_dir: &Path, files: &mut Vec<GoldenFile>) -> Result<()> {
    for entry in fs::read_dir(dir).map_err(FixtureError::Io)? {
        let entry = entry.map_err(FixtureError::Io)?;
        let path = entry.path();

        if path.is_file() {
            if let Ok(relative_path) = path.strip_prefix(base_dir) {
                if let Ok(golden) = GoldenFile::load(base_dir, relative_path) {
                    files.push(golden);
                }
            }
        } else if path.is_dir() {
            discover_golden_files(&path, base_dir, files)?;
        }
    }

    Ok(())
}

/// Recursively copy a directory
fn copy_dir_recursive(src: &Path, dst: &Path) -> std::io::Result<()> {
    fs::create_dir_all(dst)?;

    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let path = entry.path();
        let file_name = entry.file_name();
        let dest_path = dst.join(&file_name);

        if path.is_dir() {
            copy_dir_recursive(&path, &dest_path)?;
        } else {
            fs::copy(&path, &dest_path)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_discover_files() {
        let temp_dir = tempfile::TempDir::new().unwrap();
        let test_dir = temp_dir.path().join("test");
        fs::create_dir(&test_dir).unwrap();

        fs::write(test_dir.join("file1.ttl"), "").unwrap();
        fs::write(test_dir.join("file2.ttl"), "").unwrap();
        fs::write(test_dir.join("file3.rdf"), "").unwrap();
        fs::write(test_dir.join("file4.txt"), "").unwrap();

        let ttl_files = discover_files(&test_dir, &["ttl"]).unwrap();
        assert_eq!(ttl_files.len(), 2);

        let rdf_files = discover_files(&test_dir, &["rdf"]).unwrap();
        assert_eq!(rdf_files.len(), 1);

        let all_files = discover_files(&test_dir, &["ttl", "rdf"]).unwrap();
        assert_eq!(all_files.len(), 3);
    }

    #[test]
    fn test_copy_dir_recursive() {
        let temp_dir = tempfile::TempDir::new().unwrap();
        let src = temp_dir.path().join("src");
        let dst = temp_dir.path().join("dst");

        fs::create_dir(&src).unwrap();
        fs::create_dir(src.join("subdir")).unwrap();
        fs::write(src.join("file1.txt"), "content1").unwrap();
        fs::write(src.join("subdir/file2.txt"), "content2").unwrap();

        copy_dir_recursive(&src, &dst).unwrap();

        assert!(dst.join("file1.txt").exists());
        assert!(dst.join("subdir/file2.txt").exists());
        assert_eq!(
            fs::read_to_string(dst.join("file1.txt")).unwrap(),
            "content1"
        );
    }

    #[test]
    fn test_fixture_copy_to_temp() {
        let temp_dir = tempfile::TempDir::new().unwrap();
        let fixture_dir = temp_dir.path().join("fixture");

        fs::create_dir(&fixture_dir).unwrap();
        fs::create_dir(fixture_dir.join("ontology")).unwrap();
        fs::create_dir(fixture_dir.join("templates")).unwrap();
        fs::write(fixture_dir.join("ggen.toml"), "[package]").unwrap();
        fs::write(fixture_dir.join("ontology/schema.ttl"), "").unwrap();
        fs::write(fixture_dir.join("templates/main.tera"), "").unwrap();

        let fixture = TestFixture::load(&fixture_dir, "test").unwrap();
        let copy = fixture.copy_to_temp().unwrap();

        assert!(copy.path().join("ggen.toml").exists());
        assert!(copy.path().join("ontology/schema.ttl").exists());
    }
}
