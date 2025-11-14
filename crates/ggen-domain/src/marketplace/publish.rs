//! Marketplace publish domain logic
//!
//! Real implementation of package publishing functionality.

use ggen_utils::error::Result;
use std::path::{Path, PathBuf};

/// Publish command arguments
#[derive(Debug, Clone, Default)]
pub struct PublishInput {
    /// Package directory path
    pub path: PathBuf,

    /// Version tag
    pub tag: Option<String>,

    /// Dry run (simulate publish)
    pub dry_run: bool,

    /// Force publish even if version exists
    pub force: bool,
}

/// Publish package and report progress
///
/// # Arguments
///
/// * `path` - Path to the package directory to publish
/// * `tag` - Optional version tag for the package
/// * `dry_run` - Simulate publishing without actually publishing
/// * `force` - Force publish even if version exists
///
/// # Returns
///
/// Returns Ok(()) on success, or an error if publishing fails
pub async fn publish_and_report(
    path: &Path, tag: Option<&str>, dry_run: bool, force: bool,
) -> Result<()> {
    // Validate package directory
    if !path.exists() {
        return Err(ggen_utils::error::Error::new(&format!(
            "path not found: {}",
            path.display()
        )));
    }

    // Load package manifest
    let manifest_path = path.join("package.json");
    if !manifest_path.exists() {
        return Err(ggen_utils::error::Error::new(
            "package.json not found. This doesn't appear to be a valid package.",
        ));
    }

    let manifest_content = tokio::fs::read_to_string(&manifest_path)
        .await
        .map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to read package.json: {}", e))
        })?;

    let manifest: PackageManifest = serde_json::from_str(&manifest_content)?;

    // Determine version (use tag if provided, otherwise use manifest version)
    let version = tag.unwrap_or(&manifest.version);

    ggen_utils::alert_info!(
        "📦 Preparing to publish {} version {}",
        manifest.name,
        version
    );

    // Validate package
    validate_package(&manifest)?;

    if dry_run {
        ggen_utils::alert_info!("🔍 Dry run: Would publish package to registry");
        ggen_utils::alert_info!("   Package: {}", manifest.name);
        ggen_utils::alert_info!("   Version: {}", version);
        ggen_utils::alert_info!("   Title: {}", manifest.title);
        ggen_utils::alert_info!("   Description: {}", manifest.description);
        return Ok(());
    }

    // Get registry path
    let registry_path = dirs::home_dir()
        .ok_or_else(|| ggen_utils::error::Error::new("home directory not found"))?
        .join(".ggen")
        .join("registry");

    // Ensure registry exists
    tokio::fs::create_dir_all(&registry_path)
        .await
        .map_err(|e| ggen_utils::error::Error::new(&format!("IO error: {}", e)))?;

    // Check if package version already exists
    if !force && package_version_exists(&registry_path, &manifest.name, version).await? {
        return Err(ggen_utils::error::Error::new(&format!(
            "Package {} version {} already exists. Use --force to overwrite.",
            manifest.name, version
        )));
    }

    // Create package tarball
    ggen_utils::alert_info!("📦 Creating package tarball...");
    let tarball_path = create_tarball(path, &manifest.name, version).await?;

    // Update registry index
    ggen_utils::alert_info!("📝 Updating registry index...");
    update_registry_index(&registry_path, &manifest, version, &tarball_path).await?;

    ggen_utils::alert_success!(
        "Successfully published {} version {}",
        manifest.name,
        version
    );
    ggen_utils::alert_info!("   Registry: {}", registry_path.display());

    Ok(())
}

/// Execute publish command using ggen-marketplace backend
pub async fn execute_publish(input: PublishInput) -> Result<PublishOutput> {
    use ggen_marketplace::backend::LocalRegistry;
    use ggen_marketplace::prelude::*;
    use sha2::{Digest, Sha256};

    // Read package manifest
    let manifest_path = input.path.join("package.json");
    let manifest_content = tokio::fs::read_to_string(&manifest_path).await?;
    let manifest: PackageManifest = serde_json::from_str(&manifest_content)?;

    // Determine version
    let version_str = input.tag.unwrap_or(manifest.version.clone());

    if input.dry_run {
        return Ok(PublishOutput {
            package_name: manifest.name.clone(),
            version: version_str,
            dry_run: true,
            registry_path: String::new(),
        });
    }

    // Initialize local registry
    let registry_path = dirs::home_dir()
        .ok_or_else(|| ggen_utils::error::Error::new("home directory not found"))?
        .join(".ggen")
        .join("registry");

    let registry = LocalRegistry::new(registry_path.clone())
        .await
        .map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to initialize registry: {}", e))
        })?;

    // Create tarball
    let tarball_name = format!("{}-{}.tar.gz", manifest.name.replace('/', "-"), version_str);
    let tarball_path = input
        .path
        .parent()
        .unwrap_or(&input.path)
        .join(&tarball_name);

    create_tarball(&input.path, &manifest.name, &version_str).await?;

    // Calculate checksum
    let mut file = tokio::fs::File::open(&tarball_path).await?;
    let mut hasher = Sha256::new();
    let mut buffer = vec![0; 8192];

    use tokio::io::AsyncReadExt;
    loop {
        let n = file.read(&mut buffer).await?;
        if n == 0 {
            break;
        }
        hasher.update(&buffer[..n]);
    }
    let checksum = format!("{:x}", hasher.finalize());

    // Build package using ggen-marketplace models
    let package_id = PackageId::new("local", &manifest.name);

    // Parse version string (format: "major.minor.patch")
    // Return error if version format is invalid instead of silently creating 0.0.0
    let version_parts: Vec<u32> = version_str
        .split('.')
        .map(|s| {
            s.parse::<u32>().map_err(|_| {
                ggen_utils::error::Error::new(&format!(
                    "Invalid version component '{}' in version string '{}'. Expected format: major.minor.patch (e.g., 1.2.3)",
                    s, version_str
                ))
            })
        })
        .collect::<std::result::Result<Vec<u32>, ggen_utils::error::Error>>()?;

    if version_parts.len() < 3 {
        return Err(ggen_utils::error::Error::new(&format!(
            "Invalid version format '{}'. Expected format: major.minor.patch (e.g., 1.2.3). Found {} component(s)",
            version_str, version_parts.len()
        )));
    }

    let version = Version::new(version_parts[0], version_parts[1], version_parts[2]);

    let content_id = ContentId::new(&checksum, HashAlgorithm::Sha256);

    let mut builder = Package::builder(package_id.clone(), version.clone())
        .title(&manifest.title)
        .description(&manifest.description)
        .license("MIT")
        .content_id(content_id);

    // Add tags individually using .tag() method
    for tag in &manifest.tags {
        builder = builder.tag(tag);
    }

    let unvalidated = builder
        .build()
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to build package: {}", e)))?;

    // Validate package before publishing (Poka-yoke: ensures package meets requirements)
    let validated = unvalidated.validate().map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to validate package: {}", e))
    })?;

    // Extract Package from ValidatedPackage for publishing
    let package = validated.package().clone();

    // Publish to registry
    registry
        .publish(package)
        .await
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to publish: {}", e)))?;

    // Update index
    update_registry_index(
        &registry_path,
        &manifest,
        &version_str,
        &tarball_path.display().to_string(),
    )
    .await?;

    Ok(PublishOutput {
        package_name: manifest.name,
        version: version_str,
        dry_run: false,
        registry_path: registry_path.display().to_string(),
    })
}

/// Run publish command (legacy wrapper - creates runtime)
pub fn run(args: &PublishInput) -> Result<()> {
    let rt = tokio::runtime::Runtime::new()
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to create runtime: {}", e)))?;

    let output = rt.block_on(execute_publish(args.clone()))?;

    if !output.dry_run {
        ggen_utils::alert_success!(
            "Successfully published {} version {}",
            output.package_name,
            output.version
        );
        ggen_utils::alert_info!("   Registry: {}", output.registry_path);
    }

    Ok(())
}

/// Publish operation output
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct PublishOutput {
    /// Name of the published package
    pub package_name: String,

    /// Version that was published
    pub version: String,

    /// Whether this was a dry run
    pub dry_run: bool,

    /// Path to the registry where package was published
    pub registry_path: String,
}

/// Validate package manifest
fn validate_package(manifest: &PackageManifest) -> Result<()> {
    if manifest.name.is_empty() {
        return Err(ggen_utils::error::Error::new("Package name is required"));
    }

    if manifest.version.is_empty() {
        return Err(ggen_utils::error::Error::new("Package version is required"));
    }

    if manifest.title.is_empty() {
        return Err(ggen_utils::error::Error::new("Package title is required"));
    }

    if manifest.description.is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Package description is required",
        ));
    }

    Ok(())
}

/// Check if package version already exists in registry
async fn package_version_exists(
    registry_path: &Path, package_name: &str, version: &str,
) -> Result<bool> {
    let index_path = registry_path.join("index.json");

    if !index_path.exists() {
        return Ok(false);
    }

    let content = tokio::fs::read_to_string(&index_path)
        .await
        .map_err(|e| ggen_utils::error::Error::new(&format!("IO error: {}", e)))?;

    let index: serde_json::Value = serde_json::from_str(&content)?;

    Ok(index
        .get("packages")
        .and_then(|p| p.get(package_name))
        .and_then(|versions| versions.as_array())
        .map(|versions| {
            versions
                .iter()
                .any(|v| v.get("version").and_then(|v| v.as_str()) == Some(version))
        })
        .unwrap_or(false))
}

/// Create tarball of package
async fn create_tarball(path: &Path, package_name: &str, version: &str) -> Result<String> {
    use flate2::write::GzEncoder;
    use flate2::Compression;
    use std::fs::File;
    use tar::Builder;

    let tarball_name = format!("{}-{}.tar.gz", package_name.replace('/', "-"), version);
    let tarball_path = path.parent().unwrap_or(path).join(&tarball_name);

    // Create tarball file
    let tarball_file = File::create(&tarball_path)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to create tarball: {}", e)))?;

    let encoder = GzEncoder::new(tarball_file, Compression::default());
    let mut tar = Builder::new(encoder);

    // Add all files from package directory to tarball
    tar.append_dir_all(".", path).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to add files to tarball: {}", e))
    })?;

    tar.finish()
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to finish tarball: {}", e)))?;

    Ok(tarball_path.to_string_lossy().to_string())
}

/// Update registry index with new package
async fn update_registry_index(
    registry_path: &Path, manifest: &PackageManifest, version: &str, tarball_path: &str,
) -> Result<()> {
    let index_path = registry_path.join("index.json");

    let mut index: serde_json::Value = if index_path.exists() {
        let content = tokio::fs::read_to_string(&index_path)
            .await
            .map_err(|e| ggen_utils::error::Error::new(&format!("IO error: {}", e)))?;
        serde_json::from_str(&content)?
    } else {
        serde_json::json!({
            "version": "1.0",
            "packages": {}
        })
    };

    // Add/update package entry
    if let Some(packages) = index.get_mut("packages").and_then(|p| p.as_object_mut()) {
        let package_versions = packages
            .entry(manifest.name.clone())
            .or_insert_with(|| serde_json::json!([]));

        if let Some(versions) = package_versions.as_array_mut() {
            // Remove existing version if present
            versions.retain(|v| v.get("version").and_then(|v| v.as_str()) != Some(version));

            // Add new version
            versions.push(serde_json::json!({
                "version": version,
                "title": manifest.title,
                "description": manifest.description,
                "categories": manifest.categories,
                "tags": manifest.tags,
                "downloads": 0,
                "stars": 0,
                "tarball": tarball_path,
                "published_at": chrono::Utc::now().to_rfc3339()
            }));
        }
    }

    // Write index
    let content = serde_json::to_string_pretty(&index)?;
    tokio::fs::write(&index_path, content)
        .await
        .map_err(|e| ggen_utils::error::Error::new(&format!("IO error: {}", e)))?;

    Ok(())
}

/// Package manifest structure
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct PackageManifest {
    name: String,
    version: String,
    title: String,
    description: String,
    #[serde(default)]
    categories: Vec<String>,
    #[serde(default)]
    tags: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[tokio::test]
    async fn test_publish_dry_run() {
        let temp_dir = tempdir().unwrap();
        let package_dir = temp_dir.path().join("test-package");
        tokio::fs::create_dir_all(&package_dir).await.unwrap();

        let manifest = PackageManifest {
            name: "test/example".to_string(),
            version: "1.0.0".to_string(),
            title: "Example Package".to_string(),
            description: "A test package".to_string(),
            categories: vec!["testing".to_string()],
            tags: vec!["test".to_string()],
        };

        let manifest_content = serde_json::to_string_pretty(&manifest).unwrap();
        tokio::fs::write(package_dir.join("package.json"), manifest_content)
            .await
            .unwrap();

        let result = publish_and_report(&package_dir, Some("1.0.0"), true, false).await;
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_package_valid() {
        let manifest = PackageManifest {
            name: "test/example".to_string(),
            version: "1.0.0".to_string(),
            title: "Example Package".to_string(),
            description: "A test package".to_string(),
            categories: vec![],
            tags: vec![],
        };

        assert!(validate_package(&manifest).is_ok());
    }

    #[test]
    fn test_validate_package_empty_name() {
        let manifest = PackageManifest {
            name: String::new(),
            version: "1.0.0".to_string(),
            title: "Example Package".to_string(),
            description: "A test package".to_string(),
            categories: vec![],
            tags: vec![],
        };

        assert!(validate_package(&manifest).is_err());
    }

    #[tokio::test]
    async fn test_package_version_exists_no_registry() {
        let temp_dir = tempdir().unwrap();
        let registry_path = temp_dir.path().join("registry");

        let exists = package_version_exists(&registry_path, "test/example", "1.0.0")
            .await
            .unwrap();
        assert!(!exists);
    }
}
