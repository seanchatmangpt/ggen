# Marketplace API Reference

Complete API documentation for the ggen marketplace registry and package format.

## 📡 Registry API

### Registry URL

```
https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml
```

### Registry Format

The registry is a TOML file containing all package metadata:

```toml
# Registry metadata
version = "1.0.0"
registry_url = "https://github.com/seanchatmangpt/ggen"
raw_base_url = "https://raw.githubusercontent.com/seanchatmangpt/ggen/master"
pages_url = "https://seanchatmangpt.github.io/ggen"

# Package entries
[[package]]
name = "package-name"
# ... package fields ...
```

### Fetching the Registry

```bash
# Download registry
curl -L https://seanchatmangpt.github.io/ggen/marketplace/registry/packages.toml

# Parse with toml-cli
toml get packages.toml package[0].name

# Using ggen
ggen market search --all --json
```

## 📦 Package Format

### Package Metadata Schema

```toml
[[package]]
# Required fields
name = "string"                    # Unique package identifier
version = "string"                 # Semantic version (e.g., "1.0.0")
description = "string"             # Brief description (max 200 chars)
author = "string"                  # Package author/maintainer
license = "string"                 # SPDX license identifier

# Optional fields
full_name = "string"               # owner/repo format
category = "string"                # Category: templates, utilities, ai, frameworks
repository = "string"              # Git repository URL
homepage = "string"                # Project homepage
documentation = "string"           # Documentation URL
download_url = "string"            # Package download URL
path = "string"                    # Path within repository
changelog = "string"               # CHANGELOG.md URL

# Arrays
features = ["string"]              # Feature list
tags = ["string"]                  # Searchable tags
keywords = ["string"]              # Search keywords
dependencies = ["string"]          # Package dependencies
authors = ["string"]               # Multiple authors

# Metadata
[package.metadata]
min_ggen_version = "string"        # Minimum ggen version
max_ggen_version = "string"        # Maximum ggen version
stability = "string"               # alpha, beta, stable
deprecated = boolean               # Deprecation flag
replacement = "string"             # Replacement package name
```

### Field Constraints

| Field | Required | Format | Max Length |
|-------|----------|--------|------------|
| `name` | Yes | `[a-z0-9-]+` | 50 |
| `version` | Yes | SemVer | 20 |
| `description` | Yes | Text | 200 |
| `author` | Yes | Text | 100 |
| `license` | Yes | SPDX ID | 50 |
| `category` | No | Enum | 20 |
| `tags` | No | Array | 10 items |
| `keywords` | No | Array | 20 items |
| `features` | No | Array | 50 items |

### Category Values

```toml
category = "templates"     # Project templates
category = "utilities"     # Utility packages
category = "ai"            # AI integrations
category = "frameworks"    # Frameworks and libraries
category = "tools"         # Development tools
category = "examples"      # Example projects
```

### Version Format (SemVer)

```toml
version = "1.0.0"           # Stable release
version = "1.0.0-alpha"     # Alpha release
version = "1.0.0-beta.1"    # Beta release
version = "1.0.0-rc.2"      # Release candidate
```

## 🗂️ Package Directory Structure

### Minimal Package

```
marketplace/packages/package-name/
├── package.toml          # Package metadata
├── README.md             # Documentation
└── src/                  # Source code
```

### Complete Package

```
marketplace/packages/package-name/
├── package.toml          # Package metadata
├── README.md             # User documentation
├── CHANGELOG.md          # Version history
├── LICENSE               # License file
├── .gitignore            # Git ignore rules
├── make.toml             # Lifecycle configuration
├── ggen.toml             # ggen configuration
│
├── src/                  # Source code
│   ├── lib.rs
│   ├── main.rs
│   └── modules/
│
├── templates/            # Code generation templates
│   ├── template.hbs
│   └── partials/
│
├── data/                 # SPARQL/RDF data
│   ├── schema.ttl
│   └── queries.rq
│
├── tests/                # Test suite
│   ├── integration/
│   └── unit/
│
├── examples/             # Usage examples
│   └── basic.rs
│
├── docs/                 # Additional documentation
│   └── guide.md
│
└── scripts/              # Build/deploy scripts
    └── setup.sh
```

## 🔍 Search API

### Search Query Format

```bash
# Text search (searches name, description, tags, keywords)
ggen market search "query string"

# Category filter
ggen market search "query" --category templates

# Tag filter
ggen market search "query" --tag rust

# Multiple filters
ggen market search "api" \
  --category templates \
  --tag production \
  --limit 10
```

### Search Response

```json
{
  "packages": [
    {
      "name": "advanced-rust-api-8020",
      "version": "0.1.0",
      "description": "Production-ready REST API...",
      "author": "ggen-team",
      "downloads": 1234,
      "stars": 56
    }
  ],
  "total": 1
}
```

## 📥 Installation API

### Install Request

```rust
pub struct InstallInput {
    /// Package name (format: name@version or just name)
    pub package: String,

    /// Target directory (default: current directory)
    pub target: Option<String>,

    /// Force overwrite existing files
    pub force: bool,

    /// Skip dependency installation
    pub no_dependencies: bool,

    /// Simulate installation without writing files
    pub dry_run: bool,
}
```

### Install Response

```rust
pub struct InstallResult {
    /// Installed package name
    pub package_name: String,

    /// Installed version
    pub version: String,

    /// Installation path
    pub install_path: PathBuf,

    /// Installed dependencies
    pub dependencies_installed: Vec<String>,
}
```

### Install JSON Response

```json
{
  "package": "advanced-rust-api-8020",
  "version": "0.1.0",
  "path": "/Users/user/projects/my-api",
  "dependencies": []
}
```

## 📋 List API

### List Request

```rust
pub struct ListInput {
    /// Show detailed information
    pub detailed: bool,

    /// JSON output format
    pub json: bool,
}
```

### List Response

```rust
pub struct ListOutput {
    /// List of installed packages
    pub packages: Vec<InstalledPackage>,

    /// Total count
    pub total: usize,
}

pub struct InstalledPackage {
    pub name: String,
    pub version: String,
    pub title: String,
    pub description: String,
}
```

## 📤 Publish API

### Publish Request

```rust
pub struct PublishInput {
    /// Package directory path
    pub path: PathBuf,

    /// Version tag (e.g., "v1.0.0")
    pub tag: Option<String>,

    /// Simulate publishing without creating PR
    pub dry_run: bool,

    /// Force publish even with warnings
    pub force: bool,
}
```

### Publish Response

```rust
pub struct PublishOutput {
    /// Published package name
    pub package_name: String,

    /// Published version
    pub version: String,
}
```

## 🔗 Dependency Resolution

### Dependency Format

```toml
[[package]]
name = "my-package"
dependencies = [
    "base-template",           # Latest version
    "utils@1.0.0",             # Specific version
    "helpers@^1.0",            # Compatible version
    "tools@~1.2.3"             # Patch version range
]
```

### Version Specifiers

| Specifier | Meaning | Example | Matches |
|-----------|---------|---------|---------|
| `1.0.0` | Exact | `1.0.0` | 1.0.0 only |
| `^1.0.0` | Compatible | `^1.0.0` | ≥1.0.0, <2.0.0 |
| `~1.0.0` | Patch | `~1.0.0` | ≥1.0.0, <1.1.0 |
| `>=1.0.0` | Greater/Equal | `>=1.0.0` | ≥1.0.0 |
| `*` | Any | `*` | Any version |

### Dependency Resolution Algorithm

1. Parse package dependencies
2. Check if dependency is installed
3. If not, fetch from registry
4. Recursively resolve dependencies
5. Detect circular dependencies
6. Install in correct order

## 🔒 Security & Verification

### Package Checksums

```toml
[[package]]
name = "package-name"
checksum = "sha256:abc123..."  # SHA256 hash of package contents
signature = "gpg:..."           # GPG signature (future)
```

### Verification Process

```bash
# Verify package integrity
ggen market verify "package-name"

# Output:
# ✓ Checksum verified: sha256:abc123...
# ✓ Signature verified (future)
# ✓ No known vulnerabilities
```

## 📊 Statistics & Metrics

### Package Statistics

```toml
[[package]]
name = "package-name"

[package.stats]
downloads = 1234        # Total downloads
stars = 56              # GitHub stars
last_updated = "2024-01-15T10:30:00Z"
created = "2023-06-01T00:00:00Z"
```

### Fetching Statistics

```bash
# Get package stats
ggen market stats "package-name"

# Output:
# Downloads: 1,234
# Stars: 56
# Last Updated: 2024-01-15
# Created: 2023-06-01
```

## 🌍 Registry Endpoints

### Production Registry

```
Base URL: https://seanchatmangpt.github.io/ggen/marketplace/

Endpoints:
  GET /registry/packages.toml          - Package registry
  GET /packages/{name}/                - Package directory
  GET /packages/{name}/README.md       - Package README
  GET /packages/{name}/package.toml    - Package metadata
  GET /stats/{name}.json               - Package statistics (future)
```

### Raw Content URLs

```
Base URL: https://raw.githubusercontent.com/seanchatmangpt/ggen/master/

Endpoints:
  GET /marketplace/registry/packages.toml
  GET /marketplace/packages/{name}/{file}
```

## 🛠️ CLI Commands Reference

### Search Commands

```bash
ggen market search <query>                    # Search packages
ggen market search --category <category>      # Filter by category
ggen market search --tag <tag>                # Filter by tag
ggen market search --all                      # List all packages
ggen market search --limit <n>                # Limit results
ggen market search --json                     # JSON output
```

### Info Commands

```bash
ggen market info <package>                    # Package details
ggen market readme <package>                  # View README
ggen market versions <package>                # List versions
ggen market deps <package>                    # Show dependencies
ggen market source <package>                  # View source URL
```

### Install Commands

```bash
ggen market install <package>                 # Install package
ggen market install <package>@<version>       # Install version
ggen market install <package> --target <dir>  # Install to directory
ggen market install <package> --force         # Force overwrite
ggen market install <package> --dry-run       # Preview install
ggen market install <package> --no-deps       # Skip dependencies
```

### Management Commands

```bash
ggen market list                              # List installed
ggen market list --detailed                   # Detailed list
ggen market update <package>                  # Update package
ggen market update --all                      # Update all
ggen market remove <package>                  # Remove package
ggen market verify <package>                  # Verify integrity
```

### Publishing Commands

```bash
ggen market publish                           # Publish package
ggen market publish --tag <tag>               # Publish with tag
ggen market publish --dry-run                 # Preview publish
ggen market publish --force                   # Force publish
```

### Cache Commands

```bash
ggen market cache update                      # Update cache
ggen market cache clear                       # Clear cache
ggen market cache path                        # Show cache location
```

## 🔌 Programmatic API (Rust)

### Search Packages

```rust
use ggen_domain::marketplace::{execute_search, SearchInput};

let input = SearchInput {
    query: "rust api".to_string(),
    limit: 10,
    category: Some("templates".to_string()),
    ..Default::default()
};

let results = execute_search(input).await?;
for package in results {
    println!("{} - {}", package.name, package.description);
}
```

### Install Package

```rust
use ggen_domain::marketplace::{execute_install, InstallInput};

let input = InstallInput {
    package: "advanced-rust-api-8020".to_string(),
    target: Some("./my-project".to_string()),
    force: false,
    no_dependencies: false,
    dry_run: false,
};

let result = execute_install(input).await?;
println!("Installed {} v{}", result.package_name, result.version);
```

### List Packages

```rust
use ggen_domain::marketplace::{execute_list, ListInput};

let input = ListInput {
    detailed: true,
    json: false,
};

let result = execute_list(input).await?;
println!("Total packages: {}", result.packages_listed);
```

### Publish Package

```rust
use ggen_domain::marketplace::{execute_publish, PublishInput};
use std::path::PathBuf;

let input = PublishInput {
    path: PathBuf::from("./my-package"),
    tag: Some("v1.0.0".to_string()),
    dry_run: false,
    force: false,
};

let result = execute_publish(input).await?;
println!("Published {} v{}", result.package_name, result.version);
```

## 🧪 Testing

### Registry Testing

```bash
# Validate registry syntax
toml-cli validate marketplace/registry/packages.toml

# Check for duplicates
ggen market validate-registry

# Test package installation
ggen market install "test-package" --dry-run
```

### Package Testing

```bash
# Validate package.toml
toml-cli validate package.toml

# Test templates
ggen template validate templates/*.hbs

# Run lifecycle
ggen lifecycle run --dry-run
```

## 📚 Examples

### Complete Package Entry

```toml
[[package]]
name = "advanced-rust-api-8020"
full_name = "ggen/advanced-rust-api-8020"
version = "0.1.0"
description = "Production-ready REST API with complete lifecycle, AI generation, and 80/20 principles"
category = "templates"
author = "ggen-team"
repository = "https://github.com/seanchatmangpt/ggen"
homepage = "https://seanchatmangpt.github.io/ggen/"
documentation = "https://seanchatmangpt.github.io/ggen/docs/api"
download_url = "https://github.com/seanchatmangpt/ggen/archive/refs/heads/master.zip"
path = "examples/advanced-rust-api-8020"
license = "MIT"
changelog = "https://github.com/seanchatmangpt/ggen/blob/master/CHANGELOG.md"

dependencies = []

features = [
    "Complete lifecycle management with make.toml",
    "AI-powered code generation from SPARQL specifications",
    "Production readiness tracking (80/20 rule)",
    "JWT authentication with bcrypt",
    "Comprehensive error handling",
    "Structured logging with tracing",
    "Health check endpoints",
    "OpenAPI documentation ready",
    "Docker containerization",
    "Complete test suite with 80%+ coverage",
    "Database migrations with SQLx",
    "Environment-based configuration"
]

tags = ["rust", "api", "rest", "production", "lifecycle", "ai-generation", "8020", "axum", "jwt"]
keywords = ["rust", "axum", "rest-api", "production", "template", "ai", "sparql", "lifecycle"]

[package.metadata]
min_ggen_version = "2.5.0"
stability = "stable"
```

## 🆘 Error Codes

| Code | Description | Resolution |
|------|-------------|------------|
| `REGISTRY_FETCH_FAILED` | Cannot fetch registry | Check network connection |
| `PACKAGE_NOT_FOUND` | Package doesn't exist | Verify package name |
| `VERSION_NOT_FOUND` | Version doesn't exist | Check available versions |
| `INSTALL_FAILED` | Installation error | Check permissions |
| `DEPENDENCY_CONFLICT` | Dependency version mismatch | Resolve conflicts |
| `CHECKSUM_MISMATCH` | Package integrity check failed | Re-download package |
| `INVALID_MANIFEST` | package.toml is invalid | Fix TOML syntax |

## 📞 Support

- **API Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Documentation**: https://seanchatmangpt.github.io/ggen/
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions

---

**API Version**: 1.0.0
**Last Updated**: 2024-01-15
**Stability**: Stable
