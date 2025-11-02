# V2.0 Command Migration Template

## Architecture: Three-Layer Pattern

```
┌─────────────────────────────────────────────────────────┐
│ LAYER 1: CLI (Sync Wrapper)                             │
│ Location: cli/src/cmds/<category>/<command>.rs          │
│ Purpose: Clap argument parsing + async→sync bridge      │
│ Size: ~30 LOC per command                               │
└─────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────┐
│ LAYER 2: Domain (Async Business Logic)                  │
│ Location: cli/src/domain/<category>/<operation>.rs      │
│ Purpose: Pure business logic, async, testable           │
│ Size: ~200 LOC per operation                            │
└─────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────┐
│ LAYER 3: Runtime (Core Services)                        │
│ Location: ggen-core/, ggen-ai/                          │
│ Purpose: Graph engine, template engine, AI services     │
│ Size: Existing codebase                                 │
└─────────────────────────────────────────────────────────┘
```

## Pattern 1: Simple Command (No Domain Logic Needed)

**Example: `ggen utils doctor`**

### Layer 1: CLI Wrapper (cmds/utils/doctor.rs)

```rust
//! System diagnostics command - CLI layer
//!
//! Provides the `ggen utils doctor` command interface.

use clap::Args;
use ggen_utils::error::Result;
use crate::domain::utils::doctor::{SystemChecker, DefaultSystemChecker};

#[derive(Args, Debug)]
pub struct DoctorArgs {
    /// Show verbose diagnostic information
    #[arg(short, long)]
    pub verbose: bool,

    /// Output format (text, json)
    #[arg(long, default_value = "text")]
    pub format: String,
}

/// Synchronous entry point - bridges to async domain logic
pub async fn run(args: &DoctorArgs) -> Result<()> {
    let checker = DefaultSystemChecker;
    let result = checker.check_system(args.verbose)?;

    // CLI presentation logic only
    if args.format == "json" {
        println!("{}", serde_json::to_string_pretty(&result)?);
    } else {
        print_text_report(&result);
    }

    if result.summary.has_failures() {
        std::process::exit(1);
    }

    Ok(())
}

fn print_text_report(result: &SystemCheckResult) {
    println!("System Diagnostics Report");
    println!("========================\n");

    for check in &result.checks {
        let icon = match check.status {
            CheckStatus::Pass => "✓",
            CheckStatus::Warn => "⚠",
            CheckStatus::Fail => "✗",
            CheckStatus::Info => "ℹ",
        };

        println!("{} {} - {}", icon, check.name, check.message);
        if let Some(details) = &check.details {
            println!("  {}", details);
        }
    }

    println!("\nSummary: {}/{} checks passed",
             result.summary.passed, result.summary.total);
}
```

### Layer 2: Domain Logic (domain/utils/doctor.rs)

```rust
//! System diagnostics - Domain layer
//!
//! Pure business logic for checking system health.

use ggen_utils::error::Result;
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize)]
pub struct SystemCheckResult {
    pub checks: Vec<SystemCheck>,
    pub summary: CheckSummary,
    pub check_duration_ms: u64,
}

#[derive(Debug, Clone, Serialize)]
pub struct SystemCheck {
    pub name: String,
    pub status: CheckStatus,
    pub message: String,
    pub details: Option<String>,
    pub required: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum CheckStatus {
    Pass, Warn, Fail, Info
}

/// Trait for system checking - enables mocking in tests
pub trait SystemChecker {
    fn check_system(&self, verbose: bool) -> Result<SystemCheckResult>;
}

pub struct DefaultSystemChecker;

impl SystemChecker for DefaultSystemChecker {
    fn check_system(&self, verbose: bool) -> Result<SystemCheckResult> {
        let start = std::time::Instant::now();
        let mut checks = Vec::new();

        // Core checks
        checks.push(self.check_rust()?);
        checks.push(self.check_cargo()?);
        checks.push(self.check_git()?);

        if verbose {
            checks.push(self.check_disk_space()?);
            checks.push(self.check_network()?);
        }

        let summary = CheckSummary::from_checks(&checks);
        let duration = start.elapsed().as_millis() as u64;

        Ok(SystemCheckResult { checks, summary, check_duration_ms: duration })
    }
}

impl DefaultSystemChecker {
    fn check_rust(&self) -> Result<SystemCheck> {
        let output = std::process::Command::new("rustc")
            .arg("--version")
            .output();

        match output {
            Ok(output) if output.status.success() => {
                let version = String::from_utf8_lossy(&output.stdout).to_string();
                Ok(SystemCheck {
                    name: "Rust".to_string(),
                    status: CheckStatus::Pass,
                    message: format!("Rust is installed: {}", version.trim()),
                    details: None,
                    required: true,
                })
            }
            _ => Ok(SystemCheck {
                name: "Rust".to_string(),
                status: CheckStatus::Fail,
                message: "Rust is not installed or not in PATH".to_string(),
                details: Some("Install from https://rustup.rs".to_string()),
                required: true,
            }),
        }
    }

    // ... more check methods
}
```

## Pattern 2: Complex Command (Async Domain Logic)

**Example: `ggen marketplace install`**

### Layer 1: CLI Wrapper (cmds/marketplace/install.rs)

```rust
//! Marketplace package installation - CLI layer

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct InstallArgs {
    /// Package name or URL
    pub package: String,

    /// Force reinstall
    #[arg(short, long)]
    pub force: bool,

    /// Target directory
    #[arg(short, long)]
    pub output: Option<String>,
}

/// Async entry point - direct async execution
pub async fn run(args: &InstallArgs) -> Result<()> {
    use crate::domain::marketplace::install;

    println!("Installing package: {}", args.package);

    // Call async domain logic
    let result = install::install_package(
        &args.package,
        args.output.as_deref(),
        args.force
    ).await?;

    // CLI presentation
    println!("✓ Installed {} to {}", result.name, result.path);
    if !result.warnings.is_empty() {
        println!("\nWarnings:");
        for warning in &result.warnings {
            println!("  ⚠ {}", warning);
        }
    }

    Ok(())
}
```

### Layer 2: Domain Logic (domain/marketplace/install.rs)

```rust
//! Package installation logic - Domain layer

use ggen_utils::error::Result;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct InstallResult {
    pub name: String,
    pub version: String,
    pub path: String,
    pub warnings: Vec<String>,
}

/// Install a package from marketplace or URL
pub async fn install_package(
    package: &str,
    output_dir: Option<&str>,
    force: bool
) -> Result<InstallResult> {
    // 1. Resolve package source
    let source = resolve_package_source(package).await?;

    // 2. Download package
    let temp_path = download_package(&source).await?;

    // 3. Validate package
    let manifest = validate_package(&temp_path)?;

    // 4. Install to target directory
    let target_dir = determine_install_dir(output_dir)?;
    let install_path = install_to_directory(&temp_path, &target_dir, force)?;

    // 5. Run post-install hooks
    let warnings = run_post_install_hooks(&install_path).await?;

    Ok(InstallResult {
        name: manifest.name,
        version: manifest.version,
        path: install_path.display().to_string(),
        warnings,
    })
}

async fn resolve_package_source(package: &str) -> Result<PackageSource> {
    // Business logic: Determine if package is:
    // - Marketplace package (name@version)
    // - Git URL
    // - Local path
    // - HTTP URL

    if package.starts_with("http://") || package.starts_with("https://") {
        return Ok(PackageSource::HttpUrl(package.to_string()));
    }

    if package.contains('@') {
        let parts: Vec<&str> = package.split('@').collect();
        return Ok(PackageSource::Marketplace {
            name: parts[0].to_string(),
            version: parts.get(1).map(|v| v.to_string()),
        });
    }

    if Path::new(package).exists() {
        return Ok(PackageSource::Local(PathBuf::from(package)));
    }

    // Default to marketplace lookup
    Ok(PackageSource::Marketplace {
        name: package.to_string(),
        version: None,
    })
}

// ... more helper functions
```

## Pattern 3: Command with Runtime Services

**Example: `ggen template generate`**

### Layer 1: CLI Wrapper (cmds/template/generate.rs)

```rust
//! Template generation command - CLI layer

use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct GenerateArgs {
    /// Template name
    pub template: String,

    /// Template variables (key=value)
    #[arg(short, long)]
    pub var: Vec<String>,

    /// Output directory
    #[arg(short, long)]
    pub output: Option<String>,
}

pub async fn run(args: &GenerateArgs) -> Result<()> {
    use crate::domain::template::generate;

    // Parse variables from CLI args
    let vars = parse_variables(&args.var)?;

    println!("Generating from template: {}", args.template);

    // Call async domain logic
    let result = generate::generate_from_template(
        &args.template,
        &vars,
        args.output.as_deref()
    ).await?;

    // CLI presentation
    println!("✓ Generated {} files", result.files_created.len());
    for file in &result.files_created {
        println!("  + {}", file);
    }

    Ok(())
}

fn parse_variables(vars: &[String]) -> Result<HashMap<String, String>> {
    let mut map = HashMap::new();
    for var in vars {
        let parts: Vec<&str> = var.splitn(2, '=').collect();
        if parts.len() != 2 {
            return Err(ggen_utils::error::Error::new(&format!(
                "Invalid variable format: {}. Expected key=value", var
            )));
        }
        map.insert(parts[0].to_string(), parts[1].to_string());
    }
    Ok(map)
}
```

### Layer 2: Domain Logic (domain/template/generate.rs)

```rust
//! Template generation business logic - Domain layer

use ggen_core::template::Template;
use ggen_core::graph::Graph;
use ggen_utils::error::Result;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct GenerateResult {
    pub files_created: Vec<String>,
    pub template_path: String,
}

/// Generate files from a template with variables
pub async fn generate_from_template(
    template_name: &str,
    vars: &HashMap<String, String>,
    output_dir: Option<&str>
) -> Result<GenerateResult> {
    // 1. Locate template
    let template_path = locate_template(template_name)?;

    // 2. Load and parse template (uses Layer 3: ggen-core)
    let template_content = tokio::fs::read_to_string(&template_path).await?;
    let mut template = Template::parse(&template_content)?;

    // 3. Initialize graph for RDF/SPARQL support (Layer 3)
    let mut graph = Graph::new();

    // 4. Create rendering context
    let mut tera = tera::Tera::default();
    let context = create_context(vars)?;

    // 5. Process template with graph integration
    template.process_graph(&mut graph, &mut tera, &context, &template_path)?;

    // 6. Render template
    let rendered = template.render(&mut tera, &context)?;

    // 7. Write output files
    let files_created = write_output_files(
        &template,
        &rendered,
        output_dir
    ).await?;

    Ok(GenerateResult {
        files_created,
        template_path: template_path.display().to_string(),
    })
}

fn locate_template(name: &str) -> Result<PathBuf> {
    // Business logic: Template resolution priority
    // 1. Current directory templates/
    // 2. User home ~/.ggen/templates/
    // 3. System templates /usr/share/ggen/templates/

    let search_paths = vec![
        PathBuf::from("templates").join(name),
        dirs::home_dir()
            .ok_or_else(|| ggen_utils::error::Error::new("Cannot determine home directory"))?
            .join(".ggen/templates")
            .join(name),
    ];

    for path in search_paths {
        if path.exists() {
            return Ok(path);
        }
    }

    Err(ggen_utils::error::Error::new(&format!(
        "Template not found: {}", name
    )))
}

// ... more helper functions
```

## Migration Checklist

For each command `ggen <noun> <verb>`:

### 1. Create CLI Wrapper (cmds/ layer)
- [ ] Create `cli/src/cmds/<noun>/<verb>.rs`
- [ ] Add `#[derive(Args)]` struct for arguments
- [ ] Implement `pub async fn run(args: &Args) -> Result<()>`
- [ ] Add clap documentation comments
- [ ] Add CLI presentation logic (println, formatting)
- [ ] Update `cmds/<noun>/mod.rs` to export the command

### 2. Create Domain Logic (domain/ layer)
- [ ] Create `cli/src/domain/<noun>/<verb>.rs`
- [ ] Implement pure business logic functions (async)
- [ ] Define result types and error types
- [ ] Add trait definitions for testability
- [ ] Write comprehensive unit tests
- [ ] Update `domain/<noun>/mod.rs` to export functions

### 3. Update Runtime (if needed)
- [ ] Add new ggen-core functions if needed
- [ ] Update ggen-ai integrations if needed
- [ ] Add new traits to utils if needed

### 4. Testing
- [ ] Unit tests for domain layer (mock external dependencies)
- [ ] Integration tests for CLI layer
- [ ] E2E tests for full command flow

### 5. Documentation
- [ ] Add module-level documentation
- [ ] Add function-level documentation
- [ ] Update user-facing docs
- [ ] Add examples to docs/

## Key Principles

1. **Sync CLI Wrapper → Async Domain Logic**
   - CLI layer: ~30 LOC per command
   - Domain layer: ~200 LOC per operation
   - Keep CLI layer thin!

2. **Testability**
   - Domain layer: 100% mockable (traits)
   - CLI layer: Integration tests only
   - Runtime layer: Unit tests

3. **Separation of Concerns**
   - CLI: Argument parsing + presentation
   - Domain: Business logic + orchestration
   - Runtime: Core services + algorithms

4. **Error Handling**
   - CLI: User-friendly error messages
   - Domain: Detailed error context
   - Runtime: Technical error types

5. **Async by Default**
   - All domain logic is async
   - CLI layer bridges sync→async
   - Use `tokio::runtime::Runtime` for blocking

## Command Count

**Total Commands to Migrate: 77**

- template: generate, list, show, new, regenerate (5)
- marketplace: search, install, publish, list, update (5)
- graph: load, export, query, validate, stats, diff, snapshot (7)
- ai: generate, chat, refactor, analyze (4)
- project: init, build, test, clean (4)
- utils: doctor, env, config (3)
- shell: completion, init (2)
- ci: workflow, trigger, release, pages (4)
- ... and 43 more

**Estimated Effort:**
- 77 commands × 30 LOC = 2,310 LOC (CLI wrappers)
- 77 operations × 200 LOC = 15,400 LOC (Domain logic)
- Total: ~17,710 LOC

**Timeline:**
- v2.0: Core commands (20 commands, ~4,400 LOC)
- v2.1: Remaining commands (57 commands, ~13,300 LOC)
- v2.2: Remove deprecated commands/ module

## Examples

See existing implementations:
- `cli/src/cmds/shell/completion.rs` - Simple command
- `cli/src/domain/utils/doctor.rs` - Domain logic
- `cli/src/runtime.rs` - Async bridge utilities
