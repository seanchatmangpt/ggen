# Critical Fixes Required for Ggen Marketplace

**Date:** 2025-10-30
**Priority:** CRITICAL - Production Blocker
**Estimated Effort:** 1-2 weeks for basic functionality

---

## TL;DR - The Bottom Line

**Current Status:** Ggen marketplace is **documentation-only**. The promised workflow doesn't work.

**The Problem:**
```bash
# Documentation says this should work:
ggen market search "rust web"     # ✅ Works (mock data)
ggen market add "rust-web-service" # ❌ FAILS
ggen lifecycle run init            # ❌ FAILS
ggen template generate service.tmpl # ❌ FAILS

# Reality: Nothing works without manual setup
```

**What Users Actually Need:**
```bash
# One command to go from zero to working project:
ggen new my-app --type rust-web --framework axum
cd my-app
cargo run
# ^ This should just work
```

---

## Critical Fix #1: Bootstrap Command (HIGHEST PRIORITY)

### Problem
Cannot create a new project from scratch. All commands require existing configuration.

### Solution
Implement `ggen new` command:

```rust
// cli/src/commands/new.rs
pub async fn new_project(args: NewArgs) -> Result<()> {
    let project = ProjectBootstrap::new(args.name)
        .with_type(args.project_type)
        .with_framework(args.framework)
        .with_features(args.features);

    // Create directory structure
    project.create_directory()?;

    // Generate make.toml
    project.generate_lifecycle_config()?;

    // Generate Cargo.toml or package.json
    project.generate_package_manifest()?;

    // Generate basic source files
    project.generate_initial_code()?;

    // Validate project builds
    project.validate()?;

    println!("✅ Project created: {}", args.name);
    println!("Run: cd {} && cargo run", args.name);

    Ok(())
}
```

### Usage
```bash
# Basic usage
ggen new my-app --type rust-web

# With options
ggen new my-api \
  --type rust-web \
  --framework axum \
  --features auth,database,docker \
  --database postgres

# Interactive mode
ggen new --interactive
```

### Expected Output
```
Creating project: my-app
  ✅ Created directory structure
  ✅ Generated Cargo.toml
  ✅ Generated src/main.rs
  ✅ Generated make.toml
  ✅ Generated README.md
  ✅ Running cargo check... OK

Project ready! Run:
  cd my-app
  cargo run
```

### Files Generated
```
my-app/
├── Cargo.toml          # Working package manifest
├── make.toml           # Lifecycle configuration
├── ggen.toml           # Ggen settings
├── README.md           # Project documentation
├── src/
│   ├── main.rs         # Working entry point
│   └── lib.rs          # Library code
└── tests/
    └── integration.rs  # Basic tests
```

### Acceptance Criteria
- [ ] Project directory created
- [ ] All files have actual code (not placeholders)
- [ ] `cargo build` succeeds
- [ ] `cargo test` succeeds
- [ ] `cargo run` starts server (for web projects)
- [ ] Generated project is immediately usable

---

## Critical Fix #2: Marketplace Registry (HIGH PRIORITY)

### Problem
No actual marketplace registry exists. All searches return mock data.

### Solution
Create and populate registry file:

```bash
# Create registry directory
mkdir -p ~/.ggen/registry

# Create registry.json
cat > ~/.ggen/registry/packages.json << 'EOF'
{
  "version": "1.0.0",
  "updated": "2025-10-30T00:00:00Z",
  "packages": [
    {
      "id": "rust-axum-service",
      "name": "Rust Axum Web Service",
      "description": "Production-ready Axum web service with authentication and database",
      "version": "1.0.0",
      "category": "web",
      "tags": ["rust", "web", "axum", "api"],
      "templates": [
        "service.tmpl",
        "handler.tmpl",
        "model.tmpl",
        "test.tmpl"
      ],
      "dependencies": ["postgresql-database", "redis-cache"],
      "source": "local:./marketplace/packages/rust-axum-service",
      "metadata": {
        "stars": 1200,
        "downloads": 45000,
        "health_score": 95,
        "last_updated": "2025-10-28"
      }
    },
    {
      "id": "rust-actix-service",
      "name": "Rust Actix Web Service",
      "description": "High-performance Actix-web service",
      "version": "1.0.0",
      "category": "web",
      "tags": ["rust", "web", "actix", "performance"],
      "templates": ["service.tmpl", "handler.tmpl"],
      "dependencies": [],
      "source": "local:./marketplace/packages/rust-actix-service",
      "metadata": {
        "stars": 890,
        "downloads": 32000,
        "health_score": 92
      }
    },
    {
      "id": "postgresql-database",
      "name": "PostgreSQL Database Integration",
      "description": "PostgreSQL database with migrations and connection pooling",
      "version": "1.0.0",
      "category": "database",
      "tags": ["database", "postgresql", "sql"],
      "templates": ["schema.tmpl", "migration.tmpl"],
      "dependencies": [],
      "source": "local:./marketplace/packages/postgresql-database"
    }
  ]
}
EOF
```

### Implementation
```rust
// ggen-core/src/marketplace/registry.rs
impl Registry {
    pub fn load() -> Result<Self> {
        let registry_path = Self::registry_path();

        if !registry_path.exists() {
            return Ok(Self::empty_with_defaults());
        }

        let content = std::fs::read_to_string(&registry_path)?;
        let registry: Registry = serde_json::from_str(&content)?;

        Ok(registry)
    }

    fn registry_path() -> PathBuf {
        dirs::home_dir()
            .expect("Failed to find home directory")
            .join(".ggen")
            .join("registry")
            .join("packages.json")
    }

    fn empty_with_defaults() -> Self {
        // Load from embedded defaults
        let defaults = include_str!("../../../registry/default-packages.json");
        serde_json::from_str(defaults)
            .expect("Failed to parse default registry")
    }
}
```

### Registry Update Command
```bash
# Update registry from remote
ggen market update

# Update from local source
ggen market update --source ./marketplace/packages

# Force refresh
ggen market update --force
```

### Acceptance Criteria
- [ ] Registry file created at `~/.ggen/registry/packages.json`
- [ ] Contains all example packages from `marketplace/packages/`
- [ ] `ggen market search` returns actual packages
- [ ] `ggen market add` can install packages
- [ ] Packages are usable after installation

---

## Critical Fix #3: Real Code Generation (HIGH PRIORITY)

### Problem
AI generates template files with frontmatter, not actual working code.

### Current Behavior
```rust
// Generated Cargo.toml
Frontmatter { to: Some("generated.tmpl"), ... }
---
Generated project structure content
```

### Expected Behavior
```toml
# Generated Cargo.toml
[package]
name = "my-app"
version = "0.1.0"
edition = "2021"

[dependencies]
axum = "0.7"
tokio = { version = "1", features = ["full"] }
serde = { version = "1", features = ["derive"] }
anyhow = "1"
```

### Solution
```rust
// ggen-ai/src/generation.rs
impl AIProjectGenerator {
    pub async fn generate(&self, spec: &ProjectSpec) -> Result<GeneratedProject> {
        let mut project = GeneratedProject::new(&spec.name);

        // Generate actual code, not templates
        let cargo_toml = self.generate_cargo_toml(spec)?;
        let main_rs = self.generate_main_rs(spec)?;
        let lib_rs = self.generate_lib_rs(spec)?;
        let readme = self.generate_readme(spec)?;

        // Add files with actual content
        project.add_file("Cargo.toml", cargo_toml);
        project.add_file("src/main.rs", main_rs);
        project.add_file("src/lib.rs", lib_rs);
        project.add_file("README.md", readme);

        // Validate generated code
        self.validate_project(&project)?;

        Ok(project)
    }

    fn generate_cargo_toml(&self, spec: &ProjectSpec) -> Result<String> {
        let template = CargoTomlTemplate::new(spec);
        template.render()
    }

    fn generate_main_rs(&self, spec: &ProjectSpec) -> Result<String> {
        match spec.framework {
            Framework::Axum => self.generate_axum_main(spec),
            Framework::Actix => self.generate_actix_main(spec),
            Framework::Warp => self.generate_warp_main(spec),
        }
    }

    fn validate_project(&self, project: &GeneratedProject) -> Result<()> {
        // Run cargo check
        let output = Command::new("cargo")
            .arg("check")
            .current_dir(&project.path)
            .output()?;

        if !output.status.success() {
            return Err(anyhow!("Generated project failed cargo check"));
        }

        // Run cargo test
        let output = Command::new("cargo")
            .arg("test")
            .current_dir(&project.path)
            .output()?;

        if !output.status.success() {
            return Err(anyhow!("Generated project tests failed"));
        }

        Ok(())
    }
}
```

### Acceptance Criteria
- [ ] `ggen ai project` generates actual Rust code
- [ ] Generated Cargo.toml is valid
- [ ] Generated src/main.rs compiles
- [ ] `cargo build` succeeds on generated project
- [ ] `cargo test` passes
- [ ] No frontmatter in generated files

---

## Critical Fix #4: Standalone Template Usage (MEDIUM PRIORITY)

### Problem
Templates require `make.toml` and complex task setup to use.

### Solution
Make templates self-contained:

```bash
# Should work without make.toml
ggen template apply /path/to/template.tmpl \
  --var name=my-service \
  --var port=3000 \
  --output ./my-service

# Or from marketplace
ggen template apply rust-axum-service:handler.tmpl \
  --var handler_name=create_user \
  --var path=/api/users \
  --output src/handlers/create_user.rs
```

### Implementation
```rust
// cli/src/commands/template.rs
pub async fn apply_template(args: ApplyArgs) -> Result<()> {
    let template = Template::load(&args.template)?;

    // Resolve variables
    let vars = args.vars
        .iter()
        .map(|v| {
            let parts: Vec<&str> = v.split('=').collect();
            (parts[0].to_string(), parts[1].to_string())
        })
        .collect();

    // Render template
    let rendered = template.render(vars)?;

    // Write output
    std::fs::write(&args.output, rendered)?;

    println!("✅ Generated: {}", args.output);

    Ok(())
}
```

### Acceptance Criteria
- [ ] Templates work without `make.toml`
- [ ] Can apply templates from marketplace
- [ ] Can apply templates from local files
- [ ] Variables properly substituted
- [ ] Output is valid code

---

## Implementation Timeline

### Week 1: Bootstrap and Registry (Critical)
**Days 1-2:** Implement `ggen new` command
- Project directory creation
- Basic file generation
- Validation

**Days 3-4:** Create marketplace registry
- Registry file format
- Package metadata
- Load/save functionality

**Day 5:** Integration and testing
- Test `ggen new` with registry
- Validate generated projects
- Fix bugs

### Week 2: Code Generation and Templates (High Priority)
**Days 1-2:** Fix AI code generation
- Generate actual code, not templates
- Add validation
- Test with different frameworks

**Days 3-4:** Standalone templates
- Template application without make.toml
- Variable substitution
- Marketplace integration

**Day 5:** End-to-end testing
- Complete workflow validation
- Documentation updates
- Bug fixes

---

## Success Metrics

### Before Fixes
- ❌ Cannot create new project
- ❌ Marketplace returns mock data
- ❌ AI generates non-functional templates
- ❌ Templates require complex setup
- **Success Rate:** 0% (no working workflow)

### After Fixes
- ✅ `ggen new my-app` creates working project
- ✅ `cargo build` succeeds immediately
- ✅ Marketplace search returns real packages
- ✅ `ggen market add` installs packages
- ✅ Templates work standalone
- **Target Success Rate:** 100% (complete workflow works)

---

## Testing Checklist

### Basic Workflow Test
```bash
# Should complete without errors
ggen new my-api --type rust-web --framework axum
cd my-api
cargo build
cargo test
cargo run

# Should see:
# Server listening on 0.0.0.0:3000
```

### Marketplace Test
```bash
# Should return real packages
ggen market search "web service"

# Should install successfully
ggen market add rust-axum-service

# Should list installed package
ggen market list
```

### Template Test
```bash
# Should generate working code
ggen template apply rust-axum-service:handler.tmpl \
  --var name=get_users \
  --output src/handlers/users.rs

# Should compile
cargo check
```

---

## Next Steps

1. **Immediate (Today):**
   - Review this document
   - Prioritize fixes
   - Assign implementation tasks

2. **This Week:**
   - Implement `ggen new` command
   - Create marketplace registry
   - Test basic workflow

3. **Next Week:**
   - Fix AI code generation
   - Enable standalone templates
   - Complete end-to-end testing

4. **Follow-up:**
   - Update documentation to match reality
   - Add more packages to marketplace
   - Implement incremental features (Phase 2)

---

## Conclusion

**The marketplace vision is sound, but the implementation is incomplete.**

With these 4 critical fixes, ggen would go from:
- ❌ Documentation-only prototype
- ✅ Functional development tool

**Estimated effort:** 10-15 days for working prototype
**Impact:** Unlocks the entire marketplace-first workflow

**Priority:** CRITICAL - These fixes are production blockers.

---

**Document Created:** 2025-10-30
**Status:** ACTION REQUIRED
**Owner:** Ggen Core Team
