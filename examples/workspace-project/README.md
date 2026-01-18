# Workspace Project Example

This example demonstrates how to use `ggen.toml` in a multi-crate Rust workspace with hierarchical configuration.

## Project Structure

```
workspace-project/
├── ggen.toml                 # Root workspace configuration
├── Cargo.toml                # Rust workspace config
├── make.toml                 # Lifecycle tasks (cargo-make)
├── templates/                # Shared templates
│   ├── core/                 # Templates for core crate
│   ├── cli/                  # Templates for CLI crate
│   └── utils/                # Templates for utils crate
├── crates/                   # Workspace members
│   ├── core/
│   │   ├── ggen.toml         # Core crate config
│   │   ├── Cargo.toml
│   │   └── src/
│   ├── cli/
│   │   ├── ggen.toml         # CLI crate config
│   │   ├── Cargo.toml
│   │   └── src/
│   └── utils/
│       ├── ggen.toml         # Utils crate config
│       ├── Cargo.toml
│       └── src/
└── generated/                # Workspace-level generated code
```

## Configuration Hierarchy

### Root ggen.toml (Workspace)

The root `ggen.toml` defines workspace-wide settings:

- Global template directories
- Shared RDF prefixes
- Workspace lifecycle phases
- Performance and security settings

```toml
[project]
name = "workspace-project"
version = "1.0.0"

[workspace]
members = [
    "crates/core/ggen.toml",
    "crates/cli/ggen.toml",
    "crates/utils/ggen.toml"
]

[lifecycle.phases]
generate-all = ["generate-core", "generate-cli", "generate-utils"]
```

### Member ggen.toml (Per-Crate)

Each crate has its own `ggen.toml` with:

- Crate-specific project metadata
- Local template paths (relative to workspace root)
- Crate-specific RDF configuration
- Custom logging and features

**Example: crates/core/ggen.toml**
```toml
[project]
name = "workspace-core"
version = "1.0.0"

[templates]
source_dir = "../../templates/core"
output_dir = "src/generated"
```

## Usage

### 1. Initialize Workspace

```bash
cd examples/workspace-project

# Initialize root workspace
ggen project init

# Initialize each member crate
cd crates/core && ggen project init && cd ../..
cd crates/cli && ggen project init && cd ../..
cd crates/utils && ggen project init && cd ../..
```

### 2. Generate Code for Entire Workspace

```bash
# From workspace root
ggen project generate

# Or use lifecycle phases
cargo make generate-all
```

### 3. Generate Code for Specific Crate

```bash
# Generate for core crate only
cd crates/core
ggen project generate

# Or from workspace root with config flag
ggen project generate --config crates/core/ggen.toml
```

### 4. Build and Test

```bash
# Use lifecycle phases
cargo make build-all
cargo make test-all

# Or standard Cargo commands
cargo build --workspace
cargo test --workspace
```

## Configuration Patterns

### Pattern 1: Shared Templates

Templates are organized by crate in `templates/`:

```
templates/
├── core/           # Shared by all crates
│   ├── models.tmpl
│   └── traits.tmpl
├── cli/            # CLI-specific
│   ├── commands.tmpl
│   └── args.tmpl
└── utils/          # Utils-specific
    └── helpers.tmpl
```

### Pattern 2: Crate-Specific RDF

Each crate can define its own ontology:

**Root ggen.toml:**
```toml
[rdf]
base_uri = "https://example.com/workspace/"
prefixes = { ex = "https://example.com/workspace/" }
```

**crates/core/ggen.toml:**
```toml
[rdf]
base_uri = "https://example.com/workspace/core/"
prefixes = { ex = "https://example.com/workspace/core/" }
```

### Pattern 3: Lifecycle Coordination

**Root ggen.toml:**
```toml
[lifecycle.phases]
generate-all = ["generate-core", "generate-cli", "generate-utils"]
build-all = ["build-core", "build-cli", "build-utils"]
test-all = ["test-core", "test-cli", "test-utils"]
```

**make.toml (cargo-make):**
```toml
[tasks.generate-core]
cwd = "crates/core"
command = "ggen"
args = ["project", "generate"]

[tasks.generate-cli]
cwd = "crates/cli"
command = "ggen"
args = ["project", "generate"]

[tasks.generate-utils]
cwd = "crates/utils"
command = "ggen"
args = ["project", "generate"]
```

## Coexistence with Cargo Workspace

This example shows how `ggen.toml` complements Cargo workspace:

**Cargo.toml (workspace root):**
```toml
[workspace]
members = ["crates/core", "crates/cli", "crates/utils"]

[workspace.dependencies]
tokio = "1.0"
serde = "1.0"
```

**ggen.toml (workspace root):**
```toml
[project]
name = "workspace-project"

[workspace]
members = ["crates/*/ggen.toml"]

[templates]
source_dir = "templates"
```

**Separation of Concerns:**
- **Cargo.toml**: Dependencies, build configuration, Rust features
- **ggen.toml**: Code generation, templates, RDF, lifecycle
- **make.toml**: Build automation, task orchestration

## Benefits of Workspace Setup

1. **Shared Configuration**: Common settings in root `ggen.toml`
2. **Crate Isolation**: Each crate has its own generation config
3. **Coordinated Generation**: Lifecycle phases coordinate multi-crate generation
4. **Template Reuse**: Shared templates reduce duplication
5. **Knowledge Graph Integration**: Each crate can have its own ontology

## Advanced Features

### Multi-Environment Generation

Generate different code for different environments:

```bash
# Development
ggen project generate --config ggen.dev.toml

# Production
ggen project generate --config ggen.prod.toml
```

### Parallel Generation

Enable parallel generation in root `ggen.toml`:

```toml
[performance]
parallel_execution = true
max_workers = 4
```

### Dependency Tracking

Use lifecycle phases to manage crate dependencies:

```toml
[lifecycle.phases]
generate-cli = ["generate-core", "cli-templates"]
```

## Next Steps

1. Explore the [Simple Project Example](../simple-project/)
2. Read the [Workspace Documentation](../../docs/ggen-toml-guide.md#workspace-configuration)
3. Try generating code with `ggen project generate`
4. Experiment with custom templates
5. Add AI-powered generation with `[ai]` section

## Learn More

- [ggen.toml User Guide](../../docs/ggen-toml-guide.md)
- [Complete Reference](../../docs/ggen-toml-reference.md)
- [Migration Guide](../../docs/ggen-toml-migration.md)
- [Cargo Workspaces](https://doc.rust-lang.org/book/ch14-03-cargo-workspaces.html)
