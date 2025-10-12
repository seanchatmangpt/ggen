# ggen Lifecycle System Documentation

This directory contains the complete design and architecture for ggen's universal lifecycle system.

## Documents

### Core Design
- **[LIFECYCLE_SYSTEM_DESIGN.md](./LIFECYCLE_SYSTEM_DESIGN.md)** - Complete vision and design document
- **[make-toml-complete-example.md](./make-toml-complete-example.md)** - Real-world example: Nuxt 4 + Rust

### PlantUML Diagrams

#### Architecture Diagrams
- **[lifecycle-architecture.puml](./lifecycle-architecture.puml)** - System architecture overview
- **[template-simplicity.puml](./template-simplicity.puml)** - Why templates stay simple

#### Flow Diagrams
- **[lifecycle-flow.puml](./lifecycle-flow.puml)** - Execution flow from command to generation

#### Vision Diagrams
- **[make-toml-rust-equivalent.puml](./make-toml-rust-equivalent.puml)** - Why make.toml is the Rust equivalent of package.json
- **[nuxt-with-rust-example.puml](./nuxt-with-rust-example.puml)** - Cross-language project example
- **[adoption-strategy.puml](./adoption-strategy.puml)** - Path to universal adoption

## Viewing Diagrams

### Online (Recommended)
Visit [PlantUML Online Server](http://www.plantuml.com/plantuml/uml/) and paste the contents of any `.puml` file.

### VS Code
Install the [PlantUML extension](https://marketplace.visualstudio.com/items?itemName=jebbs.plantuml) and use `Alt+D` to preview.

### Command Line
```bash
# Install PlantUML
brew install plantuml  # macOS
sudo apt install plantuml  # Ubuntu

# Generate PNG
plantuml lifecycle-architecture.puml
```

### Generate All Diagrams
```bash
# Generate all diagrams as PNG
cd docs
for file in *.puml; do
    plantuml "$file"
done
```

## Key Concepts

### 1. Universal Lifecycle Phases
Standard phases that work across all frameworks:
- `init` - Project initialization
- `setup` - Dependency installation
- `dev` - Development server
- `build` - Production build
- `test` - Run tests
- `deploy` - Deployment

### 2. make.toml
The universal project descriptor (like `package.json` but for all languages):
```toml
[project]
name = "my-app"

[workspace.frontend]
path = "apps/web"
framework = "nuxt"
runtime = "node:20"

[workspace.backend]
path = "apps/api"
framework = "axum"
runtime = "rust:1.75"

[lifecycle.dev]
command = "ggen parallel:dev"
workspaces = ["frontend", "backend"]
```

### 3. Simple Templates
Templates focus on code generation, hooks handled by make.toml:
```yaml
---
to: "components/{{name}}.vue"
sh_before: "ggen hooks:before generate:component {{name}}"
sh_after: "ggen hooks:after generate:component {{name}}"
---
<template>
  <div>{{ name }}</div>
</template>
```

### 4. Hooks System
Before/after hooks for any phase:
```toml
[hooks]
before_build = ["test", "lint"]
after_build = ["analyze:bundle"]
```

### 5. State Management
Track what's been generated, enable rollbacks:
```json
{
  "phases_run": ["init", "build"],
  "generated": [...],
  "migrations": [...]
}
```

## Why This Matters

### For Developers
- **One mental model** across all frameworks
- **Cross-language projects** made simple
- **Type safety** between languages
- **Consistent commands** everywhere

### For Framework Authors
- **Standard interface** for tooling
- **Lower barrier** to adoption
- **Ecosystem effects** from shared tooling
- **AI-understandable** metadata

### For The Industry
- **Universal standard** like package.json
- **Framework-agnostic** CI/CD
- **Polyglot teams** using same tools
- **Knowledge transfer** between ecosystems

## Example: Nuxt + Rust Project

```bash
# Initialize
ggen init my-app --template fullstack-rust

# Generate across languages
ggen generate:api users
# Creates: Rust route + Nuxt composable + TypeScript types

# Development
ggen dev  # Starts both Nuxt + Rust, types synced

# Build & Deploy
ggen build --env production
ggen deploy
```

## Implementation Status

### ✅ Completed (v1.0-1.1)
- Template system with frontmatter
- RDF/SPARQL integration
- Basic hooks (sh_before, sh_after)
- State tracking

### 🚧 In Progress (v1.2)
- [ ] make.toml parser
- [ ] Lifecycle runner
- [ ] Hook executor
- [ ] Workspace support
- [ ] Environment management

### 📋 Planned (v1.3+)
- [ ] Framework adapters (Nuxt, Next, Rust)
- [ ] Cross-language type sync
- [ ] IDE integrations
- [ ] GitHub Actions support

## Contributing

See the main design document for architecture decisions. Key principles:

1. **Templates stay simple** - Only sh_before/sh_after
2. **make.toml orchestrates** - All complex logic here
3. **Universal by design** - Works with any language
4. **Semantic-first** - RDF metadata throughout
5. **State-aware** - Track everything, enable rollbacks

## Questions?

- Open an issue for design questions
- See [LIFECYCLE_SYSTEM_DESIGN.md](./LIFECYCLE_SYSTEM_DESIGN.md) for complete vision
- See [make-toml-complete-example.md](./make-toml-complete-example.md) for practical example

---

**The future is cross-language, and ggen is the interface.**
