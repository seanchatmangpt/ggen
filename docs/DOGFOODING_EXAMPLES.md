# Dogfooding Examples - Using ggen to Build ggen Projects

## Overview

This document demonstrates using ggen's own capabilities to generate production-ready Rust projects, following the 80/20 principle and core team best practices.

## Prerequisites

```bash
# Ensure ggen CLI is installed
cargo install --path cli

# Set up AI provider (choose one)
export OPENAI_API_KEY="your-key"
# OR
export ANTHROPIC_API_KEY="your-key"
# OR use mock for testing
```

## Example 1: Generate Advanced CLI Tool Using AI

```bash
# Use ggen AI to generate a complete CLI project
ggen ai project \
  --description "High-performance CLI tool for file processing with async I/O" \
  --name "proc-cli" \
  --language rust \
  --output examples/ai-generated/proc-cli \
  --tests \
  --docs \
  --ci

# Navigate to generated project
cd examples/ai-generated/proc-cli

# Initialize lifecycle (creates make.toml if not present)
cat > make.toml <<'EOF'
[lifecycle.init]
command = "echo 'Project initialized by ggen AI'"

[lifecycle.setup]
commands = [
    "rustup component add rustfmt clippy",
    "cargo fetch"
]

[lifecycle.build]
command = "cargo build --release"

[lifecycle.test]
command = "cargo test --all-features"

[lifecycle.deploy]
commands = [
    "cargo build --release",
    "mkdir -p ../../target/ai-generated",
    "cp target/release/proc-cli ../../target/ai-generated/"
]

[hooks]
before_build = ["format", "lint"]
after_build = ["test"]

[lifecycle.format]
command = "cargo fmt -- --check"

[lifecycle.lint]
command = "cargo clippy -- -D warnings"
EOF

# Run full lifecycle
ggen run build
ggen run test
ggen run deploy
```

## Example 2: Graph-Based Code Generation

```bash
# Create an RDF graph describing a Rust module
cat > examples/graphs/rust-module.ttl <<'EOF'
@prefix schema: <http://schema.org/> .
@prefix rust: <http://example.org/rust#> .

<#MyModule> a rust:Module ;
    rust:name "my_module" ;
    rust:hasFunction <#process_data> , <#validate_input> .

<#process_data> a rust:Function ;
    rust:name "process_data" ;
    rust:parameters "input: &str" ;
    rust:returns "Result<String>" ;
    rust:documentation "Process input data and return result" .

<#validate_input> a rust:Function ;
    rust:name "validate_input" ;
    rust:parameters "input: &str" ;
    rust:returns "bool" ;
    rust:documentation "Validate input meets requirements" .
EOF

# Use AI to generate code from graph
ggen ai graph \
  --input examples/graphs/rust-module.ttl \
  --output examples/ai-generated/graph-module/src/lib.rs \
  --format rust

# Generate SPARQL query to analyze the graph
ggen ai sparql \
  --description "Find all functions with their parameters and return types" \
  --graph examples/graphs/rust-module.ttl \
  --output examples/queries/functions.sparql
```

## Example 3: Template-Driven Development

```bash
# Generate a template for a Rust struct
ggen ai generate \
  --description "Generate a template for a configuration struct with validation" \
  --examples "name: String" "enabled: bool" "max_retries: u32" \
  --output templates/config-struct.tmpl \
  --validate \
  --max-iterations 3

# Use the template with lifecycle
cat > make.toml <<'EOF'
[lifecycle.codegen]
command = "ggen template apply templates/config-struct.tmpl --output src/config.rs"

[lifecycle.build]
commands = [
    "ggen run codegen",
    "cargo build"
]

[hooks]
before_build = ["codegen"]
EOF
```

## Example 4: Complete Workspace with Lifecycle

```bash
# Generate a multi-crate workspace
ggen ai project \
  --description "Monorepo with core library, CLI, and utils crates" \
  --name "workspace-example" \
  --language rust \
  --output examples/ai-generated/workspace \
  --tests \
  --docs \
  --mock

cd examples/ai-generated/workspace

# Create root make.toml with workspace support
cat > make.toml <<'EOF'
[workspace.core]
path = "crates/core"

[workspace.cli]
path = "crates/cli"

[workspace.utils]
path = "crates/utils"

[lifecycle.format]
command = "cargo fmt --all -- --check"
parallel = true

[lifecycle.lint]
command = "cargo clippy --all-targets -- -D warnings"
parallel = true

[lifecycle.build]
command = "cargo build --release"
parallel = true

[lifecycle.test]
command = "cargo test --all-features"
parallel = true

[lifecycle.bench]
command = "cargo bench --no-fail-fast"
parallel = false

[lifecycle.deploy]
commands = [
    "cargo build --release",
    "mkdir -p ../../target/workspace-release",
    "find target/release -maxdepth 1 -type f -executable -exec cp {} ../../target/workspace-release/ \\;"
]

[hooks]
before_all = ["format", "lint"]
before_build = ["format", "lint"]
after_build = ["test"]
before_deploy = ["build", "test", "bench"]

[env]
RUST_BACKTRACE = "1"
CARGO_INCREMENTAL = "1"
EOF

# Run parallel workspace build
ggen run build  # Builds all workspaces in parallel
ggen run test   # Tests all workspaces in parallel
```

## Example 5: AI-Powered Development Workflow

```bash
# Complete AI-driven development cycle
PROJECT_NAME="smart-parser"

# Step 1: Generate project structure
ggen ai project \
  --description "Parser for custom DSL with error recovery" \
  --name "$PROJECT_NAME" \
  --language rust \
  --framework "nom parser combinators" \
  --output "examples/ai-generated/$PROJECT_NAME" \
  --tests \
  --docs \
  --ci

cd "examples/ai-generated/$PROJECT_NAME"

# Step 2: Generate core parsing logic template
ggen ai generate \
  --description "Parser combinator for DSL tokens with error recovery" \
  --examples "keyword: Token::Keyword" "identifier: Token::Ident" \
  --output "templates/parser.tmpl" \
  --validate

# Step 3: Generate test cases
ggen ai generate \
  --description "Property-based tests for parser with QuickCheck" \
  --output "templates/parser-tests.tmpl"

# Step 4: Create lifecycle with AI integration
cat > make.toml <<'EOF'
[lifecycle.generate-code]
commands = [
    "ggen ai generate --description 'AST node definitions' --output src/ast.rs",
    "ggen ai generate --description 'Lexer implementation' --output src/lexer.rs",
    "ggen ai generate --description 'Parser implementation' --output src/parser.rs"
]

[lifecycle.generate-tests]
command = "ggen ai generate --description 'Integration tests' --output tests/integration.rs"

[lifecycle.build]
commands = [
    "ggen run generate-code",
    "cargo build --release"
]

[lifecycle.test]
commands = [
    "ggen run generate-tests",
    "cargo test --all-features"
]

[lifecycle.bench]
command = "cargo bench --bench parsing_bench"

[lifecycle.doc]
commands = [
    "cargo doc --no-deps",
    "ggen ai generate --description 'API documentation' --output docs/API.md"
]

[hooks]
before_build = ["generate-code"]
before_test = ["generate-tests"]

[env]
RUST_LOG = "debug"
EOF

# Step 5: Run full lifecycle
ggen run build
ggen run test
ggen run doc
```

## Example 6: Graph Query-Driven Development

```bash
# Create knowledge graph of system architecture
cat > architecture.ttl <<'EOF'
@prefix arch: <http://example.org/architecture#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

<#System> a arch:System ;
    arch:hasComponent <#Parser> , <#Evaluator> , <#CodeGen> .

<#Parser> a arch:Component ;
    rdfs:label "Parser Component" ;
    arch:dependsOn <#Lexer> ;
    arch:produces <#AST> .

<#Evaluator> a arch:Component ;
    rdfs:label "Evaluator Component" ;
    arch:dependsOn <#AST> ;
    arch:produces <#IR> .

<#CodeGen> a arch:Component ;
    rdfs:label "Code Generator" ;
    arch:dependsOn <#IR> ;
    arch:produces <#Output> .
EOF

# Generate SPARQL to find dependency order
ggen ai sparql \
  --description "Find topological order of components based on dependencies" \
  --graph architecture.ttl \
  --output queries/build-order.sparql

# Use query results to generate build scripts
cat > make.toml <<'EOF'
[lifecycle.analyze-deps]
command = "ggen query execute queries/build-order.sparql --format json --output build-order.json"

[lifecycle.generate-build]
command = "ggen ai generate --description 'Build script from dependency graph' --examples 'build-order.json' --output build.sh"

[lifecycle.build]
commands = [
    "ggen run analyze-deps",
    "ggen run generate-build",
    "chmod +x build.sh",
    "./build.sh"
]
EOF
```

## Example 7: Benchmarking with Lifecycle

```bash
# Generate performance-critical code
ggen ai generate \
  --description "Optimized hash table implementation with SIMD" \
  --examples "ahash for hashing" "rayon for parallelism" \
  --output src/hashtable.rs \
  --validate

# Create performance-focused lifecycle
cat > make.toml <<'EOF'
[lifecycle.bench-baseline]
command = "cargo bench --bench hashtable -- --save-baseline baseline"

[lifecycle.optimize]
commands = [
    "RUSTFLAGS='-C target-cpu=native' cargo build --release",
    "cargo bench --bench hashtable -- --baseline baseline"
]

[lifecycle.profile]
command = "cargo flamegraph --bench hashtable -- --bench"

[lifecycle.validate-perf]
command = "cargo criterion --message-format json | jq '.reason'"

[env]
RUSTFLAGS = "-C target-cpu=native -C opt-level=3"
CARGO_PROFILE_RELEASE_LTO = "fat"
EOF

ggen run bench-baseline
ggen run optimize
ggen run profile
```

## Best Practices Demonstrated

### 1. AI-First Development
- Use `ggen ai project` to bootstrap projects
- Use `ggen ai generate` for code templates
- Leverage validation with `--validate` flag
- Iterate with `--max-iterations` for quality

### 2. Graph-Driven Architecture
- Model system architecture in RDF
- Generate SPARQL queries for analysis
- Use graphs to drive code generation
- Maintain single source of truth

### 3. Lifecycle Integration
- Define clear lifecycle phases
- Use hooks for automation
- Enable parallel execution for workspaces
- Cache-aware builds with `cache_key`

### 4. Production Readiness
- Include tests (`--tests` flag)
- Generate documentation (`--docs` flag)
- Add CI/CD (`--ci` flag)
- Validate with `--validate`

## 80/20 Quick Wins

### 20% Effort, 80% Value

1. **Project Bootstrap** (5 minutes)
   ```bash
   ggen ai project --description "Your idea" --name project --mock --tests --docs
   ```

2. **Template Generation** (2 minutes)
   ```bash
   ggen ai generate --description "What you need" --validate
   ```

3. **Lifecycle Setup** (3 minutes)
   ```bash
   cp examples/make.toml.template make.toml
   ggen run build
   ```

4. **Parallel Workspace** (5 minutes)
   ```bash
   # Add to make.toml:
   [workspace.crate1]
   path = "crates/crate1"

   [lifecycle.build]
   parallel = true
   ```

## Troubleshooting

### AI Commands Not Working
```bash
# Check AI configuration
ggen ai config show

# Use mock client for testing
ggen ai generate --description "test" --mock

# Set provider explicitly
export GGEN_AI_PROVIDER=openai
```

### Lifecycle Execution Issues
```bash
# Check make.toml syntax
ggen validate make.toml

# Run with debug output
RUST_LOG=debug ggen run build

# Check workspace paths
ggen workspace list
```

### Performance Issues
```bash
# Enable parallel execution
[lifecycle.build]
parallel = true

# Use caching
[lifecycle.build]
cache_key = "{{ hash('Cargo.lock') }}"

# Limit threads
[env]
RAYON_NUM_THREADS = "8"
```

## Next Steps

1. Review generated examples in `examples/ai-generated/`
2. Customize make.toml for your workflows
3. Add custom lifecycle phases
4. Integrate with CI/CD pipelines
5. Publish successful patterns to marketplace

## Related Documentation

- [Lifecycle System Design](LIFECYCLE_SYSTEM_DESIGN.md)
- [Production Readiness](PRODUCTION_READINESS_8020.md)
- [AI Integration Guide](../ggen-ai/README.md)
- [Graph Processing](../ggen-core/README.md)
