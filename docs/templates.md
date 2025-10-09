# Templates

rgen supports two types of templates: **marketplace rpacks** and **local templates**. Marketplace rpacks are recommended for most use cases, while local templates provide full customization control.

## Marketplace Rpacks

Marketplace rpacks are curated, versioned template collections published to the rgen registry.

### Rpack Structure

```
<rpack-id>/
├── rgen.toml          # Rpack manifest
├── templates/          # Template files
│   └── cli/
│       └── subcommand/
│           └── rust.tmpl
├── macros/            # Reusable template fragments
│   └── common.tera
├── graphs/            # RDF graphs
│   └── cli.ttl
└── tests/             # Golden tests
    └── test_hello.rs
```

### Rpack Manifest (`rgen.toml`)

```toml
[rpack]
id = "io.rgen.rust.cli-subcommand"
name = "Rust CLI subcommand"
version = "0.2.1"
description = "Generate clap subcommands"
license = "MIT"
rgen_compat = ">=0.2 <0.4"

[dependencies]
"io.rgen.macros.std" = "^0.2"

[templates]
entrypoints = ["cli/subcommand/rust.tmpl"]
includes   = ["macros/**/*.tera"]

[rdf]
base = "http://example.org/"
prefixes.ex = "http://example.org/"
files  = ["graphs/*.ttl"]
inline = ["@prefix ex: <http://example.org/> . ex:Foo a ex:Type ."]
```

### Using Rpack Templates

```bash
# Install rpack
rgen add io.rgen.rust.cli-subcommand

# Use template
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello
```

## Local Templates

Local templates are stored in your project's `templates/` directory.

### Location
```
templates/<scope>/<action>/*.tmpl
```

### Structure
- Frontmatter (YAML) header
- Body (rendered)

### Common Keys
- `to:` output path
- `vars:` defaults
- `rdf:` includes/inline graphs
- `shape:` SHACL shape files
- `sparql.vars:` single-value bindings
- `sparql.matrix:` fan-out rows
- `determinism:` seed, sort key

### Using Local Templates

```bash
# Generate from local template
rgen gen cli subcommand --vars cmd=hello summary="Print greeting"
```

## Template Discovery Order

rgen searches for templates in this order:

1. **Installed rpacks** (from `.rgen/rpacks/`)
2. **Local templates** (from `templates/`)

If both exist, rpack templates take precedence.

## Template Reference Syntax

### Rpack Templates
```
<rpack-id>:<template-path>
```

Examples:
- `io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl`
- `io.rgen.python.api:api/endpoint/fastapi.tmpl`

### Local Templates
```
<scope> <action>
```

Examples:
- `cli subcommand`
- `api endpoint`

## Rpack Dependencies

Rpacks can depend on other rpacks for shared functionality:

```toml
[dependencies]
"io.rgen.macros.std" = "^0.2"
"io.rgen.common.rdf" = "~0.1.0"
```

Dependencies are automatically resolved when installing rpacks.

## Versioning and Compatibility

### Rpack Versioning
- Follows semantic versioning (semver)
- Specified in `rgen.toml` manifest
- Locked in `rgen.lock` file

### Compatibility
- `rgen_compat` field specifies required rgen version
- Dependency resolution follows semver rules
- Updates respect compatibility constraints

## Template Development

### For Rpack Authors

```bash
# Initialize new rpack
rgen pack init

# Add templates
mkdir -p templates/cli/subcommand
# Create template files...

# Test rpack
rgen pack test

# Lint for publishing
rgen pack lint

# Publish to registry
rgen pack publish
```

### For Local Template Authors

```bash
# Create template structure
mkdir -p templates/cli/subcommand

# Create template file
cat > templates/cli/subcommand/rust.tmpl << 'EOF'
---
to: src/cmds/{{ name }}.rs
vars:
  name: hello
---
pub fn {{ name }}() {
    println!("Hello from {{ name }}!");
}
EOF

# Test template
rgen gen cli subcommand --vars name=world
```
