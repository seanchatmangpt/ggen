# Templates

ggen supports two types of templates: **marketplace gpacks** and **local templates**. Marketplace gpacks are recommended for most use cases, while local templates provide full customization control.

## Marketplace Gpacks

Marketplace gpacks are curated, versioned template collections published to the ggen registry.

### Gpack Structure

```
<gpack-id>/
├── ggen.toml          # Gpack manifest
├── templates/          # Template files
│   └── cli/
│       └── subcommand/
│           ├── rust.tmpl
│           ├── graphs/         # Local RDF data
│           │   ├── cli.ttl
│           │   └── shapes/
│           │       └── cli.shacl.ttl
│           └── queries/        # Local SPARQL queries
│               └── commands.rq
├── macros/            # Reusable template fragments
│   └── common.tera
└── tests/             # Golden tests
    └── test_hello.rs
```

### Gpack Manifest (`ggen.toml`)

```toml
[gpack]
id = "io.ggen.rust.cli-subcommand"
name = "Rust CLI subcommand"
version = "0.2.1"
description = "Generate clap subcommands"
license = "MIT"
ggen_compat = ">=0.2 <0.4"

[dependencies]
"io.ggen.macros.std" = "^0.2"

[templates]
entrypoints = ["cli/subcommand/rust.tmpl"]
includes   = ["macros/**/*.tera"]

[rdf]
base = "http://example.org/"
prefixes.ex = "http://example.org/"
files  = ["templates/**/graphs/*.ttl"]
inline = ["@prefix ex: <http://example.org/> . ex:Foo a ex:Type ."]
```

### Using Gpack Templates

```bash
# Install gpack
ggen add io.ggen.rust.cli-subcommand

# Use template
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello
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
ggen gen cli subcommand --vars cmd=hello summary="Print greeting"
```

## Template Discovery Order

ggen searches for templates in this order:

1. **Installed gpacks** (from `.ggen/gpacks/`)
2. **Local templates** (from `templates/`)

If both exist, gpack templates take precedence.

## Template Reference Syntax

### Gpack Templates
```
<gpack-id>:<template-path>
```

Examples:
- `io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl`
- `io.ggen.python.api:api/endpoint/fastapi.tmpl`

### Local Templates
```
<scope> <action>
```

Examples:
- `cli subcommand`
- `api endpoint`

## Gpack Dependencies

Gpacks can depend on other gpacks for shared functionality:

```toml
[dependencies]
"io.ggen.macros.std" = "^0.2"
"io.ggen.common.rdf" = "~0.1.0"
```

Dependencies are automatically resolved when installing gpacks.

## Versioning and Compatibility

### Gpack Versioning
- Follows semantic versioning (semver)
- Specified in `ggen.toml` manifest
- Locked in `ggen.lock` file

### Compatibility
- `ggen_compat` field specifies required ggen version
- Dependency resolution follows semver rules
- Updates respect compatibility constraints

## Template Development

### For Gpack Authors

```bash
# Initialize new gpack
ggen pack init

# Add templates
mkdir -p templates/cli/subcommand
# Create template files...

# Test gpack
ggen pack test

# Lint for publishing
ggen pack lint

# Publish to registry
ggen pack publish
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
ggen gen cli subcommand --vars name=world
```
