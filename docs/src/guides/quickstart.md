# Quickstart

## Marketplace Workflow (Recommended)

The fastest way to get started with ggen is using marketplace gpacks - pre-built, tested template collections.

### 1. Search for Templates

```bash
# Search for CLI subcommand templates
ggen search rust cli

# Output:
# ID                                    LATEST     KIND       TAGS
# io.ggen.rust.cli-subcommand           0.2.1      template   rust, cli, clap
# io.ggen.rust.api-endpoint             0.1.5      template   rust, api, axum
```

### 2. Install a Gpack

```bash
# Install the latest version
ggen add io.ggen.rust.cli-subcommand

# Or install specific version
ggen add io.ggen.rust.cli-subcommand@0.2.0
```

### 3. Generate Code

```bash
# Use the installed gpack template
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello description="Print a greeting"
```

### 4. Verify Installation

```bash
# List installed gpacks
ggen packs

# Show template details
ggen show io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl
```

Result:

```
src/cmds/hello.rs
```

## Local Templates (Advanced)

For custom templates or when you need full control over the generation process:

### 1. Initialize Project Structure

```bash
mkdir -p templates/cli/subcommand
```

### 2. Create Template

Create `templates/cli/subcommand/rust.tmpl`:

```yaml
---
to: src/cmds/{{ slug }}.rs
vars: { cmd: hello, summary: "Print a greeting", seed: cosmos }
rdf:
  - "graphs/cli.ttl"              # Local RDF file
shape:
  - "graphs/shapes/cli.shacl.ttl" # Local SHACL shapes
sparql:
  vars:
    - name: slug
      query: |
        PREFIX cli: <urn:ggen:cli#>
        SELECT ?slug WHERE { ?c a cli:Command ; cli:slug ?slug } LIMIT 1
determinism: { seed: "{{ seed }}" }
---
pub fn {{ slug }}(name:&str){ println!("hello {}", name); }
```

### 3. Generate

```bash
ggen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

## Marketplace vs Local Templates

| Feature | Marketplace Gpacks | Local Templates |
|---------|-------------------|-----------------|
| **Setup Time** | Instant | Requires creation |
| **Quality** | Community tested | Custom quality |
| **Updates** | Automatic | Manual |
| **Dependencies** | Managed | Manual |
| **Versioning** | Semantic | Ad-hoc |
| **Best For** | Common patterns | Custom needs |

## Troubleshooting

### First-time Setup Issues

```bash
# If ggen command not found after installation
which ggen
# Should show path to ggen binary

# If marketplace search fails
ggen search --help
# Check network connectivity and registry access
```

### Template Not Found

```bash
# Check if gpack is installed
ggen packs

# Reinstall if missing
ggen add io.ggen.rust.cli-subcommand

# Verify template path
ggen show io.ggen.rust.cli-subcommand
```

### Generation Errors

```bash
# Use dry run to preview
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello --dry

# Check variable requirements
ggen show io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl
```
