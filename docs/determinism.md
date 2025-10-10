# Determinism

ggen ensures deterministic, reproducible code generation through manifest hashing and version locking.

## Manifest Key Calculation

For local templates:
```
K = SHA256(seed || graph_hash || shapes_hash || frontmatter_hash || rows_hash)
```

For marketplace gpacks:
```
K = SHA256(seed || gpack_version || gpack_deps_hash || graph_hash || shapes_hash || frontmatter_hash || rows_hash)
```

## Hash Components

### Local Templates
- **Graph hash**: sorted N-Quads from RDF sources
- **Shapes hash**: sorted N-Quads from SHACL validation
- **Frontmatter hash**: rendered header + body bytes
- **Rows hash**: ordered serialization of matrix rows

### Marketplace Gpacks
- **Gpack version**: exact version from `ggen.toml`
- **Gpack deps hash**: hash of all dependency versions
- **Graph hash**: sorted N-Quads from gpack RDF sources
- **Shapes hash**: sorted N-Quads from gpack SHACL validation
- **Frontmatter hash**: rendered header + body bytes
- **Rows hash**: ordered serialization of matrix rows

## Version Locking

### Gpack Version Management

```bash
# Install specific version
ggen add io.ggen.rust.cli-subcommand@0.2.1

# Check installed versions
ggen packs

# View lockfile
cat ggen.lock
```

### Lockfile Structure

```toml
[lockfile]
version = "1.0"

[gpacks]
"io.ggen.rust.cli-subcommand" = "0.2.1"
"io.ggen.macros.std" = "0.2.0"

[dependencies]
"io.ggen.rust.cli-subcommand" = {
    version = "0.2.1",
    source = "registry",
    checksum = "sha256:abc123..."
}
```

## Deterministic Behavior

### Same Inputs → Identical Outputs

```bash
# First generation
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello

# Second generation (same inputs)
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello

# Outputs are byte-identical
diff src/cmds/hello.rs src/cmds/hello.rs
# No differences
```

### Cross-Language Consistency

```bash
# Generate across languages with same inputs
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello
ggen gen io.ggen.python.cli-subcommand:cli/subcommand/python.tmpl name=hello
ggen gen io.ggen.bash.cli-subcommand:cli/subcommand/bash.tmpl name=hello

# All outputs share the same semantic model
# Variable binding is consistent across languages
```

## Gpack Dependencies and Determinism

### Dependency Resolution

```bash
# Install gpack with dependencies
ggen add io.ggen.rust.cli-subcommand

# Dependencies are automatically resolved
ggen packs

# Output:
# ID                                    VERSION    KIND       TAGS
# io.ggen.rust.cli-subcommand           0.2.1      template   rust, cli, clap
# io.ggen.macros.std                    0.2.0      macro      rust, macros
```

### Dependency Hashing

Dependencies affect determinism through their versions:

```bash
# Check dependency versions
ggen show io.ggen.rust.cli-subcommand --deps

# Update dependencies
ggen update

# Verify determinism after update
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello --dry
```

## Ensuring Determinism

### Gpack Development

```bash
# Pin versions in development
ggen add io.ggen.rust.cli-subcommand@0.2.1

# Test determinism
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=test1
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=test1
# Should produce identical output
```

### Production Deployment

```bash
# Lock versions for production
ggen add io.ggen.rust.cli-subcommand@0.2.1
ggen add io.ggen.python.cli-subcommand@0.1.8

# Commit lockfile
git add ggen.lock
git commit -m "Lock gpack versions for production"

# Deploy with locked versions
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=api
```

## Troubleshooting Determinism

### Non-Deterministic Output

```bash
# Check for version changes
ggen packs

# Verify lockfile
cat ggen.lock

# Check for dependency updates
ggen update --dry
```

### Manifest Key Changes

```bash
# Enable tracing to see hash components
GGEN_TRACE=1 ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello

# Output shows:
# Manifest key: sha256:abc123...
# Gpack version: 0.2.1
# Graph hash: sha256:def456...
# Frontmatter hash: sha256:ghi789...
```

### Cross-Environment Consistency

```bash
# Ensure same gpack versions across environments
ggen add io.ggen.rust.cli-subcommand@0.2.1

# Verify environment consistency
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello --dry
```

## Best Practices

### Version Pinning
- Pin specific versions for production
- Use semantic versioning
- Test updates before applying
- Maintain lockfile consistency

### Dependency Management
- Minimize dependency depth
- Use stable dependency versions
- Test dependency updates
- Document version requirements

### Deterministic Testing
- Test with multiple variable sets
- Verify cross-language consistency
- Use golden tests for validation
- Monitor manifest key changes

Same inputs + same gpack versions + same dependencies ⇒ byte-identical outputs across all environments.
