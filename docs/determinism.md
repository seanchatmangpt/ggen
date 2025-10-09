# Determinism

rgen ensures deterministic, reproducible code generation through manifest hashing and version locking.

## Manifest Key Calculation

For local templates:
```
K = SHA256(seed || graph_hash || shapes_hash || frontmatter_hash || rows_hash)
```

For marketplace rpacks:
```
K = SHA256(seed || rpack_version || rpack_deps_hash || graph_hash || shapes_hash || frontmatter_hash || rows_hash)
```

## Hash Components

### Local Templates
- **Graph hash**: sorted N-Quads from RDF sources
- **Shapes hash**: sorted N-Quads from SHACL validation
- **Frontmatter hash**: rendered header + body bytes
- **Rows hash**: ordered serialization of matrix rows

### Marketplace Rpacks
- **Rpack version**: exact version from `rgen.toml`
- **Rpack deps hash**: hash of all dependency versions
- **Graph hash**: sorted N-Quads from rpack RDF sources
- **Shapes hash**: sorted N-Quads from rpack SHACL validation
- **Frontmatter hash**: rendered header + body bytes
- **Rows hash**: ordered serialization of matrix rows

## Version Locking

### Rpack Version Management

```bash
# Install specific version
rgen add io.rgen.rust.cli-subcommand@0.2.1

# Check installed versions
rgen packs

# View lockfile
cat rgen.lock
```

### Lockfile Structure

```toml
[lockfile]
version = "1.0"

[rpacks]
"io.rgen.rust.cli-subcommand" = "0.2.1"
"io.rgen.macros.std" = "0.2.0"

[dependencies]
"io.rgen.rust.cli-subcommand" = {
    version = "0.2.1",
    source = "registry",
    checksum = "sha256:abc123..."
}
```

## Deterministic Behavior

### Same Inputs → Identical Outputs

```bash
# First generation
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello

# Second generation (same inputs)
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello

# Outputs are byte-identical
diff src/cmds/hello.rs src/cmds/hello.rs
# No differences
```

### Cross-Language Consistency

```bash
# Generate across languages with same inputs
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello
rgen gen io.rgen.python.cli-subcommand:cli/subcommand/python.tmpl name=hello
rgen gen io.rgen.bash.cli-subcommand:cli/subcommand/bash.tmpl name=hello

# All outputs share the same semantic model
# Variable binding is consistent across languages
```

## Rpack Dependencies and Determinism

### Dependency Resolution

```bash
# Install rpack with dependencies
rgen add io.rgen.rust.cli-subcommand

# Dependencies are automatically resolved
rgen packs

# Output:
# ID                                    VERSION    KIND       TAGS
# io.rgen.rust.cli-subcommand           0.2.1      template   rust, cli, clap
# io.rgen.macros.std                    0.2.0      macro      rust, macros
```

### Dependency Hashing

Dependencies affect determinism through their versions:

```bash
# Check dependency versions
rgen show io.rgen.rust.cli-subcommand --deps

# Update dependencies
rgen update

# Verify determinism after update
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello --dry
```

## Ensuring Determinism

### Rpack Development

```bash
# Pin versions in development
rgen add io.rgen.rust.cli-subcommand@0.2.1

# Test determinism
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=test1
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=test1
# Should produce identical output
```

### Production Deployment

```bash
# Lock versions for production
rgen add io.rgen.rust.cli-subcommand@0.2.1
rgen add io.rgen.python.cli-subcommand@0.1.8

# Commit lockfile
git add rgen.lock
git commit -m "Lock rpack versions for production"

# Deploy with locked versions
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=api
```

## Troubleshooting Determinism

### Non-Deterministic Output

```bash
# Check for version changes
rgen packs

# Verify lockfile
cat rgen.lock

# Check for dependency updates
rgen update --dry
```

### Manifest Key Changes

```bash
# Enable tracing to see hash components
RGEN_TRACE=1 rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello

# Output shows:
# Manifest key: sha256:abc123...
# Rpack version: 0.2.1
# Graph hash: sha256:def456...
# Frontmatter hash: sha256:ghi789...
```

### Cross-Environment Consistency

```bash
# Ensure same rpack versions across environments
rgen add io.rgen.rust.cli-subcommand@0.2.1

# Verify environment consistency
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello --dry
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

Same inputs + same rpack versions + same dependencies ⇒ byte-identical outputs across all environments.
