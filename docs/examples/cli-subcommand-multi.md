# Multi-language CLI subcommand

## Using Marketplace Rpacks (Recommended)

Generate the same CLI subcommand across multiple languages using curated rpacks.

### 1. Install Language-Specific Rpacks

```bash
# Install rpacks for different languages
rgen add io.rgen.rust.cli-subcommand
rgen add io.rgen.python.cli-subcommand
rgen add io.rgen.bash.cli-subcommand
rgen add io.rgen.go.cli-subcommand
```

### 2. Generate Across Languages

```bash
# Generate Rust CLI subcommand
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello description="Print a greeting"

# Generate Python CLI subcommand
rgen gen io.rgen.python.cli-subcommand:cli/subcommand/python.tmpl name=hello description="Print a greeting"

# Generate Bash CLI subcommand
rgen gen io.rgen.bash.cli-subcommand:cli/subcommand/bash.tmpl name=hello description="Print a greeting"

# Generate Go CLI subcommand
rgen gen io.rgen.go.cli-subcommand:cli/subcommand/go.tmpl name=hello description="Print a greeting"
```

### 3. Verify Deterministic Output

```bash
# Check that all outputs are consistent
ls -la src/cmds/hello.rs commands/hello.py commands/hello.sh cmd/hello.go

# Verify determinism by regenerating
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello description="Print a greeting"
# Should produce identical output
```

Produces:
```
src/cmds/hello.rs
commands/hello.py
commands/hello.sh
cmd/hello.go
```

## Using Local Templates (Advanced)

For custom multi-language generation using local templates:

```bash
rgen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

Produces, if templates exist:
```
src/cmds/hello.rs
commands/hello.py
commands/hello.sh
```

## Determinism Verification

### Rpack Version Locking

Rpacks ensure determinism through version locking:

```bash
# Check installed versions
rgen packs

# Output:
# ID                                    VERSION    KIND       TAGS
# io.rgen.rust.cli-subcommand           0.2.1      template   rust, cli, clap
# io.rgen.python.cli-subcommand         0.1.8      template   python, cli, click
# io.rgen.bash.cli-subcommand            0.1.2      template   bash, cli, getopts
```

### Lockfile Management

```bash
# View lockfile
cat rgen.lock

# Update to latest compatible versions
rgen update

# Verify determinism
rgen gen io.rgen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello description="Print a greeting" --dry
```

## Cross-Language Consistency

All generated subcommands share the same semantic model:

- **Same RDF ontology** across all languages
- **Consistent variable binding** via SPARQL queries
- **Identical frontmatter** structure
- **Deterministic output** through version locking

## Best Practices

### Rpack Selection

```bash
# Search for multi-language rpacks
rgen search cli subcommand

# Look for rpacks with multiple language support
rgen show io.rgen.rust.cli-subcommand
```

### Version Management

```bash
# Pin specific versions for production
rgen add io.rgen.rust.cli-subcommand@0.2.1
rgen add io.rgen.python.cli-subcommand@0.1.8

# Update carefully
rgen update --dry  # Preview updates
rgen update        # Apply updates
```

### Testing Multi-Language Output

```bash
# Test all languages
for lang in rust python bash go; do
  rgen gen io.rgen.${lang}.cli-subcommand:cli/subcommand/${lang}.tmpl name=test description="Test command"
done

# Verify consistency
diff <(grep -o 'name.*test' src/cmds/test.rs) <(grep -o 'name.*test' commands/test.py)
```

Same RDF + seed + rpack versions ⇒ byte-identical outputs across all languages.
