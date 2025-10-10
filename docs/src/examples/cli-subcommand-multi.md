# Multi-language CLI subcommand

## Using Marketplace Gpacks (Recommended)

Generate the same CLI subcommand across multiple languages using curated gpacks.

### 1. Install Language-Specific Gpacks

```bash
# Install gpacks for different languages
ggen add io.ggen.rust.cli-subcommand
ggen add io.ggen.python.cli-subcommand
ggen add io.ggen.bash.cli-subcommand
ggen add io.ggen.go.cli-subcommand
```

### 2. Generate Across Languages

```bash
# Generate Rust CLI subcommand
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello description="Print a greeting"

# Generate Python CLI subcommand
ggen gen io.ggen.python.cli-subcommand:cli/subcommand/python.tmpl name=hello description="Print a greeting"

# Generate Bash CLI subcommand
ggen gen io.ggen.bash.cli-subcommand:cli/subcommand/bash.tmpl name=hello description="Print a greeting"

# Generate Go CLI subcommand
ggen gen io.ggen.go.cli-subcommand:cli/subcommand/go.tmpl name=hello description="Print a greeting"
```

### 3. Verify Deterministic Output

```bash
# Check that all outputs are consistent
ls -la src/cmds/hello.rs commands/hello.py commands/hello.sh cmd/hello.go

# Verify determinism by regenerating
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello description="Print a greeting"
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
ggen gen cli subcommand --vars cmd=hello summary="Print a greeting"
```

Produces, if templates exist:
```
src/cmds/hello.rs
commands/hello.py
commands/hello.sh
```

## Determinism Verification

### Gpack Version Locking

Gpacks ensure determinism through version locking:

```bash
# Check installed versions
ggen packs

# Output:
# ID                                    VERSION    KIND       TAGS
# io.ggen.rust.cli-subcommand           0.2.1      template   rust, cli, clap
# io.ggen.python.cli-subcommand         0.1.8      template   python, cli, click
# io.ggen.bash.cli-subcommand            0.1.2      template   bash, cli, getopts
```

### Lockfile Management

```bash
# View lockfile
cat ggen.lock

# Update to latest compatible versions
ggen update

# Verify determinism
ggen gen io.ggen.rust.cli-subcommand:cli/subcommand/rust.tmpl name=hello description="Print a greeting" --dry
```

## Cross-Language Consistency

All generated subcommands share the same semantic model:

- **Same RDF ontology** across all languages
- **Consistent variable binding** via SPARQL queries
- **Identical frontmatter** structure
- **Deterministic output** through version locking

## Best Practices

### Gpack Selection

```bash
# Search for multi-language gpacks
ggen search cli subcommand

# Look for gpacks with multiple language support
ggen show io.ggen.rust.cli-subcommand
```

### Version Management

```bash
# Pin specific versions for production
ggen add io.ggen.rust.cli-subcommand@0.2.1
ggen add io.ggen.python.cli-subcommand@0.1.8

# Update carefully
ggen update --dry  # Preview updates
ggen update        # Apply updates
```

### Testing Multi-Language Output

```bash
# Test all languages
for lang in rust python bash go; do
  ggen gen io.ggen.${lang}.cli-subcommand:cli/subcommand/${lang}.tmpl name=test description="Test command"
done

# Verify consistency
diff <(grep -o 'name.*test' src/cmds/test.rs) <(grep -o 'name.*test' commands/test.py)
```

Same RDF + seed + gpack versions ⇒ byte-identical outputs across all languages.
