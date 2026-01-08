# ggen PaaS CLI Reference

Auto-generated from `.specify/cli-commands.ttl`
Last Updated: 2026-01-08

## Overview

The ggen PaaS command provides semantic noun-verb interface for managing RDF-driven infrastructure.

**Commands**: init, update, validate, sync, deploy, status, logs, describe, explain
**Nouns**: submodule, artifact, specification, environment

## Global Options

```
--verbose           Enable verbose output
--spec-dir PATH     Specification directory (default: .specify)
--output-dir PATH   Output directory (default: ./generated)
--help              Show help message
--version           Show version
```

## Commands

### init - Initialize Submodule

Initialize a git submodule (ggen-spec-kit or clap-noun-verb).

**Usage:**
```bash
ggen paas init <NAME> [OPTIONS]
```

**Arguments:**
- `NAME`: Submodule name (`ggen-spec-kit`, `clap-noun-verb`)

**Options:**
- `--recursive`: Initialize with all dependencies
- `--shallow`: Use shallow clone (for large submodules)

**Examples:**
```bash
# Initialize ggen-spec-kit
ggen paas init ggen-spec-kit

# Initialize with dependencies
ggen paas init clap-noun-verb --recursive

# Shallow clone
ggen paas init ggen-spec-kit --shallow
```

**SLO:**
- Max execution time: 60s
- Success rate target: 99%

---

### update - Update Submodule

Update submodule(s) to latest version.

**Usage:**
```bash
ggen paas update [NAME] [OPTIONS]
```

**Arguments:**
- `NAME`: Submodule name (optional, updates all if not specified)

**Options:**
- `--recursive`: Update with dependencies
- `--checkout REF`: Checkout specific ref/tag/branch

**Examples:**
```bash
# Update all submodules
ggen paas update

# Update specific submodule
ggen paas update ggen-spec-kit

# Checkout specific tag
ggen paas update ggen-spec-kit --checkout v0.2.0
```

**SLO:**
- Max execution time: 30s
- Success rate target: 99%

---

### validate - Validate Specifications

Validate specification closure and SHACL constraints.

**Usage:**
```bash
ggen paas validate [OPTIONS]
```

**Options:**
- `--spec PATH`: Specification file/directory (default: .specify)
- `--min-closure PERCENT`: Minimum closure percentage (default: 95)
- `--strict`: Fail on warnings (strict mode)

**Examples:**
```bash
# Validate with defaults
ggen paas validate

# Validate custom directory
ggen paas validate --spec ./specs

# Strict validation
ggen paas validate --strict --min-closure 100
```

**SLO:**
- Max execution time: 5s
- Success rate target: 99.5%

---

### sync - Synchronize Specifications

Sync specifications with generated code.

**Usage:**
```bash
ggen paas sync [OPTIONS]
```

**Options:**
- `--source PATH`: Source specification directory (default: .specify)
- `--target PATH`: Target code directory (default: ./generated)
- `--dry-run`: Show changes without applying

**Examples:**
```bash
# Standard sync
ggen paas sync

# Preview changes
ggen paas sync --dry-run

# Custom paths
ggen paas sync --source ./specs --target ./src/generated
```

**SLO:**
- Max execution time: 10s
- Success rate target: 99%

---

### deploy - Deploy Artifacts

Deploy generated artifacts to environment.

**Usage:**
```bash
ggen paas deploy --environment ENV [OPTIONS]
```

**Options:**
- `--environment ENV`: Target environment (`development`, `staging`, `production`)
- `--target TYPE`: Deployment target (`docker`, `kubernetes`, `terraform`)
- `--dry-run`: Validate without deploying
- `--force`: Skip safety checks

**Examples:**
```bash
# Deploy to staging
ggen paas deploy --environment staging

# Dry run to production
ggen paas deploy --environment production --dry-run

# Force deployment
ggen paas deploy --environment production --force
```

**SLO:**
- Max execution time: 600s
- Success rate target: 99.5%

---

### status - Show Status

Display deployment or system status.

**Usage:**
```bash
ggen paas status [OPTIONS]
```

**Options:**
- `--environment ENV`: Show status for specific environment
- `--detailed`: Show detailed status

**Examples:**
```bash
# Show all status
ggen paas status

# Detailed status for staging
ggen paas status --environment staging --detailed
```

**SLO:**
- Max execution time: 5s
- Success rate target: 99%

---

### logs - Stream Logs

Retrieve operation logs.

**Usage:**
```bash
ggen paas logs [OPTIONS]
```

**Options:**
- `-l, --lines N`: Number of lines (default: 50)
- `--deployment NAME`: Filter by deployment
- `-f, --follow`: Follow (stream) logs
- `--level LEVEL`: Filter by log level (`error`, `warn`, `info`, `debug`)

**Examples:**
```bash
# Show last 50 lines
ggen paas logs

# Follow logs in real-time
ggen paas logs --follow

# Show error logs only
ggen paas logs --level error --lines 100

# Logs for specific deployment
ggen paas logs --deployment prod-us-west --follow
```

**SLO:**
- Max execution time: 5s
- Success rate target: 99%

---

### describe - Describe Resource

Show detailed information about artifact or specification.

**Usage:**
```bash
ggen paas describe <NAME> [OPTIONS]
```

**Arguments:**
- `NAME`: Artifact or resource name

**Options:**
- `--detailed`: Show full details
- `--format FORMAT`: Output format (`table`, `json`, `yaml`)

**Examples:**
```bash
# Describe artifact
ggen paas describe my-service

# JSON output
ggen paas describe my-service --format json

# Detailed description
ggen paas describe my-service --detailed
```

**SLO:**
- Max execution time: 5s
- Success rate target: 99%

---

### explain - Explain Artifact Origin

Explain how artifact was generated from RDF specification.

**Usage:**
```bash
ggen paas explain <PATH> [OPTIONS]
```

**Arguments:**
- `PATH`: File path to artifact

**Options:**
- `--show-spec`: Show RDF specification that generated this
- `--show-pipeline`: Show transformation pipeline details

**Examples:**
```bash
# Explain generated code
ggen paas explain ./generated/service.rs

# With specification details
ggen paas explain ./generated/service.rs --show-spec

# With pipeline details
ggen paas explain ./generated/service.rs --show-pipeline

# Full explanation
ggen paas explain ./generated/service.rs --show-spec --show-pipeline
```

**SLO:**
- Max execution time: 2s
- Success rate target: 99.5%

---

## Error Codes

| Code | Meaning | Recovery |
|------|---------|----------|
| `CLOSURE_INCOMPLETE` | Specification closure below threshold | Verify all .specify/*.ttl files present |
| `SUBMODULE_NOT_INIT` | Submodule not initialized | Run `ggen paas init <name>` |
| `SUBMODULE_EXISTS` | Submodule already exists | Use `update` command instead |
| `GIT_FAILED` | Git operation failed | Check git config and permissions |
| `SPEC_INVALID` | Specification validation failed | Check TTL syntax |
| `INVALID_TARGET` | Invalid environment/target | Use development/staging/production |
| `TIMEOUT` | Operation exceeded timeout | Increase timeout or check RDF store |
| `MISSING_OPTION` | Required option missing | Use --help to see options |

## Environment Variables

- `GGEN_SPEC_DIR`: Override specification directory
- `GGEN_OUTPUT_DIR`: Override output directory
- `GGEN_DEBUG`: Enable debug logging (any value enables)
- `RUST_LOG`: Control log level (error, warn, info, debug, trace)

## Configuration File

Edit `ggen-paas.toml`:

```toml
[paas]
spec_dir = ".specify"
output_dir = "./generated"
template_dir = "./templates"
enable_receipts = true

[safety]
require_spec_closure = true
min_closure_percentage = 95
validate_before_generate = true
validate_before_deploy = true

[submodules.ggen-spec-kit]
required = true
auto_update = true

[submodules.clap-noun-verb]
required = true
auto_update = true
```

## Examples

### Complete Workflow

```bash
# 1. Initialize submodules
ggen paas init ggen-spec-kit --recursive
ggen paas init clap-noun-verb --recursive

# 2. Validate specifications
ggen paas validate --strict

# 3. Generate code (implicit in next step)
ggen paas sync --source .specify --target ./generated

# 4. Deploy to staging
ggen paas deploy --environment staging --dry-run
ggen paas deploy --environment staging

# 5. Monitor
ggen paas status --detailed
ggen paas logs --follow

# 6. Investigate issues
ggen paas describe my-service --detailed
ggen paas explain ./generated/service.rs --show-spec
```

### Troubleshooting

```bash
# Enable debug output
GGEN_DEBUG=1 ggen paas validate

# Check configuration
cat ggen-paas.toml

# Validate RDF directly
ggen paas validate --spec .specify --strict

# List recent operations
ggen paas logs --lines 200 --level info

# Dry-run before real deployment
ggen paas deploy --environment production --dry-run
```

## See Also

- [Submodule Integration Guide](./SUBMODULE-INTEGRATION.md)
- [Architecture Documentation](./ARCHITECTURE.md)
- [ggen Specification Format](../rdf-specs.md)
