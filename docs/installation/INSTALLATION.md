<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Installation - AI Agent Integration](#ggen-installation---ai-agent-integration)
  - [Agent Installation Methods](#agent-installation-methods)
    - [Method 1: Claude Code Web (Recommended for Agents)](#method-1-claude-code-web-recommended-for-agents)
    - [Method 2: Pre-built Binary (Fast)](#method-2-pre-built-binary-fast)
    - [Method 3: Debian Package](#method-3-debian-package)
  - [Agent Integration Patterns](#agent-integration-patterns)
    - [Pattern 1: Specification Validation Agent](#pattern-1-specification-validation-agent)
    - [Pattern 2: Deterministic Code Generation Agent](#pattern-2-deterministic-code-generation-agent)
    - [Pattern 3: Watch Mode for Continuous Generation](#pattern-3-watch-mode-for-continuous-generation)
  - [System Requirements for Agents](#system-requirements-for-agents)
  - [Exit Codes for Agent Automation](#exit-codes-for-agent-automation)
  - [Environment Variables for Agents](#environment-variables-for-agents)
  - [Version Information](#version-information)
  - [Success Indicators](#success-indicators)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Installation - AI Agent Integration

**For Claude Code Web agents and AI automation systems**

## Agent Installation Methods

### Method 1: Claude Code Web (Recommended for Agents)

```bash
# Single command - use in TaskStart hooks or agent workflows
cargo install ggen

# Environment setup in .claude/settings.json
{
  "environment": {
    "GGEN_HOME": ".ggen",
    "GGEN_LOG_LEVEL": "debug",
    "RUST_BACKTRACE": "1"
  },
  "hooks": {
    "SessionStart": [
      {
        "type": "command",
        "command": "cargo install ggen --locked",
        "timeout": 120000
      }
    ]
  }
}
```

### Method 2: Pre-built Binary (Fast)

```bash
# For agents with network constraints
curl -L https://github.com/seanchatmangpt/ggen/releases/download/v6.0.0/ggen-x86_64-unknown-linux-gnu.tar.gz | tar xz
export PATH="$PWD:$PATH"
```

### Method 3: Debian Package

```bash
sudo dpkg -i ggen_6.0.0_amd64.deb
```

## Agent Integration Patterns

### Pattern 1: Specification Validation Agent

```bash
# Use in validation workflows
ggen sync --validate_only true --output json

# Returns: JSON structure with validation status
# Perfect for: Pre-flight checks before generation
```

### Pattern 2: Deterministic Code Generation Agent

```bash
# Use for reproducible generation
ggen sync --audit true --force true

# Guarantees:
# - Identical output for identical input
# - Cryptographic proof in .ggen/receipts/latest.json
# - Audit trail for verification
```

### Pattern 3: Watch Mode for Continuous Generation

```bash
# For agents maintaining living documentation
ggen sync --watch true

# Triggers regeneration on ontology changes
# Maintains: Real-time consistency between specs and artifacts
```

## System Requirements for Agents

| Requirement | Value | Notes |
|---|---|---|
| CPU | 1+ cores | 2 cores recommended for parallel processing |
| RAM | 512 MB minimum | 2 GB for large ontologies (1k+ triples) |
| Disk | 100 MB | For binary + cache + generated artifacts |
| Timeout | 120s | Recommended for network-limited environments |
| Network | Limited | Uses pre-installed Rust toolchain only |

## Exit Codes for Agent Automation

| Code | Meaning | Agent Action |
|------|---------|--------------|
| 0 | Success | Continue with generated artifacts |
| 1 | General error | Log error, retry with validation |
| 2 | Validation failed | Trigger validation-only mode |
| 3 | Generation failed | Check ontology syntax, retry |
| 4 | SPARQL query failed | Inspect .ttl file, log context |
| 5 | Template rendering failed | Check template syntax |

## Environment Variables for Agents

```bash
# Logging - set before execution
export GGEN_LOG_LEVEL=debug           # trace, debug, info, warn, error
export GGEN_LOG_FILE=.ggen/ggen.log

# Performance
export GGEN_CACHE_DIR=.ggen/cache
export GGEN_CACHE_MAX_SIZE=1073741824 # 1 GB

# Behavioral
export GGEN_TIMEOUT=120               # seconds
export GGEN_HOME=.ggen
```

## Version Information

**Current Version**: 6.0.0
**Release Date**: January 18, 2026
**Stability**: Production-ready

**Breaking Changes from v5.x**: Yes - see migration guide

## Success Indicators

After installation, verify with:

```bash
ggen --version
# Expected: ggen 6.0.0

ggen init --help
# Expected: Subcommand help output

ggen sync --help
# Expected: Command reference
```

---

**For agent developers**: See `/docs/reference/01-commands.md` for complete command API
