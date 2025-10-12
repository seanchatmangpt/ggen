# Lifecycle Management Guide

Universal lifecycle management for ggen projects using make.toml configuration.

## Overview

The ggen lifecycle system provides a universal project management approach that works across different programming languages and frameworks through declarative make.toml configuration files.

## Core Commands

```bash
# List available phases
ggen lifecycle list

# Run a specific phase
ggen lifecycle run init
ggen lifecycle run build
ggen lifecycle run test

# Run multiple phases in sequence
ggen lifecycle pipeline "init build test deploy"

# Show phase details
ggen lifecycle show build
```

## Production Readiness Integration

```bash
# Check production readiness status
ggen lifecycle readiness

# Update requirement status
ggen lifecycle readiness-update auth-basic complete

# Validate for deployment
ggen lifecycle validate --env production

# Show placeholders that need implementation
ggen lifecycle placeholders --category critical
```

## Available Phases

- **init** - Initialize project structure and dependencies
- **setup** - Set up development environment
- **build** - Build project artifacts
- **test** - Run comprehensive test suite
- **lint** - Code quality checks
- **docs** - Generate documentation
- **deploy** - Deploy to target environment
- **clean** - Clean build artifacts

## Hooks System

Lifecycle phases support before/after hooks for cross-cutting concerns:

```toml
[hooks]
before_all = ["pre-flight-checks"]
after_test = ["coverage-report"]
after_deploy = ["post-deploy-verification"]
```

## Configuration

See `make.toml` files in example projects for complete configuration examples.
