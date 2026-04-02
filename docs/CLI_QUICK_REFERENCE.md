<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen-paas CLI Quick Reference Guide](#ggen-paas-cli-quick-reference-guide)
  - [Table of Contents](#table-of-contents)
  - [Quick Start](#quick-start)
    - [Installation](#installation)
    - [First Command](#first-command)
  - [Command Overview](#command-overview)
    - [Generation Commands](#generation-commands)
      - [`ggen paas generate <artifact>`](#ggen-paas-generate-artifact)
    - [Validation Commands](#validation-commands)
      - [`ggen paas validate <target>`](#ggen-paas-validate-target)
    - [Synchronization Commands](#synchronization-commands)
      - [`ggen paas sync <target>`](#ggen-paas-sync-target)
    - [Deployment Commands](#deployment-commands)
      - [`ggen paas deploy <environment>`](#ggen-paas-deploy-environment)
    - [Management Commands](#management-commands)
      - [`ggen paas status [target]`](#ggen-paas-status-target)
      - [`ggen paas logs <service>`](#ggen-paas-logs-service)
      - [`ggen paas describe <resource>`](#ggen-paas-describe-resource)
    - [Help Commands](#help-commands)
      - [`ggen paas explain <concept>`](#ggen-paas-explain-concept)
  - [Global Options](#global-options)
  - [Adding New Commands](#adding-new-commands)
    - [1. Define in RDF Specification](#1-define-in-rdf-specification)
    - [2. Generate Code](#2-generate-code)
    - [3. Implement Command Logic](#3-implement-command-logic)
    - [4. Write Tests](#4-write-tests)
    - [5. Verify](#5-verify)
  - [Project Structure](#project-structure)
  - [Configuration](#configuration)
    - [User Configuration](#user-configuration)
    - [Project Configuration](#project-configuration)
  - [Troubleshooting](#troubleshooting)
    - [Problem: Command not found](#problem-command-not-found)
    - [Problem: Validation fails](#problem-validation-fails)
    - [Problem: Artifacts don't validate](#problem-artifacts-dont-validate)
    - [Problem: Deployment stuck](#problem-deployment-stuck)
    - [Problem: Need verbose debugging](#problem-need-verbose-debugging)
  - [Key Concepts](#key-concepts)
    - [RDF-Driven CLI](#rdf-driven-cli)
    - [Chicago TDD](#chicago-tdd)
    - [Specification Closure](#specification-closure)
  - [Further Reading](#further-reading)
  - [Support](#support)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen-paas CLI Quick Reference Guide

**Last Updated**: 2026-01-08
**Specification Source**: `.specify/cli-commands.ttl`
**Generation Rules**: `ggen-paas-cli.toml`

---

## Table of Contents

1. [Quick Start](#quick-start)
2. [Command Overview](#command-overview)
3. [Global Options](#global-options)
4. [Adding New Commands](#adding-new-commands)
5. [Troubleshooting](#troubleshooting)

---

## Quick Start

### Installation

```bash
# Navigate to ggen-paas submodule
cd ggen-paas

# Install dependencies
npm install

# Generate CLI from RDF specification
npm run generate

# Verify installation
./bin/paas --help
```

### First Command

```bash
# Show help
ggen paas --help

# Generate all artifacts
ggen paas generate all

# View status
ggen paas status

# Stream logs
ggen paas logs api-gateway --follow
```

---

## Command Overview

### Generation Commands

#### `ggen paas generate <artifact>`
Generate infrastructure artifacts from RDF specifications.

```bash
# Generate specific artifact
ggen paas generate docker          # Docker Compose
ggen paas generate kubernetes      # Kubernetes manifests
ggen paas generate terraform       # Terraform configuration
ggen paas generate openapi         # OpenAPI specification

# Generate everything
ggen paas generate all

# Options
--output, -o          Output format: text, json, yaml (default: text)
--validate            Validate generated artifacts (default: true)
--watch, -w           Watch for spec changes and auto-regenerate
--dry-run             Show what would be generated without writing
```

**SLA**: 10 seconds max, 99% success rate

---

### Validation Commands

#### `ggen paas validate <target>`
Validate specifications and generated artifacts.

```bash
# Validate generated artifacts
ggen paas validate artifacts

# Validate specification closure (100% complete?)
ggen paas validate closure

# Validate deployed infrastructure
ggen paas validate infrastructure

# Validate everything
ggen paas validate all

# Options
--verbose, -v         Detailed validation results
--strict              Fail on warnings, not just errors
```

**SLA**: 30 seconds max, 99.5% success rate

---

### Synchronization Commands

#### `ggen paas sync <target>`
Synchronize with RDF store and cloud infrastructure.

```bash
# Sync specifications to RDF store
ggen paas sync specifications

# Sync infrastructure with cloud
ggen paas sync infrastructure

# Sync everything
ggen paas sync all

# Options
--dry-run             Show what would be synced
--force               Sync without confirmation
--environment, -e     Target environment: development, staging, production
```

**SLA**: 60 seconds max, 99% success rate

---

### Deployment Commands

#### `ggen paas deploy <environment>`
Deploy infrastructure to target environment.

```bash
# Deploy to development
ggen paas deploy development

# Deploy to staging (with preview)
ggen paas deploy staging --dry-run

# Deploy to production
ggen paas deploy production

# Options
--dry-run             Preview deployment
--region, -r          AWS region (default: us-east-1)
--namespace, -n       Kubernetes namespace (default: ggen-paas)
--timeout             Deployment timeout in seconds (default: 300)
```

**SLA**: 10 minutes max, 99.5% success rate

---

### Management Commands

#### `ggen paas status [target]`
Show current status of services and infrastructure.

```bash
# Overall status
ggen paas status

# Service status
ggen paas status services

# Infrastructure status
ggen paas status infrastructure

# Options
--output, -o          Output format: text, json, yaml
--watch, -w           Watch for status changes continuously
```

**SLA**: 5 seconds max, 99.5% success rate

---

#### `ggen paas logs <service>`
Stream logs from services.

```bash
# Stream logs from API Gateway
ggen paas logs api-gateway

# Follow logs (like tail -f)
ggen paas logs web-ui --follow

# Last 100 lines from auth service
ggen paas logs auth-service --lines 100

# Options
--follow, -f          Follow log stream
--lines               Number of recent lines (default: 20)
--namespace, -n       Kubernetes namespace
--level               Log level: debug, info, warn, error (default: info)
```

**SLA**: 5 seconds max, 99% success rate

---

#### `ggen paas describe <resource>`
Describe a resource in detail.

```bash
# Describe a service
ggen paas describe api-gateway

# Describe data store
ggen paas describe postgres

# Get full details
ggen paas describe redis --detail full

# Options
--output, -o          Output format: text, json, yaml
--detail              Detail level: summary, full, extended (default: summary)
```

**SLA**: 5 seconds max, 99% success rate

---

### Help Commands

#### `ggen paas explain <concept>`
Explain RDF concepts and relationships.

```bash
# Explain a concept
ggen paas explain Container

# Explain with full details
ggen paas explain DataStore --detail full

# Get YAML format
ggen paas explain SLA --output yaml

# Options
--detail              Detail level: summary, full, extended
--output, -o          Output format: text, json, yaml
```

**SLA**: 2 seconds max, 99.5% success rate

---

## Global Options

These options work with any command:

```
--help, -h              Show help message and exit
--version, -v           Show version and exit
--verbose, -vvv         Increase verbosity (stackable: -vvv for max)
--quiet, -q             Suppress output (errors only)
--config, -c FILE       Config file override (~/.ggen-paas/config.yaml)
```

---

## Adding New Commands

### 1. Define in RDF Specification

Edit `.specify/cli-commands.ttl` and add a new command:

```turtle
cli:MyNewCommand
  a cli:Command ;
  rdfs:label "mynew" ;
  rdfs:comment "What this command does" ;
  cli:aliases "mn", "my" ;
  cli:category cli:CategoryManagement ;
  cli:positionalArgs cli:MyNewArg ;
  cli:options cli:MyNewOption ;
  cli:slo cli:MyNewSLO ;
  cli:examples """
    ggen paas mynew arg1
    ggen paas mynew arg1 --option value
    """ ;
  cli:handler "lib/commands/mynew.js" ;
  cli:test "tests/commands/mynew.test.js" ;
  .

cli:MyNewArg
  a cli:PositionalArgument ;
  rdfs:label "arg1" ;
  cli:position 0 ;
  cli:type xsd:string ;
  cli:required true ;
  .

cli:MyNewOption
  a cli:Option ;
  rdfs:label "option" ;
  cli:longForm "--option" ;
  cli:type xsd:string ;
  cli:default "default_value" ;
  .

cli:MyNewSLO
  a cli:SLO ;
  cli:maxDurationMs 5000 ;
  cli:expectedPassRate 99.0 ;
  .
```

### 2. Generate Code

```bash
cd ggen-paas
npm run generate    # Runs: ggen sync (triggers code generation)
```

This will:
- ✓ Extract command from `.specify/cli-commands.ttl`
- ✓ Generate `lib/commands/mynew.js` from template
- ✓ Generate `tests/commands/mynew.test.js` test skeleton
- ✓ Regenerate `lib/cli-dispatcher.js` with new command routing
- ✓ Update help documentation
- ✓ Update CLI schema

### 3. Implement Command Logic

Edit `lib/commands/mynew.js` and implement the `handleMynew()` method:

```javascript
async handleMynew(args, options, ontologyManager, logger, projectRoot) {
  // Your implementation here
  const result = {
    success: true,
    data: { /* your results */ },
    duration: this.getDuration(),
  };
  return result;
}
```

### 4. Write Tests

Edit `tests/commands/mynew.test.js` and implement Chicago TDD tests:

```javascript
it('should execute mynew command with valid arguments', async () => {
  // Arrange
  const args = { arg1: 'value' };
  const options = { option: 'test' };

  // Act
  const result = await command.execute(args, options);

  // Assert
  expect(result.success).toBe(true);
});
```

### 5. Verify

```bash
npm test              # Run all tests
npm run lint          # Check code style
npm run build         # Full build pipeline
```

---

## Project Structure

```
ggen-paas/
├── bin/
│   └── paas                          # CLI entry point
├── lib/
│   ├── commands/
│   │   ├── generate.js               # Generated
│   │   ├── validate.js               # Generated
│   │   ├── sync.js                   # Generated
│   │   ├── deploy.js                 # Generated
│   │   ├── status.js                 # Generated
│   │   ├── logs.js                   # Generated
│   │   ├── describe.js                # Generated
│   │   └── explain.js                # Generated
│   ├── cli-dispatcher.js             # Generated
│   └── utils/
│       ├── ontology.js               # RDF loading
│       ├── logger.js                 # Logging
│       └── config.js                 # Configuration
├── tests/
│   ├── commands/
│   │   ├── generate.test.js          # Generated
│   │   ├── validate.test.js          # Generated
│   │   └── ...
│   └── cli-integration.test.js       # Integration tests
├── generated/
│   ├── commands/                     # Generated code
│   ├── docs/
│   │   ├── cli-reference.md          # Generated help
│   │   └── commands/
│   │       └── *.md                  # Command docs
│   ├── cli-schema.json               # Generated
│   └── .manifest.json                # Generation receipt
├── .specify/
│   ├── cli-commands.ttl              # RDF source of truth
│   └── cli-schema.ttl                # SHACL validation
├── templates/
│   ├── cli-command.tera              # Command template
│   ├── cli-dispatcher.tera           # Dispatcher template
│   └── cli-help.md                   # Help template
├── ggen-paas-cli.toml                # Generation rules
├── package.json
└── README.md
```

---

## Configuration

### User Configuration

File: `~/.ggen-paas/config.yaml`

```yaml
# Global CLI settings
cli:
  verbose: false
  color: true
  output: text

# AWS settings
aws:
  region: us-east-1
  profile: default

# Kubernetes settings
kubernetes:
  namespace: ggen-paas
  context: docker-desktop

# Deployment settings
deployment:
  confirm: true          # Require confirmation before deploying
  timeout: 300           # Timeout in seconds
  retry_count: 3         # Retry failed operations
  dry_run: false         # Default to dry-run

# Logging
logging:
  level: info
  file: ~/.ggen-paas/logs/cli.log
  max_size: 10MB         # Log rotation
```

### Project Configuration

File: `ggen-paas/ggen-paas.config.ttl`

```turtle
# RDF configuration for this specific project
paas:ProjectConfig
  a paas:Configuration ;
  paas:projectName "ggen-paas" ;
  paas:environment paas:ProductionEnvironment ;
  paas:deploymentRegion "us-east-1" ;
  paas:kubernetesNamespace "ggen-paas" ;
  .
```

---

## Troubleshooting

### Problem: Command not found

```bash
$ ggen paas mynew
Error: Unknown command: mynew
```

**Solution**: Run `npm run generate` to regenerate CLI from RDF spec.

---

### Problem: Validation fails

```bash
$ ggen paas validate closure
ERROR: Specification closure validation failed
```

**Solution**: Check `.specify/cli-commands.ttl` for incomplete command definitions:

```bash
# All commands must have:
# - rdfs:label (command name)
# - rdfs:comment (description)
# - cli:handler (path to handler file)
# - cli:slo (SLA definition)
```

---

### Problem: Artifacts don't validate

```bash
$ ggen paas validate artifacts
ERROR [E_VALIDATION_FAILED]: YAML validation failed
```

**Solution**: First generate artifacts:

```bash
ggen paas generate all
ggen paas validate artifacts --verbose  # See detailed errors
```

---

### Problem: Deployment stuck

```bash
$ ggen paas deploy production
(waiting... waiting... timeout)
```

**Solution**: Check status and use timeout option:

```bash
ggen paas status
ggen paas deploy production --timeout 600  # 10 minutes
```

---

### Problem: Need verbose debugging

```bash
# Increase verbosity
ggen paas generate all -vvv    # Maximum verbosity

# Check logs
tail ~/.ggen-paas/logs/cli.log

# Dry-run to see what would happen
ggen paas deploy production --dry-run -vvv
```

---

## Key Concepts

### RDF-Driven CLI
All command definitions come from `.specify/cli-commands.ttl`. Commands are **generated**, not hand-coded. This means:

- **Single source of truth**: Edit TTL file, everything else is generated
- **Deterministic**: Same TTL always produces identical CLI
- **Testable**: SHACL validation ensures 100% specification closure

### Chicago TDD
All generated tests use Chicago TDD pattern:

```javascript
it('should work', async () => {
  // Arrange: Set up test data
  const input = setup();

  // Act: Execute the command
  const result = await cmd.execute(input);

  // Assert: Verify observable behavior
  expect(result.success).toBe(true);
});
```

### Specification Closure
The CLI specification is **100% complete** when:

- ✓ All commands have definitions
- ✓ All options are documented
- ✓ All arguments have types and choices
- ✓ All commands have SLA definitions
- ✓ All handlers are mapped to files

Verify closure:
```bash
ggen paas validate closure
```

---

## Further Reading

- **Design Document**: `docs/CLI_SUBMODULE_DESIGN.md`
- **RDF Specification**: `.specify/cli-commands.ttl`
- **Generation Rules**: `ggen-paas-cli.toml`
- **Generated Commands**: `generated/cli-reference.md`

---

## Support

For issues or questions:

1. Check the help system: `ggen paas <command> --help`
2. Review the RDF specification: `.specify/cli-commands.ttl`
3. Check logs: `~/.ggen-paas/logs/cli.log`
4. Run validation: `ggen paas validate closure --verbose`
5. Report issues with verbose output: `ggen paas <cmd> -vvv`
