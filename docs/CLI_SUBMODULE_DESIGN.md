# Git Submodule CLI Design Patterns for ggen-paas

## Executive Summary

This document outlines the architectural design for integrating ggen-paas as a Git submodule with its own command-line interface (CLI), inspired by Rails' approach to providing a cohesive developer experience.

**Core Principle**: The CLI is not hand-coded—it is **specification-driven** via RDF and **generated** by the ggen sync pipeline. Command definitions live in `.specify/cli-commands.ttl`, and the CLI itself is generated from that ontology.

---

## 1. High-Level Architecture

### 1.1 The Three-Layer Model

```
Layer 1: Host Project (ggen)
         ↓
         Invokes: ggen paas <subcommand>
         ↓
Layer 2: Submodule CLI Entry Point (ggen-paas/bin/paas)
         ↓
         Routes to generated CLI dispatcher
         ↓
Layer 3: Generated Command Handler (ggen-paas/lib/commands/<command>.js)
         ↓
         Executes actual business logic
         ↓
         Returns structured output (JSON, YAML, text)
```

### 1.2 Submodule Organization

```
ggen-paas/                          # Git submodule
├── bin/
│   └── paas                         # CLI entry point (executable)
├── lib/
│   ├── commands/                    # Generated command handlers
│   │   ├── generate.js
│   │   ├── validate.js
│   │   ├── sync.js
│   │   ├── deploy.js
│   │   └── status.js
│   ├── cli-dispatcher.js            # Command routing (generated)
│   └── utils/
│       ├── logger.js
│       └── config.js
├── .specify/
│   ├── cli-commands.ttl             # CLI ontology (RDF source of truth)
│   └── cli-schema.ttl               # SHACL validation for commands
├── generated/
│   ├── commands/                    # Generated command code
│   └── docs/
│       └── cli-reference.md         # Generated from RDF
├── package.json
├── ggen-paas.config.ttl             # Main configuration
└── README.md
```

---

## 2. Naming Conventions

### 2.1 Command Structure: `ggen paas <verb> <noun> [options]`

Inspired by Rails and Kubernetes, commands follow a consistent pattern:

```
ggen paas generate docker               # Generate Docker artifacts
ggen paas generate kubernetes           # Generate K8s manifests
ggen paas generate terraform            # Generate Terraform
ggen paas generate all                  # Generate everything

ggen paas sync infrastructure           # Sync infrastructure to cloud
ggen paas sync specifications           # Sync specs with RDF store

ggen paas validate artifacts            # Validate generated outputs
ggen paas validate infrastructure       # Validate deployed state
ggen paas validate closure              # Validate spec closure

ggen paas deploy development            # Deploy to dev environment
ggen paas deploy staging                # Deploy to staging
ggen paas deploy production             # Deploy to prod

ggen paas status                        # Show current status
ggen paas status services               # Show service status
ggen paas status deployment             # Show deployment status

ggen paas logs <service>                # Stream logs from service
ggen paas describe <resource>           # Describe a resource
ggen paas explain <concept>             # Explain RDF concepts
```

### 2.2 Option Conventions

```
# Boolean flags (verb-subject form)
--dry-run                # Execute without applying changes
--validate-only          # Validate but don't generate/deploy
--force                  # Skip confirmation prompts

# Input/Output
--output, -o <format>    # Output format: json, yaml, text (default)
--input, -i <file>       # Input file path
--config, -c <file>      # Config file override

# Control
--verbose, -v            # Increase logging verbosity (stackable: -vvv)
--quiet, -q              # Suppress output
--watch                  # Watch for changes and auto-regenerate
--parallel <n>           # Number of parallel workers

# Context
--environment, -e <env>  # Target environment (dev, staging, prod)
--namespace, -n <ns>     # Kubernetes namespace
--region, -r <region>    # AWS region
```

### 2.3 File Paths and Constants

```javascript
// CLI constants (generated from RDF)
CLI_VERSION = "1.0.0"                    // From package.json
CLI_HOME = ~/.ggen-paas                  // Local state directory
CLI_CONFIG = ~/.ggen-paas/config.yaml    // User config
SPEC_PATH = ./ggen-paas/.specify/        // Spec directory
GENERATED_PATH = ./generated/            // Generated outputs
```

---

## 3. Execution Patterns

### 3.1 Command Invocation Flow

```
User types: ggen paas generate docker --output yaml
                ↓
Host project (ggen) calls: ./ggen-paas/bin/paas generate docker --output yaml
                ↓
bin/paas (shebang #!/usr/bin/env node)
  1. Loads CLI dispatcher from lib/cli-dispatcher.js
  2. Parses command line args
  3. Routes to lib/commands/generate.js
                ↓
lib/commands/generate.js
  1. Validates arguments against SHACL schema
  2. Loads ggen-paas ontology
  3. Extracts "docker" artifact specification
  4. Executes generation logic (Tera templates, RDF queries)
  5. Validates output
  6. Returns result with exit code
```

### 3.2 Command Handler Pattern

Each command is a **specification-driven function** that follows the same signature:

```javascript
// lib/commands/<command>.js

import { CommandBase } from '../cli-dispatcher.js';
import { OntologyManager } from '../utils/ontology.js';

export default class GenerateCommand extends CommandBase {
  constructor() {
    super({
      name: 'generate',
      description: 'Generate infrastructure artifacts from RDF specs',
      aliases: ['gen', 'g'],
    });
  }

  // Define expected arguments from RDF schema
  defineSchema() {
    return {
      positional: ['artifact'],    // Artifact type (docker, k8s, terraform, etc.)
      options: {
        output: { type: 'string', default: 'text' },
        validate: { type: 'boolean', default: true },
        watch: { type: 'boolean', default: false },
      },
    };
  }

  // Main execution
  async execute(args, options) {
    const logger = this.logger;
    logger.info(`Generating ${args.artifact} artifacts...`);

    try {
      // Load ontology
      const ontologyManager = new OntologyManager();
      await ontologyManager.load();

      // Validate specification closure
      const closure = ontologyManager.validateClosure();
      if (!closure.valid) {
        logger.error('Specification closure validation failed');
        return this.fail('SPEC_INCOMPLETE', closure.issues);
      }

      // Execute generation for this artifact type
      const result = await this.generateArtifact(args.artifact, ontologyManager, options);

      // Format and output result
      return this.success(result, options.output);
    } catch (error) {
      return this.fail('GENERATION_ERROR', error.message);
    }
  }

  async generateArtifact(artifactType, ontologyManager, options) {
    // Implementation uses templates and RDF data
  }
}
```

### 3.3 Exit Codes (Poka-Yoke)

```
0:   SUCCESS                    # Command completed successfully
1:   GENERAL_ERROR             # Unspecified error
2:   INVALID_ARGUMENTS         # Wrong arguments
3:   SPEC_INCOMPLETE           # Specification closure validation failed
4:   VALIDATION_FAILED         # Generated artifacts failed validation
5:   CONFIG_ERROR              # Configuration file invalid
6:   ENVIRONMENT_ERROR         # Missing dependencies, wrong Node version, etc.
7:   PERMISSION_ERROR          # Access denied to required resources
8:   TIMEOUT_ERROR             # Operation exceeded time limit
9:   INTERRUPTED               # User interrupted (Ctrl+C)
```

---

## 4. RDF-Driven CLI Specification

### 4.1 The CLI Ontology (`.specify/cli-commands.ttl`)

The CLI structure is **defined as RDF triples**, not hardcoded:

```turtle
@prefix cli: <http://ggen.org/cli#> .
@prefix paas: <http://ggen.org/paas#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Command: generate
cli:GenerateCommand
  a cli:Command ;
  rdfs:label "generate" ;
  rdfs:comment "Generate infrastructure artifacts from RDF specifications" ;
  cli:aliases "gen", "g" ;
  cli:aliases "generate" ;
  cli:positionalArgs cli:ArtifactArg ;
  cli:options cli:OutputOption, cli:ValidateOption, cli:WatchOption ;
  cli:slo cli:GenerateSLO ;
  cli:examples """
    ggen paas generate docker
    ggen paas generate kubernetes --output yaml
    ggen paas generate all --validate
    """ ;
  .

cli:ArtifactArg
  a cli:PositionalArgument ;
  rdfs:label "artifact" ;
  cli:type xsd:string ;
  cli:required true ;
  cli:choices "docker", "kubernetes", "terraform", "openapi", "all" ;
  rdfs:comment "Type of artifact to generate" ;
  .

cli:OutputOption
  a cli:Option ;
  rdfs:label "output" ;
  cli:shortForm "-o" ;
  cli:longForm "--output" ;
  cli:type xsd:string ;
  cli:default "text" ;
  cli:choices "text", "json", "yaml" ;
  rdfs:comment "Output format" ;
  .

cli:ValidateOption
  a cli:Option ;
  rdfs:label "validate" ;
  cli:longForm "--validate" ;
  cli:type xsd:boolean ;
  cli:default true ;
  rdfs:comment "Validate generated artifacts" ;
  .

cli:WatchOption
  a cli:Option ;
  rdfs:label "watch" ;
  cli:shortForm "-w" ;
  cli:longForm "--watch" ;
  cli:type xsd:boolean ;
  cli:default false ;
  rdfs:comment "Watch for changes and regenerate automatically" ;
  .

# SLA for generate command
cli:GenerateSLO
  a cli:SLO ;
  cli:maxDurationMs 10000 ;
  cli:expectedPassRate 99.0 ;
  .

# Command: validate
cli:ValidateCommand
  a cli:Command ;
  rdfs:label "validate" ;
  rdfs:comment "Validate generated artifacts and specification closure" ;
  cli:aliases "check", "test" ;
  cli:positionalArgs cli:ValidateTargetArg ;
  cli:slo cli:ValidateSLO ;
  .

cli:ValidateTargetArg
  a cli:PositionalArgument ;
  rdfs:label "target" ;
  cli:type xsd:string ;
  cli:required true ;
  cli:choices "artifacts", "closure", "infrastructure", "all" ;
  .

# And so on for other commands...
cli:SyncCommand a cli:Command ; ...
cli:DeployCommand a cli:Command ; ...
cli:StatusCommand a cli:Command ; ...
cli:LogsCommand a cli:Command ; ...
cli:DescribeCommand a cli:Command ; ...
cli:ExplainCommand a cli:Command ; ...

# Relationship between CLI and PaaS specifications
cli:GenerateCommand cli:generatesFrom paas:DockerArtifact ;
cli:GenerateCommand cli:generatesFrom paas:KubernetesArtifact ;
cli:GenerateCommand cli:generatesFrom paas:TerraformArtifact ;
cli:GenerateCommand cli:generatesFrom paas:OpenAPIArtifact ;
```

### 4.2 CLI Generation Pipeline

```
Input:  .specify/cli-commands.ttl (RDF source of truth)
            ↓
SPARQL EXTRACT: Parse all cli:Command instances
            ↓
GENERATION: For each command, generate:
  - lib/commands/<command>.js (JavaScript handler)
  - docs/commands/<command>.md (Auto-generated documentation)
  - tests/commands/<command>.test.js (Test skeleton)
            ↓
EMISSION: Route generated files to appropriate directories
            ↓
CANONICALIZATION: Ensure all files follow code style, types, patterns
            ↓
RECEIPT: Verify all commands load, all docs generated, test count matches
            ↓
Output: Fully functional, self-documenting CLI with 100% specification coverage
```

---

## 5. Integration with Host Project

### 5.1 How `ggen` Invokes Submodule CLI

In the host `ggen` project's CLI:

```rust
// crates/ggen-cli/src/commands/paas.rs

use std::process::Command;

pub fn exec_paas_subcommand(args: Vec<String>) -> Result<ExitCode> {
    // Verify submodule is initialized
    if !Path::new("ggen-paas/.git").exists() {
        return Err("ggen-paas submodule not initialized. Run: git submodule update --init");
    }

    // Ensure submodule is up to date
    Command::new("git")
        .args(&["submodule", "update", "--remote", "ggen-paas"])
        .output()?;

    // Delegate to submodule CLI
    let mut cmd = Command::new("node");
    cmd.arg("ggen-paas/bin/paas");
    cmd.args(&args);

    let output = cmd.output()?;
    println!("{}", String::from_utf8_lossy(&output.stdout));

    if !output.status.success() {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
    }

    Ok(output.status.code().unwrap_or(1).into())
}
```

Usage:
```bash
ggen paas generate docker                # Host delegates to submodule
ggen paas validate artifacts             # All commands flow through submodule
ggen paas deploy production              # Full separation of concerns
```

### 5.2 Submodule Package Integration

`ggen-paas/package.json`:
```json
{
  "name": "@ggen/paas",
  "version": "1.0.0",
  "description": "ggen PaaS infrastructure generation and management",
  "type": "module",
  "bin": {
    "paas": "./bin/paas"
  },
  "scripts": {
    "generate": "cargo make ggen:sync",
    "test": "vitest",
    "lint": "eslint lib/",
    "build": "npm run generate && npm run lint && npm run test"
  },
  "dependencies": {
    "n3": "^1.17.0",
    "yargs": "^17.0.0",
    "chalk": "^5.0.0",
    "js-yaml": "^4.0.0"
  },
  "devDependencies": {
    "vitest": "^0.34.0",
    "eslint": "^8.0.0"
  }
}
```

---

## 6. Command Discovery and Help System

### 6.1 Dynamic Help Generation

Help text is **generated from RDF**, not maintained manually:

```bash
$ ggen paas --help

ggen paas - Infrastructure generation and management for ggen PaaS

USAGE
  ggen paas <command> [options]

COMMANDS
  generate      Generate infrastructure artifacts from RDF specs
                Aliases: gen, g

  validate      Validate generated artifacts and specification closure
                Aliases: check, test

  sync          Synchronize infrastructure state with cloud
  deploy        Deploy to target environment
  status        Show current deployment status
  logs          Stream logs from services
  describe      Describe a resource in detail
  explain       Explain RDF concepts and relationships

GLOBAL OPTIONS
  --help, -h          Show this help message
  --version, -v       Show version
  --verbose, -vvv     Increase verbosity (stack for more)
  --quiet, -q         Suppress output
  --config, -c FILE   Config file override

EXAMPLES
  ggen paas generate all
  ggen paas validate artifacts --verbose
  ggen paas deploy production --dry-run

For help on a specific command:
  ggen paas generate --help
  ggen paas deploy --help
```

### 6.2 Command-Specific Help

```bash
$ ggen paas generate --help

ggen paas generate - Generate infrastructure artifacts from RDF specs

USAGE
  ggen paas generate <artifact> [options]

ARGUMENTS
  artifact              Type of artifact to generate
                        Choices: docker, kubernetes, terraform, openapi, all
                        Required: yes

OPTIONS
  --output, -o FORMAT   Output format
                        Default: text
                        Choices: text, json, yaml

  --validate            Validate generated artifacts
                        Default: true

  --watch, -w           Watch for changes and regenerate
                        Default: false

  --dry-run             Show what would be generated without writing

SLA
  Max Duration:  10 seconds
  Expected Pass Rate: 99%

EXAMPLES
  ggen paas generate docker
  ggen paas generate all --output yaml
  ggen paas generate kubernetes --watch
  ggen paas generate terraform --dry-run

SOURCE
  Defined in: .specify/cli-commands.ttl
  Generated: 2026-01-08T10:00:00Z
```

---

## 7. Error Handling and Recovery

### 7.1 Structured Error Messages

```typescript
interface CLIError {
  code: string;                    // Machine-readable error code
  message: string;                 // User-friendly message
  details: Record<string, any>;    // Structured error data
  suggestion?: string;             // What to do about it
  context?: string;                // Where in the CLI it happened
  timestamp: string;               // ISO timestamp
}
```

Examples:
```bash
$ ggen paas generate invalid

ERROR [E_INVALID_ARGUMENT]
  Invalid artifact type: invalid

Details:
  provided: "invalid"
  valid: docker, kubernetes, terraform, openapi, all

Suggestion:
  Run 'ggen paas generate --help' to see valid choices

Context:
  Command: generate
  Position: argument[0]

Timestamp: 2026-01-08T10:00:00Z
```

```bash
$ ggen paas deploy production (with invalid credentials)

ERROR [E_CONFIG_ERROR]
  AWS credentials not found

Details:
  error: NoCredentialProvider
  searched: ~/.aws/credentials, AWS_ACCESS_KEY_ID env var, IAM role

Suggestion:
  1. Create ~/.aws/credentials with [default] profile
  2. Or set: export AWS_ACCESS_KEY_ID=...
  3. Or run: aws configure

Resources:
  https://docs.aws.amazon.com/cli/latest/userguide/cli-configure-files.html
```

### 7.2 Andon Signals (Signal-Based Control)

```
🔴 RED (Exit 1):    Critical error - stop immediately
  Examples: Spec closure validation failed, generation error

🟡 YELLOW (Exit 0): Warning - continue but alert user
  Examples: Deprecated command, missing optional config

🟢 GREEN (Exit 0):  Success - all good
  Examples: Command completed successfully

⚪ WHITE (Exit 2):  User error - show help
  Examples: Invalid arguments, malformed options
```

---

## 8. Concrete Usage Examples

### 8.1 Development Workflow

```bash
# Initialize submodule
cd ggen
git submodule add https://github.com/user/ggen-paas ggen-paas
git submodule update --init --recursive

# Generate new architecture
cd ggen-paas
ggen paas generate all --output yaml
ggen paas validate closure --verbose

# Watch for changes
ggen paas generate all --watch    # Auto-regenerates on spec changes

# Dry-run deployment
ggen paas deploy staging --dry-run --verbose

# Check status before real deployment
ggen paas status --output json | jq '.services[] | select(.status != "healthy")'

# Deploy to production with confirmation
ggen paas deploy production
# Output: Deploying to production (10 services)
#         Ready to proceed? [y/N]: y
#         ✓ Service 1: deployment successful
#         ✓ Service 2: deployment successful
#         ...
```

### 8.2 CI/CD Pipeline Integration

```yaml
# .github/workflows/deploy.yml

name: Deploy ggen-paas

on:
  push:
    branches: [main]
    paths: ['.specify/**', 'ggen-paas/**']

jobs:
  validate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
        with:
          submodules: true

      - uses: actions/setup-node@v3
        with:
          node-version: '18'

      - name: Validate specifications
        run: ggen paas validate closure

      - name: Generate artifacts
        run: ggen paas generate all

      - name: Validate artifacts
        run: ggen paas validate artifacts

  deploy:
    needs: validate
    runs-on: ubuntu-latest
    if: github.ref == 'refs/heads/main'
    steps:
      - uses: actions/checkout@v3
        with:
          submodules: true

      - uses: actions/setup-node@v3

      - name: Deploy to production
        run: |
          ggen paas deploy production \
            --environment production \
            --region us-east-1
```

### 8.3 Local Development with Hot Reload

```bash
# Terminal 1: Watch for spec changes and regenerate
ggen paas generate all --watch --verbose

# Terminal 2: Monitor deployment status
ggen paas status --watch

# Terminal 3: Stream logs from specific service
ggen paas logs api-gateway --follow

# Terminal 4: Run tests
npm test --watch
```

---

## 9. Implementation Strategy

### 9.1 Phase 1: RDF Specification (Week 1)

1. Create `.specify/cli-commands.ttl` with all command definitions
2. Create SHACL schema (`.specify/cli-schema.ttl`) for validation
3. Verify specification closure (100% coverage)
4. Document command semantics in RDF

**Deliverable**: Complete RDF ontology describing entire CLI

### 9.2 Phase 2: Code Generation (Week 2)

1. Create `ggen-paas.toml` generation rules for CLI commands
2. Implement `templates/cli-command.tera` for command generation
3. Implement `templates/cli-help.tera` for help text generation
4. Run `ggen sync` to generate all command handlers

**Deliverable**: Auto-generated CLI command code from RDF

### 9.3 Phase 3: CLI Dispatcher (Week 3)

1. Create `lib/cli-dispatcher.js` - command routing and execution
2. Implement `CommandBase` class - common interface for all commands
3. Wire help system (--help, command --help)
4. Implement exit codes and error handling

**Deliverable**: Working CLI with command routing and help

### 9.4 Phase 4: Integration & Testing (Week 4)

1. Wire submodule into host `ggen` project
2. Implement `ggen paas <command>` delegation
3. Create integration tests for all commands
4. Document usage patterns and examples

**Deliverable**: Fully integrated submodule CLI with tests

---

## 10. Comparison: Manual vs. RDF-Driven

### Without RDF (Traditional Approach)

```
✗ Commands defined in multiple places (help text, handler, tests, docs)
✗ Changes to command structure require edits in 4+ files
✗ Help text and actual implementation can drift
✗ No single source of truth for API contract
✗ Difficult to validate completeness
✗ Manual synchronization between spec and code
```

### With RDF (ggen-paas Approach)

```
✓ Commands defined once in .specify/cli-commands.ttl
✓ Help, handlers, tests, docs all generated from single source
✓ ggen sync ensures consistency across all artifacts
✓ RDF spec is authoritative API contract
✓ SHACL validation ensures specification closure (100% coverage)
✓ Automatic detection of missing commands or options
✓ Deterministic, bit-perfect command generation
✓ Easy to add new commands (edit TTL, run ggen sync)
```

---

## 11. Future Extensions

### 11.1 Plugin System

Commands could be extended via plugins defined in RDF:

```turtle
cli:CustomCommand
  a cli:Plugin ;
  cli:providedBy "https://github.com/user/ggen-paas-plugin-custom" ;
  cli:executablePath "lib/plugins/custom.js" ;
  rdfs:label "custom" ;
  .
```

### 11.2 Interactive Mode

```bash
$ ggen paas --interactive

ggen-paas > generate all
↳ Generating docker... ✓
↳ Generating kubernetes... ✓
↳ Generating terraform... ✓
↳ Generating openapi... ✓

ggen-paas > validate artifacts
↳ Validating YAML... ✓
↳ Validating HCL... ✓
↳ Validating JSON... ✓
↳ Validating Markdown... ✓

ggen-paas > deploy staging
↳ Deploying 10 services...
↳ Waiting for health checks...
↳ All services healthy ✓

ggen-paas > quit
Goodbye!
```

### 11.3 Remote Command Execution

Commands could be executed on remote systems:

```bash
ggen paas generate all --remote production
# Executes command on production server via SSH
# Retrieves output back to local machine
```

---

## 12. Architecture Decision Records (ADRs)

### ADR-1: RDF-Driven CLI (ACCEPTED)

**Decision**: Define all CLI commands in RDF (.ttl), generate handler code

**Rationale**:
- Single source of truth (no drift between spec and implementation)
- Specification closure validation ensures completeness
- Deterministic outputs (same .ttl → same code every time)
- Easy to add/modify commands (edit RDF, not code)

**Alternatives Considered**:
- Hard-coded CLI (simpler initially, harder to maintain)
- JSON schema for CLI (less expressive than RDF)
- YAML config (less formal than RDF ontology)

**Consequences**:
- Requires RDF expertise to understand CLI definitions
- Generation pipeline adds complexity (SPARQL → templates → code)
- Benefits outweigh complexity for large, evolving CLIs

---

## Appendix A: File Structure Summary

```
ggen-paas/
├── bin/paas                           # Entry point
├── lib/
│   ├── commands/
│   │   ├── generate.js
│   │   ├── validate.js
│   │   ├── sync.js
│   │   ├── deploy.js
│   │   ├── status.js
│   │   ├── logs.js
│   │   ├── describe.js
│   │   └── explain.js
│   ├── cli-dispatcher.js              # Command routing
│   ├── utils/
│   │   ├── ontology.js
│   │   ├── logger.js
│   │   └── config.js
│   └── templates/                     # Template helpers
├── tests/
│   ├── commands/                      # Command tests
│   ├── cli-integration.test.js        # End-to-end tests
│   └── fixtures/                      # Test data
├── generated/
│   ├── commands/                      # Generated command code
│   └── docs/
│       ├── cli-reference.md
│       └── commands/
│           ├── generate.md
│           └── ...
├── docs/
│   ├── architecture/
│   ├── usage/
│   └── development/
├── .specify/
│   ├── cli-commands.ttl               # Source of truth
│   └── cli-schema.ttl                 # Validation schema
├── ggen-paas.config.ttl               # Main config
├── ggen-paas.toml                     # Generation rules
├── package.json
└── README.md
```

---

## Conclusion

The submodule CLI architecture follows the **Holographic Imperative**: the CLI itself is a **projection of the RDF specification** (`cli-commands.ttl`). Commands aren't "built" but "precipitated" from the ontology via the ggen sync pipeline.

This ensures:
- **Specification-First**: TTL is source of truth, code is derived
- **Deterministic**: Same spec → identical CLI every time
- **Self-Documenting**: Help text generated from spec
- **Maintainable**: Add commands by editing RDF, not code
- **Testable**: SHACL validation ensures completeness
- **Composable**: CLI specification can reference other ontologies (paas, deploy, arch)

**Next Steps**:
1. Create `.specify/cli-commands.ttl` with complete command ontology
2. Create generation templates for command handlers
3. Run `ggen sync` to generate CLI code
4. Integrate submodule into host `ggen` project
5. Document command reference and usage patterns
