# Pack System: Complete Verb Specification

## Overview

This document defines ALL verbs/commands for a feature-complete packs system. The design follows ggen's clap-noun-verb architecture and marketplace patterns.

## Verb Categories

1. **Discovery**: Finding and exploring packs
2. **Management**: Installing and removing packs
3. **Generation**: Creating projects from packs
4. **Composition**: Multi-pack operations
5. **Validation**: Quality and compatibility checking
6. **Publishing**: Sharing packs with others

---

## 1. Discovery Verbs

### `ggen pack list`

**Purpose**: List available packs from local registry and remote sources

**Arguments**:
```bash
ggen pack list [OPTIONS]

OPTIONS:
  --source <SOURCE>       Filter by source: local, remote, all [default: all]
  --category <CATEGORY>   Filter by category: startup, devops, frontend, etc.
  --tag <TAG>             Filter by tag (can specify multiple)
  --installed             Show only installed packs
  --available             Show only available (not installed) packs
  --format <FORMAT>       Output format: table, json, yaml [default: table]
  --sort <FIELD>          Sort by: name, downloads, rating, updated [default: name]
  --limit <N>             Limit results [default: 50]
```

**Output**:
```
NAME                VERSION   CATEGORY    RATING   DOWNLOADS   STATUS
startup-pack        1.2.0     startup     4.8/5    1,234       installed
devops-pack         2.0.1     devops      4.6/5    892         available
monitoring-pack     1.0.0     observ...   4.9/5    567         installed
```

**Exit Codes**:
- 0: Success
- 1: No packs found
- 2: Invalid filter/argument

---

### `ggen pack search`

**Purpose**: Search packs by query string (full-text search)

**Arguments**:
```bash
ggen pack search <QUERY> [OPTIONS]

ARGUMENTS:
  <QUERY>                 Search query string

OPTIONS:
  --category <CATEGORY>   Filter by category
  --min-rating <N>        Minimum rating (1-5)
  --limit <N>             Limit results [default: 20]
  --format <FORMAT>       Output format: table, json, yaml [default: table]
```

**Example**:
```bash
ggen pack search "kubernetes microservice"
ggen pack search "react" --category frontend
ggen pack search "startup" --min-rating 4.5
```

---

### `ggen pack show`

**Purpose**: Display detailed information about a specific pack

**Arguments**:
```bash
ggen pack show <PACK_NAME> [OPTIONS]

ARGUMENTS:
  <PACK_NAME>             Pack name or ID

OPTIONS:
  --version <VERSION>     Show specific version [default: latest]
  --format <FORMAT>       Output format: human, json, yaml [default: human]
  --show-templates        List all templates in pack
  --show-queries          List all SPARQL queries
  --show-dependencies     Show dependency tree
  --show-variables        List all configurable variables
  --show-examples         Show usage examples
```

**Output**:
```
Pack: startup-pack v1.2.0
Category: startup
Author: ggen-community
Rating: 4.8/5 (234 ratings)
Downloads: 1,234

Description:
  Complete startup project pack with backend API, frontend, and DevOps setup.

Templates:
  ✓ rust-microservice      (io.ggen.rust.api v2.1.0)
  ✓ react-frontend         (io.ggen.react.app v1.5.0)
  ✓ docker-compose         (io.ggen.docker.compose v1.0.0)

SPARQL Queries: 3
  - service-discovery.sparql
  - api-schema-generation.sparql
  - deployment-config.sparql

Dependencies:
  devops-pack >= 2.0.0
  monitoring-pack ^1.0.0

Variables (12):
  project_name         [required]  Project name
  api_port            [default: 8080]  API server port
  database_type       [default: postgres]  Database type
  ...

Installation:
  ggen pack install startup-pack

Usage:
  ggen pack generate startup-pack --output my-startup
```

---

### `ggen pack info`

**Purpose**: Quick pack information (alias for `show` with minimal output)

**Arguments**:
```bash
ggen pack info <PACK_NAME>
```

---

## 2. Management Verbs

### `ggen pack install`

**Purpose**: Install a pack to local registry

**Arguments**:
```bash
ggen pack install <PACK_NAME> [OPTIONS]

ARGUMENTS:
  <PACK_NAME>             Pack name or ID

OPTIONS:
  --version <VERSION>     Install specific version [default: latest]
  --source <URL>          Install from custom source/URL
  --force                 Force reinstall if already installed
  --no-dependencies       Skip dependency installation
  --dry-run               Show what would be installed without installing
```

**Behavior**:
1. Resolve pack dependencies
2. Download pack manifest and templates
3. Verify checksums
4. Install to `~/.ggen/packs/<pack-name>/`
5. Update local pack registry

**Output**:
```
Installing startup-pack v1.2.0...
Resolving dependencies...
  ✓ devops-pack v2.0.1
  ✓ monitoring-pack v1.0.0
Downloading templates...
  ✓ rust-microservice (2.1 MB)
  ✓ react-frontend (1.8 MB)
  ✓ docker-compose (0.5 MB)
Installing to ~/.ggen/packs/startup-pack/
✓ Installation complete

To use: ggen pack generate startup-pack --output my-project
```

---

### `ggen pack uninstall`

**Purpose**: Remove an installed pack

**Arguments**:
```bash
ggen pack uninstall <PACK_NAME> [OPTIONS]

ARGUMENTS:
  <PACK_NAME>             Pack name to uninstall

OPTIONS:
  --purge                 Remove generated projects cache
  --keep-dependencies     Don't uninstall dependencies
  --force                 Skip confirmation prompt
```

---

### `ggen pack update`

**Purpose**: Update installed pack(s) to latest version

**Arguments**:
```bash
ggen pack update [PACK_NAME] [OPTIONS]

ARGUMENTS:
  [PACK_NAME]             Pack to update (omit for all packs)

OPTIONS:
  --check                 Check for updates without updating
  --dry-run               Show what would be updated
```

**Output**:
```
Checking for updates...
  startup-pack: 1.2.0 → 1.3.0 (available)
  devops-pack: 2.0.1 (up to date)
  monitoring-pack: 1.0.0 → 1.1.0 (available)

Update 2 packs? [y/N]: y
Updating startup-pack...
Updating monitoring-pack...
✓ All packs updated
```

---

### `ggen pack clean`

**Purpose**: Clean pack cache and temporary files

**Arguments**:
```bash
ggen pack clean [OPTIONS]

OPTIONS:
  --cache                 Clean generation cache
  --downloads             Clean downloaded pack archives
  --all                   Clean everything
```

---

## 3. Generation Verbs

### `ggen pack generate`

**Purpose**: Generate a project from a pack

**Arguments**:
```bash
ggen pack generate <PACK_NAME> [OPTIONS]

ARGUMENTS:
  <PACK_NAME>             Pack to generate from

OPTIONS:
  --output <DIR>          Output directory [required]
  --var <KEY=VALUE>       Set template variable (repeatable)
  --vars-file <FILE>      Load variables from YAML/JSON file
  --version <VERSION>     Use specific pack version
  --dry-run               Show what would be generated
  --overwrite             Overwrite existing files
  --interactive           Prompt for variables interactively
  --template <NAME>       Generate only specific template from pack
  --skip-hooks            Skip pre/post generation hooks
  --skip-queries          Skip SPARQL query execution
```

**Examples**:
```bash
# Basic generation
ggen pack generate startup-pack --output my-startup

# With variables
ggen pack generate startup-pack --output my-app \
  --var project_name=MyApp \
  --var api_port=3000 \
  --var database_type=mongodb

# Interactive mode
ggen pack generate startup-pack --output my-app --interactive

# From variables file
ggen pack generate startup-pack --output my-app --vars-file vars.yaml

# Dry run
ggen pack generate startup-pack --output my-app --dry-run
```

**Output**:
```
Generating project from startup-pack v1.2.0...

Variables:
  project_name = MyStartup
  api_port = 8080
  database_type = postgres

Executing templates...
  ✓ rust-microservice → src/backend/
  ✓ react-frontend → src/frontend/
  ✓ docker-compose → docker/

Executing SPARQL queries...
  ✓ service-discovery.sparql → config/services.yaml
  ✓ api-schema-generation.sparql → api/schema.json

Running post-generation hooks...
  ✓ format-code
  ✓ install-dependencies

✓ Project generated to: my-startup/

Next steps:
  cd my-startup
  docker-compose up
```

---

### `ggen pack regenerate`

**Purpose**: Regenerate parts of an existing pack-generated project

**Arguments**:
```bash
ggen pack regenerate [OPTIONS]

OPTIONS:
  --template <NAME>       Regenerate specific template
  --all                   Regenerate entire project
  --preserve <PATTERN>    Preserve files matching pattern
  --dry-run               Show what would be regenerated
```

**Use Case**: Update generated code after pack version update without losing manual changes.

---

## 4. Composition Verbs

### `ggen pack compose`

**Purpose**: Compose multiple packs into a single project

**Arguments**:
```bash
ggen pack compose [OPTIONS]

OPTIONS:
  --packs <PACK1,PACK2,...>   Comma-separated pack names [required]
  --output <DIR>              Output directory [required]
  --vars-file <FILE>          Variables for all packs
  --composition-file <FILE>   Load composition from YAML
  --resolve-conflicts <MODE>  Conflict resolution: ask, overwrite, skip, merge
  --dry-run                   Show composition plan without executing
  --interactive               Prompt for conflict resolution
```

**Examples**:
```bash
# Compose 3 packs
ggen pack compose \
  --packs startup-pack,devops-pack,monitoring-pack \
  --output my-complete-app

# From composition file
ggen pack compose --composition-file composition.yaml --output my-app
```

**composition.yaml Example**:
```yaml
name: my-complete-app
packs:
  - name: startup-pack
    version: 1.2.0
    variables:
      project_name: MyApp
      api_port: 8080

  - name: devops-pack
    version: 2.0.1
    variables:
      ci_platform: github-actions

  - name: monitoring-pack
    version: 1.0.0
    variables:
      metrics_backend: prometheus

conflict_resolution:
  mode: merge
  rules:
    - pattern: "*.md"
      action: merge
    - pattern: "Dockerfile"
      action: overwrite
      priority: devops-pack
```

---

### `ggen pack merge`

**Purpose**: Merge pack-generated projects (post-generation merge)

**Arguments**:
```bash
ggen pack merge <SOURCE_DIR> <TARGET_DIR> [OPTIONS]

ARGUMENTS:
  <SOURCE_DIR>            Source project directory
  <TARGET_DIR>            Target project directory

OPTIONS:
  --strategy <STRATEGY>   Merge strategy: overwrite, skip, ask, smart
  --preserve <PATTERN>    Preserve files matching pattern
  --dry-run               Show merge plan
```

---

### `ggen pack plan`

**Purpose**: Show composition plan without executing

**Arguments**:
```bash
ggen pack plan [OPTIONS]

OPTIONS:
  --packs <PACK1,PACK2,...>   Packs to compose
  --composition-file <FILE>   Load composition
  --format <FORMAT>           Output format: human, json, yaml
```

**Output**:
```
Composition Plan for: startup-pack + devops-pack + monitoring-pack

Dependency Resolution:
  ✓ startup-pack v1.2.0
    → devops-pack v2.0.1 (satisfied)
    → monitoring-pack v1.0.0 (satisfied)
  ✓ devops-pack v2.0.1
  ✓ monitoring-pack v1.0.0

Template Execution Order:
  1. devops-pack/docker-base
  2. startup-pack/rust-microservice
  3. startup-pack/react-frontend
  4. monitoring-pack/prometheus-config
  5. devops-pack/ci-pipeline

Variable Requirements (23 total):
  Required (3):
    - project_name
    - domain
    - organization

  Optional (20):
    - api_port (default: 8080)
    - database_type (default: postgres)
    ...

File Conflicts (2):
  • Dockerfile
    - startup-pack → Dockerfile
    - devops-pack → Dockerfile
    Resolution: Use devops-pack (priority)

  • README.md
    - All 3 packs generate README.md
    Resolution: Merge sections

SPARQL Queries: 7 total

Estimated Generation Time: 8-12 seconds
```

---

## 5. Validation Verbs

### `ggen pack validate`

**Purpose**: Validate pack compatibility and quality

**Arguments**:
```bash
ggen pack validate <PACK_NAME> [OPTIONS]

ARGUMENTS:
  <PACK_NAME>             Pack to validate

OPTIONS:
  --compatibility         Check compatibility with other packs
  --with <PACK>           Check compatibility with specific pack
  --check-templates       Validate all templates
  --check-queries         Validate SPARQL queries
  --check-dependencies    Verify dependencies exist
  --check-variables       Check variable definitions
  --strict                Fail on warnings
  --format <FORMAT>       Output format: human, json, yaml
```

**Output**:
```
Validating startup-pack v1.2.0...

✓ Pack manifest valid
✓ All dependencies exist
✓ Templates (3/3 valid)
✓ SPARQL queries (3/3 valid)
✓ Variables (12/12 valid)

Compatibility Check with devops-pack:
  ✓ No conflicts
  ✓ Shared variables compatible
  ✓ Template outputs compatible

Warnings:
  ⚠ Variable 'api_port' has no default value
  ⚠ Template 'docker-compose' is deprecated

Validation Score: 92/100

Status: Production Ready
```

---

### `ggen pack lint`

**Purpose**: Lint pack for quality issues (like template lint)

**Arguments**:
```bash
ggen pack lint <PACK_PATH> [OPTIONS]

ARGUMENTS:
  <PACK_PATH>             Path to pack directory or manifest

OPTIONS:
  --strict                Fail on warnings
  --fix                   Auto-fix issues where possible
  --format <FORMAT>       Output format: human, json, yaml
```

**Checks**:
- Manifest syntax and completeness
- Template validity
- SPARQL query syntax
- Variable naming conventions
- Dependency version constraints
- Documentation completeness

---

### `ggen pack check`

**Purpose**: Quick health check (alias for validate with minimal checks)

**Arguments**:
```bash
ggen pack check <PACK_NAME>
```

---

## 6. Publishing Verbs

### `ggen pack publish`

**Purpose**: Publish a pack to registry

**Arguments**:
```bash
ggen pack publish <PACK_PATH> [OPTIONS]

ARGUMENTS:
  <PACK_PATH>             Path to pack directory

OPTIONS:
  --registry <URL>        Target registry [default: official]
  --token <TOKEN>         Authentication token
  --dry-run               Validate without publishing
  --force                 Publish even with warnings
  --tag <TAG>             Additional tags
```

**Pre-publish Checks**:
1. Pack validation (must score > 80/100)
2. Template validation
3. SPARQL query validation
4. Documentation completeness
5. Version uniqueness

**Output**:
```
Publishing startup-pack v1.2.0...

Pre-publish Validation:
  ✓ Pack manifest valid
  ✓ All templates valid
  ✓ SPARQL queries valid
  ✓ Documentation complete (README, CHANGELOG, LICENSE)
  ✓ Version 1.2.0 available

Packaging...
  ✓ Templates bundled (4.2 MB)
  ✓ Manifest generated
  ✓ Checksum computed

Uploading to registry...
  ✓ Published to: https://packs.ggen.io/startup-pack/1.2.0

View: ggen pack show startup-pack
```

---

### `ggen pack create`

**Purpose**: Create a new pack from scratch (interactive wizard)

**Arguments**:
```bash
ggen pack create [OPTIONS]

OPTIONS:
  --name <NAME>           Pack name
  --category <CATEGORY>   Pack category
  --template <TEMPLATE>   Initialize from template
  --interactive           Interactive wizard [default]
  --output <DIR>          Output directory [default: ./<name>]
```

**Interactive Wizard**:
```
Creating new pack...

Pack name: my-custom-pack
Category: [startup/devops/frontend/backend/other]: startup
Description: My custom startup pack
Author: john@example.com
License: [MIT/Apache-2.0/other]: MIT

Add templates:
  1. From marketplace
  2. From local file
  3. Inline template

Select option [1-3]: 1
Search marketplace: rust microservice
Found: io.ggen.rust.api v2.1.0
Add this template? [y/N]: y

Add more templates? [y/N]: n

Add SPARQL queries? [y/N]: n

Create pack at ./my-custom-pack? [y/N]: y

✓ Pack created at: ./my-custom-pack
✓ Generated pack.toml

Next steps:
  cd my-custom-pack
  ggen pack lint .
  ggen pack publish .
```

---

### `ggen pack init`

**Purpose**: Initialize a pack in existing directory

**Arguments**:
```bash
ggen pack init [OPTIONS]

OPTIONS:
  --name <NAME>           Pack name [default: dir name]
  --interactive           Interactive wizard
```

---

## 7. Benchmarking/Scoring Verbs

### `ggen pack benchmark`

**Purpose**: Benchmark pack generation performance

**Arguments**:
```bash
ggen pack benchmark <PACK_NAME> [OPTIONS]

ARGUMENTS:
  <PACK_NAME>             Pack to benchmark

OPTIONS:
  --iterations <N>        Number of iterations [default: 10]
  --output <FILE>         Save results to file
  --compare <PACK>        Compare with another pack
  --format <FORMAT>       Output format: human, json, yaml
```

**Output**:
```
Benchmarking startup-pack v1.2.0...

Running 10 iterations...

Results:
  Mean generation time:    8.4s  (± 0.8s)
  Median:                  8.2s
  Min:                     7.1s
  Max:                     10.3s

  Template rendering:      4.2s  (50%)
  SPARQL execution:        2.8s  (33%)
  File I/O:               1.4s  (17%)

Performance Grade: A- (faster than 85% of packs)

Breakdown by Template:
  rust-microservice:       3.2s  (38%)
  react-frontend:          2.1s  (25%)
  docker-compose:          0.9s  (11%)
  ...
```

---

### `ggen pack score`

**Purpose**: Score pack quality/maturity (like marketplace maturity)

**Arguments**:
```bash
ggen pack score <PACK_NAME> [OPTIONS]

ARGUMENTS:
  <PACK_NAME>             Pack to score

OPTIONS:
  --format <FORMAT>       Output format: human, json, yaml
  --verbose               Show detailed scoring breakdown
```

**Output**:
```
Pack Maturity Score: startup-pack v1.2.0

Overall Score: 87/100 (Production Ready)

Category Scores:
  Documentation:   92/100  ⭐⭐⭐⭐⭐
  Testing:         85/100  ⭐⭐⭐⭐☆
  Templates:       90/100  ⭐⭐⭐⭐⭐
  Dependencies:    88/100  ⭐⭐⭐⭐☆
  Maintenance:     80/100  ⭐⭐⭐⭐☆
  Community:       75/100  ⭐⭐⭐⭐☆

Strengths:
  ✓ Comprehensive documentation
  ✓ All templates validated
  ✓ Active maintenance (updated 2 days ago)

Improvements:
  • Add integration tests
  • Increase test coverage (currently 78%)
  • Add more usage examples

Maturity Level: Production Ready
Recommended for: All projects
```

---

## 8. Utility Verbs

### `ggen pack tree`

**Purpose**: Display pack dependency tree

**Arguments**:
```bash
ggen pack tree <PACK_NAME> [OPTIONS]

ARGUMENTS:
  <PACK_NAME>             Pack to display tree for

OPTIONS:
  --depth <N>             Maximum depth [default: unlimited]
  --format <FORMAT>       Output format: tree, json, dot
```

**Output**:
```
startup-pack v1.2.0
├─┬ devops-pack v2.0.1
│ ├── docker-base v1.0.0
│ └── ci-templates v1.5.0
└─┬ monitoring-pack v1.0.0
  ├── prometheus-config v2.0.0
  └── grafana-dashboards v1.2.0
```

---

### `ggen pack diff`

**Purpose**: Compare two pack versions

**Arguments**:
```bash
ggen pack diff <PACK_NAME> <VERSION1> <VERSION2> [OPTIONS]

ARGUMENTS:
  <PACK_NAME>             Pack name
  <VERSION1>              First version
  <VERSION2>              Second version

OPTIONS:
  --format <FORMAT>       Output format: human, unified, json
  --templates             Show template changes only
  --variables             Show variable changes only
```

---

### `ggen pack export`

**Purpose**: Export pack as standalone bundle

**Arguments**:
```bash
ggen pack export <PACK_NAME> [OPTIONS]

ARGUMENTS:
  <PACK_NAME>             Pack to export

OPTIONS:
  --output <FILE>         Output file [default: <pack>.tar.gz]
  --include-dependencies  Bundle dependencies
  --format <FORMAT>       Archive format: tar.gz, zip
```

---

### `ggen pack import`

**Purpose**: Import pack from bundle

**Arguments**:
```bash
ggen pack import <FILE> [OPTIONS]

ARGUMENTS:
  <FILE>                  Pack bundle file

OPTIONS:
  --force                 Overwrite existing pack
  --verify                Verify checksums
```

---

## Summary: Complete Verb Matrix

| Category | Verbs | Count |
|----------|-------|-------|
| **Discovery** | list, search, show, info | 4 |
| **Management** | install, uninstall, update, clean | 4 |
| **Generation** | generate, regenerate | 2 |
| **Composition** | compose, merge, plan | 3 |
| **Validation** | validate, lint, check | 3 |
| **Publishing** | publish, create, init | 3 |
| **Scoring** | benchmark, score | 2 |
| **Utility** | tree, diff, export, import | 4 |

**Total: 25 verbs**

## Prioritization

### MVP (Must Have)
1. list, show, search
2. install, uninstall
3. generate
4. validate
5. create

### Phase 2 (Should Have)
6. compose, plan
7. benchmark, score
8. publish
9. update, clean

### Phase 3 (Nice to Have)
10. regenerate, merge
11. lint, check
12. tree, diff
13. export, import
14. init, info

## CLI Consistency

All verbs follow ggen's established patterns:
- clap-noun-verb architecture
- JSON/YAML/human-readable output formats
- Consistent flag naming (--output, --format, --dry-run)
- Exit codes: 0 (success), 1 (error), 2 (validation failure)
- Progressive verbosity (--verbose, --quiet)
- Interactive prompts with sensible defaults
