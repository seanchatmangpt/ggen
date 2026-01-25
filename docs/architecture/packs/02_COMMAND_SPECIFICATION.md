<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Packs Command Specification](#packs-command-specification)
  - [Command Priority Levels](#command-priority-levels)
    - [CRITICAL (Must Have) - 80% Value](#critical-must-have---80-value)
    - [HIGH (Essential) - 15% Value](#high-essential---15-value)
    - [MEDIUM (Nice to Have) - 4% Value](#medium-nice-to-have---4-value)
    - [LOW (Future) - 1% Value](#low-future---1-value)
  - [CRITICAL Commands (Must Have)](#critical-commands-must-have)
    - [1. `ggen packs list`](#1-ggen-packs-list)
    - [2. `ggen packs show <pack-id>`](#2-ggen-packs-show-pack-id)
    - [3. `ggen packs install <pack-id>`](#3-ggen-packs-install-pack-id)
    - [4. `ggen packs generate <pack-id> <project-name>`](#4-ggen-packs-generate-pack-id-project-name)
    - [5. `ggen packs validate <pack-id>`](#5-ggen-packs-validate-pack-id)
  - [HIGH Commands (Essential)](#high-commands-essential)
    - [6. `ggen packs compose <pack-ids...>`](#6-ggen-packs-compose-pack-ids)
    - [7. `ggen packs dependencies <pack-id>`](#7-ggen-packs-dependencies-pack-id)
    - [8. `ggen packs info <pack-id>`](#8-ggen-packs-info-pack-id)
    - [9. `ggen packs search <query>`](#9-ggen-packs-search-query)
  - [Implementation Roadmap](#implementation-roadmap)
    - [Phase 1: CRITICAL Commands (Week 1-2)](#phase-1-critical-commands-week-1-2)
    - [Phase 2: HIGH Commands (Week 3)](#phase-2-high-commands-week-3)
    - [Phase 3: MEDIUM Commands (Week 4)](#phase-3-medium-commands-week-4)
    - [Phase 4: LOW Commands (Future)](#phase-4-low-commands-future)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Packs Command Specification

**Version:** 3.2.0
**Prioritization:** 80/20 Rule (Focus on commands that enable 80% of workflows)

## Command Priority Levels

### CRITICAL (Must Have) - 80% Value
These commands enable complete user workflows and are essential for MVP:

1. `list` - Discover available packs
2. `show` - Inspect pack contents
3. `install` - Install pack packages
4. `generate` - Create project from pack templates
5. `validate` - Verify pack integrity

### HIGH (Essential) - 15% Value
These commands enhance usability and enable advanced workflows:

6. `compose` - Combine multiple packs
7. `dependencies` - Show dependency tree
8. `info` - Detailed pack information
9. `search` - Find packs by criteria

### MEDIUM (Nice to Have) - 4% Value
These commands improve developer experience:

10. `score` - Calculate health score
11. `benchmark` - Performance metrics

### LOW (Future) - 1% Value
These commands can be deferred to later versions:

12. `publish` - Publish pack to marketplace
13. `import` - Import external pack
14. `export` - Export pack definition
15. `diff` - Compare pack versions

---

## CRITICAL Commands (Must Have)

### 1. `ggen packs list`

**Purpose:** Discover available packs in marketplace and local installation

**Usage:**
```bash
# List all packs
ggen packs list

# Filter by category
ggen packs list --category web

# Show only installed
ggen packs list --installed

# JSON output
ggen packs list --json
```

**Implementation:**
```rust
pub async fn list_packs(input: &ListPacksInput) -> Result<ListPacksOutput> {
    // 1. Load packs from marketplace registry
    let registry_packs = load_registry_packs().await?;

    // 2. Load installed packs
    let installed_packs = load_installed_packs()?;

    // 3. Merge and deduplicate
    let all_packs = merge_pack_lists(registry_packs, installed_packs);

    // 4. Apply filters
    let filtered = apply_filters(all_packs, &input.filters);

    // 5. Sort by relevance
    let sorted = sort_by_relevance(filtered);

    Ok(ListPacksOutput { packs: sorted })
}
```

**Output Example:**
```
Available Packs (5 found):

  web-api-starter (v1.2.0) ✓ Installed
  └─ Full-stack web API with REST + GraphQL
     Category: web | Packages: 8 | Templates: 12

  ml-data-science (v2.0.1)
  └─ Machine learning project with notebooks
     Category: ml | Packages: 15 | Templates: 8

  cli-app-template (v0.5.0) ✓ Installed
  └─ Command-line application scaffold
     Category: cli | Packages: 3 | Templates: 6
```

**Exit Codes:**
- 0: Success
- 1: No packs found
- 2: Registry connection failed

---

### 2. `ggen packs show <pack-id>`

**Purpose:** Display detailed information about a specific pack

**Usage:**
```bash
# Show pack details
ggen packs show web-api-starter

# Include template list
ggen packs show web-api-starter --templates

# Include dependency tree
ggen packs show web-api-starter --dependencies

# Raw manifest
ggen packs show web-api-starter --manifest
```

**Implementation:**
```rust
pub async fn show_pack(pack_id: &str, options: &ShowOptions) -> Result<PackDetails> {
    // 1. Load pack manifest
    let pack = load_pack_metadata(pack_id)?;

    // 2. Load additional info based on options
    let templates = if options.show_templates {
        Some(list_pack_templates(&pack)?)
    } else {
        None
    };

    let dependencies = if options.show_dependencies {
        Some(resolve_dependency_tree(&pack).await?)
    } else {
        None
    };

    Ok(PackDetails {
        pack,
        templates,
        dependencies,
    })
}
```

**Output Example:**
```
Pack: web-api-starter v1.2.0

Description:
  Production-ready web API starter with REST and GraphQL endpoints,
  authentication, database integration, and deployment configs.

Author: ggen-team
Category: web
Tags: rest, graphql, api, backend, production

Packages (8):
  ✓ express-api         v4.18.2
  ✓ graphql-server      v4.1.0
  ✓ auth-jwt            v1.5.0
  ✓ database-postgres   v3.2.1
  ✓ logging-winston     v2.8.0
  ✓ testing-jest        v29.5.0
  ✓ docker-compose      v2.1.0
  ✓ ci-github-actions   v1.0.0

Templates (12):
  • src/server.ts           - Express server setup
  • src/graphql/schema.ts   - GraphQL schema
  • src/auth/jwt.ts         - JWT authentication
  • src/db/connection.ts    - Database connection
  • tests/api.test.ts       - API integration tests
  • docker-compose.yml      - Docker compose config
  • .github/workflows/ci.yml - CI/CD pipeline
  • README.md               - Project documentation
  • package.json            - NPM dependencies
  • tsconfig.json           - TypeScript config
  • .env.example            - Environment template
  • Dockerfile              - Container definition

Dependencies (2):
  • base-typescript v1.0.0  (provides TypeScript config)
  • devops-basics   v0.8.0  (provides Docker + CI templates)

SPARQL Queries (3):
  • find_related_packages   - Discover related packages
  • validate_compatibility  - Check version compatibility
  • generate_docs           - Extract documentation

Installation Status: ✓ Installed
Location: ~/.ggen/packages/web-api-starter
Health Score: 92/100
```

---

### 3. `ggen packs install <pack-id>`

**Purpose:** Install all packages in a pack (REAL marketplace integration)

**Usage:**
```bash
# Install pack
ggen packs install web-api-starter

# Install specific version
ggen packs install web-api-starter@1.2.0

# Install to custom location
ggen packs install web-api-starter --target ./my-packs

# Force reinstall
ggen packs install web-api-starter --force

# Dry run
ggen packs install web-api-starter --dry-run

# Skip dependencies
ggen packs install web-api-starter --no-dependencies
```

**Implementation:**
```rust
pub async fn install_pack(input: &InstallPackInput) -> Result<InstallPackOutput> {
    // 1. Load pack manifest
    let pack = load_pack_metadata(&input.pack_id)?;

    // 2. Resolve dependencies
    let dep_graph = if input.install_dependencies {
        resolve_dependency_graph(&pack).await?
    } else {
        DependencyGraph::from_pack(&pack)
    };

    // 3. Validate no circular dependencies
    dep_graph.validate_acyclic()?;

    // 4. Topological sort for install order
    let install_order = dep_graph.topological_sort()?;

    // 5. Install packages in order (REAL marketplace calls)
    let mut installed = Vec::new();
    for package_ref in install_order {
        let result = marketplace::install_package(&InstallOptions {
            package_name: package_ref.name.clone(),
            version: Some(package_ref.version.clone()),
            target_path: input.target_dir.clone(),
            force: input.force,
            with_dependencies: false, // Already resolved
            dry_run: input.dry_run,
        }).await?;

        installed.push(result);
    }

    // 6. Update pack lockfile
    if !input.dry_run {
        update_pack_lockfile(&pack, &installed)?;
    }

    Ok(InstallPackOutput {
        pack_id: input.pack_id.clone(),
        packages_installed: installed,
        total_time: start.elapsed(),
    })
}
```

**Output Example:**
```
📦 Installing pack: web-api-starter v1.2.0

Resolving dependencies...
  ✓ Found 8 packages + 5 dependencies

Installing packages (13 total):
  [1/13] express-api@4.18.2         ████████████████ ✓ (2.3s)
  [2/13] graphql-server@4.1.0       ████████████████ ✓ (1.8s)
  [3/13] auth-jwt@1.5.0             ████████████████ ✓ (1.2s)
  [4/13] database-postgres@3.2.1    ████████████████ ✓ (3.1s)
  [5/13] logging-winston@2.8.0      ████████████████ ✓ (0.9s)
  [6/13] testing-jest@29.5.0        ████████████████ ✓ (4.2s)
  [7/13] docker-compose@2.1.0       ████████████████ ✓ (1.1s)
  [8/13] ci-github-actions@1.0.0    ████████████████ ✓ (0.8s)
  [9/13] typescript-base@1.0.0      ████████████████ ✓ (1.5s)
  [10/13] eslint-config@2.3.0       ████████████████ ✓ (1.0s)
  [11/13] prettier-config@1.8.0     ████████████████ ✓ (0.7s)
  [12/13] tsconfig-base@3.0.0       ████████████████ ✓ (0.5s)
  [13/13] devops-basics@0.8.0       ████████████████ ✓ (2.1s)

✅ Pack installed successfully!
   Location: ~/.ggen/packages/web-api-starter
   Total packages: 13
   Total time: 21.2s

Next steps:
  1. Generate project: ggen packs generate web-api-starter my-api
  2. View templates: ggen packs show web-api-starter --templates
```

---

### 4. `ggen packs generate <pack-id> <project-name>`

**Purpose:** Generate project from pack templates (REAL template rendering)

**Usage:**
```bash
# Generate project
ggen packs generate web-api-starter my-api

# Specify variables
ggen packs generate web-api-starter my-api \
  --var author="John Doe" \
  --var license=MIT \
  --var database=postgres

# Specific template only
ggen packs generate web-api-starter my-api \
  --template server.ts

# Output to custom directory
ggen packs generate web-api-starter my-api \
  --output ~/projects/my-api

# Interactive mode
ggen packs generate web-api-starter my-api --interactive
```

**Implementation:**
```rust
pub async fn generate_from_pack(input: &GenerateInput) -> Result<GenerateOutput> {
    // 1. Load pack manifest
    let pack = load_pack_metadata(&input.pack_id)?;

    // 2. Resolve variables (user + defaults + auto-detected)
    let variables = resolve_variables(&pack, &input)?;

    // 3. Validate required variables present
    validate_required_variables(&pack.templates, &variables)?;

    // 4. Filter templates if specific template requested
    let templates = if let Some(ref tmpl) = input.template_name {
        pack.templates.iter()
            .filter(|t| t.name == *tmpl)
            .collect()
    } else {
        pack.templates.iter().collect()
    };

    // 5. Render each template (REAL template engine calls)
    let mut generated = Vec::new();
    for template in templates {
        let result = template::generate_file(&GenerateFileOptions {
            template_path: resolve_template_path(&pack, template)?,
            output_path: input.output_dir.join(&template.output_path),
            variables: variables.clone(),
            force_overwrite: input.force,
        })?;

        generated.push(result);
    }

    // 6. Run post-generation hooks (if any)
    run_post_generation_hooks(&pack, &input.output_dir)?;

    Ok(GenerateOutput {
        pack_id: input.pack_id.clone(),
        project_name: input.project_name.clone(),
        templates_generated: generated.len(),
        files_created: generated.iter().map(|r| r.files_created).sum(),
        output_path: input.output_dir.clone(),
    })
}
```

**Output Example:**
```
🚀 Generating project: my-api from web-api-starter v1.2.0

Variables:
  • project_name: my-api
  • author: John Doe
  • license: MIT
  • database: postgres
  • timestamp: 2025-01-15T10:30:00Z

Rendering templates (12 files):
  ✓ src/server.ts                 (324 bytes)
  ✓ src/graphql/schema.ts         (1.2 KB)
  ✓ src/auth/jwt.ts               (856 bytes)
  ✓ src/db/connection.ts          (432 bytes)
  ✓ tests/api.test.ts             (2.1 KB)
  ✓ docker-compose.yml            (568 bytes)
  ✓ .github/workflows/ci.yml      (891 bytes)
  ✓ README.md                     (3.4 KB)
  ✓ package.json                  (712 bytes)
  ✓ tsconfig.json                 (398 bytes)
  ✓ .env.example                  (245 bytes)
  ✓ Dockerfile                    (634 bytes)

✅ Project generated successfully!
   Location: ./my-api
   Files created: 12
   Total size: 11.2 KB

Next steps:
  1. cd my-api
  2. npm install
  3. npm run dev
```

---

### 5. `ggen packs validate <pack-id>`

**Purpose:** Validate pack integrity and dependencies

**Usage:**
```bash
# Validate pack
ggen packs validate web-api-starter

# Validate with deep checks
ggen packs validate web-api-starter --deep

# Check specific aspects
ggen packs validate web-api-starter --check dependencies
ggen packs validate web-api-starter --check templates
ggen packs validate web-api-starter --check manifest
```

**Implementation:**
```rust
pub async fn validate_pack(input: &ValidateInput) -> Result<ValidationResult> {
    let pack = load_pack_metadata(&input.pack_id)?;
    let mut checks = Vec::new();

    // 1. Manifest syntax validation
    checks.push(validate_manifest_syntax(&pack)?);

    // 2. Package existence validation (marketplace check)
    if input.deep || input.check_packages {
        for pkg in &pack.packages {
            let exists = marketplace::package_exists(&pkg.name, &pkg.version).await?;
            checks.push(ValidationCheck {
                name: format!("package:{}", pkg.name),
                passed: exists,
                message: if exists {
                    format!("Package {} exists in marketplace", pkg.name)
                } else {
                    format!("Package {} not found", pkg.name)
                },
            });
        }
    }

    // 3. Template validation
    if input.deep || input.check_templates {
        for tmpl in &pack.templates {
            let result = validate_template(&pack, tmpl)?;
            checks.push(result);
        }
    }

    // 4. Dependency validation
    if input.deep || input.check_dependencies {
        let dep_result = validate_dependencies(&pack).await?;
        checks.push(dep_result);
    }

    // 5. SPARQL query validation
    if input.deep {
        for (name, query) in &pack.sparql_queries {
            let result = validate_sparql_syntax(query)?;
            checks.push(ValidationCheck {
                name: format!("sparql:{}", name),
                passed: result.is_ok(),
                message: result.err().map(|e| e.to_string()).unwrap_or_default(),
            });
        }
    }

    let passed = checks.iter().all(|c| c.passed);

    Ok(ValidationResult {
        pack_id: input.pack_id.clone(),
        passed,
        checks,
    })
}
```

**Output Example:**
```
🔍 Validating pack: web-api-starter v1.2.0

Manifest Syntax:
  ✓ TOML structure valid
  ✓ Required fields present
  ✓ Version format correct

Package Availability (8 packages):
  ✓ express-api@4.18.2         (found in marketplace)
  ✓ graphql-server@4.1.0       (found in marketplace)
  ✓ auth-jwt@1.5.0             (found in marketplace)
  ✓ database-postgres@3.2.1    (found in marketplace)
  ✓ logging-winston@2.8.0      (found in marketplace)
  ✓ testing-jest@29.5.0        (found in marketplace)
  ✓ docker-compose@2.1.0       (found in marketplace)
  ✓ ci-github-actions@1.0.0    (found in marketplace)

Template Validation (12 templates):
  ✓ src/server.ts              (syntax valid, variables defined)
  ✓ src/graphql/schema.ts      (syntax valid, variables defined)
  ✓ src/auth/jwt.ts            (syntax valid, variables defined)
  ⚠ tests/api.test.ts          (optional variable 'test_framework' not defined)
  ✓ docker-compose.yml         (syntax valid)
  ✓ .github/workflows/ci.yml   (syntax valid)
  ✓ README.md                  (syntax valid)
  ✓ package.json               (valid JSON schema)
  ✓ tsconfig.json              (valid JSON schema)
  ✓ .env.example               (syntax valid)
  ✓ Dockerfile                 (syntax valid)

Dependencies:
  ✓ No circular dependencies
  ✓ base-typescript v1.0.0     (available)
  ✓ devops-basics v0.8.0       (available)
  ✓ Version constraints satisfied

SPARQL Queries (3):
  ✓ find_related_packages      (syntax valid)
  ✓ validate_compatibility     (syntax valid)
  ✓ generate_docs              (syntax valid)

Overall: ✅ VALID (1 warning)
Health Score: 92/100
```

---

## HIGH Commands (Essential)

### 6. `ggen packs compose <pack-ids...>`

**Purpose:** Combine multiple packs into a unified project

**Usage:**
```bash
# Compose multiple packs
ggen packs compose web-api-starter ml-data-science devops-basics \
  --output my-full-stack-app

# Resolve conflicts interactively
ggen packs compose web-api ml-data --strategy interactive

# Merge strategy
ggen packs compose web-api ml-data --strategy merge  # default
ggen packs compose web-api ml-data --strategy layer  # order matters
```

**Output:**
```
🔀 Composing packs: web-api-starter + ml-data-science + devops-basics

Analyzing composition:
  • Total packages: 25 (8 + 15 + 2)
  • Duplicate packages: 3
  • Conflicting versions: 1

Resolving conflicts:
  ⚠ typescript: v4.9.0 (web-api) vs v5.0.0 (ml-data)
     → Resolution: Use highest compatible version: v5.0.0

  ✓ docker-compose: v2.1.0 (same in web-api and devops-basics)
  ✓ eslint-config: v2.3.0 (same in web-api and ml-data)

Merging templates:
  • web-api:      12 templates
  • ml-data:      8 templates
  • devops:       4 templates
  • Conflicts:    2 (README.md, .gitignore)
     → Resolution: Merge sections with markers

✅ Composition complete!
   Output: composition.pack.toml
   Total packages: 22 (deduplicated)
   Total templates: 22 (2 merged)
```

### 7. `ggen packs dependencies <pack-id>`

**Purpose:** Show dependency tree with version resolution

**Output:**
```
🌳 Dependency tree: web-api-starter v1.2.0

web-api-starter@1.2.0
├── base-typescript@1.0.0
│   ├── typescript@5.0.0
│   └── tsconfig-base@3.0.0
└── devops-basics@0.8.0
    ├── docker-compose@2.1.0
    ├── ci-github-actions@1.0.0
    └── monitoring-prometheus@1.5.0
        └── metrics-client@2.0.0

Total dependencies: 8 (3 direct, 5 transitive)
Max depth: 3
Conflicts: 0
```

### 8. `ggen packs info <pack-id>`

Enhanced version of `show` with performance metrics:

```
Performance Metrics:
  • Install time: ~21s (8 packages)
  • Generation time: ~2s (12 templates)
  • Disk usage: 45 MB (packages + templates)
  • Cache hit rate: 85%

Popularity:
  • Downloads: 12,543
  • Stars: 234
  • Contributors: 8

Quality:
  • Health score: 92/100
  • Test coverage: 87%
  • Documentation: Complete
  • Last updated: 2025-01-10
```

### 9. `ggen packs search <query>`

**Purpose:** Search packs by keywords, tags, category

**Usage:**
```bash
# Text search
ggen packs search "web api"

# Filter by category
ggen packs search --category ml

# Filter by tags
ggen packs search --tags rest,graphql

# Fuzzy search
ggen packs search "webapi" --fuzzy
```

---

## Implementation Roadmap

### Phase 1: CRITICAL Commands (Week 1-2)
- ✅ Implement list, show, install, generate, validate
- ✅ Real marketplace integration
- ✅ Real template rendering
- ✅ Dependency resolution
- ✅ Error handling for all failure modes

### Phase 2: HIGH Commands (Week 3)
- Implement compose, dependencies, info, search
- Advanced features (conflict resolution, performance metrics)

### Phase 3: MEDIUM Commands (Week 4)
- Implement score, benchmark
- Performance optimization

### Phase 4: LOW Commands (Future)
- Implement publish, import, export, diff
- Advanced tooling

---

**Related Documents:**
- [Architecture Overview](01_ARCHITECTURE_OVERVIEW.md)
- [Data Model](03_DATA_MODEL.md)
- [Integration Layer](04_INTEGRATION_LAYER.md)
