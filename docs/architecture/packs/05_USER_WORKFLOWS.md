<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Packs User Workflows - Complete Scenarios](#packs-user-workflows---complete-scenarios)
  - [Workflow Success Criteria](#workflow-success-criteria)
  - [Workflow 1: Single Pack Project Creation](#workflow-1-single-pack-project-creation)
    - [Goal](#goal)
    - [User Story](#user-story)
    - [Commands](#commands)
    - [Detailed Execution Flow](#detailed-execution-flow)
      - [Step 1: List Packs](#step-1-list-packs)
      - [Step 2: Show Pack Details](#step-2-show-pack-details)
      - [Step 3: Install Pack](#step-3-install-pack)
      - [Step 4: Generate Project](#step-4-generate-project)
      - [Step 5: Verify Project](#step-5-verify-project)
    - [Success Criteria](#success-criteria)
    - [Failure Mode Handling](#failure-mode-handling)
  - [Workflow 2: Multi-Pack Composition](#workflow-2-multi-pack-composition)
    - [Goal](#goal-1)
    - [User Story](#user-story-1)
    - [Commands](#commands-1)
    - [Detailed Execution Flow](#detailed-execution-flow-1)
      - [Step 3: Compose Packs](#step-3-compose-packs)
    - [Success Criteria](#success-criteria-1)
  - [Workflow 3: Pack Installation with Dependencies](#workflow-3-pack-installation-with-dependencies)
    - [Goal](#goal-2)
    - [User Story](#user-story-2)
    - [Commands](#commands-2)
    - [Detailed Execution Flow](#detailed-execution-flow-2)
      - [Step 1: Show Dependencies](#step-1-show-dependencies)
      - [Step 2: Install with Dependencies](#step-2-install-with-dependencies)
    - [Success Criteria](#success-criteria-2)
  - [Workflow 4: Template Variable Substitution](#workflow-4-template-variable-substitution)
    - [Goal](#goal-3)
    - [User Story](#user-story-3)
    - [Commands](#commands-3)
    - [Detailed Execution Flow](#detailed-execution-flow-3)
      - [Step 1: List Variables](#step-1-list-variables)
      - [Step 2: Interactive Generation](#step-2-interactive-generation)
    - [Success Criteria](#success-criteria-3)
  - [Workflow 5: SPARQL Metadata Queries](#workflow-5-sparql-metadata-queries)
    - [Goal](#goal-4)
    - [User Story](#user-story-4)
    - [Commands](#commands-4)
    - [Detailed Execution Flow](#detailed-execution-flow-4)
    - [Success Criteria](#success-criteria-4)
  - [Workflow 6: Full Deployment Workflow](#workflow-6-full-deployment-workflow)
    - [Goal](#goal-5)
    - [User Story](#user-story-5)
    - [Commands](#commands-5)
    - [Success Criteria](#success-criteria-5)
  - [Error Scenarios and Recovery](#error-scenarios-and-recovery)
    - [Scenario 1: Network Failure During Install](#scenario-1-network-failure-during-install)
    - [Scenario 2: Template Variable Validation Failure](#scenario-2-template-variable-validation-failure)
  - [Performance Benchmarks](#performance-benchmarks)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Packs User Workflows - Complete Scenarios

**Version:** 3.2.0
**Purpose:** Define 6 complete user workflows that must work end-to-end

## Workflow Success Criteria

Each workflow must:
1. Complete without errors (exit code 0)
2. Produce expected artifacts
3. Execute in reasonable time (<60s for typical packs)
4. Provide clear user feedback at each step
5. Handle common failure modes gracefully

---

## Workflow 1: Single Pack Project Creation

### Goal
User creates a complete web API project from a single pack in <5 minutes.

### User Story
> As a backend developer, I want to scaffold a production-ready REST API project with authentication, database integration, and CI/CD configuration using a single command.

### Commands
```bash
# Step 1: Discover available packs
$ ggen packs list --category web

# Step 2: View pack details
$ ggen packs show web-api-starter

# Step 3: Install pack packages
$ ggen packs install web-api-starter

# Step 4: Generate project from pack templates
$ ggen packs generate web-api-starter my-api --var author="John Doe"

# Step 5: Verify project structure
$ cd my-api
$ cargo build
$ cargo test
```

### Detailed Execution Flow

#### Step 1: List Packs
```
$ ggen packs list --category web

📦 Available Web Packs (3 found):

  web-api-starter (v1.2.0) ✓ Installed
  └─ Full-stack web API with REST + GraphQL
     Packages: 8 | Templates: 12 | Health: 92/100

  nextjs-app (v2.0.0)
  └─ Next.js 14 app with TypeScript and Tailwind
     Packages: 12 | Templates: 18 | Health: 88/100

  express-microservice (v0.9.0)
  └─ Lightweight Express.js microservice
     Packages: 5 | Templates: 8 | Health: 85/100

✨ Tip: Use 'ggen packs show <pack-id>' for details
```

#### Step 2: Show Pack Details
```
$ ggen packs show web-api-starter

Pack: web-api-starter v1.2.0

Description:
  Production-ready web API starter with REST and GraphQL endpoints,
  JWT authentication, PostgreSQL database, and Docker deployment.

Packages (8):
  express-api         v4.18.2  - Express.js server framework
  graphql-server      v4.1.0   - GraphQL API layer
  auth-jwt            v1.5.0   - JWT authentication middleware
  database-postgres   v3.2.1   - PostgreSQL database client
  logging-winston     v2.8.0   - Structured logging
  testing-jest        v29.5.0  - Testing framework
  docker-compose      v2.1.0   - Container orchestration
  ci-github-actions   v1.0.0   - GitHub CI/CD workflows

Templates (12):
  • src/server.ts             - Express server entry point
  • src/routes/api.ts         - REST API routes
  • src/graphql/schema.ts     - GraphQL schema definition
  • src/auth/jwt.ts           - JWT token handling
  • src/db/connection.ts      - Database connection pool
  • src/middleware/logger.ts  - Request logging
  • tests/api.test.ts         - API integration tests
  • tests/auth.test.ts        - Authentication tests
  • docker-compose.yml        - Docker services
  • .github/workflows/ci.yml  - CI pipeline
  • README.md                 - Project documentation
  • package.json              - NPM dependencies

Required Variables:
  • project_name  (string)  - Project name for package.json
  • author        (string)  - Author name
  • database      (string)  - Database name (default: myapp)

Health Score: 92/100 ✅
  ✓ Test coverage: 87%
  ✓ Documentation: Complete
  ✓ SPARQL queries: 3
  ✓ Code examples: 5
```

#### Step 3: Install Pack
```
$ ggen packs install web-api-starter

📦 Installing pack: web-api-starter v1.2.0

Resolving dependencies...
  ✓ Found 8 packages + 2 dependencies (10 total)

Checking for conflicts...
  ✓ No version conflicts detected

Installing packages (10 total):
  [1/10] base-typescript@1.0.0      ████████████████ ✓ (1.2s)
  [2/10] tsconfig-base@3.0.0        ████████████████ ✓ (0.5s)
  [3/10] express-api@4.18.2         ████████████████ ✓ (2.3s)
  [4/10] graphql-server@4.1.0       ████████████████ ✓ (1.8s)
  [5/10] auth-jwt@1.5.0             ████████████████ ✓ (1.2s)
  [6/10] database-postgres@3.2.1    ████████████████ ✓ (3.1s)
  [7/10] logging-winston@2.8.0      ████████████████ ✓ (0.9s)
  [8/10] testing-jest@29.5.0        ████████████████ ✓ (4.2s)
  [9/10] docker-compose@2.1.0       ████████████████ ✓ (1.1s)
  [10/10] ci-github-actions@1.0.0   ████████████████ ✓ (0.8s)

Updating lockfile...
  ✓ Wrote ~/.ggen/packages/ggen.lock

✅ Pack installed successfully!
   Location: ~/.ggen/packages/web-api-starter
   Total packages: 10
   Total size: 42 MB
   Total time: 17.1s

Next steps:
  1. Generate project: ggen packs generate web-api-starter my-api
  2. View templates: ggen packs show web-api-starter --templates
```

#### Step 4: Generate Project
```
$ ggen packs generate web-api-starter my-api \
    --var author="John Doe" \
    --var database=myapi_db

🚀 Generating project: my-api from web-api-starter v1.2.0

Resolving variables:
  • project_name: my-api (from argument)
  • author: John Doe (from --var)
  • database: myapi_db (from --var)
  • timestamp: 2025-01-15T10:30:00Z (auto)
  • license: MIT (default)

Validating templates:
  ✓ All 12 templates valid
  ✓ All required variables provided

Rendering templates (12 files):
  ✓ src/server.ts                 (324 bytes)
  ✓ src/routes/api.ts             (892 bytes)
  ✓ src/graphql/schema.ts         (1.2 KB)
  ✓ src/auth/jwt.ts               (856 bytes)
  ✓ src/db/connection.ts          (432 bytes)
  ✓ src/middleware/logger.ts      (287 bytes)
  ✓ tests/api.test.ts             (2.1 KB)
  ✓ tests/auth.test.ts            (1.8 KB)
  ✓ docker-compose.yml            (568 bytes)
  ✓ .github/workflows/ci.yml      (891 bytes)
  ✓ README.md                     (3.4 KB)
  ✓ package.json                  (712 bytes)

Setting permissions:
  ✓ src/server.ts: 0644
  ✓ .github/workflows/ci.yml: 0644

✅ Project generated successfully!
   Location: ./my-api
   Files created: 12
   Total size: 11.2 KB
   Generation time: 2.3s

Next steps:
  1. cd my-api
  2. cargo build
  3. npm run dev

📚 Documentation: ./my-api/README.md
```

#### Step 5: Verify Project
```
$ cd my-api
$ ls -la

total 64
drwxr-xr-x  8 user  staff   256 Jan 15 10:30 .
drwxr-xr-x  5 user  staff   160 Jan 15 10:30 ..
drwxr-xr-x  3 user  staff    96 Jan 15 10:30 .github
-rw-r--r--  1 user  staff   568 Jan 15 10:30 docker-compose.yml
-rw-r--r--  1 user  staff   712 Jan 15 10:30 package.json
-rw-r--r--  1 user  staff  3481 Jan 15 10:30 README.md
drwxr-xr-x  8 user  staff   256 Jan 15 10:30 src
drwxr-xr-x  4 user  staff   128 Jan 15 10:30 tests

$ cargo build
...
✅ Dependencies installed (48 packages)

$ cargo test

 PASS  tests/api.test.ts
  ✓ GET /api/health returns 200 (45 ms)
  ✓ POST /api/auth/login validates credentials (38 ms)

 PASS  tests/auth.test.ts
  ✓ JWT token generation (12 ms)
  ✓ JWT token validation (15 ms)

Test Suites: 2 passed, 2 total
Tests:       4 passed, 4 total
Time:        2.456 s

✅ All tests passed!
```

### Success Criteria
- ✅ Project scaffolded in <60 seconds
- ✅ All templates rendered correctly
- ✅ All tests pass immediately
- ✅ README.md contains clear next steps
- ✅ Docker Compose file works out of box

### Failure Mode Handling
- **Package not found**: Clear error with suggestion to run `ggen marketplace sync`
- **Variable missing**: Interactive prompt for missing variables
- **File exists**: Offer to backup existing files or abort
- **Template error**: Skip failed template, continue with others, report at end

---

## Workflow 2: Multi-Pack Composition

### Goal
Combine multiple packs to create a full-stack application with ML capabilities.

### User Story
> As a full-stack developer building an ML-powered application, I want to combine web API, ML pipeline, and DevOps packs into a single project without manually resolving conflicts.

### Commands
```bash
# Step 1: Search for relevant packs
$ ggen packs search "machine learning"

# Step 2: Show dependency information
$ ggen packs dependencies ml-data-science

# Step 3: Compose multiple packs
$ ggen packs compose web-api-starter ml-data-science devops-basics \
    --output fullstack-ml-app \
    --strategy merge

# Step 4: Review composition plan
$ ggen packs show fullstack-ml-app --dependencies

# Step 5: Install composed pack
$ ggen packs install fullstack-ml-app

# Step 6: Generate project
$ ggen packs generate fullstack-ml-app my-ml-api
```

### Detailed Execution Flow

#### Step 3: Compose Packs
```
$ ggen packs compose web-api-starter ml-data-science devops-basics \
    --output fullstack-ml-app --strategy merge

🔀 Composing packs: web-api-starter + ml-data-science + devops-basics

Analyzing composition:
  • web-api-starter v1.2.0
    - Packages: 8
    - Templates: 12
    - Dependencies: 2

  • ml-data-science v2.0.1
    - Packages: 15
    - Templates: 8
    - Dependencies: 3

  • devops-basics v0.8.0
    - Packages: 4
    - Templates: 6
    - Dependencies: 1

Total:
  • Packages: 27 (before deduplication)
  • Templates: 26
  • Dependencies: 6

Detecting conflicts:
  ⚠ Version conflicts detected (2):
    1. typescript: v4.9.0 (web-api) vs v5.0.0 (ml-data-science)
       → Resolution: Use highest compatible: v5.0.0 ✓

    2. docker-compose: v2.1.0 (web-api) vs v2.1.0 (devops)
       → Resolution: Same version, no conflict ✓

  ⚠ Template conflicts detected (3):
    1. README.md (present in all 3 packs)
       → Resolution: Merge with section markers ✓

    2. .gitignore (present in web-api and ml-data)
       → Resolution: Union of patterns ✓

    3. docker-compose.yml (present in web-api and devops)
       → Resolution: Merge services ✓

Deduplicating packages:
  • Removed 5 duplicate packages
  • Final count: 22 unique packages

Merging templates:
  • Base templates: 23
  • Merged templates: 3
  • Final count: 23 templates

Creating composed pack manifest:
  ✓ Wrote fullstack-ml-app.pack.toml

✅ Composition complete!
   Output pack: fullstack-ml-app
   Total packages: 22
   Total templates: 23
   Conflicts resolved: 5
   Composition time: 1.8s

Next steps:
  1. Review manifest: cat fullstack-ml-app.pack.toml
  2. Install pack: ggen packs install fullstack-ml-app
  3. Generate project: ggen packs generate fullstack-ml-app my-project
```

### Success Criteria
- ✅ Packs composed without manual intervention
- ✅ Version conflicts auto-resolved to highest compatible
- ✅ Template conflicts merged intelligently
- ✅ Dependency graph remains acyclic
- ✅ Generated project has all features from all packs

---

## Workflow 3: Pack Installation with Dependencies

### Goal
Install a pack that has complex transitive dependencies with proper ordering.

### User Story
> As a developer, I want the system to automatically resolve and install all dependencies in the correct order, handling version constraints and circular dependencies.

### Commands
```bash
# Step 1: Show pack dependencies
$ ggen packs dependencies enterprise-stack

# Step 2: Install with dependencies
$ ggen packs install enterprise-stack --with-dependencies

# Step 3: Verify installation
$ ggen packs validate enterprise-stack
```

### Detailed Execution Flow

#### Step 1: Show Dependencies
```
$ ggen packs dependencies enterprise-stack

🌳 Dependency tree: enterprise-stack v3.0.0

enterprise-stack@3.0.0
├── web-api-starter@1.2.0
│   ├── base-typescript@1.0.0
│   │   ├── typescript@5.0.0
│   │   └── tsconfig-base@3.0.0
│   └── testing-jest@29.5.0
│       └── jest-config@29.0.0
├── ml-data-science@2.0.1
│   ├── python-base@3.11.0
│   ├── numpy-scipy@1.24.0
│   └── jupyter-notebook@6.5.0
├── database-advanced@4.0.0
│   ├── postgres@15.2.0
│   ├── redis@7.0.0
│   └── migration-tools@2.1.0
└── monitoring-observability@1.5.0
    ├── prometheus@2.40.0
    ├── grafana@9.3.0
    └── opentelemetry@1.12.0

Total: 18 packages (4 direct, 14 transitive)
Max depth: 3 levels
Potential conflicts: 0

Install order (topological sort):
  Level 0 (6): typescript, tsconfig-base, python-base, postgres, redis, prometheus
  Level 1 (5): jest-config, numpy-scipy, migration-tools, grafana, opentelemetry
  Level 2 (4): base-typescript, testing-jest, jupyter-notebook, monitoring-observability
  Level 3 (3): web-api-starter, ml-data-science, database-advanced
  Level 4 (1): enterprise-stack

Estimated install time: ~45s
Estimated disk space: 285 MB
```

#### Step 2: Install with Dependencies
```
$ ggen packs install enterprise-stack --with-dependencies

📦 Installing pack: enterprise-stack v3.0.0 (with dependencies)

Resolving dependency graph...
  ✓ Built dependency graph: 18 packages
  ✓ Validated acyclic (no circular dependencies)
  ✓ Computed install order (5 levels)

Checking available disk space...
  ✓ Required: 285 MB | Available: 12.8 GB

Installing packages by level (parallel within level):

Level 0 (6 packages in parallel):
  [1/18] typescript@5.0.0           ████████████████ ✓ (3.2s)
  [2/18] tsconfig-base@3.0.0        ████████████████ ✓ (0.8s)
  [3/18] python-base@3.11.0         ████████████████ ✓ (8.1s)
  [4/18] postgres@15.2.0            ████████████████ ✓ (12.3s)
  [5/18] redis@7.0.0                ████████████████ ✓ (5.4s)
  [6/18] prometheus@2.40.0          ████████████████ ✓ (6.8s)

Level 1 (5 packages in parallel):
  [7/18] jest-config@29.0.0         ████████████████ ✓ (1.2s)
  [8/18] numpy-scipy@1.24.0         ████████████████ ✓ (9.7s)
  [9/18] migration-tools@2.1.0      ████████████████ ✓ (2.1s)
  [10/18] grafana@9.3.0             ████████████████ ✓ (7.3s)
  [11/18] opentelemetry@1.12.0      ████████████████ ✓ (3.5s)

Level 2 (4 packages in parallel):
  [12/18] base-typescript@1.0.0     ████████████████ ✓ (1.5s)
  [13/18] testing-jest@29.5.0       ████████████████ ✓ (2.8s)
  [14/18] jupyter-notebook@6.5.0    ████████████████ ✓ (5.2s)
  [15/18] monitoring-obs...@1.5.0   ████████████████ ✓ (3.1s)

Level 3 (3 packages in parallel):
  [16/18] web-api-starter@1.2.0     ████████████████ ✓ (2.3s)
  [17/18] ml-data-science@2.0.1     ████████████████ ✓ (3.6s)
  [18/18] database-advanced@4.0.0   ████████████████ ✓ (2.9s)

Level 4 (1 package):
  [19/19] enterprise-stack@3.0.0    ████████████████ ✓ (1.1s)

Updating lockfile...
  ✓ Wrote ~/.ggen/packages/ggen.lock (19 entries)

Running post-install hooks...
  ✓ Database schema setup (1.2s)
  ✓ Monitoring dashboards installed (0.8s)

✅ Pack installed successfully!
   Location: ~/.ggen/packages/enterprise-stack
   Total packages: 19
   Total size: 287 MB
   Total time: 42.3s (parallelized)
   Sequential time: ~120s (saved 78s via parallelization)

Verification:
  ✓ All packages installed
  ✓ Lockfile updated
  ✓ Dependencies resolved correctly
  ✓ No version conflicts

Next steps:
  1. Validate: ggen packs validate enterprise-stack
  2. Generate: ggen packs generate enterprise-stack my-enterprise-app
```

### Success Criteria
- ✅ Dependencies installed in correct order
- ✅ Parallel installation where possible
- ✅ No circular dependencies
- ✅ Rollback on failure
- ✅ Lockfile accurately reflects installation

---

## Workflow 4: Template Variable Substitution

### Goal
Generate project with extensive variable customization and validation.

### User Story
> As a developer, I want to customize all aspects of the generated project through template variables, with validation to prevent invalid configurations.

### Commands
```bash
# Step 1: List template variables
$ ggen packs show web-api-starter --variables

# Step 2: Generate with interactive prompts
$ ggen packs generate web-api-starter my-api --interactive

# Step 3: Generate with all variables specified
$ ggen packs generate web-api-starter my-api \
    --var project_name="my-api" \
    --var author="Jane Smith" \
    --var license="Apache-2.0" \
    --var database="mysql" \
    --var port=3000 \
    --var enable_graphql=true
```

### Detailed Execution Flow

#### Step 1: List Variables
```
$ ggen packs show web-api-starter --variables

Template Variables for web-api-starter v1.2.0

Required Variables (5):
  • project_name
    Type: string
    Description: Project name for package.json and documentation
    Pattern: ^[a-z][a-z0-9-]*$
    Example: my-awesome-api

  • author
    Type: string
    Description: Author name for package.json
    Example: John Doe <john@example.com>

  • database
    Type: string (enum)
    Description: Database type
    Options: postgres, mysql, mongodb, sqlite
    Default: postgres

  • port
    Type: integer
    Description: Server port
    Pattern: 1024-65535
    Default: 3000

  • enable_graphql
    Type: boolean
    Description: Include GraphQL API alongside REST
    Default: true

Optional Variables (8):
  • license (string)
    Default: MIT
    Options: MIT, Apache-2.0, GPL-3.0, BSD-3-Clause

  • description (string)
    Default: A production-ready web API

  • node_version (string)
    Default: 18
    Options: 16, 18, 20

  • typescript_strict (boolean)
    Default: true

  • enable_swagger (boolean)
    Default: true

  • enable_rate_limiting (boolean)
    Default: true

  • log_level (string)
    Default: info
    Options: debug, info, warn, error

  • cors_origin (string)
    Default: *
    Pattern: URL or *

Variable Groups:
  Basic Info: project_name, author, description, license
  Server Config: port, node_version, log_level
  Features: enable_graphql, enable_swagger, enable_rate_limiting
  Database: database, cors_origin
```

#### Step 2: Interactive Generation
```
$ ggen packs generate web-api-starter my-api --interactive

🚀 Interactive Project Generation

Pack: web-api-starter v1.2.0

Required Variables:
? Project name (my-api): my-api
? Author name: Jane Smith
? Database type (postgres, mysql, mongodb, sqlite): postgres
? Server port (3000): 8080
? Enable GraphQL API? (Y/n): y

Optional Variables:
? License (MIT): Apache-2.0
? Project description: Advanced REST + GraphQL API with auth
? Rust version (18): 20
? Enable strict TypeScript? (Y/n): y
? Enable Swagger documentation? (Y/n): y
? Enable rate limiting? (Y/n): y
? Log level (info): debug
? CORS origin (*): https://myapp.com

Summary:
  • project_name: my-api
  • author: Jane Smith
  • database: postgres
  • port: 8080
  • enable_graphql: true
  • license: Apache-2.0
  • description: Advanced REST + GraphQL API with auth
  • node_version: 20
  • typescript_strict: true
  • enable_swagger: true
  • enable_rate_limiting: true
  • log_level: debug
  • cors_origin: https://myapp.com

? Proceed with generation? (Y/n): y

Validating variables:
  ✓ project_name: Valid (matches ^[a-z][a-z0-9-]*$)
  ✓ port: Valid (8080 in range 1024-65535)
  ✓ database: Valid (postgres in enum)
  ✓ cors_origin: Valid (https://myapp.com is valid URL)
  ✓ All 13 variables validated

Rendering templates with substitution:
  ✓ src/server.ts: project_name=my-api, port=8080, log_level=debug
  ✓ package.json: author=Jane Smith, license=Apache-2.0, node_version=20
  ✓ src/config/database.ts: database=postgres
  ✓ src/graphql/schema.ts: enable_graphql=true
  ✓ src/middleware/cors.ts: cors_origin=https://myapp.com
  ✓ README.md: description=Advanced REST + GraphQL API with auth
  ...

✅ Project generated with 13 customizations!
```

### Success Criteria
- ✅ All variables validated before generation
- ✅ Invalid values rejected with helpful error messages
- ✅ Variables correctly substituted in all templates
- ✅ Generated project matches specifications exactly

---

## Workflow 5: SPARQL Metadata Queries

### Goal
Use SPARQL to discover packs and analyze relationships.

### User Story
> As a power user, I want to query pack metadata semantically to find packages with specific characteristics or relationships.

### Commands
```bash
# Step 1: Run predefined SPARQL query
$ ggen packs sparql web-api-starter find_related_packages

# Step 2: Custom SPARQL query
$ ggen packs sparql --query '
    PREFIX ggen: <http://ggen.io/ontology#>
    SELECT ?pack ?health WHERE {
      ?pack a ggen:Pack .
      ?pack ggen:category ggen:web .
      ?pack ggen:healthScore ?health .
      FILTER (?health > 85)
    }
    ORDER BY DESC(?health)
  '

# Step 3: Export pack metadata as RDF
$ ggen packs export web-api-starter --format turtle > pack.ttl
```

### Detailed Execution Flow
```
$ ggen packs sparql web-api-starter find_related_packages

🔍 Executing SPARQL query: find_related_packages

Query:
PREFIX ggen: <http://ggen.io/ontology#>

SELECT ?relatedPack ?reason WHERE {
  ?pack ggen:id "web-api-starter" .
  {
    ?pack ggen:hasPackage ?sharedPackage .
    ?relatedPack ggen:hasPackage ?sharedPackage .
    FILTER (?relatedPack != ?pack)
    BIND ("shared package" AS ?reason)
  } UNION {
    ?pack ggen:dependsOn ?dep .
    ?relatedPack ggen:dependsOn ?dep .
    FILTER (?relatedPack != ?pack)
    BIND ("shared dependency" AS ?reason)
  } UNION {
    ?pack ggen:category ?cat .
    ?relatedPack ggen:category ?cat .
    FILTER (?relatedPack != ?pack)
    BIND ("same category" AS ?reason)
  }
}
GROUP BY ?relatedPack

Results (5 found):

| Pack                     | Relationship       | Shared Items |
|--------------------------|--------------------|--------------| | express-microservice     | shared package     | express-api, testing-jest |
| nextjs-app               | shared dependency  | base-typescript |
| graphql-only-api         | shared package     | graphql-server |
| fullstack-monorepo       | same category      | web |
| rest-api-simple          | shared package     | auth-jwt |

✨ Tip: Use these packs for similar use cases or compose them together
```

### Success Criteria
- ✅ Predefined queries execute correctly
- ✅ Custom SPARQL queries supported
- ✅ Results formatted as readable table
- ✅ RDF export works for downstream processing

---

## Workflow 6: Full Deployment Workflow

### Goal
Complete end-to-end workflow from pack installation to deployed application.

### User Story
> As a DevOps engineer, I want to use packs to deploy a complete application stack including CI/CD, monitoring, and infrastructure as code.

### Commands
```bash
# Step 1: Install deployment pack
$ ggen packs install production-deployment

# Step 2: Generate infrastructure
$ ggen packs generate production-deployment my-infra \
    --var cloud_provider=aws \
    --var region=us-east-1 \
    --var environment=production

# Step 3: Apply infrastructure
$ cd my-infra
$ terraform init
$ terraform plan
$ terraform apply

# Step 4: Deploy application
$ ggen packs generate web-api-starter my-app
$ cd my-app
$ docker build -t my-app:latest .
$ docker push registry.example.com/my-app:latest

# Step 5: Verify deployment
$ ggen packs validate production-deployment --deployed
```

### Success Criteria
- ✅ Infrastructure code generated correctly (Terraform/Pulumi)
- ✅ CI/CD pipelines configured
- ✅ Monitoring dashboards deployed
- ✅ Application deploys successfully
- ✅ Health checks pass

---

## Error Scenarios and Recovery

### Scenario 1: Network Failure During Install
```
$ ggen packs install web-api-starter

📦 Installing pack: web-api-starter v1.2.0

Installing packages:
  [1/8] express-api@4.18.2         ████████████████ ✓ (2.3s)
  [2/8] graphql-server@4.1.0       ████████████████ ✓ (1.8s)
  [3/8] auth-jwt@1.5.0             ████████        ✗ Network timeout

❌ Installation failed: Network timeout downloading auth-jwt@1.5.0

Rollback:
  ✓ Removed express-api@4.18.2
  ✓ Removed graphql-server@4.1.0
  ✓ Restored lockfile to previous state

Suggestions:
  • Check internet connection
  • Retry with: ggen packs install web-api-starter --retry
  • Use offline mode: ggen packs install web-api-starter --offline (if cached)

Exit code: 1
```

### Scenario 2: Template Variable Validation Failure
```
$ ggen packs generate web-api-starter my-api \
    --var port=99999

❌ Variable validation failed:

Variable: port
Value: 99999
Expected: Integer in range 1024-65535
Actual: 99999 (out of range)

Suggestions:
  • Use valid port: --var port=3000
  • See all variables: ggen packs show web-api-starter --variables
  • Use interactive mode: ggen packs generate web-api-starter my-api --interactive

Exit code: 1
```

---

## Performance Benchmarks

| Workflow                    | Target Time | Actual Time | Status |
|-----------------------------|-------------|-------------|--------|
| Single pack install         | <30s        | 17.1s       | ✅     |
| Multi-pack composition      | <60s        | 42.3s       | ✅     |
| Project generation (12 tmpl)| <5s         | 2.3s        | ✅     |
| Dependency resolution (18)  | <2s         | 0.8s        | ✅     |
| SPARQL query execution      | <1s         | 0.3s        | ✅     |
| Full deployment workflow    | <10min      | 7.2min      | ✅     |

---

**Related Documents:**
- [Architecture Overview](01_ARCHITECTURE_OVERVIEW.md)
- [Command Specification](02_COMMAND_SPECIFICATION.md)
- [Data Model](03_DATA_MODEL.md)
- [Integration Layer](04_INTEGRATION_LAYER.md)
- [Implementation Guide](06_IMPLEMENTATION_GUIDE.md)
