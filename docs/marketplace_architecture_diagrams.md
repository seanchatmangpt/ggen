# ggen Marketplace Architecture Diagrams

**Version**: 1.0
**Date**: 2025-01-16
**Purpose**: Visual representation of marketplace system architecture

---

## System Context Diagram (C4 Level 1)

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                                                                             │
│                           External Systems                                  │
│                                                                             │
│  ┌──────────────┐      ┌──────────────┐       ┌──────────────┐            │
│  │   GitHub     │      │ Package CDN  │       │   NPM/Cargo  │            │
│  │   Registry   │      │   (Future)   │       │   Registries │            │
│  └──────┬───────┘      └──────┬───────┘       └──────┬───────┘            │
│         │                     │                       │                     │
│         │                     │                       │                     │
└─────────┼─────────────────────┼───────────────────────┼─────────────────────┘
          │                     │                       │
          │  metadata sync      │  package download     │  dependency check
          │                     │                       │
          ▼                     ▼                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                                                                             │
│                      ggen Marketplace System                                │
│                                                                             │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │                       CLI Interface                                │    │
│  │  ggen marketplace search | install | validate | maturity | ...    │    │
│  └────────────────────┬───────────────────────────────────────────────┘    │
│                       │                                                     │
│                       ▼                                                     │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │                    Domain Layer                                    │    │
│  │  Search | Install | Validate | Maturity | Recommend | Compare     │    │
│  └────────────────────┬───────────────────────────────────────────────┘    │
│                       │                                                     │
│                       ▼                                                     │
│  ┌────────────────────────────────────────────────────────────────────┐    │
│  │                Infrastructure Layer                                │    │
│  │  Registry | Storage | Search Engine | Crypto | Maturity Evaluator │    │
│  └────────────────────────────────────────────────────────────────────┘    │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
          │                     │                       │
          │  read/write         │  read/write           │  read
          │                     │                       │
          ▼                     ▼                       ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                                                                             │
│                          File System                                        │
│                                                                             │
│  ┌──────────────┐      ┌──────────────┐       ┌──────────────┐            │
│  │  ~/.ggen/    │      │ marketplace/ │       │  templates/  │            │
│  │  registry/   │      │  packages/   │       │              │            │
│  │  index.json  │      │  ...         │       │  ...         │            │
│  └──────────────┘      └──────────────┘       └──────────────┘            │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘

Legend:
  User       → CLI commands
  CLI        → Domain logic (business rules)
  Domain     → Infrastructure (technical implementations)
  Infra      → External storage (filesystem, network)
```

---

## Component Diagram (C4 Level 2)

### CLI Layer

```
┌────────────────────────────────────────────────────────────────┐
│                     CLI Component                              │
│  (crates/ggen-cli/src/cmds/marketplace.rs)                     │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐        │
│  │   #[verb]    │  │   #[verb]    │  │   #[verb]    │        │
│  │   search     │  │   install    │  │   validate   │        │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘        │
│         │                 │                  │                 │
│         ▼                 ▼                  ▼                 │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐        │
│  │SearchInput   │  │InstallInput  │  │ValidateInput │        │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘        │
│         │                 │                  │                 │
│         └─────────────────┴──────────────────┘                 │
│                           │                                    │
│                           ▼                                    │
│               execute_async_verb()                             │
│                           │                                    │
└───────────────────────────┼────────────────────────────────────┘
                            │
                            ▼ (calls domain functions)
                  Domain Layer Functions
```

### Domain Layer

```
┌────────────────────────────────────────────────────────────────┐
│                    Domain Layer                                │
│  (crates/ggen-domain/src/marketplace/)                         │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  ┌────────────────────────────────────────────────────┐       │
│  │         Core Business Functions                     │       │
│  │                                                      │       │
│  │  execute_search(SearchInput) → Vec<SearchResult>   │       │
│  │  execute_install(InstallInput) → InstallResult     │       │
│  │  execute_list(ListInput) → ListOutput              │       │
│  │  validate_package(Path) → PackageValidation        │       │
│  │  execute_publish(PublishInput) → PublishOutput     │       │
│  └────────────┬───────────────────────────────────────┘       │
│               │                                                │
│               ▼                                                │
│  ┌────────────────────────────────────────────────────┐       │
│  │         Supporting Modules                          │       │
│  │                                                      │       │
│  │  recommender.rs     → Recommendation engine        │       │
│  │  bundles.rs         → Sector bundle management     │       │
│  │  guards.rs          → Quality guard checks         │       │
│  │  quality_autopilot.rs → Improvement suggestions    │       │
│  │  production_readiness.rs → Production gates        │       │
│  │  types.rs           → Validated types (poka-yoke)  │       │
│  └────────────┬───────────────────────────────────────┘       │
│               │                                                │
└───────────────┼────────────────────────────────────────────────┘
                │
                ▼ (uses infrastructure)
       Infrastructure Traits & Implementations
```

### Infrastructure Layer

```
┌────────────────────────────────────────────────────────────────┐
│                Infrastructure Layer                            │
│  (crates/ggen-marketplace/src/)                                │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  ┌────────────────────────────────────────────────────┐       │
│  │             Core Traits (Abstractions)              │       │
│  │                                                      │       │
│  │  trait Registry                                     │       │
│  │  trait PackageStore                                 │       │
│  │  trait SearchEngine                                 │       │
│  │  trait CryptoVerifier                               │       │
│  └────────────┬───────────────────────────────────────┘       │
│               │                                                │
│               ▼                                                │
│  ┌────────────────────────────────────────────────────┐       │
│  │          Concrete Implementations                   │       │
│  │                                                      │       │
│  │  LocalRegistry          (registry.rs)              │       │
│  │  FilesystemStore        (storage/filesystem.rs)    │       │
│  │  MemoryStore            (storage/memory.rs)        │       │
│  │  TantivySearchEngine    (search/tantivy.rs)        │       │
│  │  Ed25519Verifier        (crypto/ed25519.rs)        │       │
│  │  MaturityEvaluator      (maturity_evaluator.rs)    │       │
│  └────────────┬───────────────────────────────────────┘       │
│               │                                                │
└───────────────┼────────────────────────────────────────────────┘
                │
                ▼ (reads/writes)
            File System / Network
```

---

## Data Flow Diagrams

### Search Command Flow

```
┌─────────┐
│  User   │
└────┬────┘
     │ ggen marketplace search --query "rust api" --min-maturity production
     ▼
┌─────────────────────────────────────────────────────────┐
│  CLI: marketplace.rs::search()                          │
│  1. Parse args → SearchInput                            │
│  2. Validate query (NonEmptyQuery type)                 │
└────┬────────────────────────────────────────────────────┘
     │ SearchInput { query, min_maturity, ... }
     ▼
┌─────────────────────────────────────────────────────────┐
│  Domain: search.rs::execute_search()                    │
│  1. Load Registry from infra                            │
│  2. Filter packages by query + maturity                 │
│  3. Enrich with maturity scores                         │
│  4. Return Vec<SearchResult>                            │
└────┬────────────────────────────────────────────────────┘
     │ calls Registry trait
     ▼
┌─────────────────────────────────────────────────────────┐
│  Infrastructure: registry.rs::LocalRegistry             │
│  1. Load ~/.ggen/registry/index.json                    │
│  2. Check LRU cache for metadata                        │
│  3. Query TantivySearchEngine                           │
│  4. Return Package objects                              │
└────┬────────────────────────────────────────────────────┘
     │ returns packages
     ▼
┌─────────────────────────────────────────────────────────┐
│  Domain: Enrich results with MaturityEvaluator          │
│  1. For each package, calculate maturity score          │
│  2. Create SearchResult with score + level              │
│  3. Sort by relevance/score                             │
└────┬────────────────────────────────────────────────────┘
     │ Vec<SearchResult>
     ▼
┌─────────────────────────────────────────────────────────┐
│  CLI: Format output as JSON or human-readable           │
│  {                                                      │
│    "packages": [...],                                   │
│    "total": 5                                           │
│  }                                                      │
└────┬────────────────────────────────────────────────────┘
     │
     ▼
┌─────────┐
│  Output │
└─────────┘
```

### Install Command Flow

```
┌─────────┐
│  User   │
└────┬────┘
     │ ggen marketplace install microservice-template
     ▼
┌─────────────────────────────────────────────────────────┐
│  CLI: marketplace.rs::install()                         │
│  1. Parse args → InstallInput                           │
└────┬────────────────────────────────────────────────────┘
     │ InstallInput { package, target, force, ... }
     ▼
┌─────────────────────────────────────────────────────────┐
│  Domain: install.rs::execute_install()                  │
│  1. Validate package name                               │
│  2. Load Registry, resolve latest version               │
│  3. Check if already installed                          │
│  4. Validate maturity score >= threshold                │
└────┬────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────┐
│  Infrastructure: Download package                       │
│  1. GET package from download_url                       │
│  2. Verify SHA-256 checksum                             │
│  3. Extract to ~/.ggen/packages/<name>/                 │
└────┬────────────────────────────────────────────────────┘
     │
     ▼
┌─────────────────────────────────────────────────────────┐
│  Domain: Resolve dependencies (recursive)               │
│  1. Parse dependencies from package.toml                │
│  2. For each dependency:                                │
│     - Check if installed                                │
│     - If not, install recursively                       │
└────┬────────────────────────────────────────────────────┘
     │ InstallResult
     ▼
┌─────────────────────────────────────────────────────────┐
│  CLI: Format output                                     │
│  {                                                      │
│    "package": "microservice-template",                  │
│    "version": "2.1.0",                                  │
│    "path": "~/.ggen/packages/microservice-template",    │
│    "dependencies": []                                   │
│  }                                                      │
└────┬────────────────────────────────────────────────────┘
     │
     ▼
┌─────────┐
│  Output │
└─────────┘
```

### Maturity Assessment Flow

```
┌─────────┐
│  User   │
└────┬────┘
     │ ggen marketplace maturity agent-editor
     ▼
┌─────────────────────────────────────────────────────────┐
│  CLI: marketplace.rs::maturity()                        │
│  1. Parse package_id                                    │
│  2. Create EvaluationInput from metadata                │
└────┬────────────────────────────────────────────────────┘
     │ EvaluationInput { package_id, has_readme, test_coverage, ... }
     ▼
┌─────────────────────────────────────────────────────────┐
│  Infrastructure: maturity_evaluator.rs                  │
│  MaturityEvaluator::evaluate()                          │
│                                                         │
│  ┌─────────────────────────────────────────────┐       │
│  │  1. evaluate_documentation()                │       │
│  │     - has_readme → 5 pts                    │       │
│  │     - has_api_docs → 5 pts                  │       │
│  │     - has_examples → 5 pts                  │       │
│  │     - has_changelog → 5 pts                 │       │
│  │     Total: 0-20 points                      │       │
│  └─────────────────────────────────────────────┘       │
│                                                         │
│  ┌─────────────────────────────────────────────┐       │
│  │  2. evaluate_testing()                      │       │
│  │     - unit_tests (coverage %) → 0-8 pts     │       │
│  │     - integration_tests → 0-6 pts           │       │
│  │     - e2e_tests → 0-4 pts                   │       │
│  │     Total: 0-20 points                      │       │
│  └─────────────────────────────────────────────┘       │
│                                                         │
│  ┌─────────────────────────────────────────────┐       │
│  │  3. evaluate_security()                     │       │
│  │     - vulnerabilities → 0-10 pts            │       │
│  │     - dependency_audit → 0-5 pts            │       │
│  │     - safe_code (no unsafe) → 0-5 pts       │       │
│  │     Total: 0-20 points                      │       │
│  └─────────────────────────────────────────────┘       │
│                                                         │
│  ┌─────────────────────────────────────────────┐       │
│  │  4. evaluate_performance()                  │       │
│  │     - benchmarks → 0-8 pts                  │       │
│  │     - optimization → 0-4 pts                │       │
│  │     - determinism → 0-3 pts                 │       │
│  │     Total: 0-15 points                      │       │
│  └─────────────────────────────────────────────┘       │
│                                                         │
│  ┌─────────────────────────────────────────────┐       │
│  │  5. evaluate_adoption()                     │       │
│  │     - downloads → 0-6 pts                   │       │
│  │     - citations → 0-5 pts                   │       │
│  │     - community → 0-4 pts                   │       │
│  │     Total: 0-15 points                      │       │
│  └─────────────────────────────────────────────┘       │
│                                                         │
│  ┌─────────────────────────────────────────────┐       │
│  │  6. evaluate_maintenance()                  │       │
│  │     - release_cadence → 0-5 pts             │       │
│  │     - responsiveness → 0-3 pts              │       │
│  │     - active_maintenance → 0-2 pts          │       │
│  │     Total: 0-10 points                      │       │
│  └─────────────────────────────────────────────┘       │
│                                                         │
│  ═══════════════════════════════════════════════       │
│  Total Score: Sum of all dimensions (0-100)            │
│  Maturity Level: from_score(total)                     │
│  ═══════════════════════════════════════════════       │
└────┬────────────────────────────────────────────────────┘
     │ MaturityAssessment
     ▼
┌─────────────────────────────────────────────────────────┐
│  CLI: Format output                                     │
│  {                                                      │
│    "package_id": "agent-editor",                        │
│    "total_score": 92,                                   │
│    "maturity_level": "enterprise",                      │
│    "scores": {                                          │
│      "documentation": 20,                               │
│      "testing": 18,                                     │
│      "security": 19,                                    │
│      "performance": 14,                                 │
│      "adoption": 12,                                    │
│      "maintenance": 9                                   │
│    },                                                   │
│    "feedback": [],                                      │
│    "next_steps": [...]                                  │
│  }                                                      │
└────┬────────────────────────────────────────────────────┘
     │
     ▼
┌─────────┐
│  Output │
└─────────┘
```

---

## State Diagrams

### Package Lifecycle States

```
┌─────────────┐
│  Discovered │  (Package exists in remote registry)
└──────┬──────┘
       │ sync registry
       ▼
┌─────────────┐
│   Indexed   │  (Package metadata in local index.json)
└──────┬──────┘
       │ install command
       ▼
┌─────────────┐
│ Installing  │  (Downloading + extracting)
└──────┬──────┘
       │ extraction complete
       ▼
┌─────────────┐
│  Installed  │  (Package in ~/.ggen/packages/)
└──────┬──────┘
       │ validate command
       ▼
┌─────────────┐
│  Validated  │  (Quality checks passed)
└──────┬──────┘
       │ production_ready = true
       ▼
┌─────────────┐
│  Production │  (Score >= 80, all guards pass)
│    Ready    │
└─────────────┘

Transitions:
  Discovered → Indexed:     sync command
  Indexed → Installing:     install command
  Installing → Installed:   extraction success
  Installed → Validated:    validate command
  Validated → Production:   score >= 80 + guards pass
```

### Registry Sync State

```
┌─────────────┐
│   No Index  │  (First run)
└──────┬──────┘
       │ ggen marketplace sync
       ▼
┌─────────────┐
│  Fetching   │  (HTTP GET registry URL)
└──────┬──────┘
       │ download complete
       ▼
┌─────────────┐
│   Parsing   │  (Parse JSON index)
└──────┬──────┘
       │ validation success
       ▼
┌─────────────┐
│   Cached    │  (index.json saved locally)
└──────┬──────┘
       │ TTL expires (24h)
       ▼
┌─────────────┐
│    Stale    │  (Needs refresh)
└──────┬──────┘
       │ auto-sync or manual sync
       └──────► (back to Fetching)

Error States:
  Fetching → Error:   Network failure
  Parsing → Error:    Invalid JSON
  Validation → Error: Corrupt data
```

---

## Sequence Diagrams

### Recommendation Flow

```
User          CLI             Domain          Maturity        Registry
 │             │               │              Evaluator          │
 │ recommend   │               │                 │               │
 │  production │               │                 │               │
 ├────────────►│               │                 │               │
 │             │ execute_      │                 │               │
 │             │ recommend()   │                 │               │
 │             ├──────────────►│                 │               │
 │             │               │ generate_all_  │               │
 │             │               │ assessments()   │               │
 │             │               ├────────────────►│               │
 │             │               │                 │ list_packages()│
 │             │               │                 ├──────────────►│
 │             │               │                 │ packages[]    │
 │             │               │                 │◄──────────────┤
 │             │               │                 │               │
 │             │               │ assessments[]   │               │
 │             │               │◄────────────────┤               │
 │             │               │                 │               │
 │             │               │ filter_by_use_case("production")│
 │             │               ├─────────────────┐               │
 │             │               │ min_score: 65    │               │
 │             │               │◄─────────────────┘               │
 │             │               │                 │               │
 │             │               │ sort_by_score() │               │
 │             │               ├────────────────┐│               │
 │             │               │◄────────────────┘│               │
 │             │               │                 │               │
 │             │ recommendations[]               │               │
 │             │◄──────────────┤                 │               │
 │ JSON output │               │                 │               │
 │◄────────────┤               │                 │               │
 │             │               │                 │               │
```

### Package Validation with Guards

```
CLI           Domain          Guards          FileSystem      Maturity
 │             │               │                 │            Evaluator
 │ validate    │               │                 │               │
 │  pkg-name   │               │                 │               │
 ├────────────►│               │                 │               │
 │             │ validate_     │                 │               │
 │             │ package()     │                 │               │
 │             ├──────────────►│                 │               │
 │             │               │ check_readme()  │               │
 │             │               ├────────────────►│               │
 │             │               │ README.md exists│               │
 │             │               │◄────────────────┤               │
 │             │               │ ✓ PASS          │               │
 │             │               │                 │               │
 │             │               │ check_tests()   │               │
 │             │               ├────────────────►│               │
 │             │               │ tests/ exists   │               │
 │             │               │◄────────────────┤               │
 │             │               │ ✓ PASS          │               │
 │             │               │                 │               │
 │             │               │ check_coverage()│               │
 │             │               ├────────────────►│               │
 │             │               │ coverage: 88%   │               │
 │             │               │◄────────────────┤               │
 │             │               │ ✓ PASS (>80%)   │               │
 │             │               │                 │               │
 │             │               │ check_security()│               │
 │             │               ├────────────────►│               │
 │             │               │ no vulnerabilities│              │
 │             │               │◄────────────────┤               │
 │             │               │ ✓ PASS          │               │
 │             │               │                 │               │
 │             │               │ all_checks[]    │               │
 │             │◄──────────────┤                 │               │
 │             │               │                 │               │
 │             │ evaluate()    │                 │               │
 │             ├──────────────────────────────────────────────►│
 │             │                                  maturity_score │
 │             │◄────────────────────────────────────────────────┤
 │             │                                  78/100         │
 │             │               │                 │               │
 │  validation │               │                 │               │
 │  result     │               │                 │               │
 │◄────────────┤               │                 │               │
 │             │               │                 │               │
```

---

## Database Schema (Future)

### Package Registry Schema

```sql
-- Packages table
CREATE TABLE packages (
    id              UUID PRIMARY KEY,
    name            VARCHAR(100) UNIQUE NOT NULL,
    description     TEXT,
    author          VARCHAR(100),
    category        VARCHAR(50),
    license         VARCHAR(50),
    repository_url  TEXT,
    homepage_url    TEXT,
    is_8020         BOOLEAN DEFAULT FALSE,
    is_8020_certified BOOLEAN DEFAULT FALSE,
    sector          VARCHAR(50),
    created_at      TIMESTAMP DEFAULT NOW(),
    updated_at      TIMESTAMP DEFAULT NOW()
);

-- Package versions
CREATE TABLE package_versions (
    id              UUID PRIMARY KEY,
    package_id      UUID REFERENCES packages(id) ON DELETE CASCADE,
    version         VARCHAR(50) NOT NULL,
    download_url    TEXT NOT NULL,
    checksum_sha256 CHAR(64) NOT NULL,
    size_bytes      BIGINT NOT NULL,
    published_at    TIMESTAMP DEFAULT NOW(),
    UNIQUE(package_id, version)
);

-- Package dependencies
CREATE TABLE package_dependencies (
    id              UUID PRIMARY KEY,
    version_id      UUID REFERENCES package_versions(id) ON DELETE CASCADE,
    dependency_name VARCHAR(100) NOT NULL,
    version_req     VARCHAR(50) NOT NULL,
    optional        BOOLEAN DEFAULT FALSE
);

-- Maturity assessments
CREATE TABLE maturity_assessments (
    id                  UUID PRIMARY KEY,
    package_id          UUID REFERENCES packages(id) ON DELETE CASCADE,
    assessed_at         TIMESTAMP DEFAULT NOW(),
    total_score         INTEGER CHECK (total_score >= 0 AND total_score <= 100),
    maturity_level      VARCHAR(20) CHECK (maturity_level IN ('experimental', 'beta', 'production', 'enterprise')),

    -- Dimension scores
    documentation_score INTEGER CHECK (documentation_score >= 0 AND documentation_score <= 20),
    testing_score       INTEGER CHECK (testing_score >= 0 AND testing_score <= 20),
    security_score      INTEGER CHECK (security_score >= 0 AND security_score <= 20),
    performance_score   INTEGER CHECK (performance_score >= 0 AND performance_score <= 15),
    adoption_score      INTEGER CHECK (adoption_score >= 0 AND adoption_score <= 15),
    maintenance_score   INTEGER CHECK (maintenance_score >= 0 AND maintenance_score <= 10),

    UNIQUE(package_id, assessed_at)
);

-- Package downloads (analytics)
CREATE TABLE package_downloads (
    id              UUID PRIMARY KEY,
    package_id      UUID REFERENCES packages(id) ON DELETE CASCADE,
    version_id      UUID REFERENCES package_versions(id) ON DELETE CASCADE,
    downloaded_at   TIMESTAMP DEFAULT NOW(),
    user_ip         INET,
    user_agent      TEXT
);

-- Indexes for performance
CREATE INDEX idx_packages_category ON packages(category);
CREATE INDEX idx_packages_sector ON packages(sector);
CREATE INDEX idx_packages_8020 ON packages(is_8020_certified);
CREATE INDEX idx_versions_published ON package_versions(published_at DESC);
CREATE INDEX idx_assessments_score ON maturity_assessments(total_score DESC);
CREATE INDEX idx_assessments_level ON maturity_assessments(maturity_level);
CREATE INDEX idx_downloads_time ON package_downloads(downloaded_at DESC);
```

---

## Deployment Architecture (Future)

### Multi-Tier Deployment

```
┌─────────────────────────────────────────────────────────────────┐
│                      Load Balancer (HTTPS)                      │
│                   marketplace.ggen.io                           │
└────────────────┬────────────────────┬───────────────────────────┘
                 │                    │
                 ▼                    ▼
┌─────────────────────────┐  ┌─────────────────────────┐
│   Web API Server 1      │  │   Web API Server 2      │
│   (GraphQL + REST)      │  │   (GraphQL + REST)      │
│   - Package search      │  │   - Package search      │
│   - Maturity API        │  │   - Maturity API        │
│   - Download proxy      │  │   - Download proxy      │
└────────┬────────────────┘  └────────┬────────────────┘
         │                            │
         └────────────┬───────────────┘
                      │
                      ▼
         ┌────────────────────────┐
         │  PostgreSQL Database   │
         │  - Package metadata    │
         │  - Maturity scores     │
         │  - Analytics           │
         └────────┬───────────────┘
                  │
                  ▼
         ┌────────────────────────┐
         │  Elasticsearch Cluster │
         │  - Full-text search    │
         │  - Faceted filtering   │
         └────────────────────────┘

┌────────────────────────────────────────────────────────────────┐
│                  CDN (Package Artifacts)                       │
│                  - .tar.gz archives                            │
│                  - Static assets                               │
└────────────────────────────────────────────────────────────────┘
```

---

**End of Architecture Diagrams**
