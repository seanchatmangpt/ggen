# GGen Marketplace CLI - Architecture Diagrams

## 1. Three-Layer Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    USER SHELL                                   │
│          $ ggen marketplace <command> [options]                  │
└────────────────────────┬────────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────────┐
│  LAYER 1: CLI COMMAND ROUTING (ggen-cli)                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  main.rs (14 LOC)                                               │
│  └─> cli_match() ─────────────────────────────────────────────┐ │
│      lib.rs (163 LOC)                                          │ │
│      └─> cmds::run_cli() ────────────────────────────────────┐ │ │
│          cmds/mod.rs (38 LOC)                                │ │ │
│          └─> clap_noun_verb::run() [auto-discovery]         │ │ │
│              clap-noun-verb v3.4.0                           │ │ │
│                                                             │ │ │
│              Finds all #[verb] functions:                   │ │ │
│              cmds/marketplace.rs (1,746 LOC)                │ │ │
│              ├─ search()             (lines 153)            │ │ │
│              ├─ install()            (lines 186)            │ │ │
│              ├─ publish()            (lines 295)            │ │ │
│              ├─ list()               (lines 229)            │ │ │
│              ├─ validate()           (lines 335)            │ │ │
│              ├─ maturity()           (lines 469)            │ │ │
│              ├─ dashboard()          (lines 566)            │ │ │
│              ├─ recommend()          (lines 788)            │ │ │
│              ├─ compare()            (lines 888)            │ │ │
│              ├─ search_maturity()    (lines 1035)           │ │ │
│              ├─ export()             (lines 1135)           │ │ │
│              ├─ list_bundles()       (lines 1239)           │ │ │
│              ├─ bundle_info()        (lines 1294)           │ │ │
│              ├─ install_bundle()     (lines 1357)           │ │ │
│              ├─ emit_receipts()      (lines 1430)           │ │ │
│              ├─ report()             (lines 1503)           │ │ │
│              ├─ generate_artifacts() (lines 1560)           │ │ │
│              ├─ improve()            (lines 1614)           │ │ │
│              └─ maturity_batch()     (lines 685)            │ │ │
│                                                             │ │ │
│              20 Total Commands                              │ │ │
│                                                             │ │ │
│              Pattern: #[verb] fn(args) -> Result<T>         │ │ │
│              - Sync verb functions                          │ │ │
│              - clap-noun-verb macros for arg parsing        │ │ │
│              - Serializable output types                    │ │ │
│                                                             │ │ │
│ ┌─────────────────────────────────────────────────────────┐ │ │ │
│ │ ASYNC BRIDGE (runtime_helper.rs)                        │ │ │ │
│ │ ┌──────────────────────────────────────────────────┐    │ │ │ │
│ │ │ execute_async_verb<F, T>(future: F)              │    │ │ │ │
│ │ │ - Detects existing tokio runtime                 │    │ │ │ │
│ │ │ - Creates new runtime if needed                  │    │ │ │ │
│ │ │ - Handles nested runtime panics                  │    │ │ │ │
│ │ │ - Converts anyhow::Error → NounVerbError         │    │ │ │ │
│ │ └──────────────────────────────────────────────────┘    │ │ │ │
│ └─────────────────────────────────────────────────────────┘ │ │ │
│                                                             │ │ │
└─────────────────────────────┬───────────────────────────────┘ │ │
                              │                                 │ │
                              ▼                                 │ │
┌─────────────────────────────────────────────────────────────┐ │ │
│  LAYER 2: DOMAIN LOGIC (ggen-domain/marketplace)            │ │ │
├─────────────────────────────────────────────────────────────┤ │ │
│                                                             │ │ │
│  Async Functions (12.4K LOC across 22 modules):            │ │ │
│                                                             │ │ │
│  ┌──────────────────────────────────────────────────────┐  │ │ │
│  │ pub async fn execute_search(input)                   │  │ │ │
│  │ pub async fn execute_install(input)                  │  │ │ │
│  │ pub async fn execute_publish(input)                  │  │ │ │
│  │ pub async fn execute_list(input)                     │  │ │ │
│  │ pub fn validate_package(path)                        │  │ │ │
│  │ pub fn validate_all_packages(path)                   │  │ │ │
│  │ pub async fn execute_update(input)                   │  │ │ │
│  │ pub fn execute_update(input)                         │  │ │ │
│  └──────────────────────────────────────────────────────┘  │ │ │
│                                                             │ │ │
│  Modules:                                                   │ │ │
│  ├─ search.rs (1,335 LOC)        - Search + scoring        │ │ │
│  ├─ install.rs (1,649 LOC)       - Installation logic      │ │ │
│  ├─ validate.rs (1,106 LOC)      - Validation logic        │ │ │
│  ├─ registry.rs (1,103 LOC)      - Registry management     │ │ │
│  ├─ publish.rs (630 LOC)         - Publishing logic        │ │ │
│  ├─ guards.rs (703 LOC)          - Guard validation        │ │ │
│  ├─ bundles.rs (271 LOC)         - Bundle management       │ │ │
│  ├─ artifact_generator.rs (294)  - Report generation       │ │ │
│  ├─ receipt_emitter.rs (281)     - Receipt generation      │ │ │
│  ├─ types.rs (351 LOC)           - Poka-yoke types        │ │ │
│  ├─ production_readiness.rs      - Readiness checks        │ │ │
│  ├─ quality_autopilot.rs         - Improvement plans       │ │ │
│  ├─ mape_k_integration.rs        - Autonomic marketplace   │ │ │
│  └─ 14 more supporting modules                             │ │ │
│                                                             │ │ │
└─────────────────────────────┬───────────────────────────────┘ │ │
                              │                                 │ │
                              ▼                                 │ │
┌─────────────────────────────────────────────────────────────┐ │ │
│  LAYER 3: LEGACY & MATURITY (ggen-marketplace)              │ │ │
├─────────────────────────────────────────────────────────────┤ │ │
│                                                             │ │ │
│  ggen-marketplace/src/                                      │ │ │
│  ├─ maturity_evaluator.rs                                  │ │ │
│  │  └─ pub fn MaturityEvaluator::evaluate(input)           │ │ │
│  │                                                         │ │ │
│  ├─ assessment_helpers.rs                                  │ │ │
│  │  ├─ generate_all_assessments()                          │ │ │
│  │  ├─ find_for_use_case()                                 │ │ │
│  │  ├─ export_as_csv()                                     │ │ │
│  │  └─ export_as_json()                                    │ │ │
│  │                                                         │ │ │
│  └─ Many other modules                                     │ │ │
│     (models, traits, search, backend, storage,             │ │ │
│      template_search, guards, etc.)                        │ │ │
│                                                             │ │ │
│  TYPES:                                                     │ │ │
│  ├─ MaturityEvaluator                                      │ │ │
│  ├─ MaturityAssessment                                     │ │ │
│  ├─ MaturityDashboard                                      │ │ │
│  ├─ MaturityLevel {                                        │ │ │
│  │  ├─ Experimental (0-40)                                 │ │ │
│  │  ├─ Beta (41-60)                                        │ │ │
│  │  ├─ Production (61-80)                                  │ │ │
│  │  └─ Enterprise (81-100)                                 │ │ │
│  └─ EvaluationInput                                        │ │ │
│                                                             │ │ │
│  STATUS: Transitional - being phased out for v2/v3         │ │ │
│                                                             │ │ │
└─────────────────────────────────────────────────────────────┘ │ │
                              │                                 │ │
                              ▼                                 │ │
┌─────────────────────────────────────────────────────────────┐ │ │
│  FUTURE LAYER: v2/v3 MARKETPLACE (ggen-marketplace-v2)      │ │ │
├─────────────────────────────────────────────────────────────┤ │ │
│                                                             │ │ │
│  Under Development:                                         │ │ │
│  ├─ registry_rdf.rs (12.3K LOC)  - RDF backend             │ │ │
│  ├─ v3.rs (9.8K LOC)             - V3 optimization         │ │ │
│  ├─ ontology.rs (11K LOC)        - RDF ontology            │ │ │
│  ├─ search_sparql.rs             - SPARQL queries          │ │ │
│  └─ Other advanced features                                │ │ │
│                                                             │ │ │
│  STATUS: Not yet integrated into CLI                        │ │ │
│  BACKEND: oxigraph (RDF triplestore)                        │ │ │
│                                                             │ │ │
└─────────────────────────────────────────────────────────────┘ │ │
                                                               │ │
└───────────────────────────────────────────────────────────────┘
```

## 2. Command Discovery & Routing Flow

```
┌─────────────────────────────────────────────────────────────┐
│                     User invokes:                            │
│          $ ggen marketplace search --query "rust"            │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ ggen-cli/src/main.rs                                        │
│ #[tokio::main] async fn main()                              │
│   └─> ggen_cli_lib::cli_match()                             │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ ggen-cli/src/lib.rs                                         │
│ async fn cli_match()                                        │
│   └─> cmds::run_cli()                                       │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ ggen-cli/src/cmds/mod.rs                                    │
│ pub fn run_cli()                                            │
│   └─> clap_noun_verb::run()                                 │
│       [Auto-discovers all #[verb] functions]                │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ clap_noun_verb v3.4.0 (external crate)                      │
│ Macro system parses args and routes to verb functions       │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ ggen-cli/src/cmds/marketplace.rs                            │
│ Found: search() function at line 153                        │
│ Signatures parsed:                                          │
│   fn search(                                                │
│     query: String,          <- matches --query "rust"       │
│     limit: Option<usize>,   <- default 10                   │
│     category: Option<String> <- default None                │
│   ) -> Result<SearchOutput>                                 │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ searchverb function body (lines 153-182)                    │
│ 1. Create SearchInput from args                             │
│ 2. Call execute_async_verb(async { ... })                   │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ ggen-cli/src/runtime_helper.rs                              │
│ pub fn execute_async_verb<F, T>(future: F)                  │
│ - Detects: Are we in a tokio runtime? Yes (main.rs)         │
│ - Solution: Spawn blocking thread to avoid nested runtime   │
│ - Create new runtime in thread                              │
│ - Block on future: rt.block_on(future)                      │
│ - Return result or convert error                            │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ ggen-domain/src/marketplace/search.rs                       │
│ pub async fn execute_search(input: SearchInput)             │
│   -> Result<Vec<SearchResult>>                              │
│ - Build filters from SearchInput                            │
│ - Call search_packages(&query, &filters).await              │
│ - Perform relevance scoring                                 │
│ - Return results                                            │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ Back to verb function (lines 160-181)                       │
│ 1. Get SearchResult[] from execute_search()                 │
│ 2. Transform to SearchOutput type                           │
│ 3. Return Ok(SearchOutput)                                  │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ clap_noun_verb formatting & serialization                   │
│ - Serialize SearchOutput to JSON (via serde)                │
│ - Print to stdout                                           │
└────────────────────────┬────────────────────────────────────┘
                         │
                         ▼
┌─────────────────────────────────────────────────────────────┐
│ Output to user:                                             │
│ {                                                           │
│   "packages": [                                             │
│     { "name": "...", "version": "...", ... },              │
│     ...                                                     │
│   ],                                                        │
│   "total": 42                                               │
│ }                                                           │
└─────────────────────────────────────────────────────────────┘
```

## 3. Dependency Graph

```
ggen-cli (binary + lib)
├── clap-noun-verb v3.4.0
│   └─ Macro-based command discovery & routing
├── ggen-domain v3.0.0
│   ├── ggen-utils (error handling, types)
│   ├── serde (serialization)
│   ├── tokio (async runtime)
│   └── Many others (SHA256, zip, etc.)
├── ggen-marketplace v3.0.0 (LEGACY - being phased out)
│   ├── maturity_evaluator
│   ├── assessment_helpers
│   ├── backend (LocalRegistry)
│   ├── crypto (Ed25519Verifier)
│   ├── search (TantivySearchEngine)
│   └── storage (FilesystemStore, MemoryStore)
├── tokio (async runtime bridge)
├── anyhow (error handling)
├── serde_json (JSON serialization)
├── toml (TOML parsing)
├── gag (output capture for Node addon)
└── Many others

ggen-domain (library)
├── ggen-marketplace (LEGACY)
├── ggen-utils
├── serde
├── tokio
└── Others

ggen-marketplace (library - LEGACY, transitional)
├── backend (registry implementations)
├── crypto (verification)
├── models (Package, Version, Query)
├── search (TantivySearchEngine)
├── storage (package storage)
├── traits (Registry, PackageStore, SearchEngine, CryptoVerifier)
├── template_search
├── guards (validation)
└── Others

ggen-marketplace-v2 (library - NEW, parallel, NOT YET INTEGRATED)
├── oxigraph (RDF triplestore)
└── Others
```

## 4. Module Dependency Chain

```
CLI Commands (marketplace.rs)
      │
      ├─ search()
      │  └─> execute_search()
      │      └─> ggen-domain/marketplace/search.rs
      │
      ├─ install()
      │  └─> execute_install()
      │      └─> ggen-domain/marketplace/install.rs
      │
      ├─ publish()
      │  └─> execute_publish()
      │      └─> ggen-domain/marketplace/publish.rs
      │
      ├─ list()
      │  └─> execute_list()
      │      └─> ggen-domain/marketplace/list.rs
      │
      ├─ validate()
      │  └─> validate_package() or validate_all_packages()
      │      └─> ggen-domain/marketplace/validate.rs
      │
      ├─ maturity()
      │  └─> MaturityEvaluator::evaluate()
      │      └─> ggen-marketplace/maturity_evaluator.rs (LEGACY)
      │
      ├─ dashboard()
      │  └─> MaturityDashboard::new()
      │      └─> ggen-marketplace (LEGACY)
      │
      ├─ recommend()
      │  └─> generate_all_assessments()
      │      └─> ggen-marketplace/assessment_helpers.rs (LEGACY)
      │
      ├─ compare()
      │  └─> generate_all_assessments()
      │      └─> ggen-marketplace/assessment_helpers.rs (LEGACY)
      │
      ├─ search_maturity()
      │  └─> generate_all_assessments()
      │      └─> ggen-marketplace/assessment_helpers.rs (LEGACY)
      │
      ├─ export()
      │  └─> export_as_csv(), export_as_json()
      │      └─> ggen-marketplace/assessment_helpers.rs (LEGACY)
      │
      ├─ list_bundles()
      │  └─> BundleRegistry::list_bundles()
      │      └─> ggen-domain/marketplace/bundles.rs
      │
      ├─ bundle_info()
      │  └─> BundleRegistry::get_bundle(), generate_bundle_docs()
      │      └─> ggen-domain/marketplace/bundles.rs
      │
      ├─ install_bundle()
      │  └─> BundleRegistry::get_bundle(), BundleInstallManifest
      │      └─> ggen-domain/marketplace/bundles.rs
      │
      ├─ emit_receipts()
      │  └─> emit_receipts_for_marketplace(), generate_validation_report()
      │      └─> ggen-domain/marketplace/receipt_emitter.rs
      │
      ├─ report()
      │  └─> generate_validation_report()
      │      └─> ggen-domain/marketplace/receipt_emitter.rs
      │
      ├─ generate_artifacts()
      │  └─> generate_registry_index(), generate_packages_markdown()
      │      └─> ggen-domain/marketplace/artifact_generator.rs
      │
      ├─ improve()
      │  └─> generate_improvement_plan(), apply_template_improvements()
      │      └─> ggen-domain/marketplace/quality_autopilot.rs
      │
      └─ maturity_batch()
         └─> MaturityAssessment::new()
             └─> ggen-marketplace (LEGACY)
```

## 5. Input/Output Flow

```
┌──────────────────────────────┐
│   CLI Arguments (strings)    │
│   $ ggen marketplace search  │
│     --query "rust"           │
│     --limit 10               │
│     --category "web"         │
└────────┬─────────────────────┘
         │
         ▼
┌──────────────────────────────┐
│  Verb Function Arguments     │
│  fn search(                  │
│    query: String,            │
│    limit: Option<usize>,     │
│    category: Option<String>  │
│  )                           │
└────────┬─────────────────────┘
         │
         ▼
┌──────────────────────────────┐
│  Domain Input Type           │
│  SearchInput {               │
│    query: String,            │
│    limit: usize,             │
│    category: Option<String>, │
│    keyword: Option<String>,  │
│    author: Option<String>,   │
│    fuzzy: bool,              │
│    only_8020: bool,          │
│    ...                       │
│  }                           │
└────────┬─────────────────────┘
         │
         ▼
┌──────────────────────────────┐
│  Domain Function Execution   │
│  execute_search(input)       │
│  -> Result<Vec<SearchResult>>│
└────────┬─────────────────────┘
         │
         ▼
┌──────────────────────────────┐
│  CLI Output Type             │
│  SearchOutput {              │
│    packages: Vec<PackageInfo>│
│    total: usize              │
│  }                           │
└────────┬─────────────────────┘
         │
         ▼
┌──────────────────────────────┐
│  JSON Serialization (serde)  │
│  {                           │
│    "packages": [...],        │
│    "total": 42               │
│  }                           │
└────────┬─────────────────────┘
         │
         ▼
┌──────────────────────────────┐
│  Output to stdout            │
│  (user sees JSON)            │
└──────────────────────────────┘
```

---

## Key Insights

1. **Auto-Discovery Magic**: No manual routing - clap-noun-verb finds functions via macros
2. **Three Generations**: CLI bridges old (ggen-marketplace), current (ggen-domain), and new (v2/v3)
3. **Async Bridge Required**: Sync verb functions need runtime helper for async domain logic
4. **Type Safety**: Input types validated at compile time (ValidatedPackageName, etc.)
5. **Extensible**: Adding new commands = adding new #[verb] function, automatically discovered
6. **Maturity System**: 6-dimension scoring (docs, testing, security, performance, adoption, maintenance)
7. **Migration Path**: v2/v3 ready in parallel crate, awaiting CLI integration refactor

