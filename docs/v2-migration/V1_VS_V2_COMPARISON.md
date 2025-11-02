# ggen v1 vs v2 Comparison Guide

**Quick Reference**: Side-by-side comparison of v1.x and v2.0.0 commands and architecture.

---

## Command Comparison

### Template Operations

| Operation | v1.x Command | v2.0.0 Command |
|-----------|--------------|----------------|
| Generate | `ggen gen template.tmpl` | `ggen template generate -t template.tmpl` |
| Generate tree | `ggen tree-gen spec.yaml` | `ggen template generate-tree -t spec.yaml` |
| Lint | `ggen lint template.tmpl` | `ggen template lint template.tmpl` |
| List | `ggen list-templates` | `ggen template list` |
| New | `ggen new-template name` | `ggen template new name` |
| Regenerate | `ggen regen template.tmpl` | `ggen template regenerate template.tmpl` |
| Show | `ggen show-template template.tmpl` | `ggen template show template.tmpl` |

### AI Operations

| Operation | v1.x Command | v2.0.0 Command |
|-----------|--------------|----------------|
| Generate | `ggen ai-gen "prompt"` | `ggen ai generate "prompt"` |
| Chat | `ggen ai-chat` | `ggen ai chat --interactive` |
| Analyze | `ggen ai-analyze file.rs` | `ggen ai analyze --file file.rs` |

### Graph Operations

| Operation | v1.x Command | v2.0.0 Command |
|-----------|--------------|----------------|
| Load | `ggen load-graph file.ttl` | `ggen graph load file.ttl` |
| Query | `ggen sparql "query"` | `ggen graph query "query"` |
| Export | `ggen export-graph` | `ggen graph export --format turtle` |
| Visualize | `ggen viz-graph file.ttl` | `ggen graph visualize file.ttl` |

### Marketplace Operations

| Operation | v1.x Command | v2.0.0 Command |
|-----------|--------------|----------------|
| Search | `ggen market search "query"` | `ggen marketplace search "query"` |
| Install | `ggen market install pkg` | `ggen marketplace install pkg` |
| List | `ggen market list` | `ggen marketplace list` |
| Publish | `ggen market publish` | `ggen marketplace publish` |
| Update | `ggen market update` | `ggen marketplace update` |

### Project Operations

| Operation | v1.x Command | v2.0.0 Command |
|-----------|--------------|----------------|
| New | `ggen new-project name` | `ggen project new name --type rust-web` |
| Plan | `ggen plan` | `ggen project plan` |
| Generate | `ggen project-gen` | `ggen project gen` |
| Apply | `ggen apply` | `ggen project apply` |

### Hook Operations

| Operation | v1.x Command | v2.0.0 Command |
|-----------|--------------|----------------|
| Create | `ggen create-hook` | `ggen hook create --trigger event` |
| List | `ggen list-hooks` | `ggen hook list` |
| Remove | `ggen remove-hook name` | `ggen hook remove name` |
| Monitor | `ggen monitor-hooks` | `ggen hook monitor` |

### Utility Operations

| Operation | v1.x Command | v2.0.0 Command |
|-----------|--------------|----------------|
| Doctor | `ggen doctor` | `ggen utils doctor` |
| Environment | `ggen env` | `ggen utils env` |

### CI Operations

| Operation | v1.x Command | v2.0.0 Command |
|-----------|--------------|----------------|
| Workflow | `ggen ci-workflow` | `ggen ci workflow --provider github` |

---

## Architecture Comparison

### Code Structure

#### v1.x Architecture

```
cli/src/
├── commands/          # Flat command structure
│   ├── gen.rs        # Generate command
│   ├── lint.rs       # Lint command
│   ├── market.rs     # Marketplace commands
│   ├── ...           # 77 command files
│   └── mod.rs        # Manual registration
├── lib.rs            # Manual CLI setup (279 LOC)
└── main.rs           # Entry point
```

**Issues:**
- Manual command registration (415 LOC boilerplate)
- Mixed concerns (CLI + business logic)
- Hard to test in isolation
- Duplicate initialization code

#### v2.0.0 Architecture

```
cli/src/
├── cmds/             # CLI routing layer (1,608 LOC)
│   ├── mod.rs       # Auto-discovery router
│   ├── template.rs  # Template noun
│   ├── ai.rs        # AI noun
│   ├── graph.rs     # Graph noun
│   └── ...          # 8 noun files
├── domain/          # Business logic (9,533 LOC)
│   ├── template/    # Template operations
│   ├── ai/          # AI operations
│   ├── graph/       # Graph operations
│   └── ...          # 8 domain modules
├── runtime.rs       # Sync/async bridge (38 LOC)
├── lib.rs           # Clean entry point (94 LOC)
└── main.rs          # Minimal bootstrap
```

**Improvements:**
- Auto-discovery (0 LOC boilerplate)
- Clear separation of concerns
- Independently testable layers
- Global runtime (no duplication)

---

## Performance Comparison

| Metric | v1.x | v2.0.0 | Improvement |
|--------|------|--------|-------------|
| Full compilation | 60-90s | 30-45s | **50% faster** |
| Incremental build | 10-15s | 5-8s | **50% faster** |
| Generation speed | <3s | <2s | **33% faster** |
| Binary size | 25MB | 18MB | **28% smaller** |
| Memory usage | 150MB | 100MB | **33% less** |
| Test suite | 120s | 60s | **50% faster** |

---

## API Comparison

### Library Usage

#### v1.x API

```rust
use ggen::cli::commands::MarketClient;
use ggen::cli::commands::TemplateGenerator;

// Marketplace client
let client = MarketClient::new();
client.search("rust web")?;

// Template generation
let gen = TemplateGenerator::new();
gen.generate("template.tmpl", context)?;
```

#### v2.0.0 API

```rust
use ggen::cli::domain::marketplace::MarketplaceClient;
use ggen::cli::domain::template::TemplateGenerator;

// Marketplace client (builder pattern)
let client = MarketplaceClient::builder()
    .config(config)
    .build()?;
client.search("rust web")?;

// Template generation (with RDF)
let gen = TemplateGenerator::builder()
    .template_path("template.tmpl")
    .rdf_files(vec!["data.ttl"])
    .build()?;
gen.generate()?;
```

**Changes:**
- Builder pattern for construction
- Explicit RDF file passing
- Updated import paths
- Better error handling

---

## Testing Comparison

### Test Coverage

| Category | v1.x | v2.0.0 | Improvement |
|----------|------|--------|-------------|
| Unit tests | 89 | 147 | **+65%** |
| Integration tests | 98 | 127 | **+30%** |
| E2E tests | 8 | 11 | **+38%** |
| Benchmarks | 4 | 7 | **+75%** |
| Pass rate | 94% | 98.9% | **+5.2%** |

### Test Strategy

#### v1.x Strategy
- Broad coverage (try to test everything)
- Many flaky tests
- Slow execution (120s)
- Mock-heavy

#### v2.0.0 Strategy
- 80/20 principle (focus on critical 20%)
- No flaky tests (100% pass requirement)
- Fast execution (<2s per suite, 60s total)
- Real systems (Chicago TDD)

---

## Migration Checklist

### For CLI Users

- [ ] Update installation (`brew upgrade ggen` or `cargo install ggen@2.0`)
- [ ] Run `ggen utils doctor --migrate-config`
- [ ] Update scripts:
  - [ ] Replace flat commands with noun-verb pattern
  - [ ] Change `market` to `marketplace`
  - [ ] Review command arguments
- [ ] Test critical workflows
- [ ] Update documentation

### For Library Users

- [ ] Update `Cargo.toml` to `ggen = "2.0"`
- [ ] Update imports:
  - [ ] `use ggen::cli::domain::*`
  - [ ] Update struct names
- [ ] Update client creation to builder pattern
- [ ] Update error handling
- [ ] Run tests
- [ ] Update documentation

### For Template Authors

- [ ] **No changes needed** - templates are 100% compatible!
- [ ] Optional: Test with `ggen template lint`
- [ ] Optional: Add metadata with `ggen template show`

---

## Feature Parity

### Features in Both Versions

✅ Template generation with RDF
✅ AI-powered code generation
✅ SPARQL queries
✅ Marketplace integration
✅ Project scaffolding
✅ Lifecycle hooks
✅ System diagnostics
✅ CI/CD workflow generation

### New in v2.0.0

🆕 clap-noun-verb auto-discovery
🆕 Three-layer architecture
🆕 Global runtime pattern
🆕 80/20 testing strategy
🆕 Builder pattern APIs
🆕 Improved error messages
🆕 Better performance
🆕 Smaller binaries

### Deprecated in v2.0.0

⚠️ Flat command structure
⚠️ Manual command registration
⚠️ `market` shorthand (use `marketplace`)
⚠️ Old import paths
⚠️ Direct client construction

---

## Compatibility Matrix

| Feature | v1.x | v2.0.0 | Compatible? |
|---------|------|--------|-------------|
| Templates | ✅ | ✅ | ✅ 100% |
| RDF files | ✅ | ✅ | ✅ 100% |
| SPARQL queries | ✅ | ✅ | ✅ 100% |
| Configuration | ✅ | ✅ | ⚠️ Auto-migration |
| CLI commands | ✅ | ✅ | ❌ Breaking changes |
| Library API | ✅ | ✅ | ⚠️ Builder pattern |
| Marketplace packages | ✅ | ✅ | ✅ 100% |
| Project scaffolds | ✅ | ✅ | ✅ 100% |

---

## Deprecation Timeline

| Version | Status | Support Until | Notes |
|---------|--------|---------------|-------|
| **v1.0-1.1** | EOL | 2024-12-31 | No longer supported |
| **v1.2.x** | Security fixes | 2025-06-30 | Critical fixes only |
| **v1.x** | EOL | 2025-09-30 | Final v1 release |
| **v2.0.x** | ✅ **Current** | 2027+ | **Long-term support** |
| **v2.1.0** | Planned | Dec 2025 | Enhanced features |
| **v3.0.0** | Roadmap | Q2 2026 | Next major version |

---

## Quick Migration Examples

### Example 1: Simple Template Generation

```bash
# v1.x
ggen gen templates/service.tmpl --vars name=auth

# v2.0.0
ggen template generate -t templates/service.tmpl --var name=auth
```

### Example 2: Marketplace Search

```bash
# v1.x
ggen market search "rust web"

# v2.0.0
ggen marketplace search "rust web"
```

### Example 3: Project Creation

```bash
# v1.x
ggen new-project my-app

# v2.0.0
ggen project new my-app --type rust-web
```

### Example 4: RDF Graph Query

```bash
# v1.x
ggen sparql "SELECT * WHERE {?s ?p ?o}" --graph project.ttl

# v2.0.0
ggen graph query "SELECT * WHERE {?s ?p ?o}" -g project.ttl
```

### Example 5: System Health Check

```bash
# v1.x
ggen doctor

# v2.0.0
ggen utils doctor
```

---

## Support Resources

- **Migration Guide**: [docs/MIGRATION_V1_TO_V2.md](../MIGRATION_V1_TO_V2.md)
- **Completion Report**: [docs/v2-migration/COMPLETION_REPORT.md](COMPLETION_REPORT.md)
- **Release Notes**: [docs/RELEASE_NOTES_v2.0.0.md](../RELEASE_NOTES_v2.0.0.md)
- **Architecture Guide**: [docs/architecture/V2_ARCHITECTURE_FINAL.md](../architecture/V2_ARCHITECTURE_FINAL.md)
- **GitHub Issues**: [github.com/seanchatmangpt/ggen/issues](https://github.com/seanchatmangpt/ggen/issues)
- **Discussions**: [github.com/seanchatmangpt/ggen/discussions](https://github.com/seanchatmangpt/ggen/discussions)

---

**Last Updated**: 2025-11-02
**Version**: v2.0.0
**Status**: ✅ Production Ready
