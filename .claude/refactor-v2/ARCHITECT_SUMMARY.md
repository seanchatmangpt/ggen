# Architect Agent Summary - ggen v2.0.0

**Agent**: System Architect
**Task**: Design complete v2.0.0 architecture
**Status**: ✅ COMPLETE
**Date**: 2025-11-01

---

## Deliverables

1. **Architecture Design Document** (25,000+ words)
   - Location: `.claude/refactor-v2/03-v2-architecture-design.md`
   - Comprehensive design for clap-noun-verb migration
   - Domain layer separation strategy
   - Frozen section template enhancement
   - Migration sequencing plan

2. **Architecture Diagrams** (Visual Reference)
   - Location: `.claude/refactor-v2/ARCHITECTURE_DIAGRAMS.md`
   - 10 key diagrams for quick reference
   - Command flow diagrams
   - Module structure visualizations

---

## Key Design Decisions

### 1. Three-Layer Architecture

```
CLI Layer (commands/) → Domain Layer (domain/) → Infrastructure (ggen-core)
```

**Rationale**: Separation of concerns enables:
- Testable business logic (no CLI dependencies)
- Easy addition of alternative interfaces (TUI, web API)
- Clear responsibility boundaries

### 2. Auto-Discovery via clap-noun-verb v3.0.0

**Pattern**: Filesystem-based routing with `#[verb]` attributes

**Benefits**:
- No central enum registration
- Easy to add new commands (drop file in `commands/{noun}/{verb}.rs`)
- Clear noun-verb command structure

**Trade-off**: Migration complexity (dual systems during transition)

### 3. Frozen Section Support in Templates

**Feature**: Mark template sections as immutable across regenerations

**Syntax**:
```rust
// FREEZE START: impl
// User modifications preserved
// FREEZE END: impl
```

**Use Case**: Preserve user customizations when regenerating templates

### 4. Backwards Compatibility Strategy

**Approach**: Support both `cmds/` (v1.2.0) and `commands/` (v2.0.0) during migration

**Timeline**:
- v2.0.0: Both active, deprecation warnings
- v2.1.0: Enhanced warnings, migration guide
- v2.2.0: Remove `cmds/`

---

## Module Structure

```
cli/src/
├── lib.rs               # Auto-discovery entry point
├── commands/            # CLI layer (thin wrappers, #[verb] attrs)
│   ├── utils/
│   │   └── doctor.rs   # #[verb] → calls domain::utils::doctor
│   ├── project/
│   │   ├── new.rs
│   │   └── gen.rs
│   └── market/
│       └── search.rs
│
└── domain/             # Business layer (pure logic, no clap!)
    ├── utils/
    │   └── doctor.rs   # Pure business logic
    ├── project/
    │   ├── new.rs
    │   └── gen.rs
    └── market/
        └── search.rs
```

---

## Migration Phases

### Phase 1: Foundation (Week 1)
- Create `domain/` directory structure
- Migrate `utils/doctor` as proof-of-concept
- Establish patterns for domain layer

### Phase 2: Core Commands (Week 2-3)
- Migrate `project` (new, gen, plan, apply)
- Migrate `market` (search, add, install)
- Implement frozen section parser

### Phase 3: Auto-Discovery (Week 4)
- Integrate clap-noun-verb v3.0.0
- Add #[verb] attributes
- Test filesystem routing

### Phase 4: Full Migration (Week 5-6)
- Migrate all remaining commands
- Add deprecation warnings
- Update documentation

### Phase 5: Enhanced Features (Week 7)
- Template frozen sections
- RDF schema support
- Streaming generation

### Phase 6: Cleanup (Week 8)
- Remove deprecated `cmds/`
- Final testing
- Release v2.0.0

---

## API Design Highlights

### Command Pattern

```rust
// cli/src/commands/project/new.rs
use clap::Args;
use clap_noun_verb::verb;

#[derive(Args, Debug)]
pub struct NewArgs {
    name: String,
    #[arg(short = 't', long)]
    project_type: String,
}

#[verb]
pub async fn run(args: &NewArgs) -> Result<()> {
    crate::domain::project::create_new_project(
        &args.name,
        &args.project_type
    ).await.map_err(Into::into)
}
```

### Domain Pattern

```rust
// cli/src/domain/project/mod.rs
// No clap imports!

pub async fn create_new_project(
    name: &str,
    project_type: &str,
) -> DomainResult<ProjectManifest> {
    // Pure business logic
    let project_name = ProjectName::new(name)?;
    let template = load_template(project_type).await?;
    let files = template.generate(&project_name)?;

    Ok(ProjectManifest { name: project_name.0, files })
}
```

### Frozen Section Parser

```rust
// ggen-core/src/template/frozen.rs
impl FrozenSectionParser {
    pub fn parse(&self, content: &str) -> Result<Vec<FrozenSection>> {
        // Extract // FREEZE START: marker ... // FREEZE END: marker
    }

    pub fn merge(
        &self,
        template_output: &str,
        frozen_markers: &[String],
    ) -> Result<String> {
        // Replace template sections with preserved user content
    }
}
```

---

## Quality Attributes

### Performance Targets

| Metric | Target |
|--------|--------|
| CLI startup | ≤100ms |
| Template generation | ≤1s |
| Frozen section merge | ≤200ms |
| Auto-discovery overhead | ≤50ms |
| Memory usage | ≤100MB |

### Testing Requirements

- **Domain layer**: 100% coverage for public APIs
- **Frozen sections**: All edge cases tested
- **Auto-discovery**: Route resolution verified
- **Integration**: Full command execution (600+ tests maintained)

### Security

- Input validation at domain layer
- Path traversal prevention (`../` rejected)
- Template injection prevention (escape user vars)
- SPARQL injection prevention (parameterized queries)

---

## Architecture Decision Records (ADRs)

### ADR-001: Use clap-noun-verb for Auto-Discovery
**Decision**: Adopt clap-noun-verb v3.0.0 for filesystem-based routing
**Consequences**: ✅ Easier to add commands, ⚠️ Migration complexity

### ADR-002: Separate Domain from CLI Layer
**Decision**: Create domain/ layer with zero CLI dependencies
**Consequences**: ✅ Testable in isolation, ⚠️ More boilerplate

### ADR-003: Frozen Sections in Templates
**Decision**: Implement frozen section markers in template engine
**Consequences**: ✅ User modifications preserved, ⚠️ Parser complexity

### ADR-004: Backwards Compatibility via Dual Systems
**Decision**: Support both cmds/ and commands/ during migration
**Consequences**: ✅ Gradual migration, ⚠️ Maintenance burden

---

## Next Steps for Hive Mind

### Immediate (Code Agent)
1. Implement domain layer foundation
   - Create `cli/src/domain/mod.rs`
   - Implement `DomainError` types
   - Migrate `utils/doctor` as proof-of-concept

### Week 1 (Test Agent)
1. Create domain layer test suite
   - Unit tests for doctor domain logic
   - Integration tests for commands/utils/doctor

### Week 1 (Documentation Agent)
1. Update architecture docs
   - Document domain layer patterns
   - Create migration guide

---

## Success Criteria

- ✅ Zero breaking changes for v1.2.0 users
- ✅ All tests passing (600+ tests)
- ✅ Performance maintained (≤3s generation)
- ✅ Documentation complete
- ✅ Migration guide published

---

## Coordination Data

**Stored in Hive Memory**:
- Key: `hive/architect/v2-design`
- File: `.claude/refactor-v2/03-v2-architecture-design.md`
- Task ID: `architecture-design`

**Dependencies**:
- Requires: Gap analysis (01-gap-analysis.md), clap-noun-verb patterns (02-clap-noun-verb-patterns.md)
- Enables: Code Agent (implementation), Test Agent (test suite), Doc Agent (migration guide)

---

**Architecture Design Complete** ✅

Ready for implementation by Code Agent.
