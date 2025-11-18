# Architecture Decision Record: Pack System Design

## ADR-001: Pack System Architecture

**Status**: Proposed
**Date**: 2025-11-17
**Deciders**: System Architecture Team
**Context**: Design of comprehensive packs system for ggen

---

## Decision 1: Packs as Pure Composition Layer

### Context

We need to decide whether packs should:
1. Implement their own template rendering engine
2. Extend/modify existing templates
3. Act as a pure composition layer orchestrating existing infrastructure

### Decision

**Chosen**: Packs are a **pure composition layer** with zero template rendering logic.

**Rationale**:
- **No Duplication**: Reuses ggen-core TemplateEngine and marketplace infrastructure
- **Maintainability**: Single source of truth for template logic
- **Compatibility**: Works with ALL existing marketplace templates without modification
- **Simplicity**: Packs only handle orchestration, not execution
- **Testing**: Leverage existing template/marketplace test suites

### Consequences

**Positive**:
- Smaller codebase (composition only)
- Faster development (no template engine reimplementation)
- Guaranteed compatibility with marketplace
- Reduced maintenance burden

**Negative**:
- Limited ability to modify template behavior
- Must work within constraints of existing engines
- Pack-specific optimizations harder to implement

**Mitigations**:
- Use hooks for pack-specific behavior
- Template configuration for customization
- SPARQL queries for semantic enhancements

---

## Decision 2: TOML vs YAML for Pack Manifests

### Context

Pack manifests need a human-readable, structured format. Options:
1. TOML (like Cargo.toml)
2. YAML (like docker-compose.yml)
3. JSON (machine-friendly but less readable)

### Decision

**Chosen**: **TOML** as primary format, with YAML support.

**Rationale**:
- **Rust Ecosystem**: Standard for Rust projects (Cargo.toml)
- **Type Safety**: Stricter than YAML, fewer ambiguities
- **Simplicity**: Easier to hand-edit than JSON
- **Tooling**: Excellent Rust library support (serde + toml)
- **Precedent**: ggen already uses TOML in some areas

**YAML Support**:
- Accept YAML manifests as alternative
- Convert to internal TOML representation
- Useful for users from Docker/Kubernetes ecosystems

### Consequences

**Positive**:
- Familiar to Rust developers
- Fewer parsing edge cases than YAML
- Strong type checking at parse time
- Good error messages

**Negative**:
- Less flexible than YAML
- No multi-line strings without escaping
- Steeper learning curve for non-Rust users

**Mitigations**:
- Comprehensive documentation with examples
- Interactive pack creation wizard
- YAML alternative for familiarity

---

## Decision 3: Local vs Cloud Registry

### Context

Where should packs be stored and distributed?
1. Local filesystem only (`~/.ggen/packs/`)
2. Cloud-hosted central registry
3. Both (local cache + remote registry)

### Decision

**Chosen**: **Local filesystem (MVP)**, with cloud registry in Phase 4.

**Rationale (MVP)**:
- **Simplicity**: No backend infrastructure needed
- **Offline Support**: Works without network
- **Speed**: No network latency
- **Privacy**: No data leaves user's machine
- **Development Velocity**: Faster initial implementation

**Future Cloud Registry**:
- CDN-backed distribution
- Versioning and checksums
- Community pack sharing
- Discovery and ratings

### Consequences

**Positive (MVP)**:
- Faster time to market
- Zero operational costs
- Complete offline functionality
- No authentication complexity

**Negative (MVP)**:
- No centralized pack discovery
- Manual pack distribution
- No automatic updates
- Limited community sharing

**Migration Path**:
1. MVP: Local registry only
2. Phase 2: Optional remote registry
3. Phase 3: Hybrid (local cache + remote)
4. Phase 4: Full cloud registry with CDN

---

## Decision 4: Dependency Resolution Strategy

### Context

How should pack dependencies be resolved when versions conflict?

Options:
1. **Pessimistic**: Fail on any version conflict
2. **Optimistic**: Use highest compatible version
3. **User-Driven**: Prompt user for resolution
4. **Policy-Based**: Configurable resolution strategy

### Decision

**Chosen**: **Optimistic with user override**.

**Rationale**:
- **Default Behavior**: Resolve to highest compatible version (semantic versioning)
- **Automatic**: Most cases resolved without user intervention
- **Override**: User can pin specific versions in composition.yaml
- **Precedent**: npm, Cargo, pip use similar strategies

**Resolution Algorithm**:
```
1. Collect all version constraints for each dependency
2. Find highest version satisfying all constraints
3. If no version satisfies all: Error with conflict details
4. If user pinned version: Use that (even if incompatible warning)
```

### Consequences

**Positive**:
- Works automatically for most cases
- Follows semantic versioning best practices
- User has ultimate control
- Matches Rust/npm ecosystem expectations

**Negative**:
- May silently upgrade dependencies
- Potential for subtle breakage with semver violations
- Complex resolution logic for large dependency trees

**Mitigations**:
- Lock file (pack.lock) to record exact versions used
- Warning on major version upgrades
- `pack tree` command to visualize dependencies
- Dry-run mode shows resolution before execution

---

## Decision 5: Conflict Resolution for Multi-Pack Composition

### Context

When composing multiple packs, file/variable conflicts will occur. How should these be resolved?

Options:
1. **Fail Fast**: Error on any conflict
2. **Last Wins**: Last pack in composition overwrites
3. **Priority-Based**: User assigns priorities
4. **Interactive**: Prompt user for each conflict
5. **Smart Merge**: Attempt automatic merging

### Decision

**Chosen**: **Configurable strategy with smart defaults**.

**Default Strategies**:
- **Overwrite**: For binary files, generated code
- **Merge**: For text files (README, documentation)
- **Ask**: For critical files (Dockerfile, config)
- **Priority**: User-defined pack priority

**Configuration**:
```toml
[conflict_resolution]
mode = "merge"  # Default strategy

[[conflict_resolution.rules]]
pattern = "*.md"
action = "merge"

[[conflict_resolution.rules]]
pattern = "Dockerfile"
action = "overwrite"
priority_pack = "devops-pack"

[[conflict_resolution.rules]]
pattern = "src/**/*.rs"
action = "ask"
```

### Consequences

**Positive**:
- Flexible: Supports multiple resolution strategies
- Safe Defaults: Ask for critical files
- Powerful: Fine-grained control with patterns
- User-Friendly: Interactive mode for conflicts

**Negative**:
- Complex implementation (pattern matching, merging)
- More configuration surface area
- Potential for user confusion

**Mitigations**:
- Comprehensive documentation
- `pack plan` shows conflicts before execution
- Dry-run mode previews resolutions
- Sensible defaults for 90% of cases

---

## Decision 6: SPARQL Query Integration

### Context

Should packs support RDF/SPARQL queries for semantic generation?

Options:
1. **No SPARQL**: Templates only
2. **Optional SPARQL**: Users can add queries
3. **Required SPARQL**: All packs must have queries

### Decision

**Chosen**: **Optional SPARQL with first-class support**.

**Rationale**:
- **Leverage Existing**: ggen already has render_with_rdf module
- **Differentiation**: Unique feature for semantic code generation
- **Flexibility**: Not all packs need semantic queries
- **Power**: Enables ontology-driven generation

**Implementation**:
- Queries defined in pack.toml
- Execute after template rendering
- Results written to files (JSON/YAML/etc)
- Failures are warnings (queries are optional)

### Consequences

**Positive**:
- Powerful semantic generation capabilities
- Reuses existing RDF infrastructure
- Differentiates ggen from other template systems
- Enables advanced use cases (schema generation, service discovery)

**Negative**:
- Complexity for users unfamiliar with SPARQL
- Dependency on RDF store availability
- Potential performance impact
- Learning curve

**Mitigations**:
- Queries are optional (can create packs without them)
- Comprehensive examples and documentation
- Skip queries with `--skip-queries` flag
- Fail gracefully if RDF store unavailable

---

## Decision 7: Hook System Design

### Context

Packs need extensibility for custom behavior (formatting, testing, initialization).

Options:
1. **No Hooks**: Pure declarative system
2. **Shell Commands**: Execute arbitrary commands
3. **Built-in Functions**: Predefined hooks only
4. **Plugin System**: Rust/WASM plugins

### Decision

**Chosen**: **Hybrid: Built-in functions + shell commands**.

**Rationale**:
- **Built-in**: Common operations (format, git-init, install-deps)
- **Shell Commands**: User flexibility for custom operations
- **Security**: No arbitrary code execution (shell sandboxed)
- **Simplicity**: No plugin compilation/loading complexity

**Hook Points**:
- `pre_generation`: Before any generation starts
- `post_generation`: After all generation completes
- `pre_template`: Before each template
- `post_template`: After each template
- `on_error`: On generation failure

### Consequences

**Positive**:
- Balance between flexibility and security
- Common operations built-in (no shell needed)
- Users can extend with custom commands
- No complex plugin system needed

**Negative**:
- Shell commands are platform-dependent
- Security risk (arbitrary command execution)
- Limited compared to full plugin system

**Mitigations**:
- Sandboxed shell execution (timeout, permissions)
- `continue_on_error` flag for resilience
- Documentation of security implications
- Validation of hook commands (warn on suspicious patterns)

---

## Decision 8: Validation and Quality Scoring

### Context

How should pack quality be measured and enforced?

Options:
1. **No Validation**: Accept all packs
2. **Strict Validation**: Reject invalid packs
3. **Scoring System**: Quality score like marketplace

### Decision

**Chosen**: **Multi-level validation with quality scoring**.

**Levels**:
1. **Syntax Validation**: Manifest must be valid TOML/YAML (blocking)
2. **Structure Validation**: Required fields present (blocking)
3. **Semantic Validation**: Templates exist, queries valid (blocking)
4. **Quality Scoring**: Documentation, examples, tests (non-blocking)

**Score Dimensions** (like marketplace_scorer):
- Metadata completeness (100 points)
- Template quality (100 points)
- Query quality (100 points)
- Variable definitions (100 points)
- Dependency management (100 points)
- Documentation (100 points)

**Total Score**: 600 points → Normalized to 0-100

### Consequences

**Positive**:
- Ensures minimum quality bar (blocking validation)
- Encourages best practices (quality score)
- Consistency with marketplace patterns
- Actionable feedback for pack authors

**Negative**:
- Additional complexity in validation logic
- Potential for false positives/negatives
- Scoring algorithm requires tuning

**Mitigations**:
- Clear documentation of validation rules
- `pack lint` for detailed feedback
- `--strict` flag for stricter validation
- Community feedback on scoring algorithm

---

## Decision 9: Performance Targets and Trade-offs

### Context

What performance targets should drive implementation decisions?

### Decision

**Chosen**: **User-facing operations < 100ms, generation < 30s**.

**Specific Targets**:
- `pack list`: < 50ms (cold cache)
- `pack search`: < 200ms
- `pack generate` (single): < 10s
- `pack compose` (3 packs): < 30s

**Trade-offs**:
- **Caching**: Aggressive caching for discovery operations
- **Parallelism**: Parallel template rendering (risk: resource contention)
- **Lazy Loading**: Load pack manifests on-demand (risk: surprising latency)
- **Incremental**: Incremental generation (risk: complexity)

### Consequences

**Positive**:
- Clear performance expectations
- Guides optimization efforts
- Measurable success criteria
- Competitive with other tools

**Negative**:
- Aggressive targets may require trade-offs
- Caching increases memory usage
- Parallelism adds complexity
- Need comprehensive benchmarking

**Mitigations**:
- Continuous benchmarking in CI/CD
- Performance regression detection
- Profiling and optimization iterations
- Graceful degradation strategies

---

## Decision 10: Versioning and Breaking Changes

### Context

How should packs handle versioning and breaking changes?

### Decision

**Chosen**: **Semantic versioning with changelog enforcement**.

**Rules**:
1. **Patch** (1.0.x): Bug fixes, no API changes
2. **Minor** (1.x.0): New features, backward compatible
3. **Major** (x.0.0): Breaking changes

**Breaking Changes**:
- Require CHANGELOG.md entry
- Warn users before upgrade
- Keep old versions available
- Deprecation policy (3 versions)

**Version Constraints**:
- `^1.2.0`: >= 1.2.0, < 2.0.0 (recommended)
- `~1.2.0`: >= 1.2.0, < 1.3.0
- `1.2.0`: Exact version
- `*`: Any version (discouraged)

### Consequences

**Positive**:
- Predictable upgrade behavior
- Clear communication of breaking changes
- Users can pin versions
- Follows Rust ecosystem standards

**Negative**:
- Requires pack authors to follow semver
- Enforcement mechanisms needed
- Versioning can be complex for large dependency trees

**Mitigations**:
- Automated semver checking (detect breaking changes)
- Changelog templates
- `pack diff` command to compare versions
- Documentation and examples

---

## Summary of Key Decisions

| Decision | Chosen Approach | Rationale |
|----------|----------------|-----------|
| Architecture | Pure composition layer | Reuse existing infrastructure |
| Manifest Format | TOML (+ YAML support) | Rust ecosystem standard |
| Registry | Local (MVP), cloud later | Simplicity, offline support |
| Dependency Resolution | Optimistic with overrides | Automatic with user control |
| Conflict Resolution | Configurable strategies | Flexibility + safe defaults |
| SPARQL Integration | Optional with first-class support | Leverage existing, differentiate |
| Hook System | Built-in + shell commands | Balance flexibility and security |
| Validation | Multi-level with scoring | Ensure quality, guide improvement |
| Performance | Sub-100ms discovery, sub-30s generation | User experience priority |
| Versioning | Semantic with changelog | Predictability and communication |

---

## Open Questions

1. **Cloud Registry Timeline**: When should we prioritize cloud registry?
   - Recommendation: After MVP user feedback (6 months)

2. **Plugin System**: Should we add plugin system in future?
   - Recommendation: Monitor user requests, consider WASM plugins

3. **Pack Namespacing**: How to handle name collisions?
   - Recommendation: Namespace by author (e.g., `@author/pack-name`)

4. **Commercial Packs**: Should we support paid packs?
   - Recommendation: Not in MVP, consider for Phase 4

5. **Pack Signing**: Should packs be cryptographically signed?
   - Recommendation: Yes for cloud registry, checksum only for local

---

## Review and Update Process

This ADR should be reviewed:
- **Quarterly**: Ensure decisions still valid
- **After MVP**: Based on user feedback
- **Before Major Features**: Validate alignment with decisions

**Next Review Date**: 2026-02-17

---

## References

- [ggen marketplace architecture](../marketplace/)
- [ggen-core template engine](../../crates/ggen-core/)
- [render_with_rdf module](../../crates/ggen-domain/src/template/render_with_rdf/)
- [Cargo manifest format](https://doc.rust-lang.org/cargo/reference/manifest.html)
- [Semantic Versioning 2.0.0](https://semver.org/)

---

## Approval

**Architecture Review**: Pending
**Technical Lead Approval**: Pending
**Implementation Start**: After approval

---

## Appendix: Alternative Approaches Considered

### A1: Template Inheritance vs Composition

**Rejected Approach**: Template inheritance (packs extend templates)

**Why Rejected**:
- More complex implementation
- Tight coupling between packs and templates
- Harder to maintain
- Breaks marketplace template independence

**Chosen**: Composition (packs orchestrate templates)

---

### A2: GraphQL vs SPARQL

**Rejected Approach**: Use GraphQL instead of SPARQL

**Why Rejected**:
- ggen already has SPARQL infrastructure
- GraphQL doesn't support RDF/ontologies
- SPARQL is more powerful for semantic queries
- Would require new infrastructure

**Chosen**: SPARQL (leverage existing)

---

### A3: Monolithic vs Microkernel

**Rejected Approach**: Monolithic pack system (all features in core)

**Why Rejected**:
- Less flexible for users
- Harder to extend
- Larger codebase
- Testing complexity

**Chosen**: Microkernel (core + extensions via hooks)

---

## Document History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2025-11-17 | System Architect | Initial ADR |

---

**Status**: ✅ Proposed
**Next Action**: Submit for architecture review
