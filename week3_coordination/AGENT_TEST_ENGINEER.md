# Test Engineer Agent - Week 3 Mission

## Objective
Add 300+ comprehensive tests across core ggen systems to achieve 60%+ coverage.

## Timeline
5 days (Day 1-5)

## Module Breakdown

### Day 1-2: Graph Module (50+ tests)
**Location**: `crates/ggen-core/src/graph/`

**Critical Test Areas**:
1. **Core Operations** (`core.rs`)
   - Node creation, updates, deletion
   - Edge management and traversal
   - Graph validation and integrity
   - Concurrent access patterns

2. **Query System** (`query.rs`)
   - Path finding algorithms
   - Node filtering and selection
   - Complex graph queries
   - Performance edge cases

3. **Storage** (`store.rs`)
   - Persistence operations
   - Transaction handling
   - Recovery scenarios
   - Data integrity

4. **Export** (`export.rs`)
   - Format conversions
   - Large graph handling
   - Error recovery
   - Stream processing

**Test Structure**:
```rust
#[cfg(test)]
mod graph_core_tests {
    use super::*;

    #[test]
    fn test_node_creation() { /* ... */ }

    #[test]
    fn test_concurrent_updates() { /* ... */ }

    #[test]
    fn test_graph_integrity() { /* ... */ }
}
```

### Day 3-4: Generator Module (80+ tests)
**Location**: `crates/ggen-core/src/generator.rs`, `crates/ggen-ai/src/generators/`

**Critical Test Areas**:
1. **Code Generation**
   - Template rendering
   - Variable substitution
   - Conditional logic
   - Loop handling

2. **AI Integration** (ontology.rs)
   - Prompt construction
   - Response parsing
   - Error handling
   - Rate limiting

3. **Streaming** (`streaming_generator.rs`)
   - Chunk processing
   - Buffer management
   - Error recovery
   - Performance

4. **Project Generators**
   - Rust projects
   - Next.js projects
   - Common utilities

### Day 5: Ontology + Templates (170+ tests)
**Location**: `crates/ggen-core/src/ontology/`, `crates/ggen-core/src/templates/`

**Ontology Tests** (85+ tests):
1. Constitution validation
2. Control loop processing
3. Policy enforcement
4. Schema validation

**Template Tests** (85+ tests):
1. File tree generation
2. Context management
3. Format handling
4. Business logic

## Testing Strategy

### 80/20 Rule Application
- Focus on critical paths (20% code, 80% value)
- Skip trivial getters/setters
- Prioritize error handling and edge cases
- Cover integration points thoroughly

### Test Categories per Module
1. **Unit Tests** (60%): Core functionality
2. **Integration Tests** (25%): Module interactions
3. **Performance Tests** (10%): Scalability scenarios
4. **Security Tests** (5%): Input validation, injection prevention

### Quality Standards
- 100% pass rate (no failing tests)
- Clear test names describing scenario
- Minimal setup/teardown
- Fast execution (<2s total per module)
- No flaky tests

## Coordination Protocol

### Before Starting Each Module
```bash
npx claude-flow@alpha hooks pre-task --description "Testing [module]: [focus area]"
```

### After Completing Each Module
```bash
npx claude-flow@alpha hooks post-task --task-id "test-[module]"
npx claude-flow@alpha hooks notify --message "Completed [X] tests for [module], [Y] passing"
```

### Daily Progress Updates
```bash
npx claude-flow@alpha hooks post-edit --file "[test file]" --memory-key "swarm/test-engineer/day-[N]"
```

## Success Criteria

- [ ] 300+ tests created across all modules
- [ ] 100% pass rate (all tests green)
- [ ] Coverage: 53% → 60%+ on critical modules
- [ ] No regressions in existing tests
- [ ] Test execution time <10s total
- [ ] All tests documented with clear purpose

## Output Deliverables

1. **Test Files**: `crates/ggen-core/src/[module]/tests/`
2. **Coverage Report**: Module-by-module breakdown
3. **Test Summary**: Count, pass rate, execution time
4. **Blockers Log**: Any issues encountered and resolutions

## Escalation Triggers

**Immediate escalation if**:
- Test fails to compile
- Existing tests break
- Coverage target unreachable
- Module API unclear/undocumented

**Contact**: Task Orchestrator agent for blockers
