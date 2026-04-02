# Backend Developer Agent - Week 3 Mission

## Objective
Implement 3 medium-effort optimizations to achieve 50-80% performance improvements and A+ grade.

## Timeline
3-4 days (Day 1-4)

## Optimization Targets

### Optimization 1: Lockfile Resolution (Day 1-2)
**File**: `crates/ggen-core/src/lockfile.rs`
**Current Issue**: O(n²) dependency resolution
**Target**: 60-70% improvement

**Implementation Plan**:
1. **Profile Current Performance**
   ```bash
   cargo flamegraph --bin ggen -- resolve large-project
   ```

2. **Design Improvement**
   - Replace nested loops with HashMap lookups
   - Implement dependency graph with topological sort
   - Cache resolved dependencies

3. **Implementation**
   ```rust
   // BEFORE: O(n²)
   for dep in dependencies {
       for resolved in all_resolved {
           if dep.matches(resolved) { /* ... */ }
       }
   }

   // AFTER: O(n)
   let resolved_map: HashMap<String, ResolvedDep> = /* ... */;
   for dep in dependencies {
       if let Some(resolved) = resolved_map.get(&dep.name) {
           /* ... */
       }
   }
   ```

4. **Validation**
   - Benchmark before/after with 100+ dependencies
   - Verify correctness with existing test suite
   - Ensure no breaking changes

**Success Metric**: 60%+ reduction in resolution time

### Optimization 2: RDF Caching (Day 2-3)
**Files**: `crates/ggen-core/src/rdf/*.rs`
**Current Issue**: Repeated parsing of schemas
**Target**: 70-80% improvement

**Implementation Plan**:
1. **Profile RDF Operations**
   ```bash
   cargo bench --bench rdf_parsing
   ```

2. **Design Cache Layer**
   - LRU cache for parsed schemas (max 100 entries)
   - Content-based cache invalidation (hash comparison)
   - Thread-safe access with RwLock

3. **Implementation**
   ```rust
   use lru::LruCache;
   use std::sync::RwLock;

   pub struct RdfCache {
       schemas: RwLock<LruCache<String, ParsedSchema>>,
       max_size: usize,
   }

   impl RdfCache {
       pub fn get_or_parse(&self, uri: &str) -> Result<ParsedSchema> {
           // Check cache first
           if let Some(cached) = self.schemas.read().unwrap().peek(uri) {
               return Ok(cached.clone());
           }

           // Parse and cache
           let schema = parse_schema(uri)?;
           self.schemas.write().unwrap().put(uri.to_string(), schema.clone());
           Ok(schema)
       }
   }
   ```

4. **Validation**
   - Benchmark repeated schema access
   - Verify cache hit rates >80%
   - Test memory usage with large projects

**Success Metric**: 70%+ reduction in RDF parsing time

### Optimization 3: Template Processing (Day 3-4)
**Files**: `crates/ggen-core/src/templates/*.rs`
**Current Issue**: Inefficient template compilation
**Target**: 50-60% improvement

**Implementation Plan**:
1. **Profile Template System**
   ```bash
   cargo bench --bench template_rendering
   ```

2. **Design Optimization**
   - Pre-compile templates on load (one-time cost)
   - Reuse compiled templates across generations
   - Optimize variable substitution with string builder

3. **Implementation**
   ```rust
   pub struct TemplateCache {
       compiled: HashMap<PathBuf, CompiledTemplate>,
   }

   impl TemplateCache {
       pub fn compile_and_cache(&mut self, path: &Path) -> Result<&CompiledTemplate> {
           if !self.compiled.contains_key(path) {
               let template = compile_template(path)?;
               self.compiled.insert(path.to_path_buf(), template);
           }
           Ok(&self.compiled[path])
       }
   }
   ```

4. **Validation**
   - Benchmark template rendering with 50+ variables
   - Test correctness with existing templates
   - Measure memory overhead

**Success Metric**: 50%+ reduction in template processing time

## Coordination Protocol

### Before Starting Each Optimization
```bash
# Native:  hooks pre-task --description "Optimizing [system]: [approach]"
# Native:  hooks session-restore --session-id "swarm-week3"
```

### During Implementation
```bash
# After significant changes
# Native:  hooks post-edit --file "[optimized file]" --memory-key "swarm/backend-dev/optimization-[N]"

# Performance validation
# Native:  hooks notify --message "Optimization [N]: [X]% improvement achieved"
```

### After Completing Each Optimization
```bash
# Native:  hooks post-task --task-id "optimize-[system]"
```

## Quality Standards

- No breaking changes to public APIs
- All existing tests pass
- Performance improvements validated with benchmarks
- Code follows Rust best practices
- Documentation updated for optimized code paths

## Success Criteria

- [ ] 3 optimizations implemented
- [ ] 50-80% performance improvements validated
- [ ] All tests passing (100% pass rate)
- [ ] Performance grade: A+ (95+/100)
- [ ] No regressions in functionality
- [ ] Benchmarks documented

## Output Deliverables

1. **Optimized Code**: Updated source files with improvements
2. **Benchmarks**: Before/after performance data
3. **Documentation**: Optimization rationale and approach
4. **Validation Report**: Test results and performance metrics

## Escalation Triggers

**Immediate escalation if**:
- Optimization causes test failures
- Performance regresses instead of improves
- Breaking changes unavoidable
- Implementation complexity exceeds estimate

**Contact**: Performance Benchmarker for validation issues
