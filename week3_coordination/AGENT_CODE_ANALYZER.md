# Code Analyzer Agent - Week 3 Mission

## Objective
Continuously track coverage metrics, health scores, and code quality throughout Week 3.

## Timeline
Continuous monitoring (Day 1-5, daily updates)

## Monitoring Responsibilities

### 1. Coverage Tracking

**Daily Tasks**:
```bash
# Generate coverage report
cargo tarpaulin --out Html --output-dir coverage/

# Extract coverage metrics
cargo tarpaulin --out Json | jq '.files[] | {name: .name, coverage: .coverage}'

# Track progress
echo "Day [N]: Coverage [X]% → [Y]%" >> week3_coordination/metrics/coverage_progress.log
```

**Metrics to Track**:
- Overall coverage: 53% → 60%+ target
- Module-specific coverage:
  - `graph/*`: Target 65%
  - `generator.rs`: Target 60%
  - `ontology/*`: Target 55%
  - `templates/*`: Target 60%
- Line coverage vs branch coverage
- Untested critical paths

**Output Format**:
```markdown
## Coverage Report - Day [N]

| Module | Day 0 | Current | Target | Status |
|--------|-------|---------|--------|--------|
| graph  | 48%   | 62%     | 65%    | ✅ On track |
| generator | 51% | 58%   | 60%    | ⚠️ Needs 2% |
| ontology | 45% | 54%    | 55%    | ✅ On track |
| templates | 50% | 59%   | 60%    | ⚠️ Needs 1% |

**Overall**: 53% → 58% (Target: 60%+)
```

### 2. Health Score Tracking

**Daily Tasks**:
```bash
# Run health checks
cargo clippy --all-targets --all-features -- -D warnings
cargo audit
cargo outdated

# Calculate health score
python3 scripts/calculate_health_score.py
```

**Health Metrics**:
- Test pass rate: Target 100%
- Code quality: Clippy warnings (target 0)
- Security: Audit vulnerabilities (target 0)
- Dependencies: Outdated crates (minimize)
- Documentation: Public API coverage

**Scoring Formula**:
```
Health Score = (
    test_pass_rate * 0.4 +
    code_quality * 0.3 +
    security * 0.2 +
    documentation * 0.1
) * 100
```

**Current Baseline**: 73%
**Week 3 Target**: 75%
**Final Target**: 95% (Week 16)

### 3. Code Quality Analysis

**Static Analysis**:
```bash
# Complexity analysis
cargo geiger  # Unsafe code detection
cargo bloat --release  # Binary size analysis
cargo tree --duplicate  # Dependency duplication
```

**Quality Checks**:
- Cyclomatic complexity: Flag functions >10
- Code duplication: Identify copy-paste issues
- Dead code: Find unused functions/modules
- API surface: Track public API growth

### 4. Performance Impact Monitoring

**Track Optimization Impact**:
```bash
# Baseline before optimization
hyperfine 'cargo run -- generate test-project' --warmup 3 --runs 10

# After each optimization
hyperfine 'cargo run -- generate test-project' --warmup 3 --runs 10

# Compare
echo "Optimization [N]: [before]ms → [after]ms ([improvement]%)" >> metrics/performance_log.txt
```

**Performance Metrics**:
- Build time: Track cargo build duration
- Test execution: Total test suite time
- Binary size: Release binary size
- Memory usage: Peak RSS during generation

## Coordination Protocol

### Daily Updates
```bash
# Morning: Capture baseline
npx claude-flow@alpha hooks pre-task --description "Daily metrics: Coverage tracking Day [N]"

# Evening: Report progress
npx claude-flow@alpha hooks notify --message "Day [N] metrics: Coverage [X]%, Health [Y]%"
npx claude-flow@alpha hooks post-edit --file "metrics/day_[N]_report.md" --memory-key "swarm/analyzer/day-[N]"
```

### Integration Points
- **Test Engineer**: Alert if coverage target falling behind
- **Backend Dev**: Validate no performance regressions
- **Performance Benchmarker**: Coordinate benchmark runs
- **Task Orchestrator**: Daily status summaries

## Output Deliverables

### Daily Deliverables
1. **Coverage Report**: `week3_coordination/metrics/coverage_day_[N].md`
2. **Health Score**: `week3_coordination/metrics/health_day_[N].md`
3. **Quality Analysis**: `week3_coordination/metrics/quality_day_[N].md`

### End of Week Deliverables
1. **Comprehensive Coverage Report**: Module breakdown, trends
2. **Health Score Progression**: 73% → 75% validation
3. **Quality Trends**: Improvements and regressions
4. **Performance Impact**: All optimization validations

## Alert Triggers

**Immediate alerts if**:
- Coverage drops below previous day
- Health score decreases
- New clippy warnings introduced
- Test pass rate <100%
- Performance regression detected

**Escalation Path**: Notify Task Orchestrator with details

## Success Criteria

- [ ] Daily coverage reports (5 reports)
- [ ] Coverage progression: 53% → 60%+
- [ ] Health score: 73% → 75%
- [ ] Zero new quality issues
- [ ] All optimizations validated
- [ ] Trend analysis documented
