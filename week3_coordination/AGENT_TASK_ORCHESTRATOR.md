# Task Orchestrator Agent - Week 3 Mission

## Objective
Coordinate 4 parallel agents, manage dependencies, track milestones, and ensure Week 3 success.

## Timeline
Continuous coordination (Day 1-5)

## Coordination Responsibilities

### 1. Agent Deployment & Initialization

**Day 0: Setup**
```bash
# Initialize swarm coordination
# Use Task tool:  swarm init --topology mesh --max-agents 4

# Spawn specialized agents
# Use Task tool:  agent spawn --type tester --name test-engineer
# Use Task tool:  agent spawn --type optimizer --name backend-dev
# Use Task tool:  agent spawn --type analyst --name code-analyzer
# Use Task tool:  agent spawn --type coordinator --name performance-benchmarker

# Verify all agents ready
# Use Task tool:  swarm status --verbose
```

### 2. Daily Standup Coordination

**Mock Daily Standup Pattern**:
```markdown
## Day [N] Standup

**Test Engineer** (@test-engineer):
- Yesterday: Added [X] tests for [module]
- Today: Working on [next module], targeting [Y] tests
- Blockers: [None/Issue description]
- Coverage: [Current]% (Target: [Target]%)

**Backend Developer** (@backend-dev):
- Yesterday: [Optimization progress]
- Today: [Next optimization phase]
- Blockers: [None/Issue description]
- Performance: [Metric] improved by [X]%

**Code Analyzer** (@code-analyzer):
- Coverage: [X]% → [Y]% (+[Z]%)
- Health Score: [X]% (Target: 75%)
- Alerts: [None/Issue description]
- Quality: [Metrics summary]

**Performance Benchmarker** (@performance-benchmarker):
- Validated: [Quick wins/optimizations]
- Results: [Performance improvements]
- Regressions: [None/Issue description]
- Grade: [Current grade]/100

**Dependencies**:
- Backend Dev waiting for Test Engineer to complete [module tests]
- Code Analyzer tracking Backend Dev optimization impact
- Performance Benchmarker ready to validate next optimization

**Decisions**:
- [Any blockers requiring immediate attention]
- [Priority adjustments]
- [Resource reallocation]
```

### 3. Milestone Tracking

**Day 1-2 Milestones**:
```bash
# Track Test Engineer progress
check_milestone "50+ graph tests created" "test-engineer"
check_milestone "All graph tests passing" "test-engineer"

# Track Backend Dev progress
check_milestone "Lockfile optimization designed" "backend-dev"
check_milestone "Lockfile optimization benchmarked" "backend-dev"

# Track Code Analyzer progress
check_milestone "Baseline metrics captured" "code-analyzer"
check_milestone "Day 1 coverage report generated" "code-analyzer"

# Track Performance Benchmarker progress
check_milestone "Quick wins validated" "performance-benchmarker"
check_milestone "No regressions detected" "performance-benchmarker"
```

**Day 3-4 Milestones**:
```bash
check_milestone "80+ generator tests created" "test-engineer"
check_milestone "Lockfile optimization completed" "backend-dev"
check_milestone "RDF caching designed and implemented" "backend-dev"
check_milestone "Day 3 coverage progress report" "code-analyzer"
check_milestone "Lockfile optimization validated" "performance-benchmarker"
```

**Day 5 Milestones**:
```bash
check_milestone "170+ ontology/template tests created" "test-engineer"
check_milestone "All 300+ tests passing" "test-engineer"
check_milestone "Template optimization completed" "backend-dev"
check_milestone "Final coverage report: 60%+" "code-analyzer"
check_milestone "A+ performance grade achieved" "performance-benchmarker"
```

### 4. Dependency Management

**Dependency Graph**:
```
Test Engineer (Day 1-2: Graph tests)
    ↓ (enables)
Backend Dev (Day 2-3: Lockfile optimization)
    ↓ (validates)
Performance Benchmarker (Day 2-3: Benchmark lockfile)
    ↓ (tracks)
Code Analyzer (Day 2-3: Coverage update)

[Parallel track]
Test Engineer (Day 3-4: Generator tests)
Backend Dev (Day 3-4: RDF caching)
Performance Benchmarker (Day 3-4: Validate RDF)
Code Analyzer (Day 3-4: Coverage tracking)

[Final sprint]
Test Engineer (Day 5: Ontology/template tests)
Backend Dev (Day 5: Template optimization)
Performance Benchmarker (Day 5: Final validation)
Code Analyzer (Day 5: Final report)
```

**Dependency Resolution**:
- Test Engineer blocks Backend Dev (need tests before optimization)
- Backend Dev blocks Performance Benchmarker (need code before benchmark)
- All agents feed into Code Analyzer (continuous tracking)

### 5. Blocker Management

**Blocker Escalation Protocol**:
```bash
# Detect blocker
if agent_reports_blocker; then
    # Log blocker
    echo "BLOCKER: [Agent] - [Issue]" >> week3_coordination/blockers.log

    # Assess impact
    assess_blocker_impact

    # Immediate resolution or escalate
    if critical_path_blocked; then
        notify_all_agents "CRITICAL: [Issue] blocking [milestone]"
        coordinate_resolution
    else
        assign_resolution "[agent]" "[blocker]"
    fi
fi
```

**Common Blocker Patterns**:
1. **Test Compilation Failure**
   - Impact: High (blocks Test Engineer)
   - Resolution: Immediate fix by Test Engineer
   - Fallback: Skip failing test, continue with passing tests

2. **Optimization Regression**
   - Impact: High (blocks Backend Dev)
   - Resolution: Revert optimization, try alternative approach
   - Fallback: Skip optimization, focus on other two

3. **Coverage Target Miss**
   - Impact: Medium (blocks coverage goal)
   - Resolution: Extend testing to critical gaps
   - Fallback: Accept 58-59% if critical tests covered

4. **Benchmark Inconsistency**
   - Impact: Medium (blocks validation)
   - Resolution: Re-run benchmarks, adjust warmup
   - Fallback: Use alternative measurement method

### 6. Progress Reporting

**Daily Progress Report Template**:
```markdown
# Week 3 Progress - Day [N]

## Overall Status
- Tests: [X]/300+ ([Y]% complete)
- Optimizations: [X]/3 ([Y]% complete)
- Coverage: [X]% (Target: 60%+)
- Health Score: [X]% (Target: 75%)
- Performance Grade: [X]/100 (Target: 95+)

## Agent Updates

### Test Engineer
- Completed: [X] tests for [modules]
- In Progress: [Current module]
- Blocked: [None/Issue]
- Next: [Tomorrow's target]

### Backend Developer
- Completed: [Optimizations]
- In Progress: [Current optimization]
- Improvements: [Metrics]
- Next: [Tomorrow's target]

### Code Analyzer
- Coverage: [X]% → [Y]% (+[Z]%)
- Health: [X]%
- Alerts: [Count] ([Details])
- Next: [Tomorrow's tracking]

### Performance Benchmarker
- Validated: [Quick wins/optimizations]
- Results: [Summary]
- Regressions: [None/Count]
- Next: [Tomorrow's benchmarks]

## Milestones
- ✅ [Completed milestone]
- ⏳ [In progress milestone]
- ⏸️ [Blocked milestone]
- ⏭️ [Upcoming milestone]

## Risks & Issues
- [Risk/issue description]
- [Mitigation plan]

## Tomorrow's Focus
- Test Engineer: [Target]
- Backend Dev: [Target]
- Code Analyzer: [Target]
- Performance Benchmarker: [Target]
```

### 7. Final Integration

**End of Week 3 Validation**:
```bash
# Aggregate all agent outputs
aggregate_test_results
aggregate_optimization_results
aggregate_coverage_results
aggregate_performance_results

# Validate success criteria
validate_criterion "300+ tests created" test_count
validate_criterion "60%+ coverage" coverage_percentage
validate_criterion "75% health score" health_score
validate_criterion "A+ performance grade" performance_grade

# Generate final report
generate_week3_status_dashboard

# Prepare Week 4 handoff
prepare_week4_context
```

## Coordination Protocol

### Daily Coordination Hooks
```bash
# Morning: Initialize daily coordination
# Use Task tool:  hooks pre-task --description "Day [N] coordination"

# Throughout day: Track agent updates
# Use Task tool:  hooks notify --message "[Agent] update: [Status]"

# Evening: Daily summary
# Use Task tool:  hooks post-task --task-id "coordinate-day-[N]"
# Use Task tool:  hooks session-end --export-metrics true
```

### Agent Communication
- Use swarm memory for agent-to-agent coordination
- Store daily progress in `.swarm/memory.db`
- Alert patterns: `[ALERT]`, `[BLOCKER]`, `[COMPLETE]`

## Success Criteria

- [ ] All 4 agents deployed and coordinated
- [ ] 5 daily standup reports generated
- [ ] All milestones tracked and achieved
- [ ] Zero critical blockers unresolved
- [ ] Final Week 3 dashboard complete
- [ ] Week 4 handoff prepared

## Output Deliverables

1. **Daily Standup Reports**: `week3_coordination/standup_day_[N].md`
2. **Blocker Log**: `week3_coordination/blockers.log`
3. **Milestone Tracker**: `week3_coordination/milestones.md`
4. **Final Dashboard**: `week3_coordination/WEEK3_STATUS_DASHBOARD.md`
5. **Week 4 Handoff**: `week3_coordination/WEEK4_HANDOFF.md`

## Escalation Triggers

**Immediate escalation if**:
- Any agent blocked >4 hours
- Critical milestone missed
- Coverage trending <60%
- Performance grade drops below A
- Health score decreases

**Escalation Path**: Notify user with detailed status and proposed resolution
