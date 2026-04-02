# Week 3 Coordination - Agent Execution Guide

## Quick Start

This directory contains the complete coordination framework for Week 3 parallel agent execution.

## Agent Mission Documents

Each agent has a dedicated mission document with detailed responsibilities:

1. **[AGENT_TEST_ENGINEER.md](./AGENT_TEST_ENGINEER.md)** - Add 300+ core system tests
2. **[AGENT_BACKEND_DEV.md](./AGENT_BACKEND_DEV.md)** - Implement 3 performance optimizations
3. **[AGENT_CODE_ANALYZER.md](./AGENT_CODE_ANALYZER.md)** - Track coverage and health metrics
4. **[AGENT_PERFORMANCE_BENCHMARKER.md](./AGENT_PERFORMANCE_BENCHMARKER.md)** - Validate improvements
5. **[AGENT_TASK_ORCHESTRATOR.md](./AGENT_TASK_ORCHESTRATOR.md)** - Coordinate team execution

## Status Dashboard

**[WEEK3_STATUS_DASHBOARD.md](./WEEK3_STATUS_DASHBOARD.md)** - Real-time progress tracking for all agents and milestones.

## Directory Structure

```
week3_coordination/
├── README.md                           # This file
├── WEEK3_STATUS_DASHBOARD.md          # Central status tracking
├── AGENT_TEST_ENGINEER.md             # Test engineer mission
├── AGENT_BACKEND_DEV.md               # Backend developer mission
├── AGENT_CODE_ANALYZER.md             # Code analyzer mission
├── AGENT_PERFORMANCE_BENCHMARKER.md   # Performance benchmarker mission
├── AGENT_TASK_ORCHESTRATOR.md         # Task orchestrator mission
├── tests/                             # Test deliverables
├── optimizations/                     # Optimization deliverables
├── metrics/                           # Coverage and health reports
└── benchmarks/                        # Performance validation results
```

## Execution Model

### Parallel Agent Deployment

All agents execute concurrently with defined coordination points:

```
Day 1-2: Foundation Phase
┌─────────────────┐  ┌──────────────────┐  ┌────────────────┐  ┌──────────────────────┐
│ Test Engineer   │  │ Backend Dev      │  │ Code Analyzer  │  │ Performance          │
│ Add graph tests │  │ Design lockfile  │  │ Track coverage │  │ Validate quick wins  │
│ (50+ tests)     │  │ optimization     │  │ Daily reports  │  │ Benchmark lockfile   │
└─────────────────┘  └──────────────────┘  └────────────────┘  └──────────────────────┘

Day 3-4: Acceleration Phase
┌─────────────────┐  ┌──────────────────┐  ┌────────────────┐  ┌──────────────────────┐
│ Test Engineer   │  │ Backend Dev      │  │ Code Analyzer  │  │ Performance          │
│ Add generator   │  │ Implement RDF    │  │ Progress       │  │ Benchmark RDF        │
│ tests (80+)     │  │ caching          │  │ tracking       │  │ caching              │
└─────────────────┘  └──────────────────┘  └────────────────┘  └──────────────────────┘

Day 5: Final Sprint
┌─────────────────┐  ┌──────────────────┐  ┌────────────────┐  ┌──────────────────────┐
│ Test Engineer   │  │ Backend Dev      │  │ Code Analyzer  │  │ Performance          │
│ Add ontology +  │  │ Optimize         │  │ Final report   │  │ A+ grade validation  │
│ template (170+) │  │ templates        │  │ 60%+ coverage  │  │ (95+/100)            │
└─────────────────┘  └──────────────────┘  └────────────────┘  └──────────────────────┘
```

## Coordination Protocol

### Daily Standup (Mock Pattern)

Each agent reports:
- **Yesterday**: What was completed
- **Today**: Current focus
- **Blockers**: Any impediments
- **Metrics**: Quantifiable progress

Task Orchestrator synthesizes reports and manages dependencies.

### Hooks Integration

Use Claude Code Task tool and TodoWrite for coordination. Track progress in `.claude/memory/MEMORY.md`.

## Success Criteria

All agents must achieve their targets:

- ✅ **Test Engineer**: 300+ tests, 100% pass rate, 60% coverage
- ✅ **Backend Dev**: 3 optimizations, 50-80% improvements
- ✅ **Code Analyzer**: Coverage tracking, health 73% → 75%
- ✅ **Performance Benchmarker**: All validations, A+ grade

## Monitoring

### Real-Time Status

Check **WEEK3_STATUS_DASHBOARD.md** for:
- Agent status and blockers
- Milestone progress
- Coverage progression
- Performance metrics
- Risk tracking

### Daily Updates

Task Orchestrator generates daily reports in `week3_coordination/`:
- `standup_day_N.md` - Daily standup summaries
- `metrics/coverage_day_N.md` - Coverage reports
- `metrics/health_day_N.md` - Health score tracking
- `benchmarks/day_N_results.md` - Performance validations

## Blocker Management

**Escalation triggers**:
- Test compilation failures
- Optimization regressions
- Coverage target misses
- Benchmark inconsistencies

**Resolution protocol**:
1. Log blocker in `blockers.log`
2. Assess critical path impact
3. Immediate resolution or escalate
4. Notify affected agents

## Deliverables

### Tests
- `tests/graph_tests.rs` - Graph module tests (50+)
- `tests/generator_tests.rs` - Generator tests (80+)
- `tests/ontology_tests.rs` - Ontology tests (85+)
- `tests/template_tests.rs` - Template tests (85+)

### Optimizations
- `optimizations/lockfile_optimization.md` - Lockfile improvement
- `optimizations/rdf_caching.md` - RDF caching implementation
- `optimizations/template_processing.md` - Template optimization

### Reports
- `metrics/final_coverage_report.md` - Complete coverage analysis
- `metrics/health_score_report.md` - Health progression
- `benchmarks/performance_report.md` - All benchmark results
- `WEEK3_STATUS_DASHBOARD.md` - Final comprehensive report

## Week 4 Handoff

Upon successful completion:
1. Validate all success criteria
2. Generate Week 4 handoff document
3. Prepare context for specialized testing phase
4. Archive Week 3 deliverables

---

**Coordination Start**: Day 0 - 2025-11-18
**Execution Model**: Fully parallel with daily synchronization
**Target Completion**: Day 5 - All Week 3 objectives achieved
