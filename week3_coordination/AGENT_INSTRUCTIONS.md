# Week 3 Agent Execution Instructions

## FOR ALL AGENTS: Read This First

This document contains the execution protocol for Week 3 parallel agent coordination.

### Your Role

Each agent has been assigned a specific mission document:
- **Test Engineer**: See `AGENT_TEST_ENGINEER.md`
- **Backend Developer**: See `AGENT_BACKEND_DEV.md`
- **Code Analyzer**: See `AGENT_CODE_ANALYZER.md`
- **Performance Benchmarker**: See `AGENT_PERFORMANCE_BENCHMARKER.md`
- **Task Orchestrator**: See `AGENT_TASK_ORCHESTRATOR.md`

### Execution Protocol

1. **Read your mission document thoroughly**
2. **Follow the timeline** (Day 1-5 schedule)
3. **Use coordination hooks** (pre-task, post-edit, notify, post-task)
4. **Report progress daily** via Task Orchestrator
5. **Escalate blockers immediately** (don't wait)

### Coordination Hooks

**Before starting work**:
```bash
npx claude-flow@alpha hooks pre-task --description "[your task]"
npx claude-flow@alpha hooks session-restore --session-id "swarm-week3"
```

**During work** (after significant changes):
```bash
npx claude-flow@alpha hooks post-edit --file "[changed file]" --memory-key "swarm/[your-agent]/[step]"
npx claude-flow@alpha hooks notify --message "[status update]"
```

**After completing work**:
```bash
npx claude-flow@alpha hooks post-task --task-id "[your-task-id]"
npx claude-flow@alpha hooks session-end --export-metrics true
```

### Daily Standup Format

Report to Task Orchestrator daily:

**Template**:
```
Agent: [Your Agent Name]
Yesterday: [What you completed]
Today: [What you're working on]
Blockers: [None or describe issue]
Metrics: [Quantifiable progress]
```

### Success Criteria

**Your individual criteria**:
- See your mission document for specific targets
- All work must pass quality standards
- Coordinate with other agents via hooks
- Report progress transparently

**Overall Week 3 criteria**:
- 300+ tests added (Test Engineer)
- 60%+ coverage (Code Analyzer validates)
- 3 optimizations complete (Backend Dev)
- A+ grade (Performance Benchmarker validates)
- 75% health score (Code Analyzer validates)

### Blocker Escalation

**If you encounter a blocker**:
1. Log it immediately: `echo "BLOCKER: [description]" >> week3_coordination/blockers.log`
2. Notify Task Orchestrator: `npx claude-flow@alpha hooks notify --message "[BLOCKER] [description]"`
3. Assess impact: Can you work around it or is it critical path?
4. If critical: Wait for resolution. If non-critical: Continue other work.

### Quality Standards

**All agents must**:
- Maintain 100% test pass rate
- Follow Rust best practices
- Document all changes
- Use 80/20 rule (focus critical paths)
- No breaking changes to public APIs

### Deliverables

**Where to save your work**:
- Tests: `week3_coordination/tests/`
- Optimizations: `week3_coordination/optimizations/`
- Metrics: `week3_coordination/metrics/`
- Benchmarks: `week3_coordination/benchmarks/`

**File naming convention**:
- Use descriptive names: `graph_core_tests.rs`, not `tests.rs`
- Include agent identifier: `lockfile_optimization_backend_dev.md`
- Add dates to reports: `coverage_day_1_2025-11-19.md`

### Communication

**Agent-to-Agent**:
- Use swarm memory: Store context in `.swarm/memory.db`
- Use notify hooks: Real-time updates to all agents
- Check dashboard: `WEEK3_STATUS_DASHBOARD.md` for status

**Agent-to-User**:
- All communication goes through Task Orchestrator
- Don't escalate unless critical blocker
- Trust the coordination framework

### Progress Tracking

**Update dashboard regularly**:
- Task Orchestrator updates `WEEK3_STATUS_DASHBOARD.md` daily
- Check your section for current status
- Validate milestones as completed

### Tools Available

**Development**:
- `cargo test`, `cargo bench`, `cargo build`
- `cargo tarpaulin` (coverage)
- `cargo clippy` (quality)
- `hyperfine`, `flamegraph` (profiling)

**Coordination**:
- `npx claude-flow@alpha hooks [command]`
- `npx claude-flow@alpha swarm status`
- `npx claude-flow@alpha agent list`

### Timeline Awareness

**Current phase**: Day 0 (Initialization complete)
**Next phase**: Day 1 (Start execution)

**Day 1-2**: Foundation
**Day 3-4**: Acceleration  
**Day 5**: Final sprint

Stay on schedule. If falling behind, notify Task Orchestrator immediately.

### Week 4 Handoff

**After Week 3 completion**:
- Validate your success criteria
- Contribute to final report
- Document lessons learned
- Prepare context for Week 4

---

**Ready to Execute**

All agents are deployed and ready. Begin Day 1 work as per your mission document.

**Questions?** Check your mission document first, then escalate to Task Orchestrator.

**Blockers?** Log and notify immediately via hooks.

**Success?** Report progress daily via standup format.

---

_Let's achieve Week 3 objectives together through coordinated parallel execution!_
