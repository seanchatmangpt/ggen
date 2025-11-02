# 🔍 PHASE 1: ANALYSIS BRIEF

**Duration**: 30 minutes
**Lead Agent**: code-analyzer
**Support**: system-architect, code-goal-planner
**Objective**: Identify exact 365 LOC changes that unlock 80% of v2.0 value

---

## 📋 ANALYSIS SCOPE

### 12 Subsystems to Analyze

Based on ggen v2.0 migration context, identify minimal changes for:

1. **Marketplace** (cli/src/domain/marketplace/)
   - install.rs, search.rs, list.rs, publish.rs, update.rs, mod.rs

2. **Project** (cli/src/domain/project/)
   - init.rs, build.rs, mod.rs

3. **Template** (cli/src/domain/template/)
   - Core template system

4. **AI** (cli/src/domain/ai/)
   - AI integration system

5. **Graph** (cli/src/domain/graph/)
   - Graph computation system

6. **Shell** (cli/src/domain/shell/)
   - Shell integration

7. **Utils** (cli/src/domain/utils/)
   - Utility functions

8. **Audit** (cli/src/domain/audit/)
   - Audit system

9. **CI** (cli/src/domain/ci/)
   - CI integration

10. **Commands Layer** (cli/src/commands/)
    - Command routing and execution

11. **Runtime** (cli/src/runtime.rs)
    - Runtime initialization and configuration

12. **Entry Point** (src/main.rs)
    - Application entry point

---

## 🎯 ANALYSIS OBJECTIVES

### Critical Questions to Answer:

1. **What are the exact 365 LOC changes needed?**
   - File-by-file breakdown
   - Line-by-line specifications
   - Dependency graph

2. **What is the 20% that unlocks 80% value?**
   - Core refactoring patterns
   - Critical path dependencies
   - High-leverage changes

3. **What risks exist?**
   - Breaking changes
   - Test failures
   - Performance regressions
   - Integration issues

4. **What is the implementation sequence?**
   - Dependency ordering
   - Parallel execution opportunities
   - Blocking relationships

---

## 📊 DELIVERABLES

### Required Outputs:

1. **Change Specification Document**
   - Exact LOC count per file
   - Before/after code snippets
   - Rationale for each change

2. **Dependency Graph**
   - Visual representation of change dependencies
   - Critical path identification
   - Parallelization opportunities

3. **Risk Assessment**
   - High/medium/low risk changes
   - Mitigation strategies
   - Rollback procedures

4. **Implementation Plan**
   - Sequenced execution steps
   - Agent assignments (backend-dev vs sparc-coder)
   - Time estimates

5. **Test Strategy**
   - Required test updates
   - New test cases
   - Validation criteria

---

## 🔍 ANALYSIS METHODOLOGY

### Step 1: Scan Existing Migration Docs (5 min)
- Read: `/Users/sac/ggen/docs/MIGRATION_V1_TO_V2.md`
- Read: `/Users/sac/ggen/.claude/refactor-v2/*.md`
- Extract: Prior agent findings and decisions

### Step 2: Code Structure Analysis (10 min)
- Map: Current file structure vs desired v2.0 structure
- Identify: Minimal refactoring paths
- Calculate: Exact LOC impact

### Step 3: Dependency Analysis (5 min)
- Build: Dependency graph between subsystems
- Identify: Critical path and parallelization opportunities
- Flag: Circular dependencies or blocking issues

### Step 4: Risk & Test Analysis (5 min)
- Review: Existing test suite
- Identify: Test updates required
- Assess: Risk level per change

### Step 5: Synthesis & Recommendations (5 min)
- Compile: Final change specification
- Recommend: Implementation sequence
- Flag: Decision points for Queen approval

---

## 🧠 MEMORY INTEGRATION

**Read from**:
- `project/context/architecture` - System architecture
- `project/context/constraints` - Constraints and requirements
- `agents/code-analyzer/prior-findings` - Historical analysis

**Write to**:
- `hive/analysis/changes` - Change specification
- `hive/analysis/risks` - Risk assessment
- `hive/analysis/dependencies` - Dependency graph
- `hive/analysis/implementation-plan` - Execution plan

---

## ✅ SUCCESS CRITERIA

- [ ] Exact 365 LOC change specification documented
- [ ] Dependency graph visualized and validated
- [ ] Risk assessment completed with mitigation strategies
- [ ] Implementation plan sequenced and approved by Queen
- [ ] Test strategy defined and resourced

---

## 🚨 ESCALATION TRIGGERS

**Escalate to Queen if**:
- LOC count exceeds 365 by >10%
- High-risk changes identified without clear mitigation
- Circular dependencies detected
- Implementation plan requires >2 hours

---

**Status**: READY FOR EXECUTION
**Next Phase**: Phase 2 - Implementation (after DP1 approval)
