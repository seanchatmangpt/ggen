# GGEN "SHOULD DO" Documentation Index

**Purpose**: Intent-driven documentation describing what files and functions SHOULD DO (not what they currently do), to guide refactoring and maintain architectural integrity.

**Last Updated**: 2025-10-10

---

## 📚 What is "SHOULD DO" Documentation?

This documentation describes the **intended behavior and design goals** of code, not the current implementation. It answers:

- **What SHOULD this component do?** (Purpose and responsibilities)
- **How SHOULD it behave?** (Contracts and guarantees)
- **What SHOULD it NOT do?** (Anti-patterns and boundaries)
- **What SHOULD it become?** (Future evolution intent)

This approach separates **intent** from **implementation**, making refactoring safer and more effective.

---

## 🎯 Core Philosophy

> "Document what the code SHOULD DO so refactoring can make it actually DO it."

### Benefits

1. **Refactoring Safety**: Clear intent prevents breaking changes
2. **Design Clarity**: Makes architectural decisions explicit
3. **Onboarding Speed**: New developers understand goals, not just implementation
4. **Technical Debt Tracking**: Gap between SHOULD and IS reveals tech debt
5. **Future-Proofing**: Evolution intent guides enhancement decisions

---

## 📋 Documentation Structure

Each SHOULD DO document follows this template:

1. **🎯 Core Purpose**: High-level intent statement
2. **🏗️ Architectural Intent**: Design principles and patterns
3. **📋 Component Contracts**: What each part should do
4. **🔄 Workflow Contracts**: Expected behavior flows
5. **🧪 Testing Contract**: How to verify correctness
6. **📊 Performance Contract**: Expected metrics and SLAs
7. **🔒 Security/Safety Contract**: Guarantees and boundaries
8. **🚀 Future Evolution Intent**: Planned enhancements by phase
9. **🎯 Success Criteria**: Measurable outcomes
10. **🔧 Refactoring Guidance**: What to preserve vs improve

---

## 📖 Available Documentation

### Agents Module

#### [SwarmAgent](./SHOULD_DO_swarm_agent.md)
**File**: `agents/src/agents/swarm_agent.rs`
**Purpose**: Autonomous coordination for self-generating workflows
**Key Contracts**:
- 6 specialized agent types (AutonomousCoordinator, TriggerMonitor, etc.)
- 5 trigger-driven workflows (RequirementsChange, RuntimeTelemetry, ApiChange, SecurityVulnerability, PerformanceRegression)
- Multi-step execution (analyze → extend → query → regenerate)
- Knowledge graph integration
- AI-powered decision making

**Use When**:
- Adding new agent types
- Implementing new trigger handlers
- Refactoring workflow orchestration
- Adding validation or error handling

---

### GGen-AI Autonomous Module

#### [RegenerationOrchestrator](./SHOULD_DO_orchestrator.md)
**File**: `ggen-ai/src/autonomous/orchestrator.rs`
**Purpose**: Machine-timescale orchestration with parallel execution
**Key Contracts**:
- <30 second target cycle times
- Parallel event processing (up to num_cpus workers)
- Adaptive performance optimization
- Health monitoring and telemetry
- Lifecycle management (start/stop/running)

**Use When**:
- Tuning performance parameters
- Adding optimization strategies
- Implementing health checks
- Refactoring cycle execution

---

#### [RegenerationEngine](./SHOULD_DO_regeneration.md)
**File**: `ggen-ai/src/autonomous/regeneration.rs`
**Purpose**: Core regeneration engine with delta-driven template regeneration
**Key Contracts**:
- Event-driven change processing
- Dependency graph tracking (bidirectional)
- Incremental, parallel regeneration
- Multi-language code generation
- Artifact version management

**Use When**:
- Adding new target languages
- Implementing dependency tracking
- Optimizing regeneration performance
- Adding caching or incremental builds

---

### GGen-AI Test Utilities

#### [Test Helpers](./SHOULD_DO_test_helpers.md)
**File**: `ggen-ai/src/test_helpers.rs`
**Purpose**: Test utilities for Ollama integration testing
**Key Contracts**:
- Automatic Ollama availability checking (<10s)
- Graceful test skipping when Ollama unavailable
- Consistent test configuration (qwen3-coder:30b, 0.1 temp)
- Simple client creation API

**Use When**:
- Writing new integration tests
- Adding support for different models
- Implementing test mocking/recording
- Setting up CI/CD test environments

---

## 🔍 How to Use This Documentation

### For Developers

**Before Writing Code**:
1. Read relevant SHOULD DO documentation
2. Understand the intent and contracts
3. Design implementation to meet contracts
4. Verify against success criteria

**During Refactoring**:
1. Compare current code to SHOULD DO spec
2. Identify gaps (tech debt)
3. Prioritize fixes based on contract violations
4. Preserve key behaviors listed in "Refactoring Guidance"
5. Improve areas listed in "Improve these areas"

**After Implementation**:
1. Verify code meets SHOULD DO contracts
2. Update SHOULD DO doc if requirements changed
3. Add tests verifying contract compliance

---

### For Code Reviewers

**Review Checklist**:
- [ ] Does code match SHOULD DO intent?
- [ ] Are all contracts satisfied?
- [ ] Are anti-patterns avoided (SHOULD NOT)?
- [ ] Does code follow design principles?
- [ ] Are success criteria achievable?
- [ ] Is future evolution path preserved?

---

### For Architects

**Architecture Review**:
1. Verify SHOULD DO docs reflect current architecture decisions
2. Ensure contracts are consistent across modules
3. Check that evolution phases are realistic and aligned
4. Validate that success criteria are measurable
5. Ensure design principles are followed

**Evolution Planning**:
1. Review "Future Evolution Intent" sections
2. Prioritize phases based on business needs
3. Ensure backward compatibility is maintained
4. Plan migration paths for breaking changes

---

## 📊 Gap Analysis

Use this framework to identify technical debt:

### Contract Violations

**Check**: Does current code violate any SHOULD/SHOULD NOT contracts?

**Examples**:
- Function exceeds 50-line limit → Violates code quality contract
- Missing error handling → Violates error handling contract
- Blocking I/O in async function → Violates concurrency contract

**Priority**: HIGH (breaks architectural intent)

---

### Missing Features

**Check**: Are any contracted features not implemented?

**Examples**:
- No validation after template extraction → Missing validation contract
- No pattern persistence → Missing learning capability
- No progress reporting → Missing observability contract

**Priority**: MEDIUM-HIGH (incomplete implementation)

---

### Performance Gaps

**Check**: Does code meet performance contracts?

**Examples**:
- Cycle time >30s target → Violates performance contract
- Sequential processing where parallel expected → Inefficient
- No caching where specified → Missing optimization

**Priority**: MEDIUM (affects user experience)

---

### Evolution Readiness

**Check**: Is code ready for next evolution phase?

**Examples**:
- Hard-coded values block parameterization → Blocks Phase 2
- No hooks for extensibility → Blocks Phase 3
- Tight coupling prevents distribution → Blocks Phase 4

**Priority**: LOW-MEDIUM (future-proofing)

---

## 🛠️ Maintenance

### Updating SHOULD DO Documentation

**When to Update**:
- Architecture decisions change
- New contracts are established
- Evolution phases are revised
- Success criteria are refined
- New anti-patterns are discovered

**Update Process**:
1. Discuss changes with team
2. Update relevant SHOULD DO doc
3. Update this index if needed
4. Notify affected developers
5. Update related implementation docs

**Version Control**:
- Include "Last Updated" date at top
- Reference git commit or PR that changed architecture
- Maintain changelog for major contract changes

---

### Creating New SHOULD DO Documentation

**When to Create**:
- New module or component added
- Complex refactoring planned
- Architectural patterns need documentation

**Template**:
Use existing docs as templates. Include all sections from "Documentation Structure" above.

**Naming Convention**:
- `SHOULD_DO_{component_name}.md`
- Store in `/docs` directory
- Add to this index

---

## 🎯 Success Metrics

This documentation is successful when:

✅ **Developers refer to it** before coding (measured by mentions in PRs)
✅ **Refactorings align with it** (measured by contract compliance in reviews)
✅ **Gaps are tracked** (measured by tech debt tickets referencing SHOULD DO)
✅ **Onboarding is faster** (measured by time-to-first-PR for new devs)
✅ **Architecture is preserved** (measured by design principle violations)

---

## 📚 Related Documentation

- [REFACTORING_ANALYSIS.md](./REFACTORING_ANALYSIS.md) - Concrete refactoring opportunities
- [Intent-Driven Architecture](../README.md) - High-level architecture overview
- Component-specific README files - Implementation details
- Test documentation - Verification strategies

---

## 🤝 Contributing

To add or improve SHOULD DO documentation:

1. Identify component needing documentation
2. Create new `SHOULD_DO_{name}.md` file in `/docs`
3. Follow template structure
4. Focus on **intent** not **implementation**
5. Include contracts, anti-patterns, and evolution path
6. Update this index
7. Submit PR for review

**Review Criteria**:
- Clear separation of SHOULD from IS
- Specific, testable contracts
- Realistic evolution phases
- Measurable success criteria
- Actionable refactoring guidance

---

**Remember**: This documentation guides refactoring by making intent explicit. Keep it updated as the architecture evolves!
