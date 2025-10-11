# ADR-001: Intent-Driven Architecture Documentation

**Status**: Accepted
**Date**: 2025-10-10
**Authors**: Core Team
**Related**: REFACTORING_ANALYSIS.md

## Context

The ggen codebase has grown to 35+ files across multiple crates, with complex autonomous workflows, multi-agent coordination, and distributed regeneration systems. As the system evolves, we need a documentation strategy that:

1. **Guides Refactoring**: Clearly states what code SHOULD DO (intent) rather than what it currently does
2. **Maintains Vision**: Preserves architectural decisions across team changes and time
3. **Enables Validation**: Allows comparison between intent and implementation
4. **Accelerates Onboarding**: New developers understand system purpose without reading all code

Traditional implementation-focused documentation becomes outdated quickly and doesn't capture the "why" behind design decisions.

## Decision

We adopt **Intent-Driven Architecture** (IDA) documentation patterns for all modules:

### Module-Level Documentation Structure

Every module MUST include comprehensive intent documentation at the top:

```rust
//! # Module Name - Brief Description
//!
//! ## PURPOSE
//! What problem this module SHOULD solve
//!
//! ## RESPONSIBILITIES
//! What duties this module SHOULD have
//!
//! ## CONSTRAINTS
//! What limitations SHOULD apply
//!
//! ## DEPENDENCIES
//! What other systems SHOULD be coordinated with
//!
//! ## INVARIANTS
//! What conditions MUST always be true
//!
//! ## ERROR HANDLING STRATEGY
//! How errors SHOULD be handled
//!
//! ## TESTING STRATEGY
//! How this SHOULD be tested
//!
//! ## REFACTORING PRIORITIES
//! Priority-ordered improvements needed
```

### Function-Level Documentation Pattern

Key functions SHOULD document intent using this format:

```rust
/// SHOULD DO: [Intended behavior and purpose]
/// SHOULD ACCEPT: [Expected inputs and constraints]
/// SHOULD RETURN: [Expected outputs and guarantees]
/// SHOULD ENSURE: [Invariants maintained]
/// SHOULD HANDLE: [Error conditions and recovery]
async fn example_function(&self, input: Input) -> Result<Output>
```

### Architectural Decision Records

Complex architectural decisions MUST be documented in separate ADRs with:
- Context and problem statement
- Decision rationale and alternatives considered
- Consequences and trade-offs
- Migration path from current state to desired state

## Rationale

### Why "SHOULD DO" Instead of "DOES"

**Traditional Documentation:**
```rust
/// Processes events in parallel using a thread pool
async fn process_events(&self) -> Result<()>
```

**Intent-Driven Documentation:**
```rust
/// SHOULD DO: Process events concurrently with backpressure control
/// SHOULD ACCEPT: Event stream with priority ordering
/// SHOULD ENSURE: No event processed twice, failures don't block others
/// SHOULD HANDLE: Transient failures with exponential backoff
async fn process_events(&self) -> Result<()>
```

**Benefits:**
1. **Guides Refactoring**: When implementation diverges from intent, we know what to fix
2. **Captures Requirements**: Documents non-functional requirements (performance, reliability)
3. **Prevents Regression**: Tests can verify intent, not just current behavior
4. **Maintains Context**: Explains *why* decisions were made, not just *what* exists

### Alignment with Core Team Best Practices

This decision supports:
- **"Ultrathink" Philosophy**: Deep architectural thinking before coding
- **Autonomous Systems**: Clear contracts for multi-agent coordination
- **Test-Driven Development**: Intent documentation guides test design
- **Continuous Improvement**: Refactoring priorities embedded in documentation

## Consequences

### Positive

1. **Better Refactoring Guidance**: REFACTORING_ANALYSIS.md references match intent documentation
2. **Improved Onboarding**: New developers understand system purpose quickly
3. **Test Quality**: Tests verify intent, catching semantic bugs (not just syntax)
4. **Architectural Consistency**: Design patterns propagate through intent documentation
5. **Living Documentation**: Documentation drives development, not just describes it

### Negative

1. **Initial Overhead**: Requires ~30-60 minutes per module for comprehensive documentation
2. **Maintenance Burden**: Documentation must be updated when intent changes
3. **Learning Curve**: Team must understand IDA patterns and apply consistently
4. **Tool Support**: Standard doc tools don't distinguish intent from implementation docs

### Mitigation Strategies

- **Templates**: Provide copy-paste templates for module and function documentation
- **Examples**: Use ontology.rs (lines 1-51) as canonical example
- **Reviews**: Include intent documentation in code review checklist
- **Tooling**: Create lints to verify documentation structure compliance

## Implementation Plan

### Phase 1: Foundation (Completed)
- ✅ Document ontology.rs with comprehensive intent documentation
- ✅ Create REFACTORING_ANALYSIS.md with priorities
- ✅ Add intent documentation to 4 core modules:
  - swarm_agent.rs (933 lines)
  - orchestrator.rs (471 lines)
  - regeneration.rs (565 lines)
  - test_helpers.rs (80 lines)

### Phase 2: Expansion (Next 2 Weeks)
- [ ] Apply intent documentation to remaining autonomous/* modules
- [ ] Update generators/* with intent documentation
- [ ] Document all public APIs with SHOULD DO format
- [ ] Create ADR templates for future architectural decisions

### Phase 3: Enforcement (Next 1 Month)
- [ ] Add CI lint to verify documentation structure
- [ ] Create pre-commit hook for intent documentation
- [ ] Update CONTRIBUTING.md with IDA guidelines
- [ ] Train team on intent documentation patterns

### Phase 4: Validation (Ongoing)
- [ ] Compare implementation against intent during code reviews
- [ ] Flag divergence as technical debt
- [ ] Update refactoring priorities quarterly
- [ ] Measure documentation quality in team retrospectives

## Metrics for Success

### Quantitative
- **Documentation Coverage**: >80% of modules have comprehensive intent docs (baseline: 15%)
- **Refactoring Velocity**: 25% reduction in time to understand and refactor modules
- **Onboarding Time**: 30% reduction in time for new developers to make first contribution
- **Test Quality**: 40% increase in semantic bugs caught by intent-driven tests

### Qualitative
- Developers can explain system architecture without reading all code
- Refactoring decisions reference documented intent, not just current implementation
- Code reviews focus on intent alignment, not just syntax correctness
- Technical debt is visible through intent vs. implementation gaps

## Examples

### Before (Implementation-Focused)
```rust
//! Orchestrator for autonomous regeneration
use crate::autonomous::regeneration::RegenerationEngine;

/// Start the orchestrator
pub async fn start(&self) -> Result<()> {
    // implementation...
}
```

### After (Intent-Driven)
```rust
//! # Autonomous Orchestrator - Machine-Timescale Coordination
//!
//! ## PURPOSE
//! Coordinates autonomous regeneration cycles at machine timescale (sub-30 second target)
//!
//! ## RESPONSIBILITIES
//! - Parallel event processing with configurable concurrency
//! - Cycle time optimization through adaptive parallelism
//! - Health monitoring with automatic recovery
//!
//! ## CONSTRAINTS
//! - Cycle time target: <30 seconds under normal load
//! - Single orchestrator instance per deployment

use crate::autonomous::regeneration::RegenerationEngine;

/// SHOULD DO: Initialize orchestrator and begin autonomous regeneration loop
/// SHOULD ACCEPT: Valid OrchestratorConfig with reasonable limits
/// SHOULD RETURN: Running orchestrator or configuration error
/// SHOULD ENSURE: Only one orchestrator runs at a time
/// SHOULD HANDLE: Graceful shutdown on SIGTERM, completing in-flight work
pub async fn start(&self) -> Result<()> {
    // implementation...
}
```

## References

- [Intent-Driven Architecture Pattern](https://example.com/ida-pattern)
- ontology.rs:1-51 (canonical example in codebase)
- REFACTORING_ANALYSIS.md (priorities align with intent documentation)
- swarm_agent.rs:1-50 (autonomous workflow intent)
- orchestrator.rs:1-40 (machine-timescale coordination intent)

## Related ADRs

- ADR-002: Autonomous Workflow Coordination (planned)
- ADR-003: Multi-Agent Swarm Architecture (planned)
- ADR-004: Knowledge Graph Evolution Strategy (planned)

---

**Approval**: This ADR is accepted and in effect as of 2025-10-10.

**Review Schedule**: Quarterly review of intent documentation coverage and quality metrics.
