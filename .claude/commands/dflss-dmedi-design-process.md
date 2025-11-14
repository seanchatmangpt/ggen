# DMEDI Design Process - Multi-Step Workflow

## Purpose

This command guides agents through DMEDI (Define, Measure, Explore, Develop, Implement) methodology for designing new products, processes, or systems. DMEDI is a data-driven design methodology that ensures new designs meet customer needs, business goals, and quality requirements. Experts use DMEDI to systematically design solutions that delight customers while meeting cost and timing goals.

## Workflow Overview

```
Step 1: Define → Step 2: Measure → Step 3: Explore → Step 4: Develop → Step 5: Implement
```

## Step-by-Step Instructions

### Step 1: Define

**Action**: Define the design project scope, goals, and success criteria.

#### 1.1: Create Project Charter

**Action**: Document project purpose, scope, and objectives.

**Charter components**:
- **Business Case**: Why is this project needed?
- **Problem Statement**: What problem are we solving?
- **Goal Statement**: What are we trying to achieve?
- **Scope**: What's included? What's excluded?
- **Success Criteria**: How will we measure success?
- **Timeline**: What are key milestones?
- **Resources**: What resources are available?

**Example charter**:
```markdown
## Project Charter

**Business Case**: Need new test framework that reduces test execution time by 80% while maintaining coverage
**Problem Statement**: Current tests take 10 seconds, blocking rapid development cycles
**Goal Statement**: Design test framework with < 2 second execution time, 100% coverage maintained
**Scope**: 
- Included: Test execution engine, fixture management, parallel execution
- Excluded: Test authoring tools, reporting dashboards
**Success Criteria**: 
- Execution time < 2 seconds (80% improvement)
- Test coverage maintained at 100%
- Zero test failures
**Timeline**: 4 weeks (1 week per phase)
**Resources**: 2 developers, test infrastructure
```

#### 1.2: Define MGPP (Multi-Generational Product Plan)

**Action**: Plan multiple generations of the product.

**MGPP components**:
- **Generation 1**: Minimum viable product (MVP)
- **Generation 2**: Enhanced features
- **Generation 3**: Advanced capabilities
- **Migration Path**: How to evolve from Gen 1 to Gen 3

**Example MGPP**:
```markdown
## Multi-Generational Product Plan

**Generation 1 (MVP)**:
- Basic parallel test execution
- Simple fixture management
- Core test framework features
- Target: 50% speed improvement

**Generation 2 (Enhanced)**:
- Advanced parallel execution strategies
- Fixture caching and pre-compilation
- Test dependency analysis
- Target: 80% speed improvement

**Generation 3 (Advanced)**:
- Distributed test execution
- Intelligent test selection
- Predictive test optimization
- Target: 90%+ speed improvement

**Migration Path**: Incremental rollout, backward compatible
```

#### 1.3: Risk Management Plan

**Action**: Identify and plan for project risks.

**Risk categories**:
- **Technical Risks**: Technology challenges, complexity
- **Schedule Risks**: Timeline delays, resource constraints
- **Quality Risks**: Defects, performance issues
- **Business Risks**: Changing requirements, market shifts

**Example risk plan**:
```markdown
## Risk Management Plan

**Risk 1**: Parallel execution introduces race conditions
- **Probability**: Medium
- **Impact**: High
- **Mitigation**: Comprehensive testing, gradual rollout
- **Contingency**: Fallback to sequential execution

**Risk 2**: Test dependency analysis incomplete
- **Probability**: Medium
- **Impact**: Medium
- **Mitigation**: Start with simple dependency rules, iterate
- **Contingency**: Manual dependency specification
```

#### 1.4: Communication Plan

**Action**: Plan stakeholder communication.

**Communication plan**:
- **Stakeholders**: Who needs to know?
- **Frequency**: How often to communicate?
- **Format**: What format (meetings, reports, demos)?
- **Content**: What information to share?

---

### Step 2: Measure

**Action**: Measure customer needs and current performance.

#### 2.1: Voice of Customer (VOC)

**Action**: Capture customer needs and requirements.

**VOC methods**:
- **Interviews**: Direct conversations with customers
- **Surveys**: Structured questionnaires
- **Observations**: Watch customers use current solution
- **Data Analysis**: Analyze usage patterns, error logs

**VOC output**: Customer needs statements (what customers want, not how to implement)

**Example VOC**:
```markdown
## Voice of Customer

**Need 1**: "Tests should run fast - waiting 10 seconds is too slow"
**Need 2**: "I need to know tests are still comprehensive"
**Need 3**: "Tests should be reliable - no flakiness"
**Need 4**: "I want to see progress while tests run"
**Need 5**: "Tests should work the same way every time"
```

#### 2.2: Quality Function Deployment (QFD)

**Action**: Translate customer needs into design requirements.

**QFD House of Quality**:
- **Customer Needs** (rows): What customers want
- **Design Requirements** (columns): How we'll meet needs
- **Relationships**: How requirements address needs (Strong/Medium/Weak)
- **Importance**: Customer priority for each need
- **Targets**: Target values for each requirement

**Example QFD**:
```markdown
## QFD House of Quality

**Customer Need**: "Tests run fast"
- **Design Requirement**: Test execution time
- **Relationship**: Strong (directly addresses need)
- **Target**: < 2 seconds
- **Importance**: High (9/10)

**Customer Need**: "Tests are comprehensive"
- **Design Requirement**: Test coverage percentage
- **Relationship**: Strong (directly addresses need)
- **Target**: 100% coverage maintained
- **Importance**: High (9/10)
```

#### 2.3: Target Costing

**Action**: Set cost targets based on customer value and business goals.

**Target costing process**:
1. Determine target price (what customers will pay)
2. Determine target profit margin
3. Calculate target cost (price - profit)
4. Allocate cost to design requirements
5. Design to meet cost targets

**Example target costing**:
```markdown
## Target Costing

**Target Price**: $0 (open source)
**Target Profit**: N/A (open source)
**Target Cost**: Development time < 4 weeks
**Cost Allocation**:
- Define phase: 1 week
- Measure phase: 1 week
- Explore phase: 1 week
- Develop phase: 1 week
- Implement phase: Ongoing
```

#### 2.4: Scorecards

**Action**: Create scorecards to measure design performance.

**Scorecard components**:
- **Metrics**: What to measure
- **Targets**: Target values
- **Current**: Current performance
- **Gap**: Gap to target

**Example scorecard**:
```markdown
## Design Scorecard

**Metric**: Test execution time
- **Target**: < 2 seconds
- **Current**: 10 seconds
- **Gap**: 8 seconds (80% improvement needed)

**Metric**: Test coverage
- **Target**: 100%
- **Current**: 100%
- **Gap**: 0% (maintain current level)
```

#### 2.5: Current State Analysis

**Action**: Measure current performance baseline.

**Measurement steps**:
1. Measure current metrics
2. Document current process
3. Identify pain points
4. Establish baseline for comparison

**Example current state**:
```bash
# Measure current test execution time
time cargo make test
# Result: 10.0 seconds

# Measure current test coverage
cargo make test-coverage
# Result: 100% coverage

# Document current process
# Tests run sequentially, no parallelization
```

---

### Step 3: Explore

**Action**: Explore design concepts and select the best approach.

#### 3.1: Concept Generation

**Action**: Generate multiple design concepts.

**Concept generation methods**:
- **Brainstorming**: Generate many ideas
- **TRIZ**: Use TRIZ principles for innovative concepts
- **Benchmarking**: Learn from similar solutions
- **Prototyping**: Build quick prototypes to explore ideas

**Example concepts**:
```markdown
## Design Concepts

**Concept 1**: Parallel test execution
- **Description**: Run independent tests in parallel threads
- **Pros**: Simple, fast implementation
- **Cons**: Requires dependency analysis

**Concept 2**: Pre-compiled fixtures
- **Description**: Compile test fixtures at build time
- **Pros**: Reduces runtime overhead
- **Cons**: Requires build-time processing

**Concept 3**: Smart test selection
- **Description**: Run only tests for changed code
- **Pros**: Maximum speed improvement
- **Cons**: Requires change detection
```

#### 3.2: Concept Selection

**Action**: Select best concept(s) using systematic methods.

**Selection methods**:
- **Pugh Matrix**: Compare concepts to baseline
- **AHP (Analytic Hierarchy Process)**: Pairwise comparisons
- **Scoring**: Score concepts against criteria

**Example concept selection**:
```markdown
## Concept Selection

**Criteria**: Speed improvement, Implementation ease, Risk
**Selected**: Concept 1 (Parallel execution) + Concept 2 (Pre-compiled fixtures)
**Rationale**: 
- High speed improvement (80%+)
- Moderate implementation ease
- Low risk
- Complementary (can combine)
```

#### 3.3: Statistical Tolerance Design

**Action**: Design tolerances to ensure performance under variation.

**Tolerance design**:
- **Nominal**: Target value
- **Tolerance**: Acceptable variation
- **Process Capability**: Can process meet tolerance?

**Example tolerance design**:
```markdown
## Tolerance Design

**Requirement**: Test execution time
- **Nominal**: 1.5 seconds
- **Tolerance**: ±0.5 seconds (1.0 to 2.0 seconds acceptable)
- **Process Capability**: Design to meet tolerance 99.9% of time
```

#### 3.4: Monte Carlo Simulation

**Action**: Simulate design performance under variation.

**Monte Carlo process**:
1. Define input distributions
2. Generate random samples
3. Calculate outputs
4. Analyze results

**Example simulation**:
```markdown
## Monte Carlo Simulation

**Input**: Test execution time (normal distribution, mean=1.5s, std=0.2s)
**Simulation**: 10,000 runs
**Results**: 
- 99.5% of runs < 2.0 seconds ✅
- Mean: 1.5 seconds ✅
- Design meets requirements
```

---

### Step 4: Develop

**Action**: Develop detailed design and optimize performance.

#### 4.1: Detailed Design

**Action**: Create detailed design specifications.

**Design components**:
- **Architecture**: System structure
- **Interfaces**: API specifications
- **Algorithms**: Core algorithms
- **Data Structures**: Data models
- **Error Handling**: Error handling strategy

**Example detailed design**:
```rust
// Architecture: Parallel test execution engine
pub struct TestExecutor {
    thread_pool: ThreadPool,
    fixture_cache: FixtureCache,
}

// Interface: Execute tests in parallel
impl TestExecutor {
    pub fn execute_parallel(&self, tests: Vec<Test>) -> Result<TestResults> {
        // Group tests by dependency
        let groups = self.group_by_dependency(tests);
        
        // Execute groups in parallel
        // Execute tests within group sequentially
        // ...
    }
}
```

#### 4.2: Design of Experiments (DOE)

**Action**: Use DOE to optimize design parameters.

**DOE process**:
1. Identify factors (design parameters)
2. Identify levels (values to test)
3. Design experiment (full factorial, fractional factorial)
4. Run experiments
5. Analyze results
6. Optimize design

**Example DOE**:
```markdown
## Design of Experiments

**Factor 1**: Thread pool size
- **Levels**: 2, 4, 8, 16 threads
**Factor 2**: Fixture cache size
- **Levels**: 100, 500, 1000 fixtures

**Experiment**: 2^2 full factorial (4 runs)
**Response**: Test execution time

**Results**: 
- Optimal: 8 threads, 500 fixture cache
- Execution time: 1.2 seconds ✅
```

#### 4.3: Reliability Design

**Action**: Design for reliability and robustness.

**Reliability considerations**:
- **Failure Modes**: How can design fail?
- **Failure Rates**: Expected failure rates
- **Redundancy**: Backup mechanisms
- **Error Recovery**: How to recover from errors

**Example reliability design**:
```markdown
## Reliability Design

**Failure Mode**: Race condition in parallel execution
**Mitigation**: 
- Dependency analysis prevents conflicts
- Lock mechanisms for shared resources
- Fallback to sequential execution if parallel fails

**Failure Mode**: Fixture cache corruption
**Mitigation**:
- Cache validation on load
- Rebuild cache on corruption
- Fallback to on-demand fixture creation
```

#### 4.4: Robust Design

**Action**: Design to work reliably under variation.

**Robust design**:
- **Control Factors**: Design parameters we control
- **Noise Factors**: Variation we can't control
- **Response**: What we want to optimize
- **Optimization**: Find settings that work well under variation

**Example robust design**:
```markdown
## Robust Design

**Control Factor**: Thread pool size
**Noise Factors**: System load, test complexity variation
**Response**: Test execution time
**Optimization**: Choose thread pool size that minimizes execution time under varying conditions
**Result**: 8 threads optimal under all conditions ✅
```

---

### Step 5: Implement

**Action**: Implement design, validate, and deploy.

#### 5.1: Prototype and Pilot

**Action**: Build prototype and pilot with limited scope.

**Prototype steps**:
1. Build minimal prototype
2. Test with sample use cases
3. Gather feedback
4. Iterate

**Pilot steps**:
1. Deploy to limited environment
2. Monitor performance
3. Gather user feedback
4. Fix issues
5. Scale up

**Example prototype**:
```bash
# Build prototype
cargo build --features prototype

# Test with sample tests
cargo make test --prototype

# Measure performance
time cargo make test --prototype
# Result: 1.5 seconds ✅

# Gather feedback
# Users: "Much faster, works well!"
```

#### 5.2: Process Control

**Action**: Establish controls to maintain performance.

**Control mechanisms**:
- **Monitoring**: Track key metrics
- **Alerts**: Alert on performance degradation
- **Reviews**: Regular performance reviews
- **Standards**: Document standards and procedures

**Example process control**:
```markdown
## Process Control

**Metric**: Test execution time
- **Target**: < 2 seconds
- **Monitoring**: Track execution time in CI
- **Alert**: Alert if > 2.5 seconds
- **Review**: Weekly performance review

**Metric**: Test coverage
- **Target**: 100%
- **Monitoring**: Track coverage in CI
- **Alert**: Alert if < 99%
- **Review**: Weekly coverage review
```

#### 5.3: Implementation Planning

**Action**: Plan full implementation rollout.

**Implementation plan**:
- **Phases**: Rollout phases
- **Timeline**: Implementation schedule
- **Resources**: Required resources
- **Training**: Training plan
- **Support**: Support plan

**Example implementation plan**:
```markdown
## Implementation Plan

**Phase 1**: Internal testing (Week 1)
- Deploy to internal team
- Gather feedback
- Fix issues

**Phase 2**: Beta release (Week 2)
- Deploy to beta users
- Monitor performance
- Gather feedback

**Phase 3**: Full release (Week 3)
- Deploy to all users
- Monitor performance
- Provide support

**Phase 4**: Optimization (Ongoing)
- Continuous improvement
- Performance optimization
- Feature enhancements
```

#### 5.4: Create Todo List for Implementation

**CRITICAL**: Do NOT write documents or reports. Create todos and execute them.

**Action**: Create 10+ item todo list for implementing design.

**Todo list creation**:
1. Create todos for implementation steps
2. Create todos for validation steps
3. Create todos for control measures
4. Prioritize by phase
5. Execute todos systematically

**Example todo list**:
```markdown
## DMEDI Implementation Todos (10+ items)

**Define Phase**:
- [ ] Create project charter
- [ ] Define MGPP (multi-generational plan)
- [ ] Create risk management plan
- [ ] Create communication plan
- [ ] Get stakeholder approval

**Measure Phase**:
- [ ] Capture Voice of Customer
- [ ] Create QFD House of Quality
- [ ] Set target costs
- [ ] Create scorecards
- [ ] Measure current state baseline

**Explore Phase**:
- [ ] Generate design concepts (TRIZ, brainstorming)
- [ ] Select best concepts (Pugh Matrix, AHP)
- [ ] Design statistical tolerances
- [ ] Run Monte Carlo simulations
- [ ] Validate concept feasibility

**Develop Phase**:
- [ ] Create detailed design specifications
- [ ] Run Design of Experiments
- [ ] Design for reliability
- [ ] Optimize for robustness
- [ ] Validate design meets requirements

**Implement Phase**:
- [ ] Build prototype
- [ ] Run pilot test
- [ ] Establish process controls
- [ ] Plan full implementation
- [ ] Deploy and monitor
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement design)
3. Mark todos as completed as work is done
4. Verify each step works before moving to next
5. Continue until all todos complete

**Principle**: Execute design implementation, don't document it. Todos track progress, implementation delivers value.

---

## Complete Workflow Example

```markdown
# Step 1: Define
Charter: Design test framework with 80% speed improvement
MGPP: Gen 1 (MVP), Gen 2 (Enhanced), Gen 3 (Advanced)
Risks: Race conditions, dependency analysis
Timeline: 4 weeks

# Step 2: Measure
VOC: "Tests run fast", "Tests are comprehensive"
QFD: Execution time < 2s, Coverage 100%
Baseline: 10 seconds, 100% coverage

# Step 3: Explore
Concepts: Parallel execution, Pre-compiled fixtures, Smart selection
Selected: Parallel + Pre-compiled (combines well)
Tolerance: 1.5s ± 0.5s

# Step 4: Develop
Design: Parallel executor with fixture cache
DOE: Optimal 8 threads, 500 fixture cache
Reliability: Dependency analysis, error recovery
Robust: Works under varying conditions

# Step 5: Implement
Prototype: Built and tested, 1.5s execution ✅
Pilot: Deployed to beta, feedback positive ✅
Controls: Monitoring, alerts, reviews
Rollout: Phased implementation plan
```

## Integration with Other Commands

- **[Voice of Customer (QFD)](./voice-of-customer-qfd.md)** - Use in Measure phase to capture and translate customer needs
- **[TRIZ Problem Solving](./triz-problem-solving.md)** - Use in Explore phase for concept generation
- **[Concept Selection](./concept-selection.md)** - Use in Explore phase to select best concepts
- **[Robust Design](./robust-design.md)** - Use in Develop phase to optimize for variation
- **[FMEA](./fmea.md)** - Use in Develop phase to identify failure modes
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use for problem-solving within DMEDI phases

## Expert Insights

**Why this matters**: Systematic design ensures new solutions meet customer needs, business goals, and quality requirements. DMEDI prevents costly redesigns by getting it right the first time.

**Key principle**: "Design right the first time" - DMEDI ensures comprehensive design that meets all requirements before implementation.

**Remember**: 
- **Customer-focused**: Start with customer needs, end with customer satisfaction
- **Data-driven**: Use data to make design decisions, not assumptions
- **Systematic**: Follow phases systematically, don't skip steps
- **Iterative**: Refine design through exploration and development

**DMEDI mindset**: Design systematically. Measure customer needs. Explore alternatives. Develop robustly. Implement carefully.

**DfLSS alignment**: DMEDI supports DfLSS (Design for Lean Six Sigma) by addressing both efficiency (waste elimination through systematic design) AND quality (defect prevention through robust design) from the start. Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: DMEDI commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement design, not document it
3. **Verify design** - Test that design meets requirements
4. **Complete todos** - Mark todos as done as design completes

**Principle**: Execute design implementation, don't document it. Todos track progress, implementation delivers value.

---

End Command ---

