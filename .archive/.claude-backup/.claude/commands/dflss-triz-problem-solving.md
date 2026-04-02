# TRIZ Problem Solving - Multi-Step Workflow

## Purpose

This command guides agents through TRIZ (Theory of Inventive Problem Solving) methodology to find innovative solutions by identifying contradictions and applying inventive principles. TRIZ helps solve problems that seem impossible by resolving contradictions rather than compromising. Experts use TRIZ to find breakthrough solutions that eliminate trade-offs.

## Workflow Overview

```
Step 1: Define Problem → Step 2: Identify Contradictions → Step 3: Apply TRIZ Principles → Step 4: Generate Solutions → Step 5: Verify Solutions
```

## Step-by-Step Instructions

### Step 1: Define Problem

**Action**: Clearly define the problem and desired improvement.

**Problem definition format**:
- **What**: What needs to be improved?
- **Current state**: What is the current situation?
- **Desired state**: What do we want to achieve?
- **Constraints**: What constraints exist?

**Example problem definition**:
```markdown
## Problem Definition

**What**: Need to improve test execution speed while maintaining test coverage
**Current state**: Tests take 10 seconds, comprehensive coverage
**Desired state**: Tests take < 1 second, maintain same coverage
**Constraints**: Cannot reduce test coverage, must maintain accuracy
```

**Principle**: Clear problem definition enables contradiction identification.

---

### Step 2: Identify Contradictions

**Action**: Identify technical and physical contradictions in the problem.

#### 2.1: Identify Technical Contradiction

**Action**: Find where improving one parameter worsens another.

**Technical contradiction format**:
- **Improving**: What parameter are we trying to improve?
- **Worsening**: What parameter gets worse as a result?
- **Contradiction**: The conflict between improvements

**TRIZ parameters** (39 standard parameters):
- Speed, Force, Energy, Power, Loss of Energy
- Loss of Substance, Loss of Information, Loss of Time
- Quantity of Substance, Reliability, Accuracy
- Manufacturing Precision, Object-Generated Harmful Factors
- Manufacturing Ease, Operational Ease, Device Complexity
- Level of Automation, Productivity
- And more...

**Example technical contradiction**:
```markdown
## Technical Contradiction

**Improving**: Speed (test execution time)
**Worsening**: Loss of Information (test coverage)
**Contradiction**: Faster tests require fewer tests, reducing coverage
**TRIZ Parameters**: 
- Improving: Parameter #9 (Speed)
- Worsening: Parameter #24 (Loss of Information)
```

#### 2.2: Identify Physical Contradiction

**Action**: Find where the same parameter needs opposite values.

**Physical contradiction format**:
- **Parameter**: What parameter has conflicting requirements?
- **Requirement 1**: What value is needed in one context?
- **Requirement 2**: What opposite value is needed in another context?
- **Contradiction**: Same parameter needs opposite values

**Example physical contradiction**:
```markdown
## Physical Contradiction

**Parameter**: Test execution parallelism
**Requirement 1**: Need sequential execution (for shared state)
**Requirement 2**: Need parallel execution (for speed)
**Contradiction**: Tests must run sequentially AND in parallel simultaneously
```

**Principle**: Contradictions reveal the core problem. Resolving contradictions leads to innovative solutions.

---

### Step 3: Apply TRIZ Principles

**Action**: Use TRIZ inventive principles to resolve contradictions.

#### 3.1: Use Contradiction Matrix (Technical Contradictions)

**Action**: Look up inventive principles using TRIZ contradiction matrix.

**Contradiction matrix**: 39x39 matrix mapping parameter pairs to inventive principles.

**Process**:
1. Identify improving parameter (row)
2. Identify worsening parameter (column)
3. Look up principles in matrix
4. Apply principles to generate solutions

**Example contradiction matrix lookup**:
```markdown
## Contradiction Matrix Lookup

**Improving**: Parameter #9 (Speed)
**Worsening**: Parameter #24 (Loss of Information)
**Matrix Cell**: [9, 24]
**Inventive Principles**:
- Principle #10 (Prior Action): Prepare tests in advance
- Principle #13 (The Other Way Round): Run tests in reverse order
- Principle #28 (Mechanics Substitution): Use parallel execution
- Principle #35 (Parameter Changes): Change test granularity
```

#### 3.2: Apply Separation Principles (Physical Contradictions)

**Action**: Use separation principles to resolve physical contradictions.

**Separation principles**:
1. **Separation in Space**: Parameter has different values in different spaces
2. **Separation in Time**: Parameter has different values at different times
3. **Separation in Condition**: Parameter has different values under different conditions
4. **Separation in Scale**: Parameter has different values at different scales

**Example separation**:
```markdown
## Physical Contradiction Resolution

**Contradiction**: Tests must run sequentially AND in parallel
**Separation Principle**: Separation in Condition
**Solution**: Run tests sequentially when shared state needed, parallel when independent
**Implementation**: Group tests by dependency, run groups in parallel, tests within group sequentially
```

#### 3.3: Apply 40 Inventive Principles

**Action**: Review all 40 TRIZ inventive principles for solution ideas.

**40 Inventive Principles** (selected examples):
1. **Segmentation**: Divide object into independent parts
2. **Taking Out**: Remove interfering part or property
3. **Local Quality**: Change structure from uniform to non-uniform
4. **Asymmetry**: Change symmetric object to asymmetric
5. **Merging**: Bring closer or merge identical objects
6. **Universality**: Make object perform multiple functions
7. **Nested Doll**: Place object inside another
8. **Anti-Weight**: Compensate for weight
9. **Preliminary Anti-Action**: Perform counter-action in advance
10. **Prior Action**: Perform required action in advance
11. **Cushion in Advance**: Compensate for low reliability
12. **Equipotentiality**: Change working conditions
13. **The Other Way Round**: Invert action
14. **Spheroidality**: Use curves instead of straight lines
15. **Dynamics**: Make object or environment changeable
16. **Partial or Excessive Action**: Difficult to achieve 100%? Use more or less
17. **Another Dimension**: Move object in 2D or 3D space
18. **Mechanical Vibration**: Use oscillation
19. **Periodic Action**: Replace continuous action with periodic
20. **Continuity of Useful Action**: Make all parts work continuously
21. **Skipping**: Conduct process at high speed
22. **Blessing in Disguise**: Use harmful factors beneficially
23. **Feedback**: Introduce feedback
24. **Intermediary**: Use intermediary object
25. **Self-Service**: Object serves itself
26. **Copying**: Use simple copies instead of complex objects
27. **Cheap Short-Living**: Replace expensive object with cheap one
28. **Mechanics Substitution**: Replace mechanical means with other means
29. **Pneumatics and Hydraulics**: Use gas or liquid instead of solid
30. **Flexible Shells and Thin Films**: Use flexible shells instead of rigid
31. **Porous Materials**: Make object porous
32. **Color Changes**: Change color or transparency
33. **Homogeneity**: Make object interact with similar object
34. **Discarding and Recovering**: Remove parts after use, restore after use
35. **Parameter Changes**: Change object's physical state
36. **Phase Transitions**: Use phase transitions
37. **Thermal Expansion**: Use thermal expansion
38. **Strong Oxidants**: Use enriched air or oxygen
39. **Inert Atmosphere**: Replace normal atmosphere with inert
40. **Composite Materials**: Replace homogeneous with composite

**Example principle application**:
```markdown
## Inventive Principle Application

**Principle #10 (Prior Action)**: Prepare tests in advance
**Application**: Pre-compile test fixtures, cache test data
**Solution**: Build test fixtures once, reuse across test runs

**Principle #28 (Mechanics Substitution)**: Use parallel execution
**Application**: Replace sequential test execution with parallel
**Solution**: Run independent tests in parallel threads
```

---

### Step 4: Generate Solutions

**Action**: Generate innovative solutions based on TRIZ principles.

#### 4.1: Brainstorm Solutions

**Action**: Generate multiple solution ideas for each principle.

**Solution generation**:
- Apply each principle to the problem
- Generate concrete solution ideas
- Think creatively, don't limit to obvious solutions
- Consider combinations of principles

**Example solution generation**:
```markdown
## Solution Ideas

**From Principle #10 (Prior Action)**:
- Solution 1: Pre-compile test fixtures at build time
- Solution 2: Cache test data in memory
- Solution 3: Pre-generate test scenarios

**From Principle #28 (Mechanics Substitution)**:
- Solution 1: Parallel test execution with thread pool
- Solution 2: Distributed test execution across machines
- Solution 3: Pipeline test execution stages

**From Principle #13 (The Other Way Round)**:
- Solution 1: Run fast tests first, slow tests later
- Solution 2: Skip tests that haven't changed
- Solution 3: Test only changed code paths
```

#### 4.2: Evaluate Solutions

**Action**: Evaluate solutions against problem criteria.

**Evaluation criteria**:
- **Resolves contradiction**: Does solution eliminate the contradiction?
- **Feasibility**: Can solution be implemented?
- **Impact**: How much does solution improve the problem?
- **Risk**: What are the risks of the solution?

**Example evaluation**:
```markdown
## Solution Evaluation

**Solution**: Pre-compile test fixtures + Parallel execution
- **Resolves contradiction**: ✅ Yes - maintains coverage, improves speed
- **Feasibility**: ✅ High - straightforward implementation
- **Impact**: ✅ High - 5-10x speed improvement
- **Risk**: ⚠️ Medium - requires careful dependency management
**Score**: 8/10 - Strong candidate
```

#### 4.3: Select Best Solution

**Action**: Select the best solution(s) to implement.

**Selection criteria**:
- Highest impact
- Resolves contradiction completely
- Feasible to implement
- Acceptable risk

**Example selection**:
```markdown
## Selected Solution

**Solution**: Pre-compile test fixtures + Parallel execution + Smart test selection
**Rationale**: 
- Combines multiple principles for maximum impact
- Resolves contradiction (speed + coverage)
- Feasible implementation
- Acceptable risk
```

---

### Step 5: Verify Solutions

**Action**: Verify that solutions resolve contradictions and improve the problem.

#### 5.1: Verify Contradiction Resolution

**Action**: Confirm that solution eliminates the contradiction.

**Verification questions**:
- Does solution improve the desired parameter?
- Does solution maintain or improve the other parameter?
- Is the contradiction resolved?

**Example verification**:
```rust
// Solution: Pre-compile fixtures + parallel execution
// Verify: Speed improved AND coverage maintained

// Before: Sequential execution, 10 seconds
// After: Parallel execution, 1 second
// Coverage: Same tests run, same coverage ✅

// Contradiction resolved: Speed improved without losing coverage
```

#### 5.2: Test Solution

**Action**: Implement and test the solution.

**Testing steps**:
1. Implement solution
2. Run tests: `cargo make test`
3. Measure improvement
4. Verify no regressions

**Example test**:
```bash
# Test solution
cargo make test

# Measure execution time
time cargo make test
# Before: 10.0s
# After: 1.2s ✅

# Verify coverage maintained
cargo make test-coverage
# Coverage: Same as before ✅
```

#### 5.3: Create Todo List for Solution Implementation

**CRITICAL**: Do NOT write documents or reports. Create todos and execute them.

**Action**: Create 10+ item todo list for implementing solution.

**Todo list creation**:
1. Create todos for solution implementation steps
2. Create todos for verification steps
3. Create todos for prevention measures
4. Prioritize by impact
5. Execute todos systematically

**Example todo list**:
```markdown
## TRIZ Solution Implementation Todos (10+ items)

**Solution Implementation**:
- [ ] Implement pre-compilation of test fixtures
- [ ] Add fixture caching mechanism
- [ ] Implement parallel test execution
- [ ] Add test dependency analysis
- [ ] Group tests by dependency
- [ ] Run independent test groups in parallel
- [ ] Run dependent tests sequentially within groups
- [ ] Add test execution time measurement
- [ ] Verify compilation: `cargo make check`
- [ ] Run tests: `cargo make test`

**Verification**:
- [ ] Measure execution time improvement
- [ ] Verify test coverage maintained
- [ ] Verify no test failures
- [ ] Verify contradiction resolved (speed + coverage)

**Prevention**:
- [ ] Add test for parallel execution correctness
- [ ] Add test for fixture caching
- [ ] Document solution approach
- [ ] Add monitoring for test execution time
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement solution)
3. Mark todos as completed as work is done
4. Verify each step works before moving to next
5. Continue until all todos complete

**Principle**: Execute solutions, don't document them. Todos track progress, solutions resolve contradictions.

---

## Complete Workflow Example

```markdown
# Step 1: Define Problem
Problem: Need faster tests while maintaining coverage
Current: 10 seconds, full coverage
Desired: < 1 second, same coverage

# Step 2: Identify Contradictions
Technical Contradiction: Speed vs Loss of Information
Improving: Speed (Parameter #9)
Worsening: Loss of Information (Parameter #24)

# Step 3: Apply TRIZ Principles
Contradiction Matrix [9, 24]:
- Principle #10 (Prior Action)
- Principle #28 (Mechanics Substitution)
- Principle #35 (Parameter Changes)

# Step 4: Generate Solutions
Solution 1: Pre-compile fixtures (Principle #10)
Solution 2: Parallel execution (Principle #28)
Solution 3: Smart test selection (Principle #35)
Selected: Combine all three

# Step 5: Verify Solutions
Implemented: Pre-compilation + Parallel + Smart selection
Result: 1.2s execution time, same coverage ✅
Contradiction resolved ✅
```

## Integration with Other Commands

- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use TRIZ in Improve phase to find innovative solutions
- **[Root Cause Analysis](./root-cause-analysis.md)** - Use TRIZ after identifying root cause to find breakthrough solutions
- **[Poka-Yoke Design](./poka-yoke-design.md)** - Use TRIZ principles to design error-prevention mechanisms
- **[DMEDI Design Process](./dmedi-design-process.md)** - Use TRIZ in Explore phase for concept generation
- **[Concept Selection](./concept-selection.md)** - Use TRIZ-generated concepts in selection process

## Expert Insights

**Why this matters**: Traditional problem-solving often involves compromise. TRIZ helps find solutions that eliminate contradictions, leading to breakthrough improvements.

**Key principle**: "Resolve contradictions, don't compromise" - TRIZ finds solutions that improve all parameters, not trade-offs.

**Remember**: 
- **Contradictions reveal opportunities** - Where there's a contradiction, there's potential for innovation
- **Principles guide solutions** - 40 principles provide systematic approach to innovation
- **Breakthrough solutions** - TRIZ finds solutions that seem impossible but eliminate contradictions

**TRIZ mindset**: Don't accept trade-offs. Find solutions that improve everything. Contradictions are opportunities, not limitations.

**DfLSS alignment**: TRIZ supports DfLSS (Design for Lean Six Sigma) by finding solutions that address both efficiency (waste elimination) AND quality (defect prevention) simultaneously - eliminating contradictions rather than compromising. Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: TRIZ commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement solutions, not document them
3. **Verify solutions** - Test that solutions resolve contradictions
4. **Complete todos** - Mark todos as done as solutions complete

**Principle**: Execute solutions that resolve contradictions, don't document them. Todos track progress, solutions eliminate contradictions.

---

End Command ---

