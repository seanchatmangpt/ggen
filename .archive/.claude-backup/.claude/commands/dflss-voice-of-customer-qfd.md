# Voice of Customer (QFD) - Multi-Step Workflow

## Purpose

This command guides agents through Quality Function Deployment (QFD) to capture customer needs (Voice of Customer) and translate them into design requirements. QFD ensures designs meet customer needs by systematically translating "what customers want" into "how we'll deliver it". Experts use QFD to prevent designing solutions that don't meet customer needs.

## Workflow Overview

```
Step 1: Capture Voice of Customer → Step 2: Organize Customer Needs → Step 3: Build QFD House of Quality → Step 4: Prioritize Requirements → Step 5: Set Targets and Verify
```

## Step-by-Step Instructions

### Step 1: Capture Voice of Customer

**Action**: Capture customer needs through multiple methods.

#### 1.1: Customer Interviews

**Action**: Conduct direct interviews with customers.

**Interview process**:
1. **Prepare questions**: Open-ended questions about needs
2. **Conduct interviews**: One-on-one or small group sessions
3. **Record responses**: Capture verbatim customer statements
4. **Follow up**: Clarify ambiguous statements

**Interview questions**:
- "What problems do you face with the current solution?"
- "What would make your life easier?"
- "What frustrates you most?"
- "What would an ideal solution look like?"

**Example interview responses**:
```markdown
## Customer Interview Responses

**Customer 1**:
- "Tests take too long - I wait 10 seconds every time"
- "I want to see progress while tests run"
- "Tests should be reliable - no flakiness"

**Customer 2**:
- "I need tests to run fast so I can iterate quickly"
- "Tests should be easy to write"
- "I want clear error messages when tests fail"

**Customer 3**:
- "Waiting for tests blocks my workflow"
- "Tests should work the same way every time"
- "I want to know which tests are slow"
```

#### 1.2: Surveys

**Action**: Conduct structured surveys to gather quantitative data.

**Survey design**:
- **Questions**: Structured questions about needs
- **Scales**: Rating scales (1-5, 1-10)
- **Distribution**: Distribute to representative sample
- **Analysis**: Analyze responses statistically

**Example survey**:
```markdown
## Customer Survey

**Question 1**: How important is test execution speed? (1-10)
- Average: 9.2/10
- Standard deviation: 1.1

**Question 2**: How satisfied are you with current speed? (1-10)
- Average: 3.5/10
- Standard deviation: 2.1
- Gap: 9.2 - 3.5 = 5.7 (large gap - high priority)

**Question 3**: What features are most important? (Select top 3)
- Speed: 85%
- Reliability: 78%
- Ease of use: 65%
```

#### 1.3: Observations

**Action**: Observe customers using current solution.

**Observation process**:
1. **Watch usage**: Observe how customers use solution
2. **Note pain points**: Identify frustrations and workarounds
3. **Record behavior**: Document actual usage patterns
4. **Identify needs**: Infer needs from behavior

**Example observations**:
```markdown
## Customer Observations

**Observation 1**: Customer repeatedly checks test status
- **Inferred need**: "I want real-time test progress"

**Observation 2**: Customer runs tests multiple times due to flakiness
- **Inferred need**: "I want reliable tests that don't fail randomly"

**Observation 3**: Customer waits for tests before committing code
- **Inferred need**: "I want fast tests so I don't wait"
```

#### 1.4: Data Analysis

**Action**: Analyze usage data to identify needs.

**Data sources**:
- **Usage logs**: How solution is used
- **Error logs**: What errors occur
- **Performance metrics**: Performance bottlenecks
- **Support tickets**: Common issues

**Example data analysis**:
```markdown
## Data Analysis

**Usage Logs**:
- Average test execution time: 10.2 seconds
- 95th percentile: 15.8 seconds
- **Need**: "Tests should complete in < 2 seconds"

**Error Logs**:
- Flaky test rate: 5% (too high)
- **Need**: "Tests should be reliable (0% flakiness)"

**Support Tickets**:
- Top issue: "Tests are too slow" (45% of tickets)
- Second issue: "Tests are flaky" (30% of tickets)
- **Needs**: Speed and reliability are top priorities
```

---

### Step 2: Organize Customer Needs

**Action**: Organize captured needs into structured categories.

#### 2.1: Affinity Diagram

**Action**: Group related needs using affinity diagram.

**Affinity diagram process**:
1. **Write needs**: Write each need on a card/sticky note
2. **Group**: Group related needs together
3. **Label groups**: Name each group
4. **Organize**: Organize groups hierarchically

**Example affinity diagram**:
```markdown
## Affinity Diagram

**Group 1: Performance**
- "Tests run fast"
- "Tests complete quickly"
- "No waiting for tests"
- "Fast iteration"

**Group 2: Reliability**
- "Tests are reliable"
- "No flaky tests"
- "Tests work consistently"
- "Predictable test results"

**Group 3: Usability**
- "Easy to write tests"
- "Clear error messages"
- "Simple API"
- "Good documentation"

**Group 4: Visibility**
- "See test progress"
- "Know which tests are slow"
- "Understand test results"
- "Real-time feedback"
```

#### 2.2: Kano Model Classification

**Action**: Classify needs using Kano model.

**Kano model categories**:
- **Basic Needs**: Must-haves, expected features (dissatisfaction if absent)
- **Performance Needs**: More is better (satisfaction increases with performance)
- **Delight Needs**: Unexpected features (delight if present, no dissatisfaction if absent)

**Example Kano classification**:
```markdown
## Kano Model Classification

**Basic Needs** (Must-haves):
- "Tests run without errors" (expected)
- "Tests provide correct results" (expected)
- "Tests don't crash" (expected)

**Performance Needs** (More is better):
- "Tests run fast" (faster = better)
- "Tests are reliable" (more reliable = better)
- "Tests are easy to use" (easier = better)

**Delight Needs** (Unexpected):
- "Tests show progress animations" (delightful)
- "Tests suggest optimizations" (delightful)
- "Tests learn from usage patterns" (delightful)
```

#### 2.3: Prioritize Needs

**Action**: Prioritize customer needs by importance.

**Prioritization factors**:
- **Importance**: How important to customers?
- **Satisfaction Gap**: Current satisfaction vs desired
- **Frequency**: How often need arises?
- **Impact**: Impact if need not met?

**Example prioritization**:
```markdown
## Customer Needs Prioritization

**High Priority** (Must address):
1. "Tests run fast" (Importance: 9.2/10, Gap: 5.7, Frequency: High)
2. "Tests are reliable" (Importance: 8.8/10, Gap: 4.2, Frequency: High)
3. "Tests provide correct results" (Importance: 9.5/10, Gap: 0.5, Frequency: High)

**Medium Priority** (Should address):
4. "Tests are easy to use" (Importance: 7.5/10, Gap: 2.1, Frequency: Medium)
5. "Clear error messages" (Importance: 7.2/10, Gap: 3.4, Frequency: Medium)

**Low Priority** (Nice to have):
6. "Tests show progress" (Importance: 6.5/10, Gap: 1.8, Frequency: Low)
7. "Tests suggest optimizations" (Importance: 5.2/10, Gap: N/A, Frequency: Low)
```

---

### Step 3: Build QFD House of Quality

**Action**: Build QFD House of Quality to translate needs into requirements.

#### 3.1: QFD House Structure

**Action**: Create QFD House of Quality matrix.

**QFD House components**:
- **Left wall**: Customer needs (rows)
- **Ceiling**: Design requirements (columns)
- **Room**: Relationships between needs and requirements (Strong/Medium/Weak)
- **Right wall**: Importance ratings, targets, competitive analysis
- **Foundation**: Technical difficulty, target values

**Example QFD House structure**:
```markdown
## QFD House of Quality Structure

| Customer Need | Importance | Req 1: Exec Time | Req 2: Reliability | Req 3: API Simplicity | Req 4: Error Msgs | Target | Competitive |
|---------------|------------|------------------|-------------------|----------------------|-------------------|--------|-------------|
| Tests run fast | 9.2 | ●● | ○ | ○ | ○ | < 2s | Current: 10s |
| Tests reliable | 8.8 | ○ | ●● | ○ | ○ | 0% flaky | Current: 5% |
| Easy to use | 7.5 | ○ | ○ | ●● | ○ | Simple API | Current: Complex |
| Clear errors | 7.2 | ○ | ○ | ○ | ●● | Helpful msgs | Current: Cryptic |

Legend: ●● = Strong, ● = Medium, ○ = Weak
```

#### 3.2: Identify Design Requirements

**Action**: Identify design requirements that address customer needs.

**Requirement identification**:
- **For each need**: What requirement addresses it?
- **Measurable**: Requirements must be measurable
- **Actionable**: Requirements must be actionable
- **Complete**: Cover all customer needs

**Example requirements**:
```markdown
## Design Requirements

**Requirement 1**: Test execution time
- **Addresses**: "Tests run fast"
- **Measurement**: Execution time in seconds
- **Target**: < 2 seconds
- **Type**: Performance requirement

**Requirement 2**: Test reliability (flakiness rate)
- **Addresses**: "Tests are reliable"
- **Measurement**: Percentage of flaky tests
- **Target**: 0% flakiness
- **Type**: Quality requirement

**Requirement 3**: API simplicity
- **Addresses**: "Easy to use"
- **Measurement**: API complexity score, lines of code to write test
- **Target**: < 10 lines for basic test
- **Type**: Usability requirement

**Requirement 4**: Error message quality
- **Addresses**: "Clear error messages"
- **Measurement**: Error message clarity score, user feedback
- **Target**: 90%+ users understand errors
- **Type**: Usability requirement
```

#### 3.3: Map Relationships

**Action**: Map relationships between needs and requirements.

**Relationship symbols**:
- **●● (Strong)**: Requirement strongly addresses need (9 points)
- **● (Medium)**: Requirement moderately addresses need (3 points)
- **○ (Weak)**: Requirement weakly addresses need (1 point)
- **Blank**: No relationship (0 points)

**Example relationship mapping**:
```markdown
## Relationship Mapping

**Need**: "Tests run fast"
- **Req 1 (Exec Time)**: ●● (Strong - directly addresses)
- **Req 2 (Reliability)**: ○ (Weak - reliable tests may be slower)
- **Req 3 (API Simplicity)**: ○ (Weak - indirect impact)
- **Req 4 (Error Msgs)**: Blank (No relationship)

**Need**: "Tests reliable"
- **Req 1 (Exec Time)**: ○ (Weak - fast tests may be less reliable)
- **Req 2 (Reliability)**: ●● (Strong - directly addresses)
- **Req 3 (API Simplicity)**: ○ (Weak - indirect impact)
- **Req 4 (Error Msgs)**: ○ (Weak - error messages help debugging)
```

#### 3.4: Calculate Importance Scores

**Action**: Calculate importance scores for each requirement.

**Importance calculation**:
- For each requirement: Sum (Need Importance × Relationship Score)
- Higher score = more important requirement
- Prioritize requirements by score

**Example importance calculation**:
```markdown
## Requirement Importance Scores

**Requirement 1 (Exec Time)**:
- Need "Tests run fast" (9.2) × Strong (9) = 82.8
- Need "Tests reliable" (8.8) × Weak (1) = 8.8
- Need "Easy to use" (7.5) × Weak (1) = 7.5
- Need "Clear errors" (7.2) × 0 = 0
- **Total**: 99.1

**Requirement 2 (Reliability)**:
- Need "Tests run fast" (9.2) × Weak (1) = 9.2
- Need "Tests reliable" (8.8) × Strong (9) = 79.2
- Need "Easy to use" (7.5) × Weak (1) = 7.5
- Need "Clear errors" (7.2) × Weak (1) = 7.2
- **Total**: 103.1

**Ranking**: Reliability (103.1) > Exec Time (99.1) > API Simplicity > Error Msgs
```

---

### Step 3.5: Gap Analysis (CRITICAL)

**Action**: Identify gaps between customer needs and current capabilities.

**CRITICAL**: This step is mandatory. Skipping gap analysis causes QFD to verify existing requirements instead of implementing new ones to address unmet customer needs.

#### 3.5.1: Compare Needs vs Capabilities

**Action**: Compare each customer need against current capabilities.

**Gap analysis process**:
1. **List customer needs**: All needs from Step 2
2. **List current capabilities**: What the framework currently provides
3. **Identify gaps**: Needs not addressed by current capabilities
4. **Prioritize gaps**: Rank gaps by importance and impact

**Example gap analysis**:
```markdown
## Gap Analysis

**Customer Need**: "I want to see progress while tests run"
- **Current Capability**: cargo-nextest provides basic output, but status-level is set to "none"
- **Gap**: No real-time progress display
- **Priority**: High (Visibility need, Importance: 7.5/10)

**Customer Need**: "I want to know which tests are slow"
- **Current Capability**: cargo-nextest has --timings option, but not used
- **Gap**: No slow test identification feature
- **Priority**: High (Visibility need, Importance: 7.2/10)
```

#### 3.5.2: Create Requirements for Gaps

**Action**: Create new requirements to address identified gaps.

**Requirement creation**:
- **For each gap**: Create requirement that addresses it
- **Measurable**: Requirements must be measurable
- **Actionable**: Requirements must be actionable
- **Complete**: Requirements must fully address the gap

**Example requirements for gaps**:
```markdown
## New Requirements for Gaps

**Gap**: No real-time progress display
- **Requirement**: Enable test progress display in cargo-nextest configuration
- **Measurement**: Progress dots shown during test execution
- **Target**: Users can see test progress in real-time

**Gap**: No slow test identification
- **Requirement**: Add test timing report task
- **Measurement**: Timing report generated (HTML/JSON)
- **Target**: Users can identify slow tests via timing report
```

**Principle**: Gap analysis ensures QFD identifies unmet needs and creates requirements to address them, not just verify existing requirements meet targets.

---

### Step 4: Prioritize Requirements

**Action**: Prioritize design requirements based on QFD analysis.

#### 4.1: Rank Requirements

**Action**: Rank requirements by importance score.

**Ranking**:
- **High Priority**: Top requirements (must address)
- **Medium Priority**: Important requirements (should address)
- **Low Priority**: Lower priority requirements (nice to have)

**Example ranking**:
```markdown
## Requirement Ranking

**High Priority** (Must address):
1. Requirement 2: Reliability (Score: 103.1)
2. Requirement 1: Execution Time (Score: 99.1)

**Medium Priority** (Should address):
3. Requirement 3: API Simplicity (Score: 45.2)
4. Requirement 4: Error Messages (Score: 38.7)

**Focus**: Address high-priority requirements first
```

#### 4.2: Set Target Values

**Action**: Set target values for each requirement.

**Target setting**:
- **Based on customer needs**: What do customers want?
- **Based on competitive analysis**: How do competitors perform?
- **Based on feasibility**: What's achievable?
- **Stretch goals**: Aim high but achievable

**Example target values**:
```markdown
## Target Values

**Requirement 1: Execution Time**
- **Current**: 10 seconds
- **Competitor**: 5 seconds
- **Customer need**: < 2 seconds
- **Target**: < 2 seconds (stretch goal: < 1 second)

**Requirement 2: Reliability**
- **Current**: 5% flakiness
- **Competitor**: 2% flakiness
- **Customer need**: 0% flakiness
- **Target**: 0% flakiness (stretch goal: < 0.1%)

**Requirement 3: API Simplicity**
- **Current**: 20 lines for basic test
- **Competitor**: 15 lines
- **Customer need**: < 10 lines
- **Target**: < 10 lines (stretch goal: < 5 lines)
```

---

### Step 5: Set Targets and Verify

**Action**: Set final targets and verify requirements address customer needs.

#### 5.1: Finalize Targets

**Action**: Finalize target values for each requirement.

**Target finalization**:
- **Review QFD analysis**: Ensure targets address customer needs
- **Validate feasibility**: Ensure targets are achievable
- **Document targets**: Document targets clearly
- **Communicate**: Share targets with team

**Example finalized targets**:
```markdown
## Finalized Targets

**Requirement 1: Test Execution Time**
- **Target**: < 2 seconds (80% improvement from 10s)
- **Stretch**: < 1 second (90% improvement)
- **Rationale**: Addresses "Tests run fast" need (Importance: 9.2/10)

**Requirement 2: Test Reliability**
- **Target**: 0% flakiness (100% improvement from 5%)
- **Stretch**: < 0.1% flakiness
- **Rationale**: Addresses "Tests reliable" need (Importance: 8.8/10)

**Requirement 3: API Simplicity**
- **Target**: < 10 lines for basic test (50% improvement from 20 lines)
- **Stretch**: < 5 lines
- **Rationale**: Addresses "Easy to use" need (Importance: 7.5/10)

**Requirement 4: Error Message Quality**
- **Target**: 90%+ users understand errors
- **Stretch**: 95%+ users understand errors
- **Rationale**: Addresses "Clear error messages" need (Importance: 7.2/10)
```

#### 5.2: Verify Requirements Address Needs

**Action**: Verify that requirements fully address customer needs.

**Verification checklist**:
- ✅ All high-priority needs have requirements
- ✅ All requirements are measurable
- ✅ All requirements have targets
- ✅ Requirements cover all need categories

**Example verification**:
```markdown
## Requirements Verification

**Need**: "Tests run fast" (Importance: 9.2/10)
- ✅ Requirement: Execution time < 2s
- ✅ Relationship: Strong (●●)
- ✅ Target set: < 2 seconds
- **Status**: Fully addressed ✅

**Need**: "Tests reliable" (Importance: 8.8/10)
- ✅ Requirement: 0% flakiness
- ✅ Relationship: Strong (●●)
- ✅ Target set: 0% flakiness
- **Status**: Fully addressed ✅

**All high-priority needs addressed**: ✅
```

#### 5.3: Create Todo List for Requirements Implementation

**CRITICAL**: Do NOT write documents or reports. Create todos and execute them.

**Action**: Create 10+ item todo list for implementing requirements.

**Todo list creation**:
1. Create todos for each requirement implementation
2. Create todos for verification steps
3. Create todos for validation steps
4. Prioritize by requirement importance
5. Execute todos systematically

**Example todo list**:
```markdown
## Requirements Implementation Todos (10+ items)

**Requirement 1: Execution Time < 2s**:
- [ ] Design parallel execution architecture
- [ ] Implement parallel test execution
- [ ] Optimize test execution performance
- [ ] Measure execution time: `time cargo make test`
- [ ] Verify target met: < 2 seconds ✅
- [ ] Document performance improvements

**Requirement 2: Reliability (0% flakiness)**:
- [ ] Identify sources of flakiness
- [ ] Fix race conditions in tests
- [ ] Add deterministic test execution
- [ ] Run tests 1000 times, measure flakiness
- [ ] Verify target met: 0% flakiness ✅
- [ ] Document reliability improvements

**Requirement 3: API Simplicity (< 10 lines)**:
- [ ] Design simple API
- [ ] Implement concise API methods
- [ ] Write example: basic test in < 10 lines
- [ ] Verify target met: < 10 lines ✅
- [ ] Document API usage

**Requirement 4: Error Message Quality (90%+)**:
- [ ] Design clear error messages
- [ ] Implement helpful error formatting
- [ ] Test error message clarity with users
- [ ] Verify target met: 90%+ understand ✅
- [ ] Document error message guidelines

**Verification**:
- [ ] Verify all requirements implemented
- [ ] Verify all targets met
- [ ] Verify customer needs addressed
- [ ] Run full test suite: `cargo make test`
- [ ] Validate with customers
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement requirements)
3. Mark todos as completed as work is done
4. Verify each step works before moving to next
5. Continue until all todos complete

**Principle**: Execute requirements implementation, don't document it. Todos track progress, implementation delivers value.

---

## Complete Workflow Example

```markdown
# Step 1: Capture Voice of Customer
Interviews: "Tests take too long", "Tests should be reliable"
Surveys: Speed importance 9.2/10, Satisfaction 3.5/10 (Gap: 5.7)
Observations: Customers wait for tests, run tests multiple times
Data: Average 10.2s execution, 5% flakiness

# Step 2: Organize Customer Needs
Affinity: Performance, Reliability, Usability, Visibility
Kano: Basic (correct results), Performance (speed, reliability), Delight (progress)
Priority: Speed (9.2), Reliability (8.8), Ease of use (7.5)

# Step 3: Build QFD House of Quality
Needs → Requirements mapping
Relationships: Speed → Exec Time (Strong), Reliability → Reliability (Strong)
Importance scores: Reliability (103.1), Exec Time (99.1)

# Step 4: Prioritize Requirements
Ranking: Reliability > Exec Time > API Simplicity > Error Msgs
Targets: < 2s execution, 0% flakiness, < 10 lines API, 90%+ error clarity

# Step 5: Set Targets and Verify
Finalized targets: < 2s, 0% flakiness, < 10 lines, 90%+
Verification: All high-priority needs addressed ✅
Implementation: Create todos and execute
```

## Integration with Other Commands

- **[DMEDI Design Process](./dmedi-design-process.md)** - Use QFD in Measure phase to translate customer needs
- **[Concept Selection](./concept-selection.md)** - Use QFD requirements as selection criteria
- **[Robust Design](./robust-design.md)** - Use QFD requirements to design for robustness
- **[TRIZ Problem Solving](./triz-problem-solving.md)** - Use QFD to identify contradictions in customer needs
- **[DMAIC Problem Solving](./dmaic-problem-solving.md)** - Use QFD in Measure phase to understand customer needs

## Expert Insights

**Why this matters**: Designing without understanding customer needs leads to solutions that don't meet needs. QFD ensures systematic translation of customer needs into design requirements.

**Key principle**: "Customer needs drive design" - Start with customer needs, end with customer satisfaction. Don't design in a vacuum.

**Remember**: 
- **Capture comprehensively**: Use multiple methods (interviews, surveys, observations, data)
- **Organize systematically**: Use affinity diagrams and Kano model
- **Translate accurately**: QFD House ensures accurate translation
- **Prioritize objectively**: Use importance scores, not opinions
- **Verify thoroughly**: Ensure requirements address all needs

**QFD mindset**: Listen to customers. Organize needs. Translate systematically. Prioritize objectively. Verify completely.

**DfLSS alignment**: QFD supports DfLSS (Design for Lean Six Sigma) by ensuring designs address both efficiency (waste elimination - fast execution) AND quality (defect prevention - reliability) from the start - capturing customer needs for both efficiency and quality. Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: QFD commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement requirements, not document them
3. **Verify requirements** - Test that requirements address customer needs
4. **Complete todos** - Mark todos as done as implementation completes

**Principle**: Execute requirements implementation, don't document it. Todos track progress, implementation delivers value.

---

End Command ---

