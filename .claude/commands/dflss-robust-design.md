# Robust Design - Multi-Step Workflow

## Purpose

This command guides agents through robust design methodology to create systems that work reliably under variation and uncertainty. Robust design ensures systems perform well even when conditions vary (noise factors) by optimizing control factors. Experts use robust design to create systems that are insensitive to variation, reducing defects and improving reliability.

## Workflow Overview

```
Step 1: Identify Factors → Step 2: Design Experiments → Step 3: Run Experiments → Step 4: Analyze Results → Step 5: Optimize and Verify
```

## Step-by-Step Instructions

### Step 1: Identify Factors

**Action**: Identify control factors, noise factors, and response variables.

#### 1.1: Identify Control Factors

**Action**: Identify design parameters we can control.

**Control factors**:
- **Design parameters**: Parameters we set during design
- **Implementation choices**: Choices we make in implementation
- **Configuration options**: Options we can configure

**Example control factors**:
```markdown
## Control Factors

**Factor 1**: Thread pool size
- **Description**: Number of threads for parallel execution
- **Levels**: 2, 4, 8, 16 threads
- **Type**: Design parameter

**Factor 2**: Fixture cache size
- **Description**: Maximum number of fixtures to cache
- **Levels**: 100, 500, 1000 fixtures
- **Type**: Configuration option

**Factor 3**: Test grouping strategy
- **Description**: How to group tests for execution
- **Levels**: By dependency, by file, by module
- **Type**: Implementation choice

**Factor 4**: Error retry count
- **Description**: Number of retries on transient errors
- **Levels**: 0, 1, 3, 5 retries
- **Type**: Configuration option
```

#### 1.2: Identify Noise Factors

**Action**: Identify sources of variation we can't control.

**Noise factors**:
- **Environmental variation**: System load, network conditions
- **Input variation**: Different test inputs, data sizes
- **Usage variation**: Different usage patterns
- **Time variation**: Performance varies over time

**Example noise factors**:
```markdown
## Noise Factors

**Noise 1**: System load
- **Description**: CPU/memory load from other processes
- **Variation**: Low (idle) to High (busy)
- **Control**: Cannot control (external factor)
- **Impact**: Affects test execution time

**Noise 2**: Test suite size
- **Description**: Number of tests varies
- **Variation**: Small (10 tests) to Large (1000+ tests)
- **Control**: Cannot control (user input)
- **Impact**: Affects execution time and resource usage

**Noise 3**: Test complexity
- **Description**: Complexity of individual tests
- **Variation**: Simple to Complex tests
- **Control**: Cannot control (user writes tests)
- **Impact**: Affects execution time per test

**Noise 4**: Network conditions
- **Description**: Network latency and bandwidth
- **Variation**: Fast to Slow network
- **Control**: Cannot control (infrastructure)
- **Impact**: Affects distributed test execution
```

#### 1.3: Identify Response Variables

**Action**: Identify what we want to optimize (responses).

**Response variables**:
- **Performance**: Speed, throughput, latency
- **Quality**: Reliability, accuracy, correctness
- **Efficiency**: Resource usage, cost
- **Robustness**: Consistency under variation

**Example response variables**:
```markdown
## Response Variables

**Response 1**: Test execution time
- **Description**: Time to execute test suite
- **Target**: Minimize (faster is better)
- **Measurement**: Average execution time in seconds
- **Robustness**: Consistent time under varying conditions

**Response 2**: Test reliability (flakiness)
- **Description**: Percentage of flaky tests
- **Target**: Minimize (0% is best)
- **Measurement**: Flakiness rate (%)
- **Robustness**: Low flakiness under varying conditions

**Response 3**: Resource usage
- **Description**: CPU and memory usage
- **Target**: Minimize (lower is better)
- **Measurement**: Peak CPU %, Peak memory MB
- **Robustness**: Consistent usage under varying loads

**Response 4**: Test success rate
- **Description**: Percentage of tests that pass
- **Target**: Maximize (100% is best)
- **Measurement**: Success rate (%)
- **Robustness**: High success rate under varying inputs
```

---

### Step 2: Design Experiments

**Action**: Design experiments to test factor combinations.

#### 2.1: Choose Experimental Design

**Action**: Select appropriate experimental design method.

**Design methods**:
- **Full Factorial**: Test all factor combinations (comprehensive but expensive)
- **Fractional Factorial**: Test subset of combinations (efficient)
- **Taguchi Methods**: Orthogonal arrays (efficient, robust)
- **Response Surface**: For optimization (advanced)

**Example design selection**:
```markdown
## Experimental Design Selection

**Factors**: 4 control factors, 2-4 levels each
**Full Factorial**: 2×4×3×4 = 96 combinations (too many)
**Fractional Factorial**: 16 combinations (manageable)
**Taguchi L16**: 16 runs, orthogonal array (efficient)
**Selected**: Taguchi L16 orthogonal array
```

#### 2.2: Create Experimental Matrix

**Action**: Create matrix of factor combinations to test.

**Taguchi L16 example**:
```markdown
## Experimental Matrix (Taguchi L16)

| Run | Thread Pool | Cache Size | Grouping | Retry Count | Expected Performance |
|-----|-------------|------------|----------|-------------|---------------------|
| 1   | 2           | 100        | Dep      | 0           | Baseline            |
| 2   | 2           | 100        | File     | 1           | Low threads         |
| 3   | 2           | 500        | Module   | 3           | Medium cache        |
| 4   | 2           | 1000       | Dep      | 5           | Large cache         |
| 5   | 4           | 100        | File     | 3           | Medium threads      |
| 6   | 4           | 100        | Module   | 5           | Small cache         |
| 7   | 4           | 500        | Dep      | 0           | Medium cache        |
| 8   | 4           | 500        | File     | 1           | Balanced             |
| 9   | 8           | 100        | Module   | 1           | High threads         |
| 10  | 8           | 500        | Dep      | 5           | Optimal candidate    |
| 11  | 8           | 1000       | File     | 0           | Large cache         |
| 12  | 8           | 1000       | Module   | 3           | High performance     |
| 13  | 16          | 100        | Dep      | 5           | Max threads          |
| 14  | 16          | 500        | File     | 0           | Balanced             |
| 15  | 16          | 1000       | Module   | 1           | High capacity        |
| 16  | 16          | 1000       | Dep      | 3           | Maximum              |

Legend: Dep = By dependency, File = By file, Module = By module
```

#### 2.3: Plan Experiment Execution

**Action**: Plan how to execute experiments.

**Execution plan**:
- **Randomization**: Randomize run order to reduce bias
- **Replication**: Run each combination multiple times
- **Blocking**: Group runs to control noise factors
- **Measurement**: How to measure responses

**Example execution plan**:
```markdown
## Experiment Execution Plan

**Randomization**: Randomize run order
**Replication**: 3 runs per combination (48 total runs)
**Blocking**: Block by system load (low/high)
**Measurement**: 
- Execution time: `time cargo make test`
- Flakiness: Run 100 times, count failures
- Resource usage: Monitor CPU/memory
- Success rate: Count passing tests
```

---

### Step 3: Run Experiments

**Action**: Execute experiments and collect data.

#### 3.1: Execute Experiments

**Action**: Run experiments according to plan.

**Execution steps**:
1. Set control factor levels
2. Run experiment
3. Measure responses
4. Record data
5. Repeat for all combinations

**Example execution**:
```bash
# Run 1: Thread pool=2, Cache=100, Grouping=Dep, Retry=0
export THREAD_POOL_SIZE=2
export CACHE_SIZE=100
export GROUPING_STRATEGY=dependency
export RETRY_COUNT=0

time cargo make test > run1_results.txt 2>&1
# Execution time: 8.5s
# Success rate: 100%
# Flakiness: 0%

# Run 2: Thread pool=8, Cache=500, Grouping=Dep, Retry=5
export THREAD_POOL_SIZE=8
export CACHE_SIZE=500
export GROUPING_STRATEGY=dependency
export RETRY_COUNT=5

time cargo make test > run10_results.txt 2>&1
# Execution time: 1.2s
# Success rate: 100%
# Flakiness: 0%
```

#### 3.2: Collect Data

**Action**: Collect and organize experimental data.

**Data collection**:
- **Response values**: Record all response measurements
- **Factor levels**: Record factor settings for each run
- **Noise conditions**: Record noise factor conditions
- **Metadata**: Record run conditions, timestamps, etc.

**Example data collection**:
```markdown
## Experimental Data

| Run | Thread Pool | Cache | Grouping | Retry | Time (s) | Success % | Flakiness % | CPU % | Memory MB |
|-----|-------------|-------|----------|-------|----------|-----------|--------------|-------|------------|
| 1   | 2           | 100   | Dep      | 0     | 8.5      | 100       | 0            | 25    | 150        |
| 2   | 2           | 100   | File     | 1     | 8.2      | 100       | 0            | 24    | 148        |
| 3   | 2           | 500   | Module   | 3     | 7.8      | 100       | 0            | 23    | 200        |
| 4   | 2           | 1000  | Dep      | 5     | 7.5      | 100       | 0            | 22    | 250        |
| 5   | 4           | 100   | File     | 3     | 5.2      | 100       | 0            | 35    | 155        |
| 6   | 4           | 100   | Module   | 5     | 5.0      | 100       | 0            | 34    | 152        |
| 7   | 4           | 500   | Dep      | 0     | 4.8      | 100       | 0            | 33    | 205        |
| 8   | 4           | 500   | File     | 1     | 4.5      | 100       | 0            | 32    | 203        |
| 9   | 8           | 100   | Module   | 1     | 2.8      | 100       | 0            | 45    | 160        |
| 10  | 8           | 500   | Dep      | 5     | 1.2      | 100       | 0            | 42    | 210        |
| 11  | 8           | 1000  | File     | 0     | 1.0      | 100       | 0            | 40    | 260        |
| 12  | 8           | 1000  | Module   | 3     | 1.1      | 100       | 0            | 41    | 258        |
| 13  | 16          | 100   | Dep      | 5     | 2.5      | 100       | 0            | 60    | 165        |
| 14  | 16          | 500   | File     | 0     | 2.2      | 100       | 0            | 58    | 215        |
| 15  | 16          | 1000  | Module   | 1     | 2.0      | 100       | 0            | 56    | 270        |
| 16  | 16          | 1000  | Dep      | 3     | 1.8      | 100       | 0            | 55    | 268        |
```

---

### Step 4: Analyze Results

**Action**: Analyze experimental data to find optimal factor settings.

#### 4.1: Calculate Signal-to-Noise Ratios

**Action**: Calculate S/N ratios for robustness (Taguchi method).

**S/N ratio types**:
- **Larger is Better**: For maximizing (e.g., success rate)
  - S/N = -10 × log10(mean(1/y²))
- **Smaller is Better**: For minimizing (e.g., execution time)
  - S/N = -10 × log10(mean(y²))
- **Nominal is Best**: For targeting (e.g., specific value)
  - S/N = 10 × log10(mean²/variance)

**Example S/N calculation**:
```markdown
## Signal-to-Noise Ratios (Smaller is Better for Execution Time)

| Run | Time (s) | S/N Ratio (dB) |
|-----|----------|----------------|
| 1   | 8.5      | -18.6          |
| 2   | 8.2      | -18.3          |
| 3   | 7.8      | -17.8          |
| 4   | 7.5      | -17.5          |
| 5   | 5.2      | -14.3          |
| 6   | 5.0      | -14.0          |
| 7   | 4.8      | -13.6          |
| 8   | 4.5      | -13.1          |
| 9   | 2.8      | -8.9           |
| 10  | 1.2      | -1.6           |
| 11  | 1.0      | 0.0            |
| 12  | 1.1      | -0.8           |
| 13  | 2.5      | -8.0           |
| 14  | 2.2      | -6.8           |
| 15  | 2.0      | -6.0           |
| 16  | 1.8      | -5.1           |

**Best S/N**: Run 11 (S/N = 0.0 dB, Time = 1.0s)
```

#### 4.2: Analyze Factor Effects

**Action**: Analyze how each factor affects responses.

**Factor effect analysis**:
- **Main effects**: Average response for each factor level
- **Interaction effects**: How factors interact
- **Optimal levels**: Best level for each factor

**Example factor effect analysis**:
```markdown
## Factor Effect Analysis (Execution Time)

**Factor 1: Thread Pool Size**
- Level 2: Average = 7.5s
- Level 4: Average = 4.9s
- Level 8: Average = 1.5s (BEST)
- Level 16: Average = 2.1s
- **Optimal**: 8 threads

**Factor 2: Cache Size**
- Level 100: Average = 4.6s
- Level 500: Average = 3.4s (BEST)
- Level 1000: Average = 3.0s
- **Optimal**: 500-1000 fixtures

**Factor 3: Grouping Strategy**
- Level Dep (Dependency): Average = 3.2s (BEST)
- Level File: Average = 4.1s
- Level Module: Average = 4.2s
- **Optimal**: By dependency

**Factor 4: Retry Count**
- Level 0: Average = 3.5s
- Level 1: Average = 3.8s
- Level 3: Average = 3.6s
- Level 5: Average = 3.4s (BEST)
- **Optimal**: 5 retries
```

#### 4.3: Identify Optimal Settings

**Action**: Identify optimal factor level combinations.

**Optimal settings**:
- **From main effects**: Best level for each factor
- **From S/N ratios**: Settings with best robustness
- **From interactions**: Consider factor interactions

**Example optimal settings**:
```markdown
## Optimal Settings

**From Main Effects**:
- Thread Pool: 8 threads
- Cache Size: 500-1000 fixtures
- Grouping: By dependency
- Retry Count: 5 retries

**From S/N Ratios**:
- Best S/N: Run 11 (Thread=8, Cache=1000, File, Retry=0)
- But Run 10 also good: Thread=8, Cache=500, Dep, Retry=5

**Selected Optimal**:
- Thread Pool: 8 threads
- Cache Size: 500 fixtures (balance performance and memory)
- Grouping: By dependency
- Retry Count: 5 retries
- **Predicted Performance**: ~1.2s execution time
```

---

### Step 5: Optimize and Verify

**Action**: Optimize design and verify robustness.

#### 5.1: Implement Optimal Settings

**Action**: Implement optimal factor settings in design.

**Implementation steps**:
1. Set control factors to optimal levels
2. Update configuration
3. Test implementation
4. Measure performance

**Example implementation**:
```rust
// Implement optimal settings
pub struct TestExecutor {
    thread_pool: ThreadPool,
    fixture_cache: FixtureCache,
    grouping_strategy: GroupingStrategy,
    retry_count: u32,
}

impl TestExecutor {
    pub fn new() -> Self {
        Self {
            thread_pool: ThreadPool::new(8), // Optimal: 8 threads
            fixture_cache: FixtureCache::new(500), // Optimal: 500 fixtures
            grouping_strategy: GroupingStrategy::ByDependency, // Optimal: dependency
            retry_count: 5, // Optimal: 5 retries
        }
    }
}
```

#### 5.2: Verify Robustness

**Action**: Verify design works well under varying conditions.

**Robustness verification**:
- **Vary noise factors**: Test under different noise conditions
- **Measure performance**: Measure responses under variation
- **Compare to targets**: Ensure targets met under variation
- **Validate consistency**: Ensure consistent performance

**Example robustness verification**:
```markdown
## Robustness Verification

**Test 1: Low System Load**
- Execution time: 1.2s ✅
- Success rate: 100% ✅
- Flakiness: 0% ✅

**Test 2: High System Load**
- Execution time: 1.5s ✅ (still < 2s target)
- Success rate: 100% ✅
- Flakiness: 0% ✅

**Test 3: Small Test Suite (10 tests)**
- Execution time: 0.3s ✅
- Success rate: 100% ✅

**Test 4: Large Test Suite (1000+ tests)**
- Execution time: 1.8s ✅ (still < 2s target)
- Success rate: 100% ✅

**Conclusion**: Design is robust - meets targets under varying conditions ✅
```

#### 5.3: Create Todo List for Robust Design Implementation

**CRITICAL**: Do NOT write documents or reports. Create todos and execute them.

**Action**: Create 10+ item todo list for implementing robust design.

**Todo list creation**:
1. Create todos for optimal settings implementation
2. Create todos for robustness verification
3. Create todos for monitoring and control
4. Prioritize by impact
5. Execute todos systematically

**Example todo list**:
```markdown
## Robust Design Implementation Todos (10+ items)

**Optimal Settings Implementation**:
- [ ] Set thread pool size to 8 (optimal)
- [ ] Set fixture cache size to 500 (optimal)
- [ ] Implement dependency-based grouping (optimal)
- [ ] Set retry count to 5 (optimal)
- [ ] Update configuration files
- [ ] Verify compilation: `cargo make check`
- [ ] Run tests: `cargo make test`

**Robustness Verification**:
- [ ] Test under low system load
- [ ] Test under high system load
- [ ] Test with small test suite
- [ ] Test with large test suite
- [ ] Test with varying test complexity
- [ ] Measure execution time under all conditions
- [ ] Verify targets met: < 2s under all conditions
- [ ] Verify success rate: 100% under all conditions
- [ ] Verify flakiness: 0% under all conditions

**Monitoring and Control**:
- [ ] Add monitoring for execution time
- [ ] Add monitoring for success rate
- [ ] Add monitoring for flakiness
- [ ] Set up alerts for performance degradation
- [ ] Document optimal settings
- [ ] Document robustness verification results
```

**Execution**:
1. Create todos using `todo_write` tool (10+ items minimum)
2. Execute todos one by one (implement robust design)
3. Mark todos as completed as work is done
4. Verify each step works before moving to next
5. Continue until all todos complete

**Principle**: Execute robust design implementation, don't document it. Todos track progress, implementation delivers robustness.

---

## Complete Workflow Example

```markdown
# Step 1: Identify Factors
Control: Thread pool (2-16), Cache size (100-1000), Grouping (3 strategies), Retry (0-5)
Noise: System load, test suite size, test complexity, network conditions
Response: Execution time (minimize), Success rate (maximize), Flakiness (minimize)

# Step 2: Design Experiments
Design: Taguchi L16 orthogonal array (16 runs)
Plan: 3 replications, randomized order, block by system load

# Step 3: Run Experiments
Execute: Run all 16 combinations, measure responses
Data: Collected execution time, success rate, flakiness for each run

# Step 4: Analyze Results
S/N Ratios: Calculated for execution time (smaller is better)
Factor Effects: Thread=8 optimal, Cache=500 optimal, Grouping=Dep optimal, Retry=5 optimal
Optimal: Thread=8, Cache=500, Grouping=Dep, Retry=5

# Step 5: Optimize and Verify
Implement: Set optimal settings
Verify: Test under varying conditions, all meet targets ✅
```

## Integration with Other Commands

- **[DMEDI Design Process](./dmedi-design-process.md)** - Use robust design in Develop phase to optimize design
- **[Design of Experiments](./dmedi-design-process.md)** - Use DOE methods for experimental design
- **[Voice of Customer (QFD)](./voice-of-customer-qfd.md)** - Use QFD requirements as response variables
- **[Concept Selection](./concept-selection.md)** - Use robust design to evaluate concept robustness
- **[FMEA](./fmea.md)** - Use FMEA to identify noise factors and failure modes

## Expert Insights

**Why this matters**: Systems that work well only under ideal conditions fail in real-world use. Robust design ensures systems work reliably under variation, reducing defects and improving customer satisfaction.

**Key principle**: "Design for variation" - Optimize control factors to minimize sensitivity to noise factors. Make systems robust, not just optimal under ideal conditions.

**Remember**: 
- **Identify all factors**: Control factors (we control) and noise factors (we don't)
- **Design experiments**: Use systematic experimental design (Taguchi, DOE)
- **Analyze systematically**: Use S/N ratios and factor effects
- **Verify robustness**: Test under varying conditions, not just ideal conditions

**Robust design mindset**: Design for real-world variation. Optimize for robustness, not just performance. Verify under varying conditions.

**DfLSS alignment**: Robust design supports DfLSS (Design for Lean Six Sigma) by addressing both efficiency (waste elimination through consistent performance) AND quality (defect prevention through robustness) from the start - designing systems that work efficiently and reliably under variation. Don't conflate DfLSS with DFSS (Design for Six Sigma) - DFSS only addresses quality, missing critical waste elimination. See [Root Cause Analysis - DfLSS vs DFSS](./root-cause-analysis.md#dflss-vs-dfss-critical-distinction) for why conflating DfLSS with DFSS is a huge error.

---

## Command Execution Pattern

**CRITICAL**: Robust design commands must:
1. **Create 10+ item todo list** - Not documents/reports
2. **Execute todos** - Implement robust design, not document it
3. **Verify robustness** - Test that design works under variation
4. **Complete todos** - Mark todos as done as implementation completes

**Principle**: Execute robust design implementation, don't document it. Todos track progress, implementation delivers robustness.

---

End Command ---

