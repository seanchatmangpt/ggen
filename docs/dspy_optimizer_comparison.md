# DSPy Optimizer Quick Reference & Comparison

**Last Updated**: 2026-01-11

---

## Quick Decision Matrix

| Your Situation | Recommended Optimizer | Why |
|----------------|----------------------|-----|
| **< 10 examples** | GEPA | Sample efficient, reflective learning |
| **10-50 examples** | BootstrapFewShot ✅ | Quick baseline (already implemented in Rust) |
| **50-200 examples, need dynamic retrieval** | KNNFewShot 🎯 | Retrieval-augmented, scales well |
| **50-200 examples, need exploration** | BootstrapFewShotWithRandomSearch | Random search over demos |
| **50-200 examples, need quality** | BootstrapFewShotWithOptuna | Bayesian optimization |
| **200+ examples, willing to invest** | MIPROv2 | Best for complex tasks, joint optimization |
| **Large dataset (thousands)** | SIMBA | Mini-batch + introspection |
| **Instruction-focused task** | COPRO 🎯 | Coordinate ascent on instructions |
| **Production deployment** | BootstrapFinetune | Distill to small model |
| **Any size in 2026** | GEPA 🏆 | Gold standard, best ROI |

**Legend**:
- ✅ = Already implemented in Rust
- 🎯 = High priority for next implementation
- 🏆 = Best overall for 2026

---

## Optimizer Comparison Table

| Optimizer | Type | Data Needs | Cost | Time | Production Ready | Rust Status |
|-----------|------|------------|------|------|------------------|-------------|
| **GEPA** | Reflective Evolution | 3-10 scenarios | Med | Med | ⭐⭐⭐⭐⭐ | ❌ (P0) |
| **BootstrapFewShot** | Teacher Bootstrap | 10+ examples | Low | Low | ⭐⭐⭐⭐☆ | ✅ **DONE** |
| **KNNFewShot** | Retrieval | 50+ examples | Low | Low | ⭐⭐⭐⭐⭐ | ❌ (P0) |
| **COPRO** | Instruction Opt | 20+ examples | Med | Med | ⭐⭐⭐⭐☆ | ❌ (P0) |
| **MIPROv2** | Bayesian Search | 200+ examples | High | High | ⭐⭐⭐⭐⭐ | ❌ (P2) |
| **BootstrapFinetune** | Weight Distillation | 100+ examples | High | High | ⭐⭐⭐⭐⭐ | ❌ (P3) |
| **BootstrapRS** | Random Search | 50+ examples | Med | Med | ⭐⭐⭐⭐☆ | ❌ (P1) |
| **BootstrapOptuna** | Bayesian Opt | 50-200 examples | Med | Med | ⭐⭐⭐⭐☆ | ❌ (P2) |
| **SIMBA** | Mini-Batch | Large datasets | High | High | ⭐⭐⭐⭐☆ | ❌ (P3) |
| **LabeledFewShot** | Direct Selection | Any | Very Low | Very Low | ⭐⭐☆☆☆ | ❌ (P4) |
| **Ensemble** | Aggregation | Multiple programs | High | Low | ⭐⭐⭐⭐☆ | ❌ (P4) |

---

## Algorithm Type Classification

### 1. Example-Based Optimizers (What to show)
- **LabeledFewShot**: Direct selection
- **BootstrapFewShot** ✅: Teacher-validated examples
- **BootstrapFewShotWithRandomSearch**: Random exploration
- **BootstrapFewShotWithOptuna**: Bayesian selection
- **KNNFewShot** 🎯: Dynamic retrieval

### 2. Instruction-Based Optimizers (How to instruct)
- **COPRO** 🎯: Coordinate ascent
- **MIPROv2**: Grounded proposals + Bayesian search
- **GEPA** 🏆: Reflective evolution

### 3. Hybrid Optimizers (Both)
- **MIPROv2**: Instructions + Examples
- **SIMBA**: Instructions + Hard examples

### 4. Weight-Based Optimizers (Model parameters)
- **BootstrapFinetune**: Supervised finetuning

### 5. Meta-Optimizers (Combination)
- **Ensemble**: Program aggregation

---

## Performance Rankings (2026)

### Overall Best Performance
1. **GEPA** 🏆 - 37.5% → 80% (+42.5 points), outperforms GRPO by +10% with 35× fewer rollouts
2. **MIPROv2** - Best for complex tasks with 200+ examples
3. **BootstrapFinetune** - Best for production efficiency

### Sample Efficiency
1. **GEPA** - Works with 3-10 scenarios
2. **BootstrapFewShot** - Works with 10+ examples
3. **COPRO** - Works with 20+ examples

### Production Deployment
1. **BootstrapFinetune** - Distills to small model
2. **KNNFewShot** - Dynamic retrieval, scales well
3. **GEPA** - Sample efficient, reflective

### Cost Efficiency
1. **BootstrapFewShot** ✅ - Low cost, fast
2. **KNNFewShot** 🎯 - Low cost, scalable
3. **LabeledFewShot** - Very low cost, but limited

---

## Core Algorithms (Simplified)

### BootstrapFewShot ✅
```
FOR each example in trainset:
  output = teacher(example)
  IF metric(example, output) passes:
    ADD to demonstrations
  IF len(demonstrations) >= max:
    BREAK

RETURN predictor with demonstrations
```

### KNNFewShot 🎯
```
// Pre-compute
embeddings = [embed(ex) for ex in trainset]

// Per query
FOR each query:
  query_emb = embed(query)
  neighbors = find_k_nearest(query_emb, embeddings)
  RETURN predictor with neighbors as demos
```

### COPRO 🎯
```
candidates = [baseline] + generate_variants(baseline)

FOR iteration in depth:
  scores = [evaluate(c, trainset) for c in candidates]
  best = max(candidates, key=scores)
  candidates = generate_from_top_k(scores)

RETURN best
```

### GEPA 🏆
```
frontier = [baseline]

FOR iteration in num_iterations:
  parent = sample(frontier)
  mutated = reflective_mutate(parent, scenarios)
  scores = evaluate_all(mutated, scenarios)

  frontier.add(mutated, scores)
  frontier.prune_dominated()

RETURN best_by_average(frontier)
```

### MIPROv2
```
// Stage 1: Bootstrap examples
demos = bootstrap_valid_examples(student, trainset)

// Stage 2: Generate instructions
instructions = generate_grounded_proposals(program, trainset, demos)

// Stage 3: Bayesian search
FOR trial in num_trials:
  (instr, demo) = bayesian_select(instructions, demos)
  score = evaluate(program, validation)
  update_model(instr, demo, score)

RETURN best_config
```

---

## Key Differences vs Our Rust Implementation

### What We Have ✅
- BootstrapFewShot core algorithm
- Teacher/student pattern
- Metric-based validation
- Demonstration formatting
- OptimizedPredictor with few-shot prompts
- Production-quality error handling
- Async/await support

### What We're Missing ❌

#### 1. Trace Support
```python
# Python DSPy supports
def metric(example, pred, trace=None):
    if trace:
        # Validate intermediate steps
        for step in trace:
            validate(step)
    return score(example, pred)
```

```rust
// Our Rust: No trace parameter yet
pub type MetricFn = Arc<dyn Fn(&Example, &HashMap<String, Value>)
                       -> Result<bool, ModuleError> + Send + Sync>;
```

#### 2. Advanced Optimizers
- KNNFewShot (retrieval-augmented)
- COPRO (instruction optimization)
- GEPA (reflective evolution)
- MIPROv2 (Bayesian search)
- All variants (RandomSearch, Optuna, etc.)

#### 3. Optimization Statistics
```python
# Python tracks
optimizer.compile(student, trainset)
print(optimizer.statistics)
# Success rate, scores, timing, etc.
```

#### 4. Error Budget
```python
# Python has max_errors parameter
BootstrapFewShot(max_errors=5)  # Stop if too many failures
```

---

## Implementation Priority for Rust

### Phase 1: Core Enhancements (Week 1-2) ✅→🎯
1. **Enhance BootstrapFewShot** (3-5 days)
   - Add trace support
   - Add error budget
   - Add statistics tracking

2. **Implement KNNFewShot** (1 week)
   - Vectorizer trait
   - Cosine similarity
   - Dynamic retrieval predictor

### Phase 2: Instruction Optimization (Week 3-4) 🎯
3. **Implement COPRO** (1-2 weeks)
   - Instruction generation
   - Coordinate ascent
   - Deduplication cache

4. **Implement BootstrapFewShotWithRandomSearch** (2-3 days)
   - Extend existing BootstrapFewShot
   - Random sampling
   - Validation evaluation

### Phase 3: Advanced (Week 5-7) 🏆
5. **Implement GEPA** (2-3 weeks)
   - Pareto frontier
   - Reflective mutation
   - Trace-based analysis

6. **Implement BootstrapFewShotWithOptuna** (1 week)
   - Bayesian optimization
   - Demo selection

### Phase 4: Production (Week 8+)
7. **Implement BootstrapFinetune** (4+ weeks)
   - Model training integration
   - Weight distillation

8. **Implement MIPROv2** (3-4 weeks)
   - Grounded proposals
   - Bayesian search
   - Minibatching

---

## Use Case Recommendations

### Customer Support Bot
**Requirements**: Low latency, high accuracy, 100+ examples
**Pipeline**:
1. GEPA (3-10 examples) → Quick baseline
2. KNNFewShot (100 examples) → Dynamic retrieval
3. BootstrapFinetune → Distill to small model

### Code Generation
**Requirements**: Complex reasoning, instruction-following
**Pipeline**:
1. BootstrapFewShot → Quick baseline
2. COPRO → Refine instructions
3. MIPROv2 (200+ examples) → Joint optimization

### Domain-Specific QA
**Requirements**: Domain adaptation, limited data
**Pipeline**:
1. GEPA (5-10 domain examples) → Sample efficient
2. KNNFewShot → Retrieve similar domain examples
3. COPRO → Domain-specific instructions

### Multi-Turn Dialogue
**Requirements**: Context tracking, intermediate validation
**Pipeline**:
1. BootstrapFewShot with trace-based metric
2. GEPA with trace reflection
3. Ensemble for robustness

---

## Key Metrics for Evaluation

### Accuracy Metrics
- **Exact Match**: `expected == actual`
- **Fuzzy Match**: `similarity(expected, actual) >= threshold`
- **Contains**: `expected in actual or actual in expected`
- **Regex**: `pattern.matches(actual)`
- **LLM Judge**: `llm("Is answer correct?") == "YES"`

### Multi-Criteria Metrics
```python
def metric(example, pred, trace=None):
    accuracy = exact_match(example, pred)
    conciseness = len(pred) < threshold
    if trace:
        valid_steps = all(validate(s) for s in trace)
        return valid_steps and accuracy
    return accuracy * conciseness
```

### Optimization Metrics
- Success rate: `valid_demos / total_attempts`
- Average score: `sum(scores) / len(scores)`
- Optimization time: `end_time - start_time`
- LLM calls: `total_llm_calls`

---

## Python DSPy vs Rust ggen-ai

### Similarities ✅
| Feature | Python DSPy | Rust ggen-ai |
|---------|-------------|--------------|
| Core algorithm | ✅ | ✅ |
| Teacher/student | ✅ | ✅ |
| Metric validation | ✅ | ✅ |
| Demonstration formatting | ✅ | ✅ |
| Few-shot prompting | ✅ | ✅ |

### Differences ❌
| Feature | Python DSPy | Rust ggen-ai |
|---------|-------------|--------------|
| Trace support | ✅ | ❌ |
| Error budget | ✅ | ❌ |
| Statistics tracking | ✅ | ❌ |
| KNNFewShot | ✅ | ❌ |
| COPRO | ✅ | ❌ |
| GEPA | ✅ | ❌ |
| MIPROv2 | ✅ | ❌ |
| BootstrapFinetune | ✅ | ❌ |
| Random search variants | ✅ | ❌ |
| Optuna integration | ✅ | ❌ |

---

## Key Takeaways

1. **We have a solid foundation** with BootstrapFewShot (~9% of Python ecosystem)

2. **Highest ROI next steps**:
   - KNNFewShot (dynamic retrieval, production value)
   - COPRO (instruction optimization, complements existing)
   - GEPA (best 2026 performance, sample efficient)

3. **Production recommendations**:
   - Small data: GEPA
   - Medium data: KNNFewShot or BootstrapFewShot
   - Large data: MIPROv2 or SIMBA
   - Deployment: BootstrapFinetune

4. **2026 winner**: GEPA (37.5% → 80%, +42.5 points, outperforms GRPO)

---

## Quick Links

- Full Research: [`dspy_optimizer_research.md`](/home/user/ggen/docs/dspy_optimizer_research.md)
- Current Rust Implementation: [`/home/user/ggen/crates/ggen-ai/src/dspy/optimizer.rs`](/home/user/ggen/crates/ggen-ai/src/dspy/optimizer.rs)
- Python DSPy Docs: https://dspy.ai/learn/optimization/optimizers/
- GEPA Paper: https://arxiv.org/abs/2507.19457

---

**Version**: 1.0
**Maintained by**: ggen-ai team
