<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Python DSPy Optimizer Research: Complete Analysis & Rust Implementation Roadmap](#python-dspy-optimizer-research-complete-analysis--rust-implementation-roadmap)
  - [Executive Summary](#executive-summary)
  - [1. Complete Optimizer Catalog](#1-complete-optimizer-catalog)
    - [1.1 Automatic Few-Shot Learning Optimizers](#11-automatic-few-shot-learning-optimizers)
      - [A. **LabeledFewShot** (Simplest)](#a-labeledfewshot-simplest)
      - [B. **BootstrapFewShot** ✅ (Implemented in Rust)](#b-bootstrapfewshot--implemented-in-rust)
      - [C. **BootstrapFewShotWithRandomSearch**](#c-bootstrapfewshotwithrandomsearch)
      - [D. **BootstrapFewShotWithOptuna**](#d-bootstrapfewshotwithoptuna)
      - [E. **KNNFewShot** 🎯](#e-knnfewshot-)
    - [1.2 Automatic Instruction Optimization](#12-automatic-instruction-optimization)
      - [F. **COPRO** (Contrastive Prompt Optimization) 🎯](#f-copro-contrastive-prompt-optimization-)
      - [G. **MIPROv2** (Multi-Prompt Instruction Proposal Optimizer v2)](#g-miprov2-multi-prompt-instruction-proposal-optimizer-v2)
      - [H. **SIMBA** (Stochastic Introspective Mini-Batch Ascent)](#h-simba-stochastic-introspective-mini-batch-ascent)
      - [I. **GEPA** (Genetic-Pareto Algorithm) 🏆](#i-gepa-genetic-pareto-algorithm-)
    - [1.3 Automatic Finetuning](#13-automatic-finetuning)
      - [J. **BootstrapFinetune**](#j-bootstrapfinetune)
    - [1.4 Program Transformations](#14-program-transformations)
      - [K. **Ensemble**](#k-ensemble)
  - [2. Teacher/Student Patterns & Demonstration Collection](#2-teacherstudent-patterns--demonstration-collection)
    - [2.1 Core Teacher/Student Paradigm](#21-core-teacherstudent-paradigm)
      - [A. **Self-Teaching** (Student = Teacher)](#a-self-teaching-student--teacher)
      - [B. **External Teacher** (Powerful Model → Weaker Student)](#b-external-teacher-powerful-model-%E2%86%92-weaker-student)
      - [C. **Pre-Compiled Teacher** (Optimized Program → New Student)](#c-pre-compiled-teacher-optimized-program-%E2%86%92-new-student)
    - [2.2 Demonstration Collection Strategies](#22-demonstration-collection-strategies)
      - [Strategy 1: **Greedy Collection** (BootstrapFewShot)](#strategy-1-greedy-collection-bootstrapfewshot)
      - [Strategy 2: **Reservoir Sampling** (Improved BootstrapFewShot)](#strategy-2-reservoir-sampling-improved-bootstrapfewshot)
      - [Strategy 3: **Diversity-Based** (KNNFewShot)](#strategy-3-diversity-based-knnfewshot)
      - [Strategy 4: **Hard Example Mining** (SIMBA)](#strategy-4-hard-example-mining-simba)
    - [2.3 Demonstration Validation](#23-demonstration-validation)
  - [3. Metric System & Evaluation Integration](#3-metric-system--evaluation-integration)
    - [3.1 Metric Function Signature](#31-metric-function-signature)
    - [3.2 Common Metric Patterns](#32-common-metric-patterns)
      - [A. **Exact Match**](#a-exact-match)
      - [B. **Fuzzy Match** (Edit Distance)](#b-fuzzy-match-edit-distance)
      - [C. **Contains Match** (Substring)](#c-contains-match-substring)
      - [D. **Regex Match**](#d-regex-match)
      - [E. **LLM-as-Judge**](#e-llm-as-judge)
      - [F. **Multi-Criteria Metric**](#f-multi-criteria-metric)
    - [3.3 Metric Integration Modes](#33-metric-integration-modes)
      - [Mode 1: **Evaluation** (No Trace)](#mode-1-evaluation-no-trace)
      - [Mode 2: **Optimization/Compilation** (With Trace)](#mode-2-optimizationcompilation-with-trace)
      - [Mode 3: **Metric as DSPy Program** (Self-Optimization)](#mode-3-metric-as-dspy-program-self-optimization)
    - [3.4 Rust Metric System Implementation](#34-rust-metric-system-implementation)
  - [4. Production-Ready Optimizers (Ranked)](#4-production-ready-optimizers-ranked)
    - [4.1 Production Readiness Matrix](#41-production-readiness-matrix)
    - [4.2 Decision Tree for Optimizer Selection](#42-decision-tree-for-optimizer-selection)
    - [4.3 Production Use Case Recommendations](#43-production-use-case-recommendations)
      - [Use Case 1: **Customer Support Bot**](#use-case-1-customer-support-bot)
      - [Use Case 2: **Code Generation Assistant**](#use-case-2-code-generation-assistant)
      - [Use Case 3: **Domain-Specific QA**](#use-case-3-domain-specific-qa)
      - [Use Case 4: **Multi-Turn Dialogue**](#use-case-4-multi-turn-dialogue)
  - [5. Rust BootstrapFewShot Implementation Analysis](#5-rust-bootstrapfewshot-implementation-analysis)
    - [5.1 Strengths of Current Implementation](#51-strengths-of-current-implementation)
    - [5.2 Gaps vs Python DSPy](#52-gaps-vs-python-dspy)
    - [5.3 Recommended Enhancements](#53-recommended-enhancements)
      - [Enhancement 1: **Trace Support**](#enhancement-1-trace-support)
      - [Enhancement 2: **Error Budget**](#enhancement-2-error-budget)
      - [Enhancement 3: **Statistics Tracking**](#enhancement-3-statistics-tracking)
  - [6. Rust Implementation Roadmap](#6-rust-implementation-roadmap)
    - [6.1 Priority Rankings](#61-priority-rankings)
    - [6.2 Detailed Implementation Plans](#62-detailed-implementation-plans)
      - [Plan 1: **KNNFewShot** (P0, 1 week)](#plan-1-knnfewshot-p0-1-week)
      - [Plan 2: **COPRO** (P0, 1-2 weeks)](#plan-2-copro-p0-1-2-weeks)
      - [Plan 3: **GEPA** (P0, 2-3 weeks)](#plan-3-gepa-p0-2-3-weeks)
      - [Plan 4: **BootstrapFewShotWithRandomSearch** (P1, 2-3 days)](#plan-4-bootstrapfewshotwithrandomsearch-p1-2-3-days)
    - [6.3 Implementation Order](#63-implementation-order)
  - [7. Mathematical Formulations Summary](#7-mathematical-formulations-summary)
    - [7.1 Core Optimization Objective](#71-core-optimization-objective)
    - [7.2 Optimizer-Specific Formulations](#72-optimizer-specific-formulations)
      - [BootstrapFewShot](#bootstrapfewshot)
      - [KNNFewShot](#knnfewshot)
      - [COPRO (Coordinate Ascent)](#copro-coordinate-ascent)
      - [MIPROv2 (Bayesian Optimization)](#miprov2-bayesian-optimization)
      - [GEPA (Pareto Frontier)](#gepa-pareto-frontier)
      - [BootstrapFinetune (Knowledge Distillation)](#bootstrapfinetune-knowledge-distillation)
  - [8. Recommendations](#8-recommendations)
    - [8.1 Immediate Actions (Next Sprint)](#81-immediate-actions-next-sprint)
    - [8.2 Medium-Term Goals (Next Month)](#82-medium-term-goals-next-month)
    - [8.3 Long-Term Goals (2+ Months)](#83-long-term-goals-2-months)
    - [8.4 Architectural Recommendations](#84-architectural-recommendations)
  - [9. Conclusion](#9-conclusion)
    - [Key Takeaways](#key-takeaways)
    - [Next Steps](#next-steps)
  - [Sources](#sources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Python DSPy Optimizer Research: Complete Analysis & Rust Implementation Roadmap

**Research Date**: 2026-01-11
**Repository**: https://github.com/stanfordnlp/dspy
**Current Rust Implementation**: `/home/user/ggen/crates/ggen-ai/src/dspy/optimizer.rs`

---

## Executive Summary

This document catalogs all Python DSPy optimizers, analyzes their algorithms, documents teacher/student patterns, evaluates the metric system, identifies production-ready optimizers, and provides recommendations for Rust implementation priorities.

**Key Findings**:
- **11 core optimizers** in Python DSPy ecosystem
- **GEPA** is the gold standard for 2026 (37.5% → 80% improvements, +42.5 points)
- **MIPROv2** excels for instruction-following with 200+ examples
- Our Rust **BootstrapFewShot** implementation is solid but represents only ~9% of optimizer landscape
- **Next priority**: Implement **COPRO** (instruction optimization) and **KNNFewShot** (retrieval-augmented)

---

## 1. Complete Optimizer Catalog

### 1.1 Automatic Few-Shot Learning Optimizers

#### A. **LabeledFewShot** (Simplest)
**Type**: Direct Example Selection
**Parameters**: `k` (num examples), `trainset`
**Algorithm**:
```
1. Select first k examples from labeled trainset
2. Format as few-shot demonstrations
3. Prepend to prompt
```

**Mathematical Formulation**:
```
D = {(x₁, y₁), (x₂, y₂), ..., (xₙ, yₙ)}  // Training set
P_optimized(x) = LLM(D[:k] + x)           // First k examples + query
```

**Use Case**: Quick baseline with manually labeled data
**Limitations**: No validation, no quality filtering, static selection
**Production Readiness**: ⭐⭐☆☆☆ (Baseline only)

---

#### B. **BootstrapFewShot** ✅ (Implemented in Rust)
**Type**: Teacher-Guided Bootstrap
**Parameters**: `metric`, `max_bootstrapped_demos`, `max_labeled_demos`, `teacher`
**Algorithm**:
```
For each example in trainset (up to max_bootstrapped_demos):
  1. Run teacher predictor (or student) on example.inputs
  2. Evaluate output using metric function
  3. If metric passes → save as demonstration
  4. Continue until max demonstrations collected

Return OptimizedPredictor with demonstrations in prompt
```

**Mathematical Formulation**:
```
T = teacher model, S = student model, M = metric function
D_bootstrap = {(x, T(x)) | M(x, T(x)) ≥ threshold, x ∈ D_train}
P_optimized(x) = S(D_bootstrap + x)
```

**Pseudocode**:
```python
def bootstrap_fewshot(student, teacher, trainset, metric, max_demos):
    demonstrations = []
    attempts = 0

    for example in trainset:
        if len(demonstrations) >= max_demos:
            break

        # Use teacher (or student if no teacher)
        predictor = teacher if teacher else student
        output = predictor.forward(example.inputs)

        # Validate with metric
        if metric(example, output):
            demonstrations.append(Demonstration(example.inputs, output))

        attempts += 1

    return OptimizedPredictor(student.signature, demonstrations)
```

**Rust Implementation Status**: ✅ **COMPLETE**
**Location**: `/home/user/ggen/crates/ggen-ai/src/dspy/optimizer.rs`
**Quality**: Production-ready with proper error handling, async support, tracing

**Comparison with Python DSPy**:
- ✅ Core algorithm identical
- ✅ Teacher/student pattern implemented
- ✅ Metric-based validation
- ✅ Demonstration formatting
- ✅ OptimizedPredictor with few-shot prompts
- ⚠️ Python version has more configuration options (e.g., `max_errors`)
- ⚠️ Python version supports more complex trace-based metrics

**Use Case**: ~10 examples, basic few-shot optimization
**Production Readiness**: ⭐⭐⭐⭐☆

---

#### C. **BootstrapFewShotWithRandomSearch**
**Type**: Bootstrap + Random Exploration
**Parameters**: Inherits from `BootstrapFewShot` + `num_candidate_programs`
**Algorithm**:
```
1. Bootstrap num_candidate demonstrations (>> max_demos)
2. For i = 1 to num_candidate_programs:
     a. Randomly sample K demos from candidates
     b. Create program with those K demos
     c. Evaluate on validation set
3. Return program with highest validation score
```

**Mathematical Formulation**:
```
D_candidates = {d₁, d₂, ..., dₙ} where n >> k
P_i = S(sample(D_candidates, k) + x) for i ∈ [1, num_programs]
P_optimal = argmax_{P_i} score(P_i, D_val)
```

**Pseudocode**:
```python
def bootstrap_fewshot_random_search(student, trainset, metric,
                                     num_candidate_programs=16,
                                     max_demos=4):
    # Bootstrap many candidate demos
    candidates = bootstrap_many(student, trainset, metric,
                                 num_candidate=num_candidate_programs * 3)

    best_program = None
    best_score = -float('inf')

    for _ in range(num_candidate_programs):
        # Random sample K demonstrations
        demo_subset = random.sample(candidates, max_demos)
        program = OptimizedPredictor(student.signature, demo_subset)

        # Evaluate on validation set
        score = evaluate(program, validation_set, metric)

        if score > best_score:
            best_score = score
            best_program = program

    return best_program
```

**Use Case**: 50+ examples, exploration of demo combinations
**Production Readiness**: ⭐⭐⭐⭐☆
**Rust Priority**: 🟡 Medium (extends existing BootstrapFewShot)

---

#### D. **BootstrapFewShotWithOptuna**
**Type**: Bootstrap + Bayesian Optimization
**Parameters**: Inherits from `BootstrapFewShot` + Optuna config
**Algorithm**:
```
1. Bootstrap num_candidate demonstrations
2. Use Bayesian Optimization (via Optuna) to find optimal subset:
   - Build probabilistic model: P(score | demo_selection)
   - Use acquisition function to select next demo combination
   - Iterate until convergence
3. Return program with optimal demo combination
```

**Mathematical Formulation**:
```
Objective: maximize E[metric(P(D_selected), x) | x ∈ D_val]
where D_selected ⊆ D_candidates, |D_selected| = k

Bayesian Optimization:
  Acquisition(D) = μ(D) + β·σ(D)  // Upper Confidence Bound
  D_optimal = argmax_{D⊆D_candidates} Acquisition(D)
```

**Pseudocode**:
```python
def bootstrap_fewshot_optuna(student, trainset, metric, max_demos=4):
    candidates = bootstrap_many(student, trainset, metric)

    def objective(trial):
        # Optuna selects indices
        indices = [trial.suggest_categorical(f'demo_{i}',
                   range(len(candidates))) for i in range(max_demos)]
        demo_subset = [candidates[i] for i in indices]

        program = OptimizedPredictor(student.signature, demo_subset)
        return evaluate(program, validation_set, metric)

    study = optuna.create_study(direction='maximize')
    study.optimize(objective, n_trials=50)

    best_indices = [study.best_params[f'demo_{i}']
                    for i in range(max_demos)]
    best_demos = [candidates[i] for i in best_indices]

    return OptimizedPredictor(student.signature, best_demos)
```

**Use Case**: Medium datasets (50-200 examples), high-quality demo selection
**Production Readiness**: ⭐⭐⭐⭐☆
**Rust Priority**: 🟡 Medium (requires Optuna/Bayesian opt library)

---

#### E. **KNNFewShot** 🎯
**Type**: Retrieval-Augmented Few-Shot
**Parameters**: `k` (neighbors), `trainset`, `vectorizer` (embedding function)
**Algorithm**:
```
1. Vectorize all training examples using vectorizer (e.g., SentenceTransformer)
2. For each input query:
   a. Vectorize query
   b. Find k nearest neighbors in training set (cosine similarity)
   c. Use neighbors as few-shot demonstrations
   d. Bootstrap with teacher/student if needed
3. Return program that retrieves demos dynamically per query
```

**Mathematical Formulation**:
```
V: embedding function (e.g., SentenceTransformer)
D_train = {(x₁, y₁), ..., (xₙ, yₙ)}

For query x:
  v_x = V(x)
  v_i = V(x_i) for all x_i ∈ D_train
  similarity(x, x_i) = cos(v_x, v_i) = (v_x · v_i) / (||v_x|| ||v_i||)

  neighbors = top_k({(x_i, y_i) | similarity(x, x_i)}, k)
  P(x) = LLM(neighbors + x)
```

**Pseudocode**:
```python
def knn_fewshot(trainset, k=3, vectorizer=sentence_transformer):
    # Pre-compute embeddings
    embeddings = [vectorizer(ex.inputs) for ex in trainset]

    class KNNPredictor:
        def forward(self, query):
            # Compute query embedding
            query_emb = vectorizer(query)

            # Find k nearest neighbors
            similarities = [cosine_similarity(query_emb, emb)
                           for emb in embeddings]
            top_k_indices = argsort(similarities)[-k:]

            # Use neighbors as demonstrations
            demos = [trainset[i] for i in top_k_indices]

            # Bootstrap if needed
            if use_bootstrap:
                demos = validate_demos(demos, metric)

            # Generate prompt with demos
            return llm(format_prompt(demos, query))

    return KNNPredictor()
```

**Use Case**: Dynamic demo selection, domain adaptation, query-specific optimization
**Production Readiness**: ⭐⭐⭐⭐⭐ (Excellent for production)
**Rust Priority**: 🟢 **HIGH** - Significant value for production systems

**Why High Priority**:
- Dynamic retrieval scales better than static demos
- Works with large training sets efficiently
- Excellent for domain-specific queries
- Combines well with vector databases (Qdrant, Weaviate)
- Production-proven in RAG systems

---

### 1.2 Automatic Instruction Optimization

#### F. **COPRO** (Contrastive Prompt Optimization) 🎯
**Type**: Instruction Generation + Coordinate Ascent
**Parameters**: `metric`, `depth` (iterations), `breadth` (candidates per iteration), `prompt_model`
**Algorithm**:
```
Phase 1: Initialization
  For each predictor in program:
    - Extract baseline instruction
    - Generate breadth-1 alternative instructions via prompt_model
    - Include original instruction as candidate

Phase 2: Iterative Refinement (Coordinate Ascent)
  For depth iterations:
    For each predictor:
      1. Evaluate all candidates on validation set
      2. Select best instruction
      3. Update predictor with best instruction

    Generate next batch of candidates:
      - Collect top performers from previous iteration
      - Format as few-shot examples with scores
      - Prompt model to "propose better instruction"

Phase 3: Return best program
```

**Mathematical Formulation**:
```
I = instruction space
P(I) = program with instruction I
S(I) = score on validation set

Coordinate Ascent:
  For t = 1 to depth:
    I_candidates = {I₁, I₂, ..., I_breadth}
    S_i = evaluate(P(I_i), D_val) for all I_i
    I_best = argmax_{I_i} S_i

    # Generate next candidates from history
    I_next = generate_from_top_k(I_history, k=3)

  Return P(I_best)
```

**Pseudocode**:
```python
class COPRO:
    def compile(self, student, trainset, metric, depth=3, breadth=10):
        program = student.deepcopy()

        # Phase 1: Initialize candidates
        for predictor in program.predictors():
            baseline = predictor.signature.instructions
            candidates = [baseline]

            # Generate breadth-1 alternatives
            prompt = f"Improve this instruction: {baseline}"
            for _ in range(breadth - 1):
                alt = self.prompt_model(prompt)
                candidates.append(alt)

            predictor.candidates = candidates

        # Phase 2: Coordinate ascent
        for iteration in range(depth):
            for predictor in program.predictors():
                scores = {}

                # Evaluate each candidate
                for candidate in predictor.candidates:
                    predictor.signature.instructions = candidate
                    score = evaluate(program, trainset, metric)
                    scores[candidate] = score

                # Select best
                best_instruction = max(scores, key=scores.get)
                predictor.signature.instructions = best_instruction

                # Generate next candidates from top performers
                top_3 = sorted(scores.items(), key=lambda x: x[1],
                              reverse=True)[:3]

                prompt = "Based on these instructions and scores:\n"
                for instr, score in top_3:
                    prompt += f"- {instr} (score: {score})\n"
                prompt += "Propose a better instruction."

                new_candidates = [self.prompt_model(prompt)
                                 for _ in range(breadth)]
                predictor.candidates = new_candidates

        return program
```

**Key Features**:
- **Deduplication**: Skips re-evaluation of previously scored instructions
- **Multi-Predictor**: Re-evaluates all historical candidates when downstream predictors change
- **Statistics Tracking**: Collects min/max/mean/stddev for optimization trajectory

**Use Case**: Instruction optimization, clear metric, no need for examples
**Production Readiness**: ⭐⭐⭐⭐☆
**Rust Priority**: 🟢 **HIGH** - Complements BootstrapFewShot (instructions vs examples)

**Why High Priority**:
- Orthogonal to BootstrapFewShot (optimizes instructions, not examples)
- Relatively simple algorithm (coordinate ascent)
- No complex dependencies (just LLM + metric)
- High impact for instruction-following tasks

---

#### G. **MIPROv2** (Multi-Prompt Instruction Proposal Optimizer v2)
**Type**: Joint Instruction + Example Optimization with Bayesian Search
**Parameters**: `metric`, `num_candidates`, `num_trials`, `minibatch`, `minibatch_size`
**Algorithm**:
```
Stage 1: Bootstrap Few-Shot Examples
  - Randomly sample examples from training set
  - Run through program
  - Keep valid examples (correct outputs)
  - Generate num_candidates demo sets

Stage 2: Grounded Proposal Stage
  For each predictor:
    - Analyze dataset properties
    - Inspect program code
    - Review bootstrapped examples
    - Generate instruction candidates with random tips
      ("be creative", "be concise", "think step-by-step")

Stage 3: Discrete Search with Bayesian Optimization
  For num_trials iterations:
    - Use Bayesian Opt to select (instruction, demos) combination
    - Evaluate on validation set (or minibatch)
    - Update probabilistic model
    - Every minibatch_full_eval_steps: evaluate on full validation

  Return best (instruction, demos) configuration
```

**Mathematical Formulation**:
```
Search Space:
  I = {I₁, I₂, ..., I_m}  // Instruction candidates
  D = {D₁, D₂, ..., D_n}  // Demo set candidates

Objective:
  maximize E[metric(P(I_i, D_j), x) | x ∈ D_val]

Bayesian Optimization:
  Surrogate Model: GP(μ, σ) over (instruction, demos) → score
  Acquisition: EI(I, D) = E[max(0, f(I,D) - f_best)]

  For t = 1 to num_trials:
    (I_t, D_t) = argmax_{I,D} Acquisition(I, D)
    score_t = evaluate(P(I_t, D_t), D_val)
    Update GP with (I_t, D_t, score_t)
```

**Pseudocode**:
```python
class MIPROv2:
    def compile(self, student, trainset, metric,
                num_candidates=10, num_trials=40):
        # Stage 1: Bootstrap examples
        demo_candidates = []
        for _ in range(num_candidates):
            demos = bootstrap_valid_examples(student, trainset)
            demo_candidates.append(demos)

        # Stage 2: Generate instruction candidates
        instruction_candidates = []
        for predictor in student.predictors():
            context = analyze_predictor_context(predictor, trainset,
                                                demo_candidates)

            for tip in ["be creative", "be concise", "think step-by-step"]:
                prompt = f"Given context: {context}\nTip: {tip}\n"
                prompt += "Generate improved instruction."
                instr = self.prompt_model(prompt)
                instruction_candidates.append(instr)

        # Stage 3: Bayesian search
        from sklearn.gaussian_process import GaussianProcessRegressor
        gp = GaussianProcessRegressor()

        evaluated = []
        best_config = None
        best_score = -float('inf')

        for trial in range(num_trials):
            if trial < 5:  # Random exploration
                instr = random.choice(instruction_candidates)
                demos = random.choice(demo_candidates)
            else:  # Exploitation via acquisition
                instr, demos = select_via_acquisition(gp,
                                    instruction_candidates,
                                    demo_candidates)

            # Evaluate (with minibatching)
            if self.minibatch and trial % self.minibatch_full_eval_steps != 0:
                score = evaluate(student,
                                validation_minibatch, metric)
            else:
                score = evaluate(student, validation_full, metric)

            evaluated.append((instr, demos, score))
            gp.fit(evaluated)

            if score > best_score:
                best_score = score
                best_config = (instr, demos)

        # Apply best configuration
        instr, demos = best_config
        for predictor in student.predictors():
            predictor.signature.instructions = instr
            predictor.demonstrations = demos

        return student
```

**Key Features**:
- **Data-Aware Generation**: Uses dataset properties to inform instruction proposals
- **Demonstration-Aware**: Leverages bootstrapped examples during instruction generation
- **Minibatching**: Evaluates on subset initially, full validation periodically
- **Joint Optimization**: Optimizes instructions and examples together

**Use Case**: 200+ examples, 40+ trials, willing to invest in long optimization
**Production Readiness**: ⭐⭐⭐⭐⭐ (Best for complex tasks)
**Rust Priority**: 🟡 Medium (complex, requires Bayesian opt + significant engineering)

---

#### H. **SIMBA** (Stochastic Introspective Mini-Batch Ascent)
**Type**: Mini-Batch + Reflective Learning
**Parameters**: `metric`, `batch_size`, `num_batches`
**Algorithm**:
```
1. Stochastic Mini-Batch Sampling:
   - Sample challenging examples with high output variability

2. Introspective Analysis:
   - Run LLM to analyze failures
   - Generate self-reflective improvement rules

3. Add Successful Demonstrations:
   - Include demos that pass metric

4. Iterate across mini-batches
```

**Mathematical Formulation**:
```
Variance-Based Sampling:
  var(x) = variance of outputs over multiple runs on x
  P_sample(x) ∝ var(x)  // Sample high-variance examples

Batch Update:
  For each mini-batch B:
    failures = {x ∈ B | metric(P(x)) < threshold}
    reflection = LLM("Why did these fail: {failures}")
    rules = extract_rules(reflection)
    P_updated = apply_rules(P, rules)
```

**Use Case**: Large datasets, performance-critical, statistical optimization preferred
**Production Readiness**: ⭐⭐⭐⭐☆
**Rust Priority**: 🔴 Low (complex, requires variance analysis + reflective LLM)

---

#### I. **GEPA** (Genetic-Pareto Algorithm) 🏆
**Type**: Reflective Evolution + Pareto Frontier
**Status**: **GOLD STANDARD for 2026**
**Parameters**: `metric`, `num_scenarios` (3-10), `num_iterations`
**Algorithm**:
```
1. Pareto Frontier Maintenance:
   - Maintain set of candidates achieving highest score on ≥1 eval instance
   - Prevent local optima by preserving diversity

2. Reflective Prompt Evolution:
   - Capture full execution traces
   - Identify predictor-specific behavior
   - Reflect on "what worked, what didn't"
   - Propose improved prompts addressing gaps

3. Iterative Mutation:
   - Generate variants via LLM reflection
   - Evaluate on scenarios
   - Update Pareto frontier
   - Select from frontier for next iteration
```

**Mathematical Formulation**:
```
Pareto Frontier:
  P_frontier = {P_i | ∃x: score(P_i, x) ≥ score(P_j, x) ∀j≠i}

Reflective Evolution:
  trace(P, x) = {(step₁, in₁, out₁), ..., (stepₙ, inₙ, outₙ)}
  reflection = LLM("Analyze trace: what worked, what failed?")
  improvements = extract_improvements(reflection)

  P_new = mutate(P, improvements)

Pareto Selection:
  For P_new:
    If ∃x: score(P_new, x) > max_{P∈frontier} score(P, x):
      frontier = frontier ∪ {P_new}
    Remove dominated candidates from frontier
```

**Pseudocode**:
```python
class GEPA:
    def compile(self, student, scenarios, metric, num_iterations=10):
        # Initialize Pareto frontier
        frontier = [student.deepcopy()]

        for iteration in range(num_iterations):
            # Sample from frontier
            parent = random.choice(frontier)

            # Reflective mutation
            for predictor in parent.predictors():
                # Capture traces
                traces = []
                for scenario in scenarios:
                    trace = run_with_trace(predictor, scenario)
                    traces.append((scenario, trace))

                # Analyze failures
                failures = [t for s, t in traces
                           if not metric(s, t.output)]
                successes = [t for s, t in traces
                            if metric(s, t.output)]

                # Reflective prompt
                reflection_prompt = f"""
                Analyze this predictor's behavior:

                Successes: {format_traces(successes)}
                Failures: {format_traces(failures)}

                What worked? What didn't?
                Propose an improved instruction addressing the gaps.
                """

                improved_instruction = self.prompt_model(reflection_prompt)
                predictor.signature.instructions = improved_instruction

            # Evaluate on all scenarios
            scores = [metric(scenario, parent(scenario))
                     for scenario in scenarios]

            # Update Pareto frontier
            parent.scores = scores
            frontier = update_pareto_frontier(frontier, parent)

            # Prune dominated candidates
            frontier = remove_dominated(frontier)

        # Return best from frontier (highest average)
        return max(frontier, key=lambda p: mean(p.scores))
```

**Performance**:
- **37.5% → 80%** on DSPy agents (+42.5 points)
- **+10% average** improvement over GRPO
- **+19%** on Qwen3 8B with **35× fewer rollouts** than GRPO

**Use Case**: Sample-efficient (3-10 scenarios), complex reasoning, domain-specific
**Production Readiness**: ⭐⭐⭐⭐⭐ (BEST for 2026)
**Rust Priority**: 🟢 **HIGHEST** - State-of-the-art, proven results

**Why Highest Priority**:
- Best performance in 2026
- Sample efficient (works with small datasets)
- Reflective learning (self-improving)
- Pareto frontier prevents overfitting
- Outperforms RL methods (GRPO) with far fewer rollouts

---

### 1.3 Automatic Finetuning

#### J. **BootstrapFinetune**
**Type**: Distillation to Weights
**Parameters**: `metric`, `teacher`, `train_kwargs` (epochs, batch_size, learning_rate, etc.)
**Algorithm**:
```
1. Use teacher program to create training dataset:
   - Run teacher on training inputs
   - Collect (input, teacher_output) pairs

2. Finetune student LM weights:
   - Standard supervised learning
   - Train on (input, teacher_output) dataset

3. Replace student's LM with finetuned version
```

**Mathematical Formulation**:
```
Teacher T generates dataset:
  D_finetune = {(x, T(x)) | x ∈ D_train}

Supervised Finetuning:
  θ* = argmin_θ Σ_{(x,y)∈D_finetune} Loss(LLM_θ(x), y)

  Loss = CrossEntropy for text generation

Distilled Program:
  P_distilled(x) = LLM_θ*(x)  // No prompting needed
```

**Pseudocode**:
```python
class BootstrapFinetune:
    def compile(self, student, teacher, trainset, metric,
                num_epochs=3, batch_size=8, learning_rate=2e-5):
        # Generate training data
        finetune_dataset = []
        for example in trainset:
            teacher_output = teacher.forward(example.inputs)

            # Only use if metric passes
            if metric(example, teacher_output):
                finetune_dataset.append({
                    'input': format_input(example.inputs),
                    'output': format_output(teacher_output)
                })

        # Finetune student LM
        from transformers import Trainer, TrainingArguments

        training_args = TrainingArguments(
            output_dir='./finetuned_model',
            num_train_epochs=num_epochs,
            per_device_train_batch_size=batch_size,
            learning_rate=learning_rate,
            logging_steps=10,
        )

        trainer = Trainer(
            model=student.lm,
            args=training_args,
            train_dataset=finetune_dataset,
        )

        trainer.train()

        # Replace student's LM with finetuned version
        student.lm = trainer.model

        return student
```

**Use Case**: Production efficiency, distill large model → small model
**Production Readiness**: ⭐⭐⭐⭐⭐ (Excellent for deployment)
**Rust Priority**: 🟡 Medium (requires ML training framework)

---

### 1.4 Program Transformations

#### K. **Ensemble**
**Type**: Multi-Program Combination
**Parameters**: `programs` (list of optimized programs), `size` (subset size)
**Algorithm**:
```
1. Input: List of optimized DSPy programs
2. For inference:
   - Option A: Run all programs, aggregate outputs (majority vote, averaging)
   - Option B: Randomly sample subset, aggregate
3. Return aggregated result
```

**Mathematical Formulation**:
```
Programs: P = {P₁, P₂, ..., Pₙ}

Majority Vote (classification):
  P_ensemble(x) = mode({P_i(x) | P_i ∈ P})

Averaging (regression/scoring):
  P_ensemble(x) = (1/n) Σ_{i=1}^n P_i(x)

Random Subset (size k):
  S ~ sample(P, k)
  P_ensemble(x) = aggregate({P_i(x) | P_i ∈ S})
```

**Use Case**: Improve robustness, scale inference-time compute
**Production Readiness**: ⭐⭐⭐⭐☆
**Rust Priority**: 🔴 Low (simple aggregation, low priority)

---

## 2. Teacher/Student Patterns & Demonstration Collection

### 2.1 Core Teacher/Student Paradigm

**Definition**: Use a "teacher" model (powerful or pre-optimized) to generate training examples for a "student" model (target deployment model).

**Pattern Types**:

#### A. **Self-Teaching** (Student = Teacher)
```rust
let student = Predictor::new(signature);
let optimizer = BootstrapFewShot::new(metric);
// No teacher specified → student teaches itself
let optimized = optimizer.compile(&student, &trainset).await?;
```

**Use Case**: Single model, self-improvement through validation

#### B. **External Teacher** (Powerful Model → Weaker Student)
```rust
let teacher = Arc::new(Predictor::new(signature)
    .with_model("gpt-4")           // Powerful
    .with_temperature(0.0));        // Deterministic

let student = Predictor::new(signature)
    .with_model("llama-3-8b");     // Efficient

let optimizer = BootstrapFewShot::new(metric)
    .with_teacher(teacher);

let optimized = optimizer.compile(&student, &trainset).await?;
```

**Use Case**: Knowledge distillation, cost optimization

#### C. **Pre-Compiled Teacher** (Optimized Program → New Student)
```rust
// First optimize a teacher
let teacher_optimized = optimizer1.compile(&teacher, &trainset1).await?;

// Use optimized teacher for new student
let optimizer2 = BootstrapFewShot::new(metric)
    .with_teacher(Arc::new(teacher_optimized));

let student_optimized = optimizer2.compile(&student, &trainset2).await?;
```

**Use Case**: Transfer learning, domain adaptation

---

### 2.2 Demonstration Collection Strategies

#### Strategy 1: **Greedy Collection** (BootstrapFewShot)
```
For each example in trainset:
  Run teacher → Get output → Validate → If pass, add demo
  Stop when max_demos reached
```

**Pros**: Simple, fast
**Cons**: May miss diverse examples, order-dependent

#### Strategy 2: **Reservoir Sampling** (Improved BootstrapFewShot)
```
collected = []
for i, example in enumerate(trainset):
  output = teacher(example)
  if metric(example, output):
    if len(collected) < max_demos:
      collected.append(demo)
    else:
      # Replace random demo with probability k/i
      j = random.randint(0, i)
      if j < max_demos:
        collected[j] = demo
```

**Pros**: Uniform sampling from all valid demos
**Cons**: Slightly more complex

#### Strategy 3: **Diversity-Based** (KNNFewShot)
```
collected = []
embeddings = []

for example in trainset:
  output = teacher(example)
  if metric(example, output):
    demo_emb = embed(example)

    # Only add if sufficiently different
    if all(cosine_similarity(demo_emb, e) < threshold
           for e in embeddings):
      collected.append(demo)
      embeddings.append(demo_emb)
```

**Pros**: Diverse demonstrations
**Cons**: Requires embedding model

#### Strategy 4: **Hard Example Mining** (SIMBA)
```
# Collect examples with high variance
variances = {}
for example in trainset:
  outputs = [teacher(example) for _ in range(5)]
  variances[example] = variance(outputs)

# Sort by variance (hard examples)
hard_examples = sorted(trainset, key=lambda e: variances[e],
                       reverse=True)

collected = []
for example in hard_examples[:max_demos]:
  output = teacher(example)
  if metric(example, output):
    collected.append(demo)
```

**Pros**: Focuses on challenging cases
**Cons**: Expensive (multiple runs per example)

---

### 2.3 Demonstration Validation

**Metric-Based Validation** (All Bootstrap Optimizers):
```python
def validate_demonstration(example, output, metric):
    """
    Validate if teacher output is acceptable for demonstration

    Args:
        example: Training example with inputs and expected outputs
        output: Teacher's prediction
        metric: Evaluation function

    Returns:
        bool: True if output passes metric, False otherwise
    """
    return metric(example, output)
```

**Trace-Based Validation** (Advanced):
```python
def validate_with_trace(example, output, trace, metric):
    """
    Validate using full execution trace

    Enables:
    - Intermediate step validation
    - Multi-hop reasoning checks
    - Chain-of-thought verification

    Args:
        example: Training example
        output: Final output
        trace: [(step, input, output), ...] execution trace
        metric: Function(example, output, trace) -> bool

    Returns:
        bool: True if trace is valid
    """
    if trace is None:
        # Evaluation mode: return float score
        return metric(example, output)
    else:
        # Bootstrapping mode: return bool
        # Can check intermediate steps
        for step, step_input, step_output in trace:
            if not validate_step(step, step_input, step_output):
                return False
        return metric(example, output)
```

**Our Rust Implementation**:
```rust
// Current: Simple metric-based validation
pub type MetricFn = Arc<dyn Fn(&Example, &HashMap<String, Value>)
                       -> Result<bool, ModuleError> + Send + Sync>;

// Future: Trace-based validation
pub struct Trace {
    pub steps: Vec<TraceStep>,
}

pub struct TraceStep {
    pub predictor: String,
    pub inputs: HashMap<String, Value>,
    pub outputs: HashMap<String, Value>,
}

pub type TraceMetricFn = Arc<dyn Fn(&Example, &HashMap<String, Value>,
                                    Option<&Trace>)
                             -> Result<bool, ModuleError> + Send + Sync>;
```

---

## 3. Metric System & Evaluation Integration

### 3.1 Metric Function Signature

**Basic Metric** (Evaluation Mode):
```python
def metric(example, pred) -> float | int | bool:
    """
    Args:
        example: Training/validation example with expected outputs
        pred: Model's prediction (output dict)

    Returns:
        Score (higher is better)
    """
    expected = example.outputs.get("answer")
    actual = pred.get("answer")
    return 1.0 if expected == actual else 0.0
```

**Advanced Metric** (Trace-Aware):
```python
def metric(example, pred, trace=None) -> float | bool:
    """
    Args:
        example: Training example
        pred: Model's prediction
        trace: Optional execution trace (for optimization)

    Returns:
        - float if trace is None (evaluation mode)
        - bool if trace provided (bootstrapping mode)
    """
    if trace is None:
        # Evaluation: return numerical score
        return compute_score(example, pred)
    else:
        # Bootstrapping: validate intermediate steps
        for step in trace:
            if not validate_step(step):
                return False
        return compute_score(example, pred) > threshold
```

### 3.2 Common Metric Patterns

#### A. **Exact Match**
```python
def exact_match(example, pred):
    return example.outputs["answer"] == pred["answer"]
```

**Rust Implementation**:
```rust
let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
    Ok(example.outputs.get("answer") == output.get("answer"))
});
```

#### B. **Fuzzy Match** (Edit Distance)
```python
from difflib import SequenceMatcher

def fuzzy_match(example, pred, threshold=0.8):
    expected = example.outputs["answer"]
    actual = pred["answer"]
    ratio = SequenceMatcher(None, expected, actual).ratio()
    return ratio >= threshold
```

**Rust Implementation**:
```rust
use strsim::normalized_damerau_levenshtein;

let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
    let expected = example.outputs.get("answer")
        .and_then(|v| v.as_str())?;
    let actual = output.get("answer")
        .and_then(|v| v.as_str())?;

    let similarity = normalized_damerau_levenshtein(expected, actual);
    Ok(similarity >= 0.8)
});
```

#### C. **Contains Match** (Substring)
```python
def contains_match(example, pred):
    expected = example.outputs["answer"].lower()
    actual = pred["answer"].lower()
    return expected in actual or actual in expected
```

#### D. **Regex Match**
```python
import re

def regex_match(example, pred):
    pattern = example.outputs["pattern"]
    text = pred["answer"]
    return re.search(pattern, text) is not None
```

#### E. **LLM-as-Judge**
```python
def llm_judge(example, pred):
    judge_prompt = f"""
    Question: {example.inputs["question"]}
    Expected: {example.outputs["answer"]}
    Actual: {pred["answer"]}

    Is the actual answer correct? Respond with YES or NO.
    """

    response = llm(judge_prompt)
    return "YES" in response.upper()
```

#### F. **Multi-Criteria Metric**
```python
def multi_criteria(example, pred, trace=None):
    # Accuracy
    accuracy = exact_match(example, pred)

    # Conciseness
    word_count = len(pred["answer"].split())
    conciseness = 1.0 if word_count < 50 else 0.5

    # Intermediate step validation (if trace available)
    if trace:
        valid_steps = all(validate_step(s) for s in trace)
        if not valid_steps:
            return False if trace else 0.0

    # Combined score
    if trace is None:
        return accuracy * conciseness  # Evaluation mode
    else:
        return accuracy  # Bootstrapping mode
```

---

### 3.3 Metric Integration Modes

#### Mode 1: **Evaluation** (No Trace)
```python
# During dspy.Evaluate()
for example in devset:
    pred = program(example.inputs)
    score = metric(example, pred)  # trace=None
    scores.append(score)

average_score = sum(scores) / len(scores)
```

**When**: Assessing program performance on validation/test set
**Returns**: Numerical score (float/int)
**DSPy Behavior**: Does NOT track LM call traces

#### Mode 2: **Optimization/Compilation** (With Trace)
```python
# During optimizer.compile()
with dspy.tracing():
    for example in trainset:
        pred = program(example.inputs)
        trace = dspy.get_trace()  # Capture execution trace
        is_valid = metric(example, pred, trace)  # trace provided

        if is_valid:
            demonstrations.append(Demonstration(example, pred, trace))
```

**When**: Bootstrapping demonstrations, optimizing prompts
**Returns**: Boolean (valid/invalid)
**DSPy Behavior**: Tracks all LM calls, captures intermediate steps

#### Mode 3: **Metric as DSPy Program** (Self-Optimization)
```python
# Metric itself is a DSPy program
class SemanticSimilarityMetric(dspy.Module):
    def __init__(self):
        self.compare = dspy.ChainOfThought("expected, actual -> similarity")

    def forward(self, example, pred):
        result = self.compare(
            expected=example.outputs["answer"],
            actual=pred["answer"]
        )
        return float(result.similarity) > 0.8

# Optimize the metric itself!
metric = SemanticSimilarityMetric()
optimized_metric = optimizer.compile(metric, metric_trainset)
```

**When**: Complex semantic evaluation, domain-specific scoring
**Benefit**: Metric itself improves with optimization

---

### 3.4 Rust Metric System Implementation

**Current Implementation**:
```rust
// Basic metric (evaluation mode only)
pub type MetricFn = Arc<dyn Fn(&Example, &HashMap<String, Value>)
                       -> Result<bool, ModuleError> + Send + Sync>;

// Usage
let metric = Arc::new(|example: &Example, output: &HashMap<String, Value>| {
    Ok(example.outputs.get("answer") == output.get("answer"))
});

let optimizer = BootstrapFewShot::new(metric);
```

**Recommended Extensions**:
```rust
// 1. Add trace support
pub struct Trace {
    pub steps: Vec<TraceStep>,
}

pub struct TraceStep {
    pub predictor: String,
    pub inputs: HashMap<String, Value>,
    pub outputs: HashMap<String, Value>,
    pub timestamp: SystemTime,
}

// 2. Dual-mode metric
pub enum MetricMode {
    Evaluation,   // Returns float score
    Bootstrap,    // Returns bool validity
}

pub trait Metric: Send + Sync {
    fn evaluate(&self, example: &Example, output: &HashMap<String, Value>)
               -> Result<f64, ModuleError>;

    fn validate(&self, example: &Example, output: &HashMap<String, Value>,
                trace: Option<&Trace>)
               -> Result<bool, ModuleError>;
}

// 3. Built-in metrics
pub struct ExactMatch;
pub struct FuzzyMatch { threshold: f64 }
pub struct ContainsMatch;
pub struct RegexMatch { pattern: Regex }
pub struct LlmJudge { judge_model: String }

// 4. Composite metrics
pub struct MultiCriteriaMetric {
    criteria: Vec<Box<dyn Metric>>,
    weights: Vec<f64>,
}
```

---

## 4. Production-Ready Optimizers (Ranked)

### 4.1 Production Readiness Matrix

| Optimizer | Production Score | Data Needs | Cost | Complexity | Recommendation |
|-----------|-----------------|------------|------|------------|----------------|
| **GEPA** | ⭐⭐⭐⭐⭐ | 3-10 scenarios | Medium | Medium | **BEST for 2026** |
| **MIPROv2** | ⭐⭐⭐⭐⭐ | 200+ examples | High | High | Complex tasks, large datasets |
| **BootstrapFewShot** | ⭐⭐⭐⭐☆ | 10+ examples | Low | Low | **Quick start, baseline** |
| **BootstrapFinetune** | ⭐⭐⭐⭐⭐ | 100+ examples | High | Medium | **Production deployment** |
| **KNNFewShot** | ⭐⭐⭐⭐⭐ | 50+ examples | Low | Low | **Dynamic retrieval** |
| **COPRO** | ⭐⭐⭐⭐☆ | 20+ examples | Medium | Medium | Instruction-focused |
| **BootstrapFewShotWithOptuna** | ⭐⭐⭐⭐☆ | 50-200 examples | Medium | Medium | Quality over speed |
| **BootstrapFewShotWithRandomSearch** | ⭐⭐⭐⭐☆ | 50+ examples | Medium | Low | Exploration |
| **SIMBA** | ⭐⭐⭐⭐☆ | Large datasets | High | High | Large-scale systems |
| **LabeledFewShot** | ⭐⭐☆☆☆ | Any | Very Low | Very Low | Baseline only |
| **Ensemble** | ⭐⭐⭐⭐☆ | Multiple programs | High | Low | Robustness |

---

### 4.2 Decision Tree for Optimizer Selection

```
START
│
├─ Do you have < 10 examples?
│  ├─ YES → Use **GEPA** (sample efficient)
│  └─ NO → Continue
│
├─ Do you have 10-50 examples?
│  ├─ YES → Use **BootstrapFewShot** (quick baseline)
│  └─ NO → Continue
│
├─ Do you have 50-200 examples?
│  ├─ Need dynamic retrieval? → **KNNFewShot**
│  ├─ Need exploration? → **BootstrapFewShotWithRandomSearch**
│  ├─ Need quality? → **BootstrapFewShotWithOptuna**
│  └─ Instruction focus? → **COPRO**
│
├─ Do you have 200+ examples?
│  ├─ Willing to invest in long optimization? → **MIPROv2**
│  ├─ Large dataset (thousands)? → **SIMBA**
│  └─ Need production efficiency? → **BootstrapFinetune**
│
└─ For ANY dataset size in 2026 → Try **GEPA** first (best ROI)
```

---

### 4.3 Production Use Case Recommendations

#### Use Case 1: **Customer Support Bot**
**Requirements**: Low latency, high accuracy, 100+ examples
**Recommended Pipeline**:
```
1. Start: GEPA (3-10 examples) → Quick baseline
2. Expand: KNNFewShot (100 examples) → Dynamic retrieval
3. Deploy: BootstrapFinetune → Distill to small model
```

#### Use Case 2: **Code Generation Assistant**
**Requirements**: Complex reasoning, instruction-following
**Recommended Pipeline**:
```
1. Start: BootstrapFewShot → Quick baseline
2. Optimize: COPRO → Refine instructions
3. Scale: MIPROv2 (200+ examples) → Joint optimization
```

#### Use Case 3: **Domain-Specific QA**
**Requirements**: Domain adaptation, limited data
**Recommended Pipeline**:
```
1. Start: GEPA (5-10 domain examples) → Sample efficient
2. Expand: KNNFewShot → Retrieve similar domain examples
3. Refine: COPRO → Domain-specific instructions
```

#### Use Case 4: **Multi-Turn Dialogue**
**Requirements**: Context tracking, intermediate validation
**Recommended Pipeline**:
```
1. Start: BootstrapFewShot with trace-based metric
2. Optimize: GEPA with trace reflection
3. Ensemble: Multiple optimized programs for robustness
```

---

## 5. Rust BootstrapFewShot Implementation Analysis

### 5.1 Strengths of Current Implementation

✅ **Core Algorithm Fidelity**
- Faithful to Python DSPy BootstrapFewShot algorithm
- Proper teacher/student pattern
- Metric-based validation
- Demonstration bootstrapping

✅ **Production Quality**
```rust
// Error handling
pub async fn compile(&self, student: &dyn Module, trainset: &[Example])
    -> Result<OptimizedPredictor, ModuleError>

// Async native
async fn forward(&self, inputs: HashMap<String, Value>)
    -> ModuleResult<HashMap<String, Value>>

// Type safety
pub type MetricFn = Arc<dyn Fn(&Example, &HashMap<String, Value>)
                       -> Result<bool, ModuleError> + Send + Sync>;
```

✅ **Rust Idioms**
- Proper ownership/borrowing
- Zero unwrap/expect in production code
- Comprehensive error mapping
- Tracing instrumentation

✅ **Testing**
- Unit tests for all components
- Integration tests
- Example code
- Documentation

---

### 5.2 Gaps vs Python DSPy

⚠️ **Missing Features**:

1. **Trace-Based Metrics**
```python
# Python DSPy supports this
def metric(example, pred, trace=None):
    if trace:
        # Validate intermediate steps
        for step in trace:
            if not validate_step(step):
                return False
    return score(example, pred)
```

```rust
// Our Rust version doesn't support trace yet
pub type MetricFn = Arc<dyn Fn(&Example, &HashMap<String, Value>)
                       -> Result<bool, ModuleError> + Send + Sync>;
// Missing: trace parameter
```

2. **Error Budget/Max Errors**
```python
# Python DSPy has max_errors parameter
BootstrapFewShot(metric, max_bootstrapped_demos=4, max_errors=5)
# Stops if too many errors
```

```rust
// Our version tries max_attempts = trainset.len().min(max_demos * 3)
// No explicit error budget
```

3. **Labeled Demo Support**
```python
# Python DSPy supports pre-labeled demos
BootstrapFewShot(max_labeled_demos=16)
# Can use ground-truth labels directly
```

```rust
// Our version has max_labeled_demos field but doesn't use it yet
max_labeled_demos: usize,  // For future use
```

4. **Advanced Teacher Variants**
```python
# Python DSPy supports multiple teachers
teacher_programs = [teacher1, teacher2, teacher3]
BootstrapFewShot(teacher=teacher_programs)
```

5. **Metric Statistics**
```python
# Python DSPy tracks optimization statistics
optimizer.compile(student, trainset)
print(optimizer.statistics)  # Success rate, avg score, etc.
```

---

### 5.3 Recommended Enhancements

#### Enhancement 1: **Trace Support**
```rust
pub struct Trace {
    pub steps: Vec<TraceStep>,
    pub total_tokens: usize,
    pub latency_ms: u64,
}

pub struct TraceStep {
    pub predictor_name: String,
    pub inputs: HashMap<String, Value>,
    pub outputs: HashMap<String, Value>,
    pub timestamp: SystemTime,
}

pub trait Metric: Send + Sync {
    fn validate(&self, example: &Example, output: &HashMap<String, Value>,
                trace: Option<&Trace>) -> Result<bool, ModuleError>;

    fn evaluate(&self, example: &Example, output: &HashMap<String, Value>)
               -> Result<f64, ModuleError>;
}
```

#### Enhancement 2: **Error Budget**
```rust
pub struct BootstrapFewShot {
    metric: Arc<dyn Metric>,
    max_bootstrapped_demos: usize,
    max_errors: usize,  // Stop if too many failures
    teacher: Option<Arc<dyn Module>>,
}

pub async fn compile(&self, student: &dyn Module, trainset: &[Example])
    -> Result<OptimizedPredictor, ModuleError> {
    let mut demonstrations = Vec::new();
    let mut errors = 0;

    for example in trainset {
        match self.try_bootstrap(example).await {
            Ok(demo) => demonstrations.push(demo),
            Err(e) => {
                errors += 1;
                if errors >= self.max_errors {
                    return Err(ModuleError::TooManyErrors(errors));
                }
            }
        }
    }
    // ...
}
```

#### Enhancement 3: **Statistics Tracking**
```rust
pub struct OptimizationStatistics {
    pub total_attempts: usize,
    pub successful_demos: usize,
    pub failed_demos: usize,
    pub avg_metric_score: f64,
    pub optimization_time_ms: u64,
}

impl BootstrapFewShot {
    pub async fn compile_with_stats(&self, student: &dyn Module,
                                     trainset: &[Example])
        -> Result<(OptimizedPredictor, OptimizationStatistics), ModuleError> {
        // Track statistics during compilation
        // Return both optimized predictor and stats
    }
}
```

---

## 6. Rust Implementation Roadmap

### 6.1 Priority Rankings

| Priority | Optimizer | Rationale | Estimated Effort |
|----------|-----------|-----------|------------------|
| 🟢 **P0** | **GEPA** | Best 2026 performance, sample efficient | High (2-3 weeks) |
| 🟢 **P0** | **KNNFewShot** | High production value, retrieval-augmented | Medium (1 week) |
| 🟢 **P0** | **COPRO** | Complements BootstrapFewShot (instructions) | Medium (1-2 weeks) |
| 🟡 **P1** | **BootstrapFewShotWithRandomSearch** | Extends existing, simple | Low (2-3 days) |
| 🟡 **P1** | **Enhance BootstrapFewShot** | Trace support, error budget | Low (3-5 days) |
| 🟡 **P2** | **BootstrapFewShotWithOptuna** | Quality improvement | Medium (1 week) |
| 🟡 **P2** | **MIPROv2** | Complex but powerful | High (3-4 weeks) |
| 🔴 **P3** | **BootstrapFinetune** | Requires ML framework | Very High (4+ weeks) |
| 🔴 **P3** | **SIMBA** | Complex, large-scale only | High (2-3 weeks) |
| 🔴 **P4** | **Ensemble** | Simple aggregation, low ROI | Very Low (1 day) |
| 🔴 **P4** | **LabeledFewShot** | Too simple, low value | Very Low (1 day) |

---

### 6.2 Detailed Implementation Plans

#### Plan 1: **KNNFewShot** (P0, 1 week)

**Why P0**:
- High production value (dynamic retrieval)
- Scales well with large datasets
- Works with vector databases (Qdrant, Weaviate)
- Proven in RAG systems

**Algorithm**:
```rust
pub struct KNNFewShot {
    k: usize,
    trainset: Vec<Example>,
    vectorizer: Box<dyn Vectorizer>,
    embeddings: Vec<Vec<f32>>,
}

#[async_trait]
pub trait Vectorizer: Send + Sync {
    async fn embed(&self, text: &str) -> Result<Vec<f32>, VectorizerError>;
}

impl KNNFewShot {
    pub async fn compile(&self, student: &dyn Module, trainset: &[Example])
        -> Result<KNNPredictor, ModuleError> {
        // Precompute embeddings for all training examples
        let mut embeddings = Vec::new();
        for example in trainset {
            let text = format_example_text(example);
            let emb = self.vectorizer.embed(&text).await?;
            embeddings.push(emb);
        }

        Ok(KNNPredictor {
            signature: student.signature().clone(),
            trainset: trainset.to_vec(),
            embeddings,
            vectorizer: self.vectorizer.clone(),
            k: self.k,
        })
    }
}

pub struct KNNPredictor {
    signature: Signature,
    trainset: Vec<Example>,
    embeddings: Vec<Vec<f32>>,
    vectorizer: Box<dyn Vectorizer>,
    k: usize,
}

#[async_trait]
impl Module for KNNPredictor {
    async fn forward(&self, inputs: HashMap<String, Value>)
        -> ModuleResult<HashMap<String, Value>> {
        // 1. Embed query
        let query_text = format_inputs(&inputs);
        let query_emb = self.vectorizer.embed(&query_text).await?;

        // 2. Find k nearest neighbors
        let mut similarities: Vec<(usize, f32)> = self.embeddings
            .iter()
            .enumerate()
            .map(|(i, emb)| (i, cosine_similarity(&query_emb, emb)))
            .collect();

        similarities.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        let top_k_indices: Vec<usize> = similarities
            .iter()
            .take(self.k)
            .map(|(i, _)| *i)
            .collect();

        // 3. Retrieve demonstrations
        let demonstrations: Vec<Demonstration> = top_k_indices
            .iter()
            .map(|&i| {
                let ex = &self.trainset[i];
                Demonstration::new(ex.inputs.clone(), ex.outputs.clone())
            })
            .collect();

        // 4. Build prompt with retrieved demos
        let prompt = self.build_prompt(&inputs, &demonstrations)?;

        // 5. Call LLM
        let response = self.call_llm(&prompt).await?;

        // 6. Parse output
        self.parse_output(&response)
    }
}
```

**Dependencies**:
- Vectorizer trait (abstraction over embedding models)
- Cosine similarity function
- Optional: Integration with `fastembed-rs` or `rust-bert`

**Testing**:
- Unit tests for cosine similarity
- Integration test with mock vectorizer
- E2E test with real embedding model (optional)

**Timeline**:
- Day 1-2: Vectorizer trait + cosine similarity
- Day 3-4: KNNPredictor implementation
- Day 5: Testing + documentation
- Day 6-7: Integration with embedding libraries

---

#### Plan 2: **COPRO** (P0, 1-2 weeks)

**Why P0**:
- Complements BootstrapFewShot (optimizes instructions, not examples)
- High impact for instruction-following tasks
- Relatively simple algorithm (coordinate ascent)
- No complex dependencies

**Algorithm**:
```rust
pub struct COPRO {
    metric: Arc<dyn Metric>,
    depth: usize,       // Number of refinement iterations
    breadth: usize,     // Candidates per iteration
    prompt_model: String,
    temperature: f32,
}

impl COPRO {
    pub async fn compile(&self, student: &dyn Module, trainset: &[Example])
        -> Result<OptimizedPredictor, ModuleError> {
        let mut program = student.clone_module();

        // Phase 1: Initialize candidates
        let mut instruction_candidates = Vec::new();
        let baseline = program.signature().instructions.clone();
        instruction_candidates.push(baseline.clone());

        // Generate breadth-1 alternatives
        for _ in 0..(self.breadth - 1) {
            let alt = self.generate_instruction_variant(&baseline).await?;
            instruction_candidates.push(alt);
        }

        // Phase 2: Coordinate ascent
        for iteration in 0..self.depth {
            let mut scores = HashMap::new();

            // Evaluate each candidate
            for candidate in &instruction_candidates {
                let score = self.evaluate_instruction(
                    &program,
                    candidate,
                    trainset
                ).await?;
                scores.insert(candidate.clone(), score);
            }

            // Select best
            let best_instruction = scores
                .iter()
                .max_by(|a, b| a.1.partial_cmp(b.1).unwrap())
                .map(|(instr, _)| instr.clone())
                .unwrap();

            // Update program
            program.set_instructions(best_instruction.clone());

            // Generate next candidates from top performers
            let top_3: Vec<_> = scores
                .iter()
                .sorted_by(|a, b| b.1.partial_cmp(a.1).unwrap())
                .take(3)
                .collect();

            instruction_candidates = self.generate_next_batch(&top_3).await?;

            info!("COPRO iteration {}/{}: best score = {}",
                  iteration + 1, self.depth, scores[&best_instruction]);
        }

        Ok(program)
    }

    async fn generate_instruction_variant(&self, baseline: &str)
        -> Result<String, ModuleError> {
        let prompt = format!(
            "Given this instruction: \"{}\"\n\
             Propose an improved instruction that will lead a good language \
             model to perform the task even better.",
            baseline
        );

        // Call LLM to generate variant
        let response = self.call_llm(&prompt).await?;
        Ok(response.trim().to_string())
    }

    async fn evaluate_instruction(&self, program: &dyn Module,
                                   instruction: &str,
                                   trainset: &[Example])
        -> Result<f64, ModuleError> {
        // Temporarily update instruction
        let mut temp_program = program.clone_module();
        temp_program.set_instructions(instruction.to_string());

        // Evaluate on trainset
        let mut total_score = 0.0;
        for example in trainset {
            let output = temp_program.forward(example.inputs.clone()).await?;
            let score = self.metric.evaluate(example, &output)?;
            total_score += score;
        }

        Ok(total_score / trainset.len() as f64)
    }

    async fn generate_next_batch(&self, top_performers: &[(String, f64)])
        -> Result<Vec<String>, ModuleError> {
        let prompt = "Based on these instructions and their scores:\n".to_string();
        for (instr, score) in top_performers {
            prompt.push_str(&format!("- \"{}\" (score: {:.2})\n", instr, score));
        }
        prompt.push_str("\nPropose new improved instructions.");

        // Generate breadth new candidates
        let mut candidates = Vec::new();
        for _ in 0..self.breadth {
            let candidate = self.call_llm(&prompt).await?;
            candidates.push(candidate.trim().to_string());
        }

        Ok(candidates)
    }
}
```

**Key Features**:
- Deduplication cache (avoid re-evaluating same instructions)
- Statistics tracking (min/max/mean scores per iteration)
- Early stopping (if no improvement for N iterations)

**Dependencies**:
- LLM client (genai-rs)
- Metric trait (enhanced version)

**Testing**:
- Mock LLM for unit tests
- Integration test with real LLM
- Verify coordinate ascent convergence

**Timeline**:
- Day 1-2: Core COPRO structure + instruction generation
- Day 3-4: Evaluation + coordinate ascent logic
- Day 5-6: Deduplication + statistics
- Day 7: Testing + documentation
- Day 8-10: Integration + polish

---

#### Plan 3: **GEPA** (P0, 2-3 weeks)

**Why P0**:
- **Best performance in 2026** (+42.5 points improvement)
- Sample efficient (3-10 scenarios)
- Outperforms GRPO by +10% with 35× fewer rollouts
- Reflective learning (self-improving)

**Algorithm**:
```rust
pub struct GEPA {
    metric: Arc<dyn Metric>,
    num_scenarios: usize,     // 3-10 typical
    num_iterations: usize,
    prompt_model: String,
}

pub struct ParetoFrontier {
    candidates: Vec<ParetoCandidate>,
}

pub struct ParetoCandidate {
    program: Box<dyn Module>,
    scores: Vec<f64>,  // Score per scenario
}

impl GEPA {
    pub async fn compile(&self, student: &dyn Module,
                         scenarios: &[Example])
        -> Result<Box<dyn Module>, ModuleError> {
        // Initialize Pareto frontier with baseline
        let mut frontier = ParetoFrontier::new();
        frontier.add(ParetoCandidate {
            program: student.clone_module(),
            scores: self.evaluate_all_scenarios(student, scenarios).await?,
        });

        for iteration in 0..self.num_iterations {
            // Sample parent from frontier
            let parent = frontier.sample();

            // Reflective mutation
            let mutated = self.reflective_mutate(parent, scenarios).await?;

            // Evaluate on all scenarios
            let scores = self.evaluate_all_scenarios(&mutated, scenarios).await?;

            // Update Pareto frontier
            let candidate = ParetoCandidate {
                program: mutated,
                scores,
            };

            frontier.add(candidate);
            frontier.prune_dominated();

            info!("GEPA iteration {}/{}: frontier size = {}",
                  iteration + 1, self.num_iterations, frontier.size());
        }

        // Return best from frontier (highest average score)
        Ok(frontier.best_by_average())
    }

    async fn reflective_mutate(&self, parent: &dyn Module,
                                scenarios: &[Example])
        -> Result<Box<dyn Module>, ModuleError> {
        let mut mutated = parent.clone_module();

        for predictor in mutated.predictors_mut() {
            // Capture traces
            let mut traces = Vec::new();
            for scenario in scenarios {
                let trace = self.run_with_trace(predictor, scenario).await?;
                traces.push((scenario, trace));
            }

            // Separate successes and failures
            let (successes, failures): (Vec<_>, Vec<_>) = traces
                .into_iter()
                .partition(|(s, t)| {
                    self.metric.validate(s, &t.outputs, Some(&t.trace)).unwrap_or(false)
                });

            // Reflective analysis
            let reflection_prompt = format!(
                "Analyze this predictor's behavior:\n\n\
                 Successes ({}):\n{}\n\n\
                 Failures ({}):\n{}\n\n\
                 What worked? What didn't? \
                 Propose an improved instruction addressing the gaps.",
                successes.len(),
                format_traces(&successes),
                failures.len(),
                format_traces(&failures)
            );

            let improved_instruction = self.call_llm(&reflection_prompt).await?;
            predictor.set_instructions(improved_instruction);
        }

        Ok(mutated)
    }

    async fn run_with_trace(&self, predictor: &dyn Module,
                             scenario: &Example)
        -> Result<ExecutionTrace, ModuleError> {
        // Enable tracing
        let trace = Trace::new();

        // Run predictor
        let output = predictor.forward(scenario.inputs.clone()).await?;

        Ok(ExecutionTrace {
            inputs: scenario.inputs.clone(),
            outputs: output,
            trace,
        })
    }
}

impl ParetoFrontier {
    fn add(&mut self, candidate: ParetoCandidate) {
        // Check if candidate dominates any existing or is dominated
        let is_dominated = self.candidates.iter().any(|existing| {
            Self::dominates(existing, &candidate)
        });

        if !is_dominated {
            self.candidates.push(candidate);
        }
    }

    fn dominates(a: &ParetoCandidate, b: &ParetoCandidate) -> bool {
        // a dominates b if a is ≥ b on all scenarios and > on at least one
        let all_geq = a.scores.iter().zip(&b.scores)
            .all(|(a_score, b_score)| a_score >= b_score);
        let any_greater = a.scores.iter().zip(&b.scores)
            .any(|(a_score, b_score)| a_score > b_score);

        all_geq && any_greater
    }

    fn prune_dominated(&mut self) {
        let mut i = 0;
        while i < self.candidates.len() {
            let is_dominated = self.candidates.iter().enumerate()
                .any(|(j, other)| i != j && Self::dominates(other, &self.candidates[i]));

            if is_dominated {
                self.candidates.remove(i);
            } else {
                i += 1;
            }
        }
    }
}
```

**Key Features**:
- Pareto frontier maintenance
- Reflective mutation via LLM
- Trace-based analysis
- Sample efficiency (3-10 scenarios)

**Dependencies**:
- Trace system (see Enhancement 1)
- LLM client for reflection
- Module cloning trait

**Testing**:
- Pareto dominance logic
- Frontier pruning
- Reflective mutation with mock LLM
- E2E with real scenarios

**Timeline**:
- Week 1: Pareto frontier + dominance logic
- Week 2: Reflective mutation + trace analysis
- Week 3: Integration + testing + documentation

---

#### Plan 4: **BootstrapFewShotWithRandomSearch** (P1, 2-3 days)

**Why P1**:
- Extends existing BootstrapFewShot (low effort)
- Exploration of demo combinations
- Proven effective for 50+ examples

**Algorithm**:
```rust
pub struct BootstrapFewShotWithRandomSearch {
    base: BootstrapFewShot,
    num_candidate_programs: usize,  // 16 typical
}

impl BootstrapFewShotWithRandomSearch {
    pub async fn compile(&self, student: &dyn Module,
                         trainset: &[Example],
                         validation: &[Example])
        -> Result<OptimizedPredictor, ModuleError> {
        // Step 1: Bootstrap many candidates (num_candidate_programs * 3)
        let candidates = self.base.bootstrap_many(
            student,
            trainset,
            self.num_candidate_programs * 3
        ).await?;

        // Step 2: Random search over demo combinations
        let mut best_program = None;
        let mut best_score = -f64::INFINITY;

        for _ in 0..self.num_candidate_programs {
            // Random sample K demonstrations
            let demo_subset = self.random_sample(&candidates,
                                                self.base.max_bootstrapped_demos);

            let program = OptimizedPredictor::new(
                student.signature().clone(),
                demo_subset
            );

            // Evaluate on validation set
            let score = self.evaluate(&program, validation).await?;

            if score > best_score {
                best_score = score;
                best_program = Some(program);
            }
        }

        Ok(best_program.unwrap())
    }
}
```

**Timeline**:
- Day 1: Extend BootstrapFewShot with random sampling
- Day 2: Validation evaluation + selection
- Day 3: Testing + documentation

---

### 6.3 Implementation Order

**Phase 1: Core Enhancements** (Week 1-2)
1. Enhance BootstrapFewShot: Trace support, error budget, statistics
2. Implement KNNFewShot: High production value, retrieval-augmented

**Phase 2: Instruction Optimization** (Week 3-4)
3. Implement COPRO: Instruction refinement via coordinate ascent
4. Implement BootstrapFewShotWithRandomSearch: Demo exploration

**Phase 3: Advanced Optimizers** (Week 5-7)
5. Implement GEPA: Best 2026 performance, reflective learning
6. Implement BootstrapFewShotWithOptuna: Bayesian optimization

**Phase 4: Production Deployment** (Week 8+)
7. Implement BootstrapFinetune: Weight distillation for efficiency
8. Implement MIPROv2: Joint optimization for complex tasks

---

## 7. Mathematical Formulations Summary

### 7.1 Core Optimization Objective

**General Form**:
```
θ* = argmax_θ E_{x~D_val}[metric(P_θ(x), y_true)]

where:
  θ = program parameters (prompts, demonstrations, instructions)
  P_θ = DSPy program with parameters θ
  D_val = validation set
  metric = evaluation function
```

### 7.2 Optimizer-Specific Formulations

#### BootstrapFewShot
```
D_bootstrap = {(x, T(x)) | M(x, T(x)) ≥ τ, x ∈ D_train}
P_optimized(x) = S(D_bootstrap + x)

where:
  T = teacher model
  S = student model
  M = metric function
  τ = acceptance threshold
```

#### KNNFewShot
```
For query x:
  v_x = V(x)
  similarity(x, x_i) = cos(v_x, v_i) = (v_x · v_i) / (||v_x|| ||v_i||)
  neighbors = top_k({(x_i, y_i) | similarity(x, x_i)}, k)
  P(x) = LLM(neighbors + x)

where:
  V = embedding function
  k = number of neighbors
```

#### COPRO (Coordinate Ascent)
```
For t = 1 to depth:
  I_candidates = {I₁, I₂, ..., I_breadth}
  S_i = E_{x∈D_train}[M(P(I_i)(x))]
  I_best = argmax_{I_i} S_i
  I_next = generate_from_top_k(I_history, k=3)

where:
  I = instruction
  P(I) = program with instruction I
  M = metric
```

#### MIPROv2 (Bayesian Optimization)
```
Search Space: I × D (instructions × demos)
Objective: maximize E_{x∈D_val}[M(P(I,D)(x))]

Bayesian Optimization:
  Surrogate: GP(μ, σ) over (I, D) → score
  Acquisition: EI(I, D) = E[max(0, f(I,D) - f_best)]

For t = 1 to num_trials:
  (I_t, D_t) = argmax_{I,D} Acquisition(I, D)
  score_t = E_{x∈D_val}[M(P(I_t, D_t)(x))]
  Update GP with (I_t, D_t, score_t)
```

#### GEPA (Pareto Frontier)
```
Pareto Frontier:
  F = {P_i | ∃x∈D: score(P_i, x) ≥ score(P_j, x) ∀j≠i}

Reflective Mutation:
  trace(P, x) = {(step, input, output)_i}
  reflection = LLM("Analyze: what worked, what failed?")
  P_new = mutate(P, reflection)

Pareto Update:
  If ∃x: score(P_new, x) > max_{P∈F} score(P, x):
    F ← F ∪ {P_new}
  F ← remove_dominated(F)
```

#### BootstrapFinetune (Knowledge Distillation)
```
Teacher generates dataset:
  D_finetune = {(x, T(x)) | M(x, T(x)) ≥ τ}

Supervised Finetuning:
  θ* = argmin_θ Σ_{(x,y)∈D_finetune} CrossEntropy(LLM_θ(x), y)

Distilled Program:
  P_distilled(x) = LLM_θ*(x)
```

---

## 8. Recommendations

### 8.1 Immediate Actions (Next Sprint)

1. **Implement KNNFewShot** (1 week, P0)
   - Highest production ROI
   - Enables dynamic retrieval
   - Works with existing vector databases
   - Low complexity, high value

2. **Enhance BootstrapFewShot** (3-5 days, P0)
   - Add trace support for advanced metrics
   - Implement error budget
   - Add optimization statistics
   - Improves existing implementation

3. **Implement COPRO** (1-2 weeks, P0)
   - Complements BootstrapFewShot
   - Instruction optimization is orthogonal to example optimization
   - Coordinate ascent is well-understood
   - High impact for instruction-following

### 8.2 Medium-Term Goals (Next Month)

4. **Implement GEPA** (2-3 weeks, P0)
   - Best 2026 performance
   - Sample efficient (3-10 scenarios)
   - Reflective learning
   - State-of-the-art results

5. **Implement BootstrapFewShotWithRandomSearch** (2-3 days, P1)
   - Low effort extension
   - Exploration benefits
   - Works well for 50+ examples

6. **Implement BootstrapFewShotWithOptuna** (1 week, P2)
   - Quality over speed
   - Bayesian optimization
   - Better demo selection

### 8.3 Long-Term Goals (2+ Months)

7. **Implement MIPROv2** (3-4 weeks, P2)
   - Complex but powerful
   - Best for 200+ examples, 40+ trials
   - Joint instruction + demo optimization

8. **Implement BootstrapFinetune** (4+ weeks, P3)
   - Production efficiency
   - Requires ML training framework integration
   - High value for deployment

### 8.4 Architectural Recommendations

1. **Unified Metric Trait**
```rust
pub trait Metric: Send + Sync {
    fn evaluate(&self, example: &Example, output: &HashMap<String, Value>)
               -> Result<f64, ModuleError>;

    fn validate(&self, example: &Example, output: &HashMap<String, Value>,
                trace: Option<&Trace>)
               -> Result<bool, ModuleError>;
}
```

2. **Trace System**
```rust
pub struct Trace {
    pub steps: Vec<TraceStep>,
    pub metadata: TraceMetadata,
}

pub struct TraceStep {
    pub predictor: String,
    pub inputs: HashMap<String, Value>,
    pub outputs: HashMap<String, Value>,
    pub timestamp: SystemTime,
}
```

3. **Optimizer Trait**
```rust
#[async_trait]
pub trait Optimizer: Send + Sync {
    async fn compile(&self, student: &dyn Module, trainset: &[Example])
        -> Result<Box<dyn Module>, ModuleError>;

    async fn compile_with_stats(&self, student: &dyn Module,
                                 trainset: &[Example])
        -> Result<(Box<dyn Module>, OptimizationStatistics), ModuleError>;
}
```

4. **Vectorizer Abstraction** (for KNNFewShot)
```rust
#[async_trait]
pub trait Vectorizer: Send + Sync {
    async fn embed(&self, text: &str) -> Result<Vec<f32>, VectorizerError>;
    fn dimension(&self) -> usize;
}

pub struct SentenceTransformerVectorizer {
    model_name: String,
    dimension: usize,
}

pub struct OpenAIVectorizer {
    model: String,
    api_key: String,
}
```

---

## 9. Conclusion

### Key Takeaways

1. **Python DSPy Landscape**: 11 core optimizers ranging from simple (LabeledFewShot) to state-of-the-art (GEPA)

2. **Current Rust Status**: Solid BootstrapFewShot implementation (~9% of ecosystem), production-ready but missing advanced features

3. **Top Priorities for Rust**:
   - **GEPA**: Best 2026 performance, sample efficient, reflective learning
   - **KNNFewShot**: High production value, dynamic retrieval, low complexity
   - **COPRO**: Instruction optimization, complements existing work

4. **Production Recommendations**:
   - Small datasets (<10 examples): **GEPA**
   - Medium datasets (50-200): **KNNFewShot** or **BootstrapFewShotWithOptuna**
   - Large datasets (200+): **MIPROv2** or **SIMBA**
   - Deployment: **BootstrapFinetune** (distill to small model)

5. **Mathematical Foundation**: All optimizers solve `θ* = argmax E[metric(P_θ(x))]` with different search strategies:
   - **BootstrapFewShot**: Greedy validation
   - **KNN**: Nearest neighbor retrieval
   - **COPRO**: Coordinate ascent
   - **MIPROv2**: Bayesian optimization
   - **GEPA**: Reflective evolution + Pareto frontier

### Next Steps

1. **Week 1-2**: Implement KNNFewShot + enhance BootstrapFewShot
2. **Week 3-4**: Implement COPRO + BootstrapFewShotWithRandomSearch
3. **Week 5-7**: Implement GEPA (state-of-the-art)
4. **Week 8+**: MIPROv2, BootstrapFinetune (as needed)

---

## Sources

- [DSPy Optimizers Overview](https://dspy.ai/learn/optimization/optimizers/)
- [MIPROv2 Documentation](https://dspy.ai/api/optimizers/MIPROv2/)
- [COPRO Optimizer](https://dspy.ai/api/optimizers/COPRO/)
- [GEPA: Reflective Prompt Evolution](https://dspy.ai/api/optimizers/GEPA/overview/)
- [BootstrapFinetune](https://dspy.ai/api/optimizers/BootstrapFinetune/)
- [DSPy Metrics System](https://dspy.ai/learn/evaluation/metrics/)
- [DSPy GitHub Repository](https://github.com/stanfordnlp/dspy)
- [Weaviate: Your LLM Deserves Better Prompting](https://weaviate.io/blog/dspy-optimizers)
- [GEPA Paper (arxiv:2507.19457)](https://arxiv.org/abs/2507.19457)
- [GEPA: Game-Changing DSPy Optimizer](https://medium.com/superagentic-ai/gepa-the-game-changing-dspy-optimizer-for-agentic-ai-bfc1da20383a)

---

**Document Version**: 1.0
**Last Updated**: 2026-01-11
**Maintainer**: ggen-ai team
