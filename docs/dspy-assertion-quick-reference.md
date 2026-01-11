# DSPy Assertion System - Quick Reference

## Core Concepts (60 Second Overview)

**What:** Computational constraints that enable LLMs to self-correct through backtracking and retry with feedback.

**How:** When validation fails, DSPy modifies the prompt to include past failed outputs + error messages, then retries.

**Impact:** Up to 164% better rule compliance, 37% better output quality.

**Status:** Deprecated in DSPy 2.6 → Use `dspy.Refine` and `dspy.BestOfN` instead.

---

## Two Types of Assertions

| Type | Purpose | Failure Behavior | Use Case |
|------|---------|------------------|----------|
| **dspy.Assert** | Hard constraint | Raise error after max retries | Critical requirements (safety, format) |
| **dspy.Suggest** | Soft constraint | Log warning and continue | Quality improvements (length, style) |

---

## Basic Usage Pattern

```python
# Inside a DSPy Module's forward() method
def forward(self, question):
    answer = self.answer_module(question=question)

    # Hard assertion - must pass
    dspy.Assert(
        len(answer.text) > 0,
        "Answer cannot be empty",
        target_module=self.answer_module
    )

    # Soft suggestion - prefer but not required
    dspy.Suggest(
        len(answer.text) > 50,
        "Answer should be detailed (>50 chars)",
        target_module=self.answer_module
    )

    return answer
```

---

## Backtracking Flow

```
┌─────────────────────┐
│  Execute Module     │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Run Validation     │
└──────────┬──────────┘
           │
    ┌──────┴──────┐
    │   Pass?     │
    └──────┬──────┘
           │
    ┌──────┴──────┐
    │             │
   YES            NO
    │             │
    ▼             ▼
┌─────────┐   ┌──────────────────────┐
│Continue │   │ Modify Signature:    │
│Pipeline │   │ + Past Output        │
└─────────┘   │ + Error Feedback     │
              └──────────┬───────────┘
                         │
                         ▼
              ┌──────────────────────┐
              │ Retry Target Module  │
              └──────────┬───────────┘
                         │
                         ▼
              ┌──────────────────────┐
              │ Attempts < Max?      │
              └──────────┬───────────┘
                         │
                  ┌──────┴──────┐
                  │             │
                 YES            NO
                  │             │
                  │             ▼
                  │      ┌─────────────┐
                  │      │Assert: Error│
                  │      │Suggest: Warn│
                  │      └─────────────┘
                  │
                  └─────► (Retry loop)
```

---

## Signature Modification Example

### Before Failure
```python
class AnswerQuestion(dspy.Signature):
    question = dspy.InputField()
    answer = dspy.OutputField()
```

### After Failure (Automatic)
```python
class AnswerQuestion(dspy.Signature):
    question = dspy.InputField()
    past_output = dspy.InputField(desc="Previous failed: 'ok'")
    instruction = dspy.InputField(desc="Answer must be >50 chars")
    answer = dspy.OutputField()
```

---

## Common Validation Patterns

```python
# Length constraint
dspy.Suggest(
    len(output.text) <= 100,
    "Keep response under 100 characters"
)

# Format constraint
dspy.Assert(
    output.answer.count(" ") == 0,
    "Answer must be single word"
)

# Content validation
dspy.Assert(
    any(kw in output.text for kw in ["because", "therefore"]),
    "Answer must include reasoning"
)

# Structured validation
import json
dspy.Assert(
    lambda: json.loads(output.json_text) and True,
    "Output must be valid JSON"
)

# Uniqueness
dspy.Suggest(
    len(set(output.items)) == len(output.items),
    "Items must be unique"
)
```

---

## Integration with Optimization

### Three Optimization Mechanisms

1. **Assertion-Driven Backtracking (Inference)**
   - Runtime self-correction
   - Automatic retry with feedback
   - No code changes needed

2. **Assertion-Driven Example Bootstrapping (Compile)**
   - Teacher model uses assertions during BootstrapFewShot
   - Generates harder, more robust examples
   - Student learns from challenging cases

3. **Counterexample Bootstrapping (Compile)**
   - Captures failed examples + traces
   - Includes in few-shot demonstrations
   - Teaches model to avoid mistakes

### Usage with BootstrapFewShot

```python
# Teacher generates examples with assertions active
teacher = MyModule()
optimizer = BootstrapFewShot(
    metric=validation_metric,
    max_bootstrapped_demos=8
)

# Student compiled from teacher's assertion-validated examples
student = optimizer.compile(teacher=teacher, trainset=examples)

# Optional: Student can also use assertions at inference
student_with_assertions = assert_transform_module(student, backtrack_handler)
```

---

## Transform Module Pattern

```python
from dspy.primitives.assertions import assert_transform_module, backtrack_handler
import functools

# Method 1: Transform with default config (max_backtracks=2)
program = assert_transform_module(MyProgram(), backtrack_handler)

# Method 2: Custom retry count
program = assert_transform_module(
    MyProgram(),
    functools.partial(backtrack_handler, max_backtracks=5)
)

# Method 3: Direct activation
program = MyProgram()
program.activate_assertions()  # Default: max_backtracks=2
```

---

## Modern Replacement (DSPy 2.6+)

### BestOfN
Run module N times, return best based on reward function.

```python
best_of_3 = dspy.BestOfN(
    module=qa_module,
    N=3,
    reward_fn=lambda output: len(output.answer) if valid(output) else 0,
    threshold=1.0  # Return first output scoring >= 1.0
)
```

### Refine
BestOfN + automatic feedback loop.

```python
refined = dspy.Refine(
    module=qa_module,
    reward_fn=validation_score,
    threshold=0.8,
    max_iterations=3
)
```

**Key Difference:** Refine automatically generates feedback between attempts (like assertions), while BestOfN just retries.

---

## Configuration Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `max_backtracks` | 2 | Max retry attempts per assertion |
| `target_module` | Previous module | Which module to retry on failure |
| `msg` | Required | Feedback message for LLM |
| `constraint` | Required | Boolean validation condition |

---

## Error Types

```python
# Assertion failure (hard constraint)
dspy.AssertionError: "Validation failed: Answer must be one word"

# Suggestion failure (soft constraint)
SuggestionWarning: "Quality check failed: Answer should be >50 chars"
```

---

## Performance Characteristics

**Cost:**
- Each retry = 1 additional LLM call
- Max cost = (max_backtracks + 1) × base_cost

**Latency:**
- Adds retry time on validation failures
- Best case: 0 (passes first time)
- Worst case: max_backtracks × module_latency

**Reliability Gains:**
- Rule compliance: +164% (research benchmarks)
- Output quality: +37% (research benchmarks)

---

## Best Practices

### Development Phase
- Use `dspy.Assert` liberally
- Catch bugs early with strict validation
- Set low `max_backtracks` (2-3) for fast iteration

### Production Phase
- Convert non-critical asserts to `dspy.Suggest`
- Monitor suggestion failure rates
- Increase `max_backtracks` for critical paths (3-5)

### Optimization Phase
- Enable assertions during BootstrapFewShot compilation
- Collect counterexamples for hard cases
- Use assertion-driven metrics

---

## Common Pitfalls

| Pitfall | Problem | Solution |
|---------|---------|----------|
| Too many assertions | Exponential retry costs | Strategic placement on critical outputs |
| Poor feedback messages | LLM can't self-correct | Specific, actionable error messages |
| Validation side effects | Non-deterministic behavior | Pure validation functions only |
| Missing target_module | Wrong module retried | Explicitly specify target when needed |
| Validation exceptions | Pipeline crashes | Return False instead of raising |

---

## Rust Implementation Checklist

Based on DSPy analysis, Rust implementation needs:

- [ ] `Validator<T>` trait for type-safe validation
- [ ] `ValidationResult` enum (Valid | Invalid { feedback })
- [ ] `AssertionLevel` enum (Assert | Suggest)
- [ ] `BacktrackConfig` struct with retry strategy
- [ ] `Assertion<T>` builder pattern
- [ ] `BacktrackExecutor` for retry logic
- [ ] Dynamic prompt modification (RetryContext)
- [ ] Telemetry for observability
- [ ] Integration with BootstrapFewShot equivalent
- [ ] TTL specification for assertions
- [ ] Code generation from RDF specs

---

## Quick Decision Tree

```
Does output validation matter?
├─ NO → Skip assertions
└─ YES → Continue
    │
    Is this a critical constraint?
    ├─ YES → Use Assert (hard constraint)
    │   └─ Set max_backtracks: 3-5
    └─ NO → Use Suggest (soft constraint)
        └─ Set max_backtracks: 2-3
```

---

## Code Template

```python
class MyModule(dspy.Module):
    def __init__(self):
        self.processor = dspy.ChainOfThought("input -> output")

    def forward(self, input):
        # Execute module
        result = self.processor(input=input)

        # Critical constraints (must pass)
        dspy.Assert(
            len(result.output) > 0,
            "Output cannot be empty",
            target_module=self.processor
        )

        # Quality constraints (nice to have)
        dspy.Suggest(
            len(result.output) > 20,
            "Output should be detailed",
            target_module=self.processor
        )

        return result

# Activate assertions
program = assert_transform_module(
    MyModule(),
    functools.partial(backtrack_handler, max_backtracks=3)
)
```

---

## References

- Full Analysis: `docs/dspy-assertion-system-analysis.md`
- DSPy Docs: https://dspy.ai/learn/programming/7-assertions/
- Research Paper: https://arxiv.org/abs/2312.13382
- Modern Replacement: https://dspy.ai/tutorials/output_refinement/best-of-n-and-refine/
