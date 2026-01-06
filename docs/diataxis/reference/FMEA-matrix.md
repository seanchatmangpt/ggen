# Reference: FMEA Risk Matrix

**Failure Mode Effects Analysis - Prioritizing test coverage by severity, occurrence, and detectability**

---

## What is FMEA?

Failure Mode and Effects Analysis (FMEA) is a systematic method to identify and prioritize risks:

- **Failure Mode:** What can go wrong?
- **Effects:** What happens when it goes wrong?
- **Analysis:** How serious is it?
- **Mitigation:** What prevents it?

**FMEA Score = Severity × Occurrence × Detection**

---

## The Three Dimensions

### Severity (S): Impact if it happens

| Score | Impact | Example |
|-------|--------|---------|
| 1 | Cosmetic | Comment formatting error |
| 2 | Minor | Slow function (still works) |
| 3 | Moderate | Lost log message |
| 4 | Serious | Wrong calculation (detected) |
| 5 | Critical | Wrong calculation (undetected) |
| 6 | Very serious | Data corruption (recoverable) |
| 7 | High | Data loss (irrecoverable) |
| 8 | Severe | Security vulnerability (low exposure) |
| 9 | Very severe | Security vulnerability (high exposure) |
| 10 | Catastrophic | System failure, unavailable |

### Occurrence (O): How often it happens

| Score | Frequency | In 1000 Requests |
|-------|-----------|-----------------|
| 1 | Very unlikely | <1 |
| 2 | Unlikely | 1-3 |
| 3 | Low | 3-10 |
| 4 | Moderate | 10-30 |
| 5 | Moderate-High | 30-100 |
| 6 | High | 100-300 |
| 7 | Very High | 300-1000 |
| 8 | Very High | >1000 |
| 9 | High | >10000 |
| 10 | Certain | Always |

### Detection (D): Chance you'll catch it before production

| Score | Chance Undetected | When Detected |
|-------|-------------------|---------------|
| 1 | 100% | Always (guaranteed catch) |
| 2 | 50-100% | Code review always catches |
| 3 | 20-50% | Unit tests usually catch |
| 4 | 10-20% | Integration tests might catch |
| 5 | 5-10% | System tests might catch |
| 6 | 1-5% | Load tests might catch |
| 7 | <1% | Unlikely to be caught before production |
| 8 | Very rare | Rarely detected before production |
| 9 | Almost never | Almost never detected |
| 10 | Impossible | Not detectable (escape to production) |

---

## RPN (Risk Priority Number)

**RPN = Severity × Occurrence × Detection**

### Risk Thresholds

| RPN Range | Risk Level | Action |
|-----------|-----------|--------|
| 0-36 | Low | Monitor |
| 37-72 | Moderate | Test coverage required |
| 73-144 | High | Detailed testing required |
| 145-216 | Very High | Multiple test types required |
| 217-405 | Critical | Comprehensive testing + code review |
| 406+ | Catastrophic | Architectural review needed |

### Decision Table

| RPN | Decision | Test Strategy |
|-----|----------|---------------|
| <36 | Don't test | Manual spot checks if convenient |
| 36-100 | Test | Unit tests are sufficient |
| 100-200 | Test thoroughly | Unit + integration tests |
| 200-400 | Test exhaustively | Unit + integration + property-based tests |
| 400+ | Redesign | Rethink architecture before testing |

---

## Real-World Examples

### Example 1: User Email Validation

```
What can go wrong: Invalid email stored
  Severity: 5 (Broken authentication)
  Occurrence: 4 (Some users try invalid emails)
  Detection: 1 (Unit test always catches)
  RPN = 5 × 4 × 1 = 20 (LOW)
  Action: Unit test sufficient
```

### Example 2: Database Deadlock

```
What can go wrong: Concurrent writes deadlock
  Severity: 7 (Data loss)
  Occurrence: 2 (Rare, happens under load)
  Detection: 8 (Very hard to detect before production)
  RPN = 7 × 2 × 8 = 112 (HIGH)
  Action: Concurrency tests + stress testing required
```

### Example 3: SQL Injection Vulnerability

```
What can go wrong: SQL injection attack
  Severity: 10 (Complete compromise)
  Occurrence: 3 (Not all users attack)
  Detection: 9 (Won't be caught by unit tests)
  RPN = 10 × 3 × 9 = 270 (CRITICAL)
  Action: Security code review + penetration testing
```

### Example 4: Off-by-One Error in Loop

```
What can go wrong: Loop accesses one element too many
  Severity: 6 (Data corruption)
  Occurrence: 3 (Only certain inputs trigger it)
  Detection: 2 (Code review usually catches, worst case unit test)
  RPN = 6 × 3 × 2 = 36 (LOW-MODERATE)
  Action: Code review sufficient, no special testing needed
```

---

## FMEA Matrix Template

### For Test Planning

| Failure Mode | Severity | Occurrence | Detection | RPN | Action |
|--------------|----------|-----------|-----------|-----|--------|
| Empty user list | 4 | 2 | 2 | 16 | Monitor |
| Negative balance | 7 | 2 | 1 | 14 | Unit test |
| Race condition | 8 | 3 | 8 | 192 | Concurrency test |
| SQL injection | 10 | 3 | 9 | 270 | Security review |
| Memory leak | 7 | 4 | 6 | 168 | Stress test |
| Cache invalidation | 6 | 5 | 7 | 210 | Integration test |

---

## Using FMEA for Test Prioritization

### Step 1: Identify Failure Modes

List everything that can go wrong:
- Invalid inputs
- Missing data
- Concurrent access
- Resource exhaustion
- External service failures
- Security vulnerabilities

### Step 2: Score Each Failure

For each failure mode:
- How bad if it happens? (Severity)
- How likely is it? (Occurrence)
- Will we catch it? (Detection)
- Calculate RPN

### Step 3: Prioritize by RPN

Test in order:
1. RPN > 400 (Catastrophic) - Redesign
2. RPN 200-400 (Critical) - Comprehensive testing
3. RPN 100-200 (High) - Detailed testing
4. RPN 36-100 (Moderate) - Basic testing
5. RPN < 36 (Low) - Monitor only

### Step 4: Assign Test Types

- **Unit Tests** - Detection: catch during coding (D=1-3)
- **Integration Tests** - Detection: catch during integration (D=3-5)
- **Property-Based Tests** - Detection: catch edge cases (D=2-4)
- **Stress/Load Tests** - Detection: catch under load (D=5-7)
- **Security Tests** - Detection: catch attacks (D=7-9)

---

## Common Failure Modes by Category

### Data Input

| Failure | S | O | D | RPN | Test |
|---------|---|---|---|-----|------|
| Invalid email | 5 | 4 | 1 | 20 | Unit |
| Missing required field | 6 | 5 | 1 | 30 | Unit |
| Out of range value | 4 | 3 | 1 | 12 | Unit |

### Concurrency

| Failure | S | O | D | RPN | Test |
|---------|---|---|---|-----|------|
| Race condition | 8 | 3 | 8 | 192 | Concurrency |
| Deadlock | 7 | 2 | 8 | 112 | Stress |
| Double processing | 6 | 4 | 7 | 168 | Integration |

### Performance

| Failure | S | O | D | RPN | Test |
|---------|---|---|---|-----|------|
| Memory leak | 7 | 4 | 6 | 168 | Stress |
| Slow algorithm | 3 | 4 | 2 | 24 | Benchmark |
| Database query timeout | 5 | 3 | 4 | 60 | Load |

### Security

| Failure | S | O | D | RPN | Test |
|---------|---|---|---|-----|------|
| SQL injection | 10 | 3 | 9 | 270 | Security |
| XSS vulnerability | 9 | 4 | 8 | 288 | Security |
| Authentication bypass | 10 | 2 | 9 | 180 | Security |

---

## FMEA vs. Test Coverage

### Common Mistake: Coverage Metrics

```
❌ "We have 95% code coverage, we're done testing"
  Problem: Coverage doesn't measure risk
  A critical security bug is 1% of code
```

### Better Approach: Risk-Based Testing

```
✅ Use FMEA to identify critical paths
✅ Test those paths exhaustively
✅ Accept lower coverage for low-risk code
✅ Focus effort where it matters
```

---

## Reducing RPN

For each high-RPN failure mode, you can:

### Reduce Severity
- Better error handling
- Graceful degradation
- Recovery mechanisms

### Reduce Occurrence
- Input validation
- Defensive programming
- Code review

### Reduce Detection
- More thorough testing
- Automated checks
- Code analysis tools

**Example:**

```
Original: S=10, O=3, D=9 → RPN=270 (SQL injection)

Actions:
- Use parameterized queries (S=10, O=1, D=1 → RPN=10)
- Input validation (O further reduced)
- Security code review (D improved)
```

---

## FMEA vs. Risk-Based Testing Frameworks

### Comparison

| Approach | Cost | Effort | Risk Capture |
|----------|------|--------|-------------|
| Coverage-based | Low | Low | Poor |
| FMEA-based | Medium | Medium | Excellent |
| Continuous monitoring | High | High | Excellent + real data |

### Recommendation

- **Start with:** FMEA for planning
- **Supplement with:** Code coverage for gaps
- **Monitor with:** Production metrics for blind spots

---

## Template: FMEA Analysis Document

```
# FMEA: Feature X

## Failure Mode 1: [Description]
- Severity: [1-10] because [reason]
- Occurrence: [1-10] because [reason]
- Detection: [1-10] because [reason]
- RPN: [S × O × D]
- Test Strategy: [unit/integration/stress/security]

## Failure Mode 2: [Description]
- Severity: [1-10]
- ...
```

---

## Key Takeaways

1. **RPN quantifies risk** - Helps prioritize testing effort
2. **Severity × Occurrence × Detection** - Multiply to get priority
3. **Higher RPN = more rigorous testing** - 400+ needs redesign
4. **Different test types have different detection** - Use appropriate tests
5. **FMEA complements coverage** - Coverage finds gaps, FMEA prioritizes effort

---

## References

- [FMEA Wikipedia](https://en.wikipedia.org/wiki/Failure_mode_and_effects_analysis)
- [Risk-Based Testing](https://www.guru99.com/risk-analysis-in-software-testing.html)
