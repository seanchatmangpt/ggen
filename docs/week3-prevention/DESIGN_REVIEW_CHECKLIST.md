# DfLSS Design Review Checklist

**Version**: 1.0.0
**Purpose**: Prevent defects at design phase through systematic review
**Principle**: Prevention is 10x cheaper than detection, 100x cheaper than correction

---

## Instructions

1. Complete this checklist BEFORE implementing any feature
2. Score each section 1-10 (10 = perfect)
3. Minimum passing score: 7.0/10 weighted average
4. All critical failures (RPN > 100) must be mitigated before approval
5. Reviewer must sign off before implementation begins

---

## Feature Information

**Feature Name**: _______________________________________________
**Designer**: _______________________________________________
**Reviewer**: _______________________________________________
**Date**: _______________________________________________
**Target Release**: _______________________________________________

---

## Section 1: Failure Mode Analysis (FMEA)

**Weight**: 30%

### Checklist

Have we identified all failure modes?

- [ ] Input validation failures documented
- [ ] State transition errors identified
- [ ] Resource exhaustion scenarios considered
- [ ] Concurrency issues analyzed
- [ ] Integration failure points mapped
- [ ] Error propagation gaps checked

### Failure Mode Table

| Failure Mode | Severity (1-10) | Probability (1-10) | Detection (1-10) | RPN (S×P×D) | Mitigation Plan |
|--------------|-----------------|-------------------|------------------|-------------|-----------------|
| [Example: Invalid input] | 8 | 6 | 3 | 144 | [Add input validation] |
|  |  |  |  |  |  |
|  |  |  |  |  |  |
|  |  |  |  |  |  |
|  |  |  |  |  |  |

**Critical Failures (RPN > 100)**: _____________________________

**Mitigation Required**: [ ] Yes [ ] No

**Self-Assessment Score (1-10)**: _____ / 10

**Reviewer Score (1-10)**: _____ / 10

**Notes**:

---

## Section 2: Contradiction Resolution (TRIZ)

**Weight**: 25%

### Checklist

Have we resolved contradictions?

- [ ] Performance vs. Safety trade-offs addressed with technical solution
- [ ] Flexibility vs. Type Safety balanced (not compromised)
- [ ] Ergonomics vs. Explicitness resolved with API design
- [ ] No "we'll fix it later" deferred decisions
- [ ] All trade-offs documented with rationale
- [ ] Alternative solutions evaluated

### Contradiction Matrix

| Improving Parameter | Worsening Parameter | Resolution Strategy | TRIZ Principle Used |
|---------------------|---------------------|---------------------|---------------------|
| [Example: Performance] | [Example: Safety] | [Zero-cost abstractions] | [#35: Parameter changes] |
|  |  |  |  |
|  |  |  |  |

**Unresolved Contradictions**: _____________________________

**Self-Assessment Score (1-10)**: _____ / 10

**Reviewer Score (1-10)**: _____ / 10

**Notes**:

---

## Section 3: Compile-Time Guarantees

**Weight**: 25%

### Checklist

Do we have compile-time guarantees?

- [ ] Invalid states made unrepresentable through types
- [ ] State transitions enforced by PhantomData state machine
- [ ] No runtime validation that could be compile-time
- [ ] API usage errors caught by compiler
- [ ] Type-level invariants documented
- [ ] Zero-cost abstractions verified

### Type-Level Enforcement Design

```rust
// Paste type-level state machine design here
// Example:
pub struct Registry<State = Uninitialized> {
    templates: Vec<Template>,
    _state: PhantomData<State>,
}

impl Registry<Uninitialized> {
    pub fn initialize(self) -> Result<Registry<Initialized>> { ... }
}

impl Registry<Validated> {
    pub fn search(&self) -> Result<Vec<Template>> { ... }
}
```

**Compiler-Enforced Invariants**:
1. _____________________________
2. _____________________________
3. _____________________________

**Self-Assessment Score (1-10)**: _____ / 10

**Reviewer Score (1-10)**: _____ / 10

**Notes**:

---

## Section 4: Integration Contracts

**Weight**: 20%

### Checklist

Is the integration contract clear?

- [ ] Traits define explicit integration contracts
- [ ] All implementations tested against contract
- [ ] Version compatibility explicit and checked
- [ ] Breaking changes cause compile errors
- [ ] Contract test suite defined
- [ ] Documentation includes usage examples

### Contract Definition

```rust
// Paste trait definition here
// Example:
pub trait TemplateProvider: Send + Sync {
    fn discover(&self, path: &Path) -> Result<Vec<Template>>;
    fn validate(&self, template: &Template) -> Result<()>;
    fn render(&self, template: &Template, context: Context) -> Result<String>;
}
```

**Contract Test Plan**:
1. _____________________________
2. _____________________________
3. _____________________________

**Self-Assessment Score (1-10)**: _____ / 10

**Reviewer Score (1-10)**: _____ / 10

**Notes**:

---

## Section 5: Error Visibility and Propagation

**Weight**: 15%

### Checklist

Are errors visible and propagated?

- [ ] All functions return `Result<T, E>` (no silent failures)
- [ ] Error types have rich context and diagnostics
- [ ] No `unwrap()` or `expect()` in production code
- [ ] Error messages are actionable (tell user what to do)
- [ ] Error handling tested (not just happy path)
- [ ] Errors include suggestions for resolution

### Error Taxonomy Design

```rust
// Paste error enum design here
// Example:
#[derive(Error, Debug)]
pub enum GgenError {
    #[error("Template not found: {path}\nSuggestion: {suggestion}")]
    TemplateNotFound {
        path: String,
        suggestion: String,
    },
    // ...
}
```

**Error Scenarios Covered**:
1. _____________________________
2. _____________________________
3. _____________________________

**Self-Assessment Score (1-10)**: _____ / 10

**Reviewer Score (1-10)**: _____ / 10

**Notes**:

---

## Section 6: Prevention Verification

**Weight**: 10%

### Checklist

Can we prevent invalid states?

- [ ] State machine prevents invalid sequences
- [ ] Types encode domain invariants
- [ ] Compiler prevents API misuse
- [ ] Runtime validation minimized (only external inputs)
- [ ] Property tests verify invariants
- [ ] Documentation shows prevented errors

**Prevented Error Examples**:
1. _____________________________
2. _____________________________
3. _____________________________

**Self-Assessment Score (1-10)**: _____ / 10

**Reviewer Score (1-10)**: _____ / 10

**Notes**:

---

## Section 7: Test Coverage Plan

**Weight**: 10%

### Checklist

Have we planned comprehensive testing?

- [ ] Unit tests for all components
- [ ] Integration tests for all contracts
- [ ] Property tests for invariants
- [ ] Performance tests for SLOs
- [ ] Error path tests (not just happy path)
- [ ] Chicago TDD: State-based, real collaborators, AAA pattern

### Test Plan

| Test Type | Coverage Target | Test Count | Status |
|-----------|----------------|------------|--------|
| Unit | 80%+ | ___ | Not Started |
| Integration | All contracts | ___ | Not Started |
| Property | All invariants | ___ | Not Started |
| Performance | All SLOs | ___ | Not Started |
| Error Paths | All error types | ___ | Not Started |

**Self-Assessment Score (1-10)**: _____ / 10

**Reviewer Score (1-10)**: _____ / 10

**Notes**:

---

## Section 8: DfLSS Principles Adherence

**Weight**: 10%

### Checklist

Does this design follow DfLSS principles?

- [ ] Prevention over detection (compile-time > runtime)
- [ ] Design for quality (types enforce correctness)
- [ ] Waste elimination built-in (no unnecessary validation)
- [ ] Continuous improvement path clear
- [ ] Metrics defined for success measurement
- [ ] Knowledge captured for future designs

**DfLSS Alignment**:
- Prevention: _____________________________
- Quality: _____________________________
- Waste Elimination: _____________________________
- Improvement Path: _____________________________

**Self-Assessment Score (1-10)**: _____ / 10

**Reviewer Score (1-10)**: _____ / 10

**Notes**:

---

## Weighted Score Calculation

| Section | Weight | Designer Score | Reviewer Score | Weighted Score |
|---------|--------|----------------|----------------|----------------|
| 1. FMEA | 30% | _____ | _____ | _____ |
| 2. TRIZ | 25% | _____ | _____ | _____ |
| 3. Compile-Time Guarantees | 25% | _____ | _____ | _____ |
| 4. Integration Contracts | 20% | _____ | _____ | _____ |
| 5. Error Visibility | 15% | _____ | _____ | _____ |
| 6. Prevention Verification | 10% | _____ | _____ | _____ |
| 7. Test Coverage | 10% | _____ | _____ | _____ |
| 8. DfLSS Adherence | 10% | _____ | _____ | _____ |
| **TOTAL** | **145%** (overlapping) | **_____** | **_____** | **_____** |

**Normalized Score (divide by 1.45)**: _____ / 10

**Passing Score**: ≥ 7.0/10

---

## Approval Decision

### Pre-Approval Checklist

- [ ] All critical failures (RPN > 100) mitigated
- [ ] All contradictions resolved (no "we'll fix later")
- [ ] Compile-time guarantees implemented
- [ ] Integration contracts defined and tested
- [ ] Error propagation complete (no silent failures)
- [ ] Test plan approved
- [ ] DfLSS principles followed
- [ ] Weighted score ≥ 7.0/10

### Decision

**Approved for Implementation**: [ ] Yes [ ] No

**Conditional Approval** (requires changes): [ ] Yes [ ] No

**Changes Required**:
1. _____________________________
2. _____________________________
3. _____________________________

**Follow-Up Review Required**: [ ] Yes [ ] No

**Follow-Up Date**: _______________________________________________

---

## Signatures

**Designer**: _______________________________________________ **Date**: _______________

**Reviewer**: _______________________________________________ **Date**: _______________

**Approved By**: _______________________________________________ **Date**: _______________

---

## Lessons Learned

**What went well**:

**What could be improved**:

**New patterns to add to checklist**:

**Training needs identified**:

---

## Attachment Checklist

Attach the following to this review:

- [ ] FMEA analysis spreadsheet
- [ ] TRIZ contradiction matrix
- [ ] Type-level design diagrams
- [ ] Contract trait definitions
- [ ] Error taxonomy diagram
- [ ] Test plan spreadsheet
- [ ] Performance SLO definitions

---

**END OF CHECKLIST**

**Next Review**: _______________________________________________
