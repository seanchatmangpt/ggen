# FMEA Analysis - Doctest Conversion Process

## Step 1: Define Scope

**What**: Doctest conversion and documentation consistency process
**Boundaries**: 
- Included: Converting `no_run` doctests to runnable, maintaining doctest quality, verifying conversions
- Excluded: Writing new doctests from scratch, fixing pre-existing compilation errors
**Context**: Converting 210 `no_run` doctests to runnable format to improve documentation verifiability
**Goal**: Prevent doctest conversion failures (broken tests, incorrect examples, documentation regressions)

---

## Step 2: Identify Failure Modes

### Failure Mode 1: Converting Doctest That Requires File I/O
- **Component**: Doctest conversion process
- **Step**: Conversion decision
- **Description**: Converting `no_run` doctest to runnable when it requires file I/O, causing test to fail or hang

### Failure Mode 2: Converting Doctest That Requires Network
- **Component**: Doctest conversion process
- **Step**: Conversion decision
- **Description**: Converting `no_run` doctest to runnable when it requires network access, causing test to fail or timeout

### Failure Mode 3: Converting Doctest With Incorrect Syntax
- **Component**: Doctest conversion process
- **Step**: Code modification
- **Description**: Converting doctest but introducing syntax errors (wrong error handling, missing imports, incorrect assertions)

### Failure Mode 4: Not Verifying Converted Doctest Runs
- **Component**: Doctest conversion process
- **Step**: Verification
- **Description**: Converting doctest but not running `cargo test --doc` to verify it actually works

### Failure Mode 5: Breaking Existing Runnable Doctests
- **Component**: Doctest conversion process
- **Step**: Code modification
- **Description**: Modifying code in a way that breaks existing runnable doctests (e.g., changing API, removing methods)

### Failure Mode 6: Inconsistent Doctest Patterns After Conversion
- **Component**: Doctest conversion process
- **Step**: Code modification
- **Description**: Converting doctests but using inconsistent patterns (some use `unwrap()`, others use `?`, mixed error handling)

### Failure Mode 7: Documentation Claims Don't Match Converted Doctest
- **Component**: Doctest conversion process
- **Step**: Code modification
- **Description**: Converting doctest but documentation text doesn't match what doctest actually demonstrates

### Failure Mode 8: Doctest Passes But Doesn't Verify Behavior
- **Component**: Doctest conversion process
- **Step**: Code modification
- **Description**: Converting doctest but it compiles/runs without actually verifying the intended behavior (no assertions, wrong assertions)

### Failure Mode 9: Converting Doctest That Requires Async Runtime
- **Component**: Doctest conversion process
- **Step**: Conversion decision
- **Description**: Converting `no_run` doctest to runnable when it requires async runtime, causing test to fail

### Failure Mode 10: Missing Error Case Examples After Conversion
- **Component**: Doctest conversion process
- **Step**: Code modification
- **Description**: Converting success case but not adding error case examples for Result-returning functions

### Failure Mode 11: Doctest Uses Wrong API Methods
- **Component**: Doctest conversion process
- **Step**: Code modification
- **Description**: Converting doctest but using deprecated or incorrect API methods

### Failure Mode 12: Doctest Fails Due to Pre-Existing Compilation Errors
- **Component**: Doctest conversion process
- **Step**: Verification
- **Description**: Converted doctest is correct but can't be verified due to pre-existing compilation errors in codebase

---

## Step 3: Assess Severity

| Failure Mode | Severity | Rationale |
|--------------|----------|-----------|
| FM1: File I/O required | 8 (Serious) | Test fails/hangs, breaks CI, wastes developer time |
| FM2: Network required | 8 (Serious) | Test fails/timeouts, breaks CI, wastes developer time |
| FM3: Incorrect syntax | 7 (Major) | Test fails to compile, breaks CI, misleading documentation |
| FM4: Not verifying | 9 (Critical) | Broken doctest goes undetected, breaks CI later, wastes time |
| FM5: Breaking existing | 8 (Serious) | Regression, breaks existing tests, wastes time |
| FM6: Inconsistent patterns | 4 (Low) | Confusing but doesn't break functionality |
| FM7: Docs don't match | 6 (Moderate) | Misleading documentation, user confusion |
| FM8: Doesn't verify behavior | 7 (Major) | Documentation doesn't actually test behavior, false confidence |
| FM9: Async required | 8 (Serious) | Test fails, breaks CI, wastes developer time |
| FM10: Missing error cases | 5 (Minor) | Incomplete documentation but doesn't break functionality |
| FM11: Wrong API methods | 7 (Major) | Misleading documentation, user confusion, wrong usage |
| FM12: Pre-existing errors | 3 (Very Low) | Can't verify but conversion is correct, temporary issue |

---

## Step 4: Assess Frequency

| Failure Mode | Frequency | Rationale |
|--------------|-----------|-----------|
| FM1: File I/O required | 5 (Low) | Conversion criteria should catch this, but can slip through |
| FM2: Network required | 4 (Very Low) | Conversion criteria should catch this, rare |
| FM3: Incorrect syntax | 6 (Low-Moderate) | Easy to make syntax errors during conversion |
| FM4: Not verifying | 7 (Moderate) | Human error, easy to forget to verify |
| FM5: Breaking existing | 3 (Remote) | Rare, but can happen if modifying shared code |
| FM6: Inconsistent patterns | 7 (Moderate) | Common if no clear standards enforced |
| FM7: Docs don't match | 5 (Low) | Can happen if not careful during conversion |
| FM8: Doesn't verify behavior | 6 (Low-Moderate) | Common if converting without adding assertions |
| FM9: Async required | 4 (Very Low) | Conversion criteria should catch this |
| FM10: Missing error cases | 8 (Moderate-High) | Common if not systematically adding error examples |
| FM11: Wrong API methods | 4 (Very Low) | Rare, but can happen with API changes |
| FM12: Pre-existing errors | 2 (Very Remote) | Only if codebase has compilation errors |

---

## Step 5: Assess Detection

| Failure Mode | Detection | Rationale |
|--------------|-----------|-----------|
| FM1: File I/O required | 2 (Very High) | `cargo test --doc` will fail immediately |
| FM2: Network required | 2 (Very High) | `cargo test --doc` will fail/timeout immediately |
| FM3: Incorrect syntax | 1 (Almost Certain) | Compiler catches syntax errors immediately |
| FM4: Not verifying | 10 (Almost Impossible) | No automated check, only manual verification |
| FM5: Breaking existing | 2 (Very High) | `cargo test --doc` will fail immediately |
| FM6: Inconsistent patterns | 5 (Moderate) | Requires manual review or linting |
| FM7: Docs don't match | 4 (Moderately High) | Manual review or careful reading |
| FM8: Doesn't verify behavior | 6 (Low) | Requires manual review of assertions |
| FM9: Async required | 2 (Very High) | `cargo test --doc` will fail immediately |
| FM10: Missing error cases | 5 (Moderate) | Requires manual review or automated check |
| FM11: Wrong API methods | 2 (Very High) | Compiler catches wrong methods immediately |
| FM12: Pre-existing errors | 1 (Almost Certain) | Compiler errors are obvious |

---

## Step 6: Calculate RPN

| Failure Mode | Severity | Frequency | Detection | RPN | Priority |
|--------------|----------|-----------|-----------|-----|----------|
| FM4: Not verifying | 9 | 7 | 10 | **630** | **CRITICAL** |
| FM1: File I/O required | 8 | 5 | 2 | 80 | Low |
| FM2: Network required | 8 | 4 | 2 | 64 | Low |
| FM5: Breaking existing | 8 | 3 | 2 | 48 | Low |
| FM9: Async required | 8 | 4 | 2 | 64 | Low |
| FM3: Incorrect syntax | 7 | 6 | 1 | 42 | Low |
| FM8: Doesn't verify behavior | 7 | 6 | 6 | 252 | **HIGH** |
| FM11: Wrong API methods | 7 | 4 | 2 | 56 | Low |
| FM7: Docs don't match | 6 | 5 | 4 | 120 | Medium |
| FM10: Missing error cases | 5 | 8 | 5 | 200 | **HIGH** |
| FM6: Inconsistent patterns | 4 | 7 | 5 | 140 | Medium |
| FM12: Pre-existing errors | 3 | 2 | 1 | 6 | Low |

---

## Step 7: Prioritize and Fix

### Priority 1 (Critical - RPN 501-1000)

**FM4: Not Verifying Converted Doctest Runs (RPN 630)**
- **Action**: Add mandatory verification step to conversion process
- **Fix**: Always run `cargo test --doc` after conversion, add to CI

### Priority 2 (High - RPN 301-500)

**FM8: Doctest Passes But Doesn't Verify Behavior (RPN 252)**
- **Action**: Ensure all converted doctests have assertions
- **Fix**: Add assertion verification checklist

**FM10: Missing Error Case Examples (RPN 200)**
- **Action**: Systematically add error case examples
- **Fix**: Add error case examples to all Result-returning functions

### Priority 3 (Medium - RPN 101-300)

**FM7: Documentation Claims Don't Match (RPN 120)**
- **Action**: Review documentation text matches doctest
- **Fix**: Manual review during conversion

**FM6: Inconsistent Patterns (RPN 140)**
- **Action**: Standardize doctest patterns
- **Fix**: Enforce consistent patterns in conversions

