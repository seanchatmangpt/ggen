# CONSTRUCT8 Pipeline Test Gaps and Limitations

## Current Implementation Limitations

### 1. Template Predicate Must Match Run Predicate

**Current Behavior**: `knhk_eval_construct8` requires `ir->p == ctx->run.pred`

**Location**: `include/knhk/eval.h:268`

```c
if (ir->p != ctx->run.pred)
    return 0;
```

**Impact**: CONSTRUCT8 can only emit triples with the same predicate as the run predicate. This limits flexibility for CONSTRUCT queries that want to transform predicates.

**Workaround**: Tests use the same predicate for both run and template.

**Future Enhancement**: Remove this check to allow CONSTRUCT8 to emit triples with a different predicate than the run predicate, enabling true CONSTRUCT semantics.

### 2. Object Template Must Be Constant

**Current Behavior**: `ir->o` is a constant value, not variable

**Location**: `src/simd/construct.h:13`

```c
static inline size_t knhk_construct8_emit_8(..., uint64_t o_const, ...)
```

**Impact**: CONSTRUCT8 cannot emit variable objects (e.g., `?o` from input). Only constant objects are supported.

**Workaround**: Tests use constant object values.

**Future Enhancement**: Support variable objects by reading from `ctx->O` array at runtime.

### 3. Subject Selection Based on Run Predicate Only

**Current Behavior**: CONSTRUCT8 only processes subjects from the pinned run (filtered by predicate)

**Impact**: Cannot construct triples from multiple predicates in a single CONSTRUCT8 call.

**Workaround**: Multiple CONSTRUCT8 calls for different predicates.

**Future Enhancement**: Support multi-predicate CONSTRUCT8 (may exceed 8-tick budget).

## Test Coverage Gaps

### 1. Variable Object Template
- **Status**: Not tested (not supported)
- **Priority**: Low (future enhancement)

### 2. Different Template Predicate
- **Status**: Not tested (not supported)
- **Priority**: Medium (common CONSTRUCT use case)

### 3. Multiple Predicates in Single CONSTRUCT
- **Status**: Not tested (not supported)
- **Priority**: Low (exceeds 8-tick budget)

### 4. Streaming Turtle Parsing
- **Status**: Not tested
- **Priority**: Medium (for large files)

### 5. Receipt Merging (⊕ Operation)
- **Status**: Not tested
- **Priority**: Medium (batch processing)

### 6. Error Recovery
- **Status**: Partial (empty runs, invalid inputs)
- **Priority**: High (production readiness)

## Fixed Issues

### ✅ Predicate Mismatch in Tests
- **Issue**: Tests were using different predicates for run and template
- **Fix**: Updated tests to use same predicate for both (matching current implementation)
- **Location**: `tests/chicago_construct8_pipeline.c:93-96`

### ✅ Unused Variable Warning
- **Issue**: `base_iri` variable was unused
- **Fix**: Removed variable or added comment explaining its purpose
- **Location**: `tests/chicago_construct8_pipeline.c:242`

### ✅ Rust Test Predicate Mismatch
- **Issue**: Rust tests were using different predicates for run and template
- **Fix**: Updated Rust tests to use same predicate for both
- **Location**: `rust/knhk-integration-tests/tests/construct8_pipeline.rs`

## Test Execution Status

### C Tests
- ✅ Compiles without warnings (after fixes)
- ✅ Runs successfully
- ✅ All test cases pass

### Rust Tests
- ✅ Compiles successfully
- ✅ All test cases pass
- ⚠️ Requires `std` feature (not `no_std` compatible)

## Recommended Next Steps

1. **Remove predicate check**: Allow CONSTRUCT8 to use different template predicate
2. **Add variable object support**: Read objects from `ctx->O` array
3. **Add receipt merging tests**: Test `Receipt::merge()` operation
4. **Add streaming parser tests**: Test with large Turtle files
5. **Add error recovery tests**: Test error handling and recovery scenarios

