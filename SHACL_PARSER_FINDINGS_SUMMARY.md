# SHACL Parser Deep-Dive: Executive Summary

## Analysis Results

**File**: `/home/user/ggen/crates/ggen-ai/src/codegen/shacl_parser.rs` (916 lines)

**Validation Scope**:
- SHACL constraint extraction correctness
- XSD type mapping accuracy
- W3C SHACL Core compliance
- Error handling robustness
- Integration architecture

---

## Key Findings

### 1️⃣ Constraint Extraction: PARTIAL COMPLIANCE ⚠️

**What's Working** (9 constraints implemented correctly):
```
✅ sh:minCount, sh:maxCount       (cardinality)
✅ sh:minLength, sh:maxLength     (string bounds)
✅ sh:pattern                      (regex validation)
✅ sh:datatype                     (XSD type specification)
✅ sh:in / sh:oneOf               (enumeration lists via RDF traversal)
✅ sh:class                        (semantic type constraints)
```

**What's Missing** (9 core constraints NOT supported):
```
❌ sh:minInclusive/maxInclusive    (numeric range ≥ ≤) - HIGH IMPACT
❌ sh:minExclusive/maxExclusive    (numeric range > <) - HIGH IMPACT
❌ sh:nodeKind                     (IRI/Literal/BlankNode enforcement) - MEDIUM IMPACT
❌ sh:node                         (nested shape validation) - MEDIUM IMPACT
❌ sh:and, sh:or, sh:not           (shape composition) - MEDIUM IMPACT
❌ sh:closed                       (property enumeration) - MEDIUM IMPACT
```

**W3C Compliance**: **41% of Core Profile** (9/22 constraints)

---

### 2️⃣ CRITICAL DESIGN ISSUE: sh:targetClass Extraction ❌

**The Problem**:
```
SHACLParser doesn't extract sh:targetClass!
This is the PRIMARY KEY for finding which classes a shape targets.
```

**Current Code Flow**:
1. `SHACLParser.extract_properties(shape_uri)` ← Requires caller to know shape_uri
2. But how does caller find shape_uri? → Must use `TTLToSignatureTranspiler` separately
3. `TTLToSignatureTranspiler.find_classes_with_shapes()` queries sh:targetClass

**Impact**:
- SHACL parsing logic is split between two classes
- API is incomplete and requires external knowledge
- Violates separation of concerns

**Recommended Fix**: Add to SHACLParser:
```rust
pub fn extract_target_class(&self, shape_uri: &str) -> Result<Option<String>> {
    // Find the sh:targetClass for a shape
}

pub fn find_shapes_for_class(&self, class_uri: &str) -> Result<Vec<String>> {
    // Find all shapes targeting a class
}
```

---

### 3️⃣ Type Mapping: COMPREHENSIVE ✅

**Coverage**: 27 XSD types mapped to Rust types
- ✅ All string types (10 variants)
- ✅ All numeric types (signed, unsigned, special integers)
- ✅ Floating-point types (f32, f64)
- ✅ Boolean, binary, URI types
- ⚠️ Date/time types (map to String instead of proper DateTime)

**Quality**: Unit tests validate all 22 type mappings (lines 604-879)

**Gaps**:
1. DateTime types → String (loses semantic meaning)
2. Unbounded integers → i64 (loses precision semantics)
3. No Decimal type for financial data
4. gYear mapped to u16 (should be u32 for 4-digit years)

---

### 4️⃣ Error Handling: GRACEFUL BUT GENERIC ⚠️

**Strengths**:
- ✅ Uses `Result<T, E>` throughout (no unwrap/panic in production)
- ✅ Returns `Option<T>` for missing constraints (graceful degradation)
- ✅ All 9 constraint queries wrapped in error handling

**Weaknesses**:
1. **Generic Error Type**: All errors become `GgenAiError::Other { message }`
   - Cannot distinguish SPARQL failure from RDF store failure
   - Callers cannot implement retry logic

2. **Silent Failures on Errors**:
   ```rust
   if let Ok(Some(count)) = self.extract_integer_property(...) {
       constraint.min_count = Some(count as usize);
   }
   // If Err is returned, it's silently ignored! ❌
   ```

3. **SPARQL Injection Risk** (lines 261-270):
   ```rust
   let query = format!("<{subject_uri}> {property} ?value .");
   // subject_uri is unvalidated! Special characters break query
   ```

4. **RDF List Parsing**:
   - Assumes well-formed rdf:rest* chains
   - No validation that chain terminates at rdf:nil
   - No cycle detection

---

### 5️⃣ Performance Observation: 8 Queries Per Property ⚠️

Current implementation (lines 205-245):
```rust
pub fn extract_constraint(&self, prop_shape_uri: &str) -> Result<SHACLConstraint> {
    // 8 separate SPARQL queries:
    extract_integer_property(prop_shape_uri, "sh:minCount")    // Query 1
    extract_integer_property(prop_shape_uri, "sh:maxCount")    // Query 2
    extract_integer_property(prop_shape_uri, "sh:minLength")   // Query 3
    extract_integer_property(prop_shape_uri, "sh:maxLength")   // Query 4
    extract_string_property(prop_shape_uri, "sh:pattern")      // Query 5
    extract_datatype(prop_shape_uri)                           // Query 6
    extract_enum_values(prop_shape_uri)                        // Query 7
    extract_one_of_values(prop_shape_uri)                      // Query 8
}
```

**Optimization**: Use single SELECT with OPTIONAL clauses → 1 query per property

---

## Test Coverage Assessment

**What's Tested**:
- ✅ Type mappings (22 test cases in shacl_parser.rs)
- ✅ Constraint struct behavior (6 tests for SHACLConstraint)
- ✅ TTL-to-signature pipeline (40+ tests in ttl_to_signature.rs)

**What's NOT Tested**:
- ❌ Actual constraint extraction from SHACL data
- ❌ RDF list parsing (sh:in with multiple elements)
- ❌ Error handling on malformed SHACL
- ❌ Edge cases (empty lists, circular shapes)
- ❌ Integration of SHACLParser with real shapes

**Test Fixtures Available** (Good!):
- shape_with_constraints.ttl (string length, pattern, enum)
- shape_with_datatypes.ttl (7 different XSD types)
- shape_with_multiple_classes.ttl (3 classes)
- malformed_rdf.ttl (error case)

**Recommendation**: Add targeted unit tests calling `extract_constraint()` directly on fixtures

---

## Compliance Checklist: W3C SHACL 1.0.1

| Constraint | Status | Notes |
|-----------|--------|-------|
| NodeShape (implicit) | ⚠️ Partial | Not explicitly verified |
| sh:targetClass | ❌ Missing | Critical gap |
| sh:targetNode | ❌ Missing | Not used in current design |
| sh:targetSubjectsOf | ❌ Missing | Not used in current design |
| **sh:property** | ✅ **CORRECT** | Extracts property shapes and paths |
| **sh:path** | ✅ **CORRECT** | Property path extraction working |
| **sh:minCount** | ✅ **CORRECT** | Cardinality extraction |
| **sh:maxCount** | ✅ **CORRECT** | Cardinality extraction |
| **sh:minLength** | ✅ **CORRECT** | String bounds |
| **sh:maxLength** | ✅ **CORRECT** | String bounds |
| **sh:pattern** | ✅ **CORRECT** | Regex string stored as-is |
| **sh:datatype** | ✅ **CORRECT** | Mapped to Rust types |
| **sh:class** | ✅ **CORRECT** | Semantic type extraction |
| **sh:in** | ✅ **CORRECT** | RDF list parsing |
| **sh:oneOf** | ✅ **CORRECT** | RDF list parsing |
| sh:minInclusive | ❌ Missing | Numeric range (≥) |
| sh:maxInclusive | ❌ Missing | Numeric range (≤) |
| sh:minExclusive | ❌ Missing | Numeric range (>) |
| sh:maxExclusive | ❌ Missing | Numeric range (<) |
| sh:nodeKind | ❌ Missing | Node type enforcement |
| sh:node | ❌ Missing | Nested shape validation |
| sh:and | ❌ Missing | Shape conjunction |
| sh:or | ❌ Missing | Shape disjunction |
| sh:not | ❌ Missing | Shape negation |
| sh:closed | ❌ Missing | Property enumeration |

**Compliance Score**: 9/22 = **41%**

---

## Priority Fixes (Recommended Order)

### 🔴 CRITICAL (Affects API Completeness)

1. **Add sh:targetClass extraction** (2 hours)
   - Without this, SHACLParser is incomplete
   - Currently handled by TTLToSignatureTranspiler only
   - Add `extract_target_class()` and `find_shapes_for_class()` methods

2. **Add SPARQL injection defense** (1 hour)
   - Validate or escape subject_uri parameters
   - Prevent malformed queries on special characters

3. **Add constraint extraction integration tests** (3 hours)
   - Test `extract_constraint()` on real SHACL fixtures
   - Verify minLength, maxLength, pattern, enum extraction

### 🟠 HIGH (Expands Supported Constraints)

4. **Implement numeric range constraints** (3 hours)
   - Add sh:minInclusive/maxInclusive/minExclusive/maxExclusive
   - High impact for validation domains (age, amounts, years)

5. **Implement sh:nodeKind** (2 hours)
   - Support IRI, Literal, BlankNode constraints
   - Separate data from structure validation

6. **Fix RDF list validation** (2 hours)
   - Validate termination at rdf:nil
   - Add cycle detection for malformed lists
   - Handle empty lists correctly

### 🟡 MEDIUM (Improves Code Quality)

7. **Optimize constraint extraction** (2 hours)
   - Reduce 8 queries per property to 1
   - Use SELECT with OPTIONAL clauses

8. **Improve error handling** (2 hours)
   - Create specific error types instead of generic
   - Propagate errors instead of silently ignoring
   - Add debug logging

### 🟢 LOW (Advanced Features)

9. **Add shape composition** (4-6 hours)
   - sh:and, sh:or, sh:not support
   - Recursive shape validation

10. **Add advanced constraints** (4-6 hours)
    - sh:node (nested shape validation)
    - sh:closed (property enumeration)
    - sh:ignoredProperties

---

## Validation Summary Table

| Aspect | Coverage | Status | Grade |
|--------|----------|--------|-------|
| **Core Constraints** | 9/22 | 41% coverage, basic features work | D+ |
| **XSD Type Mapping** | 27/27 | Comprehensive but DateTime gaps | A |
| **Error Handling** | Generic, graceful | Silent failures on errors | C+ |
| **Code Structure** | Clear and organized | Good separation, but sh:targetClass issue | B |
| **Test Coverage** | Type mapping tested | No constraint extraction tests | C |
| **W3C Compliance** | Core Profile | Partial, missing 9 constraints | D |
| **Production Ready** | For simple shapes | Not for complex SHACL | ⚠️ |

**Overall Grade: C** (Functional for basic use, needs work for production)

---

## Code Locations Reference

### Main Implementation
- **Parser main class**: `SHACLParser` (lines 119-512)
- **Constraint struct**: `SHACLConstraint` (lines 54-117)
- **Type mapping**: `map_xsd_to_rust_type()` (lines 523-579)
- **Property extraction**: `extract_properties()` (lines 142-186)
- **Constraint extraction**: `extract_constraint()` (lines 198-248)

### Individual Constraint Extraction
- **Integer properties**: `extract_integer_property()` (lines 260-299) - minCount, maxCount, minLength, maxLength
- **String properties**: `extract_string_property()` (lines 311-354) - pattern, class
- **Datatype**: `extract_datatype()` (lines 365-405)
- **Enumeration**: `extract_enum_values()` (lines 416-458) - sh:in
- **OneOf values**: `extract_one_of_values()` (lines 469-511) - sh:oneOf

### Related Code (Integration Points)
- **TTL to Signature transpiler**: `/home/user/ggen/crates/ggen-ai/src/codegen/ttl_to_signature.rs` (lines 141-169)
- **Test fixtures**: `/home/user/ggen/crates/ggen-ai/tests/fixtures/`
- **Integration tests**: `/home/user/ggen/crates/ggen-ai/tests/ttl_to_signature.rs` (40+ tests)

---

## Conclusion

The SHACL parser provides **functional but incomplete** SHACL support:

✅ **What works well**:
- Extracts basic SHACL constraints correctly
- Comprehensive XSD type mapping
- Graceful error handling with Option<T>
- Clear code structure and documentation

❌ **What needs work**:
- Missing sh:targetClass extraction (design flaw)
- 9 important W3C constraints not supported
- No targeted unit tests for constraint extraction
- SPARQL injection vulnerability
- RDF list parsing needs validation
- Silent failures when constraint extraction fails

**Recommendation**: Implement Priority 1 fixes before using in production with complex SHACL schemas. Current implementation is suitable for simple shapes but will have gaps with modern SHACL designs.

**Estimated effort to reach 80% W3C compliance**: 30-40 hours

For detailed findings, see: `/home/user/ggen/SHACL_PARSER_VALIDATION_REPORT.md`
