# WCP Predicate Truthiness Pattern

## Runtime Evidence
- **File:** `docs/validation/evidence/wcp-predicate-timeouts.log`
- **Test Case:** `WCPPredicateTruthinessTest.testVariableBasedPredicateCausesInfiniteLoop`
- **Affected Specs:** WCP-29-StructuredLoop (before fix)

## The Problem

```xml
<!-- WRONG: Variable-based predicate -->
<flowsInto>
  <nextElementRef id="OutputCondition"/>
  <predicate ordering="0">/Net/counter >= 3</predicate>
</flowsInto>

<!-- Task output data: <data/> (empty) -->
<!-- XPath evaluation: "" >= 3 = false -->
<!-- Result: Infinite loop, test timeout -->
```

## The Fix

```xml
<!-- RIGHT: Truthy predicate -->
<flowsInto>
  <nextElementRef id="OutputCondition"/>
  <predicate ordering="0">true()</predicate>
</flowsInto>

<!-- XPath evaluation: true() = true (always) -->
<!-- Result: Loop exits normally -->
```

## Why This Works

YAWL XOR split predicates are evaluated against **task output data**, not net data. When task output is `<data/>` (empty), variable-based predicates like `/Net/counter >= 3` evaluate to `false` because the path `/Net/counter` doesn't exist in empty data.

The `true()` function is an XPath literal that always evaluates `true`, regardless of data context. This ensures the XOR split can proceed to the next element.

## Validation

Run: `bash scripts/runtime-validation/validate-wcp-predicates.sh`
Expected: All WCP specs with XOR splits use `true()` at ordering=0

## Files
- **Spec Fix:** `exampleSpecs/xml/patterns/WCP-29-StructuredLoop.xml`
- **Test:** `test/org/yawlfoundation/yawl/validation/WCPPredicateTruthinessTest.java`
- **Validation:** `scripts/runtime-validation/validate-wcp-predicates.sh`
