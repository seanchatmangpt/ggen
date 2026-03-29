# YIdentifier Cascade Delete Pattern

## Runtime Evidence
- **File:** `docs/validation/evidence/cascade-delete-exceptions.log`
- **Test Case:** `YIdentifierCascadeDeleteTest.testCascadeDeleteCausesObjectDeletedException`
- **Engine Version:** YAWL 6.0.0 GA

## The Problem

```java
// Parent-child YIdentifier relationship
YIdentifier parent = new YIdentifier("case-1");
session.save(parent);

YIdentifier child = new YIdentifier(parent, "child-1");
session.save(child);

// Database FK constraint cascades delete child when parent deleted
// But Hibernate session doesn't know
session.delete(parent);

// Later attempt to merge child fails
session.merge(child);  // Throws ObjectDeletedException
```

## The Fix

```java
// Before merge, refresh from database to sync with DB state
try {
    session.refresh(child);
} catch (EntityNotFoundException e) {
    // Child was cascade-deleted, handle gracefully
    logger.warn("YIdentifier child was cascade-deleted: " + child.getId());
    return;
}

// Now merge will succeed or fail with clear error
session.merge(child);
```

## Why This Works

Database FK constraints with `ON DELETE CASCADE` automatically delete child records when parent is deleted. However, Hibernate's first-level cache doesn't know about this database-side deletion. Attempting to merge a deleted child throws `ObjectDeletedException`.

The fix calls `session.refresh()` to sync the Hibernate session with the current database state. If the child was deleted, `refresh()` throws `EntityNotFoundException`, which we catch and handle gracefully.

## Validation

Run: `mvn test -Dtest=YIdentifierCascadeDeleteTest`
Expected: 100% pass rate, no ObjectDeletedException after refresh

## Files
- **Implementation:** `yawl-core/src/main/java/org/yawlfoundation/yawl/elements/state/YIdentifier.java`
- **Test:** `test/org/yawlfoundation/yawl/validation/YIdentifierCascadeDeleteTest.java`
- **Evidence:** `docs/validation/evidence/cascade-delete-exceptions.log`
