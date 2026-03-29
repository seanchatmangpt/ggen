# Hibernate 7 MARKED_ROLLBACK Recovery Pattern

## Runtime Evidence
- **File:** `docs/validation/evidence/rollback-exceptions.log`
- **Test Case:** `HibernateMarkedRollbackTest.testMergeAfterCascadeDeleteCausesRollbackOnly`
- **Engine Version:** YAWL 6.0.0 GA
- **Hibernate:** 7.0.0.Final

## The Problem

```java
// Step 1: merge() throws ObjectDeletedException (cascade-deleted child)
session.merge(entity);  // Throws ObjectDeletedException
// Transaction is NOW marked ROLLBACK_ONLY by Hibernate

// Step 2: Catching the exception DOES NOT clear ROLLBACK_ONLY
try {
    session.merge(entity);
} catch (ObjectDeletedException e) {
    logger.warn("Entity deleted, skipping merge");
    // Transaction status: ROLLBACK_ONLY (still!)
}

// Step 3: Next INSERT fails with HibernateException
session.persist(newEntity);  // Throws: "MARKED_ROLLBACK"
```

## The Fix

```java
} catch (Exception e) {
    // Detect MARKED_ROLLBACK in INSERT operations
    if (!update && e.getMessage() != null && e.getMessage().contains("MARKED_ROLLBACK")) {
        logger.warn("Transaction marked ROLLBACK_ONLY by previous merge failure. Attempting recovery.");
        // Rollback the contaminated transaction
        tx.rollback();
        // Start fresh transaction
        tx = session.beginTransaction();
        // Retry INSERT once
        try {
            session.persist(obj);
            tx.commit();
            return;
        } catch (Exception retryException) {
            logger.error("Retry failed after MARKED_ROLLBACK recovery", retryException);
        }
    }
    // Original error handling
    tx.rollback();
    throw new YPersistenceException(msg, e);
}
```

## Why This Works

When Hibernate throws `ObjectDeletedException` during `session.merge()`, it marks the current transaction as ROLLBACK_ONLY. This flag prevents any further operations in that transaction, including INSERT. The fix detects this condition, rolls back the contaminated transaction, starts a fresh one, and retries the INSERT operation once.

## Validation

Run: `mvn test -Dtest=HibernateMarkedRollbackTest`
Expected: 100% pass rate, FATAL flag never set

## Files
- **Implementation:** `yawl-core/src/main/java/org/yawlfoundation/yawl/engine/YPersistenceManager.java:524-574`
- **Test:** `test/org/yawlfoundation/yawl/validation/HibernateMarkedRollbackTest.java`
- **Evidence:** `docs/validation/evidence/rollback-exceptions.log`
