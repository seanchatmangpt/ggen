# Hibernate 7 MARKED_ROLLBACK Recovery Pattern

## Runtime Evidence
- **File:** `docs/validation/evidence/rollback-exceptions.log`
- **Test Case:** `HibernateMarkedRollbackTest.testMergeAfterCascadeDeleteCausesRollbackOnly`
- **Engine Version:** YAWL 6.0.0 GA
- **Hibernate:** 7.2.0.Final

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

### Runtime Stack Trace

```
org.hibernate.HibernateException: Calling method 'merge' is not valid without an active transaction (Current status: MARKED_ROLLBACK)
    at org.hibernate.context.internal.ThreadLocalSessionContext$TransactionProtectionWrapper.invoke(ThreadLocalSessionContext.java:329)
    at jdk.proxy3/jdk.proxy3.$Proxy95.merge(Unknown Source)
    at org.yawlfoundation.yawl.engine.YPersistenceManager.doPersistAction(YPersistenceManager.java:548)
```

## The Fix

```java
} catch (Exception e) {
    // Detect MARKED_ROLLBACK in INSERT operations and attempt recovery
    if (!update && e.getMessage() != null && e.getMessage().contains("MARKED_ROLLBACK")) {
        logger.warn("Transaction marked ROLLBACK_ONLY by previous merge failure. Attempting recovery.");
        
        // Rollback the contaminated transaction
        if (tx != null && tx.isActive()) {
            tx.rollback();
        }
        
        // Start fresh transaction
        tx = session.beginTransaction();
        
        // Retry the persist operation once
        try {
            session.persist(obj);
            tx.commit();
            logger.debug("MARKED_ROLLBACK recovery successful");
            return;
        } catch (Exception retryException) {
            logger.error("Retry failed after MARKED_ROLLBACK recovery", retryException);
        }
    }
    
    // Original error handling
    String msg = String.format("Failure detected whilst persisting instance of %s",
                               obj.getClass().getSimpleName());
    tx.rollback();
    throw new YPersistenceException(msg, e);
}
```

## Validation

Run: `mvn test -Dtest=HibernateMarkedRollbackTest`
Expected: 100% pass rate, FATAL flag never set

## Files
- **Implementation:** `yawl-core/src/main/java/org/yawlfoundation/yawl/engine/YPersistenceManager.java:524-574`
- **Test:** `test/org/yawlfoundation/yawl/validation/HibernateMarkedRollbackTest.java`
- **Evidence:** `docs/validation/evidence/rollback-exceptions.log`
