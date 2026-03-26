# Rule 4: Spring Data Repository Generation

Generates Spring Data JPA repository interfaces for entity persistence operations.

**Rule ID**: 4
**Input**: JPA entities from Rule 3
**Output**: Java `.java` interface files
**Pattern**: `Rule<RepositoryQuery, RepositoryTemplate>`

---

## Overview

Rule 4 creates Spring Data JPA repository interfaces that extend `JpaRepository<Entity, ID>`:
- CRUD operations (save, delete, findById, etc.)
- Custom finder methods
- Pagination and sorting support
- `@Repository` annotation

---

## Generated Output Structure

### Single Repository File

**File Path**: `repository/{EntityName}Repository.java`

**Example**: `repository/YWorkItemRepository.java`

```java
package com.example.yawl.repository;

import com.example.yawl.entity.YWorkItem;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import java.util.List;
import java.util.Optional;

/**
 * Spring Data JPA repository for YWorkItem entity
 */
@Repository
public interface YWorkItemRepository extends JpaRepository<YWorkItem, Long> {

    // Finder methods generated based on entity properties
    Optional<YWorkItem> findByTaskId(String taskId);

    List<YWorkItem> findByStatus(String status);

    List<YWorkItem> findByStatusOrderByCreatedAtDesc(String status);
}
```

---

## Generated Methods

### Automatic Methods (from JpaRepository)

| Method | Signature |
|--------|-----------|
| Save | `<S extends T> S save(S entity)` |
| SaveAll | `<S extends T> Iterable<S> saveAll(Iterable<S> entities)` |
| FindById | `Optional<T> findById(ID id)` |
| FindAll | `Iterable<T> findAll()` |
| FindAll (Pageable) | `Page<T> findAll(Pageable pageable)` |
| Count | `long count()` |
| Delete | `void delete(T entity)` |
| DeleteById | `void deleteById(ID id)` |
| DeleteAll | `void deleteAll()` |

### Custom Finder Methods

Generated based on entity fields:

| Field | Generated Method |
|-------|------------------|
| `status` | `findByStatus(String status)` |
| `taskId` | `findByTaskId(String taskId)` |
| `createdAt` | `findByCreatedAtGreaterThan(LocalDateTime date)` |
| Combination | `findByStatusAndTaskId(String status, String taskId)` |

### Method Query Derivation

From property name, Spring Data derives:
- `findBy` - SELECT all matching
- `And` - Multiple conditions
- `OrderBy` - Ordering
- `Asc` / `Desc` - Sort direction

**Example**:
```java
// From property "status" and "createdAt"
findByStatusAndCreatedAtGreaterThanOrderByCreatedAtDesc(
    String status,
    LocalDateTime date
)
```

---

## Annotations

| Annotation | Purpose |
|-----------|---------|
| `@Repository` | Spring component for DAO pattern |
| `@Transactional` | Transaction management (optional on custom methods) |

---

## Type Parameters

- `<T>` - Entity type (e.g., `YWorkItem`)
- `<ID>` - Primary key type (usually `Long`)

**Example**:
```java
public interface UserRepository extends JpaRepository<User, Long> {
    // T = User, ID = Long
}
```

---

## Generated Files

| File Type | Count | Size | Notes |
|-----------|-------|------|-------|
| Repository interface | 1 per entity | ~1KB | Spring Data interface |

---

## Testing

### Test Coverage

| Test | Purpose | Status |
|------|---------|--------|
| Repository creation | Interface extends JpaRepository | PASS |
| @Repository annotation | Annotation present | PASS |
| Naming convention | EntityRepository pattern | PASS |
| Correct imports | JpaRepository, @Repository imports | PASS |

### Test Command

```bash
cargo test -p ggen-yawl rule_4_
```

---

## Performance SLO

| Metric | Target |
|--------|--------|
| Per repository | <30ms |
| 10 repositories | <300ms |

---

## Common Use Cases

### Use Case 1: Simple CRUD Repository

```java
@Repository
public interface OrderRepository extends JpaRepository<Order, Long> {
    Optional<Order> findByOrderNumber(String orderNumber);
}
```

### Use Case 2: Complex Query

```java
// Custom query method with multiple conditions
List<Order> findByStatusAndCreatedAtGreaterThanAndTotalGreaterThan(
    String status,
    LocalDateTime date,
    BigDecimal amount
);
```

### Use Case 3: Pagination

```java
// JpaRepository automatically supports pagination
Page<Order> findByStatus(String status, Pageable pageable);

// Usage:
Pageable page = PageRequest.of(0, 20, Sort.by("createdAt").descending());
Page<Order> orders = orderRepository.findByStatus("PENDING", page);
```

---

## Related Rules

- **Rule 3**: [JPA Entity](./RULE_3_JPA_ENTITIES.md) - Entities this rule creates repos for
- **Rule 8**: [Service](./RULE_8_SERVICES.md) - Services that inject these repositories
- **Rule 6**: [Controller](./RULE_6_CONTROLLERS.md) - Controllers using services that use these repos

---

**Version**: 0.1.0
**Last Updated**: 2026-03-26
