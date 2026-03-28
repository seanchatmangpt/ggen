# TRANSACTIONAL SERVICE — Java 26 Pattern Language #19

> **"Each service in the system has a clear boundary of transactional responsibility.
> Business logic lives here — not in controllers, not in repositories."**

---

## Pattern Card

| Attribute         | Value                                                   |
|-------------------|---------------------------------------------------------|
| **Number**        | #19                                                     |
| **Name**          | TRANSACTIONAL SERVICE                                   |
| **Category**      | Domain Layer / Spring Boot                              |
| **Forces**        | Consistency vs. Performance, Cohesion vs. Coupling      |
| **Resulting Context** | Clear service boundary, optimized read/write paths |
| **Related Patterns** | #18 JPA ENTITY, #20 SPRING DATA REPOSITORY, #21 REST CONTROLLER |

---

## The Problem

You have a domain entity that needs to be created, read, updated, and deleted. The naive
approach puts SQL in controllers, business rules in repositories, and transactions
scattered everywhere. The result is:

- Untestable controllers (they call JPA directly)
- Repositories that know too much about business rules
- Transaction boundaries that cut across the wrong seams
- No single place to audit, log, or instrument domain operations

---

## The Solution

Place all business logic in a `@Service` class that owns the transaction boundary.

- **Read methods** carry `@Transactional(readOnly = true)` — Hibernate skips dirty-checking,
  Spring can route to read replicas, connection pools can optimize.
- **Write methods** carry `@Transactional` — full ACID protection, rollback on exception.
- **Command records** express intent as immutable value objects. Controllers pass commands in;
  the service decides what to do with them.

```
                  Controller
                      │  Create{{ className }}Command
                      ▼
              ┌──────────────────┐
              │  @Service        │  ← transaction boundary lives HERE
              │  @Transactional  │
              │                  │
              │  create(cmd)     │──► Repository ──► DB
              │  findById(id)    │──► Repository (readOnly)
              │  update(id, cmd) │──► Repository ──► DB
              │  delete(id)      │──► Repository ──► DB
              └──────────────────┘
```

---

## Structure

```
{{ className }}Service
├── findById(UUID)          — @Transactional(readOnly=true)
├── findAll()               — @Transactional(readOnly=true)
├── create(CreateCmd)       — @Transactional
├── update(UUID, UpdateCmd) — @Transactional
├── delete(UUID)            — @Transactional
├── record Create{{ className }}Command(...)
└── record Update{{ className }}Command(...)
```

### Why Inner Command Records?

Java 26 records are ideal command objects:

- **Immutable** — can't be accidentally mutated between validation and execution
- **Transparent** — `toString()`, `equals()`, `hashCode()` for free
- **Self-documenting** — the record name says what the user *intends* to do
- **Co-located** — living inside the service keeps the API cohesive

```java
public record Create{{ className }}Command(String name, BigDecimal price) {}

// At the call site, intent is obvious:
service.create(new Create{{ className }}Command("Widget Pro", new BigDecimal("29.99")));
```

---

## Code Walkthrough

### Read/Write Separation

```java
@Transactional(readOnly = true)          // ← performance flag
public Optional<{{ className }}> findById(UUID id) {
    return repository.findById(id);
}

@Transactional                            // ← full ACID
public {{ className }} create(Create{{ className }}Command cmd) {
    var entity = new {{ className }}(cmd.name(), cmd.price());
    return repository.save(entity);
}
```

The `readOnly = true` flag tells Spring/Hibernate:
1. Skip automatic dirty-checking at flush time
2. Set `Connection.setReadOnly(true)` on the JDBC connection
3. Allow routing to a read replica if your DataSource supports it

On high-read services this can reduce latency by 15–30% with zero code change.

### Entity-Not-Found Pattern

```java
@Transactional
public void delete(UUID id) {
    if (!repository.existsById(id)) {
        throw new EntityNotFoundException("{{ className }} not found: " + id);
    }
    repository.deleteById(id);
}
```

Throwing `EntityNotFoundException` (a JPA standard) lets a `@ControllerAdvice` map it
to `404 Not Found` automatically — no try/catch in controllers.

---

## Testing Strategy

This pack generates a companion `{{ className }}ServiceTest` that follows **Chicago TDD**:

| Test                          | Verifies                                             |
|-------------------------------|------------------------------------------------------|
| `findById_delegatesToRepository` | Service delegates to repository, wraps result     |
| `findById_returnsEmpty_whenNotFound` | Empty Optional propagated correctly           |
| `findAll_returnsListFromRepository` | List returned as-is                           |
| `delete_callsDeleteById_whenEntityExists` | Happy-path delete                       |
| `delete_throwsEntityNotFoundException_whenNotFound` | Guard condition enforced     |
| `update_throwsEntityNotFoundException_whenNotFound` | Guard condition enforced     |

Run with:

```bash
cargo make test   # via ggen test runner
# or directly:
./mvnw test -pl :catalog-service -Dtest={{ className }}ServiceTest
```

---

## How to Use This Pack

### 1. Add to your ggen project

```bash
ggen install java26-transactional-service
```

### 2. Edit the ontology

```bash
vim marketplace/packages/java26-transactional-service/ontology/domain.ttl
```

Replace the `Product` example with your aggregate root and fields.

### 3. Generate

```bash
ggen sync --rule transactional-service
ggen sync --rule transactional-service-test
```

### 4. Fill in the TODOs

The generated service compiles immediately. Two `TODO` markers indicate where your
specific construction logic goes:

```java
@Transactional
public {{ className }} create(Create{{ className }}Command cmd) {
    // TODO: construct entity from command
    var entity = new {{ className }}(cmd.name());   // ← implement this
    return repository.save(entity);
}
```

---

## Forces Balanced By This Pattern

| Force | How the pattern resolves it |
|-------|-----------------------------|
| **Consistency** | All writes go through one `@Transactional` boundary |
| **Performance** | `readOnly=true` eliminates unnecessary dirty-check overhead |
| **Testability** | Service takes repository by constructor — easy to mock in tests |
| **Cohesion** | Command records live next to the operations that consume them |
| **Encapsulation** | Controllers never see repository — only service API |

---

## Resulting Context

After applying TRANSACTIONAL SERVICE:

- Your controller becomes a thin HTTP adapter (see Pattern #21 REST CONTROLLER)
- Your repository is a pure data access object (see Pattern #20 SPRING DATA REPOSITORY)
- All business invariants live in one place, fully unit-testable
- Read/write transaction separation is consistent across the codebase
- Command records give you a type-safe audit trail of domain operations

---

## Anti-Patterns Avoided

| Anti-Pattern | How This Pattern Prevents It |
|---|---|
| **Anemic Service** | Service owns logic, not just CRUD pass-through |
| **Fat Controller** | Controller delegates everything; never calls repository directly |
| **Transaction Chaos** | One `@Transactional` per service method, no nested TX surprises |
| **Primitive Obsession** | `UUID id` not `String id`; `BigDecimal price` not `double price` |

---

## Related Patterns

- **#18 JPA ENTITY** — The entity this service manages
- **#20 SPRING DATA REPOSITORY** — The repository this service delegates to
- **#21 REST CONTROLLER** — The HTTP adapter that calls this service
- **#22 PATTERN-MATCHED CONTROLLER** — Controller variant using switch expressions for status dispatch
- **#23 VIRTUAL THREAD DAO** — Use with `spring.threads.virtual.enabled=true` for loom-based I/O

---

## Ontology Extension Points

The query selects `?className`, `?javaPackage`, and all `?fields` from any
`java26:AggregateRoot`. To add a field:

```turtle
my:sku a java26:Field ;
    java26:fieldName "sku" ;
    java26:fieldType "String" ;
    java26:id false .

my:Product java26:field my:sku .
```

Re-run `ggen sync` to regenerate the command records with the new field.

---

*Generated by ggen marketplace — Java 26 Pattern Language*
*Pattern #19: TRANSACTIONAL SERVICE*
