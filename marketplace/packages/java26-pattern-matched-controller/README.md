# PATTERN-MATCHED CONTROLLER — Java 26 Pattern Language #22

> **"The controller dispatches over the shape of domain data, not its content.
> The compiler proves exhaustiveness; you never forget a case."**

---

## Pattern Card

| Attribute         | Value                                                       |
|-------------------|-------------------------------------------------------------|
| **Number**        | #22                                                         |
| **Name**          | PATTERN-MATCHED CONTROLLER                                  |
| **Category**      | Presentation Layer / Spring Boot / Java 26 Language         |
| **Forces**        | Exhaustiveness vs. Extensibility, Safety vs. Boilerplate    |
| **Resulting Context** | Compiler-verified dispatch, no missing HTTP responses   |
| **Related Patterns** | #19 TRANSACTIONAL SERVICE, #21 REST CONTROLLER, #15 SEALED DOMAIN HIERARCHY |

---

## The Problem

Your domain has a `status` field that can take on several distinct forms — each carrying
different data. The classic approach uses `instanceof` chains or `enum` switches:

```java
// Classic enum switch — works, but:
// • Adding a new status doesn't break the compiler here
// • The developer must remember to update every switch in the codebase
// • No way to carry per-variant data (PendingOrder has placedAt; ShippedOrder has trackingId)
String label = switch (order.getStatus()) {
    case PENDING   -> "Pending";
    case SHIPPED   -> "Shipped";
    // Forgot CANCELLED — compiles fine, runtime NPE or wrong response
    default        -> "Unknown";
};
```

The `default` branch silently swallows the forgotten case. And enums can't carry
variant-specific data, so you end up with nullable fields all over the entity.

---

## The Solution

Use a **sealed interface** for the status type. Each permitted subtype is a **record**
carrying exactly the data relevant to that state. The controller dispatches using a
**Java 26 switch expression with type patterns**.

```java
sealed interface OrderStatus permits PendingOrder, ShippedOrder, DeliveredOrder, CancelledOrder {}

record PendingOrder(Instant placedAt)        implements OrderStatus {}
record ShippedOrder(String trackingId)       implements OrderStatus {}
record DeliveredOrder(Instant deliveredAt)   implements OrderStatus {}
record CancelledOrder(String cancelReason)   implements OrderStatus {}
```

```java
String label = switch (order.getStatus()) {
    case PendingOrder  p -> "Pending since "      + p.placedAt();
    case ShippedOrder  s -> "Shipped, tracking: " + s.trackingId();
    case DeliveredOrder d -> "Delivered at "      + d.deliveredAt();
    case CancelledOrder c -> "Cancelled: "        + c.cancelReason();
    // No default needed — compiler proves this is exhaustive
};
```

If you add `RefundedOrder` to the sealed hierarchy but forget to add it to the switch,
the code **does not compile**. Every controller, every switch, everywhere — must be
updated before the application can be built.

---

## Structure

```
{{ className }}Controller
├── GET    /api/{{ className | lower }}s              → getAll()
├── GET    /api/{{ className | lower }}s/{id}         → getById()
├── POST   /api/{{ className | lower }}s              → create()
├── PUT    /api/{{ className | lower }}s/{id}         → update()
├── DELETE /api/{{ className | lower }}s/{id}         → delete()
└── GET    /api/{{ className | lower }}s/{id}/status  → getStatus()  ← pattern match here
```

The status endpoint is where the Java 26 switch expression lives. All other endpoints
are standard REST — thin delegates to `{{ className }}Service`.

---

## Java 26 Switch Expressions — Reference

### Basic Type Pattern

```java
String describe(Object obj) {
    return switch (obj) {
        case Integer i  -> "integer: " + i;
        case String  s  -> "string: "  + s.toUpperCase();
        case null       -> "nothing";
        default         -> "other: " + obj.getClass().getSimpleName();
    };
}
```

### Sealed Hierarchy — No Default Required

```java
sealed interface Shape permits Circle, Rectangle, Triangle {}
record Circle(double radius)               implements Shape {}
record Rectangle(double width, double h)   implements Shape {}
record Triangle(double base, double h)     implements Shape {}

double area(Shape s) {
    return switch (s) {
        case Circle    c -> Math.PI * c.radius() * c.radius();
        case Rectangle r -> r.width() * r.h();
        case Triangle  t -> 0.5 * t.base() * t.h();
        // No default — compiler verifies exhaustiveness via sealed contract
    };
}
```

### Guarded Patterns (Java 21+, stable in Java 26)

```java
String urgency(OrderStatus status) {
    return switch (status) {
        case ShippedOrder s when s.trackingId().startsWith("EXPRESS") -> "urgent";
        case ShippedOrder s                                           -> "standard";
        case PendingOrder p when p.placedAt().isBefore(cutoff)       -> "overdue";
        case PendingOrder p                                           -> "normal";
        case DeliveredOrder d                                         -> "complete";
        case CancelledOrder c                                         -> "cancelled";
    };
}
```

The `when` guard adds a boolean condition to the type pattern. Order matters: more
specific guards must appear before less specific ones.

---

## Code Walkthrough

### Controller HTTP Dispatch

```java
@GetMapping("/{id}/status")
public ResponseEntity<String> getStatus(@PathVariable UUID id) {
    return service.findById(id)
        .map(entity -> {
            String label = switch (entity.getStatus()) {
                case PendingOrder  p -> "Pending since "      + p.placedAt();
                case ShippedOrder  s -> "Shipped, tracking: " + s.trackingId();
                case DeliveredOrder d -> "Delivered at "      + d.deliveredAt();
                case CancelledOrder c -> "Cancelled: "        + c.cancelReason();
            };
            return ResponseEntity.ok(label);
        })
        .orElse(ResponseEntity.notFound().build());
}
```

Key observations:
1. The switch is an **expression** — it returns a value, not a statement
2. Each arm accesses **the specific record's fields** — `p.placedAt()`, `s.trackingId()`
3. No casting — the compiler binds the matched type to a local variable in each arm
4. No default — sealed contract guarantees exhaustiveness

### Sealed Domain Definition

```java
// Generated from Pattern #15 SEALED DOMAIN HIERARCHY
public sealed interface OrderStatus
    permits PendingOrder, ShippedOrder, DeliveredOrder, CancelledOrder {}

public record PendingOrder(Instant placedAt)        implements OrderStatus {}
public record ShippedOrder(String trackingId)       implements OrderStatus {}
public record DeliveredOrder(Instant deliveredAt)   implements OrderStatus {}
public record CancelledOrder(String cancelReason)   implements OrderStatus {}
```

This lives in the domain package, not the controller. The controller imports it and
dispatches on it — it does not define it.

---

## Testing Strategy

This pack generates a `{{ className }}ControllerTest` using `@WebMvcTest`:

| Test                              | HTTP Contract Verified                        |
|-----------------------------------|-----------------------------------------------|
| `getAll_returns200WithList`       | `GET /` → 200 with JSON array                 |
| `getById_returns200_whenFound`    | `GET /{id}` → 200 when service returns entity |
| `getById_returns404_whenNotFound` | `GET /{id}` → 404 when service returns empty  |
| `create_returns200_withCreatedEntity` | `POST /` → 200 with entity              |
| `delete_returns204_onSuccess`     | `DELETE /{id}` → 204 No Content               |
| `delete_returns404_whenNotFound`  | `DELETE /{id}` → 404 when EntityNotFound thrown |
| `getStatus_returns200_whenEntityExists` | `GET /{id}/status` → 200             |
| `getStatus_returns404_whenNotFound` | `GET /{id}/status` → 404               |

`@WebMvcTest` boots only the web layer. `@MockBean {{ className }}Service` replaces the real
service — tests run in milliseconds without a database.

Run with:

```bash
ggen sync --rule rest-controller-test
./mvnw test -Dtest={{ className }}ControllerTest
```

---

## How to Use This Pack

### 1. Install

```bash
ggen install java26-pattern-matched-controller
```

### 2. Define your sealed hierarchy first (Pattern #15)

```turtle
# ontology/domain.ttl
my:OrderStatus a java26:SealedInterface ;
    java26:name "OrderStatus" ;
    java26:javaPackage "com.example.orders" .

my:PendingOrder a java26:SealedRecord ;
    java26:name "PendingOrder" ;
    java26:implements my:OrderStatus ;
    java26:field my:placedAt .
# ... other variants
```

### 3. Generate

```bash
ggen sync --rule rest-controller
ggen sync --rule rest-controller-test
```

### 4. Activate the switch expression

In the generated controller, uncomment the switch block and replace the placeholder
with your actual sealed type and variants.

### 5. Wire the ControllerAdvice

Add an exception handler so `EntityNotFoundException` maps to 404:

```java
@RestControllerAdvice
public class DomainExceptionAdvice {
    @ExceptionHandler(EntityNotFoundException.class)
    ResponseEntity<String> handleNotFound(EntityNotFoundException ex) {
        return ResponseEntity.status(404).body(ex.getMessage());
    }
}
```

---

## Forces Balanced By This Pattern

| Force | How the pattern resolves it |
|-------|-----------------------------|
| **Exhaustiveness** | Sealed + switch = compiler-enforced completeness |
| **Type Safety** | Each arm binds the concrete type — no casting needed |
| **Data per State** | Records carry only the fields relevant to their state |
| **Testability** | @WebMvcTest isolates HTTP contract from business logic |
| **Thin Controller** | Business logic stays in the service; controller only dispatches |

---

## Resulting Context

After applying PATTERN-MATCHED CONTROLLER:

- Adding a new status variant requires updating every switch — the compiler tells you where
- Each HTTP endpoint maps to exactly one service method
- The status endpoint self-documents the complete set of possible states
- Tests verify HTTP contracts independently of business rules

---

## Anti-Patterns Avoided

| Anti-Pattern | How This Pattern Prevents It |
|---|---|
| **Forgotten Case** | Sealed + exhaustive switch = compile error if a variant is missing |
| **Nullable Fields** | Records carry only their own data — no `Optional<Instant> shippedAt` on every Order |
| **instanceof Chains** | Single switch expression replaces chains of `if (x instanceof Y y)` |
| **Fat Controller** | Service owns business logic; controller owns HTTP mapping only |
| **Stringly Typed Status** | `String status = "SHIPPED"` replaced by `record ShippedOrder(String trackingId)` |

---

## Comparison: Before and After

### Before — Enum + Nullable Fields

```java
public enum OrderStatus { PENDING, SHIPPED, DELIVERED, CANCELLED }

@Entity
public class Order {
    private OrderStatus status;
    // Everything nullable — only some fields are valid per status
    private Instant placedAt;
    private String trackingId;
    private Instant deliveredAt;
    private String cancelReason;
}

// Controller: can't tell which fields are set for a given status
String label = switch (order.getStatus()) {
    case PENDING   -> "Pending since " + order.getPlacedAt();   // NPE if not set
    case SHIPPED   -> "Tracking: " + order.getTrackingId();     // NPE if not set
    default        -> "Unknown";  // silently hides DELIVERED and CANCELLED
};
```

### After — Sealed Records + Type Pattern

```java
sealed interface OrderStatus permits PendingOrder, ShippedOrder, DeliveredOrder, CancelledOrder {}
record PendingOrder(Instant placedAt)      implements OrderStatus {}
record ShippedOrder(String trackingId)     implements OrderStatus {}
record DeliveredOrder(Instant deliveredAt) implements OrderStatus {}
record CancelledOrder(String cancelReason) implements OrderStatus {}

// Controller: each arm has exactly the fields it needs, no nulls
String label = switch (order.getStatus()) {
    case PendingOrder  p -> "Pending since "     + p.placedAt();
    case ShippedOrder  s -> "Tracking: "         + s.trackingId();
    case DeliveredOrder d -> "Delivered: "       + d.deliveredAt();
    case CancelledOrder c -> "Cancelled because " + c.cancelReason();
    // Compiler error if any variant is missing
};
```

---

## Related Patterns

- **#15 SEALED DOMAIN HIERARCHY** — Defines the sealed interface and record variants
- **#18 JPA ENTITY** — The aggregate root that holds the sealed status
- **#19 TRANSACTIONAL SERVICE** — The service this controller delegates to
- **#21 REST CONTROLLER** — The base controller pattern (without pattern matching)
- **#23 VIRTUAL THREAD DAO** — Pair with Loom virtual threads for high-throughput HTTP

---

## Ontology Extension Points

The ontology in this pack includes a sealed interface and four records. To add a new
variant:

```turtle
my:RefundedOrder a java26:SealedRecord ;
    java26:name "RefundedOrder" ;
    java26:javaPackage "com.example.orders" ;
    java26:implements my:OrderStatus ;
    java26:field my:refundAmount .

my:refundAmount a java26:Field ;
    java26:fieldName "refundAmount" ;
    java26:fieldType "BigDecimal" .
```

Re-run `ggen sync` to regenerate. All switch expressions that don't handle `RefundedOrder`
will fail to compile — your IDE will point you to every location that needs updating.

---

*Generated by ggen marketplace — Java 26 Pattern Language*
*Pattern #22: PATTERN-MATCHED CONTROLLER*
