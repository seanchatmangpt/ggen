# 9. SEALED DOMAIN HIERARCHY **

*Name every state your domain can be in; then let the compiler enforce the naming.*

---

## Context

Every meaningful domain has states. An order is pending, then processing, then shipped,
then delivered — or cancelled at any point. The naive representation of this lifecycle is
an enum: `OrderStatus { PENDING, PROCESSING, SHIPPED, DELIVERED, CANCELLED }`. Enums work.
They are familiar, serializable, and easy to add to a JPA column. But they have a hidden cost:
every state transition that adds a field — tracking ID when shipped, delivery timestamp when
delivered, cancellation reason when cancelled — must bolt that field somewhere else. The
aggregate root grows nullable fields. `Optional<String> trackingId`, `Optional<Instant>
deliveredAt`. The meaning of each state is diffused across nullable geography.

The problem deepens when you want to process states. A method that handles all five states
usually starts with a long if-else chain or a switch with a default clause. The default clause
is a lie: it says "I handle the unexpected" but what it really means is "I silently ignore
new states I haven't heard about yet." When a sixth state is added — say `BackOrdered` —
nothing tells the programmer that seven switch statements across the codebase need a new arm.
The sixth state ships, the default swallows it, and the bug surfaces in production.

Java 26 gives us a better tool: sealed interfaces with record implementations. A sealed interface
names all permitted subtypes at declaration time. The compiler then enforces exhaustiveness
in every switch expression that matches over the type. Add a new state, run `ggen sync`, and
the compiler finds every switch that needs a new case — before the code ships, not after.
Each state variant is a record: an immutable, structurally equal value type that carries only
the fields relevant to that state. No nulls. No defensive coding. No silent default swallowing.

---

❖ ❖ ❖

## Problem

**The domain has N named states, each with different data, but the codebase represents them
as a flat enum — leaving state-specific data scattered across nullable fields and leaving
new-state exhaustiveness unverified by the compiler.**

---

## Solution

*Therefore:* Define a `java26:SealedInterface` in your domain TTL to name the type contract,
and one `java26:SealedRecord` per state variant with only the fields that state needs.
Run `ggen sync` to precipitate a Java `sealed interface ... permits` declaration and one
immutable `record` per variant. Every `switch` expression over the sealed type is compiler-
verified exhaustive. Adding a variant means editing the TTL and running sync; the compiler
then surfaces every switch that is incomplete.

---

❖ ❖ ❖

## Use This Pattern

### Step 1 — Define your domain in TTL

Open `ontology/domain.ttl` (or copy `examples/orders/domain.ttl`) and describe your hierarchy:

```turtle
@prefix java26: <http://ggen.io/java26#> .
@prefix my:     <http://example.com/my-domain#> .

# The sealed interface — the type contract
my:OrderStatus a java26:SealedInterface ;
    java26:name "OrderStatus" ;
    java26:javaPackage "com.example.orders" .

# Each variant — one record per named state
my:PendingOrder a java26:SealedRecord ;
    java26:name "PendingOrder" ;
    java26:javaPackage "com.example.orders" ;
    java26:implements my:OrderStatus ;
    java26:field my:pendingAt .

my:ShippedOrder a java26:SealedRecord ;
    java26:name "ShippedOrder" ;
    java26:javaPackage "com.example.orders" ;
    java26:implements my:OrderStatus ;
    java26:field my:trackingId .

my:DeliveredOrder a java26:SealedRecord ;
    java26:name "DeliveredOrder" ;
    java26:javaPackage "com.example.orders" ;
    java26:implements my:OrderStatus ;
    java26:field my:deliveredAt .

# Field definitions
my:pendingAt  a java26:Field ; java26:fieldName "placedAt"  ; java26:fieldType "Instant" .
my:trackingId a java26:Field ; java26:fieldName "trackingId"; java26:fieldType "String"  .
my:deliveredAt a java26:Field ; java26:fieldName "deliveredAt"; java26:fieldType "Instant" .
```

### Step 2 — Configure ggen.toml

The `[[generation.rules]]` block that activates this pattern:

```toml
[[generation.rules]]
name = "sealed-interfaces"
query    = { file = "queries/extract-sealed-interfaces.rq" }
template = { file = "templates/SealedInterface.java.tera" }
output_file = "{{ javaPackage | replace(from='.', to='/') }}/{{ interfaceName }}.java"
mode = "Overwrite"

[[generation.rules]]
name = "sealed-records"
query    = { file = "queries/extract-sealed-records.rq" }
template = { file = "templates/SealedRecord.java.tera" }
output_file = "{{ javaPackage | replace(from='.', to='/') }}/{{ recordName }}.java"
mode = "Overwrite"
```

### Step 3 — Validate

```bash
ggen validate --shapes ontology/shapes.ttl ontology/domain.ttl
```

Expected output: `SHACL validation passed — 0 violations, 0 warnings.`

### Step 4 — Generate

```bash
ggen sync
```

**Expected output** (for the Order Management example):

```
src/main/java/com/example/orders/
├── OrderStatus.java          ← sealed interface ... permits PendingOrder, ProcessingOrder, ...
├── PendingOrder.java         ← public record PendingOrder(Instant placedAt) implements OrderStatus
├── ProcessingOrder.java      ← public record ProcessingOrder(String paymentRef) implements OrderStatus
├── ShippedOrder.java         ← public record ShippedOrder(String trackingId) implements OrderStatus
├── DeliveredOrder.java       ← public record DeliveredOrder(Instant deliveredAt) implements OrderStatus
├── CancelledOrder.java       ← public record CancelledOrder(String cancelReason) implements OrderStatus
├── Order.java                ← @Entity aggregate root
└── OrderStatusPatternTest.java ← JUnit 5 exhaustiveness tests
```

The generated `OrderStatus.java` looks like this:

```java
public sealed interface OrderStatus
        permits PendingOrder, ProcessingOrder, ShippedOrder,
                DeliveredOrder, CancelledOrder {

    default String label() {
        return getClass().getSimpleName();
    }

    default boolean isTerminal() {
        return false;
    }
}
```

And `ShippedOrder.java`:

```java
public record ShippedOrder(String trackingId) implements OrderStatus {
    public ShippedOrder {
        java.util.Objects.requireNonNull(trackingId, "trackingId must not be null");
    }
}
```

Pattern-matched dispatch in your service layer:

```java
String describe(OrderStatus status) {
    return switch (status) {
        case PendingOrder p    -> "Placed at " + p.placedAt();
        case ProcessingOrder p -> "Payment ref: " + p.paymentRef();
        case ShippedOrder s    -> "Tracking: " + s.trackingId();
        case DeliveredOrder d  -> "Delivered: " + d.deliveredAt();
        case CancelledOrder c  -> "Cancelled: " + c.cancelReason();
        // No default — compiler enforces exhaustiveness
    };
}
```

---

## Consequences

**Benefits:**

- **Compiler-enforced exhaustiveness.** Every switch over the sealed type is verified at
  compile time. The compiler becomes the regression test for state coverage.
- **No nulls on state-specific fields.** Each variant carries only its own fields.
  `ShippedOrder` has a `trackingId`; `PendingOrder` does not. No `Optional`, no `null`
  guard, no `if (status == SHIPPED && trackingId != null)`.
- **Single source of truth.** The TTL file names the states. `ggen sync` keeps the Java
  in sync. Adding a state means one edit to the ontology; the compiler reports all sites
  that need updating.
- **Value semantics.** Records provide structural equality, `hashCode`, and `toString`
  for free. Two `ShippedOrder("1Z999")` instances are equal. No boilerplate.
- **Documentation in types.** The `permits` clause is machine-readable documentation
  of every valid state. No need to scan the codebase for all enum values or all switch arms.

**Tradeoffs:**

- **JPA serialization requires a converter.** Sealed interfaces are not directly mappable
  to a single JPA column. Use a `@Converter` or store the discriminator as a `String` enum
  and reconstruct the sealed record in the service layer. See Pattern #13 (JPA ENTITY MAPPING).
- **JSON serialization needs a mixin.** Jackson needs `@JsonTypeInfo` + `@JsonSubTypes` to
  deserialize a sealed interface. See Pattern #15 (REST API RESPONSE MAPPING).
- **Not suitable for open extension.** The `sealed` keyword closes the hierarchy. If external
  libraries must add variants, use an interface without `sealed`. This is a feature for closed
  internal domain models.

---

## Related Patterns

- **AGGREGATE ROOT (#8)** — provides the `@Entity` that holds the sealed status field.
  The `Order` entity holds an `OrderStatus`; the converter maps it to/from the database.

- **SEALED EVENT HIERARCHY (#10)** — the same sealed-records technique applied to domain
  events (`OrderPlaced`, `OrderShipped`, `OrderCancelled`). Events are append-only;
  status is current state. Use both together for full event-sourcing readiness.

- **PATTERN MATCHING VISITOR (#11)** — how to consume sealed hierarchies with exhaustive
  switch expressions in service methods. Provides a reusable `Visitor<R>` interface
  generated alongside the sealed hierarchy.

- **JPA ENTITY MAPPING (#13)** — how the sealed type integrates with JPA. Covers
  `AttributeConverter<OrderStatus, String>` and the bidirectional mapping between
  the sealed variant and the persistence column.

- **REST API RESPONSE MAPPING (#15)** — how to serialize a sealed hierarchy to JSON
  with Jackson `@JsonTypeInfo` and how to write a `@RestController` that pattern-matches
  over the sealed type in its request handlers.

---

*"Fifteen properties cannot be added to a building one at a time — the life of the whole
depends on them all being present together."* — Christopher Alexander, *The Nature of Order*

*Fifteen states cannot be added to a domain one at a time — the correctness of the whole
depends on the compiler verifying them all together.*
