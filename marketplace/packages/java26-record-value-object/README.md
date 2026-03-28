# RECORD VALUE OBJECT — Java 26 Pattern Language #7

> "A Java record is a value object by construction — the compiler writes the
> boilerplate so you can focus on the invariants."

---

## Context

You are modelling a domain where certain concepts are defined entirely by their
attributes rather than by a persistent identity.  Monetary amounts, e-mail
addresses, GPS coordinates, date ranges, and colour values are classic examples.
Historically these were plain Java classes (`Money.java`, `EmailAddress.java`)
carrying mutable fields, a hand-written constructor, and fragile `equals` /
`hashCode` overrides.

---

## Problem

**Mutable objects pretending to be value objects introduce hidden coupling and
correctness bugs.**

- A `Money` object with a public `setAmount` can be mutated after it has been
  passed to another component, making shared references a liability.
- Missing null checks in constructors allow invalid state to propagate silently
  until a `NullPointerException` surfaces far from the creation site.
- Forgetting to override `equals` means two `Money(9.99, "USD")` instances
  compare as unequal, breaking `Set` membership and `Map` lookup.
- Writing correct `equals`, `hashCode`, and `toString` for every value class is
  repetitive, error-prone, and adds maintenance burden with zero domain insight.

---

## Solution

**Declare value objects as Java records with a compact canonical constructor.**

Java records (JEP 395, promoted to permanent in Java 16, enhanced in Java 26
with derived record creation and `with`-expression previews) provide:

1. **Automatic structural equality** — the compiler generates `equals`,
   `hashCode`, and `toString` that operate on all record components.
2. **Immutability by default** — all components are implicitly `final`.
3. **A single invariant-enforcement point** — the compact canonical constructor
   runs before the compiler assigns fields, making it impossible to bypass
   validation.

Add a static `of()` factory method as the public entry point.  This gives you a
stable API to evolve (e.g., to add caching or interning) without changing call
sites.

---

## Use This Pattern

**Step 1 — Declare the value object in your ontology.**

```turtle
my:Money a java26:ValueObject ;
    java26:name "Money" ;
    java26:javaPackage "com.example.domain" ;
    java26:field my:amount, my:currency .

my:amount   a java26:Field ; java26:fieldName "amount" ;
                              java26:fieldType "BigDecimal" ;
                              java26:nullable false .
my:currency a java26:Field ; java26:fieldName "currency" ;
                              java26:fieldType "String" ;
                              java26:nullable false .
```

**Step 2 — Run the generator.**

```bash
ggen sync
```

ggen queries `ontology/domain.ttl` via `queries/extract-value-objects.rq`,
renders `templates/ValueObject.java.tera`, and emits
`src/main/java/com/example/domain/Money.java`.  A companion test file is
generated alongside it.

**Step 3 — Add domain validation in the compact canonical constructor.**

The generated stub contains `Objects.requireNonNull` guards for all
`java26:nullable false` fields.  Add richer domain rules here — they run before
the record is observable to any caller:

```java
public Money {
    Objects.requireNonNull(amount,   "Money.amount must not be null");
    Objects.requireNonNull(currency, "Money.currency must not be null");
    if (amount.signum() < 0) {
        throw new IllegalArgumentException(
                "Money.amount must be non-negative, was: " + amount);
    }
    if (currency.length() != 3) {
        throw new IllegalArgumentException(
                "Money.currency must be an ISO 4217 code, was: " + currency);
    }
}
```

**Step 4 — Use the factory method at call sites.**

```java
Money price    = Money.of(new BigDecimal("19.99"), "USD");
Money discount = Money.of(new BigDecimal("2.00"),  "USD");

// Records compare by value — no .equals() boilerplate required.
assert !price.equals(discount);

// Pattern matching works directly on the record.
if (price instanceof Money(var amt, var cur) && amt.compareTo(BigDecimal.TEN) > 0) {
    System.out.println("High-value item in " + cur);
}
```

---

## Consequences

**Benefits**

- Immutable by construction — sharing references across threads is safe with
  no synchronisation.
- Structural equality is correct and free — `Set<Money>` and `Map<Money, ?>`
  work without any extra code.
- Compact canonical constructor is the one true gate for invariants — impossible
  to construct an invalid value object.
- `toString` is human-readable out of the box, simplifying debugging and logging.
- Pattern matching with record patterns (JEP 440) deconstructs value objects
  cleanly in `switch` expressions.

**Liabilities**

- Records cannot extend other classes — use interfaces (`Comparable<Money>`,
  custom domain interfaces) to share behaviour.
- Record components are fixed at declaration; adding a new field is a
  source-incompatible change to the canonical constructor.  Use the ontology
  and regeneration to manage this evolution deliberately.
- Deep immutability requires that component types are themselves immutable.
  A `record Coordinate(int[] axes)` is not deeply immutable; prefer
  `List.of(...)` or value-typed arrays when available.

---

## Related Patterns

| Pattern | Number | Relationship |
|---------|--------|--------------|
| ENTITY TYPE HIERARCHY | #6 | Complementary: entities carry identity; value objects carry meaning. Use records for values, classes for entities. |
| DTO PROJECTION | #16 | Value objects model domain concepts; DTOs model API contracts. Both can be records, but their validation rules differ. |
| AGGREGATE ROOT | #8 | An aggregate root owns and composes value objects. Value objects are the leaves of the aggregate tree. |

---

*Generated by ggen v6.0.0 — edit `ontology/domain.ttl` to regenerate.*
