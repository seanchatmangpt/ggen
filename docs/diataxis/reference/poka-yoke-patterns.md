# Reference: Poka-Yoke Patterns

**Five compile-time mistake-proofing patterns that eliminate runtime failures**

---

## What is Poka-Yoke?

**Poka-Yoke** (ポカヨケ): "Mistake-proofing" - Preventing errors through design, not vigilance.

Traditional approach:
```
Hope people don't make mistakes
  ↓ Mistakes happen
  ↓ Hope they catch them
  ↓ Hope they fix them
```

Poka-Yoke approach:
```
Make the mistake impossible
  ↓ No mistakes can happen
  ↓ System is inherently correct
```

---

## Five Core Patterns

### Pattern 1: Guide Pin (Impossible to Insert Wrong)

**The Problem:**
```
Equipment with two connectors that look similar.
Someone plugs them in backwards.
System fails.
```

**The Solution:**
```
Redesign connector so it only fits one way.
Now, backward insertion is impossible.
```

**In Software: Type System**

```rust
// ❌ Easy to confuse: int, int
fn calculate(price: i32, quantity: i32) -> i32 {
    price * quantity  // Oops, what if someone passes them in wrong order?
}

// ✅ Guide Pin: Named types
struct Price(i32);
struct Quantity(i32);

fn calculate(price: Price, quantity: Quantity) -> i32 {
    price.0 * quantity.0  // Now impossible to confuse
}

// Type system guides correct usage
calculate(Price(100), Quantity(5));      // ✅ Correct
calculate(5, Price(100));                 // ❌ Type error - impossible!
```

**Real Examples:**
- Newtype wrapper prevents mixing units: `struct Miles(f64)` vs `struct Kilometers(f64)`
- Enum states prevent invalid transitions: `enum State { Ready, Running, Complete }`
- Generic types constrain operations: `HashMap<UserId, User>`

**How to Apply:**
1. Identify easy-to-confuse values
2. Wrap in newtypes or enums
3. Type system now guides correct usage

---

### Pattern 2: Limit Switches (Constrain the Operation)

**The Problem:**
```
Equipment can't overheat, but operator might set temperature too high.
Relies on operator reading manual (they don't).
Equipment overheats, fails.
```

**The Solution:**
```
Physical limit switch stops heating at safe temperature.
Even if operator sets it higher, physically impossible to exceed limit.
```

**In Software: Validation**

```rust
// ❌ No constraint, relies on caller to validate
fn set_temperature(temp: i32) {
    system.temperature = temp;  // What if temp = 1000?
}

// ✅ Limit switch: Validate at boundaries
fn set_temperature(temp: i32) -> Result<(), Error> {
    if temp < 0 || temp > 100 {
        return Err(Error::OutOfRange);
    }
    system.temperature = temp;  // Guaranteed valid
    Ok(())
}

// Even better: Use types
struct Temperature {
    value: i32,  // 0-100 only
}

impl Temperature {
    fn new(value: i32) -> Result<Self, Error> {
        if value < 0 || value > 100 {
            return Err(Error::OutOfRange);
        }
        Ok(Temperature { value })
    }
}

// Now type system prevents invalid values
let temp = Temperature::new(-50)?;  // Guaranteed valid or error
```

**Real Examples:**
- Zod schema validation: `z.string().min(1).max(255)`
- Rust's type system: `NonZeroI32`, `PositiveInt`
- Range types: `0..100` prevents out-of-bounds

**How to Apply:**
1. Identify range constraints
2. Move validation to type/constructor
3. Type system now enforces limits

---

### Pattern 3: Fail-Safe Default (Safe When Something Fails)

**The Problem:**
```
Equipment relies on sensor to detect danger.
Sensor fails (breaks).
Equipment doesn't detect danger.
System fails.
```

**The Solution:**
```
Design so sensor failure means STOP.
If sensor stops reporting, equipment shuts down.
Safe by default.
```

**In Software: Result Types**

```rust
// ❌ Ignores errors, assumes success
let data = read_file("data.json");  // What if read fails?
process(data);  // Might process garbage

// ✅ Fail-safe: Explicit error handling
let data = read_file("data.json")?;  // If fails, return Err
process(data);  // Guaranteed valid

// Even better: Force handling
match read_file("data.json") {
    Ok(data) => process(data),
    Err(e) => {
        log_error(e);
        return Err(e);  // Explicit fail-safe
    }
}

// ❌ Wrong: Ignore error with unwrap
let data = read_file("data.json").unwrap();  // Panics if fails
// ✅ Right: Propagate or handle
let data = read_file("data.json")?;  // Propagate error up
```

**Real Examples:**
- `Result<T, E>` enforces error handling
- Rust's `?` operator propagates errors
- Type system prevents `unwrap()` in production code

**How to Apply:**
1. Identify failure modes
2. Use `Result<T, E>` to make errors explicit
3. Never use `unwrap()` in production code
4. Always handle errors propagate up

---

### Pattern 4: Counter/Measurement (Verify State)

**The Problem:**
```
Assembly line produces parts.
Worker is supposed to attach exactly 3 bolts.
Sometimes attaches 2, sometimes 4.
Product fails later.
```

**The Solution:**
```
Add counter to assembly station.
Worker can't move to next step until counter = 3.
Now production is guaranteed correct.
```

**In Software: Assertions and Invariants**

```rust
// ❌ Assumes state is correct
fn process_users(users: Vec<User>) {
    for user in users {
        register(user);  // What if users is empty?
    }
    // Did all users register? Unknown.
}

// ✅ Counter: Verify state
fn process_users(users: Vec<User>) -> Result<usize, Error> {
    if users.is_empty() {
        return Err(Error::NoUsers);
    }

    let mut count = 0;
    for user in users {
        register(user)?;
        count += 1;
    }

    assert_eq!(count, users.len());  // Verify we processed all
    Ok(count)
}

// In tests: Verify invariants
#[test]
fn test_database_consistency() {
    let db = Database::new();
    db.insert(user);

    // Counter: Verify state
    assert_eq!(db.count(), 1);
    assert_eq!(db.get_all().len(), 1);  // Consistency check
}
```

**Real Examples:**
- Assertions verify invariants: `assert_eq!(cache.len(), db.len())`
- Tests verify state: `assert!(result.is_ok())`
- Logging tracks operations: `info!("Processed {} records", count)`

**How to Apply:**
1. Identify critical state
2. Add assertions to verify invariants
3. Log counts and measurements
4. Add tests that verify state

---

### Pattern 5: Sequencing (Can't Proceed Out of Order)

**The Problem:**
```
Assembly has 5 steps in order.
Worker might skip step 3 (it's tricky).
Product is defective (step 3 was critical).
```

**The Solution:**
```
Physical design makes step 2 → 3 flow automatic.
Step 3 is impossible until step 2 is done.
Sequence enforced by design.
```

**In Software: Type State Pattern**

```rust
// ❌ No sequence enforcement, can call in any order
struct Request {
    data: String,
}

impl Request {
    fn validate(&mut self) { /* ... */ }
    fn process(&mut self) { /* ... */ }
    fn respond(&mut self) { /* ... */ }
}

let mut req = Request { data: "" };
req.respond();      // ❌ Called before validate!
req.process();      // ❌ Called out of order!
req.validate();     // ❌ Too late!

// ✅ Sequencing: Type state enforces order
struct Request {
    data: String,
}

struct Validated;
struct Processed;

impl Request {
    fn validate(self) -> ValidatedRequest {
        // Validation logic
        ValidatedRequest { data: self.data }
    }
}

impl ValidatedRequest {
    fn process(self) -> ProcessedRequest {
        // Processing logic
        ProcessedRequest { data: self.data }
    }
}

impl ProcessedRequest {
    fn respond(self) -> Response {
        // Response logic
        Response { data: self.data }
    }
}

let req = Request { data: "" };
let validated = req.validate();          // ✅ Step 1
let processed = validated.process();     // ✅ Step 2
let response = processed.respond();      // ✅ Step 3

// Can't call out of order:
let req = Request { data: "" };
let response = req.respond();            // ❌ Type error!
```

**Real Examples:**
- Builder pattern enforces setup order
- State machine types enforce transitions
- Pipeline API enforces sequencing: `builder.add().filter().map().collect()`

**How to Apply:**
1. Identify mandatory sequence
2. Create types for each state
3. Make transitions explicit (methods return new type)
4. Type system enforces order

---

## Pattern Comparison

| Pattern | Guards Against | Implementation | Cost |
|---------|---|---|---|
| Guide Pin | Type confusion | Newtype/Enum | Low |
| Limit Switches | Out of range | Validation in constructor | Low |
| Fail-Safe | Ignored errors | Result type | Low |
| Counter | Wrong state | Assertions/Logging | Very Low |
| Sequencing | Wrong order | Type state pattern | Medium |

---

## Real-World Examples

### Example 1: User Creation (Combines Patterns)

```rust
// Guide Pin: Newtype prevents mixing
struct UserId(u64);
struct Email(String);

// Limit Switch: Validation
impl Email {
    fn new(s: String) -> Result<Self, Error> {
        if !s.contains('@') {
            return Err(Error::InvalidEmail);
        }
        Ok(Email(s))
    }
}

// Sequencing: Type state
struct UnvalidatedUser {
    email: String,
}

struct ValidatedUser {
    email: Email,
}

impl UnvalidatedUser {
    fn validate(self) -> Result<ValidatedUser, Error> {
        let email = Email::new(self.email)?;  // Limit Switch
        Ok(ValidatedUser { email })  // Sequencing step 1
    }
}

impl ValidatedUser {
    fn create(self) -> Result<UserId, Error> {
        // Fail-Safe: Errors propagate
        let id = db.insert(&self.email)?;
        Ok(id)  // Sequencing step 2
    }
}

// Usage: Type system guides correct sequence
let user = UnvalidatedUser {
    email: "alice@example.com".to_string(),
};

let validated = user.validate()?;  // ✅ Validates email
let id = validated.create()?;      // ✅ Creates user

// Can't call out of order:
let user = UnvalidatedUser { /* ... */ };
let id = user.create()?;           // ❌ Type error - must validate first!
```

### Example 2: Database Transaction (Sequencing + Fail-Safe)

```rust
// ✅ Sequencing: Each state is a different type
struct Transaction {
    id: u64,
}

struct OpenTransaction;
struct CommitTransaction;
struct AbortTransaction;

impl Transaction {
    fn begin() -> Transaction {
        Transaction { id: db.begin_transaction() }
    }
}

impl Transaction {
    fn execute_query(mut self, query: String) -> Result<Transaction, Error> {
        db.execute(query)?;  // Fail-Safe: Errors propagate
        Ok(self)
    }

    fn commit(self) -> Result<(), Error> {
        db.commit(self.id)?;  // Fail-Safe
        Ok(())
    }

    fn rollback(self) -> Result<(), Error> {
        db.rollback(self.id)?;  // Fail-Safe
        Ok(())
    }
}

// Type system enforces correct sequence
let tx = Transaction::begin();
let tx = tx.execute_query("UPDATE ...".to_string())?;
tx.commit()?;

// Can't forget to commit/rollback:
let tx = Transaction::begin();
let tx = tx.execute_query("UPDATE ...".to_string())?;
// Drop without commit - type system catches this in production!
```

---

## How to Choose Patterns

### For Type Confusion → Guide Pin
```
Use newtype or enum to make types distinct
```

### For Out-of-Range Values → Limit Switches
```
Validate in constructor, store only valid values
```

### For Ignored Errors → Fail-Safe
```
Use Result<T, E>, never unwrap()
```

### For Wrong State → Counter
```
Add assertions and logging
```

### For Wrong Sequence → Sequencing
```
Use type state pattern
```

---

## Combining Patterns

Real systems combine multiple patterns:

```rust
// Sequencing + Fail-Safe + Limit Switches + Counter
struct ValidatedRequest {
    user_id: UserId,          // Guide Pin: Newtype
    email: Email,             // Limit Switch: Constructor validation
}

impl ValidatedRequest {
    fn new(user_id: u64, email: String) -> Result<Self, Error> {
        let user_id = UserId(user_id);           // Guide Pin
        let email = Email::new(email)?;          // Limit Switch + Fail-Safe
        Ok(ValidatedRequest { user_id, email }) // Sequencing step 1
    }

    fn process(self) -> Result<Response, Error> {
        // Fail-Safe: All operations propagate errors
        let user = db.get_user(self.user_id)?;
        let _count = db.update_email(&user, &self.email)?;  // Counter
        Ok(Response { /* ... */ })  // Sequencing step 2
    }
}
```

---

## Key Takeaways

1. **Prevention > Detection** - Make errors impossible, not just visible
2. **Type system is your friend** - Use types to encode constraints
3. **Fail-safe means errors propagate** - Never silently ignore errors
4. **Sequencing prevents order bugs** - Type state enforces order
5. **Combine patterns** - Real systems use multiple patterns together

---

## References

- [Poka-Yoke on Wikipedia](https://en.wikipedia.org/wiki/Poka-yoke)
- [Type-Driven Development](https://www.idris-lang.org/)
- [Rust Book: Error Handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html)
