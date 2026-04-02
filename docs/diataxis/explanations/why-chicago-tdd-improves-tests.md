<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Explanation: Why Chicago-TDD Improves Tests](#explanation-why-chicago-tdd-improves-tests)
  - [Two Schools of TDD](#two-schools-of-tdd)
    - [London School (Outside-In with Mocks)](#london-school-outside-in-with-mocks)
    - [Chicago School (Inside-Out with Real Objects)](#chicago-school-inside-out-with-real-objects)
  - [Why Chicago-TDD is Better for Real Systems](#why-chicago-tdd-is-better-for-real-systems)
    - [Problem 1: Mock Lies](#problem-1-mock-lies)
    - [Problem 2: Over-Testing Internals](#problem-2-over-testing-internals)
    - [Problem 3: False Confidence](#problem-3-false-confidence)
  - [Why Chicago-TDD Works](#why-chicago-tdd-works)
    - [Principle 1: Real Behavior](#principle-1-real-behavior)
    - [Principle 2: Observable Behavior](#principle-2-observable-behavior)
    - [Principle 3: Early Detection of Issues](#principle-3-early-detection-of-issues)
  - [When to Use Real Collaborators](#when-to-use-real-collaborators)
    - [Use Real When:](#use-real-when)
    - [Use Mocks When:](#use-mocks-when)
  - [Real-World Application: ggen's Testing Pattern](#real-world-application-ggens-testing-pattern)
    - [Before (London School with Mocks)](#before-london-school-with-mocks)
    - [After (Chicago School with Real Objects)](#after-chicago-school-with-real-objects)
  - [The Comparison](#the-comparison)
  - [How to Structure Chicago-TDD Tests](#how-to-structure-chicago-tdd-tests)
    - [Step 1: Create Realistic Collaborators](#step-1-create-realistic-collaborators)
    - [Step 2: Create the Object Under Test](#step-2-create-the-object-under-test)
    - [Step 3: Exercise the Behavior](#step-3-exercise-the-behavior)
    - [Step 4: Verify Observable State](#step-4-verify-observable-state)
  - [Common Mistakes](#common-mistakes)
    - [Mistake 1: Using Real External APIs](#mistake-1-using-real-external-apis)
    - [Mistake 2: Mocking Everything](#mistake-2-mocking-everything)
    - [Mistake 3: Verifying Calls Instead of State](#mistake-3-verifying-calls-instead-of-state)
  - [Integration Testing Made Easy](#integration-testing-made-easy)
  - [Test Pyramid with Chicago-TDD](#test-pyramid-with-chicago-tdd)
  - [Why Developers Prefer Mocks (And Why It's Wrong)](#why-developers-prefer-mocks-and-why-its-wrong)
  - [Key Insights](#key-insights)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Explanation: Why Chicago-TDD Improves Tests

**Understanding inside-out testing with real collaborators instead of mocks**

---

## Two Schools of TDD

### London School (Outside-In with Mocks)

```rust
#[test]
fn test_user_creation() {
    let mut mock_db = MockDatabase::new();
    mock_db.expect_insert().return_ok(user);

    let service = UserService::new(mock_db);
    service.create_user("alice");

    // Verify the mock was called
    assert!(mock_db.insert_was_called());
}
```

**Philosophy:**
- Test interfaces first
- Mock external dependencies
- Verify calls (behavior)
- Build top-down

### Chicago School (Inside-Out with Real Objects)

```rust
#[test]
fn test_user_creation() {
    let db = RealDatabase::new_in_memory();
    let service = UserService::new(db);

    service.create_user("alice");

    // Verify actual state
    let users = db.get_all();
    assert_eq!(users[0].name, "alice");
}
```

**Philosophy:**
- Test behavior through actual state
- Use real collaborators (in-memory databases, etc.)
- Verify outcomes (not calls)
- Build inside-out

---

## Why Chicago-TDD is Better for Real Systems

### Problem 1: Mock Lies

Mocks are fake implementations. They can lie:

```rust
#[test]
fn test_with_mock() {
    let mut mock = MockDatabase::new();
    mock.expect_insert().return_ok(user);  // ← Mock always succeeds

    let service = UserService::new(mock);
    let result = service.create_user("alice");

    assert!(result.is_ok());  // Test passes!
}
```

**In production:**
```rust
// Real database has unique constraint
db.insert(user)  // ← Fails because name already exists!
```

**Result:** Test passes, production fails. Mock lied about behavior.

### Problem 2: Over-Testing Internals

Mocks encourage testing implementation details:

```rust
#[test]
fn test_with_mock() {
    let mut mock = MockService::new();
    mock.expect_process().return_ok(data);

    let handler = EventHandler::new(mock);
    handler.handle_event(event);

    // Test checks: "Did it call process exactly once?"
    assert!(mock.process_was_called_once());  // ← Implementation detail!
}
```

**Problem:** If you refactor to call process() twice (for legitimate reasons), test breaks even though behavior is correct.

### Problem 3: False Confidence

Mock-based tests can be 100% passing while production is broken:

```
Mock Test 1: PASS ✓
Mock Test 2: PASS ✓
Mock Test 3: PASS ✓
... 97 more pass ✓

Integration Test: FAIL ✗
Production: FAIL ✗
```

**Why?** The mocks never interact the way real objects do.

---

## Why Chicago-TDD Works

### Principle 1: Real Behavior

With real collaborators, you test actual behavior:

```rust
#[test]
fn test_user_creation() {
    let db = InMemoryDatabase::new();
    let service = UserService::new(db);

    service.create_user("alice");

    let users = db.get_all();
    assert_eq!(users.len(), 1);
    assert_eq!(users[0].name, "alice");
}
```

**What you're testing:**
- UserService actually creates a user
- Database actually stores it
- We can actually retrieve it

No lying, no surprises.

### Principle 2: Observable Behavior

Focus on outcomes, not implementation:

```rust
// ❌ Testing implementation (brittle)
assert!(mock.insert_was_called());

// ✅ Testing outcome (robust)
let users = db.get_all();
assert_eq!(users.len(), 1);
```

**Why it's better:** Refactor the implementation, test still passes as long as outcome is correct.

### Principle 3: Early Detection of Issues

Real collaborators catch problems immediately:

```rust
#[test]
fn test_workflow() {
    let db = InMemoryDatabase::new();
    let cache = InMemoryCache::new();
    let service = Service::new(db, cache);

    service.operation();

    // Cache is out of sync with DB?
    assert_eq!(cache.get("key"), db.get("key"));  // ← Catches immediately!
}
```

With mocks, you wouldn't catch this inconsistency.

---

## When to Use Real Collaborators

### Use Real When:

- **In-memory databases** - Fast, safe, realistic
- **Channels and queues** - Actually test messaging
- **File systems** - Temp directories, real files
- **Time** - Real clocks, not mock clocks
- **Collections** - Real Vec, HashMap, etc.

### Use Mocks When:

- **External APIs** - Truly external (no control)
- **Network calls** - Can't be fast or deterministic
- **Hardware** - Can't be simulated
- **Randomness** - When testing rare cases

**The rule:** Mock only things you don't control.

---

## Real-World Application: ggen's Testing Pattern

### Before (London School with Mocks)

```rust
#[test]
fn test_rdf_parsing() {
    let mut mock_parser = MockRDFParser::new();
    mock_parser.expect_parse().return_ok(mock_result);

    let processor = Processor::new(mock_parser);
    processor.process();

    // Test passes, but...
    assert!(mock_parser.parse_was_called());
}

// Production fails: Real RDF is malformed
// Mock never caught it!
```

### After (Chicago School with Real Objects)

```rust
#[test]
fn test_rdf_parsing() {
    let ontology = "
        @prefix api: <https://...> .
        blog:User a api:Entity .
    ";

    let processor = Processor::new_with_rdf(ontology);
    let result = processor.process();

    // Tests actual behavior
    assert_eq!(result.entities.len(), 1);
    assert_eq!(result.entities[0].name, "User");
}
```

**Advantages:**
✅ Test passes when production passes
✅ Test catches malformed RDF
✅ No mock brittleness
✅ Real verification

---

## The Comparison

| Aspect | London (Mocks) | Chicago (Real) |
|--------|---|---|
| Implementation | Mock dependencies | Real in-memory objects |
| What tested | Calls to dependencies | Observable outcomes |
| Brittleness | High (call verification) | Low (state verification) |
| Production readiness | Uncertain | High confidence |
| Setup complexity | Moderate | Simple (usually) |
| Test speed | Very fast | Fast |
| Coupling | Tight to implementation | Loose to implementation |
| Refactoring | Tests break easily | Tests stable |

---

## How to Structure Chicago-TDD Tests

### Step 1: Create Realistic Collaborators

```rust
let db = InMemoryDatabase::new();  // Real, but fast
let cache = InMemoryCache::new();   // Real, but in-memory
let logger = TestLogger::new();      // Test double, captures logs
```

### Step 2: Create the Object Under Test

```rust
let service = UserService::new(db, cache, logger);
```

### Step 3: Exercise the Behavior

```rust
service.create_user("alice");
service.get_user("alice");
```

### Step 4: Verify Observable State

```rust
// NOT: "Did it call get()?"
// BUT: "Is the user actually in the database?"
let user = db.get("alice");
assert_eq!(user.name, "alice");

// NOT: "Did it call logger.log()?"
// BUT: "Is the action logged?"
assert!(logger.contains("created user"));
```

---

## Common Mistakes

### Mistake 1: Using Real External APIs

```rust
// ❌ Bad: Real network call
#[test]
fn test_user_creation() {
    let client = HttpClient::new("https://external-api.com");
    let result = client.create_user("alice");
    assert!(result.is_ok());
}
```

**Problem:** Test depends on external service, slow, flaky

**Solution:** Mock the external API
```rust
// ✅ Good: Mock external, real internal
let mock_api = MockHttpClient::new();
mock_api.expect_request().return_ok(response);

let db = RealDatabase::new_in_memory();
let service = ApiAdapter::new(mock_api, db);
```

### Mistake 2: Mocking Everything

```rust
// ❌ Bad: Everything mocked
let mut mock_db = MockDb::new();
let mut mock_cache = MockCache::new();
let mut mock_logger = MockLogger::new();

let service = Service::new(mock_db, mock_cache, mock_logger);
// ← Service isn't tested, mocks are!
```

**Problem:** You're not testing the service, you're testing the mocks

**Solution:** Use real in-memory implementations
```rust
// ✅ Good: Real collaborators
let db = InMemoryDatabase::new();
let cache = InMemoryCache::new();
let logger = TestLogger::new();

let service = Service::new(db, cache, logger);
// ← Service is fully tested
```

### Mistake 3: Verifying Calls Instead of State

```rust
// ❌ Bad: Verify implementation detail
assert!(mock.insert_was_called_with("alice"));

// ✅ Good: Verify observable outcome
let user = db.get("alice");
assert_eq!(user.name, "alice");
```

---

## Integration Testing Made Easy

With real collaborators, integration testing becomes natural:

```rust
#[test]
fn test_complete_workflow() {
    // All real, all fast
    let db = InMemoryDatabase::new();
    let cache = InMemoryCache::new();
    let queue = InMemoryQueue::new();

    let service = CompleteService::new(db, cache, queue);

    // Exercise complete workflow
    service.create_user("alice");
    service.assign_to_team("team-1");
    service.process_queued_events();

    // Verify entire state
    let user = db.get("alice");
    assert_eq!(user.team, "team-1");
    assert_eq!(queue.len(), 0);  // All events processed
}
```

No mocks, but fully integrated and fast.

---

## Test Pyramid with Chicago-TDD

```
        UI Tests (slow)
       /            \
      /              \
    /  Integration   \    ← Real collaborators
   /    Tests         \
  /____________________\
 /                      \
/   Unit Tests (fast)    \  ← Real objects, no mocks
\____________________/
```

**All layers use real objects.** Only mock external boundaries.

---

## Why Developers Prefer Mocks (And Why It's Wrong)

Developers often choose mocks because:

1. **They're easier to set up** - Write a mock class, done!
2. **They're familiar** - "Everyone does it"
3. **Tests run faster** - Slightly faster initially
4. **They feel in control** - "I control the return value"

**But:**
1. **Mocks hide problems** - Real objects would catch them
2. **Tests become brittle** - Refactoring breaks tests
3. **False confidence** - 100% passing, production broken
4. **More code to maintain** - Mock classes, mock setup, etc.

---

## Key Insights

1. **Mocks are for boundaries.** Use them for external APIs, not internal collaborators.
2. **Real objects catch problems.** Mocks hide them.
3. **Test outcomes, not implementation.** Verify state, not calls.
4. **Integration testing should be fast.** Use in-memory implementations.
5. **Brittleness signals wrong approach.** Refactoring shouldn't break tests.

---

## Next Steps

1. **Learn TDD:** [Chicago-TDD Pattern](../../skills/chicago-tdd-pattern.md)
2. **Apply it:** [Refactor Tests with Lean](../how-to/refactor-tests-with-lean.md)
3. **Reference:** [Chicago TDD Tools](https://github.com/ggen/chicago-tdd-tools)
