# Variable Scoping Best Practices

## Purpose

This document defines variable scoping best practices to prevent compilation errors related to variable scope issues.

## Common Scoping Patterns

### Pattern 1: Mutable References in Loops

```rust
// ✅ CORRECT: Get mutable reference inside loop
let mut map = HashMap::new();
for key in keys {
    if let Some(value) = map.get_mut(&key) {
        *value += 1;
    }
}

// ❌ WRONG: Trying to use value outside scope
let mut map = HashMap::new();
let value = map.get_mut(&key); // value goes out of scope
*value += 1; // Error: value not in scope
```

### Pattern 2: If-Let Binding Scope

```rust
// ✅ CORRECT: Use variable within if-let block
if let Some(value) = map.get_mut(&key) {
    *value += 1; // value is in scope here
    if *value == 0 {
        // value still in scope
    }
}

// ❌ WRONG: Using variable outside if-let block
if let Some(value) = map.get_mut(&key) {
    *value -= 1;
}
if *value == 0 { // Error: value not in scope
    // ...
}
```

### Pattern 3: Iterator Chain Scope

```rust
// ✅ CORRECT: Complete operation in iterator chain
let result: Vec<_> = items
    .iter()
    .filter(|item| item.is_valid())
    .map(|item| item.process())
    .collect();

// ❌ WRONG: Trying to use iterator variable outside chain
let filtered = items.iter().filter(|item| item.is_valid());
let item = filtered.next(); // item may not be in scope
item.process(); // Error: item not in scope
```

## Scoping Rules

1. **Mutable references**: Must be used within the scope where they're obtained
2. **If-let bindings**: Variables are only in scope within the if-let block
3. **Match bindings**: Variables are only in scope within the match arm
4. **Iterator variables**: Only in scope within the iterator chain

## Prevention Checklist

- [ ] Mutable references used within their scope
- [ ] If-let bindings used within if-let block
- [ ] Match bindings used within match arm
- [ ] Iterator variables used within iterator chain
- [ ] No variables used outside their scope

## Root Cause Prevention

These practices prevent scope-related compilation errors by:
- Documenting correct patterns
- Providing examples of common mistakes
- Enforcing scope rules in code review



