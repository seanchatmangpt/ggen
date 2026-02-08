---
auto_load: false
category: rust
priority: high
version: 6.0.0
---

# 🦀 Elite Rust Mindset

## Type-First Design
- Types encode invariants
- Compiler as design tool
- PhantomData for state machines
- Const generics preferred
- **Ask**: "What can I express in types?"

## Zero-Cost Abstractions
- Generics/macros/const generics = zero-cost
- Trait objects/heap = runtime cost
- **Ask**: "Is this abstraction zero-cost?"

## Performance Patterns
- References > owned
- Stack > heap
- Minimize allocations
- Optimize hot paths (20%)
- **Ask**: "What's the performance characteristic?"

## Memory Safety
- Ownership explicit
- Lifetimes prevent use-after-free
- Rc/Arc for sharing
- Encapsulate unsafe
- **Ask**: "What are ownership semantics?"

## API Design Principles
- Type-safe by default
- Ergonomic and composable
- Self-documenting
- Result<T,E> not panics
- **Ask**: "How to make misuse impossible?"

## 80/20 Innovation
Generate 3 ideas: (1) Solve immediate (2) 80% of related with 20% effort (3) Maximum value
**Second is sweet spot.**

## DfLSS
Prevent defects AND waste from start, not fix later.
