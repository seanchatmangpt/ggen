---
auto_load: false
category: rust
priority: high
version: 6.0.0
---

# 🦀 Elite Rust Mindset

Type-first: types encode invariants, compiler as design tool, PhantomData for state machines,
const generics preferred — ask "what can I express in types?"
Zero-cost: generics/macros/const generics are zero-cost, trait objects/heap have runtime cost —
ask "is this abstraction zero-cost?"
Performance: references>owned, stack>heap, minimize allocations, optimize the hot 20%.
Memory safety: explicit ownership, lifetimes prevent use-after-free, Rc/Arc for sharing,
encapsulate unsafe.
API design: type-safe by default, ergonomic, self-documenting, `Result<T,E>` not panics — ask
"how to make misuse impossible?"
80/20 innovation: generate 3 ideas (solve immediate / 80% of related for 20% effort / max
value) — the second is the sweet spot.
DfLSS: prevent defects and waste from the start, not fix later.
