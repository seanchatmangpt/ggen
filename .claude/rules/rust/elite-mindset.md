---
auto_load: false
category: rust
priority: high
version: 6.0.1
---

# Elite Rust Mindset

## The Discipline

Before you write any function, answer: What can I express in types?

The compiler is your design partner. Types encode invariants. PhantomData enforces state machines. Const generics eliminate runtime branching. Every time you reach for a runtime check, ask whether the type system can make that check impossible to violate.

NARRATION is the failure mode here. Describing what your code should do without proving the type system enforces it is empty ceremony. If the compiler accepts invalid states, your types are lying.

## Soundness Contract

Every `unwrap()`, `expect()`, `.clone()` must justify itself -- same way every `unsafe` block must justify with a safety comment.

`unwrap()` claims infallibility. Prove it. If a value can be `None` or `Err` at runtime, handle it. `clone()` claims cheap duplication. Measure it. If you are cloning to satisfy a borrow checker argument, restructure the ownership, do not copy your way out of the problem.

The failure mode is SELF-CERT. You write `unwrap()` because you believe the value is present. Belief is not proof. Make the type system carry the proof.

## Zero-Cost Discipline

You will feel tempted to add `Box<dyn Trait>`. This is a runtime cost.

Generics, macros, and const generics are zero-cost abstractions. They compile to specialized code with no indirection. Trait objects introduce vtable dispatch, heap allocation, and destroyed monomorphization. The moment you erase the concrete type, you pay for that erasure at every call site.

Ask: Is this abstraction zero-cost? If not, justify the cost with a measurement, not a feeling.

## Ownership is Documentation

Ownership semantics tell the reader who owns data, who can mutate it, who can share it, and when it is destroyed. Lifetimes prevent use-after-free. `Rc` and `Arc` declare shared ownership explicitly. `&mut` guarantees exclusive access.

When you fight the borrow checker, the borrow checker is telling you something about your design. Listen to it. Restructure the data flow. Do not reflexively reach for `clone()` or `Rc` to silence the error.

## API Design

Make misuse impossible.

A well-designed API does not require documentation to explain what combinations of calls are valid. The types forbid invalid combinations at compile time. `Result<T, E>` communicates fallibility. Builder patterns enforce construction order. Newtype wrappers prevent invalid values from being constructed.

If a user of your API can cause a panic through normal use, your API has a soundness bug. Panics are for programmer errors, not user errors.

## 80/20 Innovation Test

Generate three ideas. First, solve the immediate problem. Second, solve 80% of related problems with 20% of the effort. Third, pursue maximum theoretical value.

The second idea is the target. The first is too narrow. The third is a trap -- it consumes disproportionate effort for diminishing returns. Train yourself to find the 80/20 intersection where a single design decision eliminates an entire class of problems.

## DfLSS

Prevent defects and waste from the start, not fix later.

Design for Lean Six Sigma means encoding quality into the process. If you find yourself writing defensive code to handle states that should not exist, eliminate the states at the type level. If you are writing tests to catch bugs that the compiler could prevent, move the invariant into the type system.

Every defect you prevent at design time saves ten defects you would chase at integration time.
