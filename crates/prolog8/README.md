# Prolog8

Prolog8 is a Rust/WASM proof engine for bounded rules, graph/bit execution, action admission, and replayable decisions.

## DX & QoL Features

Prolog8 includes a set of macros to make working with `Atom8`, `Rule8`, and `QueryAtom8` ergonomic and boilerplate-free.

### Constructing Atoms
Use the `atom!` macro to quickly construct bounded atoms:

```rust
use prolog8::atom;

// pred_id: 1, args: [10, 20]
let a = atom!(1, 10, 20);
```

### Constructing Rules
Use the `rule!` macro to assemble rules without initializing 8-length arrays manually:

```rust
use prolog8::{atom, rule};

let head = atom!(1, 10, 20);
let b1 = atom!(2, 10);
let b2 = atom!(3, 20);

// rule_id: 100, head, body...
let r = rule!(100, head, b1, b2);
```

### Querying
Use the `query!` macro to wrap an atom into a `QueryAtom8` for the kernel:

```rust
use prolog8::{atom, query};
use prolog8::types::ProofMode;

let a = atom!(5, 42);
// Default is ProofMode::Positive
let q1 = query!(a.clone());

// Or explicitly specify proof mode
let q2 = query!(a, ProofMode::Full);
```

## Architecture

Prolog8 rejects textual parsing at the kernel level. Instead, it relies on strict structures (`Atom8`, `Rule8`) for execution, enforcing a constant bound (maximum 8 terms/variables/body atoms) to guarantee fast, deterministic execution suitable for WASM environments.
