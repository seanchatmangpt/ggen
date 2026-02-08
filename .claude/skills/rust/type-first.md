# Type-First Design

## Elite Rust Mindset

**Principle**: Encode invariants in types. Make illegal states unrepresentable.

## Type-Level Guarantees

### NewType Pattern
```rust
// Domain-specific types prevent misuse
#[derive(Debug, Clone, Copy)]
pub struct UserId(u64);

#[derive(Debug, Clone, Copy)]
pub struct OrderId(u64);

// Compiler prevents: fn process(user: UserId, order: OrderId)
// from being called as: process(order_id, user_id)
```

### State Machines
```rust
// Type-safe state machine
struct Closed;
struct Open;
struct Authenticated;

struct Connection<State> {
    _state: PhantomData<State>,
}

impl Connection<Closed> {
    pub fn open(self) -> Connection<Open> {
        Connection { _state: PhantomData }
    }
}

impl Connection<Open> {
    pub fn authenticate(self, password: &str) -> Result<Connection<Authenticated>> {
        // Can only authenticate when open
    }
}

impl Connection<Authenticated> {
    pub fn query(&self, sql: &str) -> Result<QueryResult> {
        // Can only query when authenticated
    }
}
```

### Const Generics
```rust
// Compile-time array sizes
struct Matrix<const ROWS: usize, const COLS: usize> {
    data: [[f64; COLS]; ROWS],
}

impl<const N: usize> Matrix<N, N> {
    // Only square matrices can be inverted
    pub fn invert(&self) -> Matrix<N, N> {
        // Implementation
    }
}
```

## Zero-Cost Abstractions

### Zero-Cost ✅
```rust
// Generics monomorphize at compile time
fn process<T: Iterator>(iter: T) { }

// Macros expand at compile time
macro_rules! repeat {
    ($e:expr; $n:expr) => { [$e; $n] }
}

// Const generics compile to specialized code
```

### Has Cost ⚠️
```rust
// Trait objects require vtable lookup
fn process(iter: &dyn Iterator) { }

// Heap allocations
let boxed = Box::new(value);
let vec = Vec::new();

// Arc/Rc for shared ownership
let shared = Arc::new(value);
```

## API Design Principles

### Type-Safe by Default
```rust
// Result<T, E> not panics
pub fn parse(input: &str) -> Result<Ast, ParseError> {
    // Never unwrap() or expect() in production
}
```

### Builder Pattern
```rust
pub struct Config {
    host: String,
    port: u16,
    timeout: Duration,
}

impl Config {
    pub fn builder() -> ConfigBuilder {
        ConfigBuilder::default()
    }
}

pub struct ConfigBuilder {
    host: Option<String>,
    port: Option<u16>,
    timeout: Option<Duration>,
}

impl ConfigBuilder {
    pub fn host(mut self, host: impl Into<String>) -> Self {
        self.host = Some(host.into());
        self
    }

    pub fn build(self) -> Result<Config> {
        // Validate and construct
    }
}
```

### Make Misuse Impossible
```rust
// Lifetime prevents use-after-free
pub struct Parser<'a> {
    input: &'a str,
    position: usize,
}

// Type prevents invalid states
pub enum ConnectionState {
    Connected { socket: TcpStream },
    Disconnected { reason: String },
}
```

## 80/20 Idea Generation

For every problem, generate 3 ideas:

1. **Immediate**: Solve the problem directly
2. **80/20 Sweet Spot**: Solve 80% of related problems with 20% effort
3. **Maximum Value**: Type-level solution that prevents entire classes of bugs

**Example**: "Add validation to user input"

1. Immediate: Add runtime validation function
2. 80/20: Create ValidatedString<Rule> generic type
3. Maximum: Create type-level validation using const generics + trait bounds

**Choose #2 (80/20) by default**

## Performance Mindset

```rust
// Prefer references over owned
fn process(data: &[u8]) -> Result<Output>  // ✅

// Stack allocation when possible
let buffer: [u8; 1024] = [0; 1024];  // ✅

// Minimize allocations in hot paths
let mut cache = HashMap::with_capacity(expected_size);  // ✅

// Profile before optimizing
// cargo make bench
```

## Mental Models

- Types = invariants = compile-time guarantees
- Zero-cost = generics/macros/const generics
- Performance = references/stack/minimize allocations
- Ownership = explicit = memory safety
- APIs = type-safe = ergonomic = composable

**Ask yourself**:
- "What can I express in types?"
- "Is this abstraction zero-cost?"
- "What's the performance characteristic?"
- "What are ownership semantics?"
- "How to make misuse impossible?"
