# Type Safety Patterns - Compile-Time Error Prevention

## Purpose

This command guides agents to design code that prevents errors at compile time through Rust's type system. Making invalid states unrepresentable through types prevents entire classes of runtime errors. Experts use the type system to make errors impossible before they happen.

---

## Core Principle: Make Invalid States Unrepresentable

The fundamental principle: **If you can't represent an invalid state in your type system, you've prevented that error.**

**Why this matters for RDF/SPARQL code generation**:
- RDF graphs can have invalid states (disconnected nodes, missing types)
- SPARQL queries can have invalid patterns (undefined variables, type mismatches)
- Generated code can have invalid configurations (missing required fields)
- Type system prevents these at compile time, not at runtime

---

## Pattern 1: Newtypes for Validation

### Purpose

Create newtypes that enforce invariants. Instead of generic types (i32, String, usize), use specific types that prevent invalid values.

### Example: RDF Node Identifiers

**❌ BAD: Can have invalid state**
```rust
struct RdfNode {
    id: String,  // Could be empty - invalid RDF node
    label: String,  // Could be empty - invalid label
}
```

**✅ GOOD: Types prevent invalid state**
```rust
#[derive(Debug, Clone, PartialEq, Eq)]
struct NodeId(String);

impl NodeId {
    fn new(id: String) -> Result<Self, Error> {
        if id.is_empty() {
            Err(Error::EmptyNodeId)
        } else {
            Ok(NodeId(id))
        }
    }
}

#[derive(Debug, Clone)]
struct RdfNode {
    id: NodeId,  // Type prevents empty ID
    label: String,
}
```

### Example: Count Types

**Problem**: Using `usize` for everything - can't distinguish between different counts.

**Solution**: Create specific newtypes for each count type.

```rust
// Newtype for scenario index (prevent index confusion)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct ScenarioIndex(usize);

impl ScenarioIndex {
    fn new(index: usize) -> Result<Self, Error> {
        Ok(ScenarioIndex(index))
    }
}

// Newtype for total count (prevent treating as index)
#[derive(Debug, Clone, Copy)]
struct TotalCount(usize);

impl TotalCount {
    fn new(count: usize) -> Result<Self, Error> {
        if count == 0 {
            Err(Error::CountMustBePositive)
        } else {
            Ok(TotalCount(count))
        }
    }
}

// Type prevents mixing ScenarioIndex and TotalCount
fn validate_index(index: ScenarioIndex, total: TotalCount) -> Result<(), Error> {
    if index.0 < total.0 {
        Ok(())
    } else {
        Err(Error::IndexOutOfBounds)
    }
}
```

### Benefits

- **Type safety**: Compiler prevents passing wrong type
- **Self-documenting**: Type name explains what value represents
- **Prevents errors**: Can't accidentally use index as count
- **Zero overhead**: Newtypes are zero-cost abstractions

---

## Pattern 2: Enums for State Machines

### Purpose

Use enums to represent valid states only. Prevents being in multiple conflicting states simultaneously.

### Example: RDF Graph State

**❌ BAD: Can be in invalid state**
```rust
struct RdfGraph {
    is_validated: bool,
    is_published: bool,
    is_archived: bool,
    // Could have is_validated=true AND is_archived=true - invalid!
}
```

**✅ GOOD: Enum prevents invalid states**
```rust
enum GraphState {
    Empty,
    Loaded { node_count: usize },
    Validated { node_count: usize },
    Published { node_count: usize },
    Archived { node_count: usize },
}

struct RdfGraph {
    state: GraphState,  // Only valid states possible
}

impl RdfGraph {
    fn validate(self) -> Result<RdfGraph, Error> {
        match self.state {
            GraphState::Loaded { node_count } => {
                // Validate...
                Ok(RdfGraph {
                    state: GraphState::Validated { node_count },
                })
            }
            _ => Err(Error::InvalidStateTransition),
        }
    }

    fn publish(self) -> Result<RdfGraph, Error> {
        match self.state {
            GraphState::Validated { node_count } => {
                Ok(RdfGraph {
                    state: GraphState::Published { node_count },
                })
            }
            _ => Err(Error::MustValidateBeforePublishing),
        }
    }
}
```

### Example: SPARQL Query State

**Prevent invalid query operations**:
```rust
enum QueryState {
    Initial,
    SelectDefined { variables: Vec<String> },
    FilterApplied { variables: Vec<String> },
    Ordered { variables: Vec<String> },
    Limited { variables: Vec<String>, limit: usize },
}

struct SparqlBuilder {
    state: QueryState,
}

impl SparqlBuilder {
    fn select(mut self, vars: Vec<String>) -> Result<Self, Error> {
        match self.state {
            QueryState::Initial => {
                self.state = QueryState::SelectDefined { variables: vars };
                Ok(self)
            }
            _ => Err(Error::SelectAlreadyDefined),
        }
    }

    // Can only filter after select
    fn filter(mut self, condition: String) -> Result<Self, Error> {
        match self.state {
            QueryState::SelectDefined { variables } => {
                self.state = QueryState::FilterApplied { variables };
                Ok(self)
            }
            _ => Err(Error::MustSelectBeforeFilter),
        }
    }
}
```

### Benefits

- **Prevents invalid states**: Type system enforces state validity
- **Type-driven API**: Functions only available in certain states
- **Clear semantics**: State transitions are explicit
- **Catches errors early**: Compiler prevents invalid operations

---

## Pattern 3: Option/Result for Nullable Values

### Purpose

Use `Option<T>` instead of null, `Result<T, E>` instead of exceptions. Force handling of error/empty cases.

### Example: SPARQL Query Results

**❌ BAD: Can return null, causes runtime errors**
```rust
fn get_first_result(results: &[QueryResult]) -> QueryResult {
    // What if results is empty? Returns garbage or panics
    results[0]
}
```

**✅ GOOD: Type forces handling of empty case**
```rust
fn get_first_result(results: &[QueryResult]) -> Option<QueryResult> {
    results.first().cloned()
}

// Usage forces handling:
match get_first_result(&results) {
    Some(result) => println!("Found: {:?}", result),
    None => println!("No results"),
}
```

### Example: RDF Parsing

**❌ BAD: Panics on invalid input**
```rust
fn parse_rdf(content: &str) -> RdfGraph {
    // .unwrap() panics if parsing fails
    serde_rdf::from_str(content).unwrap()
}
```

**✅ GOOD: Type forces error handling**
```rust
fn parse_rdf(content: &str) -> Result<RdfGraph, ParseError> {
    serde_rdf::from_str(content)
}

// Usage forces handling:
match parse_rdf(content) {
    Ok(graph) => {
        // Process graph
    }
    Err(e) => {
        // Handle parse error
        eprintln!("Parse failed: {}", e);
    }
}
```

### Benefits

- **No null pointer errors**: Option type is explicit
- **No silent failures**: Result forces error handling
- **Compiler enforces handling**: Can't accidentally ignore errors
- **Self-documenting**: Type shows function can fail or return None

---

## Pattern 4: PhantomData for Type-Level Invariants

### Purpose

Use `PhantomData` to encode invariants in types without runtime cost. Useful for preventing use-after-close or other state violations.

### Example: RDF Store Connection

**Problem**: Can call operations on closed connection.

**Solution**: Encode connection state in type.

```rust
use std::marker::PhantomData;

// Type-level states
struct Open;
struct Closed;

// Connection parameterized by state
struct RdfConnection<State> {
    url: String,
    _state: PhantomData<State>,
}

// Only Open connections can query
impl RdfConnection<Open> {
    fn query(&mut self, sparql: &str) -> Result<QueryResult, Error> {
        // Perform query - only available when Open
        Ok(QueryResult::default())
    }

    fn close(self) -> RdfConnection<Closed> {
        // Consume Open handle, return Closed handle
        RdfConnection {
            url: self.url,
            _state: PhantomData,
        }
    }
}

// Closed connections can't query - compiler error!
// impl RdfConnection<Closed> {
//     fn query(&self, _sparql: &str) -> Result<QueryResult, Error> { ... }
// }  // Don't implement - compiler prevents calling query on Closed

// Usage:
let mut conn = RdfConnection::<Open> { ... };
conn.query("SELECT * WHERE ...")?;  // ✅ Valid - connection is Open

let closed = conn.close();
// closed.query("...")?;  // ❌ Compiler error - connection is Closed
```

### Benefits

- **Type-level guarantees**: Prevent use-after-close at compile time
- **Zero runtime cost**: PhantomData is erased at compile time
- **Impossible to violate**: Compiler enforces invariants
- **Self-documenting**: Type shows valid operations

---

## Pattern 5: Compile-Time Size Validation

### Purpose

Use const generics to enforce array sizes at compile time. Prevents index out-of-bounds errors.

### Example: Fixed-Size RDF Batch

**Problem**: Can index out of bounds.

**Solution**: Use const generic for batch size.

```rust
// Batch with compile-time known size
struct RdfBatch<const N: usize> {
    nodes: [RdfNode; N],
    count: usize,  // How many are actually filled
}

impl<const N: usize> RdfBatch<N> {
    // Index checked against const size at compile time
    fn get(&self, idx: usize) -> Option<&RdfNode> {
        if idx < self.count && idx < N {
            Some(&self.nodes[idx])
        } else {
            None
        }
    }

    // Can only get up to N items
    fn get_all(&self) -> &[RdfNode] {
        &self.nodes[..self.count]
    }
}

// Usage - size encoded in type:
let batch: RdfBatch<100> = RdfBatch {
    nodes: [...],
    count: 50,
};

let node = batch.get(49)?;  // ✅ Valid
// let node = batch.get(150)?;  // ❌ Compiler knows N=100, may warn
```

### Benefits

- **Compile-time bounds checking**: Compiler knows valid range
- **No runtime overhead**: Size known at compile time
- **Type-safe batching**: Can't accidentally create wrong-size batch
- **API clarity**: Type shows constraints

---

## Pattern 6: Lifetimes for Memory Safety

### Purpose

Use lifetimes to prevent use-after-free and borrowing violations. Compiler enforces that references don't outlive data.

### Example: RDF Graph References

**Problem**: Can use reference after graph is destroyed.

**Solution**: Use lifetime to tie reference to graph.

```rust
// Graph owns data, references borrow
struct RdfGraph {
    nodes: Vec<RdfNode>,
}

impl RdfGraph {
    // Lifetime 'a ties returned reference to &self
    fn get_node(&'a self, id: NodeId) -> Option<&'a RdfNode> {
        self.nodes.iter().find(|n| n.id == id)
    }

    // Can't return reference that outlives self
    // fn get_node_lifetime_error(&self) -> &'static RdfNode {
    //     &self.nodes[0]  // ❌ Compiler error - can't be 'static
    // }
}

// Usage:
let graph = RdfGraph { nodes: vec![...] };
let node_ref = graph.get_node(NodeId::new("node1"))?;
// node_ref is valid only while graph exists
// If graph is dropped, node_ref becomes invalid (compiler prevents this)
println!("{:?}", node_ref);  // ✅ Valid - graph still in scope

// graph is dropped here - node_ref automatically invalid
// println!("{:?}", node_ref);  // ❌ Compiler error - graph no longer exists
```

### Benefits

- **Prevents use-after-free**: Compiler enforces lifetime rules
- **No garbage collection needed**: Memory freed safely
- **Efficient borrowing**: Can share references safely
- **Zero runtime cost**: Lifetimes erased at compile time

---

## Pattern 7: Trait Bounds for Constraints

### Purpose

Use trait bounds to restrict types, ensuring correct interface implementation.

### Example: SPARQL Query Builders

**Prevent building invalid queries**:

```rust
// Trait for values that can be serialized to RDF
trait RdfSerializable {
    fn to_rdf(&self) -> String;
}

// Only types implementing trait can be used in query builder
impl RdfSerializable for String {
    fn to_rdf(&self) -> String { /* ... */ }
}

impl RdfSerializable for i32 {
    fn to_rdf(&self) -> String { /* ... */ }
}

struct QueryBuilder<T: RdfSerializable> {
    values: Vec<T>,
}

impl<T: RdfSerializable> QueryBuilder<T> {
    fn add_value(&mut self, value: T) {
        self.values.push(value);
    }

    fn build(self) -> String {
        self.values.iter()
            .map(|v| v.to_rdf())
            .collect::<Vec<_>>()
            .join(" ")
    }
}

// Usage:
let mut builder: QueryBuilder<String> = QueryBuilder { values: vec![] };
builder.add_value("value1".to_string());  // ✅ String implements RdfSerializable

// let mut builder: QueryBuilder<Vec<u8>> = QueryBuilder { ... };
// builder.add_value(vec![]);  // ❌ Compiler error - Vec<u8> doesn't implement RdfSerializable
```

### Benefits

- **Type-safe APIs**: Only correct types accepted
- **Compile-time verification**: Invalid types rejected
- **Self-documenting**: Trait bounds show requirements
- **Composable**: Trait-based design enables composition

---

## Complete Type Safety Workflow

```rust
// Step 1: Identify where runtime errors could occur
// - Invalid RDF node IDs (empty strings)
// - Type mismatches (index vs count)
// - Invalid state transitions (query on closed connection)

// Step 2: Design type-level prevention
#[derive(Debug, Clone)]
struct NodeId(String);  // Newtype prevents empty ID

#[derive(Debug, Copy)]
struct ScenarioIndex(usize);  // Newtype prevents index/count confusion

enum ConnectionState { Open, Closed }
struct Connection<S> { ... }  // Enum prevents invalid state

// Step 3: Verify prevention at compile time
// cargo make check
// Compiler rejects invalid operations

// Step 4: Test that valid operations work
#[test]
fn test_node_creation() {
    let node = NodeId::new("node1".to_string()).unwrap();  // ✅ Valid
}

#[test]
fn test_invalid_node_creation() {
    assert!(NodeId::new("".to_string()).is_err());  // ✅ Prevents empty
}
```

---

## Integration with Problem Solving

These patterns work with other problem-solving approaches:
- **5 Whys**: After root cause reveals "invalid state not prevented", use type safety patterns
- **Andon Signals**: Compiler errors from type violations appear as signals to fix
- **DMAIC**: Type safety is part of the "Improve" step when preventing quality issues

---

## Expert Insights

**Why this matters**: Runtime errors are expensive. Type-level prevention catches errors at compile time, before they reach production.

**Key principle**: "Make invalid states unrepresentable" - Use types to make errors impossible.

**For RDF/SPARQL**: Graphs and queries have many possible invalid states. Types prevent them at compile time.

**Remember**: The type system is your friend. Use it to prevent entire classes of errors. If you can't represent an invalid state in your type system, you've prevented that error.

---

## When to Apply

| Situation | Pattern | Benefit |
|-----------|---------|---------|
| Value with constraints (positive number, non-empty string) | Newtype | Prevents invalid values |
| Multiple states, only some valid | Enum | Prevents invalid states |
| Value might be missing | Option/Result | Prevents null/panics |
| State requires invariants across operations | PhantomData | Prevents use-after-close |
| Fixed-size collections | Const Generics | Prevents index errors |
| References with constraints | Lifetimes | Prevents use-after-free |
| Type requirements | Trait Bounds | Ensures correct interface |

