# Advanced Patterns Guide

## Type-Level State Machines

Use Rust's type system to encode state at compile time:

```rust
use std::marker::PhantomData;

// State markers (zero-cost, compile-time only)
pub struct Initial;
pub struct Validating;
pub struct Validated;
pub struct Generating;
pub struct Generated;

// State machine with PhantomData
pub struct Pipeline<State> {
    ontology: Store,
    template: String,
    config: Config,
    _state: PhantomData<State>,
}

impl Pipeline<Initial> {
    pub fn new(ontology: Store, template: String) -> Self {
        Self {
            ontology,
            template,
            config: Config::default(),
            _state: PhantomData,
        }
    }

    // Only callable in Initial state
    pub fn with_config(self, config: Config) -> Pipeline<Initial> {
        Pipeline {
            config,
            ..self
        }
    }

    // Transition: Initial → Validating
    pub fn validate(self) -> Result<Pipeline<Validating>> {
        validate_ontology(&self.ontology)?;
        validate_template(&self.template)?;

        Ok(Pipeline {
            ontology: self.ontology,
            template: self.template,
            config: self.config,
            _state: PhantomData,
        })
    }
}

impl Pipeline<Validating> {
    // Transition: Validating → Validated
    pub fn mark_validated(self) -> Pipeline<Validated> {
        Pipeline {
            ontology: self.ontology,
            template: self.template,
            config: self.config,
            _state: PhantomData,
        }
    }
}

impl Pipeline<Validated> {
    // Transition: Validated → Generating
    pub fn generate(self) -> Result<Pipeline<Generating>> {
        // Generation logic
        Ok(Pipeline {
            ontology: self.ontology,
            template: self.template,
            config: self.config,
            _state: PhantomData,
        })
    }
}

impl Pipeline<Generating> {
    // Transition: Generating → Generated
    pub fn complete(self) -> Pipeline<Generated> {
        Pipeline {
            ontology: self.ontology,
            template: self.template,
            config: self.config,
            _state: PhantomData,
        }
    }
}

impl Pipeline<Generated> {
    // Only callable on Generated state
    pub fn get_output(&self) -> String {
        "Generated code here".to_string()
    }
}

// Usage - compile-time enforced state transitions:
let pipeline = Pipeline::new(store, "rust-api".to_string());
let validated = pipeline.validate()?;  // Must validate first
let validated = validated.mark_validated();
let generating = validated.generate()?;
let generated = generating.complete();
let output = generated.get_output();  // Only works on Generated state

// This would NOT compile (skipped validation):
// let output = pipeline.generate();  // ERROR: Pipeline<Initial> has no generate()
```

## Generic Associated Types (GATs)

Enable complex generic relationships:

```rust
use std::future::Future;

// Without GATs - Limited
pub trait OldCollector {
    type Item;
    fn collect(&self) -> Vec<Self::Item>;  // Always synchronous
}

// With GATs - Powerful
pub trait Collector {
    type Item;
    type CollectFuture: Future<Output = Result<Vec<Self::Item>>>;

    fn collect(&self) -> Self::CollectFuture;
}

// Synchronous implementation
pub struct LocalCollector;

impl Collector for LocalCollector {
    type Item = String;
    type CollectFuture = std::future::Ready<Result<Vec<String>>>;

    fn collect(&self) -> Self::CollectFuture {
        std::future::ready(Ok(vec!["item".to_string()]))
    }
}

// Asynchronous implementation
pub struct RemoteCollector;

impl Collector for RemoteCollector {
    type Item = String;
    type CollectFuture = BoxFuture<'static, Result<Vec<String>>>;

    fn collect(&self) -> Self::CollectFuture {
        Box::pin(async {
            // Async HTTP call
            Ok(vec!["remote-item".to_string()])
        })
    }
}
```

## Const Generics (Zero-Cost Limits)

Enforce limits at compile time:

```rust
use std::marker::PhantomData;

// Const generic for maximum size
pub struct FixedVec<T, const N: usize> {
    items: Vec<T>,
}

impl<T, const N: usize> FixedVec<T, N> {
    pub fn new() -> Self {
        Self { items: Vec::with_capacity(N) }
    }

    pub fn push(&mut self, item: T) -> Result<()> {
        if self.items.len() >= N {
            return Err(format!("Capacity {} exceeded", N));
        }
        self.items.push(item);
        Ok(())
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }
}

// Usage - different sizes, different types:
let mut small_vec: FixedVec<String, 10> = FixedVec::new();  // Max 10 items
let mut large_vec: FixedVec<String, 1000> = FixedVec::new();  // Max 1000 items

small_vec.push("item1".to_string())?;
large_vec.push("item1".to_string())?;

// Configuration with const generics
pub struct Config<const TIMEOUT_SECS: u64, const MAX_RETRIES: usize> {
    _phantom: PhantomData<()>,
}

impl<const TIMEOUT_SECS: u64, const MAX_RETRIES: usize> Config<TIMEOUT_SECS, MAX_RETRIES> {
    pub fn timeout(&self) -> Duration {
        Duration::from_secs(TIMEOUT_SECS)
    }

    pub fn max_retries(&self) -> usize {
        MAX_RETRIES
    }
}

// Different configs, zero runtime cost:
let cli_config = Config::<5, 3>;       // 5s timeout, 3 retries
let api_config = Config::<30, 5>;      // 30s timeout, 5 retries
```

## Higher-Ranked Trait Bounds (HRTB)

Handle lifetime relationships in traits:

```rust
// Without HRTB - Limited
pub trait OldObserver {
    fn observe(&self, message: &str);  // Lifetime tied to self
}

// With HRTB - Powerful
pub trait Observer {
    fn observe<'a>(&self, message: &'a str);  // Lifetime independent of self
}

pub fn notify<'a, O: Observer>(observer: &O, message: &'a str) {
    observer.observe(message);
}

// Function that works with any lifetime
pub fn call_observer<'a>(f: impl for<'b> Fn(&'b str)) {
    f("message1");
    f("message2");
}

// Advanced pattern: Streaming parser with HRTB
pub trait StreamParser {
    fn parse_chunk<'a>(&mut self, chunk: &'a [u8]) -> Result<()>;
}

pub struct BufferedParser {
    buffer: Vec<u8>,
}

impl StreamParser for BufferedParser {
    fn parse_chunk<'a>(&mut self, chunk: &'a [u8]) -> Result<()> {
        self.buffer.extend_from_slice(chunk);
        Ok(())
    }
}
```

## Procedural Macros (Code Generation at Compile Time)

Generate code automatically during compilation:

```rust
// Macro definition (in ggen-macros crate)
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(RdfEntity)]
pub fn derive_rdf_entity(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = &input.ident;

    let expanded = quote! {
        impl RdfEntity for #name {
            fn to_rdf(&self) -> String {
                format!("<http://example.com/{}>", stringify!(#name))
            }

            fn from_rdf(_rdf: &str) -> Result<Self> {
                unimplemented!()
            }
        }
    };

    TokenStream::from(expanded)
}

// Usage
#[derive(RdfEntity)]
pub struct User {
    pub id: String,
    pub name: String,
}

// Expands to implementation of RdfEntity automatically
```

## Zero-Copy Abstractions

Minimize allocations with references and borrows:

```rust
// WRONG: Copies data
pub fn process_data(data: Vec<u8>) -> Vec<u8> {
    let mut result = vec![];
    for byte in data {
        result.push(byte * 2);
    }
    result  // Allocates new vector
}

// BETTER: Borrows reference
pub fn process_data(data: &[u8]) -> Vec<u8> {
    data.iter().map(|b| b * 2).collect()
}

// BEST: Iterator (zero-copy)
pub fn process_data(data: &[u8]) -> impl Iterator<Item = u8> + '_ {
    data.iter().map(|b| b * 2)
}

// Usage
let data = vec![1, 2, 3];
let result: Vec<u8> = process_data(&data).collect();
```

## Trait Object Composition

Combine multiple behaviors dynamically:

```rust
pub trait Validator {
    fn validate(&self, input: &str) -> Result<()>;
}

pub trait Transformer {
    fn transform(&self, input: &str) -> Result<String>;
}

pub trait Logger {
    fn log(&self, message: &str);
}

// Compose multiple traits
pub struct Pipeline {
    validators: Vec<Box<dyn Validator>>,
    transformers: Vec<Box<dyn Transformer>>,
    logger: Box<dyn Logger>,
}

impl Pipeline {
    pub fn add_validator<V: Validator + 'static>(&mut self, validator: V) {
        self.validators.push(Box::new(validator));
    }

    pub fn process(&self, input: &str) -> Result<String> {
        // Validate
        for validator in &self.validators {
            validator.validate(input)?;
        }

        // Transform
        let mut result = input.to_string();
        for transformer in &self.transformers {
            result = transformer.transform(&result)?;
        }

        // Log
        self.logger.log(&format!("Processed: {}", result));

        Ok(result)
    }
}
```

## Performance Optimization Techniques

### Inline Hints

```rust
// Force inlining for small functions
#[inline]
pub fn is_valid_id(id: &str) -> bool {
    !id.is_empty() && id.len() < 100
}

// Prevent inlining for expensive functions
#[inline(never)]
pub fn expensive_computation(data: &[u8]) -> Result<u64> {
    // Complex algorithm
    Ok(data.len() as u64)
}
```

### SIMD Operations

```rust
#[cfg(target_arch = "x86_64")]
pub fn sum_aligned(data: &[u32]) -> u64 {
    use std::arch::x86_64::*;

    unsafe {
        let mut sum = 0u64;
        for chunk in data.chunks(4) {
            if chunk.len() == 4 {
                let vec = _mm_set_epi32(
                    chunk[3] as i32,
                    chunk[2] as i32,
                    chunk[1] as i32,
                    chunk[0] as i32,
                );
                // SIMD operations
            }
        }
        sum
    }
}
```

### Memory Layout Optimization

```rust
// Good: Compact memory layout
#[repr(C)]
pub struct CompactEntity {
    id: u64,      // 8 bytes
    active: bool, // 1 byte
    name: String, // 24 bytes
}

// Better: Optimized field order
#[repr(C)]
pub struct OptimizedEntity {
    id: u64,      // 8 bytes
    name: String, // 24 bytes
    active: bool, // 1 byte + 7 padding
}
// Fewer cache misses, better performance
```

## Testing Advanced Patterns

```rust
#[test]
fn test_state_machine_transitions() {
    let pipeline = Pipeline::new(store, "template".into());
    assert!(pipeline.validate().is_ok());

    // This would NOT compile (wrong state):
    // let output = pipeline.get_output();  // ERROR: Pipeline<Initial> has no method
}

#[test]
fn test_const_generic_capacity() {
    let mut vec: FixedVec<String, 2> = FixedVec::new();
    assert!(vec.push("item1".to_string()).is_ok());
    assert!(vec.push("item2".to_string()).is_ok());
    assert!(vec.push("item3".to_string()).is_err());  // Exceeds capacity
}

#[test]
fn test_zero_copy_performance() {
    let data = vec![1, 2, 3, 4, 5];
    let result: Vec<u8> = process_data(&data).collect();
    assert_eq!(result, vec![2, 4, 6, 8, 10]);
}
```

## Critical Rules

1. **USE PhantomData** - For zero-cost type-level patterns
2. **PREFER const generics** - Over runtime values when possible
3. **LEVERAGE the type system** - Make invalid states unrepresentable
4. **TEST compile-time failures** - Ensure type safety works
5. **DOCUMENT patterns** - Advanced patterns need explanation
6. **BENCHMARK optimizations** - Don't guess, measure
7. **COMMENT unsafe code** - Explain safety invariants

---

## Quick Reference

```rust
// State machine template
struct Pipeline<S> {
    data: Data,
    _state: PhantomData<S>,
}

// Const generic template
struct Container<T, const N: usize> {
    items: Vec<T>,
}

// HRTB template
pub trait Callback {
    fn call<'a>(&self, arg: &'a str);
}

// Trait object template
let processors: Vec<Box<dyn Processor>> = vec![];
```
