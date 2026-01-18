<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Type System Design: Zero-Cost Abstractions and Compile-Time Guarantees](#type-system-design-zero-cost-abstractions-and-compile-time-guarantees)
  - [Type Safety Through Code Generation](#type-safety-through-code-generation)
    - [The Problem: Runtime Errors](#the-problem-runtime-errors)
    - [The Solution: Generated Types](#the-solution-generated-types)
  - [Type-Level Encoding](#type-level-encoding)
    - [Principle: Make Invalid States Unrepresentable](#principle-make-invalid-states-unrepresentable)
    - [Using Phantom Types (Advanced)](#using-phantom-types-advanced)
  - [Type Mapping Strategies](#type-mapping-strategies)
    - [Strategy 1: Direct Mapping](#strategy-1-direct-mapping)
    - [Strategy 2: Safe Wrappers](#strategy-2-safe-wrappers)
    - [Strategy 3: Phantom Types for Units](#strategy-3-phantom-types-for-units)
  - [Generics for Code Reuse](#generics-for-code-reuse)
    - [Without Generics (Code Duplication)](#without-generics-code-duplication)
    - [With Generics (Zero-Cost Abstraction)](#with-generics-zero-cost-abstraction)
  - [Trait-Based Design](#trait-based-design)
    - [Without Traits (Inflexible)](#without-traits-inflexible)
    - [With Traits (Flexible)](#with-traits-flexible)
  - [Type Variance and Contravariance](#type-variance-and-contravariance)
    - [Invariant (Most Common)](#invariant-most-common)
    - [Covariant (Immutable References)](#covariant-immutable-references)
  - [Type-Class Patterns](#type-class-patterns)
    - [Serialization Type Class](#serialization-type-class)
  - [Compile-Time Guarantees](#compile-time-guarantees)
    - [Linear Types (Rust)](#linear-types-rust)
    - [Newtype Pattern](#newtype-pattern)
  - [Zero-Cost Abstractions](#zero-cost-abstractions)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Type System Design: Zero-Cost Abstractions and Compile-Time Guarantees

Understanding how ggen leverages type systems across languages.

## Type Safety Through Code Generation

### The Problem: Runtime Errors

Without strong types, bugs hide until production:

```javascript
// No types - runtime error possible
function updateUser(user) {
  user.emailAddress = "test@example.com";  // Field name typo
  return user;  // Bug: property not updated
}

const user = { email: "old@example.com" };
updateUser(user);
console.log(user.email);  // Still "old@example.com" - surprise!
```

### The Solution: Generated Types

ggen generates strict types enforced at compile-time:

**TypeScript** (compile-time checking):
```typescript
interface User {
  email: string;  // Property name fixed in types
}

function updateUser(user: User) {
  user.emailAddress = "test@example.com";  // Compile error!
  // Error: Property 'emailAddress' does not exist
}
```

**Rust** (memory-safe types):
```rust
pub struct User {
    pub email: String,  // Owned string, memory safe
}

fn update_user(mut user: User) {
    user.email_address = "test@example.com";  // Compile error!
    // error[E0560]: struct `User` has no field `email_address`
}
```

**Python** (runtime checking with Pydantic):
```python
from pydantic import BaseModel

class User(BaseModel):
    email: str

user = User(email="old@example.com")
user.email_address = "test@example.com"
# Raises ValidationError: Field 'email_address' not found
```

## Type-Level Encoding

### Principle: Make Invalid States Unrepresentable

**DON'T**: Use boolean flags
```typescript
// ❌ Confusing - what do these flags mean together?
interface User {
  isActive: boolean;
  isPremium: boolean;
  isSuspended: boolean;
}

// Can user be active AND suspended?
```

**DO**: Use type-level encoding
```typescript
// ✅ Type system prevents invalid combinations
type UserStatus =
  | { kind: 'Active'; premiumLevel: 'Free' | 'Premium' }
  | { kind: 'Suspended'; reason: string }
  | { kind: 'Deleted'; deletedAt: Date };

interface User {
  status: UserStatus;  // Only valid combinations possible
}
```

### Using Phantom Types (Advanced)

Encode constraints in the type itself:

```rust
// Create a type that's verified at compile-time
pub struct Verified<T>(pub T);

impl User {
    // This function only accepts verified users
    pub fn process_payment(verified: Verified<User>) {
        // user is guaranteed to be in valid state
    }
}
```

## Type Mapping Strategies

### Strategy 1: Direct Mapping

```turtle
# RDF definition
xsd:string      → string (TypeScript)
xsd:integer     → number (TypeScript)
xsd:boolean     → boolean (TypeScript)
xsd:dateTime    → Date (TypeScript)
```

### Strategy 2: Safe Wrappers

For types where direct mapping isn't safe:

```rust
// Problem: Raw email string could be invalid
user.email = "not-an-email";  // Compiles but semantically wrong

// Solution: Use newtype pattern
pub struct Email(String);

impl Email {
    pub fn new(value: String) -> Result<Self, EmailError> {
        if value.contains('@') {
            Ok(Email(value))
        } else {
            Err(EmailError::InvalidFormat)
        }
    }
}

// Now invalid emails can't exist
let email = Email::new("not-an-email")?;  // Returns error
```

Generated Rust code can use this pattern:

```rust
pub struct User {
    pub id: UserId,              // Newtype-wrapped String
    pub email: Email,            // Verified email
    pub username: Username,      // Verified username
    pub status: UserStatus,      // Enum (discriminated union)
}
```

### Strategy 3: Phantom Types for Units

```rust
// Physical quantities with units tracked at compile time
pub struct Quantity<T, U> {
    value: T,
    unit: PhantomData<U>,
}

pub struct Kilograms;
pub struct Pounds;

fn weigh(user: User) -> Quantity<f64, Kilograms> {
    // Return weight in kg
    Quantity { value: 75.0, unit: PhantomData }
}

// Type system prevents unit confusion
let weight_kg = weigh(user);
let weight_lbs: Quantity<f64, Pounds> = weight_kg;  // Compile error!
// error: cannot assign `Kg` to `Pounds`
```

## Generics for Code Reuse

### Without Generics (Code Duplication)

```rust
// ❌ Repeated code for each type
pub struct UserRepository {
    items: Vec<User>,
}

impl UserRepository {
    pub fn find(&self, id: String) -> Option<User> {
        self.items.iter().find(|u| u.id == id).cloned()
    }
}

pub struct ProductRepository {
    items: Vec<Product>,
}

impl ProductRepository {
    pub fn find(&self, id: String) -> Option<Product> {
        self.items.iter().find(|p| p.id == id).cloned()
    }
}
```

### With Generics (Zero-Cost Abstraction)

```rust
// ✅ Single generic implementation
pub struct Repository<T> {
    items: Vec<T>,
}

impl<T: Clone + PartialEq> Repository<T> {
    pub fn find(&self, item: &T) -> Option<T> {
        self.items.iter().find(|&i| i == item).cloned()
    }
}

// Used for any type
let users = Repository { items: vec![user1, user2] };
let products = Repository { items: vec![prod1, prod2] };

// No runtime cost - generics are monomorphized at compile time
// Each type gets its own compiled version (zero-cost abstraction)
```

## Trait-Based Design

### Without Traits (Inflexible)

```rust
// ❌ Hard-coded dependency
pub struct UserValidator {
    database: UserDatabase,
}

impl UserValidator {
    pub fn validate(&self, user: &User) -> bool {
        self.database.user_exists(&user.email)  // Can't swap implementation
    }
}
```

### With Traits (Flexible)

```rust
// ✅ Generic over the trait
pub trait UserStore {
    fn user_exists(&self, email: &str) -> bool;
}

pub struct UserValidator<T: UserStore> {
    store: T,
}

impl<T: UserStore> UserValidator<T> {
    pub fn validate(&self, user: &User) -> bool {
        self.store.user_exists(&user.email)  // Works with any implementation
    }
}

// Implementations
pub struct DatabaseStore { /* ... */ }
impl UserStore for DatabaseStore { /* ... */ }

pub struct MemoryStore { /* ... */ }
impl UserStore for MemoryStore { /* ... */ }

// Use either without changing UserValidator
let db_validator = UserValidator { store: DatabaseStore::new() };
let mem_validator = UserValidator { store: MemoryStore::new() };

// Zero-cost abstraction - no runtime polymorphism overhead
```

## Type Variance and Contravariance

### Invariant (Most Common)

```rust
// Container is invariant in T - can't substitute types
pub struct Container<T> {
    item: T,
}

let mut c: Container<User> = Container { item: user };
let c_any: Container<dyn Any> = c;  // Compile error - invariant
```

### Covariant (Immutable References)

```rust
// &T is covariant - can use subtype where supertype expected
pub fn print_user(user: &User) {
    println!("{}", user.email);
}

pub fn process_admin(admin: &Admin)
where
    Admin: Deref<Target = User>,  // Admin can be used as User
{
    print_user(admin);  // Covariant - OK
}
```

## Type-Class Patterns

### Serialization Type Class

```rust
// Define capability through traits
pub trait Serialize {
    fn to_json(&self) -> String;
}

// Implement for generated types
impl Serialize for User {
    fn to_json(&self) -> String {
        // Auto-generated implementation
        format!(r#"{{"email":"{}"}}"#, self.email)
    }
}

// Works for any type implementing Serialize
pub fn save<T: Serialize>(item: &T) {
    println!("{}", item.to_json());
}

save(&user);      // User implements Serialize
save(&product);   // Product implements Serialize
```

## Compile-Time Guarantees

### Linear Types (Rust)

Types enforce resource management:

```rust
pub struct Connection {
    // Represents a database connection
}

impl Connection {
    pub fn query(&mut self, sql: &str) -> Result<Vec<Row>> {
        // Execute query
    }

    pub fn close(self) {  // Takes ownership
        // Connection must be explicitly closed
        // Can't accidentally use after close
    }
}

let conn = get_connection();
conn.query("SELECT * FROM users")?;
conn.close();
conn.query("...");  // Compile error!
// error[E0382]: borrow of moved value: `conn`
```

### Newtype Pattern

```rust
pub struct UserId(String);
pub struct ProductId(String);

pub fn user_discount(user: UserId) -> f64 {
    // Compiler guarantees this is a user ID, not a product ID
}

let user_id = UserId("user-123".to_string());
let product_id = ProductId("prod-456".to_string());

user_discount(user_id);        // OK
user_discount(product_id);     // Compile error!
// expected `UserId`, found `ProductId`
```

## Zero-Cost Abstractions

Generated types add ZERO runtime overhead:

```rust
// Generated code with generics
pub struct Repository<T> { items: Vec<T> }

// Compiled code (monomorphization)
// The compiler generates TWO versions:
pub struct Repository__User { items: Vec<User> }  // For User
pub struct Repository__Product { items: Vec<Product> }  // For Product

// No virtual function calls, no type tags, no heap allocation
// Pure static polymorphism = zero-cost
```

## Summary

Type system design enables:
- ✅ **Compile-time safety**: Errors caught before production
- ✅ **Zero-cost abstractions**: Type safety without runtime overhead
- ✅ **Expressive power**: Encode domain knowledge in types
- ✅ **Code reuse**: Generics without duplication
- ✅ **Flexibility**: Traits for swappable implementations
- ✅ **Memory safety**: Ownership prevents use-after-free
- ✅ **Invalid state prevention**: Make wrong code impossible

Your generated types are powerful tools!
