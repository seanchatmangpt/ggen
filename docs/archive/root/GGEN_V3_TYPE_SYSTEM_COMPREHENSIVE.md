<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen v3 Type System - Comprehensive Language Mapping](#ggen-v3-type-system---comprehensive-language-mapping)
  - [Table of Contents](#table-of-contents)
  - [Overview](#overview)
    - [Type Projection Model](#type-projection-model)
    - [Completeness Guarantee](#completeness-guarantee)
  - [Primitive Types](#primitive-types)
    - [XSD Base Types](#xsd-base-types)
      - [1. String Types](#1-string-types)
      - [2. Numeric Types](#2-numeric-types)
      - [3. Boolean Type](#3-boolean-type)
      - [4. Date & Time Types](#4-date--time-types)
      - [5. Other Primitive Types](#5-other-primitive-types)
  - [Composite Types](#composite-types)
    - [Struct (Record) Projections](#struct-record-projections)
      - [Rust Struct Projection](#rust-struct-projection)
      - [TypeScript Interface Projection](#typescript-interface-projection)
      - [Python Dataclass Projection](#python-dataclass-projection)
      - [Go Struct Projection](#go-struct-projection)
      - [Java Class Projection](#java-class-projection)
    - [Enum Projections](#enum-projections)
      - [Rust Enum](#rust-enum)
      - [TypeScript Enum](#typescript-enum)
      - [Python Enum](#python-enum)
  - [Generic Types](#generic-types)
    - [List/Array Types](#listarray-types)
    - [Optional/Nullable Types](#optionalnullable-types)
    - [Map/Dictionary Types](#mapdictionary-types)
    - [Tuple Types](#tuple-types)
  - [Type Constraints & Validation](#type-constraints--validation)
    - [Constraint Types in SPARQL/SHACL](#constraint-types-in-sparqlshacl)
    - [Validation Code Generation](#validation-code-generation)
      - [Rust Validator](#rust-validator)
      - [TypeScript Validator](#typescript-validator)
      - [Python Validator](#python-validator)
  - [Language-Specific Mappings](#language-specific-mappings)
    - [Complete Cross-Language Type Table](#complete-cross-language-type-table)
  - [Cross-Language Type Equivalence](#cross-language-type-equivalence)
    - [Principle: Semantic Equivalence, not Syntactic Equivalence](#principle-semantic-equivalence-not-syntactic-equivalence)
    - [Equivalence Testing](#equivalence-testing)
  - [Type Serialization](#type-serialization)
    - [Serialization Format: JSON Lines](#serialization-format-json-lines)
      - [Struct Serialization](#struct-serialization)
      - [Enum Serialization](#enum-serialization)
      - [Custom Serialization](#custom-serialization)
  - [Advanced Patterns](#advanced-patterns)
    - [Newtype Pattern](#newtype-pattern)
    - [Builder Pattern](#builder-pattern)
    - [Sum Types / Tagged Unions](#sum-types--tagged-unions)
    - [Type Coercion & Compatibility](#type-coercion--compatibility)
  - [Type System Completeness Verification](#type-system-completeness-verification)
    - [Generated Tests](#generated-tests)
    - [Cross-Language Verification](#cross-language-verification)
  - [Conclusion](#conclusion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen v3 Type System - Comprehensive Language Mapping

**Status**: COMPREHENSIVE SPECIFICATION
**Version**: 3.0.0-alpha
**Scope**: Complete type system mapping across 10+ programming languages

---

## Table of Contents

1. [Overview](#overview)
2. [Primitive Types](#primitive-types)
3. [Composite Types](#composite-types)
4. [Generic Types](#generic-types)
5. [Type Constraints & Validation](#constraints)
6. [Language-Specific Mappings](#language-mappings)
7. [Cross-Language Type Equivalence](#equivalence)
8. [Type Serialization](#serialization)
9. [Advanced Patterns](#patterns)

---

## Overview

ggen's type system ensures that a single domain ontology can be projected into multiple programming languages with **zero type drift**.

### Type Projection Model

```
RDF Ontology (ggen:Type entities)
  ↓
XSD Type Classification
  ├─ Primitive (String, Integer, Decimal, Boolean, DateTime)
  ├─ Composite (Struct, Enum, Union)
  ├─ Generic (List<T>, Map<K,V>, Optional<T>)
  └─ Custom (User-defined types)
  ↓
Language-Specific Mapping
  ├─ Rust: String → struct/enum + derives
  ├─ TypeScript: string → interface/type
  ├─ Python: str → dataclass/enum
  ├─ Go: string → struct/interface
  ├─ Java: String → class/enum
  └─ ... (5+ more languages)
  ↓
Generated Code (with serialization, validation, etc.)
```

### Completeness Guarantee

For every RDF type T in ontology O:
```
∀ language L ∈ {Rust, TypeScript, Python, Go, Java, ...}:
  ∃ generated_code(T, L) such that:
    - Type signature matches semantically
    - All constraints enforced in language idioms
    - Serialization/deserialization consistent
    - All properties preserved
```

---

## Primitive Types

### XSD Base Types

#### 1. String Types

| XSD Type | Rust | TypeScript | Python | Go | Java | C# | Kotlin |
|----------|------|-----------|--------|----|----|----|----|
| `xsd:string` | `String` | `string` | `str` | `string` | `String` | `string` | `String` |
| `xsd:normalizedString` | `String` | `string` | `str` | `string` | `String` | `string` | `String` |
| `xsd:token` | `String` | `string` | `str` | `string` | `String` | `string` | `String` |
| `xsd:language` | `String` | `string` | `str` | `string` | `String` | `string` | `String` |
| `xsd:NMTOKEN` | `String` | `string` | `str` | `string` | `String` | `string` | `String` |
| `xsd:Name` | `String` | `string` | `str` | `string` | `String` | `string` | `String` |
| `xsd:NCName` | `String` | `string` | `str` | `string` | `String` | `string` | `String` |

**Constraints & Validation**:
```
xsd:string
  max:50
    → Rust: panic if > 50 chars, or use custom BoundedString
    → TypeScript: runtime check, throw on violation
    → Python: Pydantic validator
    → Go: custom String type with validation
    → Java: String with length check

xsd:pattern [a-z]+
    → All langs: regex validation on assignment
```

#### 2. Numeric Types

| XSD Type | Rust | TypeScript | Python | Go | Java | C# | Kotlin |
|----------|------|-----------|--------|----|----|----|----|
| `xsd:integer` | `i64` | `number` | `int` | `int64` | `long` | `long` | `Long` |
| `xsd:int` | `i32` | `number` | `int` | `int32` | `int` | `int` | `Int` |
| `xsd:short` | `i16` | `number` | `int` | `int16` | `short` | `short` | `Short` |
| `xsd:byte` | `i8` | `number` | `int` | `int8` | `byte` | `byte` | `Byte` |
| `xsd:unsignedInt` | `u32` | `number` | `int` | `uint32` | `int` | `uint` | `UInt` |
| `xsd:unsignedLong` | `u64` | `number` | `int` | `uint64` | `long` | `ulong` | `ULong` |
| `xsd:decimal` | `Decimal` or `f64` | `number` | `Decimal` | `float64` | `BigDecimal` | `decimal` | `BigDecimal` |
| `xsd:double` | `f64` | `number` | `float` | `float64` | `double` | `double` | `Double` |
| `xsd:float` | `f32` | `number` | `float` | `float32` | `float` | `float` | `Float` |

**Precision Handling**:
```
xsd:decimal  (financial calculations)
  → Rust: Use rust_decimal crate for precision
  → TypeScript: Use decimal.js for precision
  → Python: decimal.Decimal (built-in)
  → Go: big.Float for arbitrary precision
  → Java: java.math.BigDecimal
  → C#: decimal (native fixed-point)
```

**Constraint Examples**:
```xml
<xsd:restriction base="xsd:integer">
  <xsd:minInclusive value="0"/>
  <xsd:maxInclusive value="100"/>
</xsd:restriction>
```

Generated code across languages:
```rust
// Rust
#[derive(Debug, Clone, Copy)]
pub struct Percentage(u8);

impl Percentage {
    pub fn new(value: u8) -> Result<Self, ValidationError> {
        if value > 100 {
            Err(ValidationError::PercentageOutOfRange)
        } else {
            Ok(Percentage(value))
        }
    }
}
```

```typescript
// TypeScript
export class Percentage {
    private readonly value: number;

    constructor(value: number) {
        if (value < 0 || value > 100) {
            throw new Error("Percentage must be 0-100");
        }
        this.value = value;
    }

    get(): number {
        return this.value;
    }
}
```

```python
# Python
from pydantic import BaseModel, Field

class Percentage(BaseModel):
    value: int = Field(..., ge=0, le=100)
```

```go
// Go
type Percentage int

func NewPercentage(value int) (Percentage, error) {
    if value < 0 || value > 100 {
        return 0, errors.New("percentage out of range")
    }
    return Percentage(value), nil
}
```

#### 3. Boolean Type

| XSD Type | Rust | TypeScript | Python | Go | Java | C# | Kotlin |
|----------|------|-----------|--------|----|----|----|----|
| `xsd:boolean` | `bool` | `boolean` | `bool` | `bool` | `boolean` | `bool` | `Boolean` |

#### 4. Date & Time Types

| XSD Type | Rust | TypeScript | Python | Go | Java | C# | Kotlin |
|----------|------|-----------|--------|----|----|----|----|
| `xsd:dateTime` | `DateTime<Utc>` | `Date` | `datetime` | `time.Time` | `Instant` | `DateTime` | `Instant` |
| `xsd:date` | `NaiveDate` | `Date` | `date` | `time.Time` | `LocalDate` | `DateTime` | `LocalDate` |
| `xsd:time` | `NaiveTime` | `string` | `time` | `time.Time` | `LocalTime` | `TimeSpan` | `LocalTime` |
| `xsd:duration` | `Duration` | `string` | `timedelta` | `time.Duration` | `Duration` | `TimeSpan` | `Duration` |

**DateTime Handling**:
```xml
<xsd:restriction base="xsd:dateTime">
  <xsd:pattern value="\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z"/>
</xsd:restriction>
```

Generated code:
```rust
// Rust - Always UTC
use chrono::{DateTime, Utc};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Event {
    pub occurred_at: DateTime<Utc>,  // Always timezone-aware
}
```

```typescript
// TypeScript - ISO 8601
export interface Event {
    occurred_at: string;  // "2025-11-17T14:30:00Z"
}

// Validation
function validateTimestamp(ts: string): boolean {
    return /\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z/.test(ts);
}
```

```python
# Python - Use timezone-aware datetime
from datetime import datetime, timezone

class Event(BaseModel):
    occurred_at: datetime  # UTC assumed, enforced via validator

    @validator('occurred_at', pre=True)
    def ensure_utc(cls, v):
        if isinstance(v, str):
            v = datetime.fromisoformat(v.replace('Z', '+00:00'))
        return v.replace(tzinfo=timezone.utc)
```

#### 5. Other Primitive Types

| XSD Type | Rust | TypeScript | Python | Go | Java | C# | Kotlin |
|----------|------|-----------|--------|----|----|----|----|
| `xsd:hexBinary` | `Vec<u8>` | `Uint8Array` | `bytes` | `[]byte` | `byte[]` | `byte[]` | `ByteArray` |
| `xsd:base64Binary` | `Vec<u8>` | `string` (base64) | `bytes` | `[]byte` | `byte[]` | `byte[]` | `ByteArray` |
| `xsd:anyURI` | `Url` | `string` (URL) | `str` | `string` | `URI` | `Uri` | `URI` |
| `xsd:QName` | `String` | `string` | `str` | `string` | `String` | `string` | `String` |
| `xsd:UUID` | `Uuid` | `string` | `UUID` | `string` | `UUID` | `Guid` | `UUID` |

---

## Composite Types

### Struct (Record) Projections

#### Rust Struct Projection

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    pub id: uuid::Uuid,
    pub name: String,
    pub email: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub phone: Option<String>,
    pub created_at: chrono::DateTime<chrono::Utc>,
}

// Automatically generated impl blocks
impl User {
    pub fn new(id: uuid::Uuid, name: String, email: String) -> Self { ... }
    pub fn validate(&self) -> Result<(), ValidationError> { ... }
}
```

#### TypeScript Interface Projection

```typescript
export interface User {
    id: string;  // UUID
    name: string;
    email: string;
    phone?: string;  // Optional
    created_at: string;  // ISO 8601
}

export class UserValidator {
    static validate(user: User): ValidationError[] {
        const errors: ValidationError[] = [];
        if (!user.name) errors.push({ field: 'name', message: 'Required' });
        if (!this.isValidEmail(user.email)) errors.push({ field: 'email', message: 'Invalid format' });
        return errors;
    }

    private static isValidEmail(email: string): boolean {
        return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email);
    }
}
```

#### Python Dataclass Projection

```python
from dataclasses import dataclass
from datetime import datetime
from typing import Optional
from uuid import UUID
from pydantic import EmailStr, Field

@dataclass
class User:
    id: UUID
    name: str = Field(..., min_length=1)
    email: EmailStr
    phone: Optional[str] = None
    created_at: datetime

    def validate(self) -> None:
        """Validate user constraints"""
        if not self.name:
            raise ValueError("Name required")
        # Additional validation
```

#### Go Struct Projection

```go
type User struct {
    ID        uuid.UUID `json:"id"`
    Name      string    `json:"name" validate:"required,min=1"`
    Email     string    `json:"email" validate:"required,email"`
    Phone     *string   `json:"phone,omitempty"`
    CreatedAt time.Time `json:"created_at"`
}

func (u *User) Validate() error {
    if u.Name == "" {
        return fmt.Errorf("name required")
    }
    if !isValidEmail(u.Email) {
        return fmt.Errorf("invalid email")
    }
    return nil
}
```

#### Java Class Projection

```java
@Data
@Builder
@Validated
public class User {
    @NotNull
    private UUID id;

    @NotBlank
    private String name;

    @Email
    private String email;

    @Nullable
    private String phone;

    @NotNull
    private Instant createdAt;

    public void validate() throws ValidationException {
        // Generated validation logic
    }
}
```

### Enum Projections

#### Rust Enum

```rust
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Status {
    Active,
    Inactive,
    Suspended,
}

impl Status {
    pub fn as_str(&self) -> &'static str {
        match self {
            Status::Active => "active",
            Status::Inactive => "inactive",
            Status::Suspended => "suspended",
        }
    }

    pub fn from_str(s: &str) -> Result<Self, ParseError> {
        match s {
            "active" => Ok(Status::Active),
            "inactive" => Ok(Status::Inactive),
            "suspended" => Ok(Status::Suspended),
            _ => Err(ParseError::UnknownStatus),
        }
    }
}
```

#### TypeScript Enum

```typescript
export const Status = {
    ACTIVE: "active",
    INACTIVE: "inactive",
    SUSPENDED: "suspended",
} as const;

export type Status = typeof Status[keyof typeof Status];

export class StatusValidator {
    static isValid(status: unknown): status is Status {
        return Object.values(Status).includes(status as any);
    }
}
```

#### Python Enum

```python
from enum import Enum

class Status(str, Enum):
    ACTIVE = "active"
    INACTIVE = "inactive"
    SUSPENDED = "suspended"

    @classmethod
    def from_string(cls, value: str) -> "Status":
        try:
            return cls(value)
        except ValueError:
            raise ValueError(f"Invalid status: {value}")
```

---

## Generic Types

### List/Array Types

```
ggen:type: xsd:string
ggen:cardinality: 0..*

Maps to:
  Rust:       Vec<String>
  TypeScript: string[]
  Python:     List[str]
  Go:         []string
  Java:       List<String>
```

### Optional/Nullable Types

```
ggen:isRequired: false

Maps to:
  Rust:       Option<T>
  TypeScript: T | undefined / T | null
  Python:     Optional[T] / T | None
  Go:         *T or pointer
  Java:       @Nullable T or Optional<T>
```

### Map/Dictionary Types

```
ggen:type: Map<String, Integer>

Maps to:
  Rust:       HashMap<String, i32>
  TypeScript: Record<string, number>
  Python:     Dict[str, int]
  Go:         map[string]int
  Java:       Map<String, Integer>
```

### Tuple Types

```
ggen:tuple: [String, Integer, Boolean]

Maps to:
  Rust:       (String, i32, bool)
  TypeScript: [string, number, boolean]
  Python:     Tuple[str, int, bool]
  Go:         struct { Field1 string; Field2 int; Field3 bool }
  Java:       record Tuple(String field1, Integer field2, Boolean field3)
```

---

## Type Constraints & Validation

### Constraint Types in SPARQL/SHACL

```sparql
# Range constraints
?type sh:minInclusive 0;
      sh:maxInclusive 100.

# String pattern
?type sh:pattern "[A-Z][a-z]+".

# Enum values
?type sh:in ("active", "inactive", "suspended").

# Cardinality
?type sh:minCount 1;
       sh:maxCount 1.

# Complex validation
?type sh:sparql [
    sh:message "Password must be >8 chars" ;
    sh:select """
        SELECT $this
        WHERE {
            BIND(STRLEN(str($this)) AS ?len)
            FILTER(?len < 8)
        }
    """
].
```

### Validation Code Generation

For the constraint:
```sparql
?name sh:minLength 1 ;
      sh:maxLength 50 ;
      sh:pattern "[a-zA-Z ]+".
```

#### Rust Validator

```rust
#[derive(Debug, Clone)]
pub struct Name(String);

impl Name {
    pub fn new(value: String) -> Result<Self, NameError> {
        if value.is_empty() {
            return Err(NameError::TooShort);
        }
        if value.len() > 50 {
            return Err(NameError::TooLong);
        }
        if !regex::Regex::new("[a-zA-Z ]+")
            .unwrap()
            .is_match(&value)
        {
            return Err(NameError::InvalidPattern);
        }
        Ok(Name(value))
    }

    pub fn get(&self) -> &str {
        &self.0
    }
}
```

#### TypeScript Validator

```typescript
export class NameValidator {
    private static readonly MIN_LENGTH = 1;
    private static readonly MAX_LENGTH = 50;
    private static readonly PATTERN = /^[a-zA-Z ]+$/;

    static validate(value: string): ValidationError | null {
        if (value.length < this.MIN_LENGTH) {
            return { field: 'name', message: 'Too short' };
        }
        if (value.length > this.MAX_LENGTH) {
            return { field: 'name', message: 'Too long' };
        }
        if (!this.PATTERN.test(value)) {
            return { field: 'name', message: 'Invalid pattern' };
        }
        return null;
    }
}

export class Name {
    private readonly value: string;

    constructor(value: string) {
        const error = NameValidator.validate(value);
        if (error) throw new Error(error.message);
        this.value = value;
    }

    getValue(): string {
        return this.value;
    }
}
```

#### Python Validator

```python
from pydantic import BaseModel, Field, validator
import re

class Name(BaseModel):
    value: str = Field(
        ...,
        min_length=1,
        max_length=50,
        regex="^[a-zA-Z ]+$"
    )

    @validator('value')
    def validate_pattern(cls, v):
        if not re.match(r'^[a-zA-Z ]+$', v):
            raise ValueError('Invalid pattern')
        return v
```

---

## Language-Specific Mappings

### Complete Cross-Language Type Table

| Concept | Rust | TS | Python | Go | Java | C# | Kotlin |
|---------|------|----|----|----|----|----|----|
| String | String | string | str | string | String | string | String |
| Integer | i64 | number | int | int64 | long | long | Long |
| Float | f64 | number | float | float64 | double | double | Double |
| Boolean | bool | boolean | bool | bool | boolean | bool | Boolean |
| List | Vec<T> | T[] | List[T] | []T | List<T> | List<T> | List<T> |
| Map | HashMap | Record | Dict | map | Map | Dictionary | Map |
| Optional | Option<T> | T\|null | Optional[T] | *T | Optional<T> | T? | T? |
| UUID | Uuid | string | UUID | string | UUID | Guid | UUID |
| DateTime | DateTime<Utc> | Date | datetime | time.Time | Instant | DateTime | Instant |
| Result | Result<T,E> | {success, error} | Exception | error | Exception | Exception | Exception |

---

## Cross-Language Type Equivalence

### Principle: Semantic Equivalence, not Syntactic Equivalence

```
ggen:Type "User" contains:
  - ggen:Field "name" (xsd:string, required)
  - ggen:Field "email" (xsd:string, email constraint)
  - ggen:Field "age" (xsd:integer, min 0, max 150)
  - ggen:Field "created_at" (xsd:dateTime)

Equivalence across languages:
  Rust:
    struct User {
        name: String,          ✓ matches
        email: String,         ✓ matches (constraint separate)
        age: u8,               ✓ matches (0-150 range)
        created_at: DateTime,  ✓ matches
    }

  TypeScript:
    interface User {
        name: string;          ✓ matches
        email: string;         ✓ matches
        age: number;           ✓ matches (0-150 enforced at runtime)
        created_at: string;    ✓ matches (ISO 8601)
    }

  Python:
    class User(BaseModel):
        name: str              ✓ matches
        email: EmailStr        ✓ matches
        age: int = Field(ge=0, le=150)  ✓ matches
        created_at: datetime   ✓ matches

  Go:
    type User struct {
        Name      string    `json:"name"`
        Email     string    `json:"email"`
        Age       uint8     `json:"age"`
        CreatedAt time.Time `json:"created_at"`
    }  ✓ matches
```

### Equivalence Testing

Generated code includes cross-language equivalence tests:

```rust
// tests/cross_language_equivalence.rs

#[test]
fn test_user_type_equivalence() {
    // Test data
    let user_json = r#"
    {
        "name": "Alice",
        "email": "alice@example.com",
        "age": 30,
        "created_at": "2025-11-17T14:30:00Z"
    }
    "#;

    // Parse in Rust
    let rust_user: User = serde_json::from_str(user_json).unwrap();
    assert_eq!(rust_user.name, "Alice");

    // Same JSON should parse identically in TypeScript, Python, Go, Java
    // (Verified by runtime tests)
}
```

---

## Type Serialization

### Serialization Format: JSON Lines

All types serialize to/from JSON for language interoperability.

#### Struct Serialization

```rust
#[derive(Serialize, Deserialize)]
pub struct User {
    pub id: Uuid,
    pub name: String,
    pub email: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub phone: Option<String>,
    pub created_at: DateTime<Utc>,
}

// To JSON:
// {"id":"550e8400-e29b-41d4-a716-446655440000","name":"Alice","email":"alice@example.com","created_at":"2025-11-17T14:30:00Z"}

// From JSON:
let user = serde_json::from_str(json_str)?;
```

#### Enum Serialization

```rust
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Status {
    Active,
    Inactive,
    Suspended,
}

// Serializes to: "active", "inactive", "suspended"
```

#### Custom Serialization

```rust
#[derive(Serialize, Deserialize)]
pub struct Price {
    #[serde(serialize_with = "serialize_price", deserialize_with = "deserialize_price")]
    amount: Decimal,
}

fn serialize_price<S>(amount: &Decimal, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    // Always serialize with 2 decimal places
    serializer.serialize_str(&format!("{:.2}", amount))
}

fn deserialize_price<'de, D>(deserializer: D) -> Result<Decimal, D::Error>
where
    D: Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    Decimal::from_str(&s).map_err(serde::de::Error::custom)
}
```

---

## Advanced Patterns

### Newtype Pattern

For semantic distinction (User ID vs Order ID, both i64):

```rust
// Ontology definition:
// ggen:newtype "UserId" base "xsd:integer"
// ggen:newtype "OrderId" base "xsd:integer"

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct UserId(i64);

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct OrderId(i64);

// Prevents accidental mixing:
// let user_id = UserId(123);
// let order_id: OrderId = user_id;  // Compile error!
```

### Builder Pattern

For complex types with many optional fields:

```rust
#[derive(Default)]
pub struct ProjectBuilder {
    name: Option<String>,
    path: Option<PathBuf>,
    ontology: Option<String>,
    templates_dir: Option<PathBuf>,
    output_dir: Option<PathBuf>,
}

impl ProjectBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn name(mut self, name: String) -> Self {
        self.name = Some(name);
        self
    }

    pub fn build(self) -> Result<Project, BuildError> {
        Ok(Project {
            name: self.name.ok_or(BuildError::MissingName)?,
            path: self.path.ok_or(BuildError::MissingPath)?,
            // ... rest of fields
        })
    }
}

// Usage:
let project = ProjectBuilder::new()
    .name("my-project".to_string())
    .path(PathBuf::from("."))
    .build()?;
```

### Sum Types / Tagged Unions

```rust
// Ontology: ggen:union "CommandResult" [Success, Error]

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
pub enum CommandResult {
    Success(String),      // Success message
    Error(String),        // Error message
    Partial(Box<Partial>) // Partial result
}

// Serializes to:
// {"type":"Success","value":"Operation completed"}
// {"type":"Error","value":"File not found"}
```

### Type Coercion & Compatibility

```rust
// Implicit coercions (bidirectional):
impl From<&str> for Name {
    fn from(s: &str) -> Self {
        Name(s.to_string())
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

// Allows:
let name: Name = "Alice".into();
let s: &str = name.as_ref();
```

---

## Type System Completeness Verification

### Generated Tests

For each type, ggen generates:

```rust
#[cfg(test)]
mod type_completeness {
    use super::*;

    #[test]
    fn test_user_serialization() {
        let user = User { /* ... */ };
        let json = serde_json::to_string(&user).unwrap();
        let user2: User = serde_json::from_str(&json).unwrap();
        assert_eq!(user, user2);
    }

    #[test]
    fn test_user_validation() {
        // Invalid email
        assert!(User::validate_email("not-an-email").is_err());

        // Valid email
        assert!(User::validate_email("user@example.com").is_ok());
    }

    #[test]
    fn test_user_constraint_adherence() {
        // Test all defined constraints
        let user = User {
            name: String::from("A".repeat(51)), // Exceeds max_length
            // ...
        };

        let result = user.validate();
        assert!(matches!(result, Err(ValidationError::NameTooLong)));
    }
}
```

### Cross-Language Verification

For each type, ggen generates equivalence tests:

```bash
# Run all type tests
cargo test type_completeness
pytest test_completeness.py
npm test completeness
go test ./completeness
mvn test -Dtest=CompletenessSuite
```

---

## Conclusion

ggen's type system ensures that:

1. **Semantic Equivalence**: Same meaning across languages
2. **Constraint Consistency**: Validation identical in all languages
3. **Serialization Compatibility**: JSON interchange format guaranteed
4. **Zero Drift**: Single ontology = identical behavior everywhere
5. **Language Idioms**: Each language uses native idioms (not translations)

This enables true polyglot code generation where types are defined once and used everywhere.

---

**Document Version**: 1.0
**Created**: November 17, 2025
**Branch**: `claude/plan-ggen-v3-rewrite-01PyJAjvvvwdVWwD6wickodF`
**Next**: Sector Bundles Catalog and Security Analysis
