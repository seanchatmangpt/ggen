# Multi-Language Projects: Synchronized Types Across Rust, TypeScript, and Python

Generate type-safe code across multiple programming languages from a single ontology, eliminating manual synchronization and type drift.

## Goal

Create a project with synchronized type definitions in Rust, TypeScript, and Python - all generated from one ontology source.

## Prerequisites

- ggen installed
- Completed [Your First CLI Command](05-first-cli-command.md)
- Basic familiarity with Rust, TypeScript, and Python
- Node.js and Python 3.8+ installed

## Step 1: Define Your Multi-Language Ontology

Create `shared-domain.ttl`:

```turtle
@prefix ex: <http://example.org/domain/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# User entity shared across all services
ex:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "A user account" .

ex:id a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:comment "Unique user identifier" .

ex:email a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:comment "User email address" .

ex:username a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:comment "Username for login" .

ex:isActive a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:boolean ;
  rdfs:comment "Whether user account is active" .

ex:createdAt a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:dateTime ;
  rdfs:comment "Account creation timestamp" .

ex:metadata a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:comment "User metadata JSON" .
```

## Step 2: Create Project Structure

```bash
mkdir -p polyglot-app/{rust,typescript,python}
cd polyglot-app
```

## Step 3: Generate Rust Types

Generate Rust models with strong type safety:

```bash
ggen ontology generate shared-domain.ttl \
  --language rust \
  --namespace "::domain" \
  --output rust/src/models.rs
```

Generated `rust/src/models.rs`:

```rust
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct User {
    pub id: String,
    pub email: String,
    pub username: String,
    pub is_active: bool,
    pub created_at: DateTime<Utc>,
    pub metadata: Option<String>,
}

impl User {
    pub fn new(
        id: String,
        email: String,
        username: String,
    ) -> Self {
        Self {
            id,
            email,
            username,
            is_active: true,
            created_at: Utc::now(),
            metadata: None,
        }
    }

    pub fn is_valid(&self) -> bool {
        !self.email.is_empty() &&
        !self.username.is_empty() &&
        self.username.len() >= 3
    }
}
```

## Step 4: Generate TypeScript Types

Generate TypeScript interfaces for frontend/Node.js:

```bash
ggen ontology generate shared-domain.ttl \
  --language typescript \
  --output typescript/src/models.ts
```

Generated `typescript/src/models.ts`:

```typescript
export interface User {
  id: string;
  email: string;
  username: string;
  isActive: boolean;
  createdAt: Date;
  metadata?: string;
}

export class UserModel implements User {
  id: string;
  email: string;
  username: string;
  isActive: boolean;
  createdAt: Date;
  metadata?: string;

  constructor(data: Partial<User>) {
    Object.assign(this, {
      isActive: true,
      createdAt: new Date(),
      ...data,
    });
  }

  isValid(): boolean {
    return (
      this.email.length > 0 &&
      this.username.length >= 3
    );
  }
}
```

## Step 5: Generate Python Types

Generate Python dataclasses with Pydantic validation:

```bash
ggen ontology generate shared-domain.ttl \
  --language python \
  --output python/models.py
```

Generated `python/models.py`:

```python
from datetime import datetime
from typing import Optional

from pydantic import BaseModel, EmailStr, Field, validator


class User(BaseModel):
    id: str
    email: EmailStr
    username: str
    is_active: bool = True
    created_at: datetime = Field(default_factory=datetime.utcnow)
    metadata: Optional[str] = None

    @validator('username')
    def username_valid_length(cls, v):
        if len(v) < 3:
            raise ValueError('Username must be at least 3 characters')
        return v

    @validator('email')
    def email_valid(cls, v):
        if not v or '@' not in v:
            raise ValueError('Invalid email address')
        return v

    def is_valid(self) -> bool:
        try:
            self.validate(self.__dict__)
            return True
        except ValueError:
            return False
```

## Step 6: Create Synchronized Services

**Rust backend service** (`rust/src/main.rs`):

```rust
use crate::models::User;

fn main() {
    let user = User::new(
        "u123".to_string(),
        "alice@example.com".to_string(),
        "alice".to_string(),
    );

    println!("User: {} ({})", user.username, user.email);
    println!("Valid: {}", user.is_valid());
}
```

**TypeScript API handler** (`typescript/src/handlers.ts`):

```typescript
import { UserModel } from './models';

export async function createUser(data: Partial<User>) {
  const user = new UserModel(data);

  if (!user.isValid()) {
    throw new Error('Invalid user data');
  }

  // Send to backend API
  const response = await fetch('/api/users', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(user),
  });

  return response.json();
}
```

**Python data processor** (`python/processor.py`):

```python
from models import User
from datetime import datetime

def process_user(data: dict) -> User:
    """Create and validate user from data."""
    user = User(**data)

    if not user.is_valid():
        raise ValueError("User validation failed")

    print(f"Processed user: {user.username}")
    return user

# Usage
user_data = {
    "id": "u456",
    "email": "bob@example.com",
    "username": "bob",
}

user = process_user(user_data)
print(f"Created: {user.created_at}")
```

## Step 7: Share Types Across Services

**Type consistency verification**:

```bash
# All languages should accept the same data
echo '{
  "id": "u789",
  "email": "charlie@example.com",
  "username": "charlie",
  "isActive": true,
  "createdAt": "2024-01-01T00:00:00Z"
}' | \
  tee >(cargo run --manifest-path rust/Cargo.toml) \
      >(node typescript/dist/validate.js) \
      >(python python/validate.py)
```

All three should validate successfully - **no drift!**

## Step 8: Keep Types Synchronized

When you update the ontology, regenerate all languages:

```bash
# Update shared-domain.ttl with new field

# Regenerate all languages
ggen ontology generate shared-domain.ttl --language rust --output rust/src/models.rs
ggen ontology generate shared-domain.ttl --language typescript --output typescript/src/models.ts
ggen ontology generate shared-domain.ttl --language python --output python/models.py

# Commit synchronized changes
git add rust/src/models.rs typescript/src/models.ts python/models.py
git commit -m "chore: sync models from shared ontology"
```

## Benefits of This Approach

**1. Single Source of Truth**: One ontology, multiple languages

**2. Type Safety**: Compile-time checking in Rust, TypeScript, Python

**3. Zero Drift**: Generated code always matches across languages

**4. Maintainability**: Update types in one place (the ontology)

**5. Consistency**: All services use identical validation logic

**6. Reproducibility**: Same ontology = identical generated code every time

## Best Practices

**1. Version your ontology**:
```bash
git tag v1.0.0-domain
```

**2. Document breaking changes**:
```turtle
ex:Field a rdf:Property ;
  rdfs:comment "Breaking change in v2.0.0: type changed from string to integer" .
```

**3. Keep languages in sync**:
```bash
# Pre-commit hook
ggen ontology generate shared-domain.ttl --language rust --output rust/src/models.rs
ggen ontology generate shared-domain.ttl --language typescript --output typescript/src/models.ts
ggen ontology generate shared-domain.ttl --language python --output python/models.py
```

## Next Steps

- Integrate into CI/CD pipelines
- [CI/CD Integration](08-ci-cd-integration.md)
- Create API contracts from the same ontology
- Use for cross-platform mobile apps
- [Advanced patterns](../reference/templates.md)

## Summary

You've learned:
- ✅ How to design ontologies for polyglot projects
- ✅ How to generate synchronized types across 3+ languages
- ✅ How to maintain type consistency automatically
- ✅ How to prevent type drift in distributed systems
- ✅ Best practices for multi-language codebases

Your polyglot projects are now type-safe and drift-free!
