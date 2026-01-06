<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Collaboration Model: Ontologies as Team Contracts](#collaboration-model-ontologies-as-team-contracts)
  - [Shared Ontology as Contract](#shared-ontology-as-contract)
    - [Traditional Model: Code Contracts](#traditional-model-code-contracts)
    - [Ontology-Driven Model: Semantic Contract](#ontology-driven-model-semantic-contract)
  - [Workflow: Making Changes to the Domain Model](#workflow-making-changes-to-the-domain-model)
    - [Step 1: Team Proposes Change](#step-1-team-proposes-change)
    - [Step 2: Review and Validate](#step-2-review-and-validate)
    - [Step 3: Regenerate All Implementations](#step-3-regenerate-all-implementations)
    - [Step 4: Each Team Implements Support](#step-4-each-team-implements-support)
    - [Step 5: Integration Testing](#step-5-integration-testing)
    - [Step 6: Deploy](#step-6-deploy)
  - [Roles and Responsibilities](#roles-and-responsibilities)
    - [Domain Architect](#domain-architect)
    - [Front-end Team](#front-end-team)
    - [Back-end Team](#back-end-team)
    - [Database Team](#database-team)
    - [QA/Testing Team](#qatesting-team)
  - [Communication Flow](#communication-flow)
    - [Team Standup](#team-standup)
    - [Conflict Resolution](#conflict-resolution)
  - [Version Control Strategy](#version-control-strategy)
    - [Branch Policy](#branch-policy)
    - [Release Coordination](#release-coordination)
    - [Backwards Compatibility](#backwards-compatibility)
  - [Scaling: Multiple Teams](#scaling-multiple-teams)
    - [Small Team (3 people)](#small-team-3-people)
    - [Medium Team (10 people)](#medium-team-10-people)
    - [Large Team (50+ people)](#large-team-50-people)
  - [Anti-Patterns to Avoid](#anti-patterns-to-avoid)
    - [❌ Creating Local Copies](#-creating-local-copies)
    - [❌ Manual Type Definitions](#-manual-type-definitions)
    - [❌ Skipping Reviews](#-skipping-reviews)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Collaboration Model: Ontologies as Team Contracts

How teams collaborate using shared ontologies.

## Shared Ontology as Contract

### Traditional Model: Code Contracts

Each team implements their part independently:

```
Frontend Team          Backend Team           Database Team
├─ models/User.ts     ├─ models/User.go      ├─ schema.sql
├─ models/Product.ts  ├─ models/Product.go   └─ migrations/
└─ api/client.ts      └─ api/handlers.go
```

**Problem**: Mismatched definitions

```
Frontend:  User { id: "user-123", email: "test@example.com" }
Backend:   User { userId: 123, emailAddress: "test@example.com", ...}
Database:  users { id, email, full_name, created_at, ... }
```

→ Integration pain, type mismatches, wasted time

### Ontology-Driven Model: Semantic Contract

Single source of truth - the ontology:

```
Shared Ontology (project.ttl)
    ↓ (Regenerate all implementations)
┌───┴──────┬──────────┬──────────┐
Frontend   Backend    Database   AI/ML
(TypeScript) (Rust) (Schema)    (PyDantic)
```

**Result**: Guaranteed consistency

All teams use the same `User` definition:
```turtle
app:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "A user account" .

app:id a rdf:Property ;
  rdfs:domain app:User ;
  rdfs:range xsd:string ;
  rdfs:label "User ID" .

app:email a rdf:Property ;
  rdfs:domain app:User ;
  rdfs:range xsd:string ;
  rdfs:label "Email" .
```

Generated in all languages from one source.

## Workflow: Making Changes to the Domain Model

### Step 1: Team Proposes Change

Product team identifies a new requirement:
- User accounts need phone numbers
- Product descriptions need rich text

**Create a feature branch**:
```bash
git checkout -b feature/add-phone-to-user
```

**Edit the ontology**:
```turtle
app:phone a rdf:Property ;
  rdfs:domain app:User ;
  rdfs:range xsd:string ;
  rdfs:label "Phone Number" ;
  rdfs:comment "User phone number (optional)" .
```

### Step 2: Review and Validate

**Validate the change**:
```bash
ggen ontology validate project.ttl
# Checks: syntax, completeness, consistency
```

**Get approval**:
- Open pull request: "Add phone field to User"
- Include ontology changes
- Add one sentence explaining why

### Step 3: Regenerate All Implementations

**CI/CD automatically regenerates**:

```yaml
# GitHub Actions workflow
on:
  push:
    paths: ['project.ttl']

jobs:
  generate:
    steps:
      - name: Generate TypeScript
        run: ggen ontology generate project.ttl --language typescript
      - name: Generate Rust
        run: ggen ontology generate project.ttl --language rust
      - name: Generate Python
        run: ggen ontology generate project.ttl --language python
      - name: Generate Database Schema
        run: ggen ontology generate project.ttl --language sql
```

**Result**: All implementations updated automatically

### Step 4: Each Team Implements Support

**Frontend** (TypeScript):
```typescript
// Auto-generated from ontology
export interface User {
  id: string;
  email: string;
  phone?: string;  // New field
}

// Frontend team adds UI
export function UserForm() {
  const [phone, setPhone] = useState('');
  return <input value={phone} onChange={e => setPhone(e.target.value)} />;
}
```

**Backend** (Rust):
```rust
// Auto-generated from ontology
pub struct User {
    pub id: String,
    pub email: String,
    pub phone: Option<String>,  // New field
}

// Backend team adds validation and API
#[post("/users")]
pub fn create_user(user: Json<User>) -> Result<Json<User>> {
    validate_phone(&user.phone)?;
    save_user(&user)?;
    Ok(Json(user))
}
```

**Database** (SQL):
```sql
-- Auto-generated from ontology
ALTER TABLE users ADD COLUMN phone VARCHAR(20);

-- Database team creates index
CREATE INDEX idx_phone ON users(phone);
```

### Step 5: Integration Testing

**All teams test together**:
```bash
# End-to-end test with new field
1. Frontend sends POST /users with phone
2. Backend validates and saves
3. Database stores phone
4. Query returns phone
5. Frontend displays phone
```

**No surprises** - all implementations match because they're generated from same ontology.

### Step 6: Deploy

**All changes go out together**:
```
Time ---→
[Ontology change merged]
      ↓
[Frontend deployed]
[Backend deployed]
[Database migrated]
      ↓
[Users can use phone field]
```

No staggered deployments, no "this service doesn't have phone yet" confusion.

## Roles and Responsibilities

### Domain Architect

**Role**: Maintains the ontology

**Responsibilities**:
- Understands all domain requirements
- Designs clean ontology structure
- Reviews all ontology changes
- Ensures consistency across teams

**Skills needed**:
- RDF/OWL knowledge
- Domain expertise
- SQL/API design
- Communication

### Front-end Team

**Role**: Implements UI using generated types

**Responsibilities**:
- Builds UI components using generated types
- Doesn't manually define data models
- Reports needed model changes
- Tests with generated types

**Guarantee**: Types match backend exactly

### Back-end Team

**Role**: Implements business logic using generated types

**Responsibilities**:
- Builds APIs using generated types
- Doesn't manually define data models
- Reports needed model changes
- Tests integration

**Guarantee**: Types match database schema exactly

### Database Team

**Role**: Generates and maintains schema from ontology

**Responsibilities**:
- Generates SQL schema from ontology
- Creates migrations
- Performance tuning
- Backup/recovery

**Guarantee**: Schema matches backend types exactly

### QA/Testing Team

**Role**: Tests using generated contracts

**Responsibilities**:
- Tests API contracts from ontology
- Validates type compatibility
- End-to-end testing
- Performance testing

**Guarantee**: All implementations match ontology

## Communication Flow

### Team Standup

```
Product: "Need to add user roles"
         → Add to ontology

Frontend: "Ontology changed, regenerating..."
          → Pulls latest types

Backend: "Ontology changed, regenerating..."
         → Pulls latest types

Database: "Ontology changed, regenerating..."
          → Pulls latest schema

All teams: "Ready to implement"
```

### Conflict Resolution

**Scenario**: Frontend and Backend disagree on field name

**Frontend proposes**: `createdAt` (camelCase)
**Backend prefers**: `created_at` (snake_case)

**Solution**: Decide in ontology once, apply everywhere

```turtle
app:created_at a rdf:Property ;
  rdfs:label "Created At" ;
  owl:javaVariableName "createdAt" ;    # Frontend uses camelCase
  owl:pythonVariableName "created_at" ;  # Python uses snake_case
  owl:sqlColumnName "created_at" ;       # Database uses snake_case
```

All implementations use correct convention for their language automatically.

## Version Control Strategy

### Branch Policy

```
main (stable ontology)
  ↑
  └─ feature/add-phone (team develops here)
      ├─ Modify project.ttl
      ├─ All implementations regenerate
      ├─ Each team implements their part
      ├─ PR review by domain architect
      └─ Merge when approved
```

### Release Coordination

```
Tag: v1.0.0 (with ontology version)
  ├─ Frontend deployed
  ├─ Backend deployed
  ├─ Database migrated
  └─ Announcement: "All services support new fields"
```

### Backwards Compatibility

**Add optional field** (backwards compatible):
```turtle
app:phone a rdf:Property ;
  rdfs:domain app:User ;
  rdfs:range xsd:string ;
  rdf:comment "Optional - added in v1.1.0" .
```

**All implementations**:
- Old clients work (phone is optional)
- New clients can send phone (all services accept it)
- Zero downtime

## Scaling: Multiple Teams

### Small Team (3 people)
```
1 Architect, 1 Frontend, 1 Backend
  (All work from shared ontology)
```

### Medium Team (10 people)
```
1 Architect, 3 Frontend, 3 Backend, 2 Database, 1 QA
  (All use same ontology as contract)
```

### Large Team (50+ people)
```
1 Domain Architect, 10 Frontend, 10 Backend, 5 Database,
5 Mobile, 5 QA, 10 Data/Analytics
  (Everyone works from same source of truth)

Benefit: Even with 50 developers, everyone develops against
the same model definitions. No "version skew" problems.
```

## Anti-Patterns to Avoid

### ❌ Creating Local Copies

```
DON'T:
Frontend: cp project.ttl local-project.ttl (modify locally)
Backend: cp project.ttl local-project.ttl (modify locally)
→ Now you have 2 different ontologies!
```

```
DO:
All teams use shared project.ttl
Feature branches for changes
Reviews before merge
CI/CD handles regeneration
```

### ❌ Manual Type Definitions

```
DON'T:
type User = { id: string; email: string }  // Manual
type User = { userId: i32; emailAddress: String }  // Different!
```

```
DO:
Define User once in ontology
Regenerate all languages
All get the same definition
```

### ❌ Skipping Reviews

```
DON'T:
Update ontology, push immediately
All teams scrambling to update code
Deployment chaos
```

```
DO:
Propose change in PR
Domain architect reviews
Discusses with affected teams
Merge with consensus
```

## Summary

Ontology-based collaboration enables:
- ✅ **Shared contracts**: One definition for all teams
- ✅ **Type consistency**: Automatic sync across languages
- ✅ **Clear ownership**: Architect owns ontology
- ✅ **Predictable integration**: No surprises
- ✅ **Scalability**: Works from 3 to 300+ developers
- ✅ **Agility**: Changes roll out together
- ✅ **Quality**: All implementations match definition

Your team is now ontology-driven!
