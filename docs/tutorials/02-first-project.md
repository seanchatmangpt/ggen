<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Tutorial: Your First REST API Project](#tutorial-your-first-rest-api-project)
  - [What You'll Build](#what-youll-build)
  - [Prerequisites](#prerequisites)
  - [Step 1: Create Project Structure (2 minutes)](#step-1-create-project-structure-2-minutes)
  - [Step 2: Design Your Data Model (5 minutes)](#step-2-design-your-data-model-5-minutes)
  - [Step 3: Create Tera Templates (5 minutes)](#step-3-create-tera-templates-5-minutes)
  - [Step 4: Configure Generation (2 minutes)](#step-4-configure-generation-2-minutes)
  - [Step 5: Preview Generation (2 minutes)](#step-5-preview-generation-2-minutes)
  - [Step 6: Generate Code (1 minute)](#step-6-generate-code-1-minute)
  - [Step 7: Build Your Implementation (3 minutes)](#step-7-build-your-implementation-3-minutes)
  - [Step 8: View Generation Receipt (1 minute)](#step-8-view-generation-receipt-1-minute)
  - [Step 9: Iterate - Add a New Endpoint (3 minutes)](#step-9-iterate---add-a-new-endpoint-3-minutes)
  - [Step 10: Test Full Workflow (1 minute)](#step-10-test-full-workflow-1-minute)
  - [Complete Project Structure](#complete-project-structure)
  - [Key Patterns Learned](#key-patterns-learned)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Tutorial: Your First REST API Project

**Build a real REST API using ggen in 20 minutes.**

This tutorial shows you how to generate a complete REST API with endpoints, database models, and documentation.

## What You'll Build

A simple **User Management REST API** with:
- 📝 GET `/users` - List all users
- ➕ POST `/users` - Create a user
- 🔍 GET `/users/{id}` - Get a specific user
- ✏️ PATCH `/users/{id}` - Update a user
- 🗑️ DELETE `/users/{id}` - Delete a user

## Prerequisites

- Completed [Tutorial 1: Getting Started](01-getting-started.md)
- 20 minutes
- Code editor (VS Code, vim, etc.)

## Step 1: Create Project Structure (2 minutes)

```bash
# Create new project
mkdir ggen-user-api
cd ggen-user-api

# Initialize ggen
ggen init

# Create additional directories
mkdir -p templates/{rust,database,docs}
```

## Step 2: Design Your Data Model (5 minutes)

Create `.specify/specs/001-users/users.ttl`:

```bash
cat > .specify/specs/001-users/users.ttl <<'EOF'
@prefix : <https://api.example.org/users/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# ============================================================================
# USER API SPECIFICATION
# ============================================================================

# Resource: User
:User a :ResourceType ;
  rdfs:label "User Resource" ;
  rdfs:comment "A user in the system" ;
  :fields ( :userId :name :email :createdAt ) .

# Fields of User resource
:userId a :Field ;
  rdfs:label "User ID" ;
  :type xsd:string ;
  :required true ;
  :description "Unique user identifier (UUID)" .

:name a :Field ;
  rdfs:label "Name" ;
  :type xsd:string ;
  :required true ;
  :minLength "1"^^xsd:int ;
  :maxLength "255"^^xsd:int ;
  :description "User's full name" .

:email a :Field ;
  rdfs:label "Email" ;
  :type xsd:string ;
  :required true ;
  :format "email" ;
  :description "User's email address" .

:createdAt a :Field ;
  rdfs:label "Created At" ;
  :type xsd:dateTime ;
  :required false ;
  :description "Timestamp when user was created" .

# ============================================================================
# REST ENDPOINTS
# ============================================================================

:UserAPI a :APIService ;
  rdfs:label "User Management API" ;
  :baseUrl "/api/v1/users" ;
  :endpoints ( :ListUsers :CreateUser :GetUser :UpdateUser :DeleteUser ) .

# GET /users - List all users
:ListUsers a :Endpoint ;
  rdfs:label "List Users" ;
  :method :GET ;
  :path "/users" ;
  :description "Retrieve all users" ;
  :responseType :UserList ;
  :statusCode "200"^^xsd:int .

# POST /users - Create new user
:CreateUser a :Endpoint ;
  rdfs:label "Create User" ;
  :method :POST ;
  :path "/users" ;
  :description "Create a new user" ;
  :requestBody :UserInput ;
  :responseType :User ;
  :statusCode "201"^^xsd:int .

# GET /users/{id} - Get specific user
:GetUser a :Endpoint ;
  rdfs:label "Get User" ;
  :method :GET ;
  :path "/users/{id}" ;
  :description "Retrieve a specific user by ID" ;
  :responseType :User ;
  :statusCode "200"^^xsd:int ;
  :errorCode "404"^^xsd:int .

# PATCH /users/{id} - Update user
:UpdateUser a :Endpoint ;
  rdfs:label "Update User" ;
  :method :PATCH ;
  :path "/users/{id}" ;
  :description "Update a user's information" ;
  :requestBody :UserUpdate ;
  :responseType :User ;
  :statusCode "200"^^xsd:int .

# DELETE /users/{id} - Delete user
:DeleteUser a :Endpoint ;
  rdfs:label "Delete User" ;
  :method :DELETE ;
  :path "/users/{id}" ;
  :description "Delete a user" ;
  :statusCode "204"^^xsd:int ;
  :errorCode "404"^^xsd:int .

# ============================================================================
# REQUEST/RESPONSE TYPES
# ============================================================================

:UserList a :ResponseType ;
  rdfs:label "User List Response" ;
  :items :User ;
  :pagination true .

:UserInput a :RequestType ;
  rdfs:label "User Creation Input" ;
  :fields ( :name :email ) ;
  :required true .

:UserUpdate a :RequestType ;
  rdfs:label "User Update Input" ;
  :fields ( :name :email ) ;
  :required false .

# ============================================================================
# DATABASE
# ============================================================================

:UsersTable a :DatabaseTable ;
  rdfs:label "Users Table" ;
  :tableName "users" ;
  :columns ( :id :name :email :created_at :updated_at ) .

:id a :Column ;
  :columnName "id" ;
  :type xsd:string ;
  :constraint :PrimaryKey ;
  :description "UUID primary key" .

:updated_at a :Column ;
  :columnName "updated_at" ;
  :type xsd:dateTime ;
  :nullable true .
EOF
```

**What this defines:**

| Element | Purpose |
|---------|---------|
| `:User` | Data model for a user |
| `:UserAPI` | Service definition |
| `:ListUsers`, `:CreateUser`, etc. | Endpoints |
| `:UserList`, `:UserInput` | Request/response types |
| `:UsersTable` | Database schema |

## Step 3: Create Tera Templates (5 minutes)

Create `templates/rust/handlers.rs.tera`:

```bash
cat > templates/rust/handlers.rs.tera <<'EOF'
//! Generated REST API handlers
//! Do not edit manually - regenerate with: ggen sync

use axum::{
    extract::{Path, Json},
    http::StatusCode,
    response::IntoResponse,
};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

{% for endpoint in endpoints %}
// ============================================================================
// {{ endpoint.label }}
// ============================================================================

/// {{ endpoint.description }}
///
/// - **Method**: {{ endpoint.method }}
/// - **Path**: {{ endpoint.path }}
{% if endpoint.statusCode %}
/// - **Status**: {{ endpoint.statusCode }}
{% endif %}
pub async fn handle_{{ endpoint.name | snake_case }}(
{% if endpoint.requestBody %}
    Json(payload): Json<{{ endpoint.requestBody }}>,
{% endif %}
{% if endpoint.pathParams %}
{% for param in endpoint.pathParams %}
    Path({{ param }}): Path<{{ param.type }}>,
{% endfor %}
{% endif %}
) -> impl IntoResponse {
    // TODO: Implement handler
    todo!("Implement {{ endpoint.label }}")
}
{% endfor %}
EOF
```

Create `templates/rust/models.rs.tera`:

```bash
cat > templates/rust/models.rs.tera <<'EOF'
//! Generated data models
//! Do not edit manually - regenerate with: ggen sync

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};

{% for resource in resources %}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct {{ resource.name }} {
{% for field in resource.fields %}
    /// {{ field.description }}
    pub {{ field.name }}: {{ field.rust_type }},
{% endfor %}
}
{% endfor %}
EOF
```

## Step 4: Configure Generation (2 minutes)

Edit `ggen.toml`:

```bash
cat > ggen.toml <<'EOF'
# ggen Project Configuration

[project]
name = "User Management API"
version = "0.1.0"
description = "REST API for user management"

[generation]
# Source: .specify/specs/**/*.ttl
# Output: src/generated/
output_dir = "src"

# Templates to use
templates = [
    "templates/rust/handlers.rs.tera",
    "templates/rust/models.rs.tera",
]

# Watch mode for development
watch = false

# Validation gates
validate_before_generation = true

[output]
# Format generated code
format = true

# Generate audit trails
audit = true

# Generate documentation
docs = true
EOF
```

## Step 5: Preview Generation (2 minutes)

```bash
# Dry-run to see what will be generated
ggen sync --dry_run true

# Output shows:
# - File: src/handlers.rs
# - File: src/models.rs
# - File: README.md (docs)
```

## Step 6: Generate Code (1 minute)

```bash
# Generate the actual code
ggen sync

# Check generated files
ls -la src/
cat src/handlers.rs
cat src/models.rs
```

## Step 7: Build Your Implementation (3 minutes)

Create `src/main.rs`:

```bash
cat > src/main.rs <<'EOF'
//! User Management REST API
//!
//! Generated from ontology. To modify endpoints:
//! 1. Edit .specify/specs/001-users/users.ttl
//! 2. Run: ggen sync
//! 3. This file will be regenerated

#[path = "handlers.rs"]
mod handlers;

#[path = "models.rs"]
mod models;

#[tokio::main]
async fn main() {
    // TODO: Setup Axum router with generated handlers
    println!("User Management API");
}
EOF
```

## Step 8: View Generation Receipt (1 minute)

```bash
# View the deterministic proof
cat .ggen/receipts/latest.json | jq '.'

# Shows:
# - Execution ID
# - Manifest hash (your configuration)
# - Ontology hash (.ttl files)
# - Generated files with content hashes
# - Timing (microseconds precision)
```

## Step 9: Iterate - Add a New Endpoint (3 minutes)

Edit `.specify/specs/001-users/users.ttl` and add:

```turtle
# Add to :UserAPI endpoints list:
# :endpoints ( :ListUsers :CreateUser :GetUser :UpdateUser :DeleteUser :SearchUsers )

:SearchUsers a :Endpoint ;
  rdfs:label "Search Users" ;
  :method :GET ;
  :path "/users/search" ;
  :queryParams ( :query ) ;
  :description "Search users by name or email" ;
  :responseType :UserList ;
  :statusCode "200"^^xsd:int .

:query a :QueryParam ;
  :name "query" ;
  :type xsd:string ;
  :required true ;
  :description "Search query string" .
```

Then regenerate:

```bash
# Preview
ggen sync --dry_run true

# Generate
ggen sync

# New SearchUsers handler automatically added!
cat src/handlers.rs | grep -A 10 "SearchUsers"
```

## Step 10: Test Full Workflow (1 minute)

```bash
# Make a small change to ontology
sed -i 's/User Management API/v2 - Advanced User Management API/' .specify/specs/001-users/users.ttl

# Regenerate
ggen sync

# Verify everything updated
grep -r "Advanced" src/
```

## Complete Project Structure

```
ggen-user-api/
├── .ggen/                       # Generation cache
│   ├── cache/
│   ├── receipts/latest.json    # Proof of generation
│   └── audit/                  # Audit trail
├── .specify/                    # Specifications (source of truth)
│   ├── specs/
│   │   └── 001-users/
│   │       └── users.ttl        # YOUR SINGLE SOURCE OF TRUTH
│   └── templates/
│       └── (shared templates)
├── templates/                   # Project-specific templates
│   ├── rust/
│   │   ├── handlers.rs.tera
│   │   └── models.rs.tera
│   ├── database/
│   │   └── schema.sql.tera
│   └── docs/
│       └── api.md.tera
├── src/                         # Generated code (DO NOT EDIT)
│   ├── handlers.rs              # Generated handlers
│   ├── models.rs                # Generated models
│   └── main.rs                  # Your implementation
├── ggen.toml                    # Project configuration
└── README.md                    # Generated documentation
```

## Key Patterns Learned

| Pattern | What It Does |
|---------|------------|
| **Single source of truth** | Edit `.ttl` files, rest is generated |
| **Deterministic** | Same ontology = identical code every time |
| **Iterative** | Add features to ontology, regenerate code |
| **Audit trail** | Each generation creates proof (SHA-256 hash) |
| **Reproducible** | Anyone can regenerate identical code |

## Next Steps

- 🔧 **[How-To: Add Database Schema](../how-to/02-database-generation.md)** - Generate SQL migrations
- 🔧 **[How-To: Multi-Language Generation](../how-to/03-multi-language.md)** - Generate TypeScript, Python, Go
- 📚 **[Command Reference](../reference/01-commands.md)** - All ggen commands
- 💡 **[Concepts: SPARQL Queries](../explanation/02-sparql.md)** - Advanced SPARQL patterns

---

**Congratulations! You've built your first REST API with ggen. 🎉**
