<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Python FastAPI Backend Generator Example](#python-fastapi-backend-generator-example)
  - [Overview](#overview)
  - [Prerequisites](#prerequisites)
  - [Step 1: Create RDF Ontology](#step-1-create-rdf-ontology)
  - [Step 2: Configure ggen for Python](#step-2-configure-ggen-for-python)
  - [Step 3: Create Tera Templates](#step-3-create-tera-templates)
    - [Pydantic Models Template](#pydantic-models-template)
    - [FastAPI Routes Template](#fastapi-routes-template)
    - [SQLAlchemy Models Template](#sqlalchemy-models-template)
  - [Step 4: Run Generation](#step-4-run-generation)
  - [Step 5: Create FastAPI Application](#step-5-create-fastapi-application)
  - [Step 6: Implement Endpoints](#step-6-implement-endpoints)
  - [Step 7: Test Generated Code](#step-7-test-generated-code)
  - [Multi-Language Proof](#multi-language-proof)
  - [Best Practices](#best-practices)
    - [1. Separate Generated and Manual Code](#1-separate-generated-and-manual-code)
    - [2. Regenerate When Ontology Changes](#2-regenerate-when-ontology-changes)
    - [3. Use Type Hints Everywhere](#3-use-type-hints-everywhere)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Python FastAPI Backend Generator Example

> Generate production-grade FastAPI applications from RDF ontologies

## Overview

This example demonstrates ggen's multi-language capabilities by generating a complete FastAPI backend from an RDF ontology:

- Pydantic data models with validation
- FastAPI route handlers with OpenAPI documentation
- SQLAlchemy ORM models
- Alembic database migrations
- Pytest test fixtures
- Type-safe request/response handling

> **Language-Agnostic Proof**: Same RDF ontology generates JavaScript (Express), Python (FastAPI), Go (Gin), and more!

## Prerequisites

- Python 3.10+
- FastAPI
- SQLAlchemy 2.0+
- Pydantic v2
- ggen CLI configured

## Step 1: Create RDF Ontology

Create `ontology.ttl` (same format as JavaScript example):

```turtle
@prefix ex: <http://example.org/api/>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>

# ===== Enumerations =====

ex:UserRole a rdfs:Datatype ;
  ex:values ("admin" "user" "guest") .

ex:PostStatus a rdfs:Datatype ;
  ex:values ("draft" "published" "archived") .

# ===== User Entity =====

ex:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "System user account" .

ex:userId a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "User ID" ;
  ex:required true ;
  ex:unique true .

ex:userName a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "Username" ;
  ex:required true ;
  ex:minLength 3 ;
  ex:maxLength 50 .

ex:userEmail a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "Email" ;
  ex:required true ;
  ex:pattern "^[^@]+@[^@]+\\.[^@]+$" ;
  ex:unique true .

ex:userRole a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range ex:UserRole ;
  ex:required true ;
  ex:default "user" .

ex:userCreatedAt a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:dateTime ;
  rdfs:label "Created At" ;
  ex:required true .

# ===== Post Entity =====

ex:Post a rdfs:Class ;
  rdfs:label "Post" ;
  rdfs:comment "User blog post" .

ex:postId a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:required true ;
  ex:unique true .

ex:postTitle a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:required true ;
  ex:minLength 5 ;
  ex:maxLength 200 .

ex:postContent a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:required true ;
  ex:minLength 10 .

ex:postStatus a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range ex:PostStatus ;
  ex:required true ;
  ex:default "draft" .

ex:postAuthorId a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:required true ;
  ex:foreignKey "User" .

ex:postCreatedAt a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:dateTime ;
  ex:required true .

ex:postPublishedAt a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:dateTime ;
  ex:required false .

# ===== Relationships =====

ex:userPosts a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range ex:Post ;
  ex:relationship "one-to-many" ;
  ex:relationshipName "posts" .

ex:postAuthor a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range ex:User ;
  ex:relationship "many-to-one" ;
  ex:relationshipName "author" .

# ===== API Endpoints =====

ex:GetUserEndpoint a rdfs:Class ;
  rdfs:label "Get User" ;
  ex:httpMethod "GET" ;
  ex:path "/users/{user_id}" ;
  ex:param "user_id" xsd:string ;
  ex:returnType "User" .

ex:CreatePostEndpoint a rdfs:Class ;
  rdfs:label "Create Post" ;
  ex:httpMethod "POST" ;
  ex:path "/posts" ;
  ex:bodyType "CreatePostInput" ;
  ex:returnType "Post" .

ex:ListPostsEndpoint a rdfs:Class ;
  rdfs:label "List Posts" ;
  ex:httpMethod "GET" ;
  ex:path "/posts" ;
  ex:queryParam "skip" xsd:integer ;
  ex:queryParam "limit" xsd:integer ;
  ex:returnType "Post" ;
  ex:returnCollection true .

ex:UpdatePostStatusEndpoint a rdfs:Class ;
  rdfs:label "Update Post Status" ;
  ex:httpMethod "PUT" ;
  ex:path "/posts/{post_id}/status" ;
  ex:param "post_id" xsd:string ;
  ex:bodyType "UpdatePostStatusInput" ;
  ex:returnType "Post" .
```

## Step 2: Configure ggen for Python

Create `ggen.toml`:

```toml
[project]
name = "blog-api-generator"
version = "1.0.0"
language = "python"

[ontology]
source = "ontology.ttl"
base_iri = "http://example.org/api/"

[generation]
output_dir = "generated/"

# ===== Pydantic Models =====

[[generation.rules]]
name = "pydantic-models"
description = "Generate Pydantic data models"
query = { inline = """
PREFIX ex: <http://example.org/api/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?className ?fieldName ?fieldType ?required ?minLength ?maxLength ?pattern ?default
WHERE {
  ?class a rdfs:Class ;
    rdfs:label ?className .
  ?prop rdfs:domain ?class ;
    rdfs:label ?fieldName ;
    rdfs:range ?fieldType .
  OPTIONAL { ?prop ex:required ?required }
  OPTIONAL { ?prop ex:minLength ?minLength }
  OPTIONAL { ?prop ex:maxLength ?maxLength }
  OPTIONAL { ?prop ex:pattern ?pattern }
  OPTIONAL { ?prop ex:default ?default }
}
ORDER BY ?className ?fieldName
""" }
template = { file = "templates/pydantic-models.tera" }
output_file = "models.py"

# ===== SQLAlchemy ORM Models =====

[[generation.rules]]
name = "sqlalchemy-models"
description = "Generate SQLAlchemy ORM models"
query = { inline = """
PREFIX ex: <http://example.org/api/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?className ?fieldName ?columnType ?required ?relationship ?relationshipType ?backref
WHERE {
  ?class a rdfs:Class ;
    rdfs:label ?className .
  ?prop rdfs:domain ?class ;
    rdfs:label ?fieldName ;
    rdfs:range ?columnType .
  OPTIONAL { ?prop ex:relationship ?relationshipType ; ex:relationshipName ?relationship }
  OPTIONAL { ?prop ex:backref ?backref }
}
ORDER BY ?className ?fieldName
""" }
template = { file = "templates/sqlalchemy-models.tera" }
output_file = "database.py"

# ===== FastAPI Routes =====

[[generation.rules]]
name = "fastapi-routes"
description = "Generate FastAPI endpoint handlers"
query = { inline = """
PREFIX ex: <http://example.org/api/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?endpointName ?method ?path ?bodyType ?returnType ?param ?queryParam
WHERE {
  ?endpoint a rdfs:Class ;
    rdfs:label ?endpointName ;
    ex:httpMethod ?method ;
    ex:path ?path ;
    ex:returnType ?returnType .
  OPTIONAL { ?endpoint ex:bodyType ?bodyType }
  OPTIONAL { ?endpoint ex:param ?param }
  OPTIONAL { ?endpoint ex:queryParam ?queryParam }
}
""" }
template = { file = "templates/fastapi-routes.tera" }
output_file = "routes.py"

# ===== Alembic Migrations =====

[[generation.rules]]
name = "alembic-migration"
description = "Generate initial Alembic migration"
query = { inline = """
PREFIX ex: <http://example.org/api/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?className
WHERE {
  ?class a rdfs:Class ;
    rdfs:label ?className .
}
ORDER BY ?className
""" }
template = { file = "templates/alembic-migration.tera" }
output_file = "migrations/versions/001_initial_migration.py"

# ===== Pytest Fixtures =====

[[generation.rules]]
name = "pytest-fixtures"
description = "Generate test fixtures"
query = { inline = """
PREFIX ex: <http://example.org/api/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?className ?fieldName ?fieldType
WHERE {
  ?class a rdfs:Class ;
    rdfs:label ?className .
  ?prop rdfs:domain ?class ;
    rdfs:label ?fieldName ;
    rdfs:range ?fieldType .
}
ORDER BY ?className
""" }
template = { file = "templates/pytest-fixtures.tera" }
output_file = "conftest.py"
```

## Step 3: Create Tera Templates

### Pydantic Models Template

Create `templates/pydantic-models.tera`:

```tera
# Generated Pydantic Models
# DO NOT EDIT - Generated from RDF ontology

from typing import Optional, List
from datetime import datetime
from pydantic import BaseModel, Field, EmailStr, validator

{% set prev_class = "" %}
{% for row in results %}
  {% if row.className != prev_class %}
    {% if prev_class != "" %}


{% endif %}
class {{ row.className }}(BaseModel):
    """{{ row.className }} model"""
    {% set prev_class = row.className %}
  {% endif %}
  {{ row.fieldName | snakeCase }}: {% if row.required %}{{ row.fieldType | mapPythonType }}{% else %}Optional[{{ row.fieldType | mapPythonType }}]{% endif %} {% if row.maxLength %}= Field(..., max_length={{ row.maxLength }}){% elif row.minLength %}= Field(..., min_length={{ row.minLength }}){% elif row.pattern %}= Field(..., regex=r"{{ row.pattern }}"){% else %}{% endif %}

    class Config:
        from_attributes = True
{% endfor %}


# Request/Response Input Models
class CreatePostInput(BaseModel):
    """Input model for creating posts"""
    title: str = Field(..., min_length=5, max_length=200)
    content: str = Field(..., min_length=10)
    status: Optional[str] = Field(default="draft")

    @validator('title')
    def title_not_empty(cls, v):
        if not v.strip():
            raise ValueError('Title cannot be empty')
        return v


class UpdatePostStatusInput(BaseModel):
    """Input model for updating post status"""
    status: str = Field(..., pattern="^(draft|published|archived)$")
```

### FastAPI Routes Template

Create `templates/fastapi-routes.tera`:

```tera
# Generated FastAPI Routes
# DO NOT EDIT - Generated from RDF ontology

from fastapi import APIRouter, HTTPException, Depends, Query
from sqlalchemy.orm import Session
from typing import List, Optional
from .models import *
from .database import get_db

router = APIRouter(prefix="/api", tags=["posts"])

{% for row in results %}
@router.{{ row.method | lower }}("{{ row.path }}")
async def {{ row.endpointName | snakeCase }}(
    {% if row.param %}{{ row.param | snakeCase }}: str,{% endif %}
    {% if row.queryParam %}skip: int = Query(0), limit: int = Query(10),{% endif %}
    {% if row.bodyType %}body: {{ row.bodyType }},{% endif %}
    db: Session = Depends(get_db)
):
    """
    {{ row.endpointName }}

    Returns {{ row.returnType }}
    """
    try:
        # TODO: Implement endpoint logic
        raise NotImplementedError(f"Implement {{ row.endpointName }}")
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

{% endfor %}
```

### SQLAlchemy Models Template

Create `templates/sqlalchemy-models.tera`:

```tera
# Generated SQLAlchemy ORM Models
# DO NOT EDIT - Generated from RDF ontology

from sqlalchemy import Column, String, Integer, DateTime, ForeignKey, Boolean
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import relationship
from datetime import datetime

Base = declarative_base()

{% set prev_class = "" %}
{% for row in results %}
  {% if row.className != prev_class %}
    {% if prev_class != "" %}


{% endif %}
class {{ row.className }}(Base):
    __tablename__ = "{{ row.className | snakeCase }}"
    {% set prev_class = row.className %}
  {% endif %}
  {% if row.relationship %}
  {{ row.fieldName | snakeCase }} = relationship("{{ row.relationshipType | pascalCase }}", back_populates="{{ row.backref }}")
  {% else %}
  {{ row.fieldName | snakeCase }} = Column({{ row.columnType | mapSQLAlchemyType }}{% if row.required %}, nullable=False{% endif %})
  {% endif %}
{% endfor %}
```

## Step 4: Run Generation

```bash
ggen-cli render

ls -la generated/
# models.py          (Pydantic models)
# database.py        (SQLAlchemy ORM)
# routes.py          (FastAPI endpoints)
# conftest.py        (Pytest fixtures)
# migrations/versions/001_initial_migration.py
```

## Step 5: Create FastAPI Application

Create `app.py`:

```python
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from sqlalchemy import create_engine
from sqlalchemy.orm import sessionmaker
from generated.routes import router
from generated.database import Base, engine

# Create tables
Base.metadata.create_all(bind=engine)

app = FastAPI(
    title="Blog API",
    description="Generated from RDF ontology",
    version="1.0.0"
)

# Add CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Include routes
app.include_router(router)

# Health check
@app.get("/health")
async def health_check():
    return {"status": "ok"}

@app.get("/docs", include_in_schema=False)
async def swagger_ui():
    """Swagger UI documentation"""
    pass

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)
```

## Step 6: Implement Endpoints

Create `src/services.py`:

```python
from sqlalchemy.orm import Session
from generated.models import User, Post
from generated.models import CreatePostInput, UpdatePostStatusInput
from datetime import datetime
from typing import List, Optional

class UserService:
    @staticmethod
    def get_user(db: Session, user_id: str) -> User:
        return db.query(User).filter(User.id == user_id).first()

    @staticmethod
    def get_users(db: Session, skip: int = 0, limit: int = 10) -> List[User]:
        return db.query(User).offset(skip).limit(limit).all()

class PostService:
    @staticmethod
    def create_post(db: Session, input_data: CreatePostInput, author_id: str) -> Post:
        post = Post(
            title=input_data.title,
            content=input_data.content,
            status=input_data.status,
            author_id=author_id,
            created_at=datetime.now()
        )
        db.add(post)
        db.commit()
        db.refresh(post)
        return post

    @staticmethod
    def get_posts(db: Session, skip: int = 0, limit: int = 10) -> List[Post]:
        return db.query(Post).offset(skip).limit(limit).all()

    @staticmethod
    def get_post(db: Session, post_id: str) -> Optional[Post]:
        return db.query(Post).filter(Post.id == post_id).first()

    @staticmethod
    def update_post_status(
        db: Session,
        post_id: str,
        input_data: UpdatePostStatusInput
    ) -> Post:
        post = db.query(Post).filter(Post.id == post_id).first()
        if not post:
            raise ValueError("Post not found")

        post.status = input_data.status
        if input_data.status == "published" and not post.published_at:
            post.published_at = datetime.now()

        db.commit()
        db.refresh(post)
        return post
```

Update `generated/routes.py`:

```python
from fastapi import APIRouter, HTTPException, Depends
from sqlalchemy.orm import Session
from .models import Post, User, CreatePostInput, UpdatePostStatusInput
from .database import get_db
from ..src.services import PostService, UserService

router = APIRouter(prefix="/api", tags=["api"])

@router.get("/users/{user_id}", response_model=User)
async def get_user(user_id: str, db: Session = Depends(get_db)):
    """Get user by ID"""
    user = UserService.get_user(db, user_id)
    if not user:
        raise HTTPException(status_code=404, detail="User not found")
    return user

@router.post("/posts", response_model=Post)
async def create_post(body: CreatePostInput, db: Session = Depends(get_db)):
    """Create new post"""
    # In production, get user_id from JWT token
    return PostService.create_post(db, body, "current-user-id")

@router.get("/posts", response_model=list[Post])
async def list_posts(skip: int = 0, limit: int = 10, db: Session = Depends(get_db)):
    """List posts with pagination"""
    return PostService.get_posts(db, skip, limit)

@router.put("/posts/{post_id}/status", response_model=Post)
async def update_post_status(
    post_id: str,
    body: UpdatePostStatusInput,
    db: Session = Depends(get_db)
):
    """Update post status"""
    post = PostService.update_post_status(db, post_id, body)
    if not post:
        raise HTTPException(status_code=404, detail="Post not found")
    return post
```

## Step 7: Test Generated Code

Create `tests/test_api.py`:

```python
import pytest
from fastapi.testclient import TestClient
from app import app
from generated.database import SessionLocal
from generated.models import User, Post

client = TestClient(app)

@pytest.fixture
def db():
    db = SessionLocal()
    yield db
    db.close()

def test_health_check():
    response = client.get("/health")
    assert response.status_code == 200
    assert response.json()["status"] == "ok"

def test_get_user_not_found():
    response = client.get("/api/users/nonexistent")
    assert response.status_code == 404

def test_create_post(db):
    response = client.post(
        "/api/posts",
        json={
            "title": "Test Post",
            "content": "This is test content",
            "status": "draft"
        }
    )
    assert response.status_code == 200
    assert response.json()["title"] == "Test Post"

def test_list_posts(db):
    response = client.get("/api/posts?skip=0&limit=10")
    assert response.status_code == 200
    assert isinstance(response.json(), list)

def test_update_post_status(db):
    # Create post first
    create_response = client.post(
        "/api/posts",
        json={
            "title": "Test Post",
            "content": "Content",
            "status": "draft"
        }
    )
    post_id = create_response.json()["id"]

    # Update status
    response = client.put(
        f"/api/posts/{post_id}/status",
        json={"status": "published"}
    )
    assert response.status_code == 200
    assert response.json()["status"] == "published"
```

## Multi-Language Proof

Same `ontology.ttl` generates:

```bash
# JavaScript (Express)
ggen-cli --template express render

# Python (FastAPI)
ggen-cli --template fastapi render

# Go (Gin)
ggen-cli --template gin render

# Ruby (Rails)
ggen-cli --template rails render
```

Each generates **idiomatic code** for that language/framework!

## Best Practices

### 1. Separate Generated and Manual Code

```
app/
├── generated/          # Generated from ggen
│   ├── models.py      # Pydantic models
│   ├── database.py    # SQLAlchemy ORM
│   └── routes.py      # Endpoint stubs
├── src/               # Manual implementation
│   ├── services.py    # Business logic
│   ├── auth.py        # Authentication
│   └── utils.py       # Utilities
└── tests/             # Test suite
```

### 2. Regenerate When Ontology Changes

```bash
# Update ontology.ttl
# Then regenerate
ggen-cli render

# Manually review generated changes
git diff generated/

# Update implementation if needed
# Commit both ontology and generated code
```

### 3. Use Type Hints Everywhere

```python
from typing import List, Optional
from generated.models import Post

def get_posts(
    skip: int = 0,
    limit: int = 10
) -> List[Post]:
    """Type-safe endpoint handler"""
    pass
```

## See Also

- [JavaScript Express Example](./JAVASCRIPT_EXPRESS_EXAMPLE.md) - Proof of language-agnostic generation
- [PostgreSQL Integration](./POSTGRESQL_INTEGRATION_EXAMPLE.md) - Database layer
- [SPARQL Inference Guide](../SPARQL_INFERENCE_GUIDE.md) - Query optimization
