# Tutorial: Build a Blog Platform in 30 Minutes with Ontology-Driven Development

## What You'll Learn

In this tutorial, you'll experience the power of ontology-driven development by building a complete blog platform. Instead of writing models by hand, you'll define your domain once in RDF/OWL and automatically generate type-safe code for both backend and frontend.

**By the end, you'll have:**
- A semantic domain model (RDF ontology)
- Type-safe Rust backend models
- TypeScript frontend types
- The ability to evolve your schema with confidence

**Time required:** 30 minutes

---

## Step 1: Define Your Domain Model

The heart of ontology-driven development is the **domain ontology** - a semantic description of your application's concepts and their relationships.

### Create the Blog Ontology

Create a file `blog.ttl` with your domain model:

```turtle
@prefix : <http://example.org/blog#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Ontology declaration
: a owl:Ontology ;
    rdfs:label "Blog Platform Ontology" ;
    rdfs:comment "Domain model for a blog platform with users, posts, and comments" .

# Classes
:User a owl:Class ;
    rdfs:label "User" ;
    rdfs:comment "A registered user of the blog platform" .

:Post a owl:Class ;
    rdfs:label "Post" ;
    rdfs:comment "A blog post written by a user" .

:Comment a owl:Class ;
    rdfs:label "Comment" ;
    rdfs:comment "A comment on a blog post" .

# User properties
:email a owl:DatatypeProperty ;
    rdfs:label "email" ;
    rdfs:domain :User ;
    rdfs:range xsd:string ;
    rdfs:comment "User's email address (unique identifier)" .

:name a owl:DatatypeProperty ;
    rdfs:label "name" ;
    rdfs:domain :User ;
    rdfs:range xsd:string ;
    rdfs:comment "User's display name" .

:joinedAt a owl:DatatypeProperty ;
    rdfs:label "joined_at" ;
    rdfs:domain :User ;
    rdfs:range xsd:dateTime ;
    rdfs:comment "Timestamp when user registered" .

# Post properties
:title a owl:DatatypeProperty ;
    rdfs:label "title" ;
    rdfs:domain :Post ;
    rdfs:range xsd:string ;
    rdfs:comment "Post title" .

:content a owl:DatatypeProperty ;
    rdfs:label "content" ;
    rdfs:domain :Post ;
    rdfs:range xsd:string ;
    rdfs:comment "Post content (markdown)" .

:publishedAt a owl:DatatypeProperty ;
    rdfs:label "published_at" ;
    rdfs:domain :Post ;
    rdfs:range xsd:dateTime ;
    rdfs:comment "Publication timestamp" .

# Comment properties
:text a owl:DatatypeProperty ;
    rdfs:label "text" ;
    rdfs:domain :Comment ;
    rdfs:range xsd:string ;
    rdfs:comment "Comment text" .

:createdAt a owl:DatatypeProperty ;
    rdfs:label "created_at" ;
    rdfs:domain :Comment ;
    rdfs:range xsd:dateTime ;
    rdfs:comment "Comment creation timestamp" .

# Relationships
:hasAuthor a owl:ObjectProperty ;
    rdfs:label "has_author" ;
    rdfs:domain :Post ;
    rdfs:range :User ;
    rdfs:comment "Post author (User)" .

:hasPosts a owl:ObjectProperty ;
    rdfs:label "has_posts" ;
    rdfs:domain :User ;
    rdfs:range :Post ;
    owl:inverseOf :hasAuthor ;
    rdfs:comment "User's posts (one-to-many)" .

:hasComments a owl:ObjectProperty ;
    rdfs:label "has_comments" ;
    rdfs:domain :Post ;
    rdfs:range :Comment ;
    rdfs:comment "Post comments (one-to-many)" .

:commentAuthor a owl:ObjectProperty ;
    rdfs:label "comment_author" ;
    rdfs:domain :Comment ;
    rdfs:range :User ;
    rdfs:comment "Comment author" .
```

### Understanding the Ontology

**Key concepts:**

1. **Classes** (`owl:Class`) - Your domain entities: `User`, `Post`, `Comment`
2. **Datatype Properties** (`owl:DatatypeProperty`) - Scalar fields like `email`, `title`, `text`
3. **Object Properties** (`owl:ObjectProperty`) - Relationships between entities
4. **Ranges** - Type constraints (e.g., `xsd:string`, `xsd:dateTime`)

**Why RDF/OWL?**
- Machine-readable and validatable
- Rich type system with inference
- Standard format with powerful tooling
- Single source of truth for all code generation

---

## Step 2: Generate Rust Backend Models

Now let's generate type-safe Rust models from the ontology.

### Generate Command

```bash
ggen template generate-rdf \
  --ontology blog.ttl \
  --template rust-models \
  --output-dir src/models
```

### Generated Code

**`src/models/user.rs`:**

```rust
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// A registered user of the blog platform
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct User {
    /// Unique identifier
    pub id: Uuid,

    /// User's email address (unique identifier)
    pub email: String,

    /// User's display name
    pub name: String,

    /// Timestamp when user registered
    pub joined_at: DateTime<Utc>,
}

impl User {
    pub fn new(email: String, name: String) -> Self {
        Self {
            id: Uuid::new_v4(),
            email,
            name,
            joined_at: Utc::now(),
        }
    }
}

/// User with relationships
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UserWithPosts {
    #[serde(flatten)]
    pub user: User,

    /// User's posts (one-to-many)
    pub posts: Vec<super::post::Post>,
}
```

**`src/models/post.rs`:**

```rust
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// A blog post written by a user
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Post {
    /// Unique identifier
    pub id: Uuid,

    /// Post title
    pub title: String,

    /// Post content (markdown)
    pub content: String,

    /// Publication timestamp
    pub published_at: DateTime<Utc>,

    /// Post author ID (foreign key)
    pub author_id: Uuid,
}

impl Post {
    pub fn new(title: String, content: String, author_id: Uuid) -> Self {
        Self {
            id: Uuid::new_v4(),
            title,
            content,
            published_at: Utc::now(),
            author_id,
        }
    }
}

/// Post with relationships
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PostWithRelations {
    #[serde(flatten)]
    pub post: Post,

    /// Post author (User)
    pub author: super::user::User,

    /// Post comments (one-to-many)
    pub comments: Vec<super::comment::Comment>,
}
```

**`src/models/comment.rs`:**

```rust
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// A comment on a blog post
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Comment {
    /// Unique identifier
    pub id: Uuid,

    /// Comment text
    pub text: String,

    /// Comment creation timestamp
    pub created_at: DateTime<Utc>,

    /// Post ID (foreign key)
    pub post_id: Uuid,

    /// Author ID (foreign key)
    pub author_id: Uuid,
}

impl Comment {
    pub fn new(text: String, post_id: Uuid, author_id: Uuid) -> Self {
        Self {
            id: Uuid::new_v4(),
            text,
            created_at: Utc::now(),
            post_id,
            author_id,
        }
    }
}
```

### What Just Happened?

The generator analyzed your ontology and created:

1. **Type-safe structs** with proper Rust types (`String`, `DateTime<Utc>`, `Uuid`)
2. **Serde integration** for JSON serialization
3. **Documentation** from RDF comments
4. **Relationship models** (e.g., `UserWithPosts`, `PostWithRelations`)
5. **Constructors** with sensible defaults

**No hand-coding required!**

---

## Step 3: Generate TypeScript Frontend Types

Now let's generate matching TypeScript types for the frontend.

### Generate Command

```bash
ggen template generate-rdf \
  --ontology blog.ttl \
  --template typescript-models \
  --output-dir frontend/src/types
```

### Generated Code

**`frontend/src/types/user.ts`:**

```typescript
/**
 * A registered user of the blog platform
 */
export interface User {
  /** Unique identifier */
  id: string;

  /** User's email address (unique identifier) */
  email: string;

  /** User's display name */
  name: string;

  /** Timestamp when user registered */
  joined_at: string; // ISO 8601 datetime
}

/**
 * User with relationships
 */
export interface UserWithPosts extends User {
  /** User's posts (one-to-many) */
  posts: Post[];
}

/**
 * Create a new user
 */
export function createUser(
  email: string,
  name: string
): Omit<User, 'id' | 'joined_at'> {
  return { email, name };
}
```

**`frontend/src/types/post.ts`:**

```typescript
import type { User } from './user';
import type { Comment } from './comment';

/**
 * A blog post written by a user
 */
export interface Post {
  /** Unique identifier */
  id: string;

  /** Post title */
  title: string;

  /** Post content (markdown) */
  content: string;

  /** Publication timestamp */
  published_at: string; // ISO 8601 datetime

  /** Post author ID (foreign key) */
  author_id: string;
}

/**
 * Post with relationships
 */
export interface PostWithRelations extends Post {
  /** Post author (User) */
  author: User;

  /** Post comments (one-to-many) */
  comments: Comment[];
}

/**
 * Create a new post
 */
export function createPost(
  title: string,
  content: string,
  author_id: string
): Omit<Post, 'id' | 'published_at'> {
  return { title, content, author_id };
}
```

**`frontend/src/types/comment.ts`:**

```typescript
/**
 * A comment on a blog post
 */
export interface Comment {
  /** Unique identifier */
  id: string;

  /** Comment text */
  text: string;

  /** Comment creation timestamp */
  created_at: string; // ISO 8601 datetime

  /** Post ID (foreign key) */
  post_id: string;

  /** Author ID (foreign key) */
  author_id: string;
}

/**
 * Create a new comment
 */
export function createComment(
  text: string,
  post_id: string,
  author_id: string
): Omit<Comment, 'id' | 'created_at'> {
  return { text, post_id, author_id };
}
```

### Perfect Type Alignment

**Notice:**
- Field names match exactly (`email`, `title`, `text`)
- Types align (Rust `DateTime<Utc>` → TypeScript `string` with ISO 8601)
- Relationships mirror the backend
- Factory functions for creating new entities

**This means:**
- No type mismatches between frontend/backend
- Refactor once, update everywhere
- Compiler-verified API contracts

---

## Step 4: Evolve Your Schema

Requirements change. Let's add comment upvoting functionality.

### Update the Ontology

Add to `blog.ttl`:

```turtle
# Comment upvotes property
:upvotes a owl:DatatypeProperty ;
    rdfs:label "upvotes" ;
    rdfs:domain :Comment ;
    rdfs:range xsd:integer ;
    rdfs:comment "Number of upvotes (likes)" .
```

### Regenerate Everything

```bash
# Regenerate Rust models
ggen template generate-rdf \
  --ontology blog.ttl \
  --template rust-models \
  --output-dir src/models

# Regenerate TypeScript types
ggen template generate-rdf \
  --ontology blog.ttl \
  --template typescript-models \
  --output-dir frontend/src/types
```

### Updated Code

**Rust (`src/models/comment.rs`):**

```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Comment {
    pub id: Uuid,
    pub text: String,
    pub created_at: DateTime<Utc>,

    /// Number of upvotes (likes)
    pub upvotes: i32,  // ← NEW FIELD

    pub post_id: Uuid,
    pub author_id: Uuid,
}

impl Comment {
    pub fn new(text: String, post_id: Uuid, author_id: Uuid) -> Self {
        Self {
            id: Uuid::new_v4(),
            text,
            created_at: Utc::now(),
            upvotes: 0,  // ← SENSIBLE DEFAULT
            post_id,
            author_id,
        }
    }
}
```

**TypeScript (`frontend/src/types/comment.ts`):**

```typescript
export interface Comment {
  id: string;
  text: string;
  created_at: string;

  /** Number of upvotes (likes) */
  upvotes: number;  // ← NEW FIELD

  post_id: string;
  author_id: string;
}
```

### What Happened?

1. **Single ontology change** propagated to all generated code
2. **Type safety preserved** - compilers catch any missing updates
3. **Default values** added automatically (`upvotes: 0`)
4. **Documentation** synced from RDF comments

**No manual synchronization needed!**

---

## Step 5: Validate with SPARQL Queries

Use SPARQL to query and validate your ontology.

### Query All Posts

```bash
ggen graph query blog.ttl --sparql "
  PREFIX : <http://example.org/blog#>
  SELECT ?post ?title WHERE {
    ?post a :Post ;
          :title ?title .
  }
"
```

### Find Users with Posts

```bash
ggen graph query blog.ttl --sparql "
  PREFIX : <http://example.org/blog#>
  SELECT ?user ?name (COUNT(?post) as ?post_count) WHERE {
    ?user a :User ;
          :name ?name ;
          :hasPosts ?post .
  }
  GROUP BY ?user ?name
"
```

### Validate Comment Schema

```bash
ggen graph query blog.ttl --sparql "
  PREFIX : <http://example.org/blog#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

  SELECT ?property ?label ?range WHERE {
    ?property rdfs:domain :Comment ;
              rdfs:label ?label ;
              rdfs:range ?range .
  }
"
```

**Expected output:**
```
property                          label         range
http://example.org/blog#text      text          xsd:string
http://example.org/blog#createdAt created_at    xsd:dateTime
http://example.org/blog#upvotes   upvotes       xsd:integer
```

### Visualize the Ontology

Generate a visual graph:

```bash
ggen graph visualize blog.ttl --format dot --output blog.dot
dot -Tpng blog.dot -o blog-graph.png
```

This creates a diagram showing:
- Classes (User, Post, Comment)
- Properties (email, title, text, upvotes)
- Relationships (hasAuthor, hasPosts, hasComments)

---

## Step 6: Add Database Migrations (Bonus)

Since your schema is machine-readable, you can generate database migrations too.

### Generate SQL Schema

```bash
ggen template generate-rdf \
  --ontology blog.ttl \
  --template sql-schema \
  --output-dir migrations
```

**Generated `migrations/001_create_blog_schema.sql`:**

```sql
-- Users table
CREATE TABLE users (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    email VARCHAR(255) NOT NULL UNIQUE,
    name VARCHAR(255) NOT NULL,
    joined_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_users_email ON users(email);

-- Posts table
CREATE TABLE posts (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    title VARCHAR(500) NOT NULL,
    content TEXT NOT NULL,
    published_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    author_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE
);

CREATE INDEX idx_posts_author ON posts(author_id);
CREATE INDEX idx_posts_published_at ON posts(published_at DESC);

-- Comments table
CREATE TABLE comments (
    id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
    text TEXT NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    upvotes INTEGER NOT NULL DEFAULT 0,
    post_id UUID NOT NULL REFERENCES posts(id) ON DELETE CASCADE,
    author_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE
);

CREATE INDEX idx_comments_post ON comments(post_id);
CREATE INDEX idx_comments_author ON comments(author_id);
```

**Foreign keys, indexes, and constraints derived from the ontology!**

---

## Step 7: Complete API Integration

Let's see how the generated models integrate into a real Rust API.

### Axum API Handler (Rust)

```rust
use axum::{extract::Path, http::StatusCode, Json};
use uuid::Uuid;
use crate::models::{PostWithRelations, CreatePostRequest};

/// GET /posts/:id - Fetch post with author and comments
pub async fn get_post(
    Path(id): Path<Uuid>,
    db: DatabaseConnection,
) -> Result<Json<PostWithRelations>, StatusCode> {
    let post = db
        .fetch_post_with_relations(id)
        .await
        .map_err(|_| StatusCode::NOT_FOUND)?;

    Ok(Json(post))
}

/// POST /posts - Create new post
pub async fn create_post(
    Json(req): Json<CreatePostRequest>,
    db: DatabaseConnection,
) -> Result<(StatusCode, Json<Post>), StatusCode> {
    let post = Post::new(req.title, req.content, req.author_id);

    db.insert_post(&post)
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    Ok((StatusCode::CREATED, Json(post)))
}
```

### React Component (TypeScript)

```typescript
import { useQuery } from '@tanstack/react-query';
import type { PostWithRelations } from '@/types/post';

function PostDetail({ postId }: { postId: string }) {
  const { data: post, isLoading } = useQuery({
    queryKey: ['post', postId],
    queryFn: async (): Promise<PostWithRelations> => {
      const res = await fetch(`/api/posts/${postId}`);
      return res.json();
    },
  });

  if (isLoading) return <div>Loading...</div>;
  if (!post) return <div>Post not found</div>;

  return (
    <article>
      <h1>{post.title}</h1>
      <p className="author">By {post.author.name}</p>
      <div className="content">{post.content}</div>

      <section className="comments">
        <h2>Comments ({post.comments.length})</h2>
        {post.comments.map(comment => (
          <div key={comment.id}>
            <p>{comment.text}</p>
            <span>{comment.upvotes} upvotes</span>
          </div>
        ))}
      </section>
    </article>
  );
}
```

**Notice:**
- Types flow seamlessly from backend to frontend
- `PostWithRelations` includes `author` and `comments` automatically
- TypeScript autocomplete works perfectly
- No manual type definitions needed

---

## Benefits Recap

### 1. Single Source of Truth
- Domain model defined once in `blog.ttl`
- All code generated from this source
- Changes propagate automatically

### 2. Type Safety Everywhere
- Rust structs with proper types
- TypeScript interfaces matching exactly
- Compiler catches schema mismatches

### 3. Effortless Evolution
- Add field → Regenerate → Done
- No manual synchronization
- No risk of frontend/backend drift

### 4. Validation & Queries
- SPARQL for semantic queries
- Ontology reasoning for validation
- Visual graphs for documentation

### 5. Database Integration
- SQL schemas generated automatically
- Foreign keys from relationships
- Indexes from query patterns

---

## Next Steps

### Extend the Ontology

Try adding these features yourself:

1. **Tags:** Add a `Tag` class and `hasTags` relationship for posts
2. **Drafts:** Add a `status` property (draft/published) to posts
3. **Replies:** Add `parentComment` for threaded comments
4. **Likes:** Create a `Like` class linking users to posts

### Explore Templates

ggen supports many RDF-to-code templates:

```bash
# List all available templates
ggen template list --category rdf-generators

# Available templates:
# - rust-models (backend structs)
# - typescript-models (frontend types)
# - sql-schema (PostgreSQL)
# - graphql-schema (GraphQL types)
# - openapi-spec (REST API docs)
# - python-pydantic (Python models)
```

### Integrate with Your Stack

Generated models work with:
- **Backend:** Axum, Actix-web, Rocket, Warp
- **Frontend:** React, Vue, Svelte, Angular
- **Database:** PostgreSQL, MySQL, SQLite, MongoDB
- **API:** REST, GraphQL, gRPC

### Learn More

- [RDF/OWL Guide](../reference/rdf-ontologies.md)
- [Template System](../reference/template-system.md)
- [Graph Commands](../reference/graph-commands.md)
- [SPARQL Tutorial](../reference/sparql-queries.md)

---

## Conclusion

You've just experienced **ontology-driven development**:

1. ✅ Defined a blog platform in 100 lines of RDF
2. ✅ Generated type-safe Rust models automatically
3. ✅ Generated matching TypeScript types
4. ✅ Evolved the schema with a single change
5. ✅ Validated with SPARQL queries
6. ✅ Generated database migrations

**Traditional approach:** Write models in Rust, duplicate in TypeScript, manually sync databases, pray nothing breaks.

**Ontology-driven approach:** Define once, generate everywhere, evolve with confidence.

**Welcome to the future of code generation.**

---

**Questions?** Check the [FAQ](../faq.md) or open an issue on [GitHub](https://github.com/ggen-project/ggen).
