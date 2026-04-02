<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [PostgreSQL Integration Example](#postgresql-integration-example)
  - [Overview](#overview)
  - [Prerequisites](#prerequisites)
  - [Step 1: Design Data Ontology](#step-1-design-data-ontology)
  - [Step 2: Configure ggen.toml](#step-2-configure-ggentoml)
  - [Step 3: Create Tera Templates](#step-3-create-tera-templates)
  - [Step 4: Run Generation](#step-4-run-generation)
  - [Step 5: Apply to Database](#step-5-apply-to-database)
    - [Using SQL directly:](#using-sql-directly)
    - [Using Drizzle Migration:](#using-drizzle-migration)
  - [Step 6: Implement Database Access Layer](#step-6-implement-database-access-layer)
  - [Advanced Patterns](#advanced-patterns)
    - [Pattern 1: Soft Deletes](#pattern-1-soft-deletes)
    - [Pattern 2: Audit Trail](#pattern-2-audit-trail)
    - [Pattern 3: Full-Text Search](#pattern-3-full-text-search)
  - [Performance Considerations](#performance-considerations)
    - [1. Index Strategy](#1-index-strategy)
    - [2. Connection Pooling](#2-connection-pooling)
    - [3. Query Optimization](#3-query-optimization)
  - [Testing](#testing)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# PostgreSQL Integration Example

> Generate database schemas and ORM models from RDF ontologies

## Overview

This guide demonstrates generating a complete PostgreSQL integration including:

- Database schema (CREATE TABLE statements)
- Drizzle ORM models with relationships
- Migration files
- TypeScript type definitions
- Query builders

## Prerequisites

- PostgreSQL 12+
- Node.js 18+
- Drizzle ORM or Prisma
- ggen CLI configured

## Step 1: Design Data Ontology

Create `ontology/database-schema.ttl`:

```turtle
@prefix ex: <http://example.org/db/>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>

# ===== Enumerations =====

ex:UserStatus a rdfs:Datatype ;
  ex:values ("active" "inactive" "suspended") .

ex:PostStatus a rdfs:Datatype ;
  ex:values ("draft" "published" "archived") .

# ===== User Table =====

ex:User a rdfs:Class ;
  rdfs:label "User" ;
  ex:tableName "users" ;
  ex:primaryKey "userId" .

ex:userId a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  ex:columnName "id" ;
  ex:columnType "UUID" ;
  ex:required true ;
  ex:unique true ;
  ex:default "gen_random_uuid()" .

ex:userName a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  ex:columnName "name" ;
  ex:columnType "VARCHAR(100)" ;
  ex:required true ;
  ex:index true .

ex:userEmail a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  ex:columnName "email" ;
  ex:columnType "VARCHAR(255)" ;
  ex:required true ;
  ex:unique true .

ex:userStatus a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range ex:UserStatus ;
  ex:columnName "status" ;
  ex:columnType "VARCHAR(50)" ;
  ex:default "active" ;
  ex:required true .

ex:userPasswordHash a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  ex:columnName "password_hash" ;
  ex:columnType "VARCHAR(255)" ;
  ex:required true ;
  ex:hidden true .

ex:userCreatedAt a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:dateTime ;
  ex:columnName "created_at" ;
  ex:columnType "TIMESTAMP" ;
  ex:default "CURRENT_TIMESTAMP" ;
  ex:required true .

ex:userUpdatedAt a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:dateTime ;
  ex:columnName "updated_at" ;
  ex:columnType "TIMESTAMP" ;
  ex:default "CURRENT_TIMESTAMP" ;
  ex:required true ;
  ex:autoUpdate true .

# ===== Post Table =====

ex:Post a rdfs:Class ;
  rdfs:label "Post" ;
  ex:tableName "posts" ;
  ex:primaryKey "postId" .

ex:postId a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:columnName "id" ;
  ex:columnType "UUID" ;
  ex:required true ;
  ex:unique true ;
  ex:default "gen_random_uuid()" .

ex:postTitle a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:columnName "title" ;
  ex:columnType "VARCHAR(255)" ;
  ex:required true ;
  ex:index true .

ex:postContent a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:columnName "content" ;
  ex:columnType "TEXT" ;
  ex:required true .

ex:postStatus a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range ex:PostStatus ;
  ex:columnName "status" ;
  ex:columnType "VARCHAR(50)" ;
  ex:default "draft" ;
  ex:required true ;
  ex:index true .

ex:postAuthorId a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:columnName "author_id" ;
  ex:columnType "UUID" ;
  ex:required true ;
  ex:foreignKey "users(id)" ;
  ex:onDelete "CASCADE" .

ex:postCreatedAt a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:dateTime ;
  ex:columnName "created_at" ;
  ex:columnType "TIMESTAMP" ;
  ex:default "CURRENT_TIMESTAMP" ;
  ex:required true .

ex:postPublishedAt a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:dateTime ;
  ex:columnName "published_at" ;
  ex:columnType "TIMESTAMP" ;
  ex:required false .

# ===== Relationship: User -> Posts =====

ex:userPosts a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range ex:Post ;
  ex:relationship "one-to-many" ;
  ex:foreignKeyField "postAuthorId" ;
  ex:relationshipName "posts" .

# ===== Tag Table (Many-to-Many Example) =====

ex:Tag a rdfs:Class ;
  rdfs:label "Tag" ;
  ex:tableName "tags" ;
  ex:primaryKey "tagId" .

ex:tagId a rdf:Property ;
  rdfs:domain ex:Tag ;
  rdfs:range xsd:string ;
  ex:columnName "id" ;
  ex:columnType "UUID" ;
  ex:required true ;
  ex:unique true .

ex:tagName a rdf:Property ;
  rdfs:domain ex:Tag ;
  rdfs:range xsd:string ;
  ex:columnName "name" ;
  ex:columnType "VARCHAR(100)" ;
  ex:required true ;
  ex:unique true ;
  ex:index true .

# ===== PostTag Junction Table =====

ex:PostTag a rdfs:Class ;
  rdfs:label "PostTag" ;
  ex:tableName "post_tags" ;
  ex:primaryKey ("postId" "tagId") .

ex:postTagPostId a rdf:Property ;
  rdfs:domain ex:PostTag ;
  rdfs:range xsd:string ;
  ex:columnName "post_id" ;
  ex:columnType "UUID" ;
  ex:required true ;
  ex:foreignKey "posts(id)" ;
  ex:onDelete "CASCADE" .

ex:postTagTagId a rdf:Property ;
  rdfs:domain ex:PostTag ;
  rdfs:range xsd:string ;
  ex:columnName "tag_id" ;
  ex:columnType "UUID" ;
  ex:required true ;
  ex:foreignKey "tags(id)" ;
  ex:onDelete "CASCADE" .

# ===== Relationships =====

ex:postTags a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range ex:Tag ;
  ex:relationship "many-to-many" ;
  ex:through "PostTag" ;
  ex:relationshipName "tags" .
```

## Step 2: Configure ggen.toml

Create `ggen.toml`:

```toml
[project]
name = "blog-database-generator"
version = "1.0.0"

[ontology]
source = "ontology/database-schema.ttl"

[generation]
output_dir = "generated/"

# ===== SQL Schema Generation =====

[[generation.rules]]
name = "sql-schema"
description = "Generate SQL CREATE TABLE statements"
query = { inline = """
PREFIX ex: <http://example.org/db/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?tableName ?columnName ?columnType ?required ?unique ?index ?foreignKey ?default
WHERE {
  ?class a rdfs:Class ;
    ex:tableName ?tableName .
  ?prop rdfs:domain ?class ;
    ex:columnName ?columnName ;
    ex:columnType ?columnType ;
    ex:required ?required .
  OPTIONAL { ?prop ex:unique ?unique }
  OPTIONAL { ?prop ex:index ?index }
  OPTIONAL { ?prop ex:foreignKey ?foreignKey }
  OPTIONAL { ?prop ex:default ?default }
}
ORDER BY ?tableName ?columnName
""" }
template = { file = "templates/sql-schema.tera" }
output_file = "schema.sql"

# ===== Drizzle ORM Models =====

[[generation.rules]]
name = "drizzle-models"
description = "Generate Drizzle ORM model definitions"
query = { inline = """
PREFIX ex: <http://example.org/db/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?className ?tableName ?columnName ?columnType ?required ?relationshipName ?relationshipType
WHERE {
  ?class a rdfs:Class ;
    rdfs:label ?className ;
    ex:tableName ?tableName .
  ?prop rdfs:domain ?class ;
    ex:columnName ?columnName ;
    ex:columnType ?columnType ;
    ex:required ?required .
  OPTIONAL { ?prop ex:relationshipName ?relationshipName ; ex:relationship ?relationshipType }
}
ORDER BY ?className ?columnName
""" }
template = { file = "templates/drizzle-models.tera" }
output_file = "models.ts"

# ===== TypeScript Types =====

[[generation.rules]]
name = "typescript-types"
description = "Generate TypeScript interfaces for database records"
query = { inline = """
PREFIX ex: <http://example.org/db/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?className ?propertyName ?propertyType
WHERE {
  ?class a rdfs:Class ;
    rdfs:label ?className .
  ?prop rdfs:domain ?class ;
    rdfs:label ?propertyName ;
    rdfs:range ?propertyType .
}
ORDER BY ?className ?propertyName
""" }
template = { file = "templates/typescript-db-types.tera" }
output_file = "db-types.ts"

# ===== Migration Templates =====

[[generation.rules]]
name = "migrations"
description = "Generate migration file template"
query = { inline = """
PREFIX ex: <http://example.org/db/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?tableName
WHERE {
  ?class a rdfs:Class ;
    ex:tableName ?tableName .
}
ORDER BY ?tableName
""" }
template = { file = "templates/migration.tera" }
output_file = "migrations/001_initial.sql"
```

## Step 3: Create Tera Templates

Create `templates/sql-schema.tera`:

```tera
-- Generated SQL Schema
-- DO NOT EDIT MANUALLY - Generated from RDF ontology
-- Source: {{ file }}

{% set prev_table = "" %}
{% for row in results %}
  {% if row.tableName != prev_table %}
    {% if prev_table != "" %}
);

{% endif %}
CREATE TABLE {{ row.tableName }} (
    {% set prev_table = row.tableName %}
    {% set first = true %}
  {% endif %}
  {% if not first %},{% endif %}
  {{ row.columnName }} {{ row.columnType }}{% if row.required %} NOT NULL{% endif %}{% if row.unique %} UNIQUE{% endif %}{% if row.default %} DEFAULT {{ row.default }}{% endif %}
  {% set first = false %}
{% endfor %}
);

-- Indexes
{% for row in results %}
  {% if row.index %}
CREATE INDEX idx_{{ row.tableName }}_{{ row.columnName }} ON {{ row.tableName }}({{ row.columnName }});
  {% endif %}
{% endfor %}

-- Foreign Keys
{% for row in results %}
  {% if row.foreignKey %}
ALTER TABLE {{ row.tableName }} ADD CONSTRAINT fk_{{ row.columnName }} FOREIGN KEY ({{ row.columnName }}) REFERENCES {{ row.foreignKey }};
  {% endif %}
{% endfor %}
```

Create `templates/drizzle-models.tera`:

```tera
// Generated Drizzle ORM Models
// DO NOT EDIT MANUALLY

import { pgTable, uuid, varchar, timestamp, text, boolean, pgEnum } from 'drizzle-orm/pg-core';
import { relations } from 'drizzle-orm';

{% set prev_table = "" %}
{% for row in results %}
  {% if row.className != prev_table %}
    {% if prev_table != "" %}
);

{% endif %}
export const {{ row.tableName }} = pgTable('{{ row.tableName }}', {
    {% set prev_table = row.className %}
  {% endif %}
  {{ row.columnName }}: {{ row.columnType | mapDrizzleType }}{% if row.required %}{% else %}.default(null){% endif %},
{% endfor %}
});

// Relationships
{% for row in results %}
  {% if row.relationshipName %}
export const {{ row.relationshipName }}Relations = relations({{ row.tableName }}, ({ one, many }) => ({
  {% if row.relationshipType == "one-to-many" %}
  {{ row.relationshipName }}: many({{ row.relationshipName | pascalCase }}),
  {% elsif row.relationshipType == "many-to-one" %}
  {{ row.relationshipName }}: one({{ row.relationshipName | pascalCase }}, {
    fields: [{{ row.tableName }}.{{ row.columnName }}],
    references: [{{ row.relationshipName | pascalCase }}.id],
  }),
  {% endif %}
}));
  {% endif %}
{% endfor %}
```

## Step 4: Run Generation

```bash
ggen-cli render

ls -la generated/
# schema.sql
# models.ts
# db-types.ts
# migrations/001_initial.sql
```

## Step 5: Apply to Database

### Using SQL directly:

```bash
psql -U postgres -d mydb -f generated/schema.sql
```

### Using Drizzle Migration:

```typescript
import { drizzle } from 'drizzle-orm/node-postgres';
import { Client } from 'pg';
import * as schema from './generated/models';

const client = new Client({
  connectionString: process.env.DATABASE_URL,
});

await client.connect();
const db = drizzle(client, { schema });

// Run migrations
import { migrate } from 'drizzle-orm/node-postgres/migrator';
await migrate(db, { migrationsFolder: './drizzle' });
```

## Step 6: Implement Database Access Layer

Create `src/db.ts`:

```typescript
import { drizzle } from 'drizzle-orm/node-postgres';
import { Client } from 'pg';
import * as schema from '../generated/models';
import { eq, and } from 'drizzle-orm';

const client = new Client({
  connectionString: process.env.DATABASE_URL,
});

export const db = drizzle(client, { schema });

// Example queries
export const queries = {
  // Get user by ID
  getUserById: async (userId: string) => {
    return db
      .select()
      .from(schema.users)
      .where(eq(schema.users.id, userId))
      .limit(1);
  },

  // Get all active users
  getActiveUsers: async () => {
    return db
      .select()
      .from(schema.users)
      .where(eq(schema.users.status, 'active'));
  },

  // Get user with posts
  getUserWithPosts: async (userId: string) => {
    return db
      .select()
      .from(schema.users)
      .leftJoin(schema.posts, eq(schema.posts.author_id, schema.users.id))
      .where(eq(schema.users.id, userId));
  },

  // Get published posts with tags
  getPublishedPostsWithTags: async () => {
    return db
      .select()
      .from(schema.posts)
      .leftJoin(schema.postTags, eq(schema.postTags.post_id, schema.posts.id))
      .leftJoin(schema.tags, eq(schema.tags.id, schema.postTags.tag_id))
      .where(eq(schema.posts.status, 'published'));
  },

  // Create new post
  createPost: async (data: {
    title: string;
    content: string;
    authorId: string;
  }) => {
    return db.insert(schema.posts).values({
      title: data.title,
      content: data.content,
      author_id: data.authorId,
      status: 'draft',
    });
  },

  // Update post status
  updatePostStatus: async (postId: string, status: 'draft' | 'published' | 'archived') => {
    return db
      .update(schema.posts)
      .set({
        status,
        updated_at: new Date(),
        ...(status === 'published' && { published_at: new Date() }),
      })
      .where(eq(schema.posts.id, postId));
  },
};
```

## Advanced Patterns

### Pattern 1: Soft Deletes

```turtle
ex:deletedAt a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:dateTime ;
  ex:columnName "deleted_at" ;
  ex:columnType "TIMESTAMP" ;
  ex:required false ;
  ex:softDelete true .
```

Implementation:

```typescript
// Get non-deleted posts
const activePosts = await db
  .select()
  .from(schema.posts)
  .where(isNull(schema.posts.deleted_at));

// Soft delete
await db
  .update(schema.posts)
  .set({ deleted_at: new Date() })
  .where(eq(schema.posts.id, postId));
```

### Pattern 2: Audit Trail

```turtle
ex:createdBy a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:columnName "created_by" ;
  ex:columnType "UUID" ;
  ex:required true ;
  ex:foreignKey "users(id)" .

ex:updatedBy a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:columnName "updated_by" ;
  ex:columnType "UUID" ;
  ex:required true ;
  ex:foreignKey "users(id)" .
```

### Pattern 3: Full-Text Search

```typescript
// Create full-text index
await client.query(`
  CREATE INDEX idx_posts_search ON posts
  USING GIN(to_tsvector('english', content))
`);

// Query full-text
const results = await db
  .select()
  .from(schema.posts)
  .where(
    sql`to_tsvector('english', ${schema.posts.content}) @@ plainto_tsquery('english', ${query})`
  );
```

## Performance Considerations

### 1. Index Strategy

```turtle
# Index frequently filtered fields
ex:userEmail ex:index true .
ex:postStatus ex:index true .
ex:postCreatedAt ex:index true .

# Composite indexes for common queries
ex:postAuthorAndStatus ex:compositeIndex ("author_id" "status") .
```

### 2. Connection Pooling

```typescript
import { Pool } from 'pg';

const pool = new Pool({
  connectionString: process.env.DATABASE_URL,
  max: 20,
  idleTimeoutMillis: 30000,
  connectionTimeoutMillis: 2000,
});

export const db = drizzle(pool, { schema });
```

### 3. Query Optimization

```typescript
// Use prepared statements
const getUserById = db
  .select()
  .from(schema.users)
  .where(eq(schema.users.id, sql.placeholder('userId')))
  .prepare();

// Execute multiple times
for (const id of userIds) {
  await getUserById.execute({ userId: id });
}
```

## Testing

```typescript
import { describe, it, expect, beforeEach } from 'vitest';
import { db } from './db';

describe('Database', () => {
  beforeEach(async () => {
    // Clear tables
    await db.delete(schema.posts);
    await db.delete(schema.users);
  });

  it('should create user', async () => {
    const result = await db.insert(schema.users).values({
      name: 'Alice',
      email: 'alice@example.com',
      password_hash: 'hashed',
    });

    const users = await db.select().from(schema.users);
    expect(users).toHaveLength(1);
    expect(users[0].name).toBe('Alice');
  });

  it('should cascade delete posts when user is deleted', async () => {
    // Create user
    const [user] = await db.insert(schema.users).values({
      name: 'Bob',
      email: 'bob@example.com',
      password_hash: 'hashed',
    });

    // Create post for user
    await db.insert(schema.posts).values({
      title: 'My Post',
      content: 'Content',
      author_id: user.id,
    });

    // Delete user
    await db.delete(schema.users).where(eq(schema.users.id, user.id));

    // Verify posts deleted
    const posts = await db.select().from(schema.posts);
    expect(posts).toHaveLength(0);
  });
});
```

## See Also

- [JavaScript Express Example](./JAVASCRIPT_EXPRESS_EXAMPLE.md) - API layer
- [GraphQL Deep Dive](./GRAPHQL_DEEP_DIVE.md) - Query interface
- [Troubleshooting Guide](../troubleshooting/TROUBLESHOOTING_GUIDE.md) - Common issues
