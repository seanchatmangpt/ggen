# GraphQL Generation Deep Dive

> Comprehensive guide to generating GraphQL schemas and resolvers from RDF ontologies

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [RDF to GraphQL Mapping](#rdf-to-graphql-mapping)
3. [Complete Workflow](#complete-workflow)
4. [Schema Generation](#schema-generation)
5. [Resolver Generation](#resolver-generation)
6. [Advanced Patterns](#advanced-patterns)
7. [Performance Optimization](#performance-optimization)
8. [Testing Generated Code](#testing-generated-code)

---

## Architecture Overview

The GraphQL generation pipeline transforms RDF ontologies into production-grade GraphQL servers:

```
RDF Ontology (TTL)
       ↓
   [SPARQL Query]
       ↓
[Schema Rules] → GraphQL Schema (SDL)
       ↓
[Resolver Rules] → TypeScript Resolvers
       ↓
Apollo Server / Express GraphQL
```

### Key Principles

- **Type-safe**: GraphQL types match RDF constraints
- **Deterministic**: Same ontology → same schema every time
- **Composable**: Combine multiple ontologies for federation
- **Testable**: Generated code includes test fixtures

---

## RDF to GraphQL Mapping

### Entity Mapping

```turtle
# RDF: Define entity with properties
ex:User a rdfs:Class ;
  rdfs:label "User" ;
  rdfs:comment "System user" .

ex:userId a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "User ID" .

ex:userName a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "Username" .

ex:userEmail a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string .
```

Generates:

```graphql
"""User: System user"""
type User {
  """User ID"""
  userId: String!

  """Username"""
  userName: String!

  """Email address"""
  userEmail: String!
}
```

### Relationship Mapping

```turtle
# RDF: Define relationships
ex:Team a rdfs:Class .

ex:teamMembers a rdf:Property ;
  rdfs:domain ex:Team ;
  rdfs:range ex:User ;
  ex:cardinality "many" .

ex:teamLeader a rdf:Property ;
  rdfs:domain ex:Team ;
  rdfs:range ex:User ;
  ex:cardinality "one" .
```

Generates:

```graphql
type Team {
  teamId: String!
  teamName: String!

  """Single leader"""
  teamLeader: User!

  """Multiple members"""
  teamMembers: [User!]!
}
```

### Enumeration Mapping

```turtle
# RDF: Define enum
ex:UserRole a rdfs:Datatype ;
  ex:values ("admin" "user" "guest") .

ex:userRole a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range ex:UserRole .
```

Generates:

```graphql
enum UserRole {
  ADMIN
  USER
  GUEST
}

type User {
  userRole: UserRole!
}
```

---

## Complete Workflow

### Step 1: Design RDF Ontology

Create `ontology/api-schema.ttl`:

```turtle
@prefix ex: <http://example.org/api/>
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
@prefix xsd: <http://www.w3.org/2001/XMLSchema#>

# ===== Types =====

ex:User a rdfs:Class ;
  rdfs:label "User" .

ex:Post a rdfs:Class ;
  rdfs:label "Post" .

ex:Comment a rdfs:Class ;
  rdfs:label "Comment" .

# ===== User Properties =====

ex:userId a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:string ;
  rdfs:label "ID" ;
  ex:required true .

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
  ex:pattern "^[^@]+@[^@]+\\.[^@]+$" .

ex:userCreatedAt a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range xsd:dateTime ;
  rdfs:label "Created At" .

# ===== Relationships =====

ex:userPosts a rdf:Property ;
  rdfs:domain ex:User ;
  rdfs:range ex:Post ;
  ex:cardinality "many" .

ex:postAuthor a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range ex:User ;
  ex:cardinality "one" ;
  ex:required true .

# ===== Post Properties =====

ex:postId a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:required true .

ex:postTitle a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:required true ;
  ex:minLength 5 .

ex:postContent a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range xsd:string ;
  ex:required true .

ex:postComments a rdf:Property ;
  rdfs:domain ex:Post ;
  rdfs:range ex:Comment ;
  ex:cardinality "many" .

# ===== Comment Properties =====

ex:commentId a rdf:Property ;
  rdfs:domain ex:Comment ;
  rdfs:range xsd:string ;
  ex:required true .

ex:commentText a rdf:Property ;
  rdfs:domain ex:Comment ;
  rdfs:range xsd:string ;
  ex:required true .

ex:commentAuthor a rdf:Property ;
  rdfs:domain ex:Comment ;
  rdfs:range ex:User ;
  ex:required true .

# ===== Query Root =====

ex:QueryRoot a rdfs:Class ;
  rdfs:label "Query" .

ex:queryUser a rdf:Property ;
  rdfs:domain ex:QueryRoot ;
  rdfs:range ex:User ;
  ex:param "id" xsd:string ;
  ex:description "Get user by ID" .

ex:queryUsers a rdf:Property ;
  rdfs:domain ex:QueryRoot ;
  rdfs:range ex:User ;
  ex:cardinality "many" ;
  ex:param "skip" xsd:integer ;
  ex:param "take" xsd:integer ;
  ex:description "List users with pagination" .

ex:queryPost a rdf:Property ;
  rdfs:domain ex:QueryRoot ;
  rdfs:range ex:Post ;
  ex:param "id" xsd:string ;
  ex:description "Get post by ID" .
```

### Step 2: Configure Generation

Create `ggen.toml`:

```toml
[project]
name = "graphql-api-generator"

[ontology]
source = "ontology/api-schema.ttl"
base_iri = "http://example.org/api/"

[ontology.prefixes]
ex = "http://example.org/api/"
rdfs = "http://www.w3.org/2000/01/rdf-schema#"

[generation]
output_dir = "generated/"

# ===== GraphQL Schema Generation =====

[[generation.rules]]
name = "graphql-schema"
description = "Generate GraphQL SDL schema"
query = { inline = """
PREFIX ex: <http://example.org/api/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
SELECT ?typeName ?propertyName ?propertyType ?cardinality ?required ?description
WHERE {
  ?class a rdfs:Class ;
    rdfs:label ?typeName .
  ?prop rdfs:domain ?class ;
    rdfs:label ?propertyName ;
    rdfs:range ?propertyType .
  OPTIONAL { ?prop ex:cardinality ?cardinality }
  OPTIONAL { ?prop ex:required ?required }
  OPTIONAL { ?prop rdfs:comment ?description }
}
ORDER BY ?typeName ?propertyName
""" }
template = { file = "templates/graphql-schema.tera" }
output_file = "schema.graphql"

# ===== TypeScript Types Generation =====

[[generation.rules]]
name = "typescript-types"
description = "Generate TypeScript types for resolvers"
query = { inline = """
PREFIX ex: <http://example.org/api/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?typeName ?propertyName ?propertyType
WHERE {
  ?class a rdfs:Class ;
    rdfs:label ?typeName .
  ?prop rdfs:domain ?class ;
    rdfs:label ?propertyName ;
    rdfs:range ?propertyType .
}
ORDER BY ?typeName ?propertyName
""" }
template = { file = "templates/typescript-types.tera" }
output_file = "types.ts"

# ===== Resolver Templates Generation =====

[[generation.rules]]
name = "resolvers"
description = "Generate resolver function stubs"
query = { inline = """
PREFIX ex: <http://example.org/api/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
SELECT ?typeName ?fieldName
WHERE {
  ?class a rdfs:Class ;
    rdfs:label ?typeName .
  ?prop rdfs:domain ?class ;
    rdfs:label ?fieldName .
  FILTER (?typeName != "Query" && ?typeName != "Mutation")
}
ORDER BY ?typeName ?fieldName
""" }
template = { file = "templates/resolvers.tera" }
output_file = "resolvers.ts"
```

### Step 3: Create Tera Templates

Create `templates/graphql-schema.tera`:

```tera
# GraphQL Schema
# Generated from RDF ontology - DO NOT EDIT MANUALLY
# Source: {{ file }}
# Generated: {{ now | date(format="%Y-%m-%d %H:%M:%S") }}

{% set prev_type = "" %}
{% for row in results %}
  {% if row.typeName != prev_type %}
    {% if prev_type != "" %}
}

{% endif %}
"""{{ row.description | default(value='') }}"""
type {{ row.typeName }} {
    {% set prev_type = row.typeName %}
  {% endif %}
  {{ row.propertyName | camelCase }}: {% if row.cardinality == "many" %}[{{ row.propertyType | mapGraphQLType }}!]!{% elif row.required %}{{ row.propertyType | mapGraphQLType }}!{% else %}{{ row.propertyType | mapGraphQLType }}{% endif %}
{% endfor %}
}

# Query root
type Query {
  # Fields from QueryRoot type
  {% for row in results %}
    {% if row.typeName == "Query" %}
  {{ row.propertyName | camelCase }}: {{ row.propertyType | mapGraphQLType }}
    {% endif %}
  {% endfor %}
}
```

Create `templates/typescript-types.tera`:

```tera
// Generated TypeScript types for GraphQL resolvers
// DO NOT EDIT MANUALLY

{% set prev_type = "" %}
{% for row in results %}
  {% if row.typeName != prev_type %}
    {% if prev_type != "" %}
}

{% endif %}
export interface {{ row.typeName }} {
    {% set prev_type = row.typeName %}
  {% endif %}
  {{ row.propertyName | camelCase }}: {{ row.propertyType | mapTypeScriptType }};
{% endfor %}
}
```

Create `templates/resolvers.tera`:

```tera
// Generated resolver functions
// Implement the logic for each field resolver

import { {{ results | map(attribute="typeName") | unique | join(", ") }} } from './types';

{% for row in results %}
/**
 * Resolver for {{ row.typeName }}.{{ row.fieldName }}
 */
export const resolve{{ row.typeName }}{{ row.fieldName | pascalCase }} = async (
  parent: {{ row.typeName }},
  args: any,
  context: any
): Promise<any> => {
  // TODO: Implement resolver logic
  throw new Error('Not implemented');
};

{% endfor %}
```

### Step 4: Run Generation

```bash
# Generate all files
ggen-cli render

# Check output
ls -la generated/
# schema.graphql
# types.ts
# resolvers.ts
```

---

## Schema Generation

### Generated GraphQL Schema

After running `ggen-cli render`, you get:

```graphql
# GraphQL Schema
# Generated from RDF ontology

type User {
  userId: String!
  userName: String!
  userEmail: String!
  userCreatedAt: DateTime
  userPosts: [Post!]!
}

type Post {
  postId: String!
  postTitle: String!
  postContent: String!
  postAuthor: User!
  postComments: [Comment!]!
}

type Comment {
  commentId: String!
  commentText: String!
  commentAuthor: User!
}

type Query {
  user(id: String!): User
  users(skip: Int, take: Int): [User!]!
  post(id: String!): Post
}
```

---

## Resolver Generation

### Implementing Resolvers

Update `generated/resolvers.ts`:

```typescript
import { User, Post, Comment } from './types';

// In-memory database (replace with real DB)
const users: Map<string, User> = new Map([
  ['1', { userId: '1', userName: 'alice', userEmail: 'alice@example.com' }],
  ['2', { userId: '2', userName: 'bob', userEmail: 'bob@example.com' }],
]);

const posts: Map<string, Post> = new Map([
  ['p1', {
    postId: 'p1',
    postTitle: 'GraphQL Basics',
    postContent: 'A guide to GraphQL...',
    postAuthorId: '1',
  }],
]);

// Query resolvers
export const resolvers = {
  Query: {
    user: async (_, { id }: { id: string }) => {
      return users.get(id) || null;
    },

    users: async (_, { skip = 0, take = 10 }: { skip?: number; take?: number }) => {
      return Array.from(users.values()).slice(skip, skip + take);
    },

    post: async (_, { id }: { id: string }) => {
      return posts.get(id) || null;
    },
  },

  // Field resolvers for relationships
  User: {
    userPosts: async (parent: User) => {
      // Find all posts by this user
      return Array.from(posts.values())
        .filter(p => p.postAuthorId === parent.userId);
    },
  },

  Post: {
    postAuthor: async (parent: Post) => {
      return users.get(parent.postAuthorId) || null;
    },
  },

  Comment: {
    commentAuthor: async (parent: Comment) => {
      return users.get(parent.commentAuthorId) || null;
    },
  },
};
```

### Apollo Server Integration

Create `src/server.ts`:

```typescript
import { ApolloServer } from 'apollo-server-express';
import express from 'express';
import { readFileSync } from 'fs';
import { join } from 'path';
import { resolvers } from '../generated/resolvers';

const app = express();
const port = 4000;

// Load generated schema
const typeDefs = readFileSync(
  join(__dirname, '../generated/schema.graphql'),
  'utf-8'
);

// Create Apollo Server
const server = new ApolloServer({
  typeDefs,
  resolvers,
  context: async ({ req }) => {
    // Add context (auth, database, etc.)
    return {
      userId: req.headers['x-user-id'],
      db: {}, // Database connection
    };
  },
});

await server.start();
server.applyMiddleware({ app });

app.listen(port, () => {
  console.log(`GraphQL server running on http://localhost:${port}/graphql`);
});
```

---

## Advanced Patterns

### Pattern 1: Pagination

```turtle
ex:UserConnection a rdfs:Class ;
  rdfs:label "UserConnection" .

ex:connectionEdges a rdf:Property ;
  rdfs:domain ex:UserConnection ;
  rdfs:range ex:UserEdge ;
  ex:cardinality "many" .

ex:pageInfo a rdf:Property ;
  rdfs:domain ex:UserConnection ;
  rdfs:range ex:PageInfo ;
  ex:required true .

ex:PageInfo a rdfs:Class ;
  rdfs:label "PageInfo" .

ex:hasNextPage a rdf:Property ;
  rdfs:domain ex:PageInfo ;
  rdfs:range xsd:boolean ;
  ex:required true .

ex:hasPreviousPage a rdf:Property ;
  rdfs:domain ex:PageInfo ;
  rdfs:range xsd:boolean ;
  ex:required true .
```

Generates:

```graphql
type UserConnection {
  edges: [UserEdge!]!
  pageInfo: PageInfo!
}

type PageInfo {
  hasNextPage: Boolean!
  hasPreviousPage: Boolean!
}
```

### Pattern 2: Mutations

```turtle
ex:CreateUserInput a rdfs:Class ;
  rdfs:label "CreateUserInput" .

ex:createUserMutation a rdf:Property ;
  rdfs:domain ex:Mutation ;
  rdfs:range ex:CreateUserPayload ;
  ex:inputType "CreateUserInput" .

ex:CreateUserPayload a rdfs:Class ;
  rdfs:label "CreateUserPayload" .

ex:payloadUser a rdf:Property ;
  rdfs:domain ex:CreateUserPayload ;
  rdfs:range ex:User ;
  ex:required true .
```

Generates:

```graphql
input CreateUserInput {
  userName: String!
  userEmail: String!
}

type CreateUserPayload {
  user: User!
  errors: [String!]!
}

type Mutation {
  createUser(input: CreateUserInput!): CreateUserPayload!
}
```

### Pattern 3: Subscriptions

```turtle
ex:userCreated a rdf:Property ;
  rdfs:domain ex:Subscription ;
  rdfs:range ex:User ;
  ex:event "user.created" .
```

Generates:

```graphql
type Subscription {
  userCreated: User!
}
```

---

## Performance Optimization

### DataLoader for N+1 Prevention

```typescript
import DataLoader from 'dataloader';

export const createDataLoaders = () => {
  const userLoader = new DataLoader(async (userIds: string[]) => {
    // Batch load users
    const users = await db.users.findByIds(userIds);
    return userIds.map(id => users.get(id) || null);
  });

  return {
    userLoader,
  };
};

// In resolver
Post: {
  postAuthor: async (parent: Post, _, { loaders }: any) => {
    return loaders.userLoader.load(parent.postAuthorId);
  },
}
```

### Query Depth Limiting

```typescript
import { depthLimit } from 'graphql-depth-limit';

new ApolloServer({
  typeDefs,
  resolvers,
  plugins: [
    depthLimit(5), // Max query depth of 5
  ],
});
```

### Fragment Caching

```typescript
// Cache frequently used fragments
const UserFragment = gql`
  fragment UserBasic on User {
    userId
    userName
    userEmail
  }
`;
```

---

## Testing Generated Code

### GraphQL Query Testing

```typescript
import { graphql } from 'graphql';
import { buildSchema } from 'graphql';
import { readFileSync } from 'fs';

const schemaString = readFileSync('generated/schema.graphql', 'utf-8');
const schema = buildSchema(schemaString);

describe('GraphQL API', () => {
  it('should get user by ID', async () => {
    const query = `
      query {
        user(id: "1") {
          userId
          userName
          userEmail
        }
      }
    `;

    const result = await graphql({
      schema,
      source: query,
      rootValue: resolvers,
    });

    expect(result.data?.user).toBeDefined();
    expect(result.data?.user.userId).toBe('1');
  });
});
```

### Schema Validation

```bash
# Generate schema and validate it
ggen-cli render

# Check schema for common errors
graphql-schema-linter generated/schema.graphql

# Validate queries against schema
apollo client:check generated/schema.graphql
```

---

## See Also

- [JavaScript Express Example](./JAVASCRIPT_EXPRESS_EXAMPLE.md) - REST API generation
- [PostgreSQL Integration](./POSTGRESQL_INTEGRATION_EXAMPLE.md) - Database patterns
- [SPARQL Inference Guide](../SPARQL_INFERENCE_GUIDE.md) - Query optimization
