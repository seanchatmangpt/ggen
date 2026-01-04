# Advanced GraphQL Code Generation Patterns

> Hyper-advanced GraphQL patterns for federation, subscriptions, and schema composition driven by RDF ontology

## Overview

This guide covers five advanced GraphQL patterns that ggen can generate from RDF ontologies, including Apollo Federation, real-time subscriptions, custom directives, query optimization, and schema composition.

## Pattern 1: Apollo Federation with Entity References

### Description

Generate multi-domain GraphQL federation with entity references, subgraph composition, and automatic gateway orchestration from a single RDF ontology.

### External Packages

```json
{
  "@apollo/subgraph": "^2.7.3",
  "@apollo/gateway": "^2.7.3",
  "@apollo/federation": "^0.38.1",
  "@apollo/server": "^4.10.0",
  "graphql": "^16.8.1",
  "@apollo/composition": "^2.7.3",
  "graphql-tag": "^2.12.6"
}
```

### RDF Ontology for Federation

```turtle
@prefix gql: <http://ggen.io/ontology/graphql#>

gql:UserService a gql:FederatedService ;
  gql:serviceName "users" ;
  gql:serviceUrl "http://localhost:4001/graphql" ;
  gql:entities gql:UserEntity ;
  gql:queries (
    [
      gql:fieldName "user" ;
      gql:args [
        [
          gql:argName "id" ;
          gql:argType "ID!"
        ]
      ] ;
      gql:returnType gql:UserEntity
    ]
  ) .

gql:UserEntity a gql:FederatedEntity, gql:ObjectType ;
  gql:typeName "User" ;
  gql:keyFields "id" ;
  gql:hasField (
    [
      gql:fieldName "id" ;
      gql:fieldType "ID!" ;
      gql:isKey true
    ]
    [
      gql:fieldName "email" ;
      gql:fieldType "String!"
    ]
    [
      gql:fieldName "name" ;
      gql:fieldType "String!"
    ]
  ) .

gql:ProductService a gql:FederatedService ;
  gql:serviceName "products" ;
  gql:serviceUrl "http://localhost:4002/graphql" ;
  gql:entities gql:ProductEntity .

gql:ProductEntity a gql:FederatedEntity, gql:ObjectType ;
  gql:typeName "Product" ;
  gql:keyFields "sku" ;
  gql:hasField (
    [
      gql:fieldName "sku" ;
      gql:fieldType "ID!" ;
      gql:isKey true
    ]
    [
      gql:fieldName "name" ;
      gql:fieldType "String!"
    ]
    [
      gql:fieldName "createdBy" ;
      gql:fieldType gql:UserEntity ;
      gql:extendsFrom gql:UserService ;
      gql:referenceResolver gql:ResolveUserReference
    ]
  ) .

gql:Gateway a gql:FederationGateway ;
  gql:supergraphSchema "generated/supergraph.graphql" ;
  gql:pollIntervalMs 10000 ;
  gql:introspectionEnabled true .
```

### Generated Users Subgraph

```typescript
// users-service/index.ts
import { ApolloServer } from '@apollo/server';
import { startStandaloneServer } from '@apollo/server/standalone';
import { buildSubgraphSchema } from '@apollo/subgraph';
import gql from 'graphql-tag';

const typeDefs = gql`
  extend schema
    @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@shareable"])

  type User @key(fields: "id") {
    id: ID!
    email: String!
    name: String!
    createdAt: DateTime!
  }

  type Query {
    user(id: ID!): User
    users: [User!]!
  }
`;

const resolvers = {
  Query: {
    user: (_, { id }) => users.find(u => u.id === id),
    users: () => users,
  },
  User: {
    __resolveReference(reference) {
      return users.find(u => u.id === reference.id);
    },
  },
};

const server = new ApolloServer({
  schema: buildSubgraphSchema([{ typeDefs, resolvers }]),
});

const { url } = await startStandaloneServer(server, { port: 4001 });
console.log(`Users subgraph ready at ${url}`);
```

### Generated Products Subgraph with Entity Extensions

```typescript
// products-service/index.ts
import { ApolloServer } from '@apollo/server';
import { buildSubgraphSchema } from '@apollo/subgraph';
import gql from 'graphql-tag';

const typeDefs = gql`
  extend schema
    @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "@external"])

  type Product @key(fields: "sku") {
    sku: ID!
    name: String!
    price: Float!
    createdBy: User!
  }

  type User @key(fields: "id", resolvable: false) {
    id: ID!
  }

  type Query {
    product(sku: ID!): Product
    products: [Product!]!
  }
`;

const resolvers = {
  Product: {
    __resolveReference(reference) {
      return products.find(p => p.sku === reference.sku);
    },
    createdBy(product) {
      return { __typename: 'User', id: product.createdById };
    },
  },
};

const server = new ApolloServer({
  schema: buildSubgraphSchema([{ typeDefs, resolvers }]),
});

const { url } = await startStandaloneServer(server, { port: 4002 });
console.log(`Products subgraph ready at ${url}`);
```

### Generated Federation Gateway

```typescript
// gateway/index.ts
import { ApolloServer } from '@apollo/server';
import { ApolloGateway, IntrospectAndCompose } from '@apollo/gateway';

const gateway = new ApolloGateway({
  supergraphSdl: new IntrospectAndCompose({
    subgraphs: [
      { name: 'users', url: 'http://localhost:4001/graphql' },
      { name: 'products', url: 'http://localhost:4002/graphql' },
    ],
    pollIntervalInMs: 10000,
  }),
});

const server = new ApolloServer({ gateway });
const { url } = await startStandaloneServer(server, { port: 4000 });
console.log(`Gateway ready at ${url}`);
```

### Implementation Complexity

- **Lines of Code**: 650
- **Files Generated**: 7 (3 subgraphs + gateway + utilities + tests)
- **Templates Needed**: 4
- **Effort Estimate**: 3-4 days

---

## Pattern 2: Real-Time Subscriptions with WebSocket & PubSub

### Description

Generate real-time GraphQL subscriptions with WebSocket transport, event streaming, and distributed PubSub systems.

### External Packages

```json
{
  "graphql-ws": "^5.14.3",
  "ws": "^8.16.0",
  "graphql-subscriptions": "^2.0.0",
  "graphql-redis-subscriptions": "^2.6.0",
  "ioredis": "^5.3.2",
  "@google-cloud/pubsub": "^4.1.1"
}
```

### RDF Ontology for Subscriptions

```turtle
@prefix gql: <http://ggen.io/ontology/graphql#>
@prefix sub: <http://ggen.io/ontology/subscription#>

sub:PostCreatedEvent a gql:SubscriptionField ;
  gql:fieldName "postCreated" ;
  gql:returnType gql:Post ;
  sub:pubsubChannel "post.created" ;
  sub:filterFunction "matchesByAuthorId" ;
  sub:payloadTransform "extractPost" .

sub:CommentAddedEvent a gql:SubscriptionField ;
  gql:fieldName "commentAdded" ;
  gql:args [
    [
      gql:argName "postId" ;
      gql:argType "ID!"
    ]
  ] ;
  gql:returnType gql:Comment ;
  sub:pubsubChannel "comment.added" ;
  sub:filterFunction "matchesByPostId" ;
  sub:payloadTransform "extractComment" .

sub:UserOnlineStatus a gql:SubscriptionField ;
  gql:fieldName "userStatusChanged" ;
  gql:returnType gql:UserStatus ;
  sub:pubsubEngine "redis" ;
  sub:pubsubChannel "user.status" ;
  sub:bufferingPolicy "latest" .
```

### Generated Subscriptions Code

```typescript
import { PubSubEngine } from 'graphql-subscriptions';
import { RedisPubSub } from 'graphql-redis-subscriptions';
import Redis from 'ioredis';
import { PubSub } from '@google-cloud/pubsub';

// Initialize PubSub (can be Redis, Google Cloud Pub/Sub, or in-memory)
const pubSub = new RedisPubSub({
  connection: new Redis(),
});

const typeDefs = gql`
  type Subscription {
    postCreated(authorId: ID!): Post!
    commentAdded(postId: ID!): Comment!
    userStatusChanged: UserStatus!
  }
`;

const resolvers = {
  Subscription: {
    // Single-argument subscription with filter
    postCreated: {
      subscribe: (_, { authorId }) => {
        return pubSub.asyncIterator(['post.created']).then(iterator => {
          return {
            [Symbol.asyncIterator]: async function* () {
              for await (const message of iterator) {
                if (message.post.authorId === authorId) {
                  yield message;
                }
              }
            }
          };
        });
      },
    },

    // Multi-argument subscription
    commentAdded: {
      subscribe: (_, { postId }) => {
        return pubSub.asyncIterator([`comment.added:${postId}`]);
      },
      resolve: (payload) => payload.comment,
    },

    // Always-on subscription with buffering
    userStatusChanged: {
      subscribe: async () => {
        return pubSub.asyncIterator(['user.status']);
      },
    },
  },
};

// Server integration
const server = new ApolloServer({
  typeDefs,
  resolvers,
});

// Socket server with graphql-ws
const wsServer = new WebSocketServer({
  server: httpServer,
  path: '/graphql',
});

const serverCleanup = useServer({ schema }, wsServer);

httpServer.listen(4000, () => {
  console.log('Server listening on :4000');
});
```

### Publishing Events

```typescript
// Publish event from resolver
const postResolver = {
  Mutation: {
    createPost: async (_, { title, content, authorId }) => {
      const post = await db.posts.create({ title, content, authorId });

      // Publish to subscribers
      await pubSub.publish('post.created', {
        postCreated: post,
      });

      return post;
    },
  },
};

// Publish from external service
export async function publishUserStatusChange(
  userId: string,
  status: 'online' | 'offline'
) {
  await pubSub.publish('user.status', {
    userStatusChanged: { userId, status, timestamp: new Date() },
  });
}
```

---

## Pattern 3: Custom Directives & Schema Transforms

### Description

Generate custom GraphQL directives for automatic validation, caching, rate limiting, and field-level authorization.

### External Packages

```json
{
  "@graphql-tools/schema": "^10.0.0",
  "graphql-directive": "^1.0.0",
  "graphql-directive-auth": "^1.2.0",
  "graphql-directive-validate": "^1.1.0"
}
```

### RDF Ontology for Directives

```turtle
@prefix gql: <http://ggen.io/ontology/graphql#>
@prefix dir: <http://ggen.io/ontology/directive#>

dir:AuthDirective a gql:CustomDirective ;
  dir:name "auth" ;
  dir:locations ("FIELD_DEFINITION" "OBJECT") ;
  dir:args [
    [
      dir:argName "requires" ;
      dir:argType "[String!]!" ;
      dir:description "Required roles/permissions"
    ]
  ] ;
  dir:implementationLanguage "typescript" .

dir:CacheDirective a gql:CustomDirective ;
  dir:name "cached" ;
  dir:locations ("FIELD_DEFINITION") ;
  dir:args [
    [
      dir:argName "ttl" ;
      dir:argType "Int!" ;
      dir:defaultValue 300
    ]
  ] ;
  dir:cacheBackend "redis" .

dir:ValidateDirective a gql:CustomDirective ;
  dir:name "validate" ;
  dir:locations ("INPUT_FIELD_DEFINITION") ;
  dir:args [
    [
      dir:argName "pattern" ;
      dir:argType "String!" ;
      dir:description "Regex pattern"
    ]
    [
      dir:argName "minLength" ;
      dir:argType "Int"
    ]
    [
      dir:argName "maxLength" ;
      dir:argType "Int"
    ]
  ] .

dir:RateLimitDirective a gql:CustomDirective ;
  dir:name "rateLimit" ;
  dir:locations ("FIELD_DEFINITION") ;
  dir:args [
    [
      dir:argName "limit" ;
      dir:argType "Int!" ;
      dir:description "Requests per window"
    ]
    [
      dir:argName "window" ;
      dir:argType "Int!" ;
      dir:description "Time window in seconds"
    ]
  ] .
```

### Generated Directive Implementations

```typescript
import { getDirective, mapSchema, MapperKind } from '@graphql-tools/utils';
import { defaultFieldResolver } from 'graphql';
import Redis from 'ioredis';

const redis = new Redis();

export function createAuthDirective(schema) {
  return mapSchema(schema, {
    [MapperKind.OBJECT_FIELD]: (fieldConfig, fieldName, typeName) => {
      const authDirective = getDirective(schema, fieldConfig, 'auth')?.[0];

      if (authDirective) {
        const { requires } = authDirective;
        const originalResolve = fieldConfig.resolve || defaultFieldResolver;

        fieldConfig.resolve = async (source, args, context, info) => {
          // Check authorization
          const userRoles = context.user?.roles || [];
          const hasPermission = requires.some(role => userRoles.includes(role));

          if (!hasPermission) {
            throw new Error(`Unauthorized: requires ${requires.join(', ')}`);
          }

          return originalResolve(source, args, context, info);
        };
      }

      return fieldConfig;
    },
  });
}

export function createCacheDirective(schema) {
  return mapSchema(schema, {
    [MapperKind.OBJECT_FIELD]: (fieldConfig, fieldName, typeName) => {
      const cacheDirective = getDirective(schema, fieldConfig, 'cached')?.[0];

      if (cacheDirective) {
        const { ttl = 300 } = cacheDirective;
        const originalResolve = fieldConfig.resolve || defaultFieldResolver;

        fieldConfig.resolve = async (source, args, context, info) => {
          const cacheKey = `${typeName}:${fieldName}:${JSON.stringify(args)}`;
          const cached = await redis.get(cacheKey);

          if (cached) {
            return JSON.parse(cached);
          }

          const result = await originalResolve(source, args, context, info);
          await redis.setex(cacheKey, ttl, JSON.stringify(result));

          return result;
        };
      }

      return fieldConfig;
    },
  });
}

export function createValidateDirective(schema) {
  return mapSchema(schema, {
    [MapperKind.INPUT_OBJECT_FIELD]: (inputFieldConfig) => {
      const validateDirective = getDirective(
        schema,
        inputFieldConfig,
        'validate'
      )?.[0];

      if (validateDirective) {
        const { pattern, minLength, maxLength } = validateDirective;
        const originalCoerce = inputFieldConfig.coerce;

        inputFieldConfig.coerce = (value) => {
          if (pattern && typeof value === 'string') {
            const regex = new RegExp(pattern);
            if (!regex.test(value)) {
              throw new Error(`Value does not match pattern: ${pattern}`);
            }
          }

          if (minLength && value.length < minLength) {
            throw new Error(`Minimum length: ${minLength}`);
          }

          if (maxLength && value.length > maxLength) {
            throw new Error(`Maximum length: ${maxLength}`);
          }

          return originalCoerce ? originalCoerce(value) : value;
        };
      }

      return inputFieldConfig;
    },
  });
}
```

### Usage in Schema

```graphql
type Query {
  # Requires admin or moderator role
  users: [User!]! @auth(requires: ["admin", "moderator"])

  # Cached for 5 minutes
  getUser(id: ID!): User @cached(ttl: 300)

  # Rate limited to 100 requests per minute
  searchPosts(query: String!): [Post!]! @rateLimit(limit: 100, window: 60)
}

input CreateUserInput {
  # Email validation via directive
  email: String! @validate(pattern: "^[^@]+@[^@]+\\.[^@]+$")

  # Username length validation
  username: String! @validate(minLength: 3, maxLength: 32)
}
```

---

## Pattern 4: DataLoader for N+1 Query Prevention

### Description

Automatically generate DataLoader instances to batch and cache database queries, preventing N+1 query problems.

### External Packages

```json
{
  "dataloader": "^2.2.2"
}
```

### RDF Ontology for DataLoading

```turtle
@prefix gql: <http://ggen.io/ontology/graphql#>
@prefix dl: <http://ggen.io/ontology/dataloader#>

dl:UserBatchLoader a dl:DataLoader ;
  dl:entityType gql:User ;
  dl:batchFunction "loadUsersByIds" ;
  dl:cacheEnabled true ;
  dl:maxBatchSize 100 .

dl:PostsBatchLoader a dl:DataLoader ;
  dl:entityType gql:Post ;
  dl:batchFunction "loadPostsByAuthorIds" ;
  dl:groupByField "authorId" ;
  dl:cacheEnabled true .
```

### Generated Code

```typescript
import DataLoader from 'dataloader';

// User batch loader
const userLoader = new DataLoader(async (userIds) => {
  const users = await db.query(`
    SELECT * FROM users WHERE id = ANY($1)
  `, [userIds]);

  // Return results in the same order as input IDs
  return userIds.map(id => users.find(u => u.id === id));
}, {
  batchScheduleFn: (callback) => setImmediate(callback),
});

// Posts grouped batch loader
const postsByAuthorLoader = new DataLoader(async (authorIds) => {
  const posts = await db.query(`
    SELECT * FROM posts WHERE author_id = ANY($1)
  `, [authorIds]);

  // Group results by author_id
  return authorIds.map(authorId =>
    posts.filter(p => p.author_id === authorId)
  );
});

const resolvers = {
  Query: {
    users: async () => {
      const userIds = await db.query('SELECT id FROM users LIMIT 10');
      return Promise.all(userIds.map(({ id }) => userLoader.load(id)));
    },
  },

  User: {
    posts: async (user, _, { dataloaders }) => {
      return postsByAuthorLoader.load(user.id);
    },
  },
};

// Pass loaders via context
const server = new ApolloServer({
  typeDefs,
  resolvers,
  context: () => ({
    dataloaders: {
      userLoader,
      postsByAuthorLoader,
    },
  }),
});
```

---

## Pattern 5: Schema Composition & Merging

### Description

Generate tools for composing multiple GraphQL schemas into a unified graph with conflict resolution and type merging.

---

## Tera Template Examples

### Subgraph Schema Template

```tera
{% for service in services %}
extend schema
  @link(url: "https://specs.apollo.dev/federation/v2.3", import: ["@key", "{{service.directives | join(", ")}}"])

{% for entity in service.entities %}
type {{ entity.typeName }} @key(fields: "{{ entity.keyFields | join(", ") }}") {
  {% for field in entity.fields %}
  {{ field.name }}: {{ field.type }}{{ field.required ? "!" : "" }}
  {% endfor %}
}
{% endfor %}

type Query {
  {% for query in service.queries %}
  {{ query.name }}({% for arg in query.args %}{{ arg.name }}: {{ arg.type }}{% endfor %}): {{ query.returnType }}
  {% endfor %}
}
{% endfor %}
```

---

## Best Practices

1. **Federation First**: Design for federation to enable independent deployment
2. **Entity References**: Use @key directives for type-safe entity resolution
3. **Directive Composition**: Layer directives for auth, caching, and validation
4. **DataLoader Always**: Prevent N+1 queries with automatic batching
5. **Subscription Scaling**: Use Redis or Cloud Pub/Sub for distributed subscriptions
6. **Schema Registry**: Maintain schema versions for compatibility checking

---

## References

- [Apollo Federation Documentation](https://www.apollographql.com/docs/federation/)
- [GraphQL WebSocket Protocol](https://github.com/enisdenjo/graphql-ws)
- [DataLoader Documentation](https://github.com/graphql/dataloader)
- [GraphQL Directive Guide](https://graphql.org/learn/queries/#directives)

## See Also

- [ADVANCED_TYPESCRIPT_PATTERNS.md](./ADVANCED_TYPESCRIPT_PATTERNS.md) - TypeScript code generation
- [ADVANCED_DEPLOYMENT_PATTERNS.md](./ADVANCED_DEPLOYMENT_PATTERNS.md) - Deployment strategies
- [GRAPHQL_DEEP_DIVE.md](../how-to-guides/GRAPHQL_DEEP_DIVE.md) - Base GraphQL guide
