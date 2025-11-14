# GraphQL Resolver Patterns

This document explains the resolver patterns used in the GraphQL API template.

## Resolver Architecture

### Basic Resolver Structure

```typescript
const resolvers = {
  Query: {
    // Field resolver
    async user(parent, args, context, info) {
      return context.loaders.userById.load(args.id);
    }
  },

  User: {
    // Nested field resolver
    async posts(parent, args, context) {
      return context.loaders.postsByUser.load(parent.id);
    }
  }
};
```

### Resolver Parameters

1. **parent**: Result from parent resolver
2. **args**: Field arguments from query
3. **context**: Shared context (database, loaders, user)
4. **info**: Query metadata and schema info

## DataLoader Integration

### Why DataLoaders?

Without DataLoaders (N+1 problem):
```
Query: Get 10 users with their posts
Result: 1 query for users + 10 queries for posts = 11 queries
```

With DataLoaders:
```
Query: Get 10 users with their posts
Result: 1 query for users + 1 batched query for posts = 2 queries
```

### DataLoader Implementation

```typescript
// Create loader
const userLoader = new DataLoader(async (ids: string[]) => {
  // Single batched query
  const users = await db.query(
    'SELECT * FROM users WHERE id = ANY($1)',
    [ids]
  );

  // Map results back to keys
  const userMap = new Map(users.map(u => [u.id, u]));
  return ids.map(id => userMap.get(id) || null);
});

// Use in resolver
async author(parent, _args, context) {
  // Automatically batched!
  return context.loaders.userById.load(parent.authorId);
}
```

### Caching

DataLoaders cache within a single request:

```typescript
// Same request
const user1 = await loader.load('1'); // Database query
const user2 = await loader.load('1'); // From cache
```

Disable caching if needed:
```typescript
const loader = new DataLoader(batchFn, { cache: false });
```

## Authorization Patterns

### Directive-Based Authorization

```graphql
type Mutation {
  createUser(input: CreateUserInput!): User! @auth(requires: ADMIN)
}
```

Implementation:
```typescript
function authDirective(next, source, args, context) {
  if (!context.user) {
    throw new GraphQLError('Not authenticated');
  }

  if (args.requires === 'ADMIN' && context.user.role !== 'ADMIN') {
    throw new GraphQLError('Insufficient permissions');
  }

  return next();
}
```

### Guard-Based Authorization (Rust)

```rust
#[Object]
impl MutationRoot {
    #[graphql(guard = "RoleGuard::new(Role::Admin)")]
    async fn create_user(&self, ctx: &Context<'_>, input: CreateUserInput) -> Result<User> {
        // Only admins reach this code
    }
}
```

### Field-Level Authorization

```typescript
User: {
  async email(parent, _args, context) {
    // Only return email if viewing own profile or admin
    if (context.user?.id === parent.id || context.user?.role === 'ADMIN') {
      return parent.email;
    }
    return null;
  }
}
```

## Pagination Resolvers

### Cursor-Based Pagination

```typescript
async users(parent, { first = 10, after }, context) {
  const limit = Math.min(first, 100); // Max 100 per page
  const offset = after ? decodeCursor(after) : 0;

  // Fetch one extra to detect hasNextPage
  const users = await db.query(
    'SELECT * FROM users ORDER BY id LIMIT $1 OFFSET $2',
    [limit + 1, offset]
  );

  const hasNextPage = users.length > limit;
  const nodes = hasNextPage ? users.slice(0, -1) : users;

  const edges = nodes.map((user, i) => ({
    node: user,
    cursor: encodeCursor(offset + i)
  }));

  return {
    edges,
    pageInfo: {
      hasNextPage,
      hasPreviousPage: offset > 0,
      startCursor: edges[0]?.cursor,
      endCursor: edges[edges.length - 1]?.cursor
    }
  };
}
```

### Cursor Encoding

```typescript
function encodeCursor(offset: number): string {
  return Buffer.from(offset.toString()).toString('base64');
}

function decodeCursor(cursor: string): number {
  return parseInt(Buffer.from(cursor, 'base64').toString());
}
```

## Subscription Resolvers

### PubSub Pattern

```typescript
const pubsub = new PubSub();

const resolvers = {
  Mutation: {
    async createUser(parent, { input }, context) {
      const user = await db.createUser(input);

      // Publish event
      pubsub.publish('USER_CREATED', { userCreated: user });

      return user;
    }
  },

  Subscription: {
    userCreated: {
      subscribe: () => pubsub.asyncIterator(['USER_CREATED'])
    }
  }
};
```

### Filtered Subscriptions

```typescript
Subscription: {
  postUpdated: {
    subscribe: () => pubsub.asyncIterator(['POST_UPDATED']),
    resolve: (payload, { userId }) => {
      // Filter events client-side
      if (userId && payload.postUpdated.authorId !== userId) {
        return null;
      }
      return payload.postUpdated;
    }
  }
}
```

### AsyncIterator Pattern (Rust)

```rust
#[Subscription]
impl SubscriptionRoot {
    async fn user_created(&self) -> impl Stream<Item = User> {
        SimpleBroker::<UserEvent>::subscribe()
            .filter_map(|event| async move {
                match event {
                    UserEvent::Created(user) => Some(user),
                    _ => None,
                }
            })
    }
}
```

## Error Handling

### GraphQL Errors

```typescript
async user(parent, { id }, context) {
  const user = await context.loaders.userById.load(id);

  if (!user) {
    throw new GraphQLError('User not found', {
      extensions: {
        code: 'USER_NOT_FOUND',
        userId: id
      }
    });
  }

  return user;
}
```

### Error Formatting

```typescript
const server = new ApolloServer({
  typeDefs,
  resolvers,
  formatError: (error) => {
    // Log internal errors
    if (error.extensions?.code === 'INTERNAL_SERVER_ERROR') {
      console.error(error);
    }

    // Return safe error to client
    return {
      message: error.message,
      code: error.extensions?.code,
      path: error.path
    };
  }
});
```

## Input Validation

### TypeScript Validation

```typescript
import { validate, Length, IsEmail } from 'class-validator';

async createUser(parent, { input }, context) {
  // Validate input
  const errors = await validate(input);
  if (errors.length > 0) {
    throw new GraphQLError('Validation failed', {
      extensions: { validationErrors: errors }
    });
  }

  return db.createUser(input);
}
```

### Rust Validation

```rust
#[derive(InputObject, Validate)]
pub struct CreateUserInput {
    #[validate(length(min = 1, max = 255))]
    pub name: String,

    #[validate(email, length(max = 255))]
    pub email: String,
}

async fn create_user(&self, ctx: &Context<'_>, input: CreateUserInput) -> Result<User> {
    input.validate()
        .map_err(|e| Error::new(format!("Validation error: {}", e)))?;

    // Create user
}
```

## Performance Optimization

### Query Complexity

```typescript
const server = new ApolloServer({
  typeDefs,
  resolvers,
  plugins: [
    {
      requestDidStart: () => ({
        didResolveOperation({ request, document }) {
          const complexity = calculateComplexity({
            schema,
            query: document,
            variables: request.variables
          });

          if (complexity > 1000) {
            throw new GraphQLError('Query too complex');
          }
        }
      })
    }
  ]
});
```

### Field-Level Caching

```graphql
type Query {
  user(id: ID!): User @cacheControl(maxAge: 300)
}
```

### Database Query Optimization

```typescript
// BAD: N+1 queries
async posts(parent) {
  return db.query('SELECT * FROM posts WHERE author_id = $1', [parent.id]);
}

// GOOD: DataLoader batching
async posts(parent, _args, context) {
  return context.loaders.postsByUser.load(parent.id);
}
```

## Best Practices

1. **Always use DataLoaders** for relationships
2. **Implement pagination** for list fields
3. **Add authorization** at field level when needed
4. **Validate inputs** before database operations
5. **Handle errors** gracefully with proper codes
6. **Cache aggressively** for read-heavy fields
7. **Monitor query complexity** to prevent abuse
8. **Use subscriptions** sparingly (they're resource-intensive)
9. **Batch database queries** whenever possible
10. **Test resolvers** with integration tests

## Testing Resolvers

```typescript
describe('User resolver', () => {
  it('loads user by ID', async () => {
    const context = createTestContext();
    const user = await resolvers.Query.user(null, { id: '1' }, context);

    expect(user).toMatchObject({
      id: '1',
      name: 'Test User'
    });
  });

  it('batches user loads', async () => {
    const context = createTestContext();
    const spy = jest.spyOn(context.pool, 'query');

    // Load multiple users
    await Promise.all([
      resolvers.Query.user(null, { id: '1' }, context),
      resolvers.Query.user(null, { id: '2' }, context)
    ]);

    // Should only query database once (batched)
    expect(spy).toHaveBeenCalledTimes(1);
  });
});
```
