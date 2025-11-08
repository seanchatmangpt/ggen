# GraphQL Subscriptions Guide

Real-time event streaming using GraphQL subscriptions over WebSockets.

## Overview

Subscriptions enable servers to push data to clients in real-time. Unlike queries and mutations (request-response), subscriptions maintain an open connection.

## Architecture

```
Client                Server
  |                      |
  |--- SUBSCRIBE ------>|  (WebSocket connection)
  |                      |
  |                   [Event occurs]
  |                      |
  |<--- EVENT DATA ------|
  |                      |
  |<--- EVENT DATA ------|  (Multiple events)
  |                      |
  |--- UNSUBSCRIBE ---->|
  |                      |
```

## Basic Subscription

### Schema Definition

```graphql
type Subscription {
  userCreated: User!
  postUpdated(userId: ID): Post!
  messageAdded(roomId: ID!): Message!
}
```

### TypeScript Implementation

```typescript
import { PubSub } from 'graphql-subscriptions';

const pubsub = new PubSub();

const resolvers = {
  Subscription: {
    userCreated: {
      subscribe: () => pubsub.asyncIterator(['USER_CREATED'])
    }
  },

  Mutation: {
    async createUser(parent, { input }, context) {
      const user = await db.createUser(input);

      // Publish event to all subscribers
      pubsub.publish('USER_CREATED', { userCreated: user });

      return user;
    }
  }
};
```

### Client Usage

```typescript
import { createClient } from 'graphql-ws';

const client = createClient({
  url: 'ws://localhost:4000/graphql'
});

const subscription = client.subscribe(
  {
    query: `
      subscription {
        userCreated {
          id
          name
          email
        }
      }
    `
  },
  {
    next: (data) => console.log('New user:', data),
    error: (error) => console.error('Error:', error),
    complete: () => console.log('Subscription complete')
  }
);

// Later: unsubscribe
subscription();
```

## Filtered Subscriptions

### With Arguments

```typescript
Subscription: {
  postUpdated: {
    subscribe: () => pubsub.asyncIterator(['POST_UPDATED']),
    resolve: (payload, args) => {
      // Filter on server-side
      if (args.userId && payload.postUpdated.authorId !== args.userId) {
        return null; // Skip this event
      }
      return payload.postUpdated;
    }
  }
}
```

### Dynamic Channels

```typescript
Subscription: {
  messageAdded: {
    subscribe: (parent, { roomId }) => {
      // Subscribe to room-specific channel
      return pubsub.asyncIterator([`MESSAGE_ADDED_${roomId}`]);
    }
  }
}

// Publish to specific room
pubsub.publish(`MESSAGE_ADDED_${roomId}`, { messageAdded: message });
```

## Rust Implementation

### Using SimpleBroker

```rust
use async_graphql::*;
use futures_util::stream::Stream;

#[derive(Clone)]
pub enum UserEvent {
    Created(User),
    Updated(User),
    Deleted(ID),
}

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

    async fn user_events(&self) -> impl Stream<Item = UserEvent> {
        SimpleBroker::<UserEvent>::subscribe()
    }
}

// Publish events
SimpleBroker::publish(UserEvent::Created(user));
```

### With Authorization

```rust
#[Subscription]
impl SubscriptionRoot {
    #[graphql(guard = "RoleGuard::new(Role::User)")]
    async fn sensitive_updates(&self) -> impl Stream<Item = Update> {
        SimpleBroker::<Update>::subscribe()
    }
}
```

## Python Implementation

### Using AsyncIterator

```python
import asyncio
from typing import AsyncIterator
from collections import defaultdict

class PubSub:
    def __init__(self):
        self.subscribers = defaultdict(list)

    async def publish(self, channel: str, message):
        if channel in self.subscribers:
            for queue in self.subscribers[channel]:
                await queue.put(message)

    async def subscribe(self, channel: str) -> AsyncIterator:
        queue = asyncio.Queue()
        self.subscribers[channel].append(queue)
        try:
            while True:
                yield await queue.get()
        finally:
            self.subscribers[channel].remove(queue)

@strawberry.type
class Subscription:
    @strawberry.subscription
    async def user_created(self, info: Info) -> AsyncIterator[User]:
        pubsub = info.context["pubsub"]
        async for user in pubsub.subscribe("USER_CREATED"):
            yield user

    @strawberry.subscription
    async def post_updated(
        self,
        info: Info,
        user_id: Optional[strawberry.ID] = None
    ) -> AsyncIterator[Post]:
        pubsub = info.context["pubsub"]
        async for post in pubsub.subscribe("POST_UPDATED"):
            if user_id is None or post.author_id == str(user_id):
                yield post
```

## Advanced Patterns

### Subscription with Context

```typescript
Subscription: {
  privateMessages: {
    subscribe: (parent, args, context) => {
      if (!context.user) {
        throw new GraphQLError('Not authenticated');
      }

      // Only subscribe to messages for this user
      return pubsub.asyncIterator([`MESSAGES_${context.user.id}`]);
    }
  }
}
```

### Batched Events

```typescript
import { withFilter } from 'graphql-subscriptions';

Subscription: {
  postsUpdated: {
    subscribe: withFilter(
      () => pubsub.asyncIterator(['POSTS_UPDATED']),
      (payload, variables, context) => {
        // Filter based on user preferences
        return context.user?.interests.includes(payload.category);
      }
    )
  }
}
```

### Throttling

```typescript
import { RateLimiterMemory } from 'rate-limiter-flexible';

const rateLimiter = new RateLimiterMemory({
  points: 10, // 10 events
  duration: 1, // per second
});

Subscription: {
  highFrequencyEvents: {
    subscribe: async (parent, args, context) => {
      await rateLimiter.consume(context.user.id);
      return pubsub.asyncIterator(['HIGH_FREQUENCY']);
    },
    resolve: async function* (payload) {
      // Throttle on server side
      await new Promise(resolve => setTimeout(resolve, 100));
      yield payload;
    }
  }
}
```

## Production Considerations

### Redis PubSub

For multi-server deployments:

```typescript
import { RedisPubSub } from 'graphql-redis-subscriptions';
import Redis from 'ioredis';

const pubsub = new RedisPubSub({
  publisher: new Redis(),
  subscriber: new Redis()
});

// Now all servers share the same event bus
pubsub.publish('USER_CREATED', { userCreated: user });
```

### Connection Limits

```typescript
const server = new ApolloServer({
  typeDefs,
  resolvers,
  plugins: [
    {
      async requestDidStart(requestContext) {
        if (requestContext.operation?.operation === 'subscription') {
          const activeConnections = await getActiveConnections();

          if (activeConnections > 10000) {
            throw new GraphQLError('Too many active subscriptions');
          }
        }
      }
    }
  ]
});
```

### Heartbeat/Ping

```typescript
import { useServer } from 'graphql-ws/lib/use/ws';
import { WebSocketServer } from 'ws';

const wsServer = new WebSocketServer({ server: httpServer });

useServer(
  {
    schema,
    context: createContext,
    connectionParams: {
      keepAlive: 10000, // Send ping every 10s
    }
  },
  wsServer
);
```

### Memory Management

```typescript
// Limit subscription lifetime
const MAX_SUBSCRIPTION_DURATION = 3600000; // 1 hour

Subscription: {
  longRunning: {
    subscribe: async function* () {
      const startTime = Date.now();
      const iterator = pubsub.asyncIterator(['EVENTS']);

      for await (const event of iterator) {
        if (Date.now() - startTime > MAX_SUBSCRIPTION_DURATION) {
          break; // Force unsubscribe
        }
        yield event;
      }
    }
  }
}
```

## Testing Subscriptions

### Integration Test

```typescript
import { createServer } from 'http';
import { WebSocketServer } from 'ws';
import { useServer } from 'graphql-ws/lib/use/ws';
import { createClient } from 'graphql-ws';

describe('Subscriptions', () => {
  let server, wsServer, client;

  beforeAll(async () => {
    server = createServer();
    wsServer = new WebSocketServer({ server });
    useServer({ schema }, wsServer);

    await new Promise(resolve => server.listen(4001, resolve));

    client = createClient({ url: 'ws://localhost:4001/graphql' });
  });

  afterAll(() => {
    client.dispose();
    server.close();
  });

  it('receives user creation events', async () => {
    const events = [];

    // Subscribe
    const unsubscribe = client.subscribe(
      { query: 'subscription { userCreated { id name } }' },
      { next: (data) => events.push(data) }
    );

    // Trigger event
    await createUser({ name: 'Test' });

    // Wait for event
    await new Promise(resolve => setTimeout(resolve, 100));

    expect(events).toHaveLength(1);
    expect(events[0].data.userCreated.name).toBe('Test');

    unsubscribe();
  });
});
```

### Unit Test

```typescript
describe('Subscription resolvers', () => {
  it('filters events by user ID', async () => {
    const resolver = resolvers.Subscription.postUpdated.resolve;

    const payload1 = { postUpdated: { id: '1', authorId: 'user1' } };
    const payload2 = { postUpdated: { id: '2', authorId: 'user2' } };

    expect(resolver(payload1, { userId: 'user1' })).toBeTruthy();
    expect(resolver(payload2, { userId: 'user1' })).toBeNull();
  });
});
```

## Performance Tips

1. **Use Redis PubSub** for multi-server deployments
2. **Filter early** on server-side, not client-side
3. **Limit active subscriptions** per user/connection
4. **Implement heartbeats** to detect dead connections
5. **Throttle high-frequency events** to prevent overwhelming clients
6. **Use withFilter** sparingly (it's expensive)
7. **Close stale connections** after timeout
8. **Monitor subscription count** and resource usage

## Security Checklist

- [ ] Authenticate users before subscribing
- [ ] Authorize access to subscription events
- [ ] Rate limit subscription creation
- [ ] Validate subscription arguments
- [ ] Prevent subscription to other users' private data
- [ ] Log subscription activity
- [ ] Implement connection timeouts
- [ ] Sanitize event payloads

## Common Pitfalls

### Memory Leaks

```typescript
// BAD: Stores all events in memory
const events = [];
pubsub.subscribe('EVENTS', event => events.push(event));

// GOOD: Process and discard
pubsub.subscribe('EVENTS', event => {
  processEvent(event);
  // Event is garbage collected
});
```

### Infinite Loops

```typescript
// BAD: Can cause infinite loop
pubsub.subscribe('USER_UPDATED', async user => {
  await updateUser(user); // This publishes USER_UPDATED again!
});

// GOOD: Guard against recursion
pubsub.subscribe('USER_UPDATED', async user => {
  if (!user._isProcessing) {
    user._isProcessing = true;
    await updateUser(user);
  }
});
```

### Over-Subscribing

```typescript
// BAD: New subscription on every render
useEffect(() => {
  subscribe({ query: '...' });
  // Missing cleanup!
}, []);

// GOOD: Clean up subscription
useEffect(() => {
  const unsubscribe = subscribe({ query: '...' });
  return () => unsubscribe();
}, []);
```

## Monitoring

Track key metrics:

- Active subscription count
- Events published per second
- Event delivery latency
- Failed deliveries
- Connection churn rate
- Memory usage per subscription

```typescript
const metrics = {
  activeSubscriptions: 0,
  eventsPublished: 0,
  eventsFailed: 0
};

pubsub.publish = ((original) => {
  return function(channel, payload) {
    metrics.eventsPublished++;
    return original.call(this, channel, payload);
  };
})(pubsub.publish);
```
