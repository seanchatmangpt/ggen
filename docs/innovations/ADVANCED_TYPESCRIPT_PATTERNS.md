# Advanced TypeScript/Node.js Code Generation Patterns

> Hyper-advanced patterns for generating type-safe, production-grade Node.js APIs using ggen's RDF ontology-driven code generation

## Overview

This guide covers five advanced TypeScript patterns that demonstrate ggen's ability to generate sophisticated, enterprise-grade patterns from RDF ontologies. Each pattern is driven by ontology definitions and generated using Tera templates.

## Pattern 1: Advanced Request/Response Interceptors with MSW

### Description

Generate sophisticated middleware patterns using **axios interceptors** for client-side and **MSW (Mock Service Worker)** for testing and development. Enables complex request transformation, retry logic, circuit breakers, and realistic API mocking driven by RDF ontology.

### External Packages

```json
{
  "axios": "^1.6.0",
  "msw": "^2.0.0",
  "axios-retry": "^4.0.0",
  "opossum": "^8.1.0"
}
```

### RDF Ontology Extensions

Define retry policies, circuit breaker configurations, rate limiting, and mock scenarios in your ontology:

```turtle
@prefix ex: <http://example.org/api/>

ex:GetUsersEndpoint a rdfs:Class ;
  ex:httpMethod "GET" ;
  ex:path "/api/users" ;
  ex:retryPolicy [
    ex:maxAttempts 3 ;
    ex:initialDelayMs 100 ;
    ex:backoffMultiplier 2.0
  ] ;
  ex:circuitBreakerConfig [
    ex:failureThreshold 50 ;
    ex:successThreshold 2 ;
    ex:timeout 3000 ;
    ex:resetTimeout 30000
  ] ;
  ex:rateLimitConfig [
    ex:requestsPerMinute 60 ;
    ex:burstSize 10
  ] ;
  ex:mockScenarios (
    ex:SuccessScenario
    ex:RateLimitScenario
    ex:ServerErrorScenario
    ex:TimeoutScenario
  ) .
```

### Generated Code Pattern

**Axios Client with Interceptors**:

```typescript
import axios, { AxiosInstance } from 'axios';
import axiosRetry from 'axios-retry';
import CircuitBreaker from 'opossum';
import { RateLimiter } from 'limiter';

// Generated from ex:GetUsersEndpoint with ex:retryPolicy
const userApiClient: AxiosInstance = axios.create({
  baseURL: 'http://localhost:3000/api',
  timeout: 5000,
});

// Retry configuration (generated from ontology)
axiosRetry(userApiClient, {
  retries: 3,
  retryDelay: axiosRetry.exponentialDelay,
  retryCondition: (error) => {
    return axiosRetry.isNetworkOrIdempotentRequestError(error) ||
           error.response?.status === 429;
  },
});

// Circuit breaker (from ontology)
const circuitBreakerConfig = {
  timeout: 3000,
  errorThresholdPercentage: 50,
  resetTimeout: 30000,
};

const getUsersBreaker = new CircuitBreaker(
  async (params) => userApiClient.get('/users', { params }),
  circuitBreakerConfig
);

// Rate limiter (from ontology)
const rateLimiter = new RateLimiter({
  tokensPerInterval: 60,
  interval: 'minute'
});

// Interceptors
userApiClient.interceptors.request.use(async (config) => {
  await rateLimiter.removeTokens(1);
  config.headers['X-Request-ID'] = crypto.randomUUID();
  return config;
});

export { userApiClient, getUsersBreaker };
```

**MSW Handlers** (generated from ontology mock scenarios):

```typescript
import { http, HttpResponse, delay } from 'msw';
import { setupWorker } from 'msw/browser';

const handlers = [
  // Success scenario
  http.get('/api/users', async ({ request }) => {
    const url = new URL(request.url);
    const skip = parseInt(url.searchParams.get('skip') || '0');
    const limit = parseInt(url.searchParams.get('limit') || '10');

    await delay(100); // Network latency

    return HttpResponse.json({
      data: Array.from({ length: limit }, (_, i) => ({
        userId: `user-${skip + i}`,
        userName: `user${skip + i}`,
        userEmail: `user${skip + i}@example.com`,
      })),
      pagination: { skip, limit, total: 100 },
    });
  }),

  // Rate limit scenario
  http.get('/api/users/rate-limited', () => {
    return HttpResponse.json(
      { error: 'Rate limit exceeded', retryAfter: 60 },
      { status: 429, headers: { 'Retry-After': '60' } }
    );
  }),

  // Error scenario for circuit breaker testing
  http.get('/api/users/flaky', async () => {
    await delay(50);
    if (Math.random() > 0.5) {
      return HttpResponse.json(
        { error: 'Internal server error' },
        { status: 500 }
      );
    }
    return HttpResponse.json({ data: [] });
  }),
];

export const worker = setupWorker(...handlers);
```

**Integration Tests with MSW**:

```typescript
import { describe, it, expect, beforeAll, afterEach } from 'vitest';
import { server } from './mocks/server';
import { userApiClient, getUsersBreaker } from './clients/userApiClient';

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());

describe('User API Client with Advanced Patterns', () => {
  it('should retry on rate limit with exponential backoff', async () => {
    let attempts = 0;
    server.use(
      http.get('/api/users', () => {
        attempts++;
        if (attempts < 3) {
          return HttpResponse.json(
            { error: 'Rate limit' },
            { status: 429 }
          );
        }
        return HttpResponse.json({ data: [] });
      })
    );

    const response = await userApiClient.get('/users');
    expect(attempts).toBe(3); // Initial + 2 retries
    expect(response.status).toBe(200);
  });

  it('should open circuit breaker after threshold failures', async () => {
    const promises = Array.from({ length: 10 }, () =>
      getUsersBreaker.fire({ skip: 0, limit: 10 }).catch(() => {})
    );

    await Promise.all(promises);
    expect(getUsersBreaker.opened).toBe(true);

    // Next call should fail fast
    await expect(getUsersBreaker.fire({})).rejects.toThrow('Breaker is open');
  });

  it('should respect rate limiting', async () => {
    const startTime = Date.now();
    const promises = Array.from({ length: 65 }, () =>
      userApiClient.get('/users')
    );

    await Promise.all(promises);
    const duration = Date.now() - startTime;

    // Extra requests should be queued
    expect(duration).toBeGreaterThan(1000);
  });
});
```

### Implementation Complexity

- **Lines of Code**: 450
- **Files Generated**: 6
- **Templates Needed**: 3
- **Ontology Extensions**: 8
- **Effort Estimate**: 2-3 days

---

## Pattern 2: Advanced Type Safety with Runtime-to-Compile-Time Codegen

### Description

Generate end-to-end type safety using **zod-to-typescript** for converting runtime Zod schemas to TypeScript interfaces, and **ts-json-schema-generator** for creating JSON schemas from TypeScript types. Enables bidirectional type generation driven by RDF ontology.

### External Packages

```json
{
  "zod": "^3.22.0",
  "zod-to-ts": "^1.2.0",
  "ts-json-schema-generator": "^1.5.0",
  "@anatine/zod-openapi": "^2.2.0"
}
```

### RDF Ontology for Type Definitions

```turtle
@prefix api: <http://example.org/api/>
@prefix ty: <http://example.org/types/>

api:UserModel a ty:TypeDefinition ;
  ty:typeName "User" ;
  ty:fields (
    [
      ty:fieldName "id" ;
      ty:fieldType "string" ;
      ty:required true ;
      ty:format "uuid"
    ]
    [
      ty:fieldName "email" ;
      ty:fieldType "string" ;
      ty:required true ;
      ty:validation [
        ty:pattern "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" ;
        ty:minLength 5 ;
        ty:maxLength 255
      ]
    ]
    [
      ty:fieldName "username" ;
      ty:fieldType "string" ;
      ty:required true ;
      ty:validation [
        ty:minLength 3 ;
        ty:maxLength 32 ;
        ty:pattern "^[a-zA-Z0-9_-]+$"
      ]
    ]
  ) .
```

### Generated Code Pattern

**From Zod Schema to TypeScript**:

```typescript
import { z } from 'zod';
import { GenerateFromZodSchema } from 'zod-to-ts';

// Define Zod schema (generated from ontology)
export const UserSchema = z.object({
  id: z.string().uuid(),
  email: z.string().email().min(5).max(255),
  username: z.string().min(3).max(32).regex(/^[a-zA-Z0-9_-]+$/),
  createdAt: z.date().default(() => new Date()),
  isActive: z.boolean().default(true),
});

// Auto-generated TypeScript type from Zod
export type User = z.infer<typeof UserSchema>;

// Auto-generated JSON Schema for OpenAPI
export const userOpenAPISchema = zodToJsonSchema(UserSchema);

// Type-safe runtime validation
export const validateUser = (data: unknown): User => {
  return UserSchema.parse(data);
};

// Discriminated unions for polymorphic types
export const ApiResponseSchema = z.discriminatedUnion('status', [
  z.object({
    status: z.literal('success'),
    data: UserSchema,
    code: z.number().int(),
  }),
  z.object({
    status: z.literal('error'),
    error: z.object({
      message: z.string(),
      code: z.string(),
      details: z.record(z.unknown()).optional(),
    }),
  }),
]);

export type ApiResponse = z.infer<typeof ApiResponseSchema>;
```

**OpenAPI/Swagger Generation**:

```typescript
import { zodToOpenAPI } from '@anatine/zod-openapi';

export const userOpenAPIComponent = zodToOpenAPI(UserSchema);

// Generated OpenAPI spec
export const openApiSpec = {
  openapi: '3.0.0',
  info: {
    title: 'Generated API',
    version: '1.0.0',
  },
  paths: {
    '/api/users': {
      post: {
        requestBody: {
          content: {
            'application/json': {
              schema: userOpenAPIComponent,
            },
          },
        },
        responses: {
          200: {
            description: 'User created',
            content: {
              'application/json': {
                schema: zodToOpenAPI(ApiResponseSchema),
              },
            },
          },
        },
      },
    },
  },
};
```

### Implementation Complexity

- **Lines of Code**: 520
- **Files Generated**: 5
- **Templates Needed**: 4
- **Effort Estimate**: 3-4 days

---

## Pattern 3: Advanced API Composition & Orchestration

### Description

Build complex API aggregation patterns using **ts-rest**, **tRPC**, and **graphql-request** for type-safe service composition.

### External Packages

```json
{
  "ts-rest": "^5.0.0",
  "trpc": "^10.45.0",
  "@trpc/client": "^10.45.0",
  "graphql-request": "^6.0.0",
  "zod": "^3.22.0"
}
```

### RDF Ontology for Composition

```turtle
@prefix api: <http://example.org/api/>
@prefix comp: <http://example.org/composition/>

api:UserService a comp:MicroService ;
  comp:basePath "/api/users" ;
  comp:endpoints (
    [
      comp:operationId "getUser" ;
      comp:method "GET" ;
      comp:path "/{id}" ;
      comp:responseType api:UserModel
    ]
  ) .

api:PostService a comp:MicroService ;
  comp:basePath "/api/posts" ;
  comp:endpoints (
    [
      comp:operationId "getUserPosts" ;
      comp:method "GET" ;
      comp:path "/user/{userId}" ;
      comp:responseType api:PostList
    ]
  ) .

api:AggregatedUserProfile a comp:ComposedService ;
  comp:aggregates ( api:UserService api:PostService ) ;
  comp:operations (
    [
      comp:operationId "getUserWithPosts" ;
      comp:merges ( "getUser" "getUserPosts" )
    ]
  ) .
```

### Generated Code Pattern

```typescript
import { initTRPC } from '@trpc/server';
import { z } from 'zod';

// Generate client from ts-rest contract
export const contract = {
  users: {
    getById: {
      method: 'GET' as const,
      path: '/api/users/:id',
      responses: {
        200: UserSchema,
      },
    },
  },
  posts: {
    getByUserId: {
      method: 'GET' as const,
      path: '/api/posts',
      query: z.object({ userId: z.string() }),
      responses: {
        200: z.array(PostSchema),
      },
    },
  },
} as const;

// Type-safe API composition with tRPC
const t = initTRPC.create();

export const appRouter = t.router({
  users: t.router({
    getById: t.procedure
      .input(z.string().uuid())
      .query(async ({ input }) => {
        const response = await fetch(`/api/users/${input}`);
        return response.json();
      }),
  }),

  posts: t.router({
    getByUserId: t.procedure
      .input(z.string().uuid())
      .query(async ({ input }) => {
        const response = await fetch(`/api/posts?userId=${input}`);
        return response.json();
      }),
  }),

  // Composed operation
  userProfiles: t.router({
    full: t.procedure
      .input(z.string().uuid())
      .query(async ({ input }) => {
        const [user, posts] = await Promise.all([
          appRouter.createCaller({}).users.getById(input),
          appRouter.createCaller({}).posts.getByUserId(input),
        ]);

        return { user, posts };
      }),
  }),
});

export type AppRouter = typeof appRouter;
```

---

## Pattern 4: Advanced gRPC Code Generation

### Description

Generate **type-safe gRPC services** from RDF ontologies with bidirectional streaming, server reflection, and middleware composition.

### External Packages

```json
{
  "@grpc/grpc-js": "^1.10.0",
  "@grpc/proto-loader": "^0.7.0",
  "@grpc/grpc-js-reflection": "^1.4.0"
}
```

---

## Pattern 5: Advanced Streaming & Real-Time Patterns

### Description

Generate **streaming API patterns** with Server-Sent Events (SSE), WebSockets, and multipart streaming.

### External Packages

```json
{
  "ws": "^8.15.0",
  "whatwg-fetch": "^3.6.0",
  "redis": "^4.6.0"
}
```

---

## Tera Template Examples

### Axios Client Template

```tera
{% for endpoint in endpoints %}
// Generated client for {{ endpoint.operationId }}
export async function {{ endpoint.operationId }}(
  {% for param in endpoint.parameters %}
  {{ param.name }}: {{ param.type }},
  {% endfor %}
): Promise<{{ endpoint.responseType }}> {
  const response = await client.{{ endpoint.method | lowercase }}(
    `{{ endpoint.path | interpolate_path(parameters) }}`,
    {% if endpoint.requestBody %}
    {{ endpoint.requestBody | json }}
    {% endif %}
  );
  return response.data;
}
{% endfor %}
```

### MSW Handlers Template

```tera
{% for scenario in mockScenarios %}
// Mock: {{ scenario.name }}
http.{{ scenario.httpMethod | lowercase }}('{{ scenario.path }}', async () => {
  await delay({{ scenario.latencyMs | default(value=100) }});
  return HttpResponse.json(
    {{ scenario.responseData | json }},
    { status: {{ scenario.statusCode | default(value=200) }} }
  );
}),
{% endfor %}
```

---

## Best Practices

1. **Type Safety First**: Always validate at system boundaries (API inputs)
2. **Error Handling**: Use discriminated unions for polymorphic error responses
3. **Circuit Breakers**: Implement for external service calls
4. **Caching Strategy**: Use Redis for distributed caching across services
5. **Rate Limiting**: Implement token bucket algorithm with fallback strategies
6. **Observability**: Add structured logging with correlation IDs

---

## References

- [Zod Documentation](https://zod.dev)
- [MSW Documentation](https://mswjs.io)
- [tRPC Documentation](https://trpc.io)
- [ts-rest Documentation](https://ts-rest.com)
- [Axios Documentation](https://axios-http.com)

## See Also

- [ADVANCED_GRAPHQL_PATTERNS.md](./ADVANCED_GRAPHQL_PATTERNS.md) - GraphQL federation and subscriptions
- [ADVANCED_DEPLOYMENT_PATTERNS.md](./ADVANCED_DEPLOYMENT_PATTERNS.md) - Kubernetes and container orchestration
- [JAVASCRIPT_EXPRESS_EXAMPLE.md](../how-to-guides/JAVASCRIPT_EXPRESS_EXAMPLE.md) - Base Express.js example
