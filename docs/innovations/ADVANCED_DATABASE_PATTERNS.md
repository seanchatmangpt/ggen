# Advanced Database Code Generation Patterns

> Hyper-advanced database patterns: query builders, multi-tenancy, sharding, caching strategies, and optimization

## Overview

This guide covers advanced database patterns generated from RDF ontologies, including query builders, multi-tenancy support, distributed caching, and sophisticated indexing strategies.

## Pattern 1: Type-Safe Query Builders

### Description

Generate type-safe, fluent query builders (similar to Knex.js, QueryBuilder) that prevent SQL injection and provide compile-time safety.

### External Packages

```json
{
  "knex": "^3.0.0",
  "pg": "^8.11.0",
  "@mikro-orm/core": "^5.9.0",
  "drizzle-orm": "^0.29.0"
}
```

### RDF Ontology for Queries

```turtle
@prefix db: <http://example.org/db/>

db:UserQuery a db:Query ;
  db:from db:UsersTable ;
  db:where [
    db:condition "email = ?" ;
    db:params ("user@example.com")
  ] ;
  db:select ("id" "email" "username") ;
  db:orderBy ("created_at DESC") ;
  db:limit 10 .

db:UserWithPostsQuery a db:Query ;
  db:from db:UsersTable ;
  db:join [
    db:type "left" ;
    db:table db:PostsTable ;
    db:on "users.id = posts.user_id"
  ] ;
  db:groupBy ("users.id") ;
  db:having "COUNT(posts.id) > ?" ;
  db:havingParams (5) .
```

### Generated TypeScript Query Builder

```typescript
import { Knex, knex } from 'knex';

export class UserQueryBuilder {
  private query: Knex.QueryBuilder;

  constructor(knex: Knex) {
    this.query = knex('users');
  }

  // Generated from db:UserQuery
  static async findByEmail(knex: Knex, email: string) {
    return knex('users')
      .where('email', email)
      .select('id', 'email', 'username')
      .orderBy('created_at', 'desc')
      .limit(10)
      .first();
  }

  // Generated from db:UserWithPostsQuery
  static async findWithPostsCount(knex: Knex, minPostCount: number) {
    return knex('users')
      .leftJoin('posts', 'users.id', 'posts.user_id')
      .groupBy('users.id')
      .having(knex.raw('COUNT(posts.id) > ?', [minPostCount]))
      .select('users.id', 'users.email', knex.raw('COUNT(posts.id) as post_count'));
  }

  // Fluent API generated from ontology
  whereEmail(email: string): this {
    this.query = this.query.where('email', email);
    return this;
  }

  whereActive(active: boolean = true): this {
    this.query = this.query.where('is_active', active);
    return this;
  }

  withPosts(): this {
    this.query = this.query
      .leftJoin('posts', 'users.id', 'posts.user_id')
      .groupBy('users.id');
    return this;
  }

  async execute(): Promise<any[]> {
    return this.query;
  }
}

// Usage
const users = await UserQueryBuilder.findByEmail(knex, 'user@example.com');
const activeUsersWithPosts = await new UserQueryBuilder(knex)
  .whereActive(true)
  .withPosts()
  .execute();
```

### Generated Drizzle ORM

```typescript
import { drizzle } from 'drizzle-orm/node-postgres';
import { Pool } from 'pg';
import {
  pgTable,
  serial,
  text,
  timestamp,
  boolean,
  foreignKey,
} from 'drizzle-orm/pg-core';
import { eq, sql, count } from 'drizzle-orm';

// Generated schema from RDF ontology
export const users = pgTable('users', {
  id: serial('id').primaryKey(),
  email: text('email').notNull().unique(),
  username: text('username').notNull(),
  isActive: boolean('is_active').default(true),
  createdAt: timestamp('created_at').defaultNow(),
});

export const posts = pgTable(
  'posts',
  {
    id: serial('id').primaryKey(),
    userId: serial('user_id').notNull(),
    title: text('title').notNull(),
    content: text('content'),
    createdAt: timestamp('created_at').defaultNow(),
  },
  (table) => ({
    userIdFk: foreignKey({
      columns: [table.userId],
      foreignColumns: [users.id],
    }),
  })
);

// Generated queries
const pool = new Pool();
const db = drizzle(pool);

// Type-safe query
const userByEmail = await db
  .select()
  .from(users)
  .where(eq(users.email, 'user@example.com'))
  .limit(1);

// Type-safe join with aggregation
const usersWithPostCount = await db
  .select({
    id: users.id,
    email: users.email,
    postCount: count(posts.id),
  })
  .from(users)
  .leftJoin(posts, eq(users.id, posts.userId))
  .groupBy(users.id)
  .having(sql`COUNT(${posts.id}) > 5`);
```

---

## Pattern 2: Multi-Tenancy & Row-Level Security

### Description

Generate multi-tenant database schemas with automatic row-level security (RLS) policies and tenant isolation.

### RDF Ontology for Multi-Tenancy

```turtle
@prefix db: <http://example.org/db/>
@prefix mt: <http://example.org/multi-tenancy/>

mt:TenantAware a mt:Pattern ;
  mt:isolationLevel "row-level" ;
  mt:storageStrategy "shared-database" .

db:OrganizationsTable a db:Table ;
  db:multiTenant true ;
  db:tenantIdColumn "organization_id" ;
  db:rls true ;
  db:columns (
    [ db:name "organization_id" ; db:type "uuid" ; db:partOf mt:TenantKey ]
    [ db:name "id" ; db:type "uuid" ]
  ) .

db:UsersInOrgTable a db:Table ;
  db:multiTenant true ;
  db:tenantIdColumn "organization_id" ;
  db:rlsPolicies (
    [
      db:policyName "users_see_own_org" ;
      db:command "SELECT" ;
      db:using "(organization_id = current_user_org_id())"
    ]
  ) .
```

### Generated PostgreSQL RLS Policies

```sql
-- Generated multi-tenancy setup from RDF ontology

-- Create organizations table
CREATE TABLE organizations (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name TEXT NOT NULL,
  created_at TIMESTAMP DEFAULT NOW()
);

-- Create users table with tenant isolation
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
  email TEXT NOT NULL,
  username TEXT NOT NULL,
  created_at TIMESTAMP DEFAULT NOW(),
  UNIQUE(organization_id, email)
);

-- Enable RLS
ALTER TABLE users ENABLE ROW LEVEL SECURITY;

-- Create function to get current organization
CREATE OR REPLACE FUNCTION current_org_id() RETURNS UUID AS $$
  SELECT current_setting('app.current_org_id')::UUID
$$ LANGUAGE SQL STABLE;

-- Policy: users can only see users in their organization
CREATE POLICY users_see_own_org ON users
  USING (organization_id = current_org_id())
  WITH CHECK (organization_id = current_org_id());

-- Posts table with multi-tenancy
CREATE TABLE posts (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  organization_id UUID NOT NULL REFERENCES organizations(id) ON DELETE CASCADE,
  user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  title TEXT NOT NULL,
  content TEXT,
  created_at TIMESTAMP DEFAULT NOW()
);

ALTER TABLE posts ENABLE ROW LEVEL SECURITY;

CREATE POLICY posts_see_own_org ON posts
  USING (organization_id = current_org_id())
  WITH CHECK (organization_id = current_org_id());

-- Grant policies to role
GRANT SELECT, INSERT, UPDATE, DELETE ON users TO app_user;
GRANT SELECT, INSERT, UPDATE, DELETE ON posts TO app_user;
```

### Generated TypeScript Multi-Tenant Client

```typescript
import { Pool } from 'pg';

export class MultiTenantClient {
  constructor(private pool: Pool) {}

  /**
   * Execute query with tenant isolation
   * Automatically sets the tenant context
   */
  async withTenant<T>(
    tenantId: string,
    callback: (query: TenantQuery) => Promise<T>
  ): Promise<T> {
    const client = await this.pool.connect();
    try {
      // Set tenant context (RLS will use this)
      await client.query(
        "SET app.current_org_id = $1",
        [tenantId]
      );

      const query = new TenantQuery(client);
      return await callback(query);
    } finally {
      client.release();
    }
  }

  /**
   * Get all users in tenant's organization
   * Automatically filtered by RLS policy
   */
  async getTenantUsers(tenantId: string) {
    return this.withTenant(tenantId, async (query) => {
      return query.selectUsers();
    });
  }
}

class TenantQuery {
  constructor(private client: any) {}

  async selectUsers() {
    // RLS will filter by organization_id automatically
    const result = await this.client.query(
      'SELECT id, email, username FROM users'
    );
    return result.rows;
  }

  async createUser(email: string, username: string) {
    const result = await this.client.query(
      'INSERT INTO users (email, username) VALUES ($1, $2) RETURNING *',
      [email, username]
    );
    return result.rows[0];
  }
}
```

---

## Pattern 3: Distributed Caching & Query Result Caching

### Description

Generate automatic query result caching with Redis, with invalidation patterns and cache-warming strategies.

### RDF Ontology for Caching

```turtle
@prefix db: <http://example.org/db/>
@prefix cache: <http://example.org/cache/>

cache:QueryCache a cache:CacheStrategy ;
  cache:backend "redis" ;
  cache:ttl 3600 ;
  cache:invalidationStrategy "event-based" .

db:CachedUserQuery a db:Query ;
  db:cacheable true ;
  db:cacheKeyTemplate "user:{{email}}" ;
  db:cacheTTL 1800 ;
  cache:invalidateOn (
    "user:updated"
    "user:deleted"
  ) .

cache:WarmingStrategy a cache:Pattern ;
  cache:triggerEvent "app:startup" ;
  cache:queries (
    db:PopularPostsQuery
    db:ActiveUsersQuery
  ) .
```

### Generated Caching Layer

```typescript
import Redis from 'ioredis';
import { createHash } from 'crypto';

export class CachedDatabase {
  constructor(
    private pool: Pool,
    private redis: Redis
  ) {}

  /**
   * Execute query with automatic caching
   * Cache key generated from query and parameters
   */
  async query<T>(
    name: string,
    sql: string,
    params: any[],
    ttl: number = 3600
  ): Promise<T[]> {
    // Generate cache key from query name and params
    const cacheKey = this.generateCacheKey(name, params);

    // Try to get from cache first
    const cached = await this.redis.get(cacheKey);
    if (cached) {
      return JSON.parse(cached);
    }

    // Execute query
    const result = await this.pool.query(sql, params);

    // Cache result
    await this.redis.setex(
      cacheKey,
      ttl,
      JSON.stringify(result.rows)
    );

    return result.rows as T[];
  }

  /**
   * Invalidate cache by pattern
   */
  async invalidateCache(pattern: string): Promise<void> {
    const keys = await this.redis.keys(`${pattern}:*`);
    if (keys.length > 0) {
      await this.redis.del(...keys);
    }
  }

  /**
   * Cache warming strategy
   * Pre-populate high-traffic queries
   */
  async warmCache(): Promise<void> {
    // Cache popular posts (generated from RDF)
    await this.query(
      'popular_posts',
      'SELECT * FROM posts ORDER BY views DESC LIMIT 100',
      [],
      7200
    );

    // Cache active users (generated from RDF)
    await this.query(
      'active_users',
      'SELECT * FROM users WHERE is_active = true',
      [],
      3600
    );
  }

  /**
   * Subscribe to invalidation events
   */
  onInvalidate(event: string, callback: () => void): void {
    this.redis.subscribe(`cache:invalidate:${event}`, callback);
  }

  private generateCacheKey(name: string, params: any[]): string {
    const hash = createHash('sha256')
      .update(JSON.stringify(params))
      .digest('hex');
    return `query:${name}:${hash}`;
  }
}

// Usage
const db = new CachedDatabase(pool, redis);

// Automatically cached for 30 minutes
const user = await db.query<User>(
  'user_by_email',
  'SELECT * FROM users WHERE email = $1',
  ['user@example.com'],
  1800
);

// When user is updated, invalidate cache
await db.invalidateCache('query:user_by_email');

// On startup, warm the cache
await db.warmCache();
```

---

## Pattern 4: Sharding & Partitioning Strategies

### Description

Generate database sharding logic and partition strategies for horizontal scaling.

### RDF Ontology for Sharding

```turtle
@prefix db: <http://example.org/db/>
@prefix shard: <http://example.org/sharding/>

shard:UserSharding a shard:Strategy ;
  shard:algorithm "consistent-hash" ;
  shard:key "user_id" ;
  shard:shardCount 16 ;
  shard:replicationFactor 3 .

shard:PostSharding a shard:Strategy ;
  shard:algorithm "range-based" ;
  shard:key "created_at" ;
  shard:rangeSize "monthly" ;
  shard:hotDataWindow 90 .
```

### Generated Sharding Layer

```typescript
import * as crypto from 'crypto';

export class ShardingStrategy {
  /**
   * Consistent hash sharding for user data
   * Generated from shard:UserSharding
   */
  static getShardForUser(userId: string, shardCount: number = 16): number {
    const hash = crypto
      .createHash('md5')
      .update(userId)
      .digest('hex');

    const hashValue = parseInt(hash.substring(0, 8), 16);
    return hashValue % shardCount;
  }

  /**
   * Range-based sharding for time-series data
   * Partitions posts by month
   */
  static getShardForPost(createdAt: Date): string {
    const year = createdAt.getFullYear();
    const month = String(createdAt.getMonth() + 1).padStart(2, '0');
    return `posts_${year}_${month}`;
  }
}

export class ShardedDatabase {
  private shards: Map<number, Pool>;

  constructor(shardConfigs: ShardConfig[]) {
    this.shards = new Map(
      shardConfigs.map((config, index) => [
        index,
        new Pool(config),
      ])
    );
  }

  /**
   * Get connection for user
   * Automatically routes to correct shard
   */
  async getShardConnection(userId: string): Promise<Pool> {
    const shardId = ShardingStrategy.getShardForUser(userId);
    return this.shards.get(shardId)!;
  }

  /**
   * Insert user into correct shard
   */
  async insertUser(user: User): Promise<void> {
    const shard = await this.getShardConnection(user.id);
    await shard.query(
      'INSERT INTO users (id, email, username) VALUES ($1, $2, $3)',
      [user.id, user.email, user.username]
    );
  }

  /**
   * Query across all shards (map-reduce pattern)
   */
  async queryAllShards<T>(sql: string, params: any[]): Promise<T[]> {
    const results = await Promise.all(
      Array.from(this.shards.values()).map((shard) =>
        shard.query(sql, params).then((result) => result.rows)
      )
    );

    return results.flat();
  }
}
```

---

## Pattern 5: Advanced Indexing & Query Optimization

### Description

Automatically generate optimal indexes and query execution plans based on RDF ontology patterns.

### RDF Ontology for Indexes

```turtle
@prefix db: <http://example.org/db/>
@prefix idx: <http://example.org/indexing/>

idx:EmailIndex a idx:Index ;
  idx:table db:UsersTable ;
  idx:columns ("email") ;
  idx:unique true ;
  idx:type "btree" .

idx:UserPostsIndex a idx:Index ;
  idx:table db:PostsTable ;
  idx:columns ("user_id" "created_at") ;
  idx:type "btree" ;
  idx:partialPredicate "is_published = true" .

idx:FullTextIndex a idx:Index ;
  idx:table db:PostsTable ;
  idx:columns ("title", "content") ;
  idx:type "gin" ;
  idx:language "english" .
```

### Generated Index Migrations

```sql
-- Generated from idx:EmailIndex
CREATE UNIQUE INDEX CONCURRENTLY idx_users_email
ON users(email);

-- Generated from idx:UserPostsIndex
CREATE INDEX CONCURRENTLY idx_posts_user_id_created_at
ON posts(user_id, created_at)
WHERE is_published = true;

-- Generated from idx:FullTextIndex (PostgreSQL FTS)
CREATE INDEX CONCURRENTLY idx_posts_fulltext
ON posts USING gin(to_tsvector('english', title || ' ' || content));

-- Query using FTS index
SELECT * FROM posts
WHERE to_tsvector('english', title || ' ' || content) @@
      plainto_tsquery('english', 'search query')
ORDER BY ts_rank(to_tsvector('english', title || ' ' || content),
                  plainto_tsquery('english', 'search query')) DESC;
```

---

## Best Practices

1. **Type Safety First**: Use generated TypeScript types for all queries
2. **Parameterized Queries**: Always use prepared statements to prevent SQL injection
3. **Index Strategy**: Generate indexes based on query patterns, not guesses
4. **Caching Layers**: Implement multi-level caching (Redis → in-memory)
5. **RLS for Multi-Tenancy**: Use PostgreSQL RLS for automatic tenant isolation
6. **Query Planning**: Analyze execution plans and optimize accordingly
7. **Connection Pooling**: Configure pools for peak load with backpressure handling

---

## References

- [Drizzle ORM Documentation](https://orm.drizzle.team/)
- [Knex.js Documentation](https://knexjs.org/)
- [PostgreSQL RLS Documentation](https://www.postgresql.org/docs/current/ddl-rowsecurity.html)
- [Query Optimization Guide](https://www.postgresql.org/docs/current/sql-explain.html)

## See Also

- [POSTGRESQL_INTEGRATION_EXAMPLE.md](../how-to-guides/POSTGRESQL_INTEGRATION_EXAMPLE.md) - Base PostgreSQL guide
- [ADVANCED_PYTHON_PATTERNS.md](./ADVANCED_PYTHON_PATTERNS.md) - Python ORM patterns
- [ADVANCED_DEPLOYMENT_PATTERNS.md](./ADVANCED_DEPLOYMENT_PATTERNS.md) - Deployment considerations
