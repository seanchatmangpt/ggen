# Database Performance & Indexing Strategies

Comprehensive guide to index optimization, query performance, and database tuning.

## Index Types & Use Cases

### B-tree Index (Default)

**Best for:**
- Equality comparisons (`=`)
- Range queries (`<`, `>`, `BETWEEN`)
- Sorted results (`ORDER BY`)
- Pattern matching with prefix (`LIKE 'prefix%'`)

**Example:**
```sql
-- Create B-tree index (default)
CREATE INDEX idx_users_email ON users(email);

-- Efficient queries
SELECT * FROM users WHERE email = 'user@example.com';  -- O(log n)
SELECT * FROM users WHERE created_at > '2025-01-01';   -- Range scan
SELECT * FROM users ORDER BY email;                     -- Index scan
```

**Performance:**
- Insertion: O(log n)
- Query: O(log n)
- Storage: Moderate

### Hash Index

**Best for:**
- Equality comparisons only (`=`)
- High-cardinality columns
- PostgreSQL only (not MySQL/SQLite)

**Example:**
```sql
-- Create hash index
CREATE INDEX idx_users_uuid ON users USING hash(uuid);

-- Efficient query
SELECT * FROM users WHERE uuid = '123e4567-e89b-12d3-a456-426614174000';

-- NOT efficient (hash doesn't support)
SELECT * FROM users WHERE uuid > '123e4567...';  -- Full scan
```

**Performance:**
- Insertion: O(1) average
- Query: O(1) average
- Storage: Less than B-tree

### GIN Index (Generalized Inverted Index)

**Best for:**
- JSONB containment queries
- Array element search
- Full-text search
- Composite types

**Example:**
```sql
-- JSONB index
CREATE INDEX idx_posts_metadata ON posts USING GIN(metadata);

-- Efficient queries
SELECT * FROM posts WHERE metadata @> '{"category": "tech"}';
SELECT * FROM posts WHERE metadata ? 'tags';  -- Key exists

-- Array index
CREATE INDEX idx_posts_tags ON posts USING GIN(tags);
SELECT * FROM posts WHERE tags @> ARRAY['rust', 'database'];

-- Full-text search
CREATE INDEX idx_posts_search ON posts USING GIN(
  to_tsvector('english', title || ' ' || content)
);
SELECT * FROM posts WHERE to_tsvector('english', title || ' ' || content) @@ to_tsquery('rust & database');
```

**Performance:**
- Insertion: Slower (maintains inverted index)
- Query: Very fast for containment
- Storage: Larger than B-tree

### GiST Index (Generalized Search Tree)

**Best for:**
- Geometric types (points, polygons)
- Range types (tsrange, int4range)
- Full-text search (alternative to GIN)
- Nearest-neighbor queries

**Example:**
```sql
-- Geometric data
CREATE INDEX idx_locations_point ON locations USING GIST(geom);

-- Efficient queries
SELECT * FROM locations WHERE geom && ST_MakeEnvelope(-74, 40, -73, 41);  -- Bounding box

-- Range types
CREATE INDEX idx_bookings_period ON bookings USING GIST(period);
SELECT * FROM bookings WHERE period @> CURRENT_TIMESTAMP;  -- Overlaps now
```

**Performance:**
- Insertion: Moderate
- Query: Good for spatial/range queries
- Storage: Moderate

### Full-Text Index

**PostgreSQL:**
```sql
-- GIN for full-text (faster queries, slower updates)
CREATE INDEX idx_articles_search ON articles USING GIN(
  to_tsvector('english', title || ' ' || content)
);

-- GiST for full-text (faster updates, slower queries)
CREATE INDEX idx_articles_search ON articles USING GIST(
  to_tsvector('english', title || ' ' || content)
);

-- Query
SELECT * FROM articles
WHERE to_tsvector('english', title || ' ' || content) @@ to_tsquery('rust & performance');
```

**MySQL:**
```sql
-- FULLTEXT index
CREATE FULLTEXT INDEX idx_articles_search ON articles(title, content);

-- Query
SELECT * FROM articles
WHERE MATCH(title, content) AGAINST('rust performance' IN NATURAL LANGUAGE MODE);
```

## Composite Indexes

### Multi-Column Indexes

```sql
-- Composite index (order matters!)
CREATE INDEX idx_orders_user_status ON orders(user_id, status);

-- Efficient queries (uses index)
SELECT * FROM orders WHERE user_id = 123 AND status = 'completed';  -- ✅ Both columns
SELECT * FROM orders WHERE user_id = 123;                            -- ✅ Leading column only

-- NOT efficient (doesn't use index)
SELECT * FROM orders WHERE status = 'completed';  -- ❌ Not leading column
```

**Rule:** Composite index `(A, B, C)` supports queries on:
- `A`
- `A, B`
- `A, B, C`

But NOT:
- `B`
- `C`
- `B, C`

### Column Order Strategy

```sql
-- Wrong order (low selectivity first)
CREATE INDEX idx_users_status_email ON users(status, email);  -- ❌

-- Most queries: WHERE email = 'user@example.com' AND status = 'active'
-- Index starts with status (few unique values), not optimal

-- Correct order (high selectivity first)
CREATE INDEX idx_users_email_status ON users(email, status);  -- ✅

-- email is unique (high selectivity), status has few values (low selectivity)
-- Index efficiently narrows down to exact email, then filters by status
```

**Rule:** Order columns by:
1. Equality conditions (`=`)
2. Inequality conditions (`<`, `>`, `BETWEEN`)
3. Sort order (`ORDER BY`)

### Covering Indexes (Include Columns)

```sql
-- PostgreSQL: INCLUDE clause
CREATE INDEX idx_orders_user_id_covering ON orders(user_id)
INCLUDE (created_at, total_amount);

-- Query uses index-only scan (no table access)
SELECT user_id, created_at, total_amount
FROM orders
WHERE user_id = 123;  -- All data in index!

-- MySQL: Add columns to index
CREATE INDEX idx_orders_user_id_covering ON orders(user_id, created_at, total_amount);
```

## Partial Indexes

### Filtering Unwanted Rows

```sql
-- Index only active users
CREATE INDEX idx_active_users_email ON users(email)
WHERE status = 'active';

-- Index only recent orders
CREATE INDEX idx_recent_orders ON orders(created_at)
WHERE created_at > CURRENT_DATE - INTERVAL '30 days';

-- Index only non-null values
CREATE INDEX idx_users_phone ON users(phone)
WHERE phone IS NOT NULL;
```

**Benefits:**
- Smaller index size
- Faster queries matching condition
- Reduced maintenance overhead

## Unique Indexes

```sql
-- Enforce uniqueness
CREATE UNIQUE INDEX idx_users_email_unique ON users(email);

-- Composite unique constraint
CREATE UNIQUE INDEX idx_user_roles_unique ON user_roles(user_id, role_id);

-- Partial unique constraint
CREATE UNIQUE INDEX idx_active_users_username ON users(username)
WHERE deleted_at IS NULL;  -- Unique only among active users
```

## Index Performance Benchmarks

### Test Setup

```sql
-- Create test table
CREATE TABLE benchmark_users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) NOT NULL,
  status VARCHAR(20) NOT NULL,
  created_at TIMESTAMP NOT NULL
);

-- Insert 1M rows
INSERT INTO benchmark_users (email, status, created_at)
SELECT
  'user' || i || '@example.com',
  CASE WHEN i % 3 = 0 THEN 'active' ELSE 'inactive' END,
  CURRENT_TIMESTAMP - (i || ' seconds')::INTERVAL
FROM generate_series(1, 1000000) AS i;

ANALYZE benchmark_users;
```

### No Index (Baseline)

```sql
EXPLAIN ANALYZE
SELECT * FROM benchmark_users WHERE email = 'user500000@example.com';

-- Result:
-- Seq Scan on benchmark_users  (cost=0.00..20833.00 rows=1 width=41) (actual time=234.567..468.901 rows=1 loops=1)
-- Planning Time: 0.123 ms
-- Execution Time: 468.934 ms  ⏱️ 469ms
```

### B-tree Index

```sql
CREATE INDEX idx_users_email ON benchmark_users(email);

EXPLAIN ANALYZE
SELECT * FROM benchmark_users WHERE email = 'user500000@example.com';

-- Result:
-- Index Scan using idx_users_email on benchmark_users  (cost=0.42..8.44 rows=1 width=41) (actual time=0.045..0.047 rows=1 loops=1)
-- Planning Time: 0.234 ms
-- Execution Time: 0.078 ms  ⏱️ 0.08ms (5,862x faster)
```

### Composite Index Performance

```sql
-- Create composite index
CREATE INDEX idx_users_status_created ON benchmark_users(status, created_at DESC);

-- Query using both columns
EXPLAIN ANALYZE
SELECT * FROM benchmark_users
WHERE status = 'active'
ORDER BY created_at DESC
LIMIT 10;

-- Result:
-- Index Scan using idx_users_status_created on benchmark_users  (cost=0.42..8.55 rows=10 width=41) (actual time=0.023..0.034 rows=10 loops=1)
-- Execution Time: 0.056 ms  ⏱️ 0.06ms
```

### GIN Index for JSONB

```sql
-- Add JSONB column and index
ALTER TABLE benchmark_users ADD COLUMN metadata JSONB;

UPDATE benchmark_users
SET metadata = jsonb_build_object('premium', id % 10 = 0, 'score', id % 100);

CREATE INDEX idx_users_metadata ON benchmark_users USING GIN(metadata);

EXPLAIN ANALYZE
SELECT * FROM benchmark_users WHERE metadata @> '{"premium": true}';

-- Result:
-- Bitmap Heap Scan on benchmark_users  (cost=24.42..8234.55 rows=100000 width=45) (actual time=12.345..89.123 rows=100000 loops=1)
--   Recheck Cond: (metadata @> '{"premium": true}'::jsonb)
--   Heap Blocks: exact=5406
--   ->  Bitmap Index Scan on idx_users_metadata  (cost=0.00..24.42 rows=100000 width=0) (actual time=8.234..8.234 rows=100000 loops=1)
-- Execution Time: 95.678 ms  ⏱️ 96ms
```

## Index Maintenance

### Concurrent Index Creation

```sql
-- PostgreSQL: Non-blocking index creation
CREATE INDEX CONCURRENTLY idx_users_email ON users(email);

-- Safe for production (doesn't lock table)
-- Takes longer but allows reads/writes during creation
```

### Rebuilding Indexes

```sql
-- PostgreSQL: Rebuild bloated index
REINDEX INDEX CONCURRENTLY idx_users_email;

-- MySQL: Rebuild all indexes on table
ALTER TABLE users ENGINE=InnoDB;

-- SQLite: Vacuum to rebuild
VACUUM;
```

### Monitoring Index Usage

```sql
-- PostgreSQL: Check index usage
SELECT
  schemaname,
  tablename,
  indexname,
  idx_scan,  -- Number of index scans
  idx_tup_read,  -- Tuples read
  idx_tup_fetch  -- Tuples fetched
FROM pg_stat_user_indexes
ORDER BY idx_scan ASC;

-- Find unused indexes
SELECT
  schemaname || '.' || tablename AS table,
  indexname AS index,
  pg_size_pretty(pg_relation_size(indexrelid)) AS size
FROM pg_stat_user_indexes
WHERE idx_scan = 0
  AND indexname NOT LIKE '%_pkey'  -- Exclude primary keys
ORDER BY pg_relation_size(indexrelid) DESC;
```

### Index Size

```sql
-- PostgreSQL: Index sizes
SELECT
  tablename,
  indexname,
  pg_size_pretty(pg_relation_size(indexrelid)) AS index_size
FROM pg_stat_user_indexes
ORDER BY pg_relation_size(indexrelid) DESC;

-- MySQL: Table and index sizes
SELECT
  table_name,
  ROUND(data_length / 1024 / 1024, 2) AS data_mb,
  ROUND(index_length / 1024 / 1024, 2) AS index_mb
FROM information_schema.tables
WHERE table_schema = 'myapp_db'
ORDER BY index_length DESC;
```

## Query Optimization

### EXPLAIN ANALYZE

```sql
-- Detailed query execution plan
EXPLAIN (ANALYZE, BUFFERS, VERBOSE)
SELECT u.email, COUNT(p.id)
FROM users u
LEFT JOIN posts p ON p.user_id = u.id
WHERE u.status = 'active'
GROUP BY u.email
ORDER BY COUNT(p.id) DESC
LIMIT 10;
```

### Common Anti-Patterns

**1. Function on Indexed Column**
```sql
-- ❌ BAD: Function prevents index usage
SELECT * FROM users WHERE LOWER(email) = 'user@example.com';

-- ✅ GOOD: Use functional index
CREATE INDEX idx_users_email_lower ON users(LOWER(email));
SELECT * FROM users WHERE LOWER(email) = 'user@example.com';

-- ✅ BETTER: Case-insensitive collation
CREATE INDEX idx_users_email ON users(email COLLATE "en_US.utf8");
```

**2. OR Conditions**
```sql
-- ❌ BAD: OR prevents index usage
SELECT * FROM users WHERE email = 'user@example.com' OR username = 'user123';

-- ✅ GOOD: Use UNION
SELECT * FROM users WHERE email = 'user@example.com'
UNION
SELECT * FROM users WHERE username = 'user123';
```

**3. Leading Wildcards**
```sql
-- ❌ BAD: Leading wildcard prevents index usage
SELECT * FROM users WHERE email LIKE '%@example.com';

-- ✅ GOOD: Use full-text search or specialized index
CREATE INDEX idx_users_email_reverse ON users(REVERSE(email));
SELECT * FROM users WHERE REVERSE(email) LIKE REVERSE('%@example.com');
```

## Best Practices

1. **Index Cardinality**: Index columns with high cardinality (many unique values)
2. **Composite Order**: Put high-selectivity columns first
3. **Covering Indexes**: Include columns to avoid table access
4. **Partial Indexes**: Filter out unneeded rows
5. **Monitor Usage**: Remove unused indexes
6. **Rebuild Regularly**: Prevent index bloat
7. **Test Performance**: Use EXPLAIN ANALYZE
8. **Concurrent Creation**: Don't block production

## See Also

- [README.md](README.md) - Quick start guide
- [MIGRATIONS.md](MIGRATIONS.md) - Schema evolution
- [SCHEMA.md](SCHEMA.md) - RDF-to-SQL transformation
