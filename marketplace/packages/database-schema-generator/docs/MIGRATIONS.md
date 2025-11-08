# Database Migration Management

This guide covers migration strategies, version control, and best practices for schema evolution.

## Overview

The Database Schema Generator provides automated migration generation with up/down scripts, version tracking, and rollback support.

## Migration Structure

### Migration Files

```
migrations/
├── 0001_initial_schema_up.sql
├── 0001_initial_schema_down.sql
├── 0002_add_users_table_up.sql
├── 0002_add_users_table_down.sql
├── 0003_add_posts_indexes_up.sql
└── 0003_add_posts_indexes_down.sql
```

### Naming Convention

```
<version>_<description>_<direction>.sql

Examples:
- 0001_initial_schema_up.sql
- 0001_initial_schema_down.sql
- 0002_add_user_roles_up.sql
- 0002_add_user_roles_down.sql
```

## Migration Version Tracking

### Schema Migrations Table

```sql
CREATE TABLE schema_migrations (
  version VARCHAR(255) PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  execution_time_ms INTEGER,
  checksum VARCHAR(64),
  applied_by VARCHAR(100)
);

CREATE INDEX idx_schema_migrations_applied_at ON schema_migrations(applied_at);
```

### Recording Migrations

```sql
-- Record successful migration
INSERT INTO schema_migrations (version, name, execution_time_ms, checksum)
VALUES ('0001', 'initial_schema', 234, 'sha256:abc123...');

-- Check if migration applied
SELECT EXISTS (
  SELECT 1 FROM schema_migrations WHERE version = '0001'
);

-- Get latest migration
SELECT version, name, applied_at
FROM schema_migrations
ORDER BY version DESC
LIMIT 1;
```

## Migration Types

### 1. Schema Creation (Initial)

**Up Migration: 0001_initial_schema_up.sql**
```sql
-- Create base tables
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) NOT NULL UNIQUE,
  username VARCHAR(100) NOT NULL UNIQUE,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

CREATE TABLE posts (
  id SERIAL PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  title VARCHAR(255) NOT NULL,
  content TEXT NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Record migration
INSERT INTO schema_migrations (version, name)
VALUES ('0001', 'initial_schema');
```

**Down Migration: 0001_initial_schema_down.sql**
```sql
-- Drop tables in reverse order
DROP TABLE IF EXISTS posts CASCADE;
DROP TABLE IF EXISTS users CASCADE;

-- Remove migration record
DELETE FROM schema_migrations WHERE version = '0001';
```

### 2. Adding Columns

**Up Migration: 0002_add_user_bio_up.sql**
```sql
-- Add bio column to users
ALTER TABLE users
ADD COLUMN bio TEXT;

-- Backfill with default value (optional)
UPDATE users SET bio = '' WHERE bio IS NULL;

-- Record migration
INSERT INTO schema_migrations (version, name)
VALUES ('0002', 'add_user_bio');
```

**Down Migration: 0002_add_user_bio_down.sql**
```sql
-- Remove bio column
ALTER TABLE users
DROP COLUMN bio;

-- Remove migration record
DELETE FROM schema_migrations WHERE version = '0002';
```

### 3. Adding Indexes

**Up Migration: 0003_add_posts_indexes_up.sql**
```sql
-- Add performance indexes
CREATE INDEX idx_posts_user_id ON posts(user_id);
CREATE INDEX idx_posts_created_at ON posts(created_at DESC);

-- Full-text search index
CREATE INDEX idx_posts_search ON posts USING GIN (
  to_tsvector('english', title || ' ' || content)
);

-- Record migration
INSERT INTO schema_migrations (version, name)
VALUES ('0003', 'add_posts_indexes');
```

**Down Migration: 0003_add_posts_indexes_down.sql**
```sql
-- Remove indexes
DROP INDEX IF EXISTS idx_posts_user_id;
DROP INDEX IF EXISTS idx_posts_created_at;
DROP INDEX IF EXISTS idx_posts_search;

-- Remove migration record
DELETE FROM schema_migrations WHERE version = '0003';
```

### 4. Modifying Columns

**Up Migration: 0004_increase_title_length_up.sql**
```sql
-- Increase title column length
ALTER TABLE posts
ALTER COLUMN title TYPE VARCHAR(500);

-- Record migration
INSERT INTO schema_migrations (version, name)
VALUES ('0004', 'increase_title_length');
```

**Down Migration: 0004_increase_title_length_down.sql**
```sql
-- Decrease title column length (may fail if data too long)
ALTER TABLE posts
ALTER COLUMN title TYPE VARCHAR(255);

-- Remove migration record
DELETE FROM schema_migrations WHERE version = '0004';
```

### 5. Adding Constraints

**Up Migration: 0005_add_email_check_up.sql**
```sql
-- Add email format validation
ALTER TABLE users
ADD CONSTRAINT check_email_format
CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$');

-- Record migration
INSERT INTO schema_migrations (version, name)
VALUES ('0005', 'add_email_check');
```

**Down Migration: 0005_add_email_check_down.sql**
```sql
-- Remove constraint
ALTER TABLE users
DROP CONSTRAINT check_email_format;

-- Remove migration record
DELETE FROM schema_migrations WHERE version = '0005';
```

### 6. Data Migrations

**Up Migration: 0006_migrate_user_data_up.sql**
```sql
-- Add new column
ALTER TABLE users
ADD COLUMN full_name VARCHAR(255);

-- Migrate data from first_name + last_name
UPDATE users
SET full_name = CONCAT(first_name, ' ', last_name)
WHERE first_name IS NOT NULL AND last_name IS NOT NULL;

-- Drop old columns
ALTER TABLE users
DROP COLUMN first_name,
DROP COLUMN last_name;

-- Record migration
INSERT INTO schema_migrations (version, name)
VALUES ('0006', 'migrate_user_data');
```

**Down Migration: 0006_migrate_user_data_down.sql**
```sql
-- Add old columns back
ALTER TABLE users
ADD COLUMN first_name VARCHAR(100),
ADD COLUMN last_name VARCHAR(100);

-- Split full_name back into first_name and last_name
UPDATE users
SET first_name = SPLIT_PART(full_name, ' ', 1),
    last_name = SUBSTRING(full_name FROM POSITION(' ' IN full_name) + 1)
WHERE full_name IS NOT NULL;

-- Drop new column
ALTER TABLE users
DROP COLUMN full_name;

-- Remove migration record
DELETE FROM schema_migrations WHERE version = '0006';
```

## Migration Execution

### Manual Execution

```bash
# Run migration up
psql -U postgres -d myapp_db -f migrations/0001_initial_schema_up.sql

# Run migration down (rollback)
psql -U postgres -d myapp_db -f migrations/0001_initial_schema_down.sql

# Run all pending migrations
for file in migrations/*_up.sql; do
  psql -U postgres -d myapp_db -f "$file"
done
```

### Automated Migration Tool

```bash
# Generate migration from RDF changes
ggen migrate generate \
  --from ontology/old_schema.ttl \
  --to ontology/new_schema.ttl \
  --output migrations/0007_schema_changes

# Apply migrations
ggen migrate up --database postgresql

# Rollback last migration
ggen migrate down --database postgresql --steps 1

# Rollback to specific version
ggen migrate down --database postgresql --to 0003

# Check migration status
ggen migrate status
```

## Best Practices

### 1. Always Create Reversible Migrations

```sql
-- ✅ GOOD: Reversible
-- Up: Add column with default
ALTER TABLE users ADD COLUMN status VARCHAR(20) DEFAULT 'active';

-- Down: Remove column
ALTER TABLE users DROP COLUMN status;

-- ❌ BAD: Non-reversible data loss
-- Up: Drop column (data lost)
ALTER TABLE users DROP COLUMN important_data;

-- Down: Can't restore lost data
ALTER TABLE users ADD COLUMN important_data TEXT;  -- Data is gone!
```

### 2. Test Migrations on Copy of Production Data

```bash
# Create database copy
pg_dump production_db | psql test_db

# Test migration on copy
psql test_db -f migrations/0007_risky_change_up.sql

# Verify data integrity
psql test_db -c "SELECT COUNT(*) FROM users WHERE email IS NULL;"

# If successful, apply to production
psql production_db -f migrations/0007_risky_change_up.sql
```

### 3. Use Transactions for Safety

```sql
BEGIN;

-- Migration steps
ALTER TABLE users ADD COLUMN new_field VARCHAR(100);
UPDATE users SET new_field = 'default' WHERE new_field IS NULL;

-- Verify changes
SELECT COUNT(*) FROM users WHERE new_field IS NULL;

-- Commit if successful, rollback if errors
COMMIT;
-- ROLLBACK;
```

### 4. Handle Large Data Migrations in Batches

```sql
-- Process in batches to avoid locks
DO $$
DECLARE
  batch_size INTEGER := 1000;
  offset_val INTEGER := 0;
  total_rows INTEGER;
BEGIN
  SELECT COUNT(*) INTO total_rows FROM users WHERE full_name IS NULL;

  WHILE offset_val < total_rows LOOP
    UPDATE users
    SET full_name = CONCAT(first_name, ' ', last_name)
    WHERE id IN (
      SELECT id FROM users WHERE full_name IS NULL
      LIMIT batch_size OFFSET offset_val
    );

    offset_val := offset_val + batch_size;
    COMMIT;  -- Release locks between batches
  END LOOP;
END $$;
```

### 5. Document Breaking Changes

```sql
-- Migration: 0008_remove_deprecated_columns_up.sql
--
-- ⚠️  BREAKING CHANGE
-- This migration removes deprecated columns that were replaced in v2.0.
-- Applications using these columns must be updated to v2.0+ before migration.
--
-- Removed columns:
-- - users.legacy_id (replaced by users.id)
-- - posts.old_status (replaced by posts.status)
--
-- Migration steps:
-- 1. Deploy application v2.0+ to all servers
-- 2. Verify all servers using new columns
-- 3. Run this migration

ALTER TABLE users DROP COLUMN legacy_id;
ALTER TABLE posts DROP COLUMN old_status;
```

## Zero-Downtime Migrations

### Strategy 1: Expand-Contract Pattern

```sql
-- Phase 1: EXPAND (add new column)
-- Migration: 0009_expand_user_email_up.sql
ALTER TABLE users ADD COLUMN email_new VARCHAR(255);

-- Dual-write in application (write to both columns)
-- Wait for all instances to deploy

-- Phase 2: MIGRATE (copy data)
-- Migration: 0010_migrate_user_email_up.sql
UPDATE users SET email_new = email WHERE email_new IS NULL;

-- Phase 3: CONTRACT (remove old column)
-- Migration: 0011_contract_user_email_up.sql
ALTER TABLE users DROP COLUMN email;
ALTER TABLE users RENAME COLUMN email_new TO email;
```

### Strategy 2: Blue-Green Deployment

```sql
-- Create new version of table
CREATE TABLE users_v2 (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) NOT NULL UNIQUE,
  -- New schema changes
  full_name VARCHAR(255) NOT NULL
);

-- Copy data with transformation
INSERT INTO users_v2 (id, email, full_name)
SELECT id, email, CONCAT(first_name, ' ', last_name)
FROM users;

-- Switch (atomic rename)
BEGIN;
ALTER TABLE users RENAME TO users_v1_backup;
ALTER TABLE users_v2 RENAME TO users;
COMMIT;
```

## Rollback Strategies

### Automatic Rollback on Failure

```sql
BEGIN;

-- Migration steps
ALTER TABLE users ADD COLUMN phone VARCHAR(20);

-- Validation check
DO $$
BEGIN
  IF (SELECT COUNT(*) FROM users WHERE phone IS NOT NULL) > 0 THEN
    RAISE EXCEPTION 'Phone column should be NULL after adding';
  END IF;
END $$;

-- Record migration
INSERT INTO schema_migrations (version, name)
VALUES ('0012', 'add_user_phone');

COMMIT;
-- If any step fails, entire transaction rolls back
```

### Manual Rollback Verification

```bash
# Before rollback, verify data
psql -c "SELECT COUNT(*) FROM users WHERE bio IS NOT NULL;"

# Execute rollback
psql -f migrations/0002_add_user_bio_down.sql

# Verify rollback successful
psql -c "SELECT column_name FROM information_schema.columns WHERE table_name='users';"
```

## Migration Checksums

```sql
-- Generate checksum for migration file
CREATE OR REPLACE FUNCTION calculate_migration_checksum(migration_sql TEXT)
RETURNS VARCHAR(64) AS $$
BEGIN
  RETURN encode(digest(migration_sql, 'sha256'), 'hex');
END;
$$ LANGUAGE plpgsql;

-- Verify migration integrity
SELECT version, name
FROM schema_migrations
WHERE checksum != calculate_migration_checksum(
  pg_read_file('/path/to/migrations/' || version || '_' || name || '_up.sql')
);
```

## See Also

- [README.md](README.md) - Quick start guide
- [SCHEMA.md](SCHEMA.md) - RDF-to-SQL transformation
- [PERFORMANCE.md](PERFORMANCE.md) - Index optimization
