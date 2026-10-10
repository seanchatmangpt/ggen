# Database Schema Generator

Generate production-ready database schemas for PostgreSQL, MySQL, and SQLite from RDF ontology using SPARQL transformations.

## Features

- **RDF Ontology-Driven**: Define database schema semantically with 250+ lines of ontology
- **Multi-Database Support**: PostgreSQL, MySQL, SQLite with database-specific optimizations
- **Advanced Constraints**: Primary keys, foreign keys, unique, check, not null
- **Performance Indexes**: B-tree, hash, GIN, GiST, full-text search
- **Triggers & Procedures**: Automated timestamp updates, validation, audit trails
- **Migration Management**: Up/down scripts with version tracking
- **SPARQL Queries**: 16+ queries to transform RDF to SQL DDL
- **Chicago TDD Testing**: 600+ lines of real database tests with testcontainers

## Quick Start

```bash
# Install package
ggen marketplace install database-schema-generator

# Generate schema for PostgreSQL
ggen generate database-schema-generator \
  --database postgresql \
  --output ./db/schema.sql

# Generate for all databases
ggen generate database-schema-generator --all

# Generate with migrations
ggen generate database-schema-generator \
  --database postgresql \
  --migrations \
  --output ./db/migrations/
```

## Usage

### 1. Define Your Schema in RDF

```turtle
@prefix ggen: <http://ggen.io/ontology/database#> .

# Define database
:MyDatabase a ggen:Database ;
    ggen:databaseName "myapp_production" ;
    ggen:databaseEngine "postgresql" ;
    ggen:characterSet "UTF8" ;
    ggen:hasTable :UsersTable .

# Define table
:UsersTable a ggen:Table ;
    ggen:tableName "users" ;
    ggen:tableComment "Application users" ;
    ggen:hasColumn :IdColumn, :EmailColumn, :CreatedAtColumn ;
    ggen:hasConstraint :UsersPK, :EmailUnique ;
    ggen:hasIndex :EmailIndex .

# Define columns
:IdColumn a ggen:Column ;
    ggen:columnName "id" ;
    ggen:hasDataType :SerialType ;
    ggen:isAutoIncrement true ;
    ggen:isNullable false .

:EmailColumn a ggen:Column ;
    ggen:columnName "email" ;
    ggen:hasDataType :VarcharType ;
    ggen:maxLength 255 ;
    ggen:isNullable false .

# Define constraints
:UsersPK a ggen:PrimaryKey ;
    ggen:constraintName "users_pkey" ;
    ggen:constrainsColumn :IdColumn .

:EmailUnique a ggen:UniqueConstraint ;
    ggen:constraintName "users_email_unique" ;
    ggen:constrainsColumn :EmailColumn .
```

### 2. Run SPARQL Queries

The package includes 16+ SPARQL queries:

- **create_tables**: Generate CREATE TABLE statements
- **create_indexes**: Generate CREATE INDEX statements
- **create_triggers**: Generate trigger definitions
- **foreign_key_relationships**: Extract FK relationships
- **alter_table_migrations**: Generate ALTER TABLE migrations
- **generate_seed_data**: Create INSERT statements

### 3. Generate SQL DDL

Output for PostgreSQL:

```sql
CREATE TABLE users (
  id SERIAL NOT NULL,
  email VARCHAR(255) NOT NULL,
  username VARCHAR(100) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,

  CONSTRAINT users_pkey PRIMARY KEY (id),
  CONSTRAINT users_email_unique UNIQUE (email)
);

CREATE INDEX idx_users_email ON users USING btree (email);

CREATE TRIGGER trigger_users_updated_at
  BEFORE UPDATE ON users
  FOR EACH ROW
  EXECUTE FUNCTION update_updated_at_column();
```

## Database-Specific Features

### PostgreSQL

- JSONB columns with GIN indexes
- Array types
- Full-text search with tsvector
- SERIAL/BIGSERIAL auto-increment
- Row-level security (RLS)
- Concurrent index building

### MySQL

- InnoDB engine optimization
- AUTO_INCREMENT
- FULLTEXT indexes
- DELIMITER for triggers
- Character set per table

### SQLite

- Lightweight constraints
- AUTOINCREMENT
- WAL journal mode
- Performance PRAGMAs
- In-memory temp storage

## Migrations

Generate up/down migration scripts:

```sql
-- Up migration: 0001_create_users_table.sql
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  email VARCHAR(255) NOT NULL UNIQUE
);

INSERT INTO schema_migrations (version, name)
VALUES ('0001', 'create_users_table');

-- Down migration: 0001_create_users_table_down.sql
DROP TABLE users;

DELETE FROM schema_migrations
WHERE version = '0001';
```

Track migrations:

```sql
CREATE TABLE schema_migrations (
  version VARCHAR(255) PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  execution_time_ms INTEGER,
  checksum VARCHAR(64)
);
```

## Testing

Run Chicago TDD test suite:

```bash
# Run all tests
cargo test --package database-schema-generator

# Run specific test
cargo test test_postgresql_schema_creation

# Run with real PostgreSQL container
cargo test --features testcontainers
```

Tests cover:

- Schema creation across all databases
- Primary key, foreign key, unique, check constraints
- Index performance (B-tree, GIN, full-text)
- Trigger execution
- Migration up/down
- Transaction rollback
- JSONB querying
- Composite indexes

## Advanced Usage

### Custom Data Types

```turtle
:JsonbType a ggen:DataType ;
    ggen:dataTypeName "JSONB" ;
    ggen:postgresqlType "JSONB" ;
    ggen:mysqlType "JSON" ;
    ggen:sqliteType "TEXT" .
```

### Triggers

```turtle
:UpdatedAtTrigger a ggen:Trigger ;
    ggen:triggerName "trigger_updated_at" ;
    ggen:triggerTiming "BEFORE" ;
    ggen:triggerEvent "UPDATE" ;
    ggen:triggerLevel "ROW" ;
    ggen:triggerFunction "update_updated_at_column()" .
```

### Indexes

```turtle
:EmailGinIndex a ggen:GINIndex ;
    ggen:indexName "idx_users_email_gin" ;
    ggen:indexesColumn :EmailColumn ;
    ggen:isConcurrent true .
```

## Configuration

```toml
[config]
default_database = "postgresql"
enable_migrations = true
enable_triggers = true
enable_indexes = true
enable_fulltext = true
enable_rls = false  # PostgreSQL only
```

## Performance

- **Index creation**: < 10ms for 10,000 rows (B-tree)
- **JSONB queries**: GIN index optimization
- **Full-text search**: tsvector with trigram matching
- **Composite indexes**: Multi-column query optimization

## License

MIT
