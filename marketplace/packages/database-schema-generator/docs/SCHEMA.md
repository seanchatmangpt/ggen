# RDF-to-SQL Schema Transformation

This document explains how the Database Schema Generator transforms RDF ontology definitions into SQL DDL statements.

## Architecture

```
RDF Ontology (Turtle)
        ↓
SPARQL Queries (16+ transformations)
        ↓
Intermediate Data Model
        ↓
Handlebars Templates (PostgreSQL/MySQL/SQLite)
        ↓
SQL DDL Output
```

## RDF Ontology Structure

### Core Classes

```turtle
mcpp:Database      → Represents entire database
mcpp:Schema        → Logical grouping (PostgreSQL/MySQL)
mcpp:Table         → Database table
mcpp:Column        → Table column
mcpp:DataType      → SQL data type
mcpp:Constraint    → Base constraint class
mcpp:Index         → Database index
mcpp:Trigger       → Trigger definition
mcpp:Migration     → Schema migration
```

### Constraint Hierarchy

```
mcpp:Constraint (base)
  ├── mcpp:PrimaryKey
  ├── mcpp:ForeignKey
  ├── mcpp:UniqueConstraint
  ├── mcpp:CheckConstraint
  └── mcpp:NotNullConstraint
```

### Index Hierarchy

```
mcpp:Index (base)
  ├── mcpp:BTreeIndex      → General-purpose (default)
  ├── mcpp:HashIndex       → Equality comparisons
  ├── mcpp:GINIndex        → JSONB, arrays, full-text
  ├── mcpp:GiSTIndex       → Geometric, range types
  └── mcpp:FullTextIndex   → Text search
```

## SPARQL Transformation Patterns

### Pattern 1: Table Creation

**RDF Input:**
```turtle
:UsersTable a mcpp:Table ;
    mcpp:tableName "users" ;
    mcpp:hasColumn :IdColumn, :EmailColumn .

:IdColumn a mcpp:Column ;
    mcpp:columnName "id" ;
    mcpp:hasDataType :SerialType ;
    mcpp:isAutoIncrement true .
```

**SPARQL Query:**
```sparql
SELECT ?tableName ?columnDefs
WHERE {
  ?table a mcpp:Table ;
         mcpp:tableName ?tableName ;
         mcpp:hasColumn ?column .

  ?column mcpp:columnName ?columnName ;
          mcpp:hasDataType ?dataType .
  ?dataType mcpp:dataTypeName ?typeName .

  OPTIONAL { ?column mcpp:isAutoIncrement ?autoInc }

  BIND(CONCAT(?columnName, " ", ?typeName,
    IF(?autoInc, " AUTO_INCREMENT", "")) AS ?columnDef)
}
```

**SQL Output (PostgreSQL):**
```sql
CREATE TABLE users (
  id SERIAL,
  email VARCHAR(255)
);
```

### Pattern 2: Foreign Keys

**RDF Input:**
```turtle
:ProductsCategoryFK a mcpp:ForeignKey ;
    mcpp:constraintName "fk_products_category" ;
    mcpp:constrainsColumn :ProductCategoryIdColumn ;
    mcpp:referencesTable :CategoriesTable ;
    mcpp:referencesColumn :CategoryIdColumn ;
    mcpp:onDelete "CASCADE" .
```

**SPARQL Query:**
```sparql
SELECT ?sourceTable ?sourceColumn ?targetTable ?targetColumn ?onDelete
WHERE {
  ?fk a mcpp:ForeignKey ;
      mcpp:constrainsColumn ?srcColumn ;
      mcpp:referencesTable ?tgtTable ;
      mcpp:referencesColumn ?tgtColumn .

  ?srcColumn mcpp:columnName ?sourceColumn ;
             ^mcpp:hasColumn ?srcTable .
  ?srcTable mcpp:tableName ?sourceTable .

  ?tgtTable mcpp:tableName ?targetTable .
  ?tgtColumn mcpp:columnName ?targetColumn .

  OPTIONAL { ?fk mcpp:onDelete ?onDelete }
}
```

**SQL Output:**
```sql
ALTER TABLE products
ADD CONSTRAINT fk_products_category
FOREIGN KEY (category_id)
REFERENCES categories(id)
ON DELETE CASCADE;
```

### Pattern 3: Indexes

**RDF Input:**
```turtle
:EmailGinIndex a mcpp:GINIndex ;
    mcpp:indexName "idx_emails_gin" ;
    mcpp:indexesColumn :EmailColumn ;
    mcpp:isConcurrent true .
```

**SPARQL Query:**
```sparql
SELECT ?indexName ?indexType ?tableName ?columnList ?isConcurrent
WHERE {
  ?index a ?indexClass ;
         mcpp:indexName ?indexName ;
         mcpp:indexesColumn ?column .

  ?column ^mcpp:hasColumn ?table .
  ?table mcpp:tableName ?tableName .

  OPTIONAL { ?index mcpp:isConcurrent ?isConcurrent }

  BIND(IF(?indexClass = mcpp:GINIndex, "GIN", "BTREE") AS ?indexType)
}
```

**SQL Output (PostgreSQL):**
```sql
CREATE INDEX CONCURRENTLY idx_emails_gin
ON users USING GIN (email);
```

## Data Type Mappings

### XSD to SQL Type Mapping

| XSD Type | PostgreSQL | MySQL | SQLite |
|----------|-----------|--------|---------|
| xsd:string | VARCHAR | VARCHAR | TEXT |
| xsd:integer | INTEGER | INT | INTEGER |
| xsd:decimal | DECIMAL | DECIMAL | REAL |
| xsd:boolean | BOOLEAN | TINYINT(1) | INTEGER |
| xsd:date | DATE | DATE | TEXT |
| xsd:dateTime | TIMESTAMP | DATETIME | TEXT |

### Database-Specific Types

**PostgreSQL:**
```turtle
:JsonbType a mcpp:DataType ;
    mcpp:postgresqlType "JSONB" ;
    mcpp:mysqlType "JSON" ;
    mcpp:sqliteType "TEXT" .

:ArrayType a mcpp:DataType ;
    mcpp:postgresqlType "INTEGER[]" ;
    mcpp:mysqlType "JSON" ;
    mcpp:sqliteType "TEXT" .
```

**MySQL:**
```turtle
:AutoIncrementType a mcpp:DataType ;
    mcpp:postgresqlType "SERIAL" ;
    mcpp:mysqlType "INT AUTO_INCREMENT" ;
    mcpp:sqliteType "INTEGER PRIMARY KEY AUTOINCREMENT" .
```

## Template Processing

### Handlebars Template Structure

```handlebars
{{#each tables}}
CREATE TABLE {{this.name}} (
  {{#each this.columns}}
  {{this.name}} {{this.type}}
    {{#unless this.nullable}}NOT NULL{{/unless}}
    {{#if this.default}}DEFAULT {{this.default}}{{/if}}
  {{/each}}

  {{#if this.constraints}}
  {{#each this.constraints}}
  CONSTRAINT {{this.name}} {{this.type}} ({{this.columns}})
  {{/each}}
  {{/if}}
);
{{/each}}
```

### Template Variables

```json
{
  "databaseName": "myapp_db",
  "characterSet": "UTF8",
  "collation": "en_US.UTF-8",
  "tables": [
    {
      "name": "users",
      "comment": "Application users",
      "columns": [
        {
          "name": "id",
          "type": "SERIAL",
          "nullable": false,
          "autoIncrement": true
        }
      ],
      "constraints": [
        {
          "name": "users_pkey",
          "type": "PRIMARY KEY",
          "columns": "id"
        }
      ]
    }
  ]
}
```

## Migration Generation

### Migration Versioning

```turtle
:Migration0001 a mcpp:Migration ;
    mcpp:migrationId "0001" ;
    mcpp:migrationName "create_users_table" ;
    mcpp:migrationTimestamp "2025-01-08T12:00:00Z" ;
    mcpp:upScript """
        CREATE TABLE users (
            id SERIAL PRIMARY KEY,
            email VARCHAR(255) NOT NULL UNIQUE
        );
    """ ;
    mcpp:downScript """
        DROP TABLE users;
    """ .
```

### Migration Tracking

```sql
CREATE TABLE schema_migrations (
  version VARCHAR(255) PRIMARY KEY,
  name VARCHAR(255) NOT NULL,
  applied_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  execution_time_ms INTEGER,
  checksum VARCHAR(64)
);

-- Record migration
INSERT INTO schema_migrations (version, name, checksum)
VALUES ('0001', 'create_users_table', 'sha256hash');
```

## Advanced Transformations

### Trigger Generation

```turtle
:UpdatedAtTrigger a mcpp:Trigger ;
    mcpp:triggerName "trigger_updated_at" ;
    mcpp:triggerTiming "BEFORE" ;
    mcpp:triggerEvent "UPDATE" ;
    mcpp:triggerLevel "ROW" ;
    mcpp:triggerFunction "update_updated_at_column()" .
```

**PostgreSQL Output:**
```sql
CREATE TRIGGER trigger_updated_at
  BEFORE UPDATE ON users
  FOR EACH ROW
  EXECUTE FUNCTION update_updated_at_column();
```

**MySQL Output:**
```sql
DELIMITER $$
CREATE TRIGGER trigger_updated_at
  BEFORE UPDATE ON users
  FOR EACH ROW
BEGIN
  SET NEW.updated_at = CURRENT_TIMESTAMP;
END$$
DELIMITER ;
```

### Full-Text Search

```turtle
:ArticleSearchIndex a mcpp:FullTextIndex ;
    mcpp:indexName "idx_articles_search" ;
    mcpp:indexesColumn :TitleColumn, :ContentColumn .
```

**PostgreSQL:**
```sql
CREATE INDEX idx_articles_search
ON articles USING GIN (
  to_tsvector('english', title || ' ' || content)
);
```

**MySQL:**
```sql
CREATE FULLTEXT INDEX idx_articles_search
ON articles (title, content);
```

## Optimization Strategies

### Index Selection Algorithm

1. **Primary Key**: Always B-tree (default)
2. **Foreign Key**: B-tree on FK column
3. **JSONB/JSON**: GIN index for containment queries
4. **Arrays**: GIN index for element search
5. **Full-text**: GIN (PostgreSQL), FULLTEXT (MySQL)
6. **Equality**: Hash index (PostgreSQL only)
7. **Range queries**: B-tree (default)

### Constraint Order

1. Create tables (no constraints)
2. Add PRIMARY KEY constraints
3. Add UNIQUE constraints
4. Add CHECK constraints
5. Add FOREIGN KEY constraints (after referenced tables exist)
6. Create indexes

## Error Handling

### Validation Rules

- Foreign keys must reference existing tables
- Primary keys cannot be nullable
- Auto-increment requires integer type
- Unique constraints on nullable columns (warning)
- Circular foreign key dependencies (error)

### SPARQL Query Validation

```sparql
# Detect circular foreign keys
SELECT ?table1 ?table2
WHERE {
  ?fk1 mcpp:constrainsColumn ?col1 .
  ?col1 ^mcpp:hasColumn ?table1 .
  ?fk1 mcpp:referencesTable ?table2 .

  ?fk2 mcpp:constrainsColumn ?col2 .
  ?col2 ^mcpp:hasColumn ?table2 .
  ?fk2 mcpp:referencesTable ?table1 .
}
```

## Performance Considerations

- **Query Optimization**: Use OPTIONAL for sparse properties
- **Aggregation**: GROUP_CONCAT for multi-column constraints
- **Indexing**: Index frequently queried RDF properties
- **Caching**: Cache SPARQL results for large ontologies

## See Also

- [MIGRATIONS.md](MIGRATIONS.md) - Migration strategies
- [PERFORMANCE.md](PERFORMANCE.md) - Indexing best practices
- [README.md](README.md) - Quick start guide
