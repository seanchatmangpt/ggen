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
ggen:Database      → Represents entire database
ggen:Schema        → Logical grouping (PostgreSQL/MySQL)
ggen:Table         → Database table
ggen:Column        → Table column
ggen:DataType      → SQL data type
ggen:Constraint    → Base constraint class
ggen:Index         → Database index
ggen:Trigger       → Trigger definition
ggen:Migration     → Schema migration
```

### Constraint Hierarchy

```
ggen:Constraint (base)
  ├── ggen:PrimaryKey
  ├── ggen:ForeignKey
  ├── ggen:UniqueConstraint
  ├── ggen:CheckConstraint
  └── ggen:NotNullConstraint
```

### Index Hierarchy

```
ggen:Index (base)
  ├── ggen:BTreeIndex      → General-purpose (default)
  ├── ggen:HashIndex       → Equality comparisons
  ├── ggen:GINIndex        → JSONB, arrays, full-text
  ├── ggen:GiSTIndex       → Geometric, range types
  └── ggen:FullTextIndex   → Text search
```

## SPARQL Transformation Patterns

### Pattern 1: Table Creation

**RDF Input:**
```turtle
:UsersTable a ggen:Table ;
    ggen:tableName "users" ;
    ggen:hasColumn :IdColumn, :EmailColumn .

:IdColumn a ggen:Column ;
    ggen:columnName "id" ;
    ggen:hasDataType :SerialType ;
    ggen:isAutoIncrement true .
```

**SPARQL Query:**
```sparql
SELECT ?tableName ?columnDefs
WHERE {
  ?table a ggen:Table ;
         ggen:tableName ?tableName ;
         ggen:hasColumn ?column .

  ?column ggen:columnName ?columnName ;
          ggen:hasDataType ?dataType .
  ?dataType ggen:dataTypeName ?typeName .

  OPTIONAL { ?column ggen:isAutoIncrement ?autoInc }

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
:ProductsCategoryFK a ggen:ForeignKey ;
    ggen:constraintName "fk_products_category" ;
    ggen:constrainsColumn :ProductCategoryIdColumn ;
    ggen:referencesTable :CategoriesTable ;
    ggen:referencesColumn :CategoryIdColumn ;
    ggen:onDelete "CASCADE" .
```

**SPARQL Query:**
```sparql
SELECT ?sourceTable ?sourceColumn ?targetTable ?targetColumn ?onDelete
WHERE {
  ?fk a ggen:ForeignKey ;
      ggen:constrainsColumn ?srcColumn ;
      ggen:referencesTable ?tgtTable ;
      ggen:referencesColumn ?tgtColumn .

  ?srcColumn ggen:columnName ?sourceColumn ;
             ^ggen:hasColumn ?srcTable .
  ?srcTable ggen:tableName ?sourceTable .

  ?tgtTable ggen:tableName ?targetTable .
  ?tgtColumn ggen:columnName ?targetColumn .

  OPTIONAL { ?fk ggen:onDelete ?onDelete }
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
:EmailGinIndex a ggen:GINIndex ;
    ggen:indexName "idx_emails_gin" ;
    ggen:indexesColumn :EmailColumn ;
    ggen:isConcurrent true .
```

**SPARQL Query:**
```sparql
SELECT ?indexName ?indexType ?tableName ?columnList ?isConcurrent
WHERE {
  ?index a ?indexClass ;
         ggen:indexName ?indexName ;
         ggen:indexesColumn ?column .

  ?column ^ggen:hasColumn ?table .
  ?table ggen:tableName ?tableName .

  OPTIONAL { ?index ggen:isConcurrent ?isConcurrent }

  BIND(IF(?indexClass = ggen:GINIndex, "GIN", "BTREE") AS ?indexType)
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
:JsonbType a ggen:DataType ;
    ggen:postgresqlType "JSONB" ;
    ggen:mysqlType "JSON" ;
    ggen:sqliteType "TEXT" .

:ArrayType a ggen:DataType ;
    ggen:postgresqlType "INTEGER[]" ;
    ggen:mysqlType "JSON" ;
    ggen:sqliteType "TEXT" .
```

**MySQL:**
```turtle
:AutoIncrementType a ggen:DataType ;
    ggen:postgresqlType "SERIAL" ;
    ggen:mysqlType "INT AUTO_INCREMENT" ;
    ggen:sqliteType "INTEGER PRIMARY KEY AUTOINCREMENT" .
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
:Migration0001 a ggen:Migration ;
    ggen:migrationId "0001" ;
    ggen:migrationName "create_users_table" ;
    ggen:migrationTimestamp "2025-01-08T12:00:00Z" ;
    ggen:upScript """
        CREATE TABLE users (
            id SERIAL PRIMARY KEY,
            email VARCHAR(255) NOT NULL UNIQUE
        );
    """ ;
    ggen:downScript """
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
:UpdatedAtTrigger a ggen:Trigger ;
    ggen:triggerName "trigger_updated_at" ;
    ggen:triggerTiming "BEFORE" ;
    ggen:triggerEvent "UPDATE" ;
    ggen:triggerLevel "ROW" ;
    ggen:triggerFunction "update_updated_at_column()" .
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
:ArticleSearchIndex a ggen:FullTextIndex ;
    ggen:indexName "idx_articles_search" ;
    ggen:indexesColumn :TitleColumn, :ContentColumn .
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
  ?fk1 ggen:constrainsColumn ?col1 .
  ?col1 ^ggen:hasColumn ?table1 .
  ?fk1 ggen:referencesTable ?table2 .

  ?fk2 ggen:constrainsColumn ?col2 .
  ?col2 ^ggen:hasColumn ?table2 .
  ?fk2 ggen:referencesTable ?table1 .
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
