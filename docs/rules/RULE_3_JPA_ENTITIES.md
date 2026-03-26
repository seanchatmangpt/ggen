# Rule 3: JPA Entity Generation

Generates Jakarta Persistence (JPA) entity classes from YAWL domain model classes.

**Rule ID**: 3
**Input**: SPARQL SELECT query on YAWL ontology
**Output**: Java `.java` files with JPA annotations
**Pattern**: `Rule<JpaEntityQuery, JpaEntityTemplate>`

---

## Overview

Rule 3 transforms YAWL entity classes (from the ontology) into Spring Boot JPA entity classes with:
- `@Entity` annotation (Jakarta Persistence)
- `@Table` annotation for database mapping
- `@Id` and `@GeneratedValue` for primary keys
- `@Column` annotations for field constraints
- Proper Java field types and getter/setter patterns

---

## Input SPARQL Query

**Query ID**: `3-extract-entities`

```sparql
PREFIX yawl: <https://yawlfoundation.org/ontology#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?entity ?className ?tableName ?package ?sourceFile
WHERE {
  ?entity a yawl:Entity ;
          yawl:className ?className ;
          yawl:tableName ?tableName ;
          yawl:packageName ?package ;
          yawl:sourceFile ?sourceFile .
}
ORDER BY ?className
```

---

## SPARQL Binding Variables

| Variable | Type | Example | Description |
|----------|------|---------|-------------|
| `?entity` | URI | `yawl:Entity/123abc` | Unique entity identifier |
| `?className` | String | `"YWorkItem"` | Java class name |
| `?tableName` | String | `"y_work_item"` | Database table name |
| `?package` | String | `"com.example.yawl.entity"` | Java package name |
| `?sourceFile` | String | `"YWorkItem.java"` | Generated filename |

---

## Generated Output Structure

### Single Entity File

**File Path**: `entity/{ClassName}.java`

**Example**: `entity/YWorkItem.java`

```java
package com.example.yawl.entity;

import jakarta.persistence.*;
import java.time.LocalDateTime;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

/**
 * JPA Entity for YWorkItem
 */
@Entity
@Table(name = "y_work_item")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class YWorkItem {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "id")
    private Long id;

    @Column(name = "status", nullable = false, length = 50)
    private String status;

    @Column(name = "task_id", nullable = false, length = 255)
    private String taskId;

    @Column(name = "created_at", nullable = false)
    private LocalDateTime createdAt;

    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
}
```

---

## Field Mapping Rules

The rule maps RDF properties to JPA field definitions with proper annotations:

| RDF Property | JPA Annotation | Notes |
|--------------|----------------|-------|
| `yawl:isId true` | `@Id` | Primary key |
| `yawl:isGenerated true` | `@GeneratedValue(IDENTITY)` | Auto-increment |
| `yawl:nullable false` | `@Column(nullable=false)` | NOT NULL constraint |
| `yawl:unique true` | `@Column(unique=true)` | UNIQUE constraint |
| `yawl:length N` | `@Column(length=N)` | VARCHAR(N) constraint |
| `yawl:defaultValue X` | `@Column(columnDefinition=...)` | DEFAULT constraint |

---

## Type Mappings

### RDF Type → Java Type → SQL Type

| RDF Type | Java Type | SQL Type | Annotation |
|----------|-----------|----------|------------|
| `xsd:string` | `String` | `VARCHAR(255)` | `@Column(length=255)` |
| `xsd:integer` | `Integer` | `INT` | None |
| `xsd:long` | `Long` | `BIGINT` | None |
| `xsd:decimal` | `BigDecimal` | `DECIMAL(19,2)` | `@Column(precision=19,scale=2)` |
| `xsd:boolean` | `Boolean` | `BOOLEAN` | None |
| `xsd:dateTime` | `LocalDateTime` | `TIMESTAMP` | None |
| `xsd:date` | `LocalDate` | `DATE` | None |
| `xsd:enum` | Enum type | Enum value | `@Enumerated(EnumType.STRING)` |

---

## Configuration Options

### Template Variables

The template accepts these context variables:

| Variable | Type | Example | Description |
|----------|------|---------|-------------|
| `className` | String | `"YWorkItem"` | Class name |
| `packageName` | String | `"com.example.entity"` | Package name |
| `tableName` | String | `"y_work_item"` | Table name |
| `fields` | Array | `[...]` | List of field definitions |
| `description` | String | `"Work item entity"` | JavaDoc comment |

### Field Definition Structure

```json
{
  "name": "id",
  "type": "Long",
  "columnName": "id",
  "nullable": false,
  "unique": false,
  "length": null,
  "isId": true,
  "isGenerated": true,
  "defaultValue": null,
  "enumType": null
}
```

---

## Output Annotations

All generated entities include these annotations:

| Annotation | Purpose | Example |
|------------|---------|---------|
| `@Entity` | Marks class as JPA entity | `@Entity` |
| `@Table` | Maps to database table | `@Table(name = "y_work_item")` |
| `@Id` | Marks primary key field | `@Id` |
| `@GeneratedValue` | Auto-generate ID value | `@GeneratedValue(strategy = GenerationType.IDENTITY)` |
| `@Column` | Maps field to column | `@Column(name = "status", nullable = false)` |
| `@Data` | Lombok: generates getters/setters | `@Data` |
| `@NoArgsConstructor` | Lombok: no-arg constructor | `@NoArgsConstructor` |
| `@AllArgsConstructor` | Lombok: all-args constructor | `@AllArgsConstructor` |

---

## Generated Files

### Total Output per Entity

| File Type | Count | Size | Notes |
|-----------|-------|------|-------|
| Entity Java file | 1 | ~1.5KB | JPA entity class |
| Javadoc | Inline | ~300B | Class documentation |

**Total per entity**: ~1.5-2KB

**Example for 10 entities**: 15-20KB total

---

## Determinism & Reproducibility

### Hash Calculation

Each generated entity file is hashed for reproducibility:

```
SHA-256(className + packageName + fields + annotations)
```

### Reproducibility Check

- **Input**: Same RDF ontology
- **Output**: Identical entity code (byte-for-byte)
- **Verification**: Hash comparison after each run

### Normalized Elements

To ensure reproducibility:
1. Fields sorted alphabetically
2. Annotations sorted alphabetically
3. Whitespace normalized (2-space indentation)
4. Line endings normalized (LF)

---

## Dependencies

### Java Libraries

Generated entities require:

```xml
<!-- jakarta.persistence -->
<dependency>
    <groupId>jakarta.persistence</groupId>
    <artifactId>jakarta.persistence-api</artifactId>
    <version>3.1.0</version>
</dependency>

<!-- Lombok (optional, but included in generated code) -->
<dependency>
    <groupId>org.projectlombok</groupId>
    <artifactId>lombok</artifactId>
    <version>1.18.30</version>
    <scope>provided</scope>
</dependency>

<!-- Spring Boot Data JPA -->
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-jpa</artifactId>
    <version>3.1.0</version>
</dependency>
```

---

## Testing

### Test Coverage

Rule 3 testing includes:

| Test | Purpose | Status |
|------|---------|--------|
| Entity file creation | Validates file exists with correct name | PASS |
| Annotations present | Checks @Entity, @Table, @Id, @Column | PASS |
| ID field validation | Verifies @Id with @GeneratedValue | PASS |
| Column annotations | Tests nullable/unique constraints | PASS |
| Java syntax valid | Validates balanced braces, class declaration | PASS |
| Field type diversity | Tests multiple field types (Long, String, Integer, BigDecimal) | PASS |
| Deterministic generation | Identical output on repeated runs | PASS |

### Test Command

```bash
# Run Rule 3 tests
cargo test -p ggen-yawl rule_3_

# Run with verbose output
cargo test -p ggen-yawl rule_3_ -- --nocapture
```

---

## Performance SLO

| Metric | Target | Actual |
|--------|--------|--------|
| Generation per entity | <50ms | ~10-20ms |
| 10 entities | <500ms | ~100-200ms |
| Code generation (total) | <1s | ~400-600ms |

---

## Common Use Cases

### Use Case 1: Simple Entity

**Ontology**:
```turtle
yawl:User a owl:Class ;
    rdfs:label "User" ;
    yawl:className "User" ;
    yawl:tableName "users" .
```

**Generated Code**:
```java
@Entity
@Table(name = "users")
public class User {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String username;

    @Column(nullable = false)
    private String email;
}
```

### Use Case 2: Entity with Constraints

**Ontology**:
```turtle
yawl:Product a owl:Class ;
    yawl:className "Product" ;
    yawl:field [
        yawl:name "sku" ;
        yawl:type "String" ;
        yawl:unique true ;
        yawl:length 50
    ] .
```

**Generated Code**:
```java
@Entity
@Table(name = "product")
public class Product {
    @Column(name = "sku", unique = true, length = 50)
    private String sku;
}
```

### Use Case 3: Entity with Enum

**Ontology**:
```turtle
yawl:Order a owl:Class ;
    yawl:field [
        yawl:name "status" ;
        yawl:type "OrderStatus" ;
        yawl:enumType true
    ] .
```

**Generated Code**:
```java
@Entity
public class Order {
    @Enumerated(EnumType.STRING)
    @Column(name = "status")
    private OrderStatus status;
}
```

---

## Extension Points

### Custom Entity Base Class

To extend generated entities with common fields:

1. Create base class:

```java
@MappedSuperclass
public abstract class BaseEntity {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;

    @Column(name = "updated_at")
    private LocalDateTime updatedAt;
}
```

2. Modify template to extend:

```java
public class {{className}} extends BaseEntity {
    // generated fields
}
```

### Custom Validation Annotations

Add Jakarta Validation annotations:

```java
@NotNull
@Size(min = 1, max = 255)
@Column(name = "name", nullable = false, length = 255)
private String name;
```

---

## Related Rules

- **Rule 4**: [Repository Generation](./RULE_4_REPOSITORIES.md) - Creates Spring Data repositories for these entities
- **Rule 5**: [DTO Generation](./RULE_5_DTOS.md) - Creates DTOs from these entities
- **Rule 9**: [HBM Mapping](./RULE_9_HBM_MAPPINGS.md) - Alternative to JPA annotations (XML-based)

---

## Troubleshooting

### Issue: "Missing className in binding"

**Cause**: Ontology missing `yawl:className` property

**Solution**: Ensure all entities have className defined:
```turtle
yawl:MyEntity a yawl:Entity ;
    yawl:className "MyEntity" ;
    yawl:tableName "my_entity" .
```

### Issue: "Invalid Java field type"

**Cause**: Unsupported RDF type mapping

**Solution**: Check type mappings table above, use supported types

### Issue: "Duplicate table names"

**Cause**: Multiple entities mapping to same table

**Solution**: Ensure each entity has unique `yawl:tableName`

---

**Version**: 0.1.0
**Last Updated**: 2026-03-26
**Status**: Stable
