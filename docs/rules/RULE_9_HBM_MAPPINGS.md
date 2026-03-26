# Rule 9: Hibernate HBM XML Mapping Generation

Generates Hibernate HBM (Hibernate Mapping) XML files as alternative to JPA annotations.

**Rule ID**: 9 | **Input**: JPA entities | **Output**: XML `.hbm.xml` mapping files

## Overview

Rule 9 creates Hibernate HBM XML mappings:
- Alternative to JPA `@Entity` annotations
- XML-based ORM mapping configuration
- Suitable for legacy systems or annotation-free environments
- Complete entity-to-table mapping

## Generated Output

**File Path**: `hbm/{EntityName}.hbm.xml`

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE hibernate-mapping PUBLIC
    "-//Hibernate/Hibernate Mapping DTD 3.0//EN"
    "http://hibernate.sourceforge.net/hibernate-mapping-3.0.dtd">

<hibernate-mapping package="com.example.yawl.entity">
    <class name="YWorkItem" table="y_work_item">
        <meta attribute="class-description">
            Work Item entity
        </meta>

        <!-- Primary Key -->
        <id name="id" column="id" type="java.lang.Long">
            <generator class="native"/>
        </id>

        <!-- Properties -->
        <property name="status" type="string">
            <column name="status" length="50" not-null="true"/>
        </property>

        <property name="taskId" type="string">
            <column name="task_id" length="255" not-null="true"/>
        </property>

        <property name="createdAt" type="timestamp">
            <column name="created_at" not-null="true"/>
        </property>

        <property name="updatedAt" type="timestamp">
            <column name="updated_at"/>
        </property>
    </class>
</hibernate-mapping>
```

## XML Structure

### Root Element

```xml
<hibernate-mapping package="com.example.yawl.entity">
```

- `package` - Default package for all classes

### Class Element

```xml
<class name="YWorkItem" table="y_work_item">
```

- `name` - Entity class name
- `table` - Database table name

### ID Element

```xml
<id name="id" column="id" type="java.lang.Long">
    <generator class="native"/>
</id>
```

- `name` - Property name
- `column` - Database column
- `type` - Java type
- `generator` - ID generation strategy (`native`, `uuid`, `assigned`)

### Property Element

```xml
<property name="status" type="string">
    <column name="status" length="50" not-null="true"/>
</property>
```

- `name` - Property name
- `type` - Java/Hibernate type
- `column` - Database column configuration

### Column Attributes

| Attribute | Example | Meaning |
|-----------|---------|---------|
| `name` | `status` | Column name |
| `length` | `50` | VARCHAR length |
| `not-null` | `true` | NOT NULL constraint |
| `unique` | `true` | UNIQUE constraint |
| `precision` | `19` | DECIMAL precision |
| `scale` | `2` | DECIMAL scale |

## Type Mappings

| Java Type | Hibernate Type | SQL Type |
|-----------|----------------|----------|
| `String` | `string` | `VARCHAR` |
| `Integer` | `integer` | `INT` |
| `Long` | `long` | `BIGINT` |
| `BigDecimal` | `big_decimal` | `DECIMAL` |
| `Boolean` | `boolean` | `BOOLEAN` |
| `LocalDateTime` | `timestamp` | `TIMESTAMP` |
| `LocalDate` | `date` | `DATE` |
| Enum | `string` | `VARCHAR` |

## ID Generators

### Native (Database-Specific)

```xml
<id name="id" column="id" type="java.lang.Long">
    <generator class="native"/>
</id>
```

- Auto-increment (MySQL)
- Sequence (PostgreSQL, Oracle)

### UUID

```xml
<id name="id" column="id" type="string">
    <generator class="uuid"/>
</id>
```

### Assigned (Manual)

```xml
<id name="id" column="id" type="java.lang.Long">
    <generator class="assigned"/>
</id>
```

## Generated Files

| File Type | Count | Size |
|-----------|-------|------|
| HBM XML mapping | 1 per entity | ~1.2KB |

## Hibernate Configuration

To use HBM mappings, configure Hibernate:

```xml
<!-- hibernate.cfg.xml -->
<hibernate-configuration>
    <session-factory>
        <mapping resource="com/example/yawl/entity/YWorkItem.hbm.xml"/>
        <mapping resource="com/example/yawl/entity/YTask.hbm.xml"/>
    </session-factory>
</hibernate-configuration>
```

Or in Spring Boot `application.yml`:

```yaml
spring:
  jpa:
    hibernate:
      mapping-locations: classpath:hbm/**/*.hbm.xml
```

## Validation

All generated HBM files:
- ✓ Valid XML syntax
- ✓ Correct DOCTYPE
- ✓ Proper element nesting
- ✓ Required attributes present

## Testing

| Test | Status |
|------|--------|
| XML file creation | PASS |
| DOCTYPE and declaration | PASS |
| `<class>` element structure | PASS |
| `<id>` element with generator | PASS |
| `<property>` elements | PASS |
| Nullable/unique constraints | PASS |

## Use Cases

### Use Case 1: Legacy System Integration

When migrating legacy Hibernate code to Spring Boot:

```xml
<!-- Use existing HBM mappings without code changes -->
<mapping resource="legacy/Order.hbm.xml"/>
```

### Use Case 2: Annotation-Free Environment

Organizations preferring configuration over annotations:

```
src/main/resources/
└── hbm/
    ├── YWorkItem.hbm.xml
    ├── YTask.hbm.xml
    └── YNet.hbm.xml
```

Entities remain POJOs without annotations.

## Determinism

All HBM files are generated deterministically:
- Properties in alphabetical order
- Attributes in consistent order
- Normalized whitespace and line endings
- Content hash for verification

## Related Rules

- **Rule 3**: [JPA Entity](./RULE_3_JPA_ENTITIES.md) - Source entities for HBM mapping
- Alternative to JPA `@Entity` annotations in Rule 3

---

**Version**: 0.1.0 | **Last Updated**: 2026-03-26
