# Rule 7: Java Enum Generation

Generates type-safe enum classes from ontology enumerated values with value converters.

**Rule ID**: 7 | **Input**: Enumerated value classes from ontology | **Output**: Java enum classes

## Overview

Rule 7 creates Java enums for status values, states, and enumerated types:
- Enum constants (ACTIVE, INACTIVE, PENDING, etc.)
- `getValue()` method - string representation
- `fromValue(String)` converter - string to enum mapping
- Exception handling for invalid values

## Generated Output

**File Path**: `enums/{EnumName}.java`

```java
package com.example.yawl.enums;

/**
 * Work Item Status enumeration
 */
public enum WorkItemStatus {
    PENDING("PENDING"),
    ACTIVE("ACTIVE"),
    COMPLETED("COMPLETED"),
    REJECTED("REJECTED");

    private final String value;

    WorkItemStatus(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public static WorkItemStatus fromValue(String value) {
        for (WorkItemStatus status : WorkItemStatus.values()) {
            if (status.value.equalsIgnoreCase(value)) {
                return status;
            }
        }
        throw new IllegalArgumentException("Unknown value: " + value);
    }
}
```

## Enum Constants

Generated from enumeration values in ontology:

```turtle
yawl:WorkItemStatus a owl:Class ;
    owl:oneOf (
        yawl:StatusPending
        yawl:StatusActive
        yawl:StatusCompleted
        yawl:StatusRejected
    ) .
```

Results in:
```java
PENDING, ACTIVE, COMPLETED, REJECTED
```

## Key Methods

### getValue()

Returns string representation:

```java
WorkItemStatus status = WorkItemStatus.ACTIVE;
String value = status.getValue();  // "ACTIVE"
```

### fromValue(String)

Converts string to enum with exception handling:

```java
// Valid conversion
WorkItemStatus status = WorkItemStatus.fromValue("ACTIVE");
// Returns: WorkItemStatus.ACTIVE

// Invalid conversion
WorkItemStatus status = WorkItemStatus.fromValue("UNKNOWN");
// Throws: IllegalArgumentException("Unknown value: UNKNOWN")
```

## Usage Examples

### In Entity Field

```java
@Entity
public class WorkItem {
    @Enumerated(EnumType.STRING)
    @Column(name = "status")
    private WorkItemStatus status;
}
```

### Serialization

```java
// From DB (string) to enum
WorkItemStatus status = WorkItemStatus.fromValue("ACTIVE");

// From enum to DB (string)
String value = status.getValue();
```

### REST API

```java
@PostMapping
public ResponseEntity<WorkItemDTO> create(@RequestBody WorkItemDTO dto) {
    // JSON: {"status": "ACTIVE"}
    // Deserialized to: WorkItemStatus.ACTIVE
}
```

## Type Safety

Enum pattern prevents invalid statuses:

```java
// Type-safe: compile error for invalid status
WorkItem.setStatus(WorkItemStatus.ACTIVE);  // OK
WorkItem.setStatus("ACTIVE");               // Compile error
```

## Generated Files

| File Type | Count | Size |
|-----------|-------|------|
| Enum class | 1 per enumerated type | ~0.6KB |

## Generated Annotations

None - pure enum implementation for type safety.

## Testing

| Test | Status |
|------|--------|
| Enum class creation | PASS |
| `getValue()` method | PASS |
| `fromValue()` converter | PASS |
| Exception on invalid value | PASS |
| Case-insensitive matching | PASS |

## Related Rules

- **Rule 3**: [JPA Entity](./RULE_3_JPA_ENTITIES.md) - Uses enums for status fields
- **Rule 10**: [Jackson Serializer](./RULE_10_JACKSON.md) - Custom serialization of enums

---

**Version**: 0.1.0 | **Last Updated**: 2026-03-26
