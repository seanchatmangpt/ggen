# Rule 10: Jackson Custom Serializer Generation

Generates Jackson `@JsonSerialize`/`@JsonDeserialize` custom handlers for JSON serialization.

**Rule ID**: 10 | **Input**: Complex types, enums, special formats | **Output**: Java serializer classes

## Overview

Rule 10 creates custom Jackson serializers for:
- Enum value serialization/deserialization
- LocalDateTime ISO8601 formatting
- Complex nested object handling
- Custom bean serializers
- Custom bean deserializers

## Generated Output

**File Path**: `serializer/{TypeName}Serializer.java`

```java
package com.example.yawl.serializer;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.example.yawl.enums.WorkItemStatus;
import java.io.IOException;

/**
 * Custom Jackson serializer for WorkItemStatus enum
 */
public class WorkItemStatusSerializer extends JsonSerializer<WorkItemStatus> {

    @Override
    public void serialize(
            WorkItemStatus value,
            JsonGenerator gen,
            SerializerProvider serializers) throws IOException {
        if (value != null) {
            gen.writeString(value.getValue());
        } else {
            gen.writeNull();
        }
    }
}

/**
 * Custom Jackson deserializer for WorkItemStatus enum
 */
public class WorkItemStatusDeserializer extends JsonDeserializer<WorkItemStatus> {

    @Override
    public WorkItemStatus deserialize(
            JsonParser p,
            DeserializationContext ctxt) throws IOException, JsonProcessingException {
        String value = p.getValueAsString();
        if (value != null) {
            return WorkItemStatus.fromValue(value);
        }
        return null;
    }
}
```

## Usage in Entity

Apply serializers via annotations:

```java
@Entity
public class YWorkItem {

    @JsonSerialize(using = WorkItemStatusSerializer.class)
    @JsonDeserialize(using = WorkItemStatusDeserializer.class)
    @Enumerated(EnumType.STRING)
    private WorkItemStatus status;

    @JsonSerialize(using = LocalDateTimeSerializer.class)
    @JsonDeserialize(using = LocalDateTimeDeserializer.class)
    private LocalDateTime createdAt;
}
```

## Serializer Types

### Enum Serializer

Converts enum to string:

```java
public class EnumSerializer extends JsonSerializer<Enum<?>> {
    public void serialize(Enum<?> value, JsonGenerator gen, SerializerProvider serializers) {
        gen.writeString(value.toString());
    }
}
```

**Usage**: `WorkItemStatus.ACTIVE` → `"ACTIVE"`

### Enum Deserializer

Converts string back to enum:

```java
public class EnumDeserializer extends JsonDeserializer<Enum<?>> {
    public Enum<?> deserialize(JsonParser p, DeserializationContext ctxt) {
        return Enum.valueOf(enumClass, p.getValueAsString());
    }
}
```

**Usage**: `"ACTIVE"` → `WorkItemStatus.ACTIVE`

### LocalDateTime Serializer

ISO8601 format:

```java
public class LocalDateTimeSerializer extends JsonSerializer<LocalDateTime> {
    public void serialize(LocalDateTime value, JsonGenerator gen, SerializerProvider serializers) {
        gen.writeString(value.toString());  // ISO8601
    }
}
```

**Usage**: `2026-03-26T14:30:00` → `"2026-03-26T14:30:00"`

### Bean Serializer (Complex Objects)

For nested objects:

```java
public class WorkItemBeanSerializer extends JsonSerializer<YWorkItem> {
    public void serialize(YWorkItem value, JsonGenerator gen, SerializerProvider serializers) {
        gen.writeStartObject();
        gen.writeStringField("status", value.getStatus().getValue());
        gen.writeStringField("taskId", value.getTaskId());
        gen.writeStringField("createdAt", value.getCreatedAt().toString());
        gen.writeEndObject();
    }
}
```

## Jackson Annotations

| Annotation | Purpose |
|-----------|---------|
| `@JsonSerialize` | Specify custom serializer |
| `@JsonDeserialize` | Specify custom deserializer |
| `@JsonProperty` | Rename JSON property |
| `@JsonIgnore` | Skip field in JSON |

## Serialization Types

### Enum Serialization

```
Java Enum (ACTIVE)
      ↓
Custom Serializer
      ↓
JSON String ("ACTIVE")
```

### LocalDateTime Serialization

```
LocalDateTime (2026-03-26T14:30:00)
      ↓
Custom Serializer
      ↓
ISO8601 String ("2026-03-26T14:30:00")
```

### Bean Serialization

```
Complex Object
      ↓
Custom Serializer (per-field handling)
      ↓
JSON Object {"field1": "value1", "field2": "value2"}
```

## Generated Files

| File Type | Count | Size |
|-----------|-------|------|
| Serializer class | 1-2 per type | ~0.8KB |
| Deserializer class | 1 per type | ~0.8KB |

## Determinism

All serializers are:
- Deterministically generated
- Content-hashed for verification
- Reproducible across runs

## Testing

| Test | Status |
|------|--------|
| Serializer class creation | PASS |
| `serialize()` method | PASS |
| Jackson imports | PASS |
| Custom bean serializers | PASS |
| Null handling | PASS |

## REST API Integration

Serializers automatically used in REST responses:

```bash
# Request (uses deserializer)
POST /api/workitems
{"status": "ACTIVE", "taskId": "task-123"}

# Response (uses serializer)
{
  "id": 1,
  "status": "ACTIVE",
  "taskId": "task-123",
  "createdAt": "2026-03-26T14:30:00"
}
```

## Related Rules

- **Rule 3**: [JPA Entity](./RULE_3_JPA_ENTITIES.md) - Entities annotated with serializers
- **Rule 7**: [Enum](./RULE_7_ENUMS.md) - Enum types needing serializers
- **Rule 5**: [DTO](./RULE_5_DTOS.md) - DTOs using serializers

---

**Version**: 0.1.0 | **Last Updated**: 2026-03-26
