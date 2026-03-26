# Rule 5: Data Transfer Object (DTO) Generation

Generates Lombok-annotated DTOs for REST API contracts, excluding ID fields.

**Rule ID**: 5 | **Input**: JPA entities | **Output**: Java DTO classes | **Pattern**: `Rule<DtoQuery, DtoTemplate>`

## Overview

Rule 5 creates Data Transfer Objects:
- Exclude ID field (no ID in transfer)
- `@Data` - auto getters/setters
- `@Builder` - fluent builder pattern
- `@NoArgsConstructor` / `@AllArgsConstructor` - constructors

## Generated Output

**File Path**: `dto/{EntityName}DTO.java`

```java
package com.example.yawl.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.LocalDateTime;

/**
 * Data Transfer Object for YWorkItem
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class YWorkItemDTO {
    private String status;
    private String taskId;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
```

## Key Differences from Entities

| Aspect | Entity | DTO |
|--------|--------|-----|
| ID field | Included | Excluded |
| Database annotations | `@Entity`, `@Table`, `@Column` | None |
| Persistence | JPA managed | Plain POJO |
| Use case | Database persistence | REST API transfer |
| Lombok | Minimal | `@Data`, `@Builder` |

## Field Exclusions

- **ID field** - Always excluded
- **Database-only fields** - Version, timestamps managed by DB

## Annotations

| Annotation | Purpose |
|-----------|---------|
| `@Data` | Generates getters, setters, equals, hashCode, toString |
| `@Builder` | Fluent builder: `YWorkItemDTO.builder().status("ACTIVE").build()` |
| `@NoArgsConstructor` | Default constructor (required by some frameworks) |
| `@AllArgsConstructor` | Constructor with all fields |

## Usage Example

```java
// Create with builder
YWorkItemDTO dto = YWorkItemDTO.builder()
    .status("ACTIVE")
    .taskId("task-123")
    .createdAt(LocalDateTime.now())
    .build();

// Convert entity to DTO
YWorkItemDTO dto = new YWorkItemDTO(
    entity.getStatus(),
    entity.getTaskId(),
    entity.getCreatedAt(),
    entity.getUpdatedAt()
);
```

## Generated Files

| File Type | Count | Size |
|-----------|-------|------|
| DTO class | 1 per entity | ~0.8KB |

## Testing

| Test | Status |
|------|--------|
| DTO file creation | PASS |
| Lombok annotations (@Data, @Builder) | PASS |
| ID field exclusion | PASS |
| Constructor generation | PASS |

## Related Rules

- **Rule 3**: [JPA Entity](./RULE_3_JPA_ENTITIES.md) - Source for DTOs
- **Rule 6**: [Controller](./RULE_6_CONTROLLERS.md) - Uses DTOs in REST endpoints

---

**Version**: 0.1.0 | **Last Updated**: 2026-03-26
