# Rule 8: Spring Service Layer Generation

Generates Spring service classes with transaction management and repository injection.

**Rule ID**: 8 | **Input**: JPA entities and repositories | **Output**: Java service classes

## Overview

Rule 8 creates Spring service layer components:
- `@Service` stereotype annotation
- `@Transactional` for transaction management
- Repository injection via `@Autowired`
- CRUD business operations
- Error handling and validation

## Generated Output

**File Path**: `service/{EntityName}Service.java`

```java
package com.example.yawl.service;

import com.example.yawl.dto.YWorkItemDTO;
import com.example.yawl.entity.YWorkItem;
import com.example.yawl.repository.YWorkItemRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Service layer for YWorkItem entity
 */
@Service
public class YWorkItemService {

    @Autowired
    private YWorkItemRepository yWorkItemRepository;

    @Transactional(readOnly = true)
    public List<YWorkItemDTO> findAll() {
        return yWorkItemRepository.findAll()
            .stream()
            .map(this::convertToDTO)
            .collect(Collectors.toList());
    }

    @Transactional(readOnly = true)
    public Optional<YWorkItemDTO> findById(Long id) {
        return yWorkItemRepository.findById(id)
            .map(this::convertToDTO);
    }

    @Transactional
    public YWorkItemDTO create(YWorkItemDTO dto) {
        YWorkItem entity = convertToEntity(dto);
        YWorkItem saved = yWorkItemRepository.save(entity);
        return convertToDTO(saved);
    }

    @Transactional
    public YWorkItemDTO update(Long id, YWorkItemDTO dto) {
        Optional<YWorkItem> entity = yWorkItemRepository.findById(id);
        if (entity.isPresent()) {
            YWorkItem toUpdate = entity.get();
            // Update fields
            toUpdate.setStatus(dto.getStatus());
            toUpdate.setTaskId(dto.getTaskId());
            YWorkItem saved = yWorkItemRepository.save(toUpdate);
            return convertToDTO(saved);
        }
        throw new IllegalArgumentException("Not found: " + id);
    }

    @Transactional
    public void delete(Long id) {
        yWorkItemRepository.deleteById(id);
    }

    // Conversion methods
    private YWorkItemDTO convertToDTO(YWorkItem entity) {
        return YWorkItemDTO.builder()
            .status(entity.getStatus())
            .taskId(entity.getTaskId())
            .createdAt(entity.getCreatedAt())
            .updatedAt(entity.getUpdatedAt())
            .build();
    }

    private YWorkItem convertToEntity(YWorkItemDTO dto) {
        YWorkItem entity = new YWorkItem();
        entity.setStatus(dto.getStatus());
        entity.setTaskId(dto.getTaskId());
        return entity;
    }
}
```

## Annotations

| Annotation | Purpose | Example |
|-----------|---------|---------|
| `@Service` | Spring component (business logic) | `@Service` |
| `@Transactional` | Transaction management | `@Transactional` |
| `@Transactional(readOnly=true)` | Read-only transaction | `@Transactional(readOnly=true)` |
| `@Autowired` | Dependency injection | `@Autowired Repository repo` |

## Generated Methods

### Query Methods (Read-Only)

```java
@Transactional(readOnly = true)
public List<YWorkItemDTO> findAll() { ... }

@Transactional(readOnly = true)
public Optional<YWorkItemDTO> findById(Long id) { ... }
```

### Mutation Methods (Transactional)

```java
@Transactional
public YWorkItemDTO create(YWorkItemDTO dto) { ... }

@Transactional
public YWorkItemDTO update(Long id, YWorkItemDTO dto) { ... }

@Transactional
public void delete(Long id) { ... }
```

## Conversion Methods

Automatically generated for Entity ↔ DTO conversion:

```java
// Entity to DTO
private YWorkItemDTO convertToDTO(YWorkItem entity) {
    return YWorkItemDTO.builder()
        .status(entity.getStatus())
        .taskId(entity.getTaskId())
        .build();
}

// DTO to Entity
private YWorkItem convertToEntity(YWorkItemDTO dto) {
    YWorkItem entity = new YWorkItem();
    entity.setStatus(dto.getStatus());
    entity.setTaskId(dto.getTaskId());
    return entity;
}
```

## Repository Injection

Service injects repository via `@Autowired`:

```java
@Autowired
private YWorkItemRepository repository;

// Usage in methods
repository.findAll();
repository.save(entity);
repository.deleteById(id);
```

## Generated Files

| File Type | Count | Size |
|-----------|-------|------|
| Service class | 1 per entity | ~2.5KB |

## Transaction Scope

| Method Type | Scope | `readOnly` | Behavior |
|-----------|-------|-----------|----------|
| Find/Get | Read | `true` | Optimized for reads, no flush |
| Create/Update/Delete | Write | `false` | Can modify data |

## Testing

| Test | Status |
|------|--------|
| Service class creation | PASS |
| `@Service` annotation | PASS |
| `@Transactional` methods | PASS |
| Repository injection | PASS |
| CRUD operations | PASS |

## Related Rules

- **Rule 3**: [JPA Entity](./RULE_3_JPA_ENTITIES.md) - Entities managed by service
- **Rule 4**: [Repository](./RULE_4_REPOSITORIES.md) - Data access injected by service
- **Rule 5**: [DTO](./RULE_5_DTOS.md) - Input/output types for service
- **Rule 6**: [Controller](./RULE_6_CONTROLLERS.md) - Uses service for business logic

---

**Version**: 0.1.0 | **Last Updated**: 2026-03-26
