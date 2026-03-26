# Rule 6: Spring REST Controller Generation

Generates Spring Web REST controller classes with HTTP endpoint mappings.

**Rule ID**: 6 | **Input**: JPA entities and DTOs | **Output**: Java REST controller classes

## Overview

Rule 6 creates Spring REST controllers:
- `@RestController` - JSON response mapping
- `@RequestMapping("/api/resource")` - Base endpoint path
- CRUD endpoints: GET, POST, PUT, DELETE
- `@Autowired` dependency injection of services
- HTTP method routing

## Generated Output

**File Path**: `controller/{EntityName}Controller.java`

```java
package com.example.yawl.controller;

import com.example.yawl.dto.YWorkItemDTO;
import com.example.yawl.service.YWorkItemService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import java.util.List;

/**
 * REST Controller for YWorkItem resource
 */
@RestController
@RequestMapping("/api/workitems")
public class YWorkItemController {

    @Autowired
    private YWorkItemService yWorkItemService;

    @GetMapping
    public ResponseEntity<List<YWorkItemDTO>> getAllWorkItems() {
        return ResponseEntity.ok(yWorkItemService.findAll());
    }

    @GetMapping("/{id}")
    public ResponseEntity<YWorkItemDTO> getWorkItemById(@PathVariable Long id) {
        return yWorkItemService.findById(id)
            .map(ResponseEntity::ok)
            .orElse(ResponseEntity.notFound().build());
    }

    @PostMapping
    public ResponseEntity<YWorkItemDTO> createWorkItem(@RequestBody YWorkItemDTO dto) {
        YWorkItemDTO created = yWorkItemService.create(dto);
        return ResponseEntity.ok(created);
    }

    @PutMapping("/{id}")
    public ResponseEntity<YWorkItemDTO> updateWorkItem(
            @PathVariable Long id,
            @RequestBody YWorkItemDTO dto) {
        YWorkItemDTO updated = yWorkItemService.update(id, dto);
        return ResponseEntity.ok(updated);
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> deleteWorkItem(@PathVariable Long id) {
        yWorkItemService.delete(id);
        return ResponseEntity.noContent().build();
    }
}
```

## Generated Endpoints

| Method | Endpoint | Operation | HTTP Code |
|--------|----------|-----------|-----------|
| GET | `/api/{resource}` | Get all | 200 |
| GET | `/api/{resource}/{id}` | Get by ID | 200 / 404 |
| POST | `/api/{resource}` | Create | 201 / 400 |
| PUT | `/api/{resource}/{id}` | Update | 200 / 404 |
| DELETE | `/api/{resource}/{id}` | Delete | 204 / 404 |

## Annotations

| Annotation | Purpose | Example |
|-----------|---------|---------|
| `@RestController` | Marks class as REST endpoint | `@RestController` |
| `@RequestMapping` | Base path for all endpoints | `@RequestMapping("/api/users")` |
| `@GetMapping` | GET request handler | `@GetMapping` |
| `@PostMapping` | POST request handler | `@PostMapping` |
| `@PutMapping` | PUT request handler | `@PutMapping("/{id}")` |
| `@DeleteMapping` | DELETE request handler | `@DeleteMapping("/{id}")` |
| `@PathVariable` | Extract URL parameter | `@PathVariable Long id` |
| `@RequestBody` | Parse request body | `@RequestBody DTO dto` |
| `@Autowired` | Inject dependency | `@Autowired Service service` |

## Service Injection

Controllers inject services via `@Autowired`:

```java
@Autowired
private YWorkItemService yWorkItemService;
```

Methods delegate to service layer:

```java
public ResponseEntity<List<YWorkItemDTO>> getAll() {
    return ResponseEntity.ok(yWorkItemService.findAll());
}
```

## Path Generation

Controller path derived from entity name:

| Entity | Base Path | Full Path |
|--------|-----------|-----------|
| `YWorkItem` | `/workitems` | `/api/workitems` |
| `LoanApplication` | `/applications` | `/api/applications` |
| `Order` | `/orders` | `/api/orders` |

## Generated Files

| File Type | Count | Size |
|-----------|-------|------|
| Controller class | 1 per entity | ~2KB |

## Testing

| Test | Status |
|------|--------|
| Controller creation | PASS |
| `@RestController` and `@RequestMapping` annotations | PASS |
| Request mapping path (/api/resource) | PASS |
| Service injection via `@Autowired` | PASS |
| HTTP method routing | PASS |

## Related Rules

- **Rule 5**: [DTO](./RULE_5_DTOS.md) - Request/response types
- **Rule 8**: [Service](./RULE_8_SERVICES.md) - Business logic injected by controller

---

**Version**: 0.1.0 | **Last Updated**: 2026-03-26
