# Code Organization

## Overview

KNKHS follows a modular architecture with clear separation of concerns. Large files have been split into focused, maintainable components following core team best practices.

## Directory Structure

```
knhk/
├── include/              # Public API headers
│   ├── knhk.h          # Main umbrella header
│   └── knhk/           # Modular API components
│       ├── types.h      # Type definitions
│       ├── eval.h       # Query evaluation
│       ├── receipts.h   # Receipt operations
│       └── utils.h      # Utilities
├── src/                 # Implementation
│   ├── simd.h           # SIMD umbrella header
│   ├── simd/            # SIMD operation modules
│   ├── core.c           # Core operations
│   ├── rdf.c            # RDF parsing
│   └── clock.c          # Span ID generation (no timing dependencies)
├── tests/               # Test suites
│   ├── chicago_enterprise_use_cases.c    # Enterprise test runner
│   ├── chicago_basic_operations.c        # Basic operations
│   ├── chicago_cardinality.c             # Cardinality tests
│   ├── chicago_object_operations.c       # Object operations
│   ├── chicago_advanced.c                # Advanced operations
│   ├── chicago_v1_test.c                 # v1.0 test runner
│   ├── chicago_v1_receipts.c            # Receipt tests
│   ├── chicago_v1_operations.c          # Operation tests
│   ├── chicago_v1_validation.c          # Validation tests
│   ├── chicago_integration_v2.c         # Integration test runner
│   ├── chicago_integration_core.c       # Core integration
│   ├── chicago_integration_systems.c    # System integration
│   ├── chicago_integration_advanced.c    # Advanced integration
│   └── chicago_test_helpers.c           # Shared test infrastructure
└── docs/                # Documentation
```

## Header Organization

### Main API (`include/knhk.h`)

The main API header is a lightweight umbrella (16 lines) that includes all components:

```c
#include "knhk/types.h"      // Constants, enums, structs
#include "knhk/utils.h"      // Context initialization, RDF loading, clock utilities
#include "knhk/receipts.h"   // Receipt operations
#include "knhk/eval.h"       // Query evaluation functions
```

### Type Definitions (`include/knhk/types.h`)

Contains all type definitions:
- Constants (`KNHK_TICK_BUDGET`, `KNHK_NROWS`, `KNHK_ALIGN`)
- Operation enum (`knhk_op_t`)
- Struct definitions (`knhk_context_t`, `knhk_hook_ir_t`, `knhk_receipt_t`, `knhk_pred_run_t`)

### Evaluation Functions (`include/knhk/eval.h`)

Contains query evaluation functions:
- `knhk_eval_bool()` - Boolean query evaluation
- `knhk_eval_construct8()` - CONSTRUCT8 operations
- All inline for hot path performance

### Receipt Operations (`include/knhk/receipts.h`)

Contains receipt utilities:
- `knhk_receipt_merge()` - Combine receipts via ⊕

### Utilities (`include/knhk/utils.h`)

Contains utility functions:
- `knhk_init_ctx()` - Context initialization
- `knhk_pin_run()` - Set predicate run
- `knhk_load_rdf()` - RDF file loading
- Clock utilities (`knhk_generate_span_id()`)

## SIMD Organization

### Umbrella Header (`src/simd.h`)

Lightweight umbrella header (24 lines) that includes all SIMD modules:

```c
#include "simd/common.h"      // Common infrastructure
#include "simd/existence.h"   // ASK operations
#include "simd/count.h"       // COUNT operations
#include "simd/compare.h"     // Comparison operations
#include "simd/select.h"      // SELECT operations
#include "simd/validate.h"    // Datatype validation
#include "simd/construct.h"   // CONSTRUCT8 operations
```

### SIMD Modules (`src/simd/`)

Each module contains focused SIMD operations:
- **`common.h`**: Common infrastructure, includes, declarations
- **`existence.h`**: ASK operations (`exists_8`, `exists_o_8`, `spo_exists_8`)
- **`count.h`**: COUNT operations (`count_8`)
- **`compare.h`**: Comparison operations (`compare_o_8`)
- **`select.h`**: SELECT operations (`select_gather_8`)
- **`validate.h`**: Datatype validation (`validate_datatype_sp_8`)
- **`construct.h`**: CONSTRUCT8 operations (`construct8_emit_8`)

All SIMD functions are `static inline` and header-only for NROWS==8.

## Test Organization

### Enterprise Tests

**Main Runner**: `tests/chicago_enterprise_use_cases.c` (49 lines)
- Orchestrates all enterprise test suites

**Test Suites**:
- `chicago_basic_operations.c` - Tests 1, 2, 5 (Authorization, Property Existence, Simple Lookups)
- `chicago_cardinality.c` - Tests 3, 6, 7, 9 (Cardinality Constraints, MaxCount, ExactCount, Uniqueness)
- `chicago_object_operations.c` - Tests 8, 10, 11, 12 (Reverse Lookup, Object Count operations)
- `chicago_advanced.c` - Tests 4, 13-19 (Type Checking, SELECT, COMPARE, VALIDATE)

**Shared Infrastructure**:
- `chicago_test_helpers.c` - Shared context, performance measurement, direct SIMD callers
- `chicago_test_helpers.h` - Test helper declarations

### v1.0 Tests

**Main Runner**: `tests/chicago_v1_test.c` (44 lines)
- Orchestrates all v1.0 test suites

**Test Suites**:
- `chicago_v1_receipts.c` - Receipt generation and merging
- `chicago_v1_operations.c` - CONSTRUCT8, batch execution, all operations
- `chicago_v1_validation.c` - Guard enforcement, constants, timing validation

### Integration Tests

**Main Runner**: `tests/chicago_integration_v2.c` (49 lines)
- Orchestrates all integration test suites

**Test Suites**:
- `chicago_integration_core.c` - Core integration (end-to-end, multi-connector)
- `chicago_integration_systems.c` - System integrations (lockchain, OTEL)
- `chicago_integration_advanced.c` - Advanced integration (pipeline complete, guards, provenance)

## Build System

The Makefile is configured to build all split test files:

```makefile
$(TEST_ENTERPRISE): tests/chicago_enterprise_use_cases.c tests/chicago_basic_operations.c tests/chicago_cardinality.c tests/chicago_object_operations.c tests/chicago_advanced.c tests/chicago_test_helpers.c $(LIB)
	$(CC) ... -o $@ ...

$(TEST_V1): tests/chicago_v1_test.c tests/chicago_v1_receipts.c tests/chicago_v1_operations.c tests/chicago_v1_validation.c $(LIB)
	$(CC) ... -o $@ ...

$(TEST_INTEGRATION_V2): tests/chicago_integration_v2.c tests/chicago_integration_core.c tests/chicago_integration_systems.c tests/chicago_integration_advanced.c $(LIB)
	$(CC) ... -o $@ ...
```

## Benefits of Modular Organization

1. **Maintainability**: Clear separation of concerns, easy to locate code
2. **Testability**: Focused test suites for each component
3. **Performance**: Hot path isolated in inline headers
4. **Extensibility**: Easy to add new operations or test suites
5. **Build Times**: Smaller compilation units improve incremental builds
6. **Code Review**: Smaller files are easier to review and understand
7. **Documentation**: Clear structure makes documentation easier to maintain

## File Size Guidelines

- **Headers**: Target <100 lines per module (excluding inline implementations)
- **Test Files**: Target <300 lines per test suite
- **Umbrella Headers**: Keep minimal (<50 lines)

Current status:
- ✅ `include/knhk.h`: 16 lines (umbrella)
- ✅ `include/knhk/types.h`: 90 lines
- ✅ `include/knhk/eval.h`: 305 lines (inline implementations)
- ✅ `include/knhk/receipts.h`: 21 lines
- ✅ `include/knhk/utils.h`: 40 lines
- ✅ `src/simd.h`: 24 lines (umbrella)
- ✅ `tests/chicago_enterprise_use_cases.c`: 49 lines (runner)
- ✅ `tests/chicago_v1_test.c`: 44 lines (runner)
- ✅ `tests/chicago_integration_v2.c`: 49 lines (runner)

## Adding New Features

### Adding a New Operation

1. Add operation enum to `include/knhk/types.h`
2. Add SIMD implementation to appropriate `src/simd/*.h` file
3. Add dispatch case to `include/knhk/eval.h` (`knhk_eval_bool`)
4. Add test to appropriate `tests/chicago_*.c` file
5. Update Makefile if new test file is created

### Adding a New Test Suite

1. Create new test file `tests/chicago_new_suite.c`
2. Include `chicago_test_helpers.h` for shared infrastructure
3. Export test function: `int chicago_test_new_suite(void)`
4. Update main test runner to call new suite
5. Update Makefile to include new test file in build

## Code Style

- **Naming**: `knhk_` prefix for all public functions
- **Inline**: Use `static inline` for hot path functions
- **Headers**: Include guards, minimal includes
- **Tests**: Chicago TDD style - real collaborators, state-based tests
- **Documentation**: Comments for public APIs, inline docs for complex logic

