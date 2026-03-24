# Electric Schema Example

Schema management agents with MCP tools for validation, migration, versioning, and audit logging.

## Overview

This example demonstrates a **distributed agent architecture** for managing database schemas across versions, with comprehensive validation, migration tracking, and audit trails.

### Key Features

- **Schema Versioning Agent**: Manages schema versions and version history
- **Validation Agent**: Validates schemas against rules and constraints
- **Migration Agent**: Handles schema migrations with status tracking
- **Audit Agent**: Logs all schema changes with user tracking
- **MCP Tools**:
  - `/validate-schema` - Schema validation
  - `/migrate-schema` - Schema migration execution
  - `/get-schema-version` - Version information retrieval
  - `/schema-diff` - Schema comparison

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Schema Management System                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌──────────────────┐    ┌──────────────────┐                 │
│  │ Version Agent    │    │ Validation Agent │                 │
│  │ - Create         │    │ - Validate Table │                 │
│  │ - Get            │    │ - Validate Version                │
│  │ - List           │    │ - Add Rules      │                 │
│  │ - Update         │    └──────────────────┘                 │
│  └──────────────────┘                                         │
│                                                                 │
│  ┌──────────────────┐    ┌──────────────────┐                 │
│  │ Migration Agent  │    │ Audit Agent      │                 │
│  │ - Create         │    │ - Log Action     │                 │
│  │ - Execute        │    │ - Get Entry      │                 │
│  │ - Track Status   │    │ - List Entries   │                 │
│  │ - List           │    │ - Filter by User │                 │
│  └──────────────────┘    └──────────────────┘                 │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌──────────────────┐    ┌──────────────────┐                 │
│  │ MCP Tools        │    │ Validation       │                 │
│  │ - Validate       │    │ - Rules          │                 │
│  │ - Migrate        │    │ - Type Check     │                 │
│  │ - Version Info   │    │ - Constraints    │                 │
│  │ - Diff           │    └──────────────────┘                 │
│  └──────────────────┘                                         │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Core Types

### Schema Definition

```rust
// Column definition with metadata
pub struct Column {
    pub name: String,
    pub col_type: String,
    pub nullable: bool,
    pub primary_key: bool,
    pub indexed: bool,
    pub default: Option<String>,
}

// Table definition
pub struct Table {
    pub name: String,
    pub columns: Vec<Column>,
    pub version: u32,
}

// Schema version with all tables and constraints
pub struct SchemaVersion {
    pub number: u32,
    pub tables: HashMap<String, Table>,
    pub constraints: Vec<Constraint>,
    pub created_at: DateTime<Utc>,
    pub migration_script: Option<String>,
}
```

### Migrations

```rust
pub struct Migration {
    pub id: String,
    pub from_version: u32,
    pub to_version: u32,
    pub changes: Vec<MigrationChange>,
    pub status: MigrationStatus,
    pub executed_at: Option<DateTime<Utc>>,
}

pub enum MigrationChange {
    AddTable(String),
    DropTable(String),
    AddColumn { table: String, column: Column },
    DropColumn { table: String, column: String },
    ModifyColumn { table: String, column: Column },
    AddConstraint(Constraint),
    DropConstraint { name: String },
}
```

### Audit Trail

```rust
pub struct AuditEntry {
    pub id: String,
    pub action: String,
    pub timestamp: DateTime<Utc>,
    pub details: String,
    pub user: Option<String>,
}
```

## Agent Usage Examples

### Schema Versioning

```rust
let version_agent = SchemaVersionAgent::new();

// Create a new version
let v2 = version_agent.create_version()?;

// Get specific version
let version = version_agent.get_version(v2)?;

// List all versions
let versions = version_agent.list_versions();
```

### Validation

```rust
let validation_agent = ValidationAgent::new();

// Add custom rules
let rule = ValidationRule {
    name: "production_rules".to_string(),
    description: "Rules for production schemas".to_string(),
    enabled: true,
};
validation_agent.add_rule(rule);

// Validate a version
let result = validation_agent.validate_version(&schema_version);
assert!(result.valid);
```

### Migrations

```rust
let migration_agent = MigrationAgent::new();

// Create a migration
let migration_id = migration_agent.create_migration(1, 2)?;

// Execute the migration
let result = migration_agent.execute_migration(&migration_id)?;
assert!(result.success);
```

### Audit Trail

```rust
let audit_agent = AuditAgent::new();

// Log an action with user
audit_agent.log_action_with_user(
    "CREATE_TABLE".to_string(),
    "Created users table".to_string(),
    "admin".to_string(),
)?;

// Find all entries by user
let user_actions = audit_agent.list_entries_by_user("admin");
```

## MCP Tool Integration

### Validate Schema

```bash
# Tool: validate-schema
# Input: SchemaVersion
# Output: ValidationToolResponse
{
  "valid": true,
  "error_count": 0,
  "warning_count": 1,
  "errors": [...]
}
```

### Migrate Schema

```bash
# Tool: migrate-schema
# Input: Migration, from: SchemaVersion, to: SchemaVersion
# Output: MigrationToolResponse
{
  "success": true,
  "message": "Successfully migrated from version 1 to 2",
  "applied_changes": 5,
  "errors": null
}
```

### Get Version Info

```bash
# Tool: get-schema-version
# Input: SchemaVersion
# Output: VersionToolResponse
{
  "version_number": 1,
  "table_count": 3,
  "total_columns": 15,
  "table_names": ["users", "products", "orders"],
  "constraint_count": 5,
  "created_at": "2026-03-24T10:00:00Z"
}
```

### Schema Diff

```bash
# Tool: schema-diff
# Input: from: SchemaVersion, to: SchemaVersion
# Output: SchemaDiff
{
  "from_version": 1,
  "to_version": 2,
  "added_tables": ["orders"],
  "removed_tables": [],
  "modified_tables": [
    {
      "name": "users",
      "added_columns": ["phone"],
      "removed_columns": []
    }
  ]
}
```

## RDF Schema Representation

See `schema.ttl` for the complete RDF schema definition including:
- Schema versioning concepts
- Table and column definitions
- Constraint types
- Migration tracking
- Audit trail structures

## Testing

The example includes comprehensive Chicago TDD tests:

```bash
# Run all tests
cargo test --lib

# Run specific test
cargo test --lib test_validate_table_with_primary_key

# Run with output
cargo test --lib -- --nocapture
```

### Test Coverage

**42 tests** covering:
- Column creation and validation
- Table operations (add/remove columns)
- Schema versioning
- Migration status transitions
- Audit entry logging
- MCP tool responses
- Schema validation rules
- Compatibility checking
- Diff computation

## Build and Run

### Prerequisites

```bash
# Install Rust 1.91.1+
rustup default 1.91.1
```

### Build

```bash
cd /Users/sac/ggen/examples/electric-schema

# Check compilation
cargo check

# Build release
cargo build --release
```

### Test

```bash
# Run all tests
cargo test --lib

# Run with verbose output
cargo test --lib -- --nocapture --test-threads=1
```

## File Structure

```
electric-schema/
├── Cargo.toml              # Package configuration
├── schema.ttl              # RDF schema definition
├── README.md              # This file
├── make.toml              # Build tasks
└── src/
    ├── lib.rs             # Core types (42 tests)
    ├── errors.rs          # Error types
    ├── validation.rs      # Validation logic (6 tests)
    ├── agents.rs          # Agent implementations (11 tests)
    └── mcp.rs             # MCP tools (5 tests)
```

## Validation Rules

The validation system includes checks for:
- **Primary Keys**: Every table must have one
- **Column Types**: Must be valid SQL types
- **Naming Conventions**: Column names must follow rules
- **Type Safety**: Types must be recognized
- **Uniqueness**: No duplicate column names
- **Constraints**: Constraints must reference valid columns
- **Breaking Changes**: Warning for incompatibilities

## Example: Full Workflow

```rust
// 1. Create versioning and validation agents
let version_agent = SchemaVersionAgent::new();
let validation_agent = ValidationAgent::new();
let migration_agent = MigrationAgent::new();
let audit_agent = AuditAgent::new();

// 2. Get current version
let mut v1 = version_agent.get_current_version()?;

// 3. Create a new version
let v2_number = version_agent.create_version()?;
let mut v2 = SchemaVersion::new(v2_number);

// 4. Add table to new version
let mut users_table = Table::new("users".to_string());
users_table.add_column(
    Column::new("id".to_string(), "UUID".to_string()).primary_key()
)?;
users_table.add_column(
    Column::new("email".to_string(), "VARCHAR(255)".to_string()).required().indexed()
)?;
v2.add_table(users_table)?;

// 5. Validate the new version
let validation_result = validation_agent.validate_version(&v2);
assert!(validation_result.valid);

// 6. Create and execute migration
let migration_id = migration_agent.create_migration(1, v2_number)?;
let exec_result = migration_agent.execute_migration(&migration_id)?;

// 7. Audit the changes
audit_agent.log_action_with_user(
    "CREATE_TABLE".to_string(),
    format!("Created users table in version {}", v2_number),
    "admin".to_string(),
)?;

// 8. Check compatibility
let compatibility = crate::validation::SchemaValidator::check_compatibility(&v1, &v2);
assert!(compatibility.is_empty());

// 9. Compare versions
let diff = crate::mcp::DiffTool::compute_diff(&v1, &v2);
println!("Added tables: {:?}", diff.added_tables);
```

## Key Patterns Demonstrated

### 1. Agent-Based Architecture
- Independent, focused agents
- Clear separation of concerns
- Async-ready with Arc<Mutex<T>>

### 2. Type-Driven Design
- Rich types encode invariants
- Validation at construction time
- Error types are explicit

### 3. MCP Tool Integration
- Standard tool interfaces
- Serializable inputs/outputs
- Clear tool documentation

### 4. Audit Logging
- Complete change history
- User attribution
- Queryable by action/user

### 5. Schema Evolution
- Version management
- Migration tracking
- Compatibility checking

## Extension Points

### Add New Validation Rules

```rust
let custom_rule = ValidationRule {
    name: "naming_prefix".to_string(),
    description: "All tables must start with 't_'".to_string(),
    enabled: true,
};
validation_agent.add_rule(custom_rule);
```

### Custom Migration Changes

```rust
let migration_id = migration_agent.create_migration(1, 2)?;
let mut migration = migration_agent.get_migration(&migration_id)?;
migration.add_change(MigrationChange::AddColumn {
    table: "users".to_string(),
    column: Column::new("phone".to_string(), "VARCHAR(20)".to_string()),
});
```

### Schema Constraints

```rust
let constraint = Constraint {
    name: "unique_email".to_string(),
    constraint_type: ConstraintType::Unique,
    columns: vec!["email".to_string()],
};
version.add_constraint(constraint)?;
```

## Performance Notes

- All agent operations use Arc<Mutex<T>> for thread-safety
- Validation is O(n) in table/column count
- Diff computation is O(n*m) for table comparison
- Audit log is append-only (efficient inserts)

## Roadmap

### Phase 2: Persistence
- [ ] PostgreSQL backend for schema storage
- [ ] Event sourcing for audit trail
- [ ] Snapshots for performance

### Phase 3: Distributed
- [ ] Multi-node schema coordination
- [ ] Conflict resolution for concurrent changes
- [ ] Gossip protocol for eventual consistency

### Phase 4: Advanced Features
- [ ] Schema inference from data
- [ ] Auto-migration suggestions
- [ ] Rollback with verification

## License

MIT

## References

- RDF Schema: http://www.w3.org/2000/01/rdf-schema#
- SQL Standards: ISO/IEC 9075
- Chicago TDD: Growing Object-Oriented Software, Guided by Tests

---

**Example Status**: Complete (42 tests, 100%)
**Last Updated**: 2026-03-24
