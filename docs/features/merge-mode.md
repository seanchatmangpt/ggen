# Merge Mode Feature

## Overview

Merge mode enables **hybrid development** where ggen-generated code and manual code coexist in the same files. Instead of the all-or-nothing behavior of `--force`, merge mode uses **git-style conflict markers** to preserve manual code while updating generated sections.

## Problem Statement

Traditional code generation forces a choice:

1. **Never overwrite** (default): Safe but blocks ontology-driven refactoring
2. **Always overwrite** (`--force`): Enables refactoring but destroys manual code

Merge mode solves this by:
- Detecting manual code regions via conflict markers
- Regenerating only the "generated" sections
- Preserving "manual" sections across sync operations
- Flagging conflicts when manual and generated regions overlap

## Conflict Marker Syntax

```rust
// <<<<<<< GENERATED (ggen-v5, rule: structs, id: uuid-1234)
pub struct User {
    pub id: Uuid,
    pub email: String,
}
// ======= MANUAL
// Custom validation logic (preserved by ggen)
impl User {
    pub fn is_valid_email(&self) -> bool {
        self.email.contains('@')
    }
}
// >>>>>>> END
```

### Marker Sections

1. **GENERATED section** (`<<<<<<<` to `=======`)
   - Managed by ggen
   - Overwritten on every sync
   - Metadata: ggen version, rule name, unique ID

2. **MANUAL section** (`=======` to `>>>>>>>`)
   - Managed by developer
   - Preserved across all syncs
   - ggen never modifies this region

## Usage

### Enabling Merge Mode

Merge mode is enabled via `ggen.toml` per generation rule:

```toml
[[generation_rule]]
name = "structs"
query = "query/structs.rq"
template = "templates/rust_struct.tera"
output_pattern = "src/models/{{ struct_name | snake_case }}.rs"
merge_mode = true  # Enable conflict markers
```

### Template Changes

Templates must include merge markers:

```rust
{# templates/rust_struct.tera #}
// <<<<<<< GENERATED (ggen-v5, rule: {{ rule_name }}, id: {{ generation_id }})
pub struct {{ struct_name }} {
  {% for field in fields %}
    pub {{ field.name }}: {{ field.type }},
  {% endfor %}
}
// ======= MANUAL
// Your custom code here (preserved by ggen)
// >>>>>>> END
```

### Initial Generation

First sync creates file with markers:

```bash
$ ggen sync --rule structs
Created: src/models/user.rs

$ cat src/models/user.rs
// <<<<<<< GENERATED (ggen-v5, rule: structs, id: abc-123)
pub struct User {
    pub id: Uuid,
    pub email: String,
}
// ======= MANUAL
// >>>>>>> END
```

### Adding Manual Code

Developer edits MANUAL section:

```rust
// <<<<<<< GENERATED (ggen-v5, rule: structs, id: abc-123)
pub struct User {
    pub id: Uuid,
    pub email: String,
}
// ======= MANUAL
impl User {
    pub fn validate(&self) -> Result<(), Error> {
        if !self.email.contains('@') {
            return Err(Error::InvalidEmail);
        }
        Ok(())
    }
}
// >>>>>>> END
```

### Subsequent Syncs

Ontology changes (add `username` field):

```bash
$ ggen sync --rule structs
Updated: src/models/user.rs

$ cat src/models/user.rs
// <<<<<<< GENERATED (ggen-v5, rule: structs, id: abc-123)
pub struct User {
    pub id: Uuid,
    pub email: String,
    pub username: String,  // <-- NEW FIELD
}
// ======= MANUAL
impl User {
    pub fn validate(&self) -> Result<(), Error> {
        if !self.email.contains('@') {
            return Err(Error::InvalidEmail);
        }
        Ok(())  // <-- PRESERVED
    }
}
// >>>>>>> END
```

**Result**: Generated section updated, manual section untouched.

## Conflict Detection

### Scenario 1: Manual Code in Generated Section

Developer accidentally edits generated section:

```rust
// <<<<<<< GENERATED (ggen-v5, rule: structs, id: abc-123)
pub struct User {
    pub id: Uuid,
    pub email: String,
    // FIXME: This should be optional  <-- MANUAL COMMENT IN GENERATED SECTION
}
// ======= MANUAL
// >>>>>>> END
```

Next sync:

```bash
$ ggen sync --rule structs
WARNING: Manual changes detected in GENERATED section: src/models/user.rs
  Line 5: "// FIXME: This should be optional"

Choose action:
  1. Discard manual changes (regenerate)
  2. Keep manual changes (skip file)
  3. Create conflict markers (three-way merge)
> 3

$ cat src/models/user.rs
// <<<<<<< GENERATED (ggen-v5, rule: structs, id: abc-123)
pub struct User {
    pub id: Uuid,
    pub email: String,
}
// ||||||| MANUAL CHANGES
pub struct User {
    pub id: Uuid,
    pub email: String,
    // FIXME: This should be optional
}
// ======= MANUAL
// >>>>>>> END
```

Developer must manually resolve conflict (like git merge conflict).

### Scenario 2: Overlapping Edits

Ontology changes field type, developer has validation logic:

**Ontology**: `email: String` → `email: EmailAddress`

**Manual code**: Validates `email: String`

```bash
$ ggen sync --rule structs
CONFLICT: src/models/user.rs
  Generated section changed field type: email: String -> email: EmailAddress
  Manual section references old type: String

Recommended action:
  1. Review manual section for type compatibility
  2. Update manual code if needed
  3. Re-run sync
```

## Workflow Examples

### Example 1: Iterative Development

```bash
# Day 1: Initial generation
ggen sync --rule models
# Files: src/models/*.rs (with conflict markers)

# Developer adds business logic
vim src/models/user.rs
# Edit MANUAL section, add validation methods

# Day 2: Ontology evolves (add fields)
vim ontology/domain.ttl
ggen sync --rule models
# Generated sections updated, manual sections preserved

# Day 3: Major refactoring (rename fields)
vim ontology/domain.ttl
ggen sync --rule models
# WARNING: Field name changes detected, manual code may be affected
# Developer reviews and updates MANUAL sections
```

### Example 2: Team Collaboration

```bash
# Developer A: Adds field to ontology
git checkout -b add-username-field
vim ontology/domain.ttl  # Add username field
ggen sync --rule models
git add -A && git commit -m "Add username field to User"
git push origin add-username-field

# Developer B: Adds validation to manual section
git checkout -b add-email-validation
vim src/models/user.rs  # Edit MANUAL section
git add -A && git commit -m "Add email validation"
git push origin add-email-validation

# Merge both branches
git checkout main
git merge add-username-field   # Regenerates GENERATED section
git merge add-email-validation # Preserves MANUAL section
# NO CONFLICT: Changes in different marker regions
```

### Example 3: CI/CD Verification

```yaml
# .github/workflows/verify-merge-markers.yml
name: Verify Merge Markers

on: [pull_request]

jobs:
  verify:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Check for unresolved conflicts
        run: |
          # Fail if git-style conflict markers exist
          if grep -r "^<<<<<<< HEAD" src/; then
            echo "ERROR: Unresolved merge conflicts detected"
            exit 1
          fi

      - name: Regenerate code
        run: ggen sync --force --audit

      - name: Verify manual sections unchanged
        run: |
          # Extract MANUAL sections from generated files
          ./scripts/extract_manual_sections.sh src/models/*.rs > manual_before.txt
          ggen sync --force
          ./scripts/extract_manual_sections.sh src/models/*.rs > manual_after.txt

          # Fail if manual sections changed
          if ! diff manual_before.txt manual_after.txt; then
            echo "ERROR: Manual sections modified by ggen sync"
            exit 1
          fi
```

## Best Practices

1. **Use merge mode for hybrid projects**
   - Domain models: Generated
   - Business logic: Manual
   - Validation: Manual
   - Serialization: Generated

2. **Never edit generated sections**
   - Changes will be overwritten
   - Fix in ontology, then regenerate

3. **Keep manual sections focused**
   - Minimize coupling to generated code
   - Use traits/interfaces for abstraction

4. **Document marker regions**
   ```rust
   // ======= MANUAL
   // Business logic for user validation
   // Dependencies: Generated User struct
   // Author: @username
   // Last modified: 2025-12-20
   impl User {
       pub fn validate(&self) -> Result<(), Error> { ... }
   }
   // >>>>>>> END
   ```

5. **Review conflicts carefully**
   - Type changes in generated section may break manual code
   - Field renames require manual section updates

6. **Use CI to enforce marker integrity**
   - Verify all files have valid markers
   - Detect accidental edits in generated sections

## Advanced Features

### Custom Marker Syntax

Configure per-language marker style in `ggen.toml`:

```toml
[merge_mode]
marker_style = "rust"  # Uses // for comments

[merge_mode.markers.rust]
generated_start = "// <<<<<<< GENERATED"
manual_start = "// ======= MANUAL"
end = "// >>>>>>> END"

[merge_mode.markers.python]
generated_start = "# <<<<<<< GENERATED"
manual_start = "# ======= MANUAL"
end = "# >>>>>>> END"
```

### Multiple Manual Sections

```rust
// <<<<<<< GENERATED (ggen-v5, rule: structs, id: abc-123)
pub struct User {
    pub id: Uuid,
    pub email: String,
}
// ======= MANUAL (section: impl-basic)
impl User {
    pub fn new(email: String) -> Self { ... }
}
// <<<<<<< GENERATED (ggen-v5, rule: traits, id: def-456)
impl Serialize for User { ... }
// ======= MANUAL (section: impl-advanced)
impl User {
    pub fn advanced_validation(&self) -> Result<(), Error> { ... }
}
// >>>>>>> END
```

### Marker Metadata

```rust
// <<<<<<< GENERATED (
//   ggen-v5,
//   rule: structs,
//   id: abc-123,
//   generated_at: 2025-12-20T14:30:45Z,
//   template: templates/rust_struct.tera,
//   query: query/structs.rq,
//   checksum: sha256:def456...
// )
pub struct User { ... }
// >>>>>>> END
```

Use checksum to detect drift:

```bash
$ ggen sync --verify-checksums
WARNING: Checksum mismatch: src/models/user.rs
  Expected: sha256:def456...
  Actual: sha256:xyz789...
  Possible manual edit in generated section
```

## Troubleshooting

### Issue: Markers not detected

**Cause**: Incorrect marker syntax or whitespace

**Fix**: Use exact marker format (no extra spaces):
```rust
// Correct
// <<<<<<< GENERATED (ggen-v5, rule: structs, id: abc-123)

// Incorrect (extra space)
//  <<<<<<< GENERATED (ggen-v5, rule: structs, id: abc-123)
```

### Issue: Manual section deleted

**Cause**: Template regenerated without merge markers

**Fix**: Re-add manual section from git history:
```bash
git show HEAD:src/models/user.rs | \
  sed -n '/======= MANUAL/,/>>>>>>> END/p' \
  >> src/models/user.rs
```

### Issue: Conflicts on every sync

**Cause**: Manual code depends on generated code structure

**Fix**: Use stable interfaces:
```rust
// <<<<<<< GENERATED
pub struct User { ... }

impl User {
    pub fn id(&self) -> Uuid { self.id }
    pub fn email(&self) -> &str { &self.email }
}
// ======= MANUAL
impl User {
    // Depend on getters, not direct fields
    pub fn validate(&self) -> Result<(), Error> {
        if !self.email().contains('@') { ... }
    }
}
// >>>>>>> END
```

## Security Considerations

- **Merge markers are visible in generated code** (not stripped)
- **Manual sections may contain sensitive logic** (review before open-sourcing)
- **Conflicts may expose internal implementation details** in error messages

## Related Documentation

- [Force Flag](force-flag.md) - Alternative to merge mode (full regeneration)
- [Audit Trail](audit-trail.md) - Track merge operations
- [Watch Mode](watch-mode.md) - Continuous merge updates
- [Validation](validation.md) - Pre-merge constraint checking
