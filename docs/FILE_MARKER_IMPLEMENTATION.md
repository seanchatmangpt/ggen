# File Marker Implementation for Multi-File Output

## Executive Summary

**Goal**: Extend `generator.rs` to support multi-file generation from a single template using Tera comment markers.

**Approach**: ONE minimal function added to existing `Generator::generate()` flow. No new modules, no architectural changes.

**Impact**: ~50 lines of code in one file. Zero breaking changes.

---

## 1. Current System Analysis

### Existing Flow (generator.rs:60-111)

```rust
pub fn generate(&mut self) -> Result<PathBuf> {
    // 1. Read template
    let input = fs::read_to_string(&self.ctx.template_path)?;
    let mut tmpl = Template::parse(&input)?;

    // 2. Create context & render frontmatter
    let mut tctx = Context::from_serialize(&self.ctx.vars)?;
    insert_env(&mut tctx);
    tmpl.render_frontmatter(&mut self.pipeline.tera, &tctx)?;

    // 3. Process RDF graph (if any)
    tmpl.process_graph(...)?;

    // 4. Render body
    let rendered = tmpl.render(&mut self.pipeline.tera, &tctx)?;

    // 5. Determine output path
    let output_path = if let Some(to_path) = &tmpl.front.to {
        // Use frontmatter 'to' field
    } else {
        // Default to template_name.out
    };

    // 6. Write single file
    if !self.ctx.dry_run {
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&output_path, rendered)?;
    }

    Ok(output_path)
}
```

**Key Insight**: The system already renders EVERYTHING into a single `rendered` string (line 80). We just need to split it AFTER rendering.

---

## 2. Design: File Marker Syntax

### Marker Format

```tera
{# FILE: path/to/file.ext #}
content for that file
{# FILE: another/file.ext #}
content for another file
```

**Why Tera Comments?**
- Already valid Tera syntax (no parser changes needed)
- Removed during rendering, won't appear in output
- Clear visual separation
- Compatible with existing templates (ignored if no markers)

### Example Template

```tera
---
to: "project_root/"  # Base directory for all files
vars:
  service_name: "UserService"
---
{# FILE: models/user.py #}
from pydantic import BaseModel

class User(BaseModel):
    id: int
    name: str

{# FILE: api/routes/users.py #}
from fastapi import APIRouter
from models.user import User

router = APIRouter()

@router.get("/users")
async def list_users():
    return []

{# FILE: tests/test_users.py #}
from api.routes.users import router

def test_list_users():
    assert router is not None
```

**Result**: 3 files generated from one template
- `project_root/models/user.py`
- `project_root/api/routes/users.py`
- `project_root/tests/test_users.py`

---

## 3. Implementation

### 3.1 Splitting Function (NEW)

Add this function to `generator.rs` before the `Generator` implementation:

```rust
/// Split rendered content by {# FILE: path #} markers
/// Returns vec of (relative_path, content) pairs
fn split_by_file_markers(rendered: &str) -> Vec<(PathBuf, String)> {
    use regex::Regex;

    // Match: {# FILE: any/path.ext #}
    let marker_regex = Regex::new(r"\{#\s*FILE:\s*([^\}]+?)\s*#\}").unwrap();

    let mut files = Vec::new();
    let mut last_end = 0;
    let mut current_path: Option<PathBuf> = None;
    let mut current_content = String::new();

    for cap in marker_regex.captures_iter(rendered) {
        let match_start = cap.get(0).unwrap().start();
        let match_end = cap.get(0).unwrap().end();
        let path_str = cap.get(1).unwrap().as_str().trim();

        // Save previous file if exists
        if let Some(path) = current_path.take() {
            let content = rendered[last_end..match_start].trim().to_string();
            if !content.is_empty() {
                files.push((path, content));
            }
        }

        // Start new file
        current_path = Some(PathBuf::from(path_str));
        last_end = match_end;
    }

    // Handle last file (or entire content if no markers)
    if let Some(path) = current_path {
        let content = rendered[last_end..].trim().to_string();
        if !content.is_empty() {
            files.push((path, content));
        }
    } else if files.is_empty() {
        // No markers found - treat as single file (backward compatible)
        // Return empty vec to signal single-file mode
    }

    files
}
```

**Pseudocode**:
```
function split_by_file_markers(rendered):
    files = []
    regex = compile("{# FILE: ([^}]+) #}")
    matches = regex.find_all(rendered)

    if matches.empty():
        return []  # No markers, use single-file mode

    for i, match in enumerate(matches):
        path = match.group(1).trim()

        # Find content between this marker and next (or end)
        start = match.end
        end = matches[i+1].start if i+1 < len(matches) else len(rendered)
        content = rendered[start:end].trim()

        files.append((PathBuf(path), content))

    return files
```

**Edge Cases Handled**:
1. **No markers**: Returns empty vec → uses existing single-file logic
2. **Trailing whitespace**: `.trim()` cleans content
3. **Empty files**: Skipped (`if !content.is_empty()`)
4. **Invalid paths**: Handled by PathBuf validation later
5. **Malformed markers**: Regex simply doesn't match → single-file fallback

---

### 3.2 Extend Generator::generate() (MODIFY EXISTING)

Replace the file writing section (lines 102-108) with:

```rust
// Determine base output directory
let base_output_dir = if let Some(to_path) = &tmpl.front.to {
    let rendered_to = self.pipeline.tera.render_str(to_path, &tctx)?;
    self.ctx.output_root.join(rendered_to)
} else {
    self.ctx.output_root.clone()
};

// Check for multi-file markers
let file_parts = split_by_file_markers(&rendered);

if !file_parts.is_empty() {
    // Multi-file mode
    let mut created_files = Vec::new();

    if !self.ctx.dry_run {
        for (rel_path, content) in file_parts {
            let full_path = base_output_dir.join(&rel_path);

            // Create parent directories
            if let Some(parent) = full_path.parent() {
                fs::create_dir_all(parent)?;
            }

            // Write file
            fs::write(&full_path, content)?;
            created_files.push(full_path);
        }
    }

    // Return the base directory for multi-file output
    Ok(base_output_dir)
} else {
    // Single-file mode (existing behavior)
    let output_path = if let Some(to_path) = &tmpl.front.to {
        let rendered_to = self.pipeline.tera.render_str(to_path, &tctx)?;
        self.ctx.output_root.join(rendered_to)
    } else {
        let template_name = self
            .ctx
            .template_path
            .file_stem()
            .ok_or_else(|| anyhow::anyhow!("Template path has no file stem"))?
            .to_string_lossy();
        self.ctx.output_root.join(format!("{}.out", template_name))
    };

    if !self.ctx.dry_run {
        if let Some(parent) = output_path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(&output_path, rendered)?;
    }

    Ok(output_path)
}
```

**Key Changes**:
1. **Line 80**: `rendered` string is still created (no change)
2. **NEW**: Call `split_by_file_markers(&rendered)`
3. **Branch**: If markers found → multi-file, else → single-file (existing)
4. **Return**: Base directory for multi-file, specific path for single-file

**Backward Compatibility**: 100% preserved. Templates without markers use exact same code path.

---

## 4. Integration Points

### 4.1 Template.render() (NO CHANGES)

The `Template::render()` method (template.rs:298-325) already:
- Handles SPARQL results injection
- Processes Tera syntax
- Returns complete rendered string

**Why No Changes**: File markers are Tera comments → stripped during rendering, but we parse BEFORE stripping by using raw `{# ... #}` regex.

**WAIT - PROBLEM DETECTED**: Tera comments are removed during rendering!

**FIX**: Markers must survive rendering. Use custom syntax instead:

```tera
{# FILE: path.ext #}  ← Won't work (Tera removes comments)
```

Better syntax:
```tera
@@FILE: path.ext@@
content
@@FILE: other.ext@@
content
```

**OR** use Tera set variables (preserved):
```tera
{% set _file_marker = "models/user.py" %}
content
{% set _file_marker = "api/routes.py" %}
content
```

**BEST SOLUTION**: Use magic comment that survives:
```tera
{#-- FILE: path.ext --#}
```

Actually, let's test what survives Tera rendering...

**SIMPLER**: Just use a marker that's NOT a Tera comment:
```
===FILE: models/user.py===
content
===FILE: api/routes.py===
content
```

This is:
- Not Tera syntax (survives rendering)
- Easy to parse with regex
- Clear visual separation
- Won't appear in single-file templates by accident

---

### 4.2 REVISED: Marker Syntax

```
===FILE: relative/path/to/file.ext===
file content here
===FILE: another/file.ext===
more content
```

**Regex**: `===FILE:\s*([^\n=]+)===`

**Example**:
```tera
---
to: "output/"
vars:
  model: "User"
---
===FILE: models/{{ model | lower }}.py===
from pydantic import BaseModel

class {{ model }}(BaseModel):
    id: int
    name: str

===FILE: api/routes/{{ model | lower }}.py===
from fastapi import APIRouter

router = APIRouter()
```

**After Tera rendering**:
```
===FILE: models/user.py===
from pydantic import BaseModel

class User(BaseModel):
    id: int
    name: str

===FILE: api/routes/user.py===
from fastapi import APIRouter

router = APIRouter()
```

**Then our splitter**:
- Parses `===FILE: ...===` markers
- Extracts paths: `models/user.py`, `api/routes/user.py`
- Splits content between markers
- Returns `Vec<(PathBuf, String)>`

---

### 4.3 Updated Implementation

```rust
/// Split rendered content by ===FILE: path=== markers
fn split_by_file_markers(rendered: &str) -> Vec<(PathBuf, String)> {
    use regex::Regex;

    // Match: ===FILE: any/path.ext===
    let marker_regex = Regex::new(r"===FILE:\s*([^\n=]+)===").unwrap();

    let mut files = Vec::new();
    let mut positions: Vec<(usize, usize, PathBuf)> = Vec::new();

    // Collect all marker positions and paths
    for cap in marker_regex.captures_iter(rendered) {
        let match_end = cap.get(0).unwrap().end();
        let path_str = cap.get(1).unwrap().as_str().trim();
        positions.push((cap.get(0).unwrap().start(), match_end, PathBuf::from(path_str)));
    }

    if positions.is_empty() {
        return files; // No markers = single file mode
    }

    // Extract content between markers
    for i in 0..positions.len() {
        let (_, content_start, path) = &positions[i];
        let content_end = if i + 1 < positions.len() {
            positions[i + 1].0
        } else {
            rendered.len()
        };

        let content = rendered[*content_start..content_end].trim().to_string();
        if !content.is_empty() {
            files.push((path.clone(), content));
        }
    }

    files
}
```

---

## 5. Testing Strategy

### 5.1 Unit Tests (add to generator.rs tests)

```rust
#[test]
fn test_split_by_file_markers_empty() {
    let input = "Regular single file content";
    let result = split_by_file_markers(input);
    assert!(result.is_empty()); // No markers = single file
}

#[test]
fn test_split_by_file_markers_single() {
    let input = r"===FILE: output.txt===
Hello, world!";
    let result = split_by_file_markers(input);
    assert_eq!(result.len(), 1);
    assert_eq!(result[0].0, PathBuf::from("output.txt"));
    assert_eq!(result[0].1, "Hello, world!");
}

#[test]
fn test_split_by_file_markers_multiple() {
    let input = r"===FILE: models/user.py===
class User:
    pass

===FILE: api/routes.py===
from fastapi import APIRouter
router = APIRouter()";

    let result = split_by_file_markers(input);
    assert_eq!(result.len(), 2);

    assert_eq!(result[0].0, PathBuf::from("models/user.py"));
    assert!(result[0].1.contains("class User"));

    assert_eq!(result[1].0, PathBuf::from("api/routes.py"));
    assert!(result[1].1.contains("APIRouter"));
}

#[test]
fn test_split_by_file_markers_whitespace() {
    let input = r"===FILE:   models/user.py  ===
class User:
    pass";

    let result = split_by_file_markers(input);
    assert_eq!(result.len(), 1);
    assert_eq!(result[0].0, PathBuf::from("models/user.py"));
}
```

### 5.2 Integration Test

```rust
#[test]
fn test_generate_multi_file_template() {
    let (_temp_dir, template_path) = create_test_template(
        r#"---
to: "project/"
---
===FILE: models/user.py===
class User:
    def __init__(self, name):
        self.name = name

===FILE: tests/test_user.py===
from models.user import User

def test_user():
    u = User("Alice")
    assert u.name == "Alice"
"#,
    );

    let output_dir = _temp_dir.path();
    let pipeline = create_test_pipeline();
    let ctx = GenContext::new(template_path, output_dir.to_path_buf());

    let mut generator = Generator::new(pipeline, ctx);
    let result = generator.generate();

    assert!(result.is_ok());

    // Verify both files created
    let user_py = output_dir.join("project/models/user.py");
    let test_py = output_dir.join("project/tests/test_user.py");

    assert!(user_py.exists());
    assert!(test_py.exists());

    let user_content = fs::read_to_string(&user_py).unwrap();
    assert!(user_content.contains("class User"));

    let test_content = fs::read_to_string(&test_py).unwrap();
    assert!(test_content.contains("def test_user"));
}
```

---

## 6. SPARQL Integration Example

### Template with Graph Data

```tera
---
to: "generated/"
prefixes:
  schema: "http://schema.org/"
rdf_inline:
  - |
    @prefix schema: <http://schema.org/> .
    schema:Person1 a schema:Person ;
        schema:name "Alice" ;
        schema:email "alice@example.com" .
    schema:Person2 a schema:Person ;
        schema:name "Bob" ;
        schema:email "bob@example.com" .
sparql:
  people: |
    SELECT ?name ?email
    WHERE {
        ?person a schema:Person ;
            schema:name ?name ;
            schema:email ?email .
    }
    ORDER BY ?name
---
===FILE: models/person.py===
from dataclasses import dataclass

@dataclass
class Person:
    name: str
    email: str

===FILE: data/people.py===
from models.person import Person

PEOPLE = [
{% for person in sparql_results.people %}
    Person(name="{{ person.name }}", email="{{ person.email }}"),
{% endfor %}
]

===FILE: tests/test_people.py===
from data.people import PEOPLE

def test_people_count():
    assert len(PEOPLE) == {{ sparql_results.people | length }}

def test_first_person():
    assert PEOPLE[0].name == "{{ sparql_first(results=sparql_results.people, column='name') }}"
```

**Result**: 3 files generated with SPARQL data:
1. `models/person.py` - Data class definition
2. `data/people.py` - List of Person objects from RDF
3. `tests/test_people.py` - Tests using SPARQL results

---

## 7. Error Handling

### Edge Cases

| Case | Behavior |
|------|----------|
| No markers | Single-file mode (existing logic) |
| Empty file content | Skipped (not written) |
| Invalid path chars | PathBuf creation fails → error returned |
| Nested directories | `fs::create_dir_all(parent)` handles |
| Duplicate paths | Last one wins (overwrites) |
| Path traversal (`../`) | Allowed (user responsibility) |
| Absolute paths | Joined to base_output_dir |

### Security Considerations

**Path Traversal**: Currently allowed. If security needed:

```rust
// Add to file writing loop:
if rel_path.components().any(|c| matches!(c, std::path::Component::ParentDir)) {
    return Err(anyhow::anyhow!("Path traversal not allowed: {}", rel_path.display()));
}
```

**Symlink Safety**: Use `fs::canonicalize()` to resolve symlinks before writing.

---

## 8. Performance Impact

**Overhead**: Regex parsing of `rendered` string

**Analysis**:
- Templates are typically < 10KB
- Regex is compiled once (lazy_static in production)
- Single pass through string
- **Impact**: < 1ms for typical templates

**Optimization** (if needed):
```rust
use lazy_static::lazy_static;

lazy_static! {
    static ref FILE_MARKER_REGEX: Regex =
        Regex::new(r"===FILE:\s*([^\n=]+)===").unwrap();
}
```

---

## 9. Documentation Updates

### 9.1 User-Facing Docs

Add to `docs/templates.md`:

```markdown
## Multi-File Generation

Generate multiple files from a single template using file markers:

### Syntax

\`\`\`
===FILE: relative/path/to/file.ext===
file content here
===FILE: another/file.ext===
more content
\`\`\`

### Example

\`\`\`tera
---
to: "src/"
vars:
  model: "User"
---
===FILE: models/{{ model | lower }}.py===
class {{ model }}:
    pass

===FILE: tests/test_{{ model | lower }}.py===
def test_{{ model | lower }}():
    assert True
\`\`\`

### Output

- `src/models/user.py`
- `src/tests/test_user.py`

### Rules

1. Markers are processed AFTER Tera rendering
2. File paths are relative to `to:` directory
3. Paths can use template variables
4. Content is trimmed (leading/trailing whitespace removed)
5. Empty files are skipped
6. No markers = single file (backward compatible)
```

### 9.2 CLI Help

Update `ggen template generate --help`:

```
MULTI-FILE OUTPUT:
    Use ===FILE: path=== markers to generate multiple files:

    ggen template generate microservice.tmpl

    Example marker:
        ===FILE: src/main.rs===
        fn main() {}
        ===FILE: tests/test.rs===
        #[test] fn test() {}
```

---

## 10. Migration Path

### Phase 1: Implementation (Week 1)
- Add `split_by_file_markers()` function
- Modify `Generator::generate()`
- Add unit tests
- Update Cargo.toml (add `regex` dependency if needed)

### Phase 2: Testing (Week 1)
- Integration tests
- SPARQL integration tests
- Edge case validation
- Performance benchmarks

### Phase 3: Documentation (Week 2)
- Update user docs
- Add examples to repo
- CLI help updates
- Blog post / announcement

### Phase 4: Rollout (Week 2)
- Release in minor version (e.g., v1.3.0)
- Announce in changelog
- Provide migration examples

---

## 11. Alternative Approaches Considered

### A. Frontmatter Array

```yaml
outputs:
  - path: "models/user.py"
    content: |
      class User: pass
  - path: "tests/test.py"
    content: |
      def test(): pass
```

**Rejected**: Verbose, hard to read, separates paths from content

### B. Multiple Templates

Use `from:` field to chain templates:
```yaml
from: "templates/*.tmpl"
```

**Rejected**: Requires filesystem organization, not self-contained

### C. Custom Preprocessor

New `{% multifile %}` Tera tag

**Rejected**: Requires Tera extension, more complex, breaks syntax highlighting

### D. Post-Processing Script

Separate tool parses output

**Rejected**: Extra step, not integrated, breaks dry-run

**Winner**: Marker syntax (chosen approach)
- ✅ Simple
- ✅ Self-contained
- ✅ Works with existing syntax
- ✅ Backward compatible
- ✅ Visual clarity

---

## 12. Success Metrics

**Definition of Done**:
- ✅ All tests pass (unit + integration)
- ✅ Backward compatibility verified (existing templates work)
- ✅ Documentation complete
- ✅ Performance impact < 5ms for typical templates
- ✅ Zero breaking changes

**Quality Gates**:
- Code coverage > 90% for new functions
- No new clippy warnings
- Passes `cargo test --all-features`
- Dry-run mode works correctly

---

## 13. Files Modified

| File | Changes | Lines |
|------|---------|-------|
| `ggen-core/src/generator.rs` | Add splitter + modify generate() | +60 |
| `ggen-core/src/generator.rs` (tests) | New test cases | +80 |
| `ggen-core/Cargo.toml` | Add `regex` dependency | +1 |
| `docs/templates.md` | Multi-file docs | +50 |
| `cli/src/main.rs` | Help text update | +5 |
| **Total** | | **~196 lines** |

**Dependencies**:
- `regex` crate (already used in project for other features)

---

## 14. Rollback Plan

If issues found after release:

**Option 1: Feature Flag**
```rust
#[cfg(feature = "multifile")]
let file_parts = split_by_file_markers(&rendered);
#[cfg(not(feature = "multifile"))]
let file_parts = vec![];
```

**Option 2: Environment Variable**
```rust
let multifile_enabled = std::env::var("GGEN_MULTIFILE").is_ok();
if multifile_enabled {
    let file_parts = split_by_file_markers(&rendered);
    // ...
}
```

**Option 3: Revert Commit**
- Single file, minimal changes → easy to revert
- Tests ensure existing behavior preserved

---

## 15. Open Questions

1. **Should we support content BEFORE first marker?**
   ```
   This content has no marker
   ===FILE: output.txt===
   This has a marker
   ```

   **Decision**: Ignore content before first marker (warn user?)

2. **What if `to:` field is a file, not directory?**
   ```yaml
   to: "output.txt"  # Not a directory
   ```

   **Decision**: Multi-file mode treats it as base directory

3. **Should markers be case-sensitive?**
   ```
   ===file: path===  vs  ===FILE: path===
   ```

   **Decision**: Case-insensitive (use `(?i)` in regex)

4. **Handle Windows paths?**
   ```
   ===FILE: models\user.py===  (backslash)
   ```

   **Decision**: PathBuf handles platform-specific separators

---

## 16. Next Steps

1. **Implement** `split_by_file_markers()` function
2. **Test** with sample templates
3. **Integrate** into `Generator::generate()`
4. **Validate** backward compatibility
5. **Document** in user guide
6. **Release** in v1.3.0

**Estimated Effort**: 2-3 days
- Day 1: Implementation + unit tests
- Day 2: Integration tests + edge cases
- Day 3: Documentation + examples

---

## Appendix A: Complete Code Diff

### generator.rs

```diff
+use regex::Regex;
+
+/// Split rendered content by ===FILE: path=== markers
+/// Returns vec of (relative_path, content) pairs
+fn split_by_file_markers(rendered: &str) -> Vec<(PathBuf, String)> {
+    let marker_regex = Regex::new(r"===FILE:\s*([^\n=]+)===").unwrap();
+    let mut files = Vec::new();
+    let mut positions: Vec<(usize, usize, PathBuf)> = Vec::new();
+
+    for cap in marker_regex.captures_iter(rendered) {
+        let match_end = cap.get(0).unwrap().end();
+        let path_str = cap.get(1).unwrap().as_str().trim();
+        positions.push((cap.get(0).unwrap().start(), match_end, PathBuf::from(path_str)));
+    }
+
+    if positions.is_empty() {
+        return files;
+    }
+
+    for i in 0..positions.len() {
+        let (_, content_start, path) = &positions[i];
+        let content_end = if i + 1 < positions.len() {
+            positions[i + 1].0
+        } else {
+            rendered.len()
+        };
+
+        let content = rendered[*content_start..content_end].trim().to_string();
+        if !content.is_empty() {
+            files.push((path.clone(), content));
+        }
+    }
+
+    files
+}
+
 impl Generator {
     pub fn generate(&mut self) -> Result<PathBuf> {
         // ... existing code ...
         let rendered = tmpl.render(&mut self.pipeline.tera, &tctx)?;

-        let output_path = if let Some(to_path) = &tmpl.front.to {
-            // ...
-        } else {
-            // ...
-        };
+        // Determine base output directory
+        let base_output_dir = if let Some(to_path) = &tmpl.front.to {
+            let rendered_to = self.pipeline.tera.render_str(to_path, &tctx)?;
+            self.ctx.output_root.join(rendered_to)
+        } else {
+            self.ctx.output_root.clone()
+        };

-        if !self.ctx.dry_run {
-            if let Some(parent) = output_path.parent() {
-                fs::create_dir_all(parent)?;
+        // Check for multi-file markers
+        let file_parts = split_by_file_markers(&rendered);
+
+        if !file_parts.is_empty() {
+            // Multi-file mode
+            if !self.ctx.dry_run {
+                for (rel_path, content) in file_parts {
+                    let full_path = base_output_dir.join(&rel_path);
+                    if let Some(parent) = full_path.parent() {
+                        fs::create_dir_all(parent)?;
+                    }
+                    fs::write(&full_path, content)?;
+                }
+            }
+            Ok(base_output_dir)
+        } else {
+            // Single-file mode (existing)
+            let output_path = if let Some(to_path) = &tmpl.front.to {
+                let rendered_to = self.pipeline.tera.render_str(to_path, &tctx)?;
+                self.ctx.output_root.join(rendered_to)
+            } else {
+                let template_name = self.ctx.template_path.file_stem()
+                    .ok_or_else(|| anyhow::anyhow!("Template path has no file stem"))?
+                    .to_string_lossy();
+                self.ctx.output_root.join(format!("{}.out", template_name))
+            };
+
+            if !self.ctx.dry_run {
+                if let Some(parent) = output_path.parent() {
+                    fs::create_dir_all(parent)?;
+                }
+                fs::write(&output_path, rendered)?;
             }
-            fs::write(&output_path, rendered)?;
+            Ok(output_path)
         }
-
-        Ok(output_path)
     }
 }
```

---

## Summary

**What**: Add multi-file generation using `===FILE: path===` markers
**Where**: One function in `generator.rs` + modify existing `generate()` method
**Why**: Enable microservice/project generation from single template
**How**: Parse markers AFTER Tera rendering, split content, write multiple files
**Risk**: Very low (backward compatible, isolated change)
**Effort**: 2-3 days
**Impact**: Unlocks powerful new use cases (SPARC integration, full project generation)

This is the MINIMAL implementation that provides MAXIMUM value with ZERO breaking changes.
