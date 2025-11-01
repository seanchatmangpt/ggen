# RDF-Driven Multi-File Generation Vision

**Status**: Architecture Vision Document
**Date**: 2025-11-01
**Author**: System Architect

---

## Executive Summary

**Vision**: Enable ONE template file to query an RDF ontology and generate ALL files for a complete project (FastAPI, Rust, etc.) - seamlessly integrated into ggen's existing frontmatter system.

**Key Principle**: RDF is the source of truth, templates query RDF, generation outputs what RDF defines.

**Implementation Strategy**: Minimal extension to existing `generator.rs` using file markers within template body.

---

## 1. Current State Analysis

### 1.1 Existing Frontmatter System

From `/Users/sac/ggen/ggen-core/src/template.rs`:

```rust
pub struct Frontmatter {
    // Output control
    pub to: Option<String>,           // Single output file path
    pub from: Option<String>,         // Input file override

    // RDF integration (ALREADY EXISTS!)
    pub rdf: Vec<String>,             // External .ttl files
    pub rdf_inline: Vec<String>,      // Inline Turtle triples
    pub sparql: BTreeMap<String, String>,  // Named SPARQL queries
    pub prefixes: BTreeMap<String, String>,
    pub base: Option<String>,

    // Results (populated during process_graph)
    pub sparql_results: BTreeMap<String, serde_json::Value>,
}
```

**Key Insight**: RDF/SPARQL integration ALREADY WORKS! Templates can:
1. Load external `.ttl` ontology files via `rdf: domain.ttl`
2. Execute multiple named SPARQL queries
3. Access results in template via `sparql_results.query_name`

### 1.2 Existing Generation Flow

From `/Users/sac/ggen/ggen-core/src/generator.rs`:

```rust
pub fn generate(&mut self) -> Result<PathBuf> {
    let input = fs::read_to_string(&self.ctx.template_path)?;
    let mut tmpl = Template::parse(&input)?;

    // 1. Render frontmatter (resolve {{vars}})
    tmpl.render_frontmatter(&mut self.pipeline.tera, &tctx)?;

    // 2. Process graph (load RDF, run SPARQL)
    tmpl.process_graph(&mut self.pipeline.graph, &mut self.pipeline.tera, &tctx, &self.ctx.template_path)?;

    // 3. Render body (with sparql_results available)
    let rendered = tmpl.render(&mut self.pipeline.tera, &tctx)?;

    // 4. Write to SINGLE output file (determined by frontmatter.to)
    let output_path = if let Some(to_path) = &tmpl.front.to {
        self.ctx.output_root.join(rendered_to)
    } else {
        self.ctx.output_root.join(format!("{}.out", template_name))
    };

    fs::write(&output_path, rendered)?;  // ← SINGLE FILE OUTPUT
    Ok(output_path)
}
```

**Current Limitation**: Generator writes ONE rendered body to ONE output file.

### 1.3 Existing Multi-File Pattern (Workaround)

Current approach for multi-file projects (see `examples/microservices-architecture/`):
- **Multiple separate template files**, each with its own frontmatter
- Each template queries the SAME RDF ontology
- Generate multiple times: `ggen gen user-service.tmpl && ggen gen api-gateway.tmpl && ...`

**Problem**:
- Template duplication (same SPARQL queries in multiple files)
- Manual coordination across templates
- Can't guarantee consistency
- Doesn't scale to 50+ files

---

## 2. The Vision: RDF-Driven Single Template → Multiple Files

### 2.1 Core Ethos

**"RDF defines WHAT exists, Templates define HOW to render it"**

```
Ontology (domain.ttl):
  :User, :Product, :Order exist
  Each has properties (:field relationships)
  Each has routes (:endpoint relationships)

Template (fastapi-stack.tmpl):
  FOR EACH model in ontology:
    Generate models/{model}.py
    Generate api/routes/{model}.py
    Generate tests/test_{model}.py
  FROM ONE TEMPLATE FILE
```

### 2.2 Concrete Example: FastAPI Project

#### Domain Ontology (`domain.ttl`)

```turtle
@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

# Model definitions
:User a :Model ;
    :modelName "User" ;
    :tableName "users" ;
    :route "/users" ;
    :field :userName, :userEmail, :userPassword ;
    :endpoint :createUser, :getUser, :updateUser, :deleteUser .

:Product a :Model ;
    :modelName "Product" ;
    :tableName "products" ;
    :route "/products" ;
    :field :productName, :productPrice, :productInventory ;
    :endpoint :createProduct, :listProducts .

# Field definitions
:userName a :StringField ;
    :fieldName "username" ;
    :fieldType "str" ;
    :required true ;
    :maxLength 50 .

:userEmail a :EmailField ;
    :fieldName "email" ;
    :fieldType "EmailStr" ;
    :required true ;
    :unique true .

:userPassword a :PasswordField ;
    :fieldName "password_hash" ;
    :fieldType "str" ;
    :writeOnly true .

:productName a :StringField ;
    :fieldName "name" ;
    :fieldType "str" ;
    :required true .

:productPrice a :DecimalField ;
    :fieldName "price" ;
    :fieldType "Decimal" ;
    :required true ;
    :minValue 0 .

:productInventory a :IntegerField ;
    :fieldName "inventory" ;
    :fieldType "int" ;
    :default 0 .

# Endpoint definitions
:createUser a :Endpoint ;
    :method "POST" ;
    :path "/" ;
    :requestBody :UserCreateRequest ;
    :response :UserResponse .

:getUser a :Endpoint ;
    :method "GET" ;
    :path "/{user_id}" ;
    :response :UserResponse .
```

#### Single Template (`fastapi-stack.tmpl`)

**This is the KEY innovation - ONE file generates ALL project files:**

```yaml
---
# Standard ggen frontmatter (existing style)
rdf: domain.ttl

sparql:
  models: |
    SELECT ?model ?modelName ?tableName ?route
    WHERE {
      ?model a :Model ;
             :modelName ?modelName ;
             :tableName ?tableName ;
             :route ?route .
    }

  fields: |
    SELECT ?model ?field ?fieldName ?fieldType ?required ?unique
    WHERE {
      ?model a :Model ; :field ?field .
      ?field :fieldName ?fieldName ; :fieldType ?fieldType .
      OPTIONAL { ?field :required ?required }
      OPTIONAL { ?field :unique ?unique }
    }

  endpoints: |
    SELECT ?model ?endpoint ?method ?path
    WHERE {
      ?model a :Model ; :endpoint ?endpoint .
      ?endpoint :method ?method ; :path ?path .
    }

# NO multi_output needed - use file markers in body!
---
{# FILE: models/__init__.py #}
"""Generated models for FastAPI project."""
{% for result in sparql_results.models -%}
from .{{ result.modelName | lower }} import {{ result.modelName }}
{% endfor %}

{# FILE: models/user.py #}
"""User model generated from RDF ontology."""
from pydantic import BaseModel, EmailStr
from typing import Optional
from datetime import datetime

{% set model_name = "User" %}
{% set fields = sparql_results.fields | selectattr("model", "containing", "User") %}

class {{ model_name }}Base(BaseModel):
    {% for field in fields -%}
    {{ field.fieldName }}: {{ field.fieldType }}{{ " | None = None" if not field.required else "" }}
    {% endfor %}

class {{ model_name }}Create({{ model_name }}Base):
    pass

class {{ model_name }}Response({{ model_name }}Base):
    id: int
    created_at: datetime

    class Config:
        orm_mode = True

{# FILE: models/product.py #}
"""Product model generated from RDF ontology."""
from pydantic import BaseModel
from decimal import Decimal
from datetime import datetime

{% set model_name = "Product" %}
{% set fields = sparql_results.fields | selectattr("model", "containing", "Product") %}

class {{ model_name }}Base(BaseModel):
    {% for field in fields -%}
    {{ field.fieldName }}: {{ field.fieldType }}{{ " = 0" if field.default else "" }}
    {% endfor %}

class {{ model_name }}Create({{ model_name }}Base):
    pass

class {{ model_name }}Response({{ model_name }}Base):
    id: int
    created_at: datetime

    class Config:
        orm_mode = True

{# FILE: api/routes/users.py #}
"""User routes generated from RDF ontology."""
from fastapi import APIRouter, Depends, HTTPException
from sqlalchemy.orm import Session
from typing import List
from ...models.user import UserCreate, UserResponse
from ...database import get_db

router = APIRouter(prefix="{{ sparql_first(results=sparql_results.models, column='route') }}", tags=["users"])

{% set endpoints = sparql_results.endpoints | selectattr("model", "containing", "User") %}
{% for endpoint in endpoints %}
@router.{{ endpoint.method | lower }}("{{ endpoint.path }}")
async def {{ endpoint.endpoint | lower | replace(":", "_") }}(
    db: Session = Depends(get_db)
) -> UserResponse:
    # TODO: Implement endpoint logic
    pass
{% endfor %}

{# FILE: api/routes/products.py #}
"""Product routes generated from RDF ontology."""
from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session
from ...models.product import ProductCreate, ProductResponse
from ...database import get_db

router = APIRouter(prefix="/products", tags=["products"])

@router.post("/")
async def create_product(product: ProductCreate, db: Session = Depends(get_db)) -> ProductResponse:
    # TODO: Implement
    pass

@router.get("/")
async def list_products(db: Session = Depends(get_db)) -> List[ProductResponse]:
    # TODO: Implement
    pass

{# FILE: tests/test_user.py #}
"""User tests generated from RDF ontology."""
import pytest
from fastapi.testclient import TestClient
from ..main import app

client = TestClient(app)

def test_create_user():
    response = client.post("/users/", json={
        "username": "testuser",
        "email": "test@example.com",
        "password_hash": "hashed_password"
    })
    assert response.status_code == 200
    assert response.json()["username"] == "testuser"

def test_get_user():
    # TODO: Implement test
    pass

{# FILE: tests/test_product.py #}
"""Product tests generated from RDF ontology."""
import pytest
from fastapi.testclient import TestClient

def test_create_product():
    # TODO: Implement test
    pass

{# FILE: main.py #}
"""FastAPI application generated from RDF ontology."""
from fastapi import FastAPI
{% for result in sparql_results.models -%}
from .api.routes.{{ result.modelName | lower }}s import router as {{ result.modelName | lower }}_router
{% endfor %}

app = FastAPI(title="Generated FastAPI Project")

{% for result in sparql_results.models -%}
app.include_router({{ result.modelName | lower }}_router)
{% endfor %}

@app.get("/")
async def root():
    return {"message": "Generated from RDF ontology"}
```

**Result**: ONE `ggen gen fastapi-stack.tmpl` generates:
```
models/
  __init__.py
  user.py
  product.py
api/
  routes/
    users.py
    products.py
tests/
  test_user.py
  test_product.py
main.py
```

---

## 3. Implementation Strategy: Minimal Extension

### 3.1 File Marker Syntax

**Add to template body (Tera comments, non-invasive):**

```jinja2
{# FILE: path/to/output.py #}
... content for this file ...

{# FILE: another/path.rs #}
... content for next file ...
```

**Why this syntax:**
- ✅ Uses Tera's existing `{# comment #}` syntax
- ✅ Doesn't break existing single-file templates
- ✅ Clear, grep-able marker: `{# FILE: `
- ✅ Familiar to developers (similar to `// FILE:` in other tools)

### 3.2 Generator Extension (Minimal Change)

**File**: `/Users/sac/ggen/ggen-core/src/generator.rs`

**Current (line 79-110):**
```rust
// Render body
let rendered = tmpl.render(&mut self.pipeline.tera, &tctx)?;

// Determine output path
let output_path = if let Some(to_path) = &tmpl.front.to {
    let rendered_to = self.pipeline.tera.render_str(to_path, &tctx)?;
    self.ctx.output_root.join(rendered_to)
} else {
    self.ctx.output_root.join(format!("{}.out", template_name))
};

if !self.ctx.dry_run {
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(&output_path, rendered)?;  // ← SINGLE FILE
}

Ok(output_path)
```

**Proposed Extension:**

```rust
// Render body
let rendered = tmpl.render(&mut self.pipeline.tera, &tctx)?;

// Check for multi-file markers
if rendered.contains("{# FILE:") {
    // Multi-file generation mode
    self.generate_multi_file(&rendered)?;

    // Return the output directory instead of single file
    Ok(self.ctx.output_root.clone())
} else {
    // Single-file generation (existing behavior)
    let output_path = if let Some(to_path) = &tmpl.front.to {
        let rendered_to = self.pipeline.tera.render_str(to_path, &tctx)?;
        self.ctx.output_root.join(rendered_to)
    } else {
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

**New helper method:**

```rust
impl Generator {
    /// Generate multiple files from file markers in rendered template
    fn generate_multi_file(&mut self, rendered: &str) -> Result<()> {
        // Regex to match: {# FILE: path/to/file.ext #}
        let file_marker_regex = regex::Regex::new(r"\{#\s*FILE:\s*([^\s#]+)\s*#\}")?;

        // Split rendered content by file markers
        let mut current_file: Option<PathBuf> = None;
        let mut current_content = String::new();

        for line in rendered.lines() {
            if let Some(captures) = file_marker_regex.captures(line) {
                // Write previous file if exists
                if let Some(path) = current_file.take() {
                    self.write_file(&path, &current_content)?;
                    current_content.clear();
                }

                // Start new file
                let file_path = captures.get(1).unwrap().as_str();
                current_file = Some(self.ctx.output_root.join(file_path));
            } else {
                // Accumulate content for current file
                current_content.push_str(line);
                current_content.push('\n');
            }
        }

        // Write final file
        if let Some(path) = current_file {
            self.write_file(&path, &current_content)?;
        }

        Ok(())
    }

    fn write_file(&self, path: &PathBuf, content: &str) -> Result<()> {
        if !self.ctx.dry_run {
            if let Some(parent) = path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(path, content.trim())?;
        }
        Ok(())
    }
}
```

**Total code addition**: ~50 lines (one new method, one conditional check).

---

## 4. Comparison: Current vs. Proposed

### 4.1 Current State (Multi-Template Approach)

**Structure**:
```
examples/microservices-architecture/
  data/
    domain.ttl          ← Shared ontology
  templates/
    user-service.tmpl   ← Each queries domain.ttl
    api-gateway.tmpl    ← Duplicate SPARQL queries
    order-service.tmpl  ← Manual coordination
  generate.sh           ← Script to run all templates
```

**user-service.tmpl**:
```yaml
---
to: "services/user-service/src/main.rs"
rdf_inline: "@prefix ex: ... ex:UserService ex:manages ex:User ."
sparql:
  user_operations: "SELECT ?operation WHERE { ex:UserService ex:handles ?operation }"
---
// Generated service for User
// ... 417 lines of Rust code ...
```

**Workflow**:
```bash
ggen gen templates/user-service.tmpl
ggen gen templates/api-gateway.tmpl
ggen gen templates/order-service.tmpl
# ... repeat for each service
```

**Problems**:
1. ❌ SPARQL query duplication across templates
2. ❌ Manual coordination of model definitions
3. ❌ Can't guarantee consistency between files
4. ❌ Doesn't scale to 50+ file projects

### 4.2 Proposed State (Single-Template Approach)

**Structure**:
```
examples/fastapi-project/
  data/
    domain.ttl           ← Single source of truth
  templates/
    fastapi-stack.tmpl   ← ONE template, ALL files
```

**fastapi-stack.tmpl**:
```yaml
---
rdf: domain.ttl
sparql:
  models: "SELECT ?model ..."
  fields: "SELECT ?field ..."
  endpoints: "SELECT ?endpoint ..."
---
{# FILE: models/__init__.py #}
{% for model in sparql_results.models %}...{% endfor %}

{# FILE: models/user.py #}
{% set fields = sparql_results.fields | filter ... %}
class User(BaseModel): ...

{# FILE: api/routes/users.py #}
@router.post("/") ...

{# FILE: tests/test_user.py #}
def test_create_user(): ...
```

**Workflow**:
```bash
ggen gen templates/fastapi-stack.tmpl
# ✅ Generates ALL 7+ files in one command
```

**Benefits**:
1. ✅ SPARQL queries defined ONCE, used everywhere
2. ✅ Automatic consistency (same ontology, same results)
3. ✅ Scales to 100+ files (just add more `{# FILE: ... #}` markers)
4. ✅ True "RDF as source of truth" architecture

---

## 5. Migration Path & Backward Compatibility

### 5.1 Backward Compatibility

**100% backward compatible!** Existing templates work unchanged:

```yaml
---
to: "output.rs"
---
fn main() { println!("Hello"); }
```

**Behavior**: No `{# FILE: #}` markers → single-file generation (existing code path).

### 5.2 Migration Strategy

**Phase 1: Feature Flag (Optional)**
```yaml
---
rdf: domain.ttl
sparql: { ... }
multi_output: true  # Optional flag to enable feature
---
{# FILE: ... #}
```

**Phase 2: Automatic Detection (Recommended)**
- If `rendered.contains("{# FILE:")` → multi-file mode
- Else → single-file mode (existing behavior)

**Phase 3: Documentation & Examples**
- Add `examples/fastapi-multi-file/`
- Update docs: "Multi-File Generation from RDF"
- CLI hint: `ggen gen --explain` shows file marker syntax

---

## 6. Advanced Features (Future Extensions)

### 6.1 Conditional File Generation

```jinja2
{% if sparql_count(results=sparql_results.models) > 0 %}
{# FILE: models/__init__.py #}
...
{% endif %}
```

### 6.2 Dynamic File Paths from RDF

```jinja2
{% for model in sparql_results.models %}
{# FILE: models/{{ model.modelName | lower }}.py #}
class {{ model.modelName }}(BaseModel): ...
{% endfor %}
```

**Result**: Number of files determined by RDF content!

### 6.3 File Organization Queries

**Ontology extension**:
```turtle
:User :outputPath "models/user.py" ;
      :testPath "tests/test_user.py" .
```

**Template**:
```jinja2
{% for model in sparql_results.models %}
{# FILE: {{ model.outputPath }} #}
...
{# FILE: {{ model.testPath }} #}
...
{% endfor %}
```

---

## 7. Alternatives Considered

### 7.1 Alternative: New Frontmatter Field

```yaml
---
rdf: domain.ttl
sparql: { ... }
outputs:
  - path: "models/user.py"
    template: "{{ user_model_template }}"
  - path: "api/routes/users.py"
    template: "{{ user_routes_template }}"
---
```

**Rejected because**:
- ❌ Breaks single-file ethos (template in YAML vs. body)
- ❌ Less flexible (can't use Tera loops in outputs)
- ❌ Harder to read/maintain

### 7.2 Alternative: External Multi-File Config

```toml
# ggen-multi.toml
[[outputs]]
path = "models/user.py"
template = "templates/user-model.tmpl"

[[outputs]]
path = "api/routes/users.py"
template = "templates/user-routes.tmpl"
```

**Rejected because**:
- ❌ Back to multiple template files
- ❌ External config adds complexity
- ❌ Doesn't leverage RDF query power

### 7.3 **Chosen: File Markers in Body** ✅

**Why**:
- ✅ Keeps everything in ONE file
- ✅ Leverages existing Tera engine
- ✅ Natural to read/write
- ✅ Minimal code change
- ✅ Familiar pattern (like Hygen, plop.js)

---

## 8. Success Metrics

**How we know this works:**

1. **Developer Experience**
   - ✅ Generate 10+ file FastAPI project: `ggen gen fastapi-stack.tmpl` (one command)
   - ✅ Modify domain.ttl → re-generate → all files updated consistently

2. **Maintainability**
   - ✅ Add new model to ontology → template automatically generates new files
   - ✅ SPARQL queries in ONE place, not duplicated across templates

3. **Scalability**
   - ✅ 50+ file enterprise project from ONE template
   - ✅ Generation time: <5 seconds for full stack

4. **Adoption**
   - ✅ Existing templates unaffected (backward compatible)
   - ✅ Clear migration path (add file markers incrementally)

---

## 9. Implementation Checklist

### Phase 1: Core Feature (Week 1)
- [ ] Add `generate_multi_file()` to `generator.rs`
- [ ] Add file marker regex parsing
- [ ] Add file writing with directory creation
- [ ] Unit tests for multi-file generation

### Phase 2: Integration (Week 2)
- [ ] Integration test: FastAPI example (domain.ttl → 7 files)
- [ ] Integration test: Rust microservices (domain.ttl → 10 files)
- [ ] CLI integration (`ggen gen --dry-run` shows file list)

### Phase 3: Documentation (Week 3)
- [ ] Update `docs/templates/multi-file.md`
- [ ] Add `examples/fastapi-multi-file/`
- [ ] Add `examples/rust-multi-module/`
- [ ] Update CLI help text

### Phase 4: Polish (Week 4)
- [ ] Error handling (duplicate file paths, invalid markers)
- [ ] `ggen gen --list-files` to preview outputs
- [ ] Template validation (warn if file paths overlap)

---

## 10. Conclusion

**The Vision**: RDF-driven multi-file generation transforms ggen from a "template engine" into a **knowledge-graph-to-codebase compiler**.

**The Path**: Minimal extension (~50 lines) to existing `generator.rs`, using file markers in template body.

**The Impact**:
- ONE template queries ONE ontology → generates COMPLETE projects
- True semantic code generation
- Scales from 1 file to 100+ files seamlessly
- Maintains ggen's simplicity and existing frontmatter style

**Next Steps**:
1. Prototype `generate_multi_file()` implementation
2. Create FastAPI example with domain.ttl
3. Validate with real-world use case (10+ file project)

---

## Appendix A: Reference Implementation

See complete prototype code:
- `/Users/sac/ggen/ggen-core/src/generator.rs` (lines 79-110)
- `/Users/sac/ggen/ggen-core/src/template.rs` (lines 38-105)
- `/Users/sac/ggen/examples/microservices-architecture/data/domain.ttl`

**Example command**:
```bash
cd examples/fastapi-multi-file
ggen gen templates/fastapi-stack.tmpl --output-dir ./generated

# Result:
# generated/
#   models/__init__.py
#   models/user.py
#   models/product.py
#   api/routes/users.py
#   api/routes/products.py
#   tests/test_user.py
#   tests/test_product.py
#   main.py
```

---

**Document End**
