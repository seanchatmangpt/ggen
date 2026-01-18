# FastAPI from RDF - Complete Example

This example demonstrates **RDF-driven code generation** for a complete FastAPI e-commerce application.

## 🎯 Concept: "RDF Defines, Template Generates"

**The Power**: Define your domain model once in RDF, generate infinite variations with different templates.

```
domain.ttl (ONE semantic model)
    ↓
api-stack.tmpl (ONE template)
    ↓
Complete FastAPI Project (15+ files)
```

## 📁 What's Included

### Input Files
1. **`domain.ttl`** - RDF ontology defining:
   - 3 models (User, Product, Order)
   - 20+ fields with types, validations, constraints
   - 4 relationships (one-to-many, many-to-many)
   - 15 API routes with full metadata

2. **`api-stack.tmpl`** - Single template that generates:
   - SQLAlchemy async models
   - Pydantic schemas (Base, Create, Update, Response)
   - FastAPI route handlers
   - Pytest test suite
   - Docker configuration
   - Requirements and documentation

### Output Structure (Generated)
```
my_api/
├── main.py                    # FastAPI app with lifespan
├── requirements.txt           # All dependencies
├── Dockerfile                 # Production Docker image
├── docker-compose.yml         # Full stack (API + PostgreSQL)
├── .env.example              # Environment template
├── core/
│   ├── config.py             # Pydantic settings
│   └── database.py           # Async SQLAlchemy setup
├── models/
│   ├── user.py               # User model with relationships
│   ├── product.py            # Product model
│   └── order.py              # Order model
├── schemas/
│   ├── user.py               # User schemas (Create, Update, Response)
│   ├── product.py            # Product schemas
│   └── order.py              # Order schemas
├── api/
│   └── routes/
│       ├── users.py          # 5 user endpoints (CRUD + list)
│       ├── products.py       # 5 product endpoints
│       └── orders.py         # 4 order endpoints
└── tests/
    ├── conftest.py           # Test fixtures and setup
    ├── test_users.py         # User endpoint tests
    ├── test_products.py      # Product endpoint tests
    └── test_orders.py        # Order endpoint tests
```

## 🚀 Quick Start

### Prerequisites
```bash
# Install ggen CLI
cargo install ggen

# Or from source
cd /Users/sac/ggen
cargo build --release
```

### Generate the Project

```bash
# Navigate to example directory
cd /Users/sac/ggen/examples/fastapi-from-rdf

# Generate the complete FastAPI project
ggen template generate api-stack.tmpl --var project_name=my_api

# The output will be in: my_api/
```

### Run the Generated Application

```bash
# Using Docker (recommended)
cd my_api
docker-compose up

# Access API at http://localhost:8000
# View docs at http://localhost:8000/docs
```

**OR**

```bash
# Local development
cd my_api
pip install -r requirements.txt
cp .env.example .env

# Set up PostgreSQL
createdb my_api

# Run the app
uvicorn main:app --reload

# Run tests
pytest
```

## 🎓 Understanding the Pattern

### 1. RDF Domain Model

The `domain.ttl` file defines the complete domain using semantic web standards:

```turtle
:User a api:Model ;
    api:hasField :User_email ;
    api:hasRelationship :User_orders ;
    api:hasRoute :User_listRoute .

:User_email a api:Field ;
    api:fieldName "email" ;
    api:pythonType "EmailStr" ;
    api:isRequired true ;
    api:validation "email" .
```

**Benefits**:
- ✅ **Single source of truth** for domain logic
- ✅ **Tool-independent** (can generate Java, Go, TypeScript, etc.)
- ✅ **Queryable** with SPARQL
- ✅ **Extensible** without changing templates

### 2. SPARQL Queries

The template uses SPARQL to extract structured data:

```yaml
sparql:
  models: |
    SELECT ?model ?label ?tableName ?routePrefix
    WHERE {
      ?model a api:Model ;
             rdfs:label ?label ;
             api:tableName ?tableName .
    }

  fields: |
    SELECT ?model ?fieldName ?pythonType ?isRequired
    WHERE {
      ?model api:hasField ?field .
      ?field api:fieldName ?fieldName ;
             api:pythonType ?pythonType .
    }
```

**Benefits**:
- ✅ **Declarative data extraction** (SQL for RDF)
- ✅ **Relationship traversal** built-in
- ✅ **Filter and aggregate** as needed

### 3. Template Generation

The template iterates over SPARQL results and generates code:

```jinja2
{% for model in query('models') %}
{# FILE: models/{{ model.label | lower }}.py #}
class {{ model.label }}(Base):
    __tablename__ = "{{ model.tableName }}"

    {% for field in query('fields') if field.model == model.model %}
    {{ field.fieldName }} = Column(...)
    {% endfor %}
{% endfor %}
```

**Benefits**:
- ✅ **Multi-file output** using `{# FILE: path #}` markers
- ✅ **Conditionals and loops** for complex logic
- ✅ **Filters and functions** for transformations

## 📊 What Gets Generated

### Models (SQLAlchemy)
- Async database models
- Proper column types (UUID, String, Integer, Decimal, DateTime)
- Relationships (one-to-many, many-to-many)
- Foreign key constraints

### Schemas (Pydantic)
- Base schemas with shared fields
- Create schemas (including secrets)
- Update schemas (all optional)
- Response schemas (from database)
- Full validation (min/max length, patterns, email)

### Routes (FastAPI)
- List endpoints with pagination
- Get by ID with 404 handling
- Create with 201 status
- Update with partial data
- Delete with 204 response

### Tests (Pytest)
- Async test suite
- Database fixtures
- Test client setup
- CRUD operation tests
- Error case tests

### Infrastructure
- Docker multi-service setup
- Environment configuration
- Database migrations ready
- Production Dockerfile

## 🔄 Workflow: Modify Domain, Regenerate

**The killer feature**: Change RDF, regenerate everything.

### Example: Add a new field

**1. Edit `domain.ttl`**:
```turtle
:User_phone a api:Field ;
    api:fieldName "phone" ;
    api:pythonType "str" ;
    api:isRequired false ;
    api:pattern "^\\+?[1-9]\\d{1,14}$" .

:User api:hasField :User_phone .
```

**2. Regenerate**:
```bash
ggen template generate api-stack.tmpl --var project_name=my_api
```

**3. Result**:
- ✅ Phone field added to User model
- ✅ Phone field in all schemas
- ✅ Phone field in tests
- ✅ Validation pattern applied

**No manual editing of 15+ files!**

## 🎯 Use Cases

### 1. Rapid Prototyping
```bash
# Define domain → Generate → Test API
# Iterate in minutes, not hours
```

### 2. Multi-Framework Support
```bash
# Same domain.ttl
# Different templates:
#   - FastAPI (Python)
#   - Express (Node.js)
#   - Spring Boot (Java)
#   - Gin (Go)
```

### 3. Consistent Architecture
```bash
# Team standard template
# Domain models per project
# = Consistent codebase across all services
```

### 4. Documentation as Code
```bash
# RDF is the documentation
# Code matches domain model
# Always in sync
```

## 🔍 Key Patterns Demonstrated

### Pattern 1: Multi-File Generation
```jinja2
{# FILE: models/user.py #}
class User(Base):
    ...

{# FILE: schemas/user.py #}
class UserBase(BaseModel):
    ...

{# FILE: api/routes/users.py #}
router = APIRouter()
...
```

### Pattern 2: Conditional Generation
```jinja2
{% if field.isPrimaryKey %}
    {{ field.fieldName }} = Column(UUID, primary_key=True)
{% elif field.isForeignKey %}
    {{ field.fieldName }} = Column(UUID, ForeignKey(...))
{% endif %}
```

### Pattern 3: Relationship Traversal
```jinja2
{% for rel in query('relationships') if rel.model == model.model %}
{% if rel.relType == 'oneToMany' %}
    {{ rel.relName }} = relationship("{{ rel.targetLabel }}")
{% endif %}
{% endfor %}
```

### Pattern 4: Data Transformation
```jinja2
{# Convert RDF field types to Python types #}
{% if field.dataType == 'String' %}
    Column(String({{ field.maxLength }}))
{% elif field.dataType == 'Decimal' %}
    Column(Numeric(precision=10, scale=2))
{% endif %}
```

## 📚 Files Reference

| File | Purpose | Lines | Key Features |
|------|---------|-------|--------------|
| `domain.ttl` | Semantic model | 400+ | Models, fields, relationships, routes |
| `api-stack.tmpl` | Code generator | 800+ | SPARQL queries, multi-file output, full stack |
| `README.md` | Documentation | - | This file! |

## 🎓 Learning Path

1. **Read `domain.ttl`**: Understand how models are defined semantically
2. **Study SPARQL queries**: See how data is extracted from RDF
3. **Examine template loops**: Follow the generation logic
4. **Generate and run**: See the real output
5. **Modify domain**: Change RDF and regenerate
6. **Create variants**: Try different templates for same domain

## 🚀 Next Steps

### Extend the Example

**Add authentication**:
```turtle
:AuthToken a api:Model ;
    api:hasField :AuthToken_token, :AuthToken_user_id ;
    api:routePrefix "/api/v1/auth" .
```

**Add business logic**:
```turtle
:Product_applyDiscount a api:Method ;
    api:methodName "apply_discount" ;
    api:parameters (:discount_percentage) ;
    api:returnType "Decimal" .
```

**Add GraphQL support**:
- Create `graphql-stack.tmpl`
- Use same `domain.ttl`
- Generate GraphQL schema + resolvers

### Advanced Topics

- **Multi-tenant models**: Add tenant_id automatically
- **Audit trails**: Generate created_at/updated_at from template
- **Soft deletes**: Add is_deleted field from configuration
- **OpenAPI extensions**: Generate full OpenAPI 3.1 spec

## 💡 Key Insights

1. **RDF is the master**: Domain model is tool-independent
2. **SPARQL is powerful**: Rich queries over semantic data
3. **Templates are reusable**: One template, many projects
4. **Multi-file is essential**: Real projects need structure
5. **Regeneration is safe**: Separate generated code from custom code

## 🎉 Success Metrics

After running this example, you'll understand:

- ✅ How to model domains in RDF/OWL
- ✅ How to query RDF with SPARQL
- ✅ How to generate multi-file projects
- ✅ How to separate domain from implementation
- ✅ How to iterate rapidly with regeneration

## 📞 Support

- **Issues**: GitHub Issues for ggen project
- **Docs**: See `/Users/sac/ggen/docs/templates.md`
- **Examples**: More examples in `/Users/sac/ggen/examples/`

---

**Generated by ggen** - The RDF-driven code generation engine
