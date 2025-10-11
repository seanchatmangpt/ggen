# Example 2: Template Rendering and Creation

This example demonstrates working with templates - creating, validating, and rendering them with variables.

## Creating a Custom Template

### User:
```
I want to create a custom template for my team's Python microservices. Can you help me set that up?
```

### Claude:
I'll help you create a custom Python microservice template. Let me start by creating the template structure.

**Tool Call:** `template_create`
```json
{
  "name": "python-microservice",
  "template_type": "project"
}
```

**Response:**
```json
{
  "status": "success",
  "template_path": "/path/to/templates/python-microservice",
  "created_files": [
    "python-microservice/template.yaml",
    "python-microservice/README.md",
    "python-microservice/files/"
  ],
  "message": "Template skeleton created. Add your files to the files/ directory."
}
```

Great! I've created a template skeleton at `/path/to/templates/python-microservice`. Here's what was created:

```
python-microservice/
├── template.yaml        # Template configuration
├── README.md           # Template documentation
└── files/              # Template files go here
```

Now you can:
1. Add your template files to `files/`
2. Configure variables in `template.yaml`
3. Test with `template_validate`

Let me show you a sample `template.yaml` for a Python microservice:

```yaml
name: python-microservice
version: 1.0.0
description: Python FastAPI microservice with Docker and tests

variables:
  service_name:
    type: string
    description: Name of the microservice
    required: true
    pattern: "^[a-z][a-z0-9-]*$"

  port:
    type: integer
    description: Service port number
    default: 8000
    minimum: 1024
    maximum: 65535

  database:
    type: string
    description: Database type
    enum: ["postgres", "mysql", "mongodb", "none"]
    default: "postgres"

  enable_auth:
    type: boolean
    description: Include authentication
    default: true

dependencies:
  - python: ">=3.9"
  - docker: "*"

files:
  - source: "app/**/*.py"
    destination: "{{service_name}}/app/"
    template: true

  - source: "Dockerfile"
    destination: "{{service_name}}/Dockerfile"
    template: true

  - source: "requirements.txt"
    destination: "{{service_name}}/requirements.txt"
    template: true

hooks:
  post_generate:
    - "cd {{service_name}} && pip install -r requirements.txt"
    - "cd {{service_name}} && docker build -t {{service_name}} ."
```

Would you like me to help you validate this template configuration?

## Validating Templates

### User:
```
Yes, please validate my template to make sure it's correctly configured
```

### Claude:
I'll validate your Python microservice template to check for any issues.

**Tool Call:** `template_validate`
```json
{
  "template": "python-microservice"
}
```

**Response:**
```json
{
  "status": "success",
  "valid": true,
  "warnings": [
    {
      "level": "info",
      "message": "Hook 'post_generate' requires pip and docker to be installed",
      "location": "template.yaml:hooks.post_generate"
    }
  ],
  "checks": {
    "schema": "valid",
    "variables": "valid",
    "files": "valid",
    "dependencies": "valid",
    "hooks": "valid"
  },
  "statistics": {
    "variables": 4,
    "files": 3,
    "hooks": 1,
    "total_size": "4.2 KB"
  }
}
```

✅ **Template is valid!**

**Validation Results:**
- ✅ Schema validation passed
- ✅ All variables properly defined
- ✅ File paths are valid
- ✅ Dependencies are correctly specified
- ✅ Hook syntax is correct

**Info:**
- Post-generate hooks require pip and docker
- Template defines 4 variables
- 3 file templates configured
- Total template size: 4.2 KB

Your template is ready to use! You can now:
```bash
ggen project gen python-microservice --vars service_name=auth-service
```

## AI-Powered Template Generation

### User:
```
Actually, can you generate a complete FastAPI microservice template for me automatically?
```

### Claude:
I'll use AI to generate a complete FastAPI microservice template based on best practices.

**Tool Call:** `ai_generate_template`
```json
{
  "description": "FastAPI microservice with authentication, database ORM, Docker, tests, and CI/CD",
  "template_type": "project",
  "provider": "anthropic"
}
```

**Response:**
```json
{
  "status": "success",
  "template_name": "fastapi-microservice-ai",
  "template_path": "/path/to/templates/fastapi-microservice-ai",
  "generated_files": [
    "template.yaml",
    "README.md",
    "files/app/main.py",
    "files/app/models.py",
    "files/app/database.py",
    "files/app/auth.py",
    "files/app/routers/users.py",
    "files/tests/test_main.py",
    "files/Dockerfile",
    "files/docker-compose.yml",
    "files/requirements.txt",
    "files/.env.example",
    "files/.github/workflows/ci.yml"
  ],
  "features": [
    "FastAPI with async support",
    "SQLAlchemy ORM with Alembic migrations",
    "JWT authentication",
    "Pydantic models for validation",
    "Docker and docker-compose setup",
    "Pytest with async support",
    "GitHub Actions CI/CD",
    "Environment variable configuration"
  ],
  "variables": [
    "service_name",
    "port",
    "database_type",
    "database_url",
    "secret_key",
    "enable_auth",
    "enable_migrations"
  ]
}
```

🎉 **AI-generated template created!**

I've created a production-ready FastAPI microservice template with:

**Features:**
- ✨ FastAPI with async/await support
- 🔐 JWT authentication
- 💾 SQLAlchemy ORM + Alembic migrations
- 🐳 Docker & docker-compose
- ✅ Pytest with async testing
- 🚀 GitHub Actions CI/CD
- ⚙️  Environment configuration

**Project Structure:**
```
fastapi-microservice-ai/
├── template.yaml
├── README.md
└── files/
    ├── app/
    │   ├── main.py
    │   ├── models.py
    │   ├── database.py
    │   ├── auth.py
    │   └── routers/
    │       └── users.py
    ├── tests/
    │   └── test_main.py
    ├── Dockerfile
    ├── docker-compose.yml
    ├── requirements.txt
    ├── .env.example
    └── .github/
        └── workflows/
            └── ci.yml
```

**Generated Code Preview (app/main.py):**
```python
from fastapi import FastAPI, Depends, HTTPException
from fastapi.security import OAuth2PasswordBearer
from sqlalchemy.orm import Session
from .database import get_db, engine
from .models import Base
from .routers import users
from . import auth

# Create database tables
Base.metadata.create_all(bind=engine)

app = FastAPI(
    title="{{service_name}}",
    description="{{description}}",
    version="1.0.0"
)

# Include routers
app.include_router(users.router, prefix="/api/v1")

@app.get("/health")
async def health_check():
    return {"status": "healthy"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port={{port}})
```

Let me validate this AI-generated template:

**Tool Call:** `template_validate`
```json
{
  "template": "fastapi-microservice-ai"
}
```

**Response:**
```json
{
  "status": "success",
  "valid": true,
  "checks": {
    "schema": "valid",
    "variables": "valid",
    "files": "valid",
    "syntax": "valid"
  }
}
```

✅ Template validated successfully!

Would you like to generate a project from this template?

## Rendering with Variables

### User:
```
Yes, generate a project called "user-service" with PostgreSQL database on port 8001
```

### Claude:
I'll generate your user-service project with the specified configuration.

**Tool Call:** `project_gen`
```json
{
  "template": "fastapi-microservice-ai",
  "vars": {
    "service_name": "user-service",
    "port": 8001,
    "database_type": "postgres",
    "database_url": "postgresql://postgres:password@db:5432/user_service",
    "enable_auth": true,
    "enable_migrations": true
  }
}
```

**Response:**
```json
{
  "status": "success",
  "output_dir": "/path/to/user-service",
  "generated_files": [
    "user-service/app/main.py",
    "user-service/app/models.py",
    "user-service/app/database.py",
    "user-service/app/auth.py",
    "user-service/app/routers/users.py",
    "user-service/tests/test_main.py",
    "user-service/Dockerfile",
    "user-service/docker-compose.yml",
    "user-service/requirements.txt",
    "user-service/.env.example",
    "user-service/.github/workflows/ci.yml",
    "user-service/alembic.ini",
    "user-service/alembic/env.py"
  ],
  "variable_substitutions": {
    "service_name": "user-service",
    "port": "8001",
    "database_type": "postgres",
    "database_url": "postgresql://postgres:password@db:5432/user_service"
  },
  "next_steps": [
    "cd user-service",
    "cp .env.example .env",
    "docker-compose up -d",
    "alembic upgrade head",
    "uvicorn app.main:app --reload --port 8001"
  ]
}
```

🚀 **Project generated successfully!**

Your FastAPI microservice is ready at `/path/to/user-service`

**Configuration:**
- 📦 Service name: user-service
- 🔌 Port: 8001
- 💾 Database: PostgreSQL
- 🔐 Authentication: Enabled
- 📝 Migrations: Enabled (Alembic)

**Get Started:**
```bash
cd user-service
cp .env.example .env
docker-compose up -d
alembic upgrade head
uvicorn app.main:app --reload --port 8001
```

**API Endpoints:**
- `GET /health` - Health check
- `POST /api/v1/users/register` - User registration
- `POST /api/v1/users/login` - User login
- `GET /api/v1/users/me` - Get current user

**Files Rendered:**
All template variables like `{{service_name}}` and `{{port}}` have been replaced with your values throughout:
- ✅ app/main.py - Service runs on port 8001
- ✅ docker-compose.yml - PostgreSQL configured
- ✅ alembic.ini - Database URL set
- ✅ requirements.txt - All dependencies included

## Improving Existing Templates

### User:
```
Can AI help improve my existing template with better error handling?
```

### Claude:
I'll use AI to analyze and improve your template with enhanced error handling.

**Tool Call:** `ai_validate_and_improve`
```json
{
  "content": "python-microservice",
  "content_type": "template",
  "provider": "anthropic"
}
```

**Response:**
```json
{
  "status": "success",
  "validation": {
    "issues_found": [
      "Missing error handling in database operations",
      "No logging configuration",
      "API endpoints lack input validation",
      "No rate limiting",
      "Missing health check endpoint"
    ],
    "security_concerns": [
      "Plain text database credentials in docker-compose.yml",
      "Missing CORS configuration",
      "No request timeout settings"
    ]
  },
  "improvements": {
    "files_modified": [
      "app/main.py",
      "app/database.py",
      "app/models.py",
      "docker-compose.yml",
      ".env.example"
    ],
    "changes": [
      "Added comprehensive error handling with custom exceptions",
      "Integrated structured logging with loguru",
      "Added Pydantic validators for all inputs",
      "Implemented rate limiting with slowapi",
      "Added detailed health check with dependency status",
      "Moved credentials to environment variables",
      "Added CORS middleware configuration",
      "Added request timeout settings"
    ]
  },
  "diff_preview": "...",
  "recommendation": "Template improved with production-ready error handling and security"
}
```

✅ **Template improved with AI assistance!**

**Issues Fixed:**
- ❌ Missing error handling → ✅ Comprehensive exception handling
- ❌ No logging → ✅ Structured logging with loguru
- ❌ Weak validation → ✅ Pydantic validators
- ❌ No rate limiting → ✅ slowapi integration
- ❌ Plain text secrets → ✅ Environment variables

**Security Enhancements:**
- 🔒 Environment-based configuration
- 🔒 CORS properly configured
- 🔒 Request timeouts added
- 🔒 Health checks with dependency monitoring

**Code Example (Enhanced error handling):**
```python
from fastapi import HTTPException, Request
from fastapi.responses import JSONResponse
from loguru import logger

class DatabaseError(Exception):
    """Custom database exception"""
    pass

@app.exception_handler(DatabaseError)
async def database_exception_handler(request: Request, exc: DatabaseError):
    logger.error(f"Database error: {exc}")
    return JSONResponse(
        status_code=500,
        content={"detail": "Database operation failed", "type": "database_error"}
    )

@app.middleware("http")
async def log_requests(request: Request, call_next):
    logger.info(f"Request: {request.method} {request.url}")
    try:
        response = await call_next(request)
        logger.info(f"Response: {response.status_code}")
        return response
    except Exception as e:
        logger.error(f"Request failed: {e}")
        raise
```

The improved template is now production-ready with proper error handling, logging, and security measures!

## Key Takeaways

1. **Create:** Use `template_create` for custom templates
2. **Validate:** Use `template_validate` to check configuration
3. **AI Generate:** Use `ai_generate_template` for automatic creation
4. **Render:** Use `project_gen` with variables to render templates
5. **Improve:** Use `ai_validate_and_improve` to enhance templates

## Related Tools

- `market_search` - Find existing templates to study
- `market_install` - Install templates as starting points
- `ai_generate_project` - Generate complete projects
- `graph_query` - Query template metadata
