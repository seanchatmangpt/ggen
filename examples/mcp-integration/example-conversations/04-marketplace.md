# Example 4: Marketplace Search and Installation

This example demonstrates discovering, evaluating, and installing templates from the ggen marketplace.

## Browsing the Marketplace

### User:
```
What kind of templates are available in the marketplace?
```

### Claude:
Let me show you what's available in the ggen marketplace by category.

**Tool Call:** `market_list`
```json
{
  "category": null,
  "tag": null
}
```

**Response:**
```json
{
  "status": "success",
  "categories": [
    {
      "name": "web",
      "count": 45,
      "description": "Web applications and frontends",
      "popular_tags": ["react", "vue", "angular", "nextjs"]
    },
    {
      "name": "api",
      "count": 32,
      "description": "REST and GraphQL APIs",
      "popular_tags": ["fastapi", "express", "flask", "graphql"]
    },
    {
      "name": "cli",
      "count": 18,
      "description": "Command-line tools",
      "popular_tags": ["rust", "python", "go", "typescript"]
    },
    {
      "name": "library",
      "count": 24,
      "description": "Reusable libraries and packages",
      "popular_tags": ["npm", "pypi", "cargo", "components"]
    },
    {
      "name": "infrastructure",
      "count": 15,
      "description": "Docker, K8s, and DevOps",
      "popular_tags": ["docker", "kubernetes", "terraform", "cicd"]
    }
  ],
  "total_packages": 134,
  "featured": [
    "react-typescript-starter",
    "fastapi-microservice",
    "rust-cli-boilerplate"
  ]
}
```

**📦 GGen Marketplace Overview**

**Total Templates:** 134 across 5 categories

**Categories:**
1. 🌐 **Web** (45 templates)
   - React, Vue, Angular, Next.js

2. 🔌 **API** (32 templates)
   - FastAPI, Express, Flask, GraphQL

3. 💻 **CLI** (18 templates)
   - Rust, Python, Go, TypeScript

4. 📚 **Library** (24 templates)
   - NPM, PyPI, Cargo, Components

5. ⚙️ **Infrastructure** (15 templates)
   - Docker, Kubernetes, Terraform, CI/CD

**⭐ Featured Templates:**
- react-typescript-starter
- fastapi-microservice
- rust-cli-boilerplate

What category interests you?

## Searching for Specific Templates

### User:
```
I need a template for building a REST API with authentication and database integration
```

### Claude:
Let me search the marketplace for REST API templates with auth and database support.

**Tool Call:** `market_search`
```json
{
  "query": "REST API authentication database",
  "category": "api",
  "limit": 10
}
```

**Response:**
```json
{
  "status": "success",
  "results": [
    {
      "id": "fastapi-auth-db",
      "name": "FastAPI Auth & DB",
      "version": "2.1.0",
      "description": "FastAPI with JWT auth, SQLAlchemy ORM, and PostgreSQL",
      "author": "fastapi-community",
      "downloads": 12453,
      "rating": 4.8,
      "stars": 245,
      "tags": ["fastapi", "jwt", "sqlalchemy", "postgres", "auth"],
      "relevance_score": 0.95
    },
    {
      "id": "express-auth-api",
      "name": "Express Auth API",
      "version": "1.5.2",
      "description": "Express.js with Passport.js auth and Prisma ORM",
      "author": "nodejs-templates",
      "downloads": 9821,
      "rating": 4.6,
      "stars": 189,
      "tags": ["express", "passport", "prisma", "auth", "typescript"],
      "relevance_score": 0.89
    },
    {
      "id": "flask-rest-auth",
      "name": "Flask REST with Auth",
      "version": "1.3.1",
      "description": "Flask-RESTX with JWT auth and Flask-SQLAlchemy",
      "author": "flask-templates",
      "downloads": 7234,
      "rating": 4.5,
      "stars": 156,
      "tags": ["flask", "jwt", "sqlalchemy", "rest", "python"],
      "relevance_score": 0.85
    }
  ],
  "total_results": 8,
  "search_time_ms": 45
}
```

Found **3 highly relevant** REST API templates with auth and database:

**1. 🥇 FastAPI Auth & DB** (95% match)
- ⭐ 4.8/5.0 (12.5K downloads)
- JWT authentication
- SQLAlchemy ORM + PostgreSQL
- Production-ready

**2. 🥈 Express Auth API** (89% match)
- ⭐ 4.6/5.0 (9.8K downloads)
- Passport.js authentication
- Prisma ORM
- TypeScript support

**3. 🥉 Flask REST with Auth** (85% match)
- ⭐ 4.5/5.0 (7.2K downloads)
- JWT authentication
- Flask-SQLAlchemy
- Python/Flask stack

Would you like detailed information about any of these?

## Getting Package Details

### User:
```
Tell me more about the FastAPI Auth & DB template
```

### Claude:
Let me get detailed information about the FastAPI Auth & DB template.

**Tool Call:** `market_info`
```json
{
  "package_id": "fastapi-auth-db"
}
```

**Response:**
```json
{
  "status": "success",
  "package": {
    "id": "fastapi-auth-db",
    "name": "FastAPI Auth & DB",
    "version": "2.1.0",
    "description": "Production-ready FastAPI template with JWT authentication, SQLAlchemy ORM, and PostgreSQL integration",
    "author": {
      "name": "fastapi-community",
      "email": "maintainer@fastapi.dev",
      "url": "https://github.com/fastapi-community"
    },
    "license": "MIT",
    "repository": "https://github.com/fastapi-community/auth-db-template",
    "homepage": "https://fastapi-auth-db.readthedocs.io",

    "stats": {
      "downloads": 12453,
      "downloads_last_month": 1832,
      "rating": 4.8,
      "reviews": 127,
      "stars": 245,
      "forks": 89,
      "watchers": 34
    },

    "features": [
      "JWT access & refresh tokens",
      "Password hashing with bcrypt",
      "SQLAlchemy 2.0 with async support",
      "Alembic database migrations",
      "Role-based access control (RBAC)",
      "Email verification",
      "Password reset functionality",
      "Rate limiting",
      "CORS configuration",
      "Comprehensive test suite",
      "Docker & docker-compose setup",
      "OpenAPI documentation",
      "CI/CD with GitHub Actions"
    ],

    "dependencies": {
      "python": ">=3.9",
      "fastapi": ">=0.104.0",
      "sqlalchemy": ">=2.0.0",
      "alembic": ">=1.12.0",
      "pydantic": ">=2.0.0",
      "python-jose": ">=3.3.0",
      "passlib": ">=1.7.4",
      "psycopg2-binary": ">=2.9.0"
    },

    "variables": {
      "project_name": {
        "type": "string",
        "description": "Name of your API project",
        "required": true,
        "pattern": "^[a-z][a-z0-9-]*$"
      },
      "database_name": {
        "type": "string",
        "description": "PostgreSQL database name",
        "default": "{{project_name}}_db"
      },
      "secret_key": {
        "type": "string",
        "description": "JWT secret key",
        "required": true,
        "sensitive": true
      },
      "enable_email_verification": {
        "type": "boolean",
        "description": "Enable email verification flow",
        "default": true
      },
      "admin_email": {
        "type": "string",
        "description": "Default admin user email",
        "format": "email"
      }
    },

    "examples": [
      {
        "title": "Basic User Management API",
        "description": "Simple user authentication and CRUD",
        "vars": {
          "project_name": "user-api",
          "enable_email_verification": false
        }
      },
      {
        "title": "Multi-tenant SaaS Backend",
        "description": "Authentication with organization support",
        "vars": {
          "project_name": "saas-backend",
          "enable_email_verification": true,
          "enable_organizations": true
        }
      }
    ],

    "health": {
      "status": "healthy",
      "last_updated": "2024-01-15",
      "maintenance_score": 95,
      "test_coverage": 92,
      "documentation_quality": "excellent",
      "issues_open": 3,
      "issues_closed": 145
    },

    "compatibility": [
      "PostgreSQL 12+",
      "Docker 20+",
      "Python 3.9+",
      "Works with: redis-session-store, celery-worker"
    ]
  }
}
```

## 📦 **FastAPI Auth & DB** - Detailed Information

### Overview
Production-ready FastAPI template with comprehensive authentication and database integration.

**Version:** 2.1.0 | **License:** MIT
**Author:** fastapi-community
**Repository:** [GitHub](https://github.com/fastapi-community/auth-db-template)

### 📊 Statistics
- 📥 **12,453** total downloads (1,832 last month)
- ⭐ **4.8/5.0** (127 reviews)
- 🌟 **245** GitHub stars
- 🔱 **89** forks

### ✨ Features
- ✅ JWT access & refresh tokens
- ✅ Password hashing with bcrypt
- ✅ SQLAlchemy 2.0 async support
- ✅ Alembic database migrations
- ✅ Role-based access control (RBAC)
- ✅ Email verification
- ✅ Password reset functionality
- ✅ Rate limiting
- ✅ CORS configuration
- ✅ Comprehensive test suite (92% coverage)
- ✅ Docker & docker-compose
- ✅ OpenAPI documentation
- ✅ GitHub Actions CI/CD

### 🔧 Requirements
- Python >=3.9
- PostgreSQL 12+
- Docker 20+

### 📝 Configuration Variables
Required:
- `project_name` - API project name
- `secret_key` - JWT secret (sensitive)

Optional:
- `database_name` - Database name (default: project_name_db)
- `enable_email_verification` - Email verification (default: true)
- `admin_email` - Admin user email

### 💡 Example Use Cases

**1. Basic User Management API:**
```bash
ggen project gen fastapi-auth-db \
  --vars project_name=user-api \
  --vars enable_email_verification=false
```

**2. Multi-tenant SaaS Backend:**
```bash
ggen project gen fastapi-auth-db \
  --vars project_name=saas-backend \
  --vars enable_email_verification=true \
  --vars enable_organizations=true
```

### 🏥 Health Score: 95/100
- ✅ Recently updated (2024-01-15)
- ✅ 92% test coverage
- ✅ Excellent documentation
- ✅ Active maintenance (3 open, 145 closed issues)

### 🔗 Compatible With
- redis-session-store
- celery-worker
- PostgreSQL extensions

Would you like to install this template?

## Installing a Template

### User:
```
Yes, install the FastAPI Auth & DB template
```

### Claude:
I'll install the FastAPI Auth & DB template for you.

**Tool Call:** `market_install`
```json
{
  "package": "fastapi-auth-db",
  "version": "2.1.0"
}
```

**Response:**
```json
{
  "status": "success",
  "installed": {
    "id": "fastapi-auth-db",
    "name": "FastAPI Auth & DB",
    "version": "2.1.0",
    "location": "/home/user/.ggen/templates/fastapi-auth-db",
    "size": "2.3 MB"
  },
  "files_installed": 47,
  "dependencies_resolved": [
    "fastapi-base@1.0.0",
    "sqlalchemy-helpers@0.5.2"
  ],
  "message": "Template installed successfully. Use 'ggen project gen fastapi-auth-db' to create a project."
}
```

✅ **Template installed successfully!**

**Installed:**
- 📦 Package: FastAPI Auth & DB v2.1.0
- 📁 Location: `~/.ggen/templates/fastapi-auth-db`
- 💾 Size: 2.3 MB
- 📄 Files: 47

**Dependencies:**
- fastapi-base@1.0.0
- sqlalchemy-helpers@0.5.2

**Usage:**
```bash
# Create a new project
ggen project gen fastapi-auth-db \
  --vars project_name=my-api \
  --vars secret_key=your-secret-key-here
```

Ready to generate a project from this template!

## Getting Recommendations

### User:
```
What other templates would work well with this API backend?
```

### Claude:
Let me get personalized recommendations based on the FastAPI Auth & DB template.

**Tool Call:** `market_recommend`
```json
{
  "based_on": "fastapi-auth-db",
  "category": null,
  "limit": 5
}
```

**Response:**
```json
{
  "status": "success",
  "recommendations": [
    {
      "id": "react-admin-dashboard",
      "name": "React Admin Dashboard",
      "reason": "Perfect frontend for FastAPI backend",
      "compatibility_score": 0.95,
      "features": ["JWT auth integration", "API client", "CRUD operations"],
      "downloads": 8234,
      "rating": 4.7
    },
    {
      "id": "celery-worker-template",
      "name": "Celery Worker Template",
      "reason": "Add background job processing",
      "compatibility_score": 0.92,
      "features": ["Task queue", "Scheduled jobs", "Redis integration"],
      "downloads": 5621,
      "rating": 4.6
    },
    {
      "id": "pytest-api-testing",
      "name": "Pytest API Testing",
      "reason": "Comprehensive API test suite",
      "compatibility_score": 0.89,
      "features": ["Integration tests", "Fixtures", "Coverage reports"],
      "downloads": 4832,
      "rating": 4.5
    },
    {
      "id": "redis-caching-layer",
      "name": "Redis Caching Layer",
      "reason": "Improve API performance",
      "compatibility_score": 0.87,
      "features": ["Response caching", "Session storage", "Rate limiting"],
      "downloads": 6102,
      "rating": 4.6
    },
    {
      "id": "prometheus-monitoring",
      "name": "Prometheus Monitoring",
      "reason": "Production monitoring and metrics",
      "compatibility_score": 0.85,
      "features": ["Metrics collection", "Grafana dashboards", "Alerts"],
      "downloads": 3921,
      "rating": 4.4
    }
  ]
}
```

**🎯 Recommended Templates** (based on fastapi-auth-db):

**1. React Admin Dashboard** (95% compatible)
- ⭐ 4.7 | 8.2K downloads
- Perfect frontend for your FastAPI backend
- Features: JWT auth integration, API client, CRUD operations

**2. Celery Worker Template** (92% compatible)
- ⭐ 4.6 | 5.6K downloads
- Add background job processing
- Features: Task queue, scheduled jobs, Redis integration

**3. Pytest API Testing** (89% compatible)
- ⭐ 4.5 | 4.8K downloads
- Comprehensive API test suite
- Features: Integration tests, fixtures, coverage reports

**4. Redis Caching Layer** (87% compatible)
- ⭐ 4.6 | 6.1K downloads
- Improve API performance
- Features: Response caching, session storage, rate limiting

**5. Prometheus Monitoring** (85% compatible)
- ⭐ 4.4 | 3.9K downloads
- Production monitoring and metrics
- Features: Metrics collection, Grafana dashboards, alerts

These templates integrate seamlessly with your FastAPI backend. Would you like to install any of these?

## Offline Search

### User:
```
I'm working offline. Can I still search the marketplace?
```

### Claude:
Yes! Let me search the cached marketplace data for you.

**Tool Call:** `market_offline_search`
```json
{
  "query": "docker kubernetes",
  "category": "infrastructure",
  "limit": 5
}
```

**Response:**
```json
{
  "status": "success",
  "source": "offline_cache",
  "cache_age_hours": 8,
  "results": [
    {
      "id": "k8s-microservice-deploy",
      "name": "Kubernetes Microservice Deploy",
      "version": "1.4.0",
      "description": "Complete K8s manifests for microservice deployment",
      "cached_at": "2024-01-10T08:00:00Z",
      "downloads": 4521,
      "rating": 4.7
    },
    {
      "id": "docker-compose-stack",
      "name": "Docker Compose Stack",
      "version": "2.0.1",
      "description": "Multi-service docker-compose configurations",
      "cached_at": "2024-01-10T08:00:00Z",
      "downloads": 7834,
      "rating": 4.6
    }
  ],
  "total_cached_packages": 98,
  "cache_completeness": "87%"
}
```

✅ **Offline search successful!**

**Cache Info:**
- 🕒 Last updated: 8 hours ago
- 📦 Cached packages: 98
- 📊 Completeness: 87%

**Found 2 templates:**

1. **Kubernetes Microservice Deploy**
   - Version: 1.4.0
   - K8s manifests for microservice deployment
   - ⭐ 4.7 | 4.5K downloads

2. **Docker Compose Stack**
   - Version: 2.0.1
   - Multi-service docker-compose configurations
   - ⭐ 4.6 | 7.8K downloads

Note: Results from cache (8h old). For latest data, connect to internet and run `market_sync`.

## Syncing Marketplace Cache

### User:
```
Update my marketplace cache with the latest data
```

### Claude:
I'll synchronize your local marketplace cache with the remote server.

**Tool Call:** `market_sync`
```json
{
  "category": null,
  "force": false
}
```

**Response:**
```json
{
  "status": "success",
  "sync_details": {
    "started_at": "2024-01-10T16:30:00Z",
    "completed_at": "2024-01-10T16:30:45Z",
    "duration_seconds": 45
  },
  "changes": {
    "new_packages": 12,
    "updated_packages": 34,
    "deprecated_packages": 2,
    "total_packages": 142
  },
  "cache_size": {
    "before": "45.2 MB",
    "after": "48.1 MB",
    "growth": "2.9 MB"
  },
  "categories_synced": ["web", "api", "cli", "library", "infrastructure"],
  "next_auto_sync": "2024-01-11T16:30:00Z"
}
```

✅ **Marketplace cache synchronized!**

**Sync Summary:**
- ⏱️ Duration: 45 seconds
- ✨ 12 new packages
- 🔄 34 updated packages
- ⚠️ 2 deprecated packages
- 📦 Total: 142 packages

**Cache Size:**
- Before: 45.2 MB
- After: 48.1 MB
- Growth: +2.9 MB

**Next auto-sync:** Tomorrow at 16:30

Your local cache is now up to date! You can work offline with current marketplace data.

## Checking Cache Status

### User:
```
Show me my marketplace cache status
```

### Claude:
Let me check your marketplace cache status.

**Tool Call:** `market_cache_status`
```json
{}
```

**Response:**
```json
{
  "status": "success",
  "cache": {
    "location": "/home/user/.ggen/cache/marketplace",
    "size": "48.1 MB",
    "packages": 142,
    "last_sync": "2024-01-10T16:30:45Z",
    "age_hours": 2,
    "health": "healthy"
  },
  "by_category": {
    "web": {
      "packages": 47,
      "size": "18.5 MB",
      "last_updated": 34
    },
    "api": {
      "packages": 35,
      "size": "14.2 MB",
      "last_updated": 28
    },
    "cli": {
      "packages": 20,
      "size": "6.8 MB",
      "last_updated": 15
    },
    "library": {
      "packages": 25,
      "size": "5.9 MB",
      "last_updated": 19
    },
    "infrastructure": {
      "packages": 15,
      "size": "2.7 MB",
      "last_updated": 12
    }
  },
  "statistics": {
    "most_popular_category": "web",
    "newest_package": "react-hooks-library",
    "most_downloaded": "fastapi-auth-db",
    "cache_hit_rate": "94.2%",
    "offline_searches": 127
  }
}
```

**📦 Marketplace Cache Status**

**Overall:**
- 📁 Location: `~/.ggen/cache/marketplace`
- 💾 Size: 48.1 MB
- 📦 Packages: 142
- 🕒 Last sync: 2 hours ago
- ✅ Health: Healthy

**By Category:**

| Category | Packages | Size | Recent Updates |
|----------|----------|------|----------------|
| 🌐 Web | 47 | 18.5 MB | 34 |
| 🔌 API | 35 | 14.2 MB | 28 |
| 💻 CLI | 20 | 6.8 MB | 15 |
| 📚 Library | 25 | 5.9 MB | 19 |
| ⚙️ Infrastructure | 15 | 2.7 MB | 12 |

**Statistics:**
- Most popular: Web templates
- Newest: react-hooks-library
- Most downloaded: fastapi-auth-db
- Cache hit rate: 94.2%
- Offline searches: 127

Your cache is healthy and up to date!

## Key Takeaways

1. **Browse:** Use `market_list` to explore categories
2. **Search:** Use `market_search` for specific needs
3. **Evaluate:** Use `market_info` for detailed package information
4. **Install:** Use `market_install` to add templates locally
5. **Discover:** Use `market_recommend` for suggestions
6. **Offline:** Use `market_offline_search` when disconnected
7. **Maintain:** Use `market_sync` to update cache
8. **Monitor:** Use `market_cache_status` to check cache health

## Related Tools

- `project_gen` - Generate projects from installed templates
- `graph_query` - Query marketplace metadata
- `ai_generate_project` - AI-assisted project creation
- `template_validate` - Validate before installation
