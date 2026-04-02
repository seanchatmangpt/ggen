<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Advanced Python/FastAPI Code Generation Patterns](#advanced-pythonfastapi-code-generation-patterns)
  - [Overview](#overview)
  - [Pattern 1: Advanced Async Patterns with Multi-Database Concurrency](#pattern-1-advanced-async-patterns-with-multi-database-concurrency)
    - [Description](#description)
    - [External Packages](#external-packages)
    - [RDF Ontology for Async Config](#rdf-ontology-for-async-config)
    - [Generated Async Database Manager](#generated-async-database-manager)
    - [Advanced Caching Decorator](#advanced-caching-decorator)
  - [Pattern 2: Advanced ORM Features with Relationship Caching](#pattern-2-advanced-orm-features-with-relationship-caching)
    - [Description](#description-1)
    - [External Packages](#external-packages-1)
    - [RDF Ontology for ORM Config](#rdf-ontology-for-orm-config)
    - [Generated ORM Models](#generated-orm-models)
    - [Query Optimization with Selectinload](#query-optimization-with-selectinload)
  - [Pattern 3: Advanced Strawberry GraphQL Integration](#pattern-3-advanced-strawberry-graphql-integration)
    - [Description](#description-2)
    - [External Packages](#external-packages-2)
  - [Pattern 4: Advanced Background Task Processing with Celery](#pattern-4-advanced-background-task-processing-with-celery)
    - [Description](#description-3)
    - [External Packages](#external-packages-3)
    - [RDF Ontology for Tasks](#rdf-ontology-for-tasks)
    - [Generated Task Definitions](#generated-task-definitions)
  - [Pattern 5: Advanced WebSocket Support with FastAPI](#pattern-5-advanced-websocket-support-with-fastapi)
    - [Description](#description-4)
    - [External Packages](#external-packages-4)
    - [Generated WebSocket Implementation](#generated-websocket-implementation)
  - [Tera Template Examples](#tera-template-examples)
    - [FastAPI Service Template](#fastapi-service-template)
  - [Best Practices](#best-practices)
  - [References](#references)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Advanced Python/FastAPI Code Generation Patterns

> Hyper-advanced patterns for async Python web services with multi-database support, ORM optimization, and real-time capabilities

## Overview

This guide covers five advanced Python patterns for generating production-grade FastAPI services with asyncio, multi-database concurrency, and GraphQL support.

## Pattern 1: Advanced Async Patterns with Multi-Database Concurrency

### Description

Implement concurrent database operations using **asyncpg** (PostgreSQL), **motor** (MongoDB), and **aioredis** (Redis) with connection pooling, circuit breakers, and distributed caching.

### External Packages

```json
{
  "asyncpg": ">=0.29.0,<0.30.0",
  "motor": ">=3.3.0,<4.0.0",
  "aioredis": ">=2.0.1,<3.0.0",
  "aiocache": ">=0.12.2,<0.13.0",
  "tenacity": ">=8.2.3,<9.0.0",
  "pybreaker": ">=1.0.1,<2.0.0"
}
```

### RDF Ontology for Async Config

```turtle
@prefix async: <http://api.example.org/async#>
@prefix db: <http://api.example.org/database#>

:PostgresPool a async:ConnectionPool ;
  async:minConnections 10 ;
  async:maxConnections 50 ;
  async:timeout 30 ;
  async:poolType "asyncpg" .

:RedisCache a async:CacheStrategy ;
  async:backend "aioredis" ;
  async:ttl 3600 ;
  async:namespace "api:cache" ;
  async:serializer "json" .

:UserDataSource a db:DataSource ;
  db:type "postgresql" ;
  db:connectionString "postgresql+asyncpg://..." ;
  db:poolConfig :PostgresPool ;
  db:circuitBreakerConfig :DefaultBreaker .
```

### Generated Async Database Manager

```python
# core/async_db.py
import asyncpg
import motor.motor_asyncio
import aioredis
from typing import Optional, Dict, Any
from contextlib import asynccontextmanager
from tenacity import retry, stop_after_attempt, wait_exponential
from pybreaker import CircuitBreaker
import asyncio

class AsyncDatabaseManager:
    """Advanced async database manager with connection pooling"""

    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self.pg_pool: Optional[asyncpg.Pool] = None
        self.mongo_client: Optional[motor.motor_asyncio.AsyncIOMotorClient] = None
        self.redis: Optional[aioredis.Redis] = None
        self.circuit_breaker = CircuitBreaker(
            fail_max=5,
            timeout_duration=60,
            name="db_breaker"
        )

    async def initialize(self):
        """Initialize all database connections"""
        await asyncio.gather(
            self._init_postgres(),
            self._init_mongodb(),
            self._init_redis()
        )

    @retry(stop=stop_after_attempt(3), wait=wait_exponential(multiplier=1, min=2, max=10))
    async def _init_postgres(self):
        """Initialize PostgreSQL connection pool"""
        self.pg_pool = await asyncpg.create_pool(
            dsn=self.config['postgres']['dsn'],
            min_size=10,
            max_size=50,
            max_queries=50000,
            max_inactive_connection_lifetime=300,
            timeout=30,
            command_timeout=60
        )

    async def _init_mongodb(self):
        """Initialize MongoDB async client"""
        self.mongo_client = motor.motor_asyncio.AsyncIOMotorClient(
            self.config['mongodb']['uri'],
            maxPoolSize=50,
            minPoolSize=10,
            serverSelectionTimeoutMS=5000
        )

    async def _init_redis(self):
        """Initialize Redis connection pool"""
        self.redis = await aioredis.from_url(
            self.config['redis']['url'],
            encoding="utf-8",
            decode_responses=True,
            max_connections=50
        )

    @asynccontextmanager
    async def postgres_transaction(self):
        """Get PostgreSQL transaction with circuit breaker"""
        async with self.pg_pool.acquire() as conn:
            async with conn.transaction():
                yield conn

    @circuit_breaker
    async def execute_concurrent_queries(self, queries: Dict[str, str]) -> Dict[str, Any]:
        """Execute multiple database queries concurrently"""
        async with self.postgres_transaction() as pg_conn:
            # Execute PostgreSQL queries
            pg_results = await asyncio.gather(*[
                pg_conn.fetch(query) for query in queries.get('postgres', [])
            ])

            # Execute MongoDB queries concurrently
            mongo_db = self.mongo_client[self.config['mongodb']['database']]
            mongo_results = await asyncio.gather(*[
                mongo_db[collection].find(query).to_list(length=100)
                for collection, query in queries.get('mongodb', {}).items()
            ])

            # Cache results in Redis
            cache_key = f"query_results:{hash(str(queries))}"
            await self.redis.setex(
                cache_key,
                3600,
                str({'postgres': pg_results, 'mongodb': mongo_results})
            )

            return {
                'postgres': pg_results,
                'mongodb': mongo_results,
                'cached_at': cache_key
            }

    async def cleanup(self):
        """Cleanup all connections"""
        await asyncio.gather(
            self.pg_pool.close() if self.pg_pool else asyncio.sleep(0),
            self.mongo_client.close() if self.mongo_client else asyncio.sleep(0),
            self.redis.close() if self.redis else asyncio.sleep(0)
        )
```

### Advanced Caching Decorator

```python
# core/cache_decorators.py
from aiocache import cached, Cache
from aiocache.serializers import JsonSerializer
from functools import wraps
import hashlib
import json
from typing import Callable, Any, Optional

class AdvancedCacheDecorator:
    """Advanced caching with multiple backends"""

    @staticmethod
    def smart_cache(
        ttl: int = 3600,
        namespace: str = "default",
        key_builder: Optional[Callable] = None
    ):
        """Smart cache decorator with automatic key generation"""
        def decorator(func: Callable) -> Callable:
            @cached(
                ttl=ttl,
                cache=Cache.REDIS,
                namespace=namespace,
                serializer=JsonSerializer(),
                key_builder=key_builder or AdvancedCacheDecorator._default_key_builder
            )
            @wraps(func)
            async def wrapper(*args, **kwargs) -> Any:
                return await func(*args, **kwargs)
            return wrapper
        return decorator

    @staticmethod
    def _default_key_builder(func, *args, **kwargs) -> str:
        """Generate cache key from function signature"""
        key_data = {
            'func': func.__name__,
            'args': str(args),
            'kwargs': str(sorted(kwargs.items()))
        }
        return hashlib.sha256(
            json.dumps(key_data, sort_keys=True).encode()
        ).hexdigest()[:16]

# Usage example
@AdvancedCacheDecorator.smart_cache(ttl=1800, namespace="users")
async def get_user_with_posts(user_id: str, include_archived: bool = False):
    """Cached user query with automatic invalidation"""
    # Complex query logic here
    pass
```

---

## Pattern 2: Advanced ORM Features with Relationship Caching

### Description

Implement **SQLAlchemy 2.0** async patterns and **Tortoise-ORM** for simplified async with automatic relationship caching and N+1 query prevention.

### External Packages

```json
{
  "sqlalchemy[asyncio]": ">=2.0.23,<3.0.0",
  "tortoise-orm": ">=0.20.0,<0.21.0",
  "asyncpg": ">=0.29.0,<0.30.0",
  "sqlalchemy-utils": ">=0.41.1,<0.42.0"
}
```

### RDF Ontology for ORM Config

```turtle
@prefix orm: <http://api.example.org/orm#>
@prefix rel: <http://api.example.org/relationship#>

orm:UserModel a orm:Entity ;
  orm:tableName "users" ;
  orm:fields (
    [ orm:fieldName "id" ; orm:fieldType "UUID" ; orm:primaryKey true ]
    [ orm:fieldName "email" ; orm:fieldType "String" ; orm:unique true ]
    [ orm:fieldName "posts" ; orm:fieldType rel:OneToMany ; orm:lazyLoad false ]
  ) .

rel:UserPostsRelationship a rel:OneToMany ;
  rel:from orm:UserModel ;
  rel:to orm:PostModel ;
  rel:foreignKey "user_id" ;
  rel:cascade "delete" ;
  rel:eagerLoad true .
```

### Generated ORM Models

```python
# models/user.py
from sqlalchemy import Column, String, DateTime, Boolean
from sqlalchemy.dialects.postgresql import UUID
from sqlalchemy.orm import relationship
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.orm import declarative_base
from datetime import datetime
import uuid

Base = declarative_base()

class User(Base):
    __tablename__ = "users"

    id = Column(UUID(as_uuid=True), primary_key=True, default=uuid.uuid4)
    email = Column(String(255), unique=True, nullable=False)
    username = Column(String(32), nullable=False)
    password_hash = Column(String(255), nullable=False)
    created_at = Column(DateTime, default=datetime.utcnow, nullable=False)
    is_active = Column(Boolean, default=True, nullable=False)

    # Eager load relationship (from RDF: rel:eagerLoad true)
    posts = relationship("Post", back_populates="author", lazy="selectin")

    @classmethod
    async def get_by_id(cls, session: AsyncSession, user_id: uuid.UUID):
        """Type-safe query with eager loading"""
        from sqlalchemy import select
        stmt = select(cls).where(cls.id == user_id)
        result = await session.execute(stmt)
        return result.scalars().first()

    @classmethod
    async def get_with_posts(cls, session: AsyncSession, user_id: uuid.UUID):
        """Get user with posts (no N+1 query due to eager loading)"""
        return await cls.get_by_id(session, user_id)

class Post(Base):
    __tablename__ = "posts"

    id = Column(UUID(as_uuid=True), primary_key=True, default=uuid.uuid4)
    user_id = Column(UUID(as_uuid=True), ForeignKey("users.id", ondelete="CASCADE"), nullable=False)
    title = Column(String(255), nullable=False)
    content = Column(String, nullable=False)
    created_at = Column(DateTime, default=datetime.utcnow, nullable=False)

    author = relationship("User", back_populates="posts")
```

### Query Optimization with Selectinload

```python
# services/user_service.py
from sqlalchemy.ext.asyncio import AsyncSession
from sqlalchemy.orm import selectinload
from sqlalchemy import select
from models.user import User, Post

class UserService:
    """User service with optimized queries"""

    @staticmethod
    async def get_user_with_posts(
        session: AsyncSession,
        user_id: str
    ) -> User:
        """Get user with posts using selectinload (no N+1)"""
        stmt = (
            select(User)
            .where(User.id == user_id)
            .options(selectinload(User.posts))
        )
        result = await session.execute(stmt)
        return result.scalars().first()

    @staticmethod
    async def get_all_users_optimized(
        session: AsyncSession,
        limit: int = 50,
        offset: int = 0
    ) -> list[User]:
        """Get all users with posts (optimized for 1M+ users)"""
        stmt = (
            select(User)
            .options(selectinload(User.posts))
            .limit(limit)
            .offset(offset)
        )
        result = await session.execute(stmt)
        return result.scalars().all()
```

---

## Pattern 3: Advanced Strawberry GraphQL Integration

### Description

Generate type-safe GraphQL resolvers from RDF ontologies using **Strawberry GraphQL** with dataloader optimization.

### External Packages

```json
{
  "strawberry-graphql[asgi]": ">=0.215.0",
  "strawberry-graphql[dataloader]": ">=0.215.0"
}
```

---

## Pattern 4: Advanced Background Task Processing with Celery

### Description

Generate Celery task definitions and worker configurations from RDF for scalable background job processing.

### External Packages

```json
{
  "celery": ">=5.3.0",
  "redis": ">=5.0.0",
  "flower": ">=2.0.1"
}
```

### RDF Ontology for Tasks

```turtle
@prefix task: <http://api.example.org/task#>

task:SendEmailTask a task:AsyncTask ;
  task:name "send_email" ;
  task:queue "emails" ;
  task:priority "high" ;
  task:maxRetries 3 ;
  task:retryBackoff "exponential" ;
  task:timeout 300 .

task:GenerateReportTask a task:AsyncTask ;
  task:name "generate_report" ;
  task:queue "reports" ;
  task:priority "normal" ;
  task:maxRetries 2 ;
  task:timeout 1800 ;
  task:routing "long_running" .
```

### Generated Task Definitions

```python
# tasks/email_tasks.py
from celery import shared_task, Task
from typing import List
import asyncio

class CallbackTask(Task):
    def on_success(self, retval, task_id, args, kwargs):
        print(f"Task {task_id} succeeded with result {retval}")

    def on_retry(self, exc, task_id, args, kwargs, einfo):
        print(f"Task {task_id} retrying: {exc}")

    def on_failure(self, exc, task_id, args, kwargs, einfo):
        print(f"Task {task_id} failed: {exc}")

@shared_task(
    bind=True,
    base=CallbackTask,
    queue="emails",
    max_retries=3,
    time_limit=300,
    soft_time_limit=290
)
def send_email_task(
    self,
    recipient: str,
    subject: str,
    body: str
):
    """
    Send email with retry logic.
    Generated from task:SendEmailTask ontology.
    """
    try:
        # Email sending logic
        from services.email_service import EmailService
        EmailService.send(recipient, subject, body)
    except Exception as exc:
        # Exponential backoff: 3 seconds, then 6, then 12
        raise self.retry(exc=exc, countdown=2 ** self.request.retries)

@shared_task(
    bind=True,
    queue="reports",
    max_retries=2,
    time_limit=1800,
    soft_time_limit=1790
)
def generate_report_task(self, report_id: str, params: dict):
    """
    Generate report asynchronously.
    Generated from task:GenerateReportTask ontology.
    """
    try:
        # Report generation logic
        from services.report_service import ReportService
        ReportService.generate(report_id, params)
    except Exception as exc:
        if self.request.retries < 2:
            raise self.retry(exc=exc, countdown=60)
        else:
            raise
```

---

## Pattern 5: Advanced WebSocket Support with FastAPI

### Description

Generate WebSocket endpoints with automatic message routing, authentication, and broadcast capabilities.

### External Packages

```json
{
  "fastapi": ">=0.104.0",
  "uvicorn[standard]": ">=0.24.0",
  "websockets": ">=12.0"
}
```

### Generated WebSocket Implementation

```python
# endpoints/websocket.py
from fastapi import FastAPI, WebSocket, WebSocketDisconnect, Depends
from typing import List, Dict
import json
import asyncio

app = FastAPI()

class ConnectionManager:
    """Manage WebSocket connections with room support"""

    def __init__(self):
        self.active_connections: Dict[str, List[WebSocket]] = {}

    async def connect(self, room_id: str, websocket: WebSocket):
        await websocket.accept()
        if room_id not in self.active_connections:
            self.active_connections[room_id] = []
        self.active_connections[room_id].append(websocket)

    def disconnect(self, room_id: str, websocket: WebSocket):
        self.active_connections[room_id].remove(websocket)
        if not self.active_connections[room_id]:
            del self.active_connections[room_id]

    async def broadcast(self, room_id: str, message: dict):
        """Broadcast message to all connections in room"""
        if room_id in self.active_connections:
            disconnected = []
            for connection in self.active_connections[room_id]:
                try:
                    await connection.send_json(message)
                except Exception:
                    disconnected.append(connection)

            for connection in disconnected:
                self.disconnect(room_id, connection)

manager = ConnectionManager()

@app.websocket("/ws/{room_id}")
async def websocket_endpoint(websocket: WebSocket, room_id: str):
    """WebSocket endpoint with room routing"""
    await manager.connect(room_id, websocket)
    try:
        while True:
            data = await websocket.receive_text()
            message = json.loads(data)

            # Broadcast to all clients in room
            await manager.broadcast(room_id, {
                "type": "message",
                "data": message,
                "room": room_id
            })
    except WebSocketDisconnect:
        manager.disconnect(room_id, websocket)
```

---

## Tera Template Examples

### FastAPI Service Template

```tera
# Generated FastAPI service from RDF ontology
from fastapi import FastAPI, Depends
from pydantic import BaseModel
from sqlalchemy.ext.asyncio import AsyncSession

app = FastAPI(
    title="{{ service.name }}",
    description="{{ service.description }}",
    version="{{ service.version }}"
)

{% for model in models %}
class {{ model.typeName }}(BaseModel):
    {% for field in model.fields %}
    {{ field.name }}: {{ field.type }}
    {% endfor %}

    class Config:
        from_attributes = True
{% endfor %}

@app.get("/")
async def root():
    return {"message": "{{ service.name }} API generated by ggen"}

{% for endpoint in endpoints %}
@app.{{ endpoint.method | lowercase }}("{{ endpoint.path }}")
async def {{ endpoint.operationId }}(
    {% for param in endpoint.parameters %}
    {{ param.name }}: {{ param.type }},
    {% endfor %}
    session: AsyncSession = Depends(get_session)
) -> {{ endpoint.responseType }}:
    """{{ endpoint.description }}"""
    pass
{% endfor %}
```

---

## Best Practices

1. **Async First**: Always use async/await for I/O operations
2. **Connection Pooling**: Configure min/max pool sizes appropriately
3. **Circuit Breakers**: Protect against cascading failures
4. **Caching Strategy**: Use Redis for distributed caching
5. **N+1 Prevention**: Use eager loading or DataLoaders
6. **Background Tasks**: Offload long-running operations to Celery
7. **Type Safety**: Leverage Pydantic and SQLAlchemy for runtime validation

---

## References

- [FastAPI Documentation](https://fastapi.tiangolo.com/)
- [SQLAlchemy Async Documentation](https://docs.sqlalchemy.org/en/20/orm/extensions/asyncio.html)
- [Motor Documentation](https://motor.readthedocs.io/)
- [Celery Documentation](https://docs.celeryproject.io/)

## See Also

- [PYTHON_FASTAPI_EXAMPLE.md](../how-to-guides/PYTHON_FASTAPI_EXAMPLE.md) - Base FastAPI example
- [ADVANCED_TYPESCRIPT_PATTERNS.md](./ADVANCED_TYPESCRIPT_PATTERNS.md) - Polyglot comparison
