"""
Generated GraphQL Schema with Strawberry GraphQL
Supports: Queries, Mutations, Subscriptions, DataLoaders
"""

import strawberry
from strawberry.permission import BasePermission
from strawberry.types import Info
from typing import List, Optional, AsyncIterator
from dataclasses import dataclass
from collections import defaultdict
import asyncpg
from asyncio import gather

# ============================================================================
# Domain Models
# ============================================================================

@dataclass
class UserModel:
    id: str
    name: str
    email: str


@dataclass
class PostModel:
    id: str
    title: str
    content: str
    author_id: str


# ============================================================================
# GraphQL Types
# ============================================================================

@strawberry.type
class User:
    id: strawberry.ID
    name: str
    email: str

    @strawberry.field
    async def posts(
        self,
        info: Info,
        first: int = 10,
        after: Optional[str] = None
    ) -> "PostConnection":
        loader = info.context["loaders"]["posts_by_user"]
        posts = await loader.load(self.id)

        offset = int(after) if after else 0
        limit = min(first, 100)

        paginated = posts[offset:offset + limit]
        has_next = len(posts) > offset + limit

        edges = [
            PostEdge(node=Post.from_model(p), cursor=str(offset + i))
            for i, p in enumerate(paginated)
        ]

        return PostConnection(
            edges=edges,
            page_info=PageInfo(
                has_next_page=has_next,
                has_previous_page=offset > 0,
                start_cursor=edges[0].cursor if edges else None,
                end_cursor=edges[-1].cursor if edges else None,
            )
        )

    @staticmethod
    def from_model(model: UserModel) -> "User":
        return User(
            id=strawberry.ID(model.id),
            name=model.name,
            email=model.email,
        )


@strawberry.type
class Post:
    id: strawberry.ID
    title: str
    content: str

    @strawberry.field
    async def author(self, info: Info) -> User:
        loader = info.context["loaders"]["user_by_id"]
        user = await loader.load(self.author_id)
        if not user:
            raise ValueError("Author not found")
        return User.from_model(user)

    author_id: strawberry.Private[str]

    @staticmethod
    def from_model(model: PostModel) -> "Post":
        return Post(
            id=strawberry.ID(model.id),
            title=model.title,
            content=model.content,
            author_id=model.author_id,
        )


# ============================================================================
# Pagination Types
# ============================================================================

@strawberry.type
class PageInfo:
    has_next_page: bool
    has_previous_page: bool
    start_cursor: Optional[str] = None
    end_cursor: Optional[str] = None


@strawberry.type
class PostEdge:
    node: Post
    cursor: str


@strawberry.type
class PostConnection:
    edges: List[PostEdge]
    page_info: PageInfo


@strawberry.type
class UserEdge:
    node: User
    cursor: str


@strawberry.type
class UserConnection:
    edges: List[UserEdge]
    page_info: PageInfo


# ============================================================================
# Input Types
# ============================================================================

@strawberry.input
class CreateUserInput:
    name: str
    email: str

    def validate(self):
        if not self.name or len(self.name) == 0 or len(self.name) > 255:
            raise ValueError("Name must be between 1 and 255 characters")
        if "@" not in self.email or len(self.email) > 255:
            raise ValueError("Invalid email address")


@strawberry.input
class UpdateUserInput:
    name: Optional[str] = None
    email: Optional[str] = None


# ============================================================================
# DataLoaders - Prevent N+1 Queries
# ============================================================================

class UserByIdLoader:
    def __init__(self, pool: asyncpg.Pool):
        self.pool = pool
        self._cache = {}

    async def load(self, key: str) -> Optional[UserModel]:
        """Load single user by ID"""
        if key in self._cache:
            return self._cache[key]

        # Batch load if needed - for now, single load
        async with self.pool.acquire() as conn:
            rows = await conn.fetch(
                "SELECT id, name, email FROM users WHERE id = ANY($1)",
                [key]
            )

        if rows:
            user = UserModel(**dict(rows[0]))
            self._cache[key] = user
            return user
        return None


class PostsByUserLoader:
    def __init__(self, pool: asyncpg.Pool):
        self.pool = pool
        self._cache = {}

    async def load(self, key: str) -> List[PostModel]:
        """Load posts for a user"""
        if key in self._cache:
            return self._cache[key]

        async with self.pool.acquire() as conn:
            rows = await conn.fetch(
                "SELECT id, title, content, author_id FROM posts WHERE author_id = ANY($1)",
                [key]
            )

        posts = [PostModel(**dict(row)) for row in rows]
        self._cache[key] = posts
        return posts


# ============================================================================
# Authorization
# ============================================================================

from enum import Enum as PyEnum

@strawberry.enum
class Role(PyEnum):
    ADMIN = "ADMIN"
    USER = "USER"


class IsAuthenticated(BasePermission):
    message = "User is not authenticated"

    def has_permission(self, source, info: Info, **kwargs) -> bool:
        return info.context.get("user") is not None


class RequireRole(BasePermission):
    def __init__(self, role: Role):
        self.role = role
        self.message = f"User must have {role.value} role"

    def has_permission(self, source, info: Info, **kwargs) -> bool:
        user = info.context.get("user")
        if not user:
            return False
        user_role = user.get("role")
        return user_role == self.role.value or user_role == "ADMIN"


# ============================================================================
# Query Root
# ============================================================================

@strawberry.type
class Query:
    @strawberry.field
    async def user(self, info: Info, id: strawberry.ID) -> Optional[User]:
        loader = info.context["loaders"]["user_by_id"]
        user = await loader.load(str(id))
        return User.from_model(user) if user else None

    @strawberry.field
    async def users(
        self,
        info: Info,
        first: int = 10,
        after: Optional[str] = None
    ) -> UserConnection:
        pool = info.context["pool"]

        offset = int(after) if after else 0
        limit = min(first, 100)

        async with pool.acquire() as conn:
            rows = await conn.fetch(
                "SELECT id, name, email FROM users ORDER BY id LIMIT $1 OFFSET $2",
                limit + 1,
                offset
            )

        has_next = len(rows) > limit
        users = rows[:limit]

        edges = [
            UserEdge(
                node=User.from_model(UserModel(**dict(row))),
                cursor=str(offset + i)
            )
            for i, row in enumerate(users)
        ]

        return UserConnection(
            edges=edges,
            page_info=PageInfo(
                has_next_page=has_next,
                has_previous_page=offset > 0,
                start_cursor=edges[0].cursor if edges else None,
                end_cursor=edges[-1].cursor if edges else None,
            )
        )


# ============================================================================
# Mutation Root
# ============================================================================

class RequireAdmin(BasePermission):
    message = "User must have ADMIN role"

    def has_permission(self, source, info: Info, **kwargs) -> bool:
        user = info.context.get("user")
        if not user:
            return False
        return user.get("role") == "ADMIN"


class RequireUser(BasePermission):
    message = "User must be authenticated"

    def has_permission(self, source, info: Info, **kwargs) -> bool:
        user = info.context.get("user")
        if not user:
            return False
        return user.get("role") in ["USER", "ADMIN"]


@strawberry.type
class Mutation:
    @strawberry.mutation(permission_classes=[RequireAdmin])
    async def create_user(
        self,
        info: Info,
        input: CreateUserInput
    ) -> User:
        input.validate()

        pool = info.context["pool"]
        pubsub = info.context["pubsub"]

        async with pool.acquire() as conn:
            row = await conn.fetchrow(
                "INSERT INTO users (name, email) VALUES ($1, $2) RETURNING id, name, email",
                input.name,
                input.email
            )

        user = User.from_model(UserModel(**dict(row)))

        # Publish event for subscriptions
        await pubsub.publish("USER_CREATED", user)

        return user

    @strawberry.mutation(permission_classes=[RequireUser])
    async def update_user(
        self,
        info: Info,
        id: strawberry.ID,
        input: UpdateUserInput
    ) -> User:
        pool = info.context["pool"]

        updates = []
        values = []
        param_index = 1

        if input.name is not None:
            updates.append(f"name = ${param_index}")
            values.append(input.name)
            param_index += 1

        if input.email is not None:
            updates.append(f"email = ${param_index}")
            values.append(input.email)
            param_index += 1

        values.append(str(id))

        async with pool.acquire() as conn:
            row = await conn.fetchrow(
                f"UPDATE users SET {', '.join(updates)} WHERE id = ${param_index} RETURNING id, name, email",
                *values
            )

        if not row:
            raise ValueError("User not found")

        return User.from_model(UserModel(**dict(row)))

    @strawberry.mutation(permission_classes=[RequireAdmin])
    async def delete_user(self, info: Info, id: strawberry.ID) -> bool:
        pool = info.context["pool"]

        async with pool.acquire() as conn:
            result = await conn.execute("DELETE FROM users WHERE id = $1", str(id))

        return result != "DELETE 0"


# ============================================================================
# Subscription Root
# ============================================================================

@strawberry.type
class Subscription:
    @strawberry.subscription(permission_classes=[RequireUser])
    async def user_created(self, info: Info) -> AsyncIterator[User]:
        pubsub = info.context["pubsub"]
        async for user in pubsub.subscribe("USER_CREATED"):
            yield user

    @strawberry.subscription
    async def post_updated(
        self,
        info: Info,
        user_id: Optional[strawberry.ID] = None
    ) -> AsyncIterator[Post]:
        pubsub = info.context["pubsub"]
        async for post in pubsub.subscribe("POST_UPDATED"):
            if user_id is None or post.author_id == str(user_id):
                yield post


# ============================================================================
# Schema Builder
# ============================================================================

schema = strawberry.Schema(
    query=Query,
    mutation=Mutation,
    subscription=Subscription,
)


def create_context(pool: asyncpg.Pool, pubsub, user=None):
    """Create GraphQL context with DataLoaders"""
    return {
        "pool": pool,
        "loaders": {
            "user_by_id": UserByIdLoader(pool),
            "posts_by_user": PostsByUserLoader(pool),
        },
        "pubsub": pubsub,
        "user": user,
    }
